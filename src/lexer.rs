use failure;
use itertools::Itertools;
use log::LogLevel;

use std::collections::VecDeque;
use std::iter::{Cloned, Peekable};
use std::slice::Iter;

#[derive(Debug, Fail)]
#[fail(display = "Placeholder")]
pub struct Error {
    pub line: usize,
    pub span: Span,
    pub message: String,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub span: Span,
}

#[derive(Debug)]
pub struct Span {
    pub start: usize,
    pub len: usize,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    // Logical Operators
    And,
    Or,
    Not,

    // Comparison operators
    Equal,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
    NotEqual,

    // Assign operators
    Initialize,
    Assign,
    AddAssign,
    DivAssign,
    MulAssign,
    SubAssign,
    ModAssign,

    // Arithmetic operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Delimiters
    CloseBrace,
    CloseBracket,
    CloseParen,
    Comma,
    SemiColon,
    Colon,

    // ReturnDeclaration,
    OpenBrace,
    OpenBracket,
    OpenParen,

    // Keywords
    If,
    Else,
    For,
    Return,
    While,
    Function,

    Ident(String),
    Bool(bool),
    Integer(i32),
    Float(f32),
    String(String),

    // Types
    BoolType,
    FloatType,
    IntType,
    StringType,
}

struct Lexer<'a> {
    line: usize,
    column: usize,
    source: Peekable<Cloned<Iter<'a, char>>>,
}

pub fn generate_tokens(source: &[char]) -> Result<VecDeque<Token>, failure::Error> {
    let lexer = Lexer {
        line: 1,
        column: 1,
        source: source.iter().cloned().peekable(),
    };

    let tokens: Result<VecDeque<Token>, Error> = lexer.collect();
    let tokens = tokens?;

    if log_enabled!(LogLevel::Debug) {
        for token in &tokens {
            debug!("{:?}", token);
        }
    }

    Ok(tokens)
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.column;

        match self.source.peek().unwrap_or(&'\0') {
            '\n' => {
                self.line += 1;
                self.column = 1;
                self.source.next();
                self.next()
            }
            c if c.is_whitespace() => {
                self.column += 1;
                self.source.next();
                self.next()
            }
            c if c.is_alphabetic() => {
                let word: String = self.source
                    .take_while_ref(|c| c.is_alphanumeric() || *c == '_')
                    .collect();

                self.column += word.len();

                let kind = match word.as_str() {
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "for" => TokenKind::For,
                    "while" => TokenKind::While,
                    "fn" => TokenKind::Function,
                    "return" => TokenKind::Return,
                    "string" => TokenKind::StringType,
                    "float" => TokenKind::FloatType,
                    "int" => TokenKind::IntType,
                    "bool" => TokenKind::BoolType,
                    "true" => TokenKind::Bool(true),
                    "false" => TokenKind::Bool(false),
                    _ => TokenKind::Ident(word),
                };

                Some(Ok(Token {
                    kind,
                    line: self.line,
                    span: Span {
                        start,
                        len: self.column - start,
                    },
                }))
            }
            c if c.is_numeric() => {
                let mut number: String = self.source.take_while_ref(|c| c.is_numeric()).collect();
                let mut is_float = false;

                if let Some(character) = self.source.peek() {
                    if *character == '.' {
                        number.push(self.source.next().unwrap());
                        number.extend(self.source.take_while_ref(|c| c.is_numeric()));
                        is_float = true;
                    }
                }

                self.column += number.len();

                let kind = if is_float {
                    TokenKind::Float(number.parse().unwrap())
                } else {
                    TokenKind::Integer(number.parse().unwrap())
                };

                Some(Ok(Token {
                    kind,
                    line: self.line,
                    span: Span {
                        start,
                        len: self.column - start,
                    },
                }))
            }
            c if is_operator(*c) => {
                let operator: String = self.source.take_while_ref(|ch| is_operator(*ch)).collect();
                self.column += operator.len();

                let kind = match operator.as_str() {
                    "&&" => TokenKind::And,
                    "||" => TokenKind::Or,
                    "==" => TokenKind::Equal,
                    "!=" => TokenKind::NotEqual,
                    "<=" => TokenKind::LesserEqual,
                    ">=" => TokenKind::GreaterEqual,
                    "+=" => TokenKind::AddAssign,
                    "-=" => TokenKind::SubAssign,
                    "*=" => TokenKind::MulAssign,
                    "/=" => TokenKind::DivAssign,
                    "%=" => TokenKind::ModAssign,
                    ":=" => TokenKind::Initialize,
                    "=" => TokenKind::Assign,
                    ">" => TokenKind::Greater,
                    "<" => TokenKind::Lesser,
                    "!" => TokenKind::Not,
                    "+" => TokenKind::Add,
                    "-" => TokenKind::Sub,
                    "*" => TokenKind::Mul,
                    "/" => TokenKind::Div,
                    "%" => TokenKind::Mod,
                    ":" => TokenKind::Colon,
                    op => {
                        return Some(Err(Error {
                            line: self.line,
                            span: Span {
                                start,
                                len: self.column - start,
                            },
                            message: format!("Invalid operator '{}'", op),
                        }))
                    }
                };

                Some(Ok(Token {
                    kind,
                    line: self.line,
                    span: Span {
                        start,
                        len: self.column - start,
                    },
                }))
            }
            c if is_delimiter(*c) => {
                let kind = match self.source.next().unwrap() {
                    '(' => TokenKind::OpenParen,
                    ')' => TokenKind::CloseParen,
                    '[' => TokenKind::OpenBracket,
                    ']' => TokenKind::CloseBracket,
                    '{' => TokenKind::OpenBrace,
                    '}' => TokenKind::CloseBrace,
                    ':' => TokenKind::Colon,
                    ';' => TokenKind::SemiColon,
                    ',' => TokenKind::Comma,
                    _ => unreachable!(),
                };

                self.column += 1;

                Some(Ok(Token {
                    kind,
                    line: self.line,
                    span: Span {
                        start,
                        len: self.column - start,
                    },
                }))
            }
            '"' => {
                self.source.next();
                let string_literal: String = self.source.take_while_ref(|c| *c != '"').collect();
                self.source.next();

                self.column += string_literal.len() + 2;

                Some(Ok(Token {
                    kind: TokenKind::String(string_literal),
                    line: self.line,
                    span: Span {
                        start,
                        len: self.column - start,
                    },
                }))
            }
            '/' => {
                self.source.next();
                if let Some(character) = self.source.peek() {
                    if *character == '/' {
                        for _ in self.source.take_while_ref(|c| *c != '\n') {}
                    } else {
                        let token: String =
                            self.source.take_while_ref(|c| !c.is_whitespace()).collect();

                        return Some(Err(Error {
                            line: self.line,
                            span: Span {
                                start,
                                len: token.len() + 1,
                            },
                            message: format!("Unknown token '/{}'", token),
                        }));
                    }
                }

                self.next()
            }
            '\0' => None,
            _ => Some(Err(Error {
                line: self.line,
                span: Span { start, len: 1 },
                message: format!(
                    "Unexpected token: {}",
                    self.source
                        .take_while_ref(|c| !c.is_whitespace())
                        .collect::<String>()
                ),
            })),
        }
    }
}

#[inline]
fn is_operator(ch: char) -> bool {
    "+-*<>=!&|:/%".contains(ch)
}

#[inline]
fn is_delimiter(ch: char) -> bool {
    "(){}[]:,;".contains(ch)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_lex(source: &[u8], tokens: &[TokenKind]) {
        let source: Vec<char> = source.iter().cloned().map(|c| c as char).collect();
        let lexer = Lexer {
            line: 1,
            column: 1,
            source: source.iter().cloned().peekable(),
        };

        for (token, expected) in lexer.zip(tokens) {
            assert!(token.is_ok());
            assert!(token.unwrap().kind == *expected);
        }
    }

    #[test]
    fn test_parse_keywords() {
        assert_lex(
            b"if else for return while fn",
            &[
                TokenKind::If,
                TokenKind::Else,
                TokenKind::For,
                TokenKind::Return,
                TokenKind::While,
                TokenKind::Function,
            ],
        )
    }

    #[test]
    fn test_parse_binary_operators() {
        assert_lex(
            b"== > >= < <= !=",
            &[
                TokenKind::Equal,
                TokenKind::Greater,
                TokenKind::GreaterEqual,
                TokenKind::Lesser,
                TokenKind::LesserEqual,
                TokenKind::NotEqual,
            ],
        )
    }

    #[test]
    fn test_parse_assignment() {
        assert_lex(
            b"= := += -= *= /= %=",
            &[
                TokenKind::Assign,
                TokenKind::Initialize,
                TokenKind::AddAssign,
                TokenKind::SubAssign,
                TokenKind::MulAssign,
                TokenKind::DivAssign,
                TokenKind::ModAssign,
            ],
        )
    }

}
