use failure;
use itertools::Itertools;
use log::LogLevel;

use context::Context;
use location::{InternedString, Location, Span};

use std::collections::VecDeque;
use std::fs;
use std::iter::Peekable;
use std::vec::IntoIter;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Location,
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

    // operators
    Assign,
    AddAssign,
    DivAssign,
    MulAssign,
    SubAssign,
    ModAssign,
    Member,

    // Declarations
    VarDecl,

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

    ReturnDecl,
    OpenBrace,
    OpenBracket,
    OpenParen,

    // Keywords
    If,
    Else,
    For,
    Return,
    While,
    FunctionDecl,
    Range(i32, i32),
    In,

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

#[derive(Debug, Fail)]
#[fail(display = "Lexer error")]
struct Error;

struct Lexer {
    line: usize,
    column: usize,
    file: InternedString,
    source: Peekable<IntoIter<char>>,
}

pub fn generate_tokens(
    file: &str,
    context: &mut Context,
) -> Result<VecDeque<Token>, failure::Error> {
    let interned_file = context.interner.intern(file);
    let source: Vec<char> = fs::read(file)?.into_iter().map(|c| c as char).collect();

    let lexer = Lexer {
        line: 1,
        file: interned_file,
        column: 1,
        source: source.into_iter().peekable(),
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

impl Iterator for Lexer {
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
                let word: String = self
                    .source
                    .take_while_ref(|c| c.is_alphanumeric() || *c == '_')
                    .collect();

                self.column += word.len();

                let kind = match word.as_str() {
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "for" => TokenKind::For,
                    "in" => TokenKind::In,
                    "while" => TokenKind::While,
                    "fn" => TokenKind::FunctionDecl,
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
                    location: Location {
                        line: self.line,
                        file: self.file,
                        span: Span {
                            start,
                            len: self.column - start,
                        },
                    },
                }))
            }
            c if c.is_numeric() => {
                let mut number: String = self.source.take_while_ref(|c| c.is_numeric()).collect();
                let mut is_float = false;

                if let Some(character) = self.source.peek() {
                    if *character == '.' {
                        self.source.next();

                        match self.source.peek() {
                            Some(ch) if *ch == '.' => {
                                let range_start = number.parse().unwrap();
                                self.column += 1;
                                self.source.next();
                                number.clear();
                                number.extend(self.source.take_while_ref(|c| c.is_numeric()));

                                let range_end = number.parse().unwrap();

                                return Some(Ok(Token {
                                    kind: TokenKind::Range(range_start, range_end),
                                    location: Location {
                                        line: self.line,
                                        file: self.file,
                                        span: Span {
                                            start,
                                            len: self.column - start,
                                        },
                                    },
                                }));
                            }
                            _ => {}
                        }

                        number.push('.');
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
                    location: Location {
                        line: self.line,
                        file: self.file,
                        span: Span {
                            start,
                            len: self.column - start,
                        },
                    },
                }))
            }
            c if is_operator(*c) => {
                let operator: String = self.source.take_while_ref(|ch| is_operator(*ch)).collect();
                self.column += operator.len();

                if operator == "//" {
                    let _: Vec<_> = self.source.take_while_ref(|c| *c != '\n').collect();
                    return self.next();
                }

                if operator == "->" {
                    return Some(Ok(Token {
                        kind: TokenKind::ReturnDecl,
                        location: Location {
                            file: self.file,
                            line: self.line,
                            span: Span {
                                start,
                                len: self.column - start,
                            },
                        },
                    }));
                }

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
                    ":=" => TokenKind::VarDecl,
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
                    _op => return Some(Err(Error)),
                };

                Some(Ok(Token {
                    kind,
                    location: Location {
                        line: self.line,
                        file: self.file,
                        span: Span {
                            start,
                            len: self.column - start,
                        },
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
                    location: Location {
                        file: self.file,
                        line: self.line,
                        span: Span {
                            start,
                            len: self.column - start,
                        },
                    },
                }))
            }
            '"' => {
                self.source.next();
                let mut string_literal = String::new();
                loop {
                    string_literal.extend(self.source.take_while_ref(|c| *c != '"'));
                    if string_literal.ends_with(r"\") {
                        string_literal.push('"');
                        self.source.next();
                    } else {
                        break;
                    }
                }

                self.source.next();
                self.column += string_literal.len() + 2;

                Some(Ok(Token {
                    kind: TokenKind::String(string_literal),
                    location: Location {
                        file: self.file,
                        line: self.line,
                        span: Span {
                            start,
                            len: self.column - start,
                        },
                    },
                }))
            }
            '\0' => None,
            _ => Some(Err(Error)),
        }
    }
}

fn is_operator(ch: char) -> bool {
    "+-*<>=!&|:/%".contains(ch)
}

fn is_delimiter(ch: char) -> bool {
    "(){}[]:,;".contains(ch)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_lex(source: &[u8], tokens: &[TokenKind]) {
        let source: Vec<char> = source.iter().map(|c| *c as char).collect();
        let lexer = Lexer {
            file: 0,
            line: 1,
            column: 1,
            source: source.into_iter().peekable(),
        };

        for (token, expected) in lexer.zip(tokens) {
            assert!(token.is_ok());
            assert!(token.unwrap().kind == *expected);
        }
    }

    #[test]
    fn lex_keywords() {
        assert_lex(
            b"if else for return while fn",
            &[
                TokenKind::If,
                TokenKind::Else,
                TokenKind::For,
                TokenKind::Return,
                TokenKind::While,
                TokenKind::FunctionDecl,
            ],
        )
    }

    #[test]
    fn lex_binary_logical_operators() {
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
    fn lex_unary_logical_operators() {
        assert_lex(b" ! ", &[TokenKind::Not]);
    }

    #[test]
    fn lex_assignment_operator() {
        assert_lex(
            b"= += -= *= /= %=",
            &[
                TokenKind::Assign,
                TokenKind::AddAssign,
                TokenKind::SubAssign,
                TokenKind::MulAssign,
                TokenKind::DivAssign,
                TokenKind::ModAssign,
            ],
        )
    }

    #[test]
    fn lex_arithmetic_operators() {
        assert_lex(
            b" + - * / %",
            &[
                TokenKind::Add,
                TokenKind::Sub,
                TokenKind::Mul,
                TokenKind::Div,
                TokenKind::Mod,
            ],
        );
    }

    #[test]
    fn lex_variable_declaration() {
        assert_lex(b" := ", &[TokenKind::VarDecl]);
    }

    #[test]
    fn lex_assignment() {
        assert_lex(
            b"age: int = 27",
            &[
                TokenKind::Ident("age".to_owned()),
                TokenKind::Colon,
                TokenKind::IntType,
                TokenKind::Assign,
                TokenKind::Integer(27),
            ],
        )
    }

    #[test]
    fn lex_comment() {
        assert_lex(
            b"hello // there friend\no",
            &[
                TokenKind::Ident("hello".to_owned()),
                TokenKind::Ident("o".to_owned()),
            ],
        )
    }

    #[test]
    fn lex_range() {
        assert_lex(b" 5..10", &[TokenKind::Range(5, 10)]);
    }

    #[test]
    fn lex_int() {
        assert_lex(b" 50 24", &[TokenKind::Integer(50), TokenKind::Integer(24)]);
    }

    #[test]
    fn lex_float() {
        assert_lex(
            b" 34.2389 15.123",
            &[TokenKind::Float(34.2389), TokenKind::Float(15.123)],
        );
    }

    #[test]
    fn lex_bool() {
        assert_lex(
            b"true false",
            &[TokenKind::Bool(true), TokenKind::Bool(false)],
        );
    }

    #[test]
    fn lex_ident() {
        assert_lex(
            b"foo bar_Baz",
            &[
                TokenKind::Ident("foo".to_owned()),
                TokenKind::Ident("bar_Baz".to_owned()),
            ],
        )
    }

    #[test]
    fn lex_if_else() {
        assert_lex(
            b" if cool_things {} else true",
            &[
                TokenKind::If,
                TokenKind::Ident("cool_things".to_owned()),
                TokenKind::OpenBrace,
                TokenKind::CloseBrace,
                TokenKind::Else,
                TokenKind::Bool(true),
            ],
        )
    }

    #[test]
    fn lex_delimiters() {
        assert_lex(
            b" (){}[]:,;",
            &[
                TokenKind::OpenParen,
                TokenKind::CloseParen,
                TokenKind::OpenBrace,
                TokenKind::CloseBrace,
                TokenKind::OpenBracket,
                TokenKind::CloseBracket,
                TokenKind::Colon,
                TokenKind::Comma,
                TokenKind::SemiColon,
            ],
        )
    }

    #[test]
    fn lex_types() {
        assert_lex(
            b"string float int bool",
            &[
                TokenKind::StringType,
                TokenKind::FloatType,
                TokenKind::IntType,
                TokenKind::BoolType,
            ],
        );
    }
}
