use std::collections::VecDeque;

use crate::common::{Context, Symbol};
use crate::location::{Location, Span};
use crate::token::{Token, TokenKind};

macro_rules! str_or_err {
    ($self:expr, $start:expr) => {
        match std::str::from_utf8(&$self.source[$start..$self.pos]) {
            Ok(s) => s,
            Err(_) => {
                $self
                    .context
                    .report_error("Invalid UTF-8", $self.make_location($start));
                return Err(());
            }
        }
    };
}

pub fn generate_tokens(
    source: &[u8],
    file: Symbol,
    context: &mut Context,
) -> Result<VecDeque<Token>, ()> {
    let mut lexer = Lexer::new(source, file, context);
    let mut tokens = VecDeque::new();
    while let Some(token) = lexer.next_token()? {
        tokens.push_back(token);
    }

    Ok(tokens)
}

struct Lexer<'a> {
    file: Symbol,
    line: u32,
    column: u32,
    pos: usize,
    source: &'a [u8],
    context: &'a mut Context,
}

impl<'a> Lexer<'a> {
    pub fn new(
        source: &'a [u8],
        file: Symbol,
        context: &'a mut Context,
    ) -> Self {
        Lexer {
            context,
            file,
            line: 1,
            column: 1,
            pos: 0,
            source,
        }
    }
    fn make_location(&self, start: usize) -> Location {
        Location {
            line: self.line,
            file: self.file,
            span: Span {
                start: start as u32,
                len: (self.pos - start) as u32,
            },
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
        self.column += 1;
    }

    fn advance_while(&mut self, predicate: impl Fn(char) -> bool) {
        while self.pos < self.source.len() {
            if !predicate(self.source[self.pos] as char) {
                break;
            }
            self.advance()
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token>, ()> {
        while self.pos < self.source.len() {
            let start = self.pos;

            match self.source[self.pos] as char {
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                    self.pos += 1;
                }
                c if c.is_whitespace() => {
                    self.advance_while(char::is_whitespace);
                }
                c if c.is_alphabetic() => {
                    self.advance_while(|c| c.is_alphanumeric() || c == '_');
                    let string = str_or_err!(self, start);

                    let kind = match string {
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
                        "struct" => TokenKind::StructDecl,
                        other => TokenKind::Ident(
                            self.context.interner.intern(other),
                        ),
                    };

                    return Ok(Some(Token {
                        kind,
                        location: self.make_location(start),
                    }));
                }
                c if c.is_numeric() => {
                    self.advance_while(|c| c.is_numeric() || c == '.');
                    let string = str_or_err!(self, start);
                    let location = self.make_location(start);

                    let kind = if let Ok(integer) = string.parse() {
                        TokenKind::Integer(integer)
                    } else if let Ok(float) = string.parse() {
                        TokenKind::Float(float)
                    } else {
                        let mut iter = string.split("..");
                        let start = iter.next();
                        let end = iter.next();
                        match (start.map(str::parse), end.map(str::parse)) {
                            (Some(Ok(start)), Some(Ok(end))) => {
                                TokenKind::Range(start, end)
                            }
                            other => {
                                self.context
                                    .report_error(&format!("Invalid syntax. Expected range, found {:?}", other), location);
                                return Err(());
                            }
                        }
                    };

                    return Ok(Some(Token { kind, location }));
                }
                c if is_operator(c) => {
                    self.advance_while(is_operator);
                    let location = self.make_location(start);
                    let operator = str_or_err!(self, start);

                    if operator == "//" {
                        self.advance_while(|c| c != '\n');
                        continue;
                    }

                    let kind = match operator {
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
                        "->" => TokenKind::ReturnDecl,
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
                        other => {
                            self.context.report_error(
                                &format!(
                                    "Could not lex unknown operator '{}'",
                                    other
                                ),
                                self.make_location(start),
                            );
                            return Err(());
                        }
                    };

                    return Ok(Some(Token {
                        kind,
                        location: self.make_location(start),
                    }));
                }
                c if is_delimiter(c) => {
                    let kind = match c {
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

                    self.advance();

                    return Ok(Some(Token {
                        kind,
                        location: self.make_location(start),
                    }));
                }
                '"' => {
                    self.advance();
                    self.advance_while(|c| c != '"');
                    let location = self.make_location(start);
                    let string = str_or_err!(self, start);
                    let kind =
                        TokenKind::String(self.context.interner.intern(string));
                    self.advance();

                    return Ok(Some(Token {
                        kind,
                        location: self.make_location(start),
                    }));
                }
                '.' => {
                    self.advance();

                    return Ok(Some(Token {
                        kind: TokenKind::Field,
                        location: self.make_location(start),
                    }));
                }
                other => {
                    self.context.report_error(
                        &format!("Could not lex unknown token '{}'", other),
                        self.make_location(start),
                    );

                    return Err(());
                }
            }
        }

        Ok(None)
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

    fn assert_lex(source: &[u8], expected_tokens: &[TokenKind]) {
        let mut context = Context::new();
        let tokens = generate_tokens(source, Symbol::new(0), &mut context);
        assert!(tokens.is_ok());

        for (found, expected) in tokens.unwrap().iter().zip(expected_tokens) {
            assert_eq!(found.kind, *expected);
        }
    }

    #[test]
    fn lex_keywords() {
        assert_lex(
            b"if else for return while fn struct",
            &[
                TokenKind::If,
                TokenKind::Else,
                TokenKind::For,
                TokenKind::Return,
                TokenKind::While,
                TokenKind::FunctionDecl,
                TokenKind::StructDecl,
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
                TokenKind::Ident(Symbol::new(0)),
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
                TokenKind::Ident(Symbol::new(0)),
                TokenKind::Ident(Symbol::new(1)),
            ],
        )
    }

    #[test]
    fn lex_range() {
        assert_lex(b" 5..10", &[TokenKind::Range(5, 10)]);
    }

    #[test]
    fn lex_int() {
        assert_lex(
            b" 50 24",
            &[TokenKind::Integer(50), TokenKind::Integer(24)],
        );
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
                TokenKind::Ident(Symbol::new(0)),
                TokenKind::Ident(Symbol::new(1)),
            ],
        )
    }

    #[test]
    fn lex_if_else() {
        assert_lex(
            b" if cool_things {} else true",
            &[
                TokenKind::If,
                TokenKind::Ident(Symbol::new(0)),
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
