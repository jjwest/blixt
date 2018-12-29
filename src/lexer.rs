use crate::common::{Context, Symbol};
use crate::location::{Location, Span};
use crate::token::{Token, TokenKind};

use std::collections::VecDeque;
use std::fs;
use std::str;

macro_rules! location {
    ($self:expr, $start:expr) => {
        Location {
            line: $self.line,
            file: $self.file,
            span: Span {
                start: $start as u32,
                len: ($self.pos - $start) as u32,
            },
        }
    };
}

pub fn generate_tokens(file: &str, context: &mut Context) -> Result<VecDeque<Token>, ()> {
    let interned_file = context.interner.intern(file);
    let source = match fs::read(file) {
        Ok(src) => src,
        Err(e) => {
            eprintln!("Error: failed to read source file ({})", e);
            return Err(());
        }
    };

    let lexer = Lexer::new(interned_file, context);
    lexer.lex(source)
}

struct Lexer<'a> {
    file: Symbol,
    line: u32,
    column: u32,
    pos: usize,
    source: Vec<u8>,
    tokens: VecDeque<Token>,
    context: &'a mut Context,
}

impl<'a> Lexer<'a> {
    pub fn new(file: Symbol, context: &'a mut Context) -> Self {
        Self {
            file,
            line: 1,
            column: 1,
            pos: 0,
            tokens: VecDeque::new(),
            source: Vec::new(),
            context,
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
        self.column += 1;
    }

    pub fn lex(mut self, source: Vec<u8>) -> Result<VecDeque<Token>, ()> {
        self.source = source;

        while self.pos < self.source.len() {
            let start = self.pos;
            let column = self.column;

            match self.source[self.pos] as char {
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                    self.pos += 1;
                }
                c if c.is_whitespace() => {
                    self.advance();
                }
                c if c.is_alphabetic() => {
                    self.advance();
                    while self.pos < self.source.len() {
                        let c = self.source[self.pos] as char;
                        if !c.is_alphanumeric() && c != '_' {
                            break;
                        }
                        self.advance();
                    }

                    let location = location!(self, start);

                    let string = match str::from_utf8(&self.source[start..self.pos]) {
                        Ok(s) => s,
                        Err(_) => {
                            self.context.report_error("Invalid UTF-8", location);
                            return Err(());
                        }
                    };

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
                        _ => TokenKind::Ident(self.context.interner.intern(string)),
                    };

                    let token = Token { kind, location };
                    self.tokens.push_back(token);
                }
                c if c.is_numeric() => {
                    self.advance();

                    while self.pos < self.source.len() {
                        let c = self.source[self.pos] as char;
                        if !c.is_numeric() && c != '.' {
                            break;
                        }
                        self.advance();
                    }

                    let location = location!(self, start);

                    let s = match str::from_utf8(&self.source[start..self.pos]) {
                        Ok(s) => s,
                        Err(_) => {
                            self.context.report_error("Invalid UTF-8", location);
                            return Err(());
                        }
                    };

                    let kind = if let Ok(integer) = s.parse() {
                        TokenKind::Integer(integer)
                    } else if let Ok(float) = s.parse() {
                        TokenKind::Float(float)
                    } else {
                        self.context
                            .report_error("Failed to parse number", location);
                        return Err(());
                    };

                    self.tokens.push_back(Token { kind, location });
                }
                c if is_operator(c) => {
                    self.advance();

                    while self.pos < self.source.len() {
                        let c = self.source[self.pos] as char;
                        if !is_operator(c) {
                            break;
                        }
                        self.advance();
                    }

                    let location = location!(self, start);

                    let operator = match str::from_utf8(&self.source[start..self.pos]) {
                        Ok(op) => op,
                        Err(_) => {
                            self.context.report_error("Invalid UTF-8", location);
                            return Err(());
                        }
                    };

                    if operator == "//" {
                        while self.pos < self.source.len() {
                            let c = self.source[self.pos] as char;
                            self.advance();

                            if c == '\n' {
                                break;
                            }
                        }

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
                                &format!("Could not lex unknown operator '{}'", other),
                                location!(self, start),
                            );
                            return Err(());
                        }
                    };

                    self.tokens.push_back(Token {
                        kind,
                        location: location!(self, start),
                    })
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

                    self.tokens.push_back(Token {
                        kind,
                        location: location!(self, start),
                    })
                }
                '"' => {
                    self.advance();

                    while self.pos < self.source.len() {
                        let c = self.source[self.pos] as char;
                        self.advance();
                        if c == '"' {
                            break;
                        }
                    }

                    let location = location!(self, start);

                    let string = match str::from_utf8(&self.source[start..self.pos]) {
                        Ok(s) => s,
                        Err(_) => {
                            self.context.report_error("Invalid UTF-8", location);
                            return Err(());
                        }
                    };

                    self.tokens.push_back(Token {
                        kind: TokenKind::String(self.context.interner.intern(string)),
                        location: location!(self, start),
                    });
                }
                '.' => {
                    self.advance();

                    self.tokens.push_back(Token {
                        kind: TokenKind::Field,
                        location: location!(self, start),
                    })
                }
                other => {
                    self.context.report_error(
                        &format!("Could not lex unknown token '{}'", other),
                        location!(self, start),
                    );

                    return Err(());
                }
            }
        }

        println!("{:#?}", self.tokens);
        Ok(self.tokens)
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
        let mut context = Context::new();
        let lexer = Lexer::new(context.interner.intern("test.txt"), &mut context);
        let lexed_tokens = lexer.lex(Vec::from(source));
        assert!(lexed_tokens.is_ok());

        for (found, expected) in lexed_tokens.unwrap().iter().zip(tokens) {
            assert!(found.kind == *expected);
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
                TokenKind::Ident(Symbol::new(1)),
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
                TokenKind::Ident(Symbol::new(1)),
                TokenKind::Ident(Symbol::new(2)),
            ],
        )
    }

    // #[test]
    // fn lex_range() {
    //     assert_lex(b" 5..10", &[TokenKind::Range(5, 10)]);
    // }

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
                TokenKind::Ident(Symbol::new(1)),
                TokenKind::Ident(Symbol::new(2)),
            ],
        )
    }

    #[test]
    fn lex_if_else() {
        assert_lex(
            b" if cool_things {} else true",
            &[
                TokenKind::If,
                TokenKind::Ident(Symbol::new(1)),
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
