use failure;

use std::iter::Peekable;
use std::vec::Drain;
use std::collections::VecDeque;

#[derive(Debug, Fail)]
enum Error {
    #[fail(display = "Unexpected character '{}' at line {} column {}", ch, line, column)]
    UnexpectedCharacter {
        ch: char,
        line: usize,
        column: usize,
    },

    #[fail(display = "Unexpected end of file at line {} column {}", line, column)]
    MissingCharacter { line: usize, column: usize },

    #[fail(display = "Unknown operator '{}' at line {} column {}", operator, line, column)]
    UnknownOperator {
        operator: String,
        line: usize,
        column: usize,
    },
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    line: usize,
    column: usize,
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
    MultAssign,
    SubAssign,

    // Arithmetic operators
    Mul,
    Minus,
    Mod,
    Plus,
    Div,

    // Delimiters
    CloseBrace,
    CloseBracket,
    CloseParen,
    Comma,
    SemiColon,
    Colon,
    ReturnDeclaration,
    OpenBrace,
    OpenBracket,
    OpenParen,

    // Keywords
    If,
    ElseIf,
    Else,
    For,
    Return,
    While,
    Print,
    Function,

    Comment,
    Eof,
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

    Consumed,
}

pub struct Lexer<'a> {
    source: Vec<u8>,
    iter: Peekable<Drain<'a, u8>>,
    tokens: VecDeque<Token>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: Vec<u8>) -> Self {
        Lexer {
            source,
            iter: source.drain(..).peekable(),
            tokens: VecDeque::new(),
            line: 1,
            column: 1,
        }
    }

    pub fn generate_tokens(mut self) -> Result<VecDeque<Token>, failure::Error> {
        while let Some(character) = self.iter.peek() {
            let character = *character as char;

            if character == '\n' {
                debug!("Lexed newline");
                self.line += 1;
                self.column = 1;
                self.iter.next().unwrap();
            } else if character.is_whitespace() {
                self.column += 1;
                self.iter.next().unwrap();
            } else if character.is_alphabetic() {
                self.lex_identifier()?;
            } else if is_delimiter(character) {
                self.lex_delimiter()?;
            } else if is_arithmetic_op(character) {
                self.lex_arithmetic_op()?;
            } else if is_logical_op(character) {
                self.lex_logical_op()?;
            }
        }

        Ok(self.tokens)
    }

    fn lex_identifier(&mut self) -> Result<(), Error> {
        debug!("Lexing identifier");

        let mut ident = String::new();

        while let Some(character) = self.iter.peek() {
            let character = *character as char;

            if character.is_alphanumeric() || character == '_' {
                ident.push(character);
                self.column += 1;
                self.iter.next().unwrap();
            } else {
                break;
            }
        }

        self.tokens.push_back(Token {
            line: self.line,
            column: self.column,
            kind: TokenKind::Ident(ident),
        });

        Ok(())
    }

    fn lex_delimiter(&mut self) -> Result<(), Error> {
        debug!("Lexing delimiter");

        let character = self.iter.next().unwrap() as char;
        let kind = match character {
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::SemiColon,
            _ => {
                return Err(Error::UnexpectedCharacter {
                    ch: character,
                    column: self.column,
                    line: self.line,
                })
            }
        };

        self.column += 1;

        self.tokens.push_back(Token {
            line: self.line,
            column: self.column,
            kind: kind,
        });

        Ok(())
    }

    fn lex_arithmetic_op(&mut self) -> Result<Token, Error> {
        debug!("Lexing arithmetic operator");

        let character = self.iter.next().unwrap() as char;
        let kind = match character {
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '/' => TokenKind::Div,
            '*' => TokenKind::Mul,
            '%' => TokenKind::Mod,
            _ => {
                return Err(Error::UnexpectedCharacter {
                    ch: character,
                    line: self.line,
                    column: self.column,
                })
            }
        };

        self.column += 1;

        Ok(Token {
            kind: kind,
            line: self.line,
            column: self.column,
        })
    }

    fn lex_logical_op(&mut self) -> Result<Token, Error> {
        debug!("Lexing logical operator");
        let mut op = String::new();

        while let Some(character) = self.iter.peek() {
            let character = *character as char;

            if is_logical_op(character) {
                op.push(character);
                self.iter.next().unwrap();
                self.column += 1;
            } else {
                break;
            }
        }

        let kind = match op.as_str() {
            "!" => TokenKind::Not,
            "&&" => TokenKind::And,
            "||" => TokenKind::Or,
            _ => {
                return Err(Error::UnknownOperator {
                    operator: op,
                    line: self.line,
                    column: self.column,
                })
            }
        };

        Ok(Token {
            kind,
            line: self.line,
            column: self.column,
        })
    }
}

fn is_delimiter(c: char) -> bool {
    match c {
        '{' | '}' | '(' | ')' | '[' | ']' | ',' | ':' | ';' => true,
        _ => false,
    }
}

fn is_arithmetic_op(c: char) -> bool {
    match c {
        '+' | '-' | '*' | '/' => true,
        _ => false,
    }
}

fn is_logical_op(c: char) -> bool {
    match c {
        '!' | '&' | '|' => true,
        _ => false,
    }
}

// pub fn generate_tokens(mut data: &[u8]) -> Result<VecDeque<Token>, failure::Error> {
//     let mut tokens = VecDeque::new();

//     loop {
//         let token = match get_token(data) {
//             IResult::Done(remaining, token) => {
//                 data = remaining;
//                 token
//             }
//             IResult::Incomplete(needed) => {
//                 return Err(format_err!(
//                     "Incomplete parsing, {:?} bytes missing",
//                     needed
//                 ))
//             }
//             IResult::Error(_) => {
//                 let source =
//                     str::from_utf8(data).map_err(|_| err_msg("Source is not valid UTF-8"))?;
//                 return Err(format_err!(
//                     "Could not parse '{}'",
//                     source.chars().next().unwrap()
//                 ));
//             }
//         };

//         match token {
//             Token { kind, .. } if kind == TokenKind::Comment => continue,
//             Token { kind, .. } if kind == TokenKind::Eof => {
//                 debug!("Eof");
//                 break;
//             },
//             token => {
//                 debug!("{:?}", token);
//                 tokens.push_back(token);
//             }
//         }
//     }

//     Ok(tokens)
// }

// named!(
//     get_token<Token>,
//     alt!(
//         file_end | string | comp_op | assign_op | delimiter | keyword | sigils | boolean | types
//             | ident | comment | logical_op | arith_op | floats | integer
//     )
// );

// named!(
//     delimiter<Token>,
//     do_parse!(
//         position: position!() >>
//         kind: map!(ws!(one_of!("(){}[],:;")), |delim: char| match delim {
//             '{' => TokenKind::OpenBrace,
//             '}' => TokenKind::CloseBrace,
//             '(' => TokenKind::OpenParen,
//             ')' => TokenKind::CloseParen,
//             '[' => TokenKind::OpenBracket,
//             ']' => TokenKind::CloseBracket,
//             ',' => TokenKind::Comma,
//             ':' => TokenKind::Colon,
//             ';' => TokenKind::SemiColon,
//             _ => unreachable!(),
//         }) >>
//             ({
//                 let position: Span = position.parse_to().unwrap();
//                 Token { kind, line: position.line, col: position.offset }
//             })
//     )
// );

// named!(
//     sigils<Token>,
//     do_parse!(
//         kind: ws!(tag!("->")) >>
//         position: position!() >>
//         (Token { kind, position }))
// );

// named!(
//     arith_op<Token>,
//     do_parse!(
//         kind: map!(ws!(one_of!("+-*/%")), |delim: char| match delim {
//         '+' => TokenKind::Plus,
//         '-' => TokenKind::Minus,
//         '*' => TokenKind::Mul,
//         '/' => TokenKind::Div,
//         '%' => TokenKind::Percent,
//         _ => unreachable!(),
//         }) >>
//         position: position!() >>
//         (Token { kind, position})
//     )
// );

// named!(
//     logical_op<Token>,
//     do_parse!(
//         kind: map!(
//         map_res!(
//             ws!(alt!(tag!("&&") | tag!("||") | tag!("!"))),
//             str::from_utf8
//         ),
//         |op: &str| match op {
//             "&&" => TokenKind::And,
//             "||" => TokenKind::Or,
//             "!" => TokenKind::Not,
//             _ => unreachable!(),
//         }) >>
//         position: position!() >>
//         (Token { kind, position })
//     )
// );

// named!(
//     keyword<Token>,
//     do_parse!(
//         kind: map!(
//             map_res!(
//                 ws!(alt!(
//                     tag!("if") | tag!("else if") | tag!("else") | tag!("for") | tag!("print")
//                         | tag!("return") | tag!("fn") | tag!("while")
//                 )),
//                 str::from_utf8
//             ),
//             |word: &str| match word {
//                 "if" => TokenKind::If,
//                 "else if" => TokenKind::ElseIf,
//                 "else" => TokenKind::Else,
//                 "for" => TokenKind::For,
//                 "print" => TokenKind::Print,
//                 "return" => TokenKind::Return,
//                 "fn" => TokenKind::Function,
//                 "while" => TokenKind::While,
//                 _ => unreachable!(),
//             }) >>
//         position: position!() >>
//         (Token { kind, position })
//     )
// );

// named!(
//     boolean<Token>,
//     do_parse!(
//         kind: map!(
//             map_res!(ws!(alt!(tag!("true") | tag!("false"))), str::from_utf8),
//             |token: &str| match token {
//                 "true" => TokenKind::Bool(true),
//                 "false" => TokenKind::Bool(false),
//                 _ => unreachable!(),
//             }) >>
//          position: position!() >>
//          (Token { kind, position })
//     )
// );

// named!(
//     types<Token>,
//     do_parse!(
//         kind: map!(
//             map_res!(
//                 ws!(alt!(
//                     tag!("string") | tag!("int") | tag!("float") | tag!("bool")
//                 )),
//                 str::from_utf8
//             ),
//             |word: &str| match word {
//                 "string" => TokenKind::StringType,
//                 "int" => TokenKind::IntType,
//                 "float" => TokenKind::FloatType,
//                 "bool" => TokenKind::BoolType,
//                 _ => unreachable!(),
//             }) >>
//         position: position!() >>
//         (Token { kind, position })
//     )
// );

// named!(
//     ident<Token>,
//     do_parse!(
//         opt!(multispace) >> init: map!(alpha, |init: &[u8]| init.to_vec())
//             >> result:
//                 map_res!(
//                     fold_many0!(
//                         alt!(alphanumeric | tag!("_")),
//                         init,
//                         |mut acc: Vec<_>, part| {
//                             acc.extend(part);
//                             acc
//                         }
//                     ),
//                     String::from_utf8
//                 ) >>
//             position: position!() >>
//             (Token { kind: TokenKind::Ident(result), position })
//     )
// );

// named!(
//     comp_op<Token>,
//     do_parse!(
//         kind: map!(
//             map_res!(
//                 ws!(alt!(
//                     tag!("<=") | tag!(">=") | tag!("!=") | tag!("==") | tag!("<") | tag!(">")
//                 )),
//                 str::from_utf8
//             ),
//             |op: &str| match op {
//                 "<=" => TokenKind::LesserEqual,
//                 ">=" => TokenKind::GreaterEqual,
//                 "==" => TokenKind::Equal,
//                 "<" => TokenKind::Lesser,
//                 ">" => TokenKind::Greater,
//                 "!=" => TokenKind::NotEqual,
//                 _ => unreachable!(),
//             }) >>
//         position: position!() >>
//         (Token { kind, position })
//     )
// );

// named!(
//     string<Token>,
//     do_parse!(
//         string:
//             map_res!(
//                 map!(
//                     ws!(delimited!(char!('"'), is_not!("\""), char!('"'))),
//                     |array: &[u8]| array.to_vec()
//                 ),
//                 String::from_utf8
//             ) >>
//             position: position!() >>
//             (Token { kind: TokenKind::String(string), position })
//     )
// );

// named!(
//     assign_op<Token>,
//     do_parse!(
//         kind: map!(
//             map_res!(
//                 ws!(alt!(
//                     tag!(":=") | tag!("+=") | tag!("-=") | tag!("*=") | tag!("/=") | tag!("=")
//                 )),
//                 str::from_utf8
//             ),
//             |op: &str| match op {
//                 ":=" => TokenKind::Initialize,
//                 "+=" => TokenKind::AddAssign,
//                 "-=" => TokenKind::SubAssign,
//                 "*=" => TokenKind::MultAssign,
//                 "/=" => TokenKind::DivAssign,
//                 "=" => TokenKind::Assign,
//                 _ => unreachable!(),
//             }) >>
//         position: position!() >>
//         (Token { kind, position })
//     )
// );

// named!(
//     file_end<Token>,
//     do_parse!(
//         ws!(eof!()) >>
//         position: position!() >>
//         (Token {kind: TokenKind::Eof, position}))
// );

// named!(
//     comment<Token>,
//     do_parse!(preceded!(ws!(tag!("//")), not_line_ending) >>
//               position: position!() >>
//               (Token { kind: TokenKind::Comment, position }))
// );

// named!(
//     integer<Token>,
//     do_parse!(
//         as_digit: map_res!(map_res!(ws!(digit), str::from_utf8), str::parse) >>
//         position: position!() >>
//         (Token { kind: TokenKind::Integer(as_digit), position })
//     )
// );

// named!(
//     floats<Token>,
//     do_parse!(
//         as_digit: ws!(float) >>
//         position: position!() >>
//         (Token { kind: TokenKind::Float(as_digit), position }))
// );

// #[cfg(test)]
// mod test {
//     use super::*;

//     fn assert_parse(source: &[u8], tokens: &[TokenKind]) {
//         let mut remaining = source;

//         for token in tokens {
//             let mut result = get_token(remaining);
//             assert!(result.is_done());
//             let (rem, parsed_token) = result.unwrap();
//             remaining = rem;
//             assert_eq!(token.kind, parsed_token);
//         }
//     }

//     #[test]
//     fn test_file_end() {
//         assert_parse(b"  ", &[TokenKind::Eof]);
//     }

//     #[test]
//     fn test_parse_sigils() {
//         assert_parse(b" -> ", &[TokenKind::ReturnDeclaration]);
//     }

//     #[test]
//     fn test_parse_keyword() {
//         assert_parse(
//             b" if else if else for while fn",
//             &[
//                 TokenKind::If,
//                 TokenKind::ElseIf,
//                 TokenKind::Else,
//                 TokenKind::For,
//                 TokenKind::While,
//                 TokenKind::Function,
//             ],
//         );
//     }

//     #[test]
//     fn test_parse_logical_operator() {
//         assert_parse(
//             b" && || ! ",
//             &[TokenKind::And, TokenKind::Or, TokenKind::Not],
//         );
//     }

//     #[test]
//     fn test_parse_types() {
//         assert_parse(
//             b" string int float bool ",
//             &[
//                 TokenKind::StringType,
//                 TokenKind::IntType,
//                 TokenKind::FloatType,
//                 TokenKind::BoolType,
//             ],
//         );
//     }

//     #[test]
//     fn test_parse_delimiter() {
//         assert_parse(
//             b" () [] {} , : ;",
//             &[
//                 TokenKind::OpenParen,
//                 TokenKind::CloseParen,
//                 TokenKind::OpenBracket,
//                 TokenKind::CloseBracket,
//                 TokenKind::OpenBrace,
//                 TokenKind::CloseBrace,
//                 TokenKind::Comma,
//                 TokenKind::Colon,
//                 TokenKind::SemiColon,
//             ],
//         );
//     }

//     #[test]
//     fn test_parse_ident() {
//         assert_parse(
//             b" name num_rows_10 true",
//             &[
//                 TokenKind::Ident("name".to_owned()),
//                 TokenKind::Ident("num_rows_10".to_owned()),
//             ],
//         );
//     }

//     #[test]
//     fn test_parse_arithmetic_operator() {
//         assert_parse(
//             b" + - * / %",
//             &[
//                 TokenKind::Plus,
//                 TokenKind::Minus,
//                 TokenKind::Mul,
//                 TokenKind::Div,
//                 TokenKind::Percent,
//             ],
//         );
//     }

//     #[test]
//     fn test_parse_string() {
//         assert_parse(
//             b" \"Hello friend\"",
//             &[TokenKind::String("Hello friend".to_owned())],
//         );
//     }

//     #[test]
//     fn test_parse_int() {
//         assert_parse(b" 457 ", &[TokenKind::Integer(457)]);
//     }

//     #[test]
//     fn test_parse_assign_operator() {
//         assert_parse(
//             b" := += -= *= /=",
//             &[
//                 TokenKind::Initialize,
//                 TokenKind::AddAssign,
//                 TokenKind::SubAssign,
//                 TokenKind::MultAssign,
//                 TokenKind::DivAssign,
//             ],
//         );
//     }

//     #[test]
//     fn test_parse_comparison_operator() {
//         assert_parse(
//             b" < > <= >= == != ",
//             &[
//                 TokenKind::Lesser,
//                 TokenKind::Greater,
//                 TokenKind::LesserEqual,
//                 TokenKind::GreaterEqual,
//                 TokenKind::Equal,
//                 TokenKind::NotEqual,
//             ],
//         );
//     }

//     #[test]
//     fn test_parse_float32() {
//         assert_parse(b" 457.45", &[TokenKind::Float(457.45)]);
//     }

//     #[test]
//     fn test_parse_comment() {
//         assert_parse(b" // hello there!!\n", &[TokenKind::Comment]);
//     }

//     #[test]
//     fn test_parse_bool() {
//         assert_parse(
//             b" true false",
//             &[TokenKind::Bool(true), TokenKind::Bool(false)],
//         );
//     }
// }
