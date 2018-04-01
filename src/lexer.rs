use failure;
use itertools::{self, Itertools, MultiPeek};

use std::collections::VecDeque;
use std::num;
use std::vec::IntoIter;

#[derive(Debug, Fail)]
enum Error {
    #[fail(display = "Unexpected character '{}' at line {} column {}", ch, line, column)]
    UnexpectedCharacter {
        ch: char,
        line: usize,
        column: usize,
    },

    #[fail(display = "Unknown thing '{}' at line {} column {}", thing, line, column)]
    UnknownThing {
        thing: String,
        line: usize,
        column: usize,
    },

    #[fail(display = "{}", _0)]
    ParseFloat(#[cause] num::ParseFloatError),

    #[fail(display = "{}", _0)]
    ParseInt(#[cause] num::ParseIntError),
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    line: usize,
    span: Span,
}

#[derive(Debug)]
pub struct Span {
    start: usize,
    len: usize,
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
    _Initialize,
    _Assign,
    _AddAssign,
    _DivAssign,
    _MultAssign,
    _SubAssign,

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

    _Comment,
    Ident(String),
    _Bool(bool),
    Integer(i32),
    Float(f32),
    _String(String),

    // Types
    _BoolType,
    _FloatType,
    _IntType,
    _StringType,
}

type SourceIterator<'a> = MultiPeek<IntoIter<char>>;

struct Lexer {
    line: usize,
    column: usize,
}

pub fn generate_tokens(source: Vec<char>) -> Result<VecDeque<Token>, failure::Error> {
    let mut lexer = Lexer { line: 1, column: 1 };
    let mut tokens = VecDeque::new();
    let mut iter = itertools::multipeek(source.into_iter());

    while let Some(character) = iter.peek() {
        if *character == '\n' {
            lexer.line += 1;
            lexer.column = 1;
            iter.next().unwrap();
        } else if character.is_whitespace() {
            lexer.column += 1;
            iter.next().unwrap();
        } else if character.is_alphabetic() {
            if let Some(token) = lexer.lex_keyword(&mut iter)? {
                tokens.push_back(token);
            } else if let Some(token) = lexer.lex_identifier(&mut iter)? {
                tokens.push_back(token);
            }
        } else if character.is_numeric() {
            tokens.push_back(lexer.lex_numeral(&mut iter)?.unwrap());
        } else if is_delimiter(*character) {
            tokens.push_back(lexer.lex_delimiter(&mut iter)?.unwrap());
        } else if is_arithmetic_op(*character) {
            tokens.push_back(lexer.lex_arithmetic_op(&mut iter)?.unwrap());
        } else if is_part_of_logical_op(*character) {
            tokens.push_back(lexer.lex_logical_op(&mut iter)?.unwrap());
        } else if is_part_of_sigil(*character) {
            tokens.push_back(lexer.lex_sigil(&mut iter)?.unwrap());
        } else {
            return Err(format_err!("Could not lex {:?}", character));
        };
    }

    Ok(tokens)
}

impl Lexer {
    fn lex_identifier(
        &mut self,
        iter: &mut SourceIterator,
    ) -> Result<Option<Token>, failure::Error> {
        trace!("Lexing identifier");

        let start = self.column;

        let mut count = 0;
        while let Some(character) = iter.peek() {
            if character.is_alphanumeric() || *character == '_' {
                count += 1;
            } else {
                break;
            }
        }

        let ident: String = iter.take(count).collect();
        self.column += count;

        let token = Token {
            line: self.line,
            span: Span {
                start,
                len: self.column - start,
            },
            kind: TokenKind::Ident(ident),
        };

        debug!("LEXED {:?}", token);

        Ok(Some(token))
    }

    fn lex_numeral(&mut self, iter: &mut SourceIterator) -> Result<Option<Token>, Error> {
        trace!("Lexing numeral");

        let start = self.column;

        let is_numeric = |ch: &char| ch.is_numeric();
        let mut digits: Vec<char> = iter.take_while_ref(is_numeric).collect();

        let mut is_float = false;

        if let Some(character) = iter.peek() {
            if *character == '.' {
                is_float = true;
                iter.next().unwrap();
                digits.push('.');
                digits.extend(iter.take_while_ref(is_numeric));
            }
        }

        let digits: String = digits.into_iter().collect();
        self.column += digits.len();

        let kind = if is_float {
            let digits = digits.parse().map_err(|e| Error::ParseFloat(e))?;
            TokenKind::Float(digits)
        } else {
            let digits = digits.parse().map_err(|e| Error::ParseInt(e))?;
            TokenKind::Integer(digits)
        };

        let token = Token {
            kind,
            line: self.line,
            span: Span {
                start: start,
                len: digits.len(),
            },
        };

        debug!("LEXED {:?}", token);
        Ok(Some(token))
    }

    fn lex_delimiter(&mut self, iter: &mut SourceIterator) -> Result<Option<Token>, Error> {
        trace!("Lexing delimiter");

        let start = self.column;

        let character = iter.next().unwrap();
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

        let token = Token {
            line: self.line,
            span: Span {
                start,
                len: self.column - start,
            },
            kind: kind,
        };

        debug!("LEXED {:?}", token);
        Ok(Some(token))
    }

    fn lex_arithmetic_op(&mut self, iter: &mut SourceIterator) -> Result<Option<Token>, Error> {
        trace!("Lexing arithmetic operator");

        let start = self.column;

        let character = iter.next().unwrap();
        let kind = match character {
            '+' => TokenKind::Add,
            '-' => TokenKind::Sub,
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

        let token = Token {
            kind: kind,
            line: self.line,
            span: Span {
                start,
                len: self.column - start,
            },
        };

        debug!("LEXED {:?}", token);
        Ok(Some(token))
    }

    fn lex_logical_op(&mut self, iter: &mut SourceIterator) -> Result<Option<Token>, Error> {
        trace!("Lexing logical operator");

        let start = self.column;
        let op: String = iter.take_while_ref(|c| is_part_of_logical_op(*c)).collect();
        self.column += op.len();

        let kind = match op.as_str() {
            "!" => TokenKind::Not,
            "&&" => TokenKind::And,
            "||" => TokenKind::Or,
            "<=" => TokenKind::LesserEqual,
            "<" => TokenKind::Lesser,
            ">=" => TokenKind::GreaterEqual,
            ">" => TokenKind::Greater,
            "==" => TokenKind::Equal,
            "!=" => TokenKind::NotEqual,
            _ => {
                return Err(Error::UnknownThing {
                    thing: op,
                    line: self.line,
                    column: self.column,
                })
            }
        };

        let token = Token {
            kind,
            line: self.line,
            span: Span {
                start,
                len: self.column - start,
            },
        };

        debug!("LEXED {:?}", token);
        Ok(Some(token))
    }

    fn lex_sigil(&mut self, iter: &mut SourceIterator) -> Result<Option<Token>, Error> {
        trace!("Entered lex_sigil");

        let mut sigil = String::new();
        let start = self.column;

        while let Some(character) = iter.peek() {
            if is_part_of_sigil(*character) {
                sigil.push(*character);
                self.column += 1;
            } else {
                break;
            }
        }

        let kind = match sigil.as_str() {
            "->" => TokenKind::ReturnDeclaration,
            _ => {
                return Err(Error::UnknownThing {
                    line: self.line,
                    column: self.column,
                    thing: sigil,
                })
            }
        };

        let token = Token {
            kind,
            line: self.line,
            span: Span {
                start,
                len: self.column - start,
            },
        };

        debug!("LEXED {:?}", token);

        Ok(Some(token))
    }

    fn lex_keyword(&mut self, iter: &mut SourceIterator) -> Result<Option<Token>, Error> {
        trace!("Entered lex_keyword");

        let start = self.column;
        let mut word = String::new();

        while let Some(character) = iter.peek() {
            if character.is_alphabetic() {
                word.push(*character);
            } else {
                break;
            }
        }

        let kind = match word.as_str() {
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "elif" => TokenKind::ElseIf,
            "return" => TokenKind::Return,
            "fn" => TokenKind::Function,
            "for" => TokenKind::For,
            "while" => TokenKind::While,
            "print" => TokenKind::Print,
            _ => return Ok(None),
        };

        for _ in iter.take(word.len()) {}

        self.column += word.len();
        let token = Token {
            kind,
            line: self.line,
            span: Span {
                start,
                len: self.column - start,
            },
        };

        debug!("LEXED {:?}", token);
        Ok(Some(token))
    }
}

fn is_delimiter(c: char) -> bool {
    "{}()[],:;".contains(c)
}

fn is_arithmetic_op(c: char) -> bool {
    "+-*/".contains(c)
}

fn is_part_of_logical_op(c: char) -> bool {
    "!&|<>=".contains(c)
}

fn is_part_of_sigil(c: char) -> bool {
    match c {
        '-' | '>' => true,
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
//             }
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
//         position: position!() >> kind: map!(ws!(one_of!("(){}[],:;")), |delim: char| match delim {
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
//         }) >> ({
//             let position: Span = position.parse_to().unwrap();
//             Token {
//                 kind,
//                 line: position.line,
//                 col: position.offset,
//             }
//         })
//     )
// );

// named!(
//     sigils<Token>,
//     do_parse!(kind: ws!(tag!("->")) >> position: position!() >> (Token { kind, position }))
// );

// named!(
//     arith_op<Token>,
//     do_parse!(
//         kind: map!(ws!(one_of!("+-*/%")), |delim: char| match delim {
//             '+' => TokenKind::Plus,
//             '-' => TokenKind::Minus,
//             '*' => TokenKind::Mul,
//             '/' => TokenKind::Div,
//             '%' => TokenKind::Percent,
//             _ => unreachable!(),
//         }) >> position: position!() >> (Token { kind, position })
//     )
// );

// named!(
//     logical_op<Token>,
//     do_parse!(
//         kind: map!(
//             map_res!(
//                 ws!(alt!(tag!("&&") | tag!("||") | tag!("!"))),
//                 str::from_utf8
//             ),
//             |op: &str| match op {
//                 "&&" => TokenKind::And,
//                 "||" => TokenKind::Or,
//                 "!" => TokenKind::Not,
//                 _ => unreachable!(),
//             }
//         ) >> position: position!() >> (Token { kind, position })
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
//             }
//         ) >> position: position!() >> (Token { kind, position })
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
//             }
//         ) >> position: position!() >> (Token { kind, position })
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
//             }
//         ) >> position: position!() >> (Token { kind, position })
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
//                 ) >> position: position!() >> (Token {
//             kind: TokenKind::Ident(result),
//             position
//         })
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
//             }
//         ) >> position: position!() >> (Token { kind, position })
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
//             ) >> position: position!() >> (Token {
//             kind: TokenKind::String(string),
//             position
//         })
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
//             }
//         ) >> position: position!() >> (Token { kind, position })
//     )
// );

// named!(
//     file_end<Token>,
//     do_parse!(
//         ws!(eof!()) >> position: position!() >> (Token {
//             kind: TokenKind::Eof,
//             position
//         })
//     )
// );

// named!(
//     comment<Token>,
//     do_parse!(
//         preceded!(ws!(tag!("//")), not_line_ending) >> position: position!() >> (Token {
//             kind: TokenKind::Comment,
//             position
//         })
//     )
// );

// named!(
//     integer<Token>,
//     do_parse!(
//         as_digit: map_res!(map_res!(ws!(digit), str::from_utf8), str::parse)
//             >> position: position!() >> (Token {
//             kind: TokenKind::Integer(as_digit),
//             position
//         })
//     )
// );

// named!(
//     floats<Token>,
//     do_parse!(
//         as_digit: ws!(float) >> position: position!() >> (Token {
//             kind: TokenKind::Float(as_digit),
//             position
//         })
//     )
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
