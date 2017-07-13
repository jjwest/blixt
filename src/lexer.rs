use std::str;

use nom::{IResult, alpha, alphanumeric, float, multispace, not_line_ending, digit};

use errors::*;

pub struct Lexer<'a> {
    data: &'a [u8],
}

#[derive(Debug, PartialEq)]
pub enum Token {
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
    Asterisk,
    Minus,
    Percent,
    Plus,
    Slash,

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
    FunctionDeclaration,

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

impl<'a> Lexer<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Lexer { data }
    }

    pub fn generate_tokens(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        loop {
            let token = match get_token(self.data) {
                IResult::Done(remaining, token) => {
                    self.data = remaining;
                    token
                }
                IResult::Incomplete(needed) => {
                    return Err(
                        format!("Incomplete parsing, {:?} bytes missing", needed).into(),
                    )
                }
                IResult::Error(e) => {
                    debug!("Error: {:?}", e);
                    return Err(format!("Could not parse '{}'", e).into());
                } 
            };

            match token {
                Token::Eof => {
                    debug!("Eof");
                    break;
                }
                token => {
                    debug!("{:?}", token);
                    tokens.push(token);
                }
            }
        }

        Ok(tokens)
    }
}


named!(get_token<Token>,
       alt!(
           file_end
               | string
               | assign_op
               | delimiter
               | keyword
               | sigils
               | boolean
               | types
               | ident
               | comment
               | comp_op
               | logical_op
               | arith_op
               | floats
               | integer
       )
);

named!(delimiter<Token>,
       map!(ws!(one_of!("(){}[],:;")),
            |delim: char| match delim {
                '{' => Token::OpenBrace,
                '}' => Token::CloseBrace,
                '(' => Token::OpenParen,
                ')' => Token::CloseParen,
                '[' => Token::OpenBracket,
                ']' => Token::CloseBracket,
                ',' => Token::Comma,
                ':' => Token::Colon,
                ';' => Token::SemiColon,
                _ => unreachable!(),
            }
       )
);

named!(sigils<Token>,
       do_parse!(
           ws!(tag!("->")) >>
           (Token::ReturnDeclaration)
       )
);

named!(arith_op<Token>,
       map!(ws!(one_of!("+-*/%")),
            |delim: char| match delim {
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Asterisk,
                '/' => Token::Slash,
                '%' => Token::Percent,
                _ => unreachable!(),
            }
       )
);

named!(logical_op<Token>,
       map!(
           map_res!(ws!(alt!(tag!("&&") | tag!("||") | tag!("!"))),
                    str::from_utf8),
           |op: &str| match op {
               "&&" => Token::And,
               "||" => Token::Or,
               "!" => Token::Not,
               _ => unreachable!(),
           }
       )
);

named!(keyword<Token>,
       map!(
           map_res!(ws!(alt!(tag!("if")
                             | tag!("else if")
                             | tag!("else")
                             | tag!("for")
                             | tag!("print")
                             | tag!("fn")
                             | tag!("while"))),
                    str::from_utf8
           ),
           |word: &str| match word {
               "if" => Token::If,
               "else if" => Token::ElseIf,
               "else" => Token::Else,
               "for" => Token::For,
               "print" => Token::Print,
               "fn" => Token::FunctionDeclaration,
               "while" => Token::While,
               _ => unreachable!(),
           }
       )
);

named!(boolean<Token>,
       map!(
           map_res!(ws!(alt!(tag!("true") | tag!("false"))),
                    str::from_utf8),
           |token: &str| match token {
               "true" => Token::Bool(true),
               "false" => Token::Bool(false),
               _ => unreachable!(),
           }
       )
);

named!(types<Token>,
       map!(
           map_res!(ws!(alt!(tag!("string")
                             | tag!("int")
                             | tag!("float")
                             | tag!("bool"))),
                    str::from_utf8
           ),
           |word: &str| match word {
               "string" => Token::StringType,
               "int" => Token::IntType,
               "float" => Token::FloatType,
               "bool" => Token::BoolType,
               _ => unreachable!(),
           }
       )
);

named!(ident<Token>,
       do_parse!(
           many0!(multispace) >>
           init: map!(alpha, |init: &[u8]| init.to_vec()) >>
           result: map_res!(
               fold_many0!(
                   alt!(alphanumeric | tag!("_")),
                   init, |mut acc: Vec<_>, part| {
                       acc.extend(part);
                       acc
                   }
               ),
               String::from_utf8
           ) >>
           (Token::Ident(result))
       )
);

named!(comp_op<Token>,
       map!(
           map_res!(
               ws!(alt!(
                   tag!("<=")
                       | tag!(">=")
                       | tag!("!=")
                       | tag!("==")
                       | tag!("<")
                       | tag!(">")
               )),
               str::from_utf8
           ),
           |op: &str| match op {
               "<=" => Token::LesserEqual,
               ">=" => Token::GreaterEqual,
               "==" => Token::Equal,
               "<" => Token::Lesser,
               ">" => Token::Greater,
               "!=" => Token::NotEqual,
               _ => unreachable!(),
           }
       ) 
);

named!(string<Token>,
       do_parse!(
           string: map_res!(
               map!(
                   ws!(delimited!(char!('"'), is_not!("\""), char!('"'))),
                   |array: &[u8]| array.to_vec()
               ),
               String::from_utf8
           ) >>
           (Token::String(string))
       )
);

named!(assign_op<Token>,
       map!(
           map_res!(
               ws!(alt!(tag!(":=") | tag!("+=") | tag!("-=") | tag!("*=") | tag!("/=") | tag!("="))),
               str::from_utf8
           ),
           |op: &str| match op {
               ":=" => Token::Initialize,
               "+=" => Token::AddAssign,
               "-=" => Token::SubAssign,
               "*=" => Token::MultAssign,
               "/=" => Token::DivAssign,
               "=" => Token::Assign,
               _ => unreachable!(),
           }
       )
);

named!(file_end<Token>,
       do_parse!(
           ws!(eof!()) >>
           (Token::Eof)
       )
);

named!(comment<Token>,
       do_parse!(
           preceded!(ws!(tag!("//")), not_line_ending) >>
           (Token::Comment)
       )
);

named!(integer<Token>,
       do_parse!(
           as_digit: map_res!(
               map_res!(
                   ws!(digit),
                   str::from_utf8
               ),
               str::parse
           ) >>
           (Token::Integer(as_digit))
       )
);

named!(floats<Token>,
       do_parse!(
           as_digit: ws!(float) >>
           (Token::Float(as_digit))
       )
);

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_file_end() {
        let source = b"  ";
        let result = get_token(source);
        assert!(result.is_done());
        assert_eq!(Token::Eof, result.unwrap().1);
    }

    #[test]
    fn test_parse_sigils() {
        let source = b" -> ";
        let result = get_token(source);
        assert!(result.is_done());
        assert_eq!(Token::ReturnDeclaration, result.unwrap().1);
    }

    #[test]
    fn test_parse_keyword() {
        let source = b" if else if else for while fn";

        let mut result = get_token(source);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::If, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::ElseIf, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Else, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::For, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::While, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (_, token) = result.unwrap();
        assert_eq!(Token::FunctionDeclaration, token);
    }

    #[test]
    fn test_parse_logical_operator() {
        let source = b" && || ! ";

        let mut result = get_token(source);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::And, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Or, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (_, token) = result.unwrap();
        assert_eq!(Token::Not, token);
    }

    #[test]
    fn test_parse_types() {
        let source = b" string int float bool ";

        let mut result = get_token(source);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::StringType, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::IntType, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::FloatType, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (_, token) = result.unwrap();
        assert_eq!(Token::BoolType, token);

    }

    #[test]
    fn test_parse_delimiter() {
        let source = b" () [] {} , : ;";

        let mut result = get_token(source);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::OpenParen, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::CloseParen, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::OpenBracket, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::CloseBracket, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::OpenBrace, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::CloseBrace, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Comma, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Colon, token);

        result = get_token(remaining);
        assert!(result.is_done());
        let (_, token) = result.unwrap();
        assert_eq!(Token::SemiColon, token);
    }

    #[test]
    fn test_parse_ident() {
        let source = b" name num_rows_10 true";
        let result = get_token(source);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Ident("name".to_owned()), token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Ident("num_rows_10".to_owned()), token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (_, token) = result.unwrap();
        assert_ne!(Token::Ident("true".to_string()), token)
    }

    #[test]
    fn test_parse_arithmetic_operator() {
        let source = b" + - * / %";

        let result = get_token(source);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Plus, token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Minus, token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Asterisk, token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Slash, token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (_, token) = result.unwrap();
        assert_eq!(Token::Percent, token);

    }

    #[test]
    fn test_parse_string() {
        let source = b" \"Hello friend\"";
        let result = get_token(source);
        assert!(result.is_done());
        let (_, token) = result.unwrap();
        assert_eq!(Token::String("Hello friend".to_owned()), token);
    }

    #[test]
    fn test_parse_int() {
        let source = b" 457 ";
        let result = get_token(source);
        assert!(result.is_done());
        assert_eq!(Token::Integer(457), result.unwrap().1);
    }

    #[test]
    fn test_parse_assign_operator() {
        let source = b" := += -= *= /=";

        let result = get_token(source);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Assign, token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::AddAssign, token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::SubAssign, token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::MultAssign, token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (_, token) = result.unwrap();
        assert_eq!(Token::DivAssign, token);
    }

    #[test]
    fn test_parse_comparison_operator() {
        let source = b" < > <= >= == != ";

        let result = get_token(source);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Lesser, token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Greater, token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::LesserEqual, token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::GreaterEqual, token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Equal, token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (_, token) = result.unwrap();
        assert_eq!(Token::NotEqual, token);
    }

    #[test]
    fn test_parse_float32() {
        let source = b" 457.45";
        let result = get_token(source);
        assert!(result.is_done());
        assert_eq!(Token::Float(457.45), result.unwrap().1);
    }

    #[test]
    fn test_parse_comment() {
        let source = b" // hello there!!\n";
        let result = get_token(source);
        assert!(result.is_done());
        assert_eq!(Token::Comment, result.unwrap().1);
    }

    #[test]
    fn test_parse_bool() {
        let source = b" true false";
        let result = get_token(source);
        assert!(result.is_done());
        let (remaining, token) = result.unwrap();
        assert_eq!(Token::Bool(true), token);

        let result = get_token(remaining);
        assert!(result.is_done());
        let (_, token) = result.unwrap();
        assert_eq!(Token::Bool(false), token);
    }
}
