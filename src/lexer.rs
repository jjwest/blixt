use std::str;

use nom::{IResult, alpha, alphanumeric, float, multispace, not_line_ending, digit};

use errors::*;

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

pub fn generate_tokens(mut data: &[u8]) -> Result<Vec<Token>> {
    let mut tokens = Vec::new();

    loop {
        let token = match get_token(data) {
            IResult::Done(remaining, token) => {
                data = remaining;
                token
            }
            IResult::Incomplete(needed) => {
                return Err(format_err!(
                    "Incomplete parsing, {:?} bytes missing",
                    needed
                ))
            }
            IResult::Error(_) => {
                let source = str::from_utf8(data).map_err(
                    |_| err_msg("Source is not valid UTF-8"),
                )?;
                return Err(format_err!(
                    "Could not parse '{}'",
                    source.chars().next().unwrap()
                ));
            } 
        };

        match token {
            Token::Comment => continue,
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


named!(get_token<Token>,
       alt!(
           file_end
               | string
               | comp_op
               | assign_op
               | delimiter
               | keyword
               | sigils
               | boolean
               | types
               | ident
               | comment
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
                             | tag!("return")
                             | tag!("fn")
                             | tag!("while"))),
                    str::from_utf8
           ),
           |word: &str| match word {
               "if"      => Token::If,
               "else if" => Token::ElseIf,
               "else"    => Token::Else,
               "for"     => Token::For,
               "print"   => Token::Print,
               "return"  => Token::Return,
               "fn"      => Token::Function,
               "while"   => Token::While,
               _         => unreachable!(),
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
               "int"    => Token::IntType,
               "float"  => Token::FloatType,
               "bool"   => Token::BoolType,
               _ => unreachable!(),
           }
       )
);

named!(ident<Token>,
       do_parse!(
           opt!(multispace) >>
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
               "<"  => Token::Lesser,
               ">"  => Token::Greater,
               "!=" => Token::NotEqual,
               _    => unreachable!(),
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

    fn assert_parse(source: &[u8], tokens: &[Token]) {
        let mut remaining = source;

        for token in tokens {
            let mut result = get_token(remaining);
            assert!(result.is_done());
            let (rem, parsed_token) = result.unwrap();
            remaining = rem;
            assert_eq!(*token, parsed_token);
        }
    }

    #[test]
    fn test_file_end() {
        assert_parse(b"  ", &[Token::Eof]);
    }

    #[test]
    fn test_parse_sigils() {
        assert_parse(b" -> ", &[Token::ReturnDeclaration]);
    }

    #[test]
    fn test_parse_keyword() {
        assert_parse(
            b" if else if else for while fn",
            &[
                Token::If,
                Token::ElseIf,
                Token::Else,
                Token::For,
                Token::While,
                Token::Function,
            ],
        );
    }

    #[test]
    fn test_parse_logical_operator() {
        assert_parse(b" && || ! ", &[Token::And, Token::Or, Token::Not]);
    }

    #[test]
    fn test_parse_types() {
        assert_parse(
            b" string int float bool ",
            &[
                Token::StringType,
                Token::IntType,
                Token::FloatType,
                Token::BoolType,
            ],
        );
    }

    #[test]
    fn test_parse_delimiter() {
        assert_parse(
            b" () [] {} , : ;",
            &[
                Token::OpenParen,
                Token::CloseParen,
                Token::OpenBracket,
                Token::CloseBracket,
                Token::OpenBrace,
                Token::CloseBrace,
                Token::Comma,
                Token::Colon,
                Token::SemiColon,
            ],
        );
    }

    #[test]
    fn test_parse_ident() {
        assert_parse(
            b" name num_rows_10 true",
            &[
                Token::Ident("name".to_owned()),
                Token::Ident("num_rows_10".to_owned()),
            ],
        );
    }

    #[test]
    fn test_parse_arithmetic_operator() {
        assert_parse(
            b" + - * / %",
            &[
                Token::Plus,
                Token::Minus,
                Token::Asterisk,
                Token::Slash,
                Token::Percent,
            ],
        );
    }

    #[test]
    fn test_parse_string() {
        assert_parse(
            b" \"Hello friend\"",
            &[Token::String("Hello friend".to_owned())],
        );
    }

    #[test]
    fn test_parse_int() {
        assert_parse(b" 457 ", &[Token::Integer(457)]);
    }

    #[test]
    fn test_parse_assign_operator() {
        assert_parse(
            b" := += -= *= /=",
            &[
                Token::Initialize,
                Token::AddAssign,
                Token::SubAssign,
                Token::MultAssign,
                Token::DivAssign,
            ],
        );
    }

    #[test]
    fn test_parse_comparison_operator() {
        assert_parse(
            b" < > <= >= == != ",
            &[
                Token::Lesser,
                Token::Greater,
                Token::LesserEqual,
                Token::GreaterEqual,
                Token::Equal,
                Token::NotEqual,
            ],
        );
    }

    #[test]
    fn test_parse_float32() {
        assert_parse(b" 457.45", &[Token::Float(457.45)]);
    }

    #[test]
    fn test_parse_comment() {
        assert_parse(b" // hello there!!\n", &[Token::Comment]);
    }

    #[test]
    fn test_parse_bool() {
        assert_parse(b" true false", &[Token::Bool(true), Token::Bool(false)]);
    }
}
