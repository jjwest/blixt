use std::str;

use nom::{IResult, alpha, anychar, not_line_ending, digit};

pub struct Lexer<'a> {
    data: &'a [u8],
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Comment,
    String,
    Integer(i32),
    Dot,
    Word(&'a str),
    Wildcard(char),
    EOF,
}

impl<'a> Lexer<'a> {
    pub fn new(data: &'a [u8]) -> Self { 
        Lexer { data }
    }

    pub fn next_token(&mut self) -> Token {
        match get_token(self.data) {
            IResult::Done(remaining, token) => {
                self.data = remaining;
                token
            }
            IResult::Incomplete(needed) => panic!("Still need {:?} bytes", needed),
            IResult::Error(e) => panic!("Error: {}", e),
        }
    }
}

named!(get_token<Token>, alt!(file_end | word | comment | integer | any));

named!(any<Token>,
       do_parse!(
           ch: ws!(anychar) >>
           (Token::Wildcard(ch))
       )
);

named!(word<Token>,
       do_parse!(
           word: map_res!(ws!(alpha), str::from_utf8) >>
           (Token::Word(word)) 
       )
);

named!(file_end<Token>,
       do_parse!(
           ws!(eof!()) >>
           (Token::EOF)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_end() {
        let source = b"  ";
        let result = file_end(source);
        assert!(result.is_done());
        assert_eq!(Token::EOF, result.unwrap().1);
    }

    #[test]
    fn test_parse_int() {
        let source = b" 457";
        let result = integer(source);
        assert!(result.is_done());
        assert_eq!(Token::Integer(457), result.unwrap().1);
    }

    #[test]
    fn test_parse_comment() {
        let source = b" // hello there!!\n";
        let result = comment(source);
        assert!(result.is_done());
        assert_eq!(Token::Comment, result.unwrap().1);
    }
}
