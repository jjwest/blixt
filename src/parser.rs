use std::str;
use std::io;

use nom::{self, alphanumeric, not_line_ending};

use errors::*;

#[derive(Default)]
pub struct Parser;

impl Parser {
    pub fn new() -> Self {
        Parser
    }

    pub fn parse(source: &[u8]) -> Result<i64> {
        if source.len() <= 1 {
            Err(io::Error::new(io::ErrorKind::InvalidInput, "File is empty".to_owned()),)
                .chain_err(|| "Failed to parse the file")
        } else {
            expr(source)
                .to_result()
                .chain_err(|| "Failed to parse the file")
        }
    }
}

pub enum AST {
    StmtList,
    Stmt,
    Return,
    Assign,
    Function,
    If,
    Loop,
    Io,
    Expr,
}

named!(comment, preceded!(ws!(tag!("//")), not_line_ending));



named!(digit <i64>, map_res!(map_res!(ws!(nom::digit), str::from_utf8), str::parse));
// named!(word, ws!(alphanumeric));

named!(parens <i64>, ws!(delimited!(tag!("("), expr, tag!(")"))));

named!(term <i64>, do_parse!(
    init: digit >>
    res: alt!(
        fold_many0!(
            pair!( alt!(tag!("*") | tag!("/")), digit ),
            init, |acc, (op, val): (&[u8], i64)| {
                if op[0] as char == '*' { acc * val } else { acc / val }
            }
        )
        | parens
    ) >>
    (res)
));

named!(expr <i64>, do_parse!(
    init: term >>
    res: fold_many0!(
        pair!(alt!(tag!("+") | tag!("-")), term),
        init, |acc, (op, val): (&[u8], i64)| {
            if op[0] as char == '+' { acc + val } else { acc - val }
        }
    ) >>
    (res)
));

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr() {
        let (_, result) = expr(b"10 + 2 * 3").unwrap();
        assert_eq!(16, result);
    }

    #[test]
    fn test_arith_expr() {
        let (_, result) = term(b"10 * 2").unwrap();
        assert_eq!(20, result);

        let (_, result) = term(b"10 * 4 / 2").unwrap();
        assert_eq!(20, result);
    }

    #[test]
    fn parse_comment() {
        let source = b"// This is a comment\nfn test_things()";
        let result = comment(source);
        let (_, comment) = result.unwrap();
        assert_eq!(b"This is a comment", comment);
    }
}
