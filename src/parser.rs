use std::iter::Peekable;
use std::vec::Drain;

use ast::{Expr, StmtList, ValueType};
use errors::*;
use lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    ast: StmtList,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            pos: 0,
            ast: StmtList::new(),
        }
    }

    pub fn parse(&mut self) -> Result<StmtList> {
        let mut ast = StmtList::new();
        Ok(ast)
    }

    fn expression(&mut self) {}

    fn logical_expression(&mut self) -> Result<Expr> {}

    fn arithmetic_expression(&mut self) -> Result<Expr> {}

    fn identifier(&mut self) -> Result<Expr> {}

    fn function_call(&mut self) -> Result<Expr> {}

    fn builtin_type(&mut self) -> Result<Expr> {}
}

fn expect(value: &Token, expected: Token) -> Result<()> {
    if *value == expected {
        Ok(())
    } else {
        Err(
            format!(
                "Error while parsing. Expected {:?}, got {:?}",
                expected,
                value
            )
                    .into(),
        )
    }
}
