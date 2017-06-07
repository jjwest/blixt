use std::mem;

use ast::{ArithmeticOp, Expr, StmtList, ValueType};
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

    fn expression(&mut self, pos: usize) {}

    // fn logical_expression(&mut self, pos: usize) -> Result<Expr> {}

    fn factor(&mut self, pos: usize) -> Option<Box<Expr>> {
        let lhs = self.atom(pos);
        let op = self.tokens.get_mut(pos + 1);
        let rhs = self.factor(self.pos + 2);

        match (lhs, op, rhs) {
            (Some(lhs), Some(&mut Token::Asterisk), Some(rhs)) => {
                Some(
                    Box::new(
                        Expr::ArithmeticExpr {
                            lhs,
                            rhs,
                            operator: ArithmeticOp::Mult,
                        },
                    ),
                )
            }
            (Some(lhs), Some(&mut Token::Slash), Some(rhs)) => {
                Some(
                    Box::new(
                        Expr::ArithmeticExpr {
                            lhs,
                            rhs,
                            operator: ArithmeticOp::Div,
                        },
                    ),
                )
            }
            (Some(lhs), Some(&mut Token::Percent), Some(rhs)) => {
                Some(
                    Box::new(
                        Expr::ArithmeticExpr {
                            lhs,
                            rhs,
                            operator: ArithmeticOp::Mod,
                        },
                    ),
                )
            }
            _ => None,
        }
    }

    fn atom(&mut self, pos: usize) -> Option<Box<Expr>> {
        if let Some(expr) = self.builtin_type(pos) {
            return Some(expr);
        }
        if let Some(expr) = self.identifier(pos) {
            return Some(expr);
        }

        None
    }

    fn identifier(&mut self, pos: usize) -> Option<Box<Expr>> {
        match self.tokens.get_mut(pos) {
            Some(&mut Token::Ident(name)) => Some(Box::new(Expr::Ident(name))),
            _ => None,
        }
    }

    // fn function_call(&mut self) -> Result<Expr> {}

    fn builtin_type(&mut self, pos: usize) -> Option<Box<Expr>> {
        match self.tokens.get_mut(pos) {            
            Some(&mut Token::Integer(value)) => Some(Box::new(Expr::Value(ValueType::Int32(value))),),
            _ => None,
        }
    }

    fn take_token(&mut self, offset: usize) -> Token {
        mem::replace(&mut self.tokens[self.pos + offset], Token::Consumed)
    }
}

fn expect(value: &Token, expected: &Token) -> Result<()> {
    if *value == *expected {
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
