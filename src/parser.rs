use std::iter::Peekable;
use std::mem;
use std::vec::Drain;

use ast::{ArithmeticOp, Expr, LogicOp, Stmt, StmtList, ValueType};
use errors::*;
use lexer::Token;

pub struct Parser<'a> {
    tokens: Peekable<Drain<'a, Token>>,
    ast: StmtList,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a mut Vec<Token>) -> Self {
        Parser {
            tokens: tokens.drain(..).peekable(),
            ast: StmtList::new(),
        }
    }

    pub fn parse(&mut self) -> Result<StmtList> {
        let mut ast = StmtList::new();
        match self.expression() {
            Some(expr) => ast.add_stmt(Stmt::Expr(*expr)),
            None => return Err("Failed to parsing".into()),
        }
        Ok(ast)
    }

    fn expression(&mut self) -> Option<Box<Expr>> {
        trace!("Entered expression");

        if let Some(logical_expr) = self.logical_expression() {
            let op = match self.tokens.peek() {
                Some(&Token::Lesser) => Some(LogicOp::Lesser),
                Some(&Token::Greater) => Some(LogicOp::Greater),
                Some(&Token::GreaterEqual) => Some(LogicOp::GreaterEqual),
                Some(&Token::LesserEqual) => Some(LogicOp::LesserEqual),
                Some(&Token::Equal) => Some(LogicOp::Equal),
                _ => None,
            };

            if let Some(op) = op {
                let _ = self.tokens.next();
                Some(
                    Box::new(
                        Expr::LogicalExpr {
                            lhs: logical_expr,
                            rhs: match self.expression() {
                                Some(expr) => expr,
                                None => return None,
                            },
                            operator: op,
                        },
                    ),
                )
            } else {
                Some(logical_expr)
            }
        } else {
            None
        }
    }

    fn logical_expression(&mut self) -> Option<Box<Expr>> {
        trace!("Entered logical_expression");

        if let Some(term) = self.term() {
            let op = match self.tokens.peek() {
                Some(&Token::Lesser) => Some(LogicOp::Lesser),
                Some(&Token::Greater) => Some(LogicOp::Greater),
                Some(&Token::GreaterEqual) => Some(LogicOp::GreaterEqual),
                Some(&Token::LesserEqual) => Some(LogicOp::LesserEqual),
                Some(&Token::Equal) => Some(LogicOp::Equal),
                _ => None,
            };

            if let Some(op) = op {
                let _ = self.tokens.next();
                Some(
                    Box::new(
                        Expr::LogicalExpr {
                            lhs: term,
                            rhs: match self.expression() {
                                Some(expr) => expr,
                                None => return None,
                            },
                            operator: op,
                        },
                    ),
                )
            } else {
                Some(term)
            }
        } else {
            None
        }
    }

    fn term(&mut self) -> Option<Box<Expr>> {
        trace!("Entered term");

        if let Some(term) = self.factor() {
            let op = match self.tokens.peek() {
                Some(&Token::Plus) => Some(ArithmeticOp::Add),
                Some(&Token::Minus) => Some(ArithmeticOp::Sub),
                _ => None,
            };

            if let Some(op) = op {
                let _ = self.tokens.next();
                Some(
                    Box::new(
                        Expr::ArithmeticExpr {
                            lhs: term,
                            rhs: match self.term() {
                                Some(expr) => expr,
                                None => return None,
                            },
                            operator: op,
                        },
                    ),
                )
            } else {
                Some(term)
            }
        } else {
            None
        }

    }

    fn factor(&mut self) -> Option<Box<Expr>> {
        trace!("Entered factor");

        if let Some(factor) = self.atom() {
            let op = match self.tokens.peek() {
                Some(&Token::Asterisk) => Some(ArithmeticOp::Mult),
                Some(&Token::Slash) => Some(ArithmeticOp::Div),
                Some(&Token::Percent) => Some(ArithmeticOp::Mod),
                _ => None,
            };

            if let Some(op) = op {
                let _ = self.tokens.next();
                Some(
                    Box::new(
                        Expr::ArithmeticExpr {
                            lhs: factor,
                            rhs: match self.factor() {
                                Some(expr) => expr,
                                None => return None,
                            },
                            operator: op,
                        },
                    ),
                )
            } else {
                Some(factor)
            }
        } else {
            None
        }


    }

    fn atom(&mut self) -> Option<Box<Expr>> {
        trace!("Entered atom");

        match self.tokens.next() {
            Some(Token::OpenParen) => {
                let expr = self.expression();
                match self.tokens.next() {
                    Some(Token::CloseParen) => expr,
                    _ => None,
                }
            }
            Some(Token::Ident(name)) => Some(Box::new(Expr::Ident(name))),
            Some(Token::Integer(value)) => Some(Box::new(Expr::Value(ValueType::Int32(value)))),
            Some(Token::Float(value)) => Some(Box::new(Expr::Value(ValueType::Float32(value)))),
            _ => None,
        }
    }
}
