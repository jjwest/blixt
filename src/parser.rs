use std::mem;

use ast::{Assignment, ArithmeticOp, Expr, LogicOp, ParameterList, Stmt, StmtList, ValueType};
use errors::*;
use lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    ast: StmtList,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            ast: StmtList::new(),
            pos: 0,
        }
    }

    pub fn parse(&mut self) -> Result<StmtList> {
        let mut syntax_tree = StmtList::new();

        while self.pos < self.tokens.len() {
            syntax_tree.add_stmt(match self.statement() {
                Some(stmt) => stmt,
                None => return Err("Error while parsing".into()),
            });
        }
        Ok(syntax_tree)
    }

    fn next_token(&mut self) -> Token {
        let token = mem::replace(&mut self.tokens[self.pos], Token::Consumed);
        debug!("Consumed {:?}", token);
        self.pos += 1;
        token
    }

    fn peek(&mut self, len: usize) -> Option<&[Token]> {
        if self.pos + len < self.tokens.len() {
            Some(&self.tokens[self.pos..self.pos + len])
        } else {
            None
        }
    }

    fn statement(&mut self) -> Option<Stmt> {
        trace!("Entered statement");

        if let Some(assignment) = self.assignment() {
            debug!("assignment");
            return Some(Stmt::Assignment(assignment));
        }
        if let Some(expr) = self.expression() {
            debug!("expression");
            return Some(Stmt::Expr(*expr));
        }

        None
    }

    fn parameter_list(&mut self) -> Option<ParameterList> {
        let mut params = ParameterList::new();
        self.parameter(&mut params);
        if !params.is_empty() {
            Some(params)
        } else {
            None
        }
    }

    fn parameter(&mut self, list: &mut ParameterList) {
        if let Some(expr) = self.expression() {
            list.push(*expr);
            if self.tokens[0] == Token::Comma {
                self.next_token();
                self.parameter(list);
            }
        }
    }


    fn assignment(&mut self) -> Option<Assignment> {
        trace!("Entered assignment");

        match self.peek(2) {
            Some(&[Token::Ident(_), Token::Assign]) => {}
            _ => return None,
        };

        let ident = match self.next_token() {
            Token::Ident(ident) => ident,
            _ => return None,
        };

        // And the assign token
        self.next_token();

        let expr = self.expression().expect("Expected expr");
        let assignment = Assignment::new(ident, *expr);
        Some(assignment)
    }

    fn expression(&mut self) -> Option<Box<Expr>> {
        trace!("Entered expression");
        self.logical_expression()
    }

    fn logical_expression(&mut self) -> Option<Box<Expr>> {
        trace!("Entered logical_expression");

        if let Some(term) = self.term() {
            let operator = match self.peek(1) {
                Some(&[Token::Lesser]) => LogicOp::Lesser,
                Some(&[Token::Greater]) => LogicOp::Greater,
                Some(&[Token::GreaterEqual]) => LogicOp::GreaterEqual,
                Some(&[Token::LesserEqual]) => LogicOp::LesserEqual,
                Some(&[Token::Equal]) => LogicOp::Equal,
                _ => return Some(term),
            };

            // And the operator
            self.next_token();

            Some(Box::new(Expr::LogicalExpr {
                lhs: term,
                rhs: match self.expression() {
                    Some(expr) => expr,
                    None => return None,
                },
                operator,
            }))
        } else {
            None
        }
    }

    fn term(&mut self) -> Option<Box<Expr>> {
        trace!("Entered term");

        if let Some(term) = self.factor() {
            let operator = match self.peek(1) {
                Some(&[Token::Plus]) => ArithmeticOp::Add,
                Some(&[Token::Minus]) => ArithmeticOp::Sub,
                _ => return Some(term),
            };

            // And the operator
            self.next_token();

            Some(Box::new(Expr::ArithmeticExpr {
                lhs: term,
                rhs: match self.term() {
                    Some(expr) => expr,
                    None => return None,
                },
                operator,
            }))
        } else {
            None
        }

    }

    fn factor(&mut self) -> Option<Box<Expr>> {
        trace!("Entered factor");

        if let Some(factor) = self.atom() {
            let operator = match self.peek(1) {
                Some(&[Token::Asterisk]) => ArithmeticOp::Mult,
                Some(&[Token::Slash]) => ArithmeticOp::Div,
                Some(&[Token::Percent]) => ArithmeticOp::Mod,
                _ => return Some(factor),
            };

            // And the operator
            self.next_token();

            Some(Box::new(Expr::ArithmeticExpr {
                lhs: factor,
                rhs: match self.factor() {
                    Some(expr) => expr,
                    None => return None,
                },
                operator,
            }))
        } else {
            None
        }


    }

    fn atom(&mut self) -> Option<Box<Expr>> {
        trace!("Entered atom");

        match self.next_token() {
            Token::OpenParen => {
                let expr = self.expression();
                match self.next_token() {
                    Token::CloseParen => expr,
                    _ => None,
                }
            }
            Token::Ident(name) => Some(Box::new(Expr::Ident(name))),
            Token::Integer(value) => Some(Box::new(Expr::Value(ValueType::Int32(value)))),
            Token::Float(value) => Some(Box::new(Expr::Value(ValueType::Float32(value)))),
            Token::Bool(value) => Some(Box::new(Expr::Value(ValueType::Bool(value)))),
            Token::String(value) => Some(Box::new(Expr::Value(ValueType::String(value)))),
            _ => None,
        }
    }
}
