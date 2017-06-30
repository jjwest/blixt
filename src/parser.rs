use std::mem;

use ast::{Assignment, ArithmeticOp, Expr, LogicOp, ParameterList, Print, Stmt, StmtList, ValueType};
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

        syntax_tree.add_stmt(match self.statement() {
            Some(stmt) => stmt,
            None => return Err("Error while parsing".into()),
        });

        Ok(syntax_tree)
    }

    fn next_token(&mut self) -> Token {
        let token = mem::replace(&mut self.tokens[self.pos], Token::Consumed);
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

    // fn print(&mut self) -> Option<Print> {
    //     if self.tokens[0] == Token::Print && self.tokens[1] == Token::OpenParen {

    //     } else {
    //         None
    //     }
    // }

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

        if let Some(logical_expr) = self.logical_expression() {
            let operator = match self.tokens[self.pos] {
                Token::Lesser => Some(LogicOp::Lesser),
                Token::Greater => Some(LogicOp::Greater),
                Token::GreaterEqual => Some(LogicOp::GreaterEqual),
                Token::LesserEqual => Some(LogicOp::LesserEqual),
                Token::Equal => Some(LogicOp::Equal),
                _ => None,
            };

            if let Some(operator) = operator {
                // We peeked a valid operator earlier, so we remove it
                self.pos += 1;
                Some(Box::new(Expr::LogicalExpr {
                    lhs: logical_expr,
                    rhs: match self.expression() {
                        Some(expr) => expr,
                        None => return None,
                    },
                    operator,
                }))
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
            let operator = match self.tokens.get(0) {
                Some(&Token::Lesser) => Some(LogicOp::Lesser),
                Some(&Token::Greater) => Some(LogicOp::Greater),
                Some(&Token::GreaterEqual) => Some(LogicOp::GreaterEqual),
                Some(&Token::LesserEqual) => Some(LogicOp::LesserEqual),
                Some(&Token::Equal) => Some(LogicOp::Equal),
                _ => None,
            };

            if let Some(operator) = operator {
                // We peeked a valid operator earlier, so we remove it
                self.pos += 1;
                Some(Box::new(Expr::LogicalExpr {
                    lhs: term,
                    rhs: match self.expression() {
                        Some(expr) => expr,
                        None => return None,
                    },
                    operator,
                }))
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
            let operator = match self.tokens.get(0) {
                Some(&Token::Plus) => Some(ArithmeticOp::Add),
                Some(&Token::Minus) => Some(ArithmeticOp::Sub),
                _ => None,
            };

            if let Some(operator) = operator {
                // We peeked a valid operator earlier, so we remove it
                self.pos += 1;
                Some(Box::new(Expr::ArithmeticExpr {
                    lhs: term,
                    rhs: match self.term() {
                        Some(expr) => expr,
                        None => return None,
                    },
                    operator,
                }))
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
            let operator = match self.tokens.get(0) {
                Some(&Token::Asterisk) => Some(ArithmeticOp::Mult),
                Some(&Token::Slash) => Some(ArithmeticOp::Div),
                Some(&Token::Percent) => Some(ArithmeticOp::Mod),
                _ => None,
            };

            if let Some(operator) = operator {
                // We peeked a valid operator earlier, so we remove it
                self.pos += 1;
                Some(Box::new(Expr::ArithmeticExpr {
                    lhs: factor,
                    rhs: match self.factor() {
                        Some(expr) => expr,
                        None => return None,
                    },
                    operator,
                }))
            } else {
                Some(factor)
            }
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
