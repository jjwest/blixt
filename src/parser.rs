use std::mem;

use ast::{Ast, Assignment, ArithmeticOp, Expr, Function, LogicOp, ParameterList, Stmt, ValueType};
use errors::*;
use lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

enum Parsed<T> {
    Some(T),
    None,
    Err(Error),
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    pub fn parse(&mut self) -> Result<Ast> {
        let mut syntax_tree = Ast::new();

        while self.pos < self.tokens.len() {
            syntax_tree.add_stmt(match self.statement() {
                Parsed::Some(stmt) => stmt,
                Parsed::None => return Err("Error while parsing".into()),
                Parsed::Err(e) => return Err(e),
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

    fn statement(&mut self) -> Parsed<Stmt> {
        trace!("Entered statement");

        if let Parsed::Some(assignment) = self.assignment() {
            debug!("assignment");
            return Parsed::Some(Stmt::Assignment(assignment));
        }
        if let Parsed::Some(expr) = self.expression() {
            debug!("expression");
            return Parsed::Some(Stmt::Expr(*expr));
        }

        Parsed::None
    }

    // fn function_declaration(&mut self) -> Parsed<Function> {
    //     if self.peek(1) != Some(&[Token::FunctionDeclaration]) {
    //         return Parsed::None;
    //     } else {
    //         self.next_token();
    //     }

    //     let ident = match self.expression() {
    //         Parsed::Some(expr) => expr,
    //         Parsed::None => return Parsed::None,
    //     };

    // }

    fn parameter_list(&mut self) -> Parsed<ParameterList> {
        let mut params = ParameterList::new();
        self.parameter(&mut params);
        if !params.is_empty() {
            Parsed::Some(params)
        } else {
            Parsed::None
        }
    }

    fn parameter(&mut self, list: &mut ParameterList) {
        if let Parsed::Some(expr) = self.expression() {
            list.push(*expr);
            if self.tokens[0] == Token::Comma {
                self.next_token();
                self.parameter(list);
            }
        }
    }


    fn assignment(&mut self) -> Parsed<Assignment> {
        trace!("Entered assignment");

        match self.peek(2) {
            Some(&[Token::Ident(_), Token::Assign]) => {}
            _ => return Parsed::None,
        };

        let ident = match self.next_token() {
            Token::Ident(ident) => ident,
            _ => unreachable!(),
        };

        // And the assign token
        self.next_token();

        let expr = match self.expression() {
            Parsed::Some(expr) => expr,
            Parsed::None => return Parsed::Err("Expected expression".into()),
            Parsed::Err(e) => return Parsed::Err(e),
        };

        let assignment = Assignment::new(ident, *expr);
        Parsed::Some(assignment)
    }

    fn expression(&mut self) -> Parsed<Box<Expr>> {
        trace!("Entered expression");
        self.logical_expression()
    }

    fn logical_expression(&mut self) -> Parsed<Box<Expr>> {
        trace!("Entered logical_expression");

        if let Parsed::Some(term) = self.term() {
            let operator = match self.peek(1) {
                Some(&[Token::Lesser]) => LogicOp::Lesser,
                Some(&[Token::Greater]) => LogicOp::Greater,
                Some(&[Token::GreaterEqual]) => LogicOp::GreaterEqual,
                Some(&[Token::LesserEqual]) => LogicOp::LesserEqual,
                Some(&[Token::Equal]) => LogicOp::Equal,
                _ => return Parsed::Some(term),
            };

            // And the operator
            self.next_token();

            Parsed::Some(Box::new(Expr::LogicalExpr {
                lhs: term,
                rhs: match self.expression() {
                    Parsed::Some(expr) => expr,
                    Parsed::None => return Parsed::None,
                    Parsed::Err(e) => return Parsed::Err(e),
                },
                operator,
            }))
        } else {
            Parsed::None
        }
    }

    fn term(&mut self) -> Parsed<Box<Expr>> {
        trace!("Entered term");

        if let Parsed::Some(term) = self.factor() {
            let operator = match self.peek(1) {
                Some(&[Token::Plus]) => ArithmeticOp::Add,
                Some(&[Token::Minus]) => ArithmeticOp::Sub,
                _ => return Parsed::Some(term),
            };

            // And the operator
            self.next_token();

            Parsed::Some(Box::new(Expr::ArithmeticExpr {
                lhs: term,
                rhs: match self.term() {
                    Parsed::Some(expr) => expr,
                    Parsed::None => return Parsed::None,
                    Parsed::Err(e) => return Parsed::Err(e),
                },
                operator,
            }))
        } else {
            Parsed::None
        }

    }

    fn factor(&mut self) -> Parsed<Box<Expr>> {
        trace!("Entered factor");

        if let Parsed::Some(factor) = self.atom() {
            let operator = match self.peek(1) {
                Some(&[Token::Asterisk]) => ArithmeticOp::Mult,
                Some(&[Token::Slash]) => ArithmeticOp::Div,
                Some(&[Token::Percent]) => ArithmeticOp::Mod,
                _ => return Parsed::Some(factor),
            };

            // And the operator
            self.next_token();

            Parsed::Some(Box::new(Expr::ArithmeticExpr {
                lhs: factor,
                rhs: match self.factor() {
                    Parsed::Some(expr) => expr,
                    Parsed::None => return Parsed::None,
                    Parsed::Err(e) => return Parsed::Err(e),
                },
                operator,
            }))
        } else {
            Parsed::None
        }


    }

    fn atom(&mut self) -> Parsed<Box<Expr>> {
        trace!("Entered atom");

        if let Parsed::Some(function_call) = self.function_call() {
            return Parsed::Some(function_call);
        }

        match self.next_token() {
            Token::OpenParen => {
                let expr = match self.expression() {
                    Parsed::Some(expr) => expr,
                    Parsed::None => {
                        return Parsed::Err(
                            format!("Expected expression, found {:?}", self.tokens[self.pos])
                                .into(),
                        )
                    }
                    Parsed::Err(e) => return Parsed::Err(e),
                };
                match self.next_token() {
                    Token::CloseParen => Parsed::Some(expr),
                    other => Parsed::Err(format!("Expected ')', found '{:?}'", other).into()),
                }
            }
            Token::Ident(name) => Parsed::Some(Box::new(Expr::Ident(name))),
            Token::Integer(value) => Parsed::Some(Box::new(Expr::Value(ValueType::Int32(value)))),
            Token::Float(value) => Parsed::Some(Box::new(Expr::Value(ValueType::Float32(value)))),
            Token::Bool(value) => Parsed::Some(Box::new(Expr::Value(ValueType::Bool(value)))),
            Token::String(value) => Parsed::Some(Box::new(Expr::Value(ValueType::String(value)))),
            _ => Parsed::None,
        }
    }

    fn function_call(&mut self) -> Parsed<Box<Expr>> {
        trace!("Entered function_call");

        match self.peek(2) {
            Some(&[Token::Ident(_), Token::OpenParen]) => {}
            _ => return Parsed::None,
        }

        let ident = match self.next_token() {
            Token::Ident(ident) => ident,
            _ => unreachable!(),
        };

        self.next_token(); // Remove the opening paren

        let args = match self.parameter_list() {
            Parsed::Some(params) => params,
            Parsed::None => return Parsed::None,
            Parsed::Err(e) => return Parsed::Err(e),
        };

        self.next_token(); // Remove closing paren

        Parsed::Some(Box::new(Expr::FunctionCall(ident, args)))
    }
}
