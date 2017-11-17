use std::mem;
use std::rc::Rc;

use ast::{AbstractSyntaxTree, ArgumentList, Assignment, ArithmeticOp, Expr, LogicOp, Parameter,
          ParameterList, Stmt, StmtList};
use builtins::{Value, ValueKind};
use errors::*;
use lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    pub fn parse(&mut self) -> Result<AbstractSyntaxTree> {
        let mut syntax_tree = AbstractSyntaxTree::new();

        while self.pos < self.tokens.len() {
            syntax_tree.add_stmt(match self.statement()? {
                Some(stmt) => stmt,
                None => return Err(err_msg("Error while parsing")),
            });
        }
        Ok(syntax_tree)
    }

    fn eat_token(&mut self) -> Token {
        if self.pos >= self.tokens.len() {
            panic!("Called eat_token when no tokens are left")
        }

        let token = mem::replace(&mut self.tokens[self.pos], Token::Consumed);
        debug!("Consumed {:?}", token);
        self.pos += 1;
        token
    }

    fn peek(&mut self, len: usize) -> Option<&[Token]> {
        if self.pos + len <= self.tokens.len() {
            Some(&self.tokens[self.pos..self.pos + len])
        } else {
            None
        }
    }

    fn statement_list(&mut self) -> Result<Option<StmtList>> {
        let mut stmt_list = StmtList::new();
        while let Some(stmt) = self.statement()? {
            stmt_list.0.push(stmt);
        }

        Ok(Some(stmt_list))
    }

    fn statement(&mut self) -> Result<Option<Stmt>> {
        trace!("Entered statement");

        if let Some(assignment) = self.assignment()? {
            return Ok(Some(Stmt::Assignment(assignment)));
        }
        if let Some(expr) = self.expression()? {
            return Ok(Some(Stmt::Expr(*expr)));
        }

        Ok(None)
    }

    fn assignment(&mut self) -> Result<Option<Assignment>> {
        trace!("Entered assignment");

        match self.peek(2) {
            Some(&[Token::Ident(_), Token::Assign]) |
            Some(&[Token::Ident(_), Token::Initialize]) |
            Some(&[Token::Ident(_), Token::AddAssign]) |
            Some(&[Token::Ident(_), Token::SubAssign]) |
            Some(&[Token::Ident(_), Token::DivAssign]) |
            Some(&[Token::Ident(_), Token::MultAssign]) |
            Some(&[Token::Ident(_), Token::Colon]) => {}
            _ => return Ok(None),
        }

        let ident = match self.eat_token() {
            Token::Ident(ident) => ident,
            other => expected!("identifier", other),
        };

        let variable_type = match self.eat_token() {
            Token::Colon => {
                let type_ = match self.eat_token() {
                    Token::BoolType => ValueKind::Bool,
                    Token::IntType => ValueKind::Int,
                    Token::FloatType => ValueKind::Float,
                    Token::StringType => ValueKind::String,
                    other => return Err(format_err!("Expected value type, found '{:?}'", other)),
                };

                match self.eat_token() {
                    Token::Assign => {}
                    other => return Err(format_err!("Expected Token::Assign, found {:?}", other)),
                }

                type_
            }
            Token::Assign | Token::Initialize => {
                match self.peek(1) {
                    Some(&[Token::Bool(_)]) => ValueKind::Bool,
                    Some(&[Token::Integer(_)]) => ValueKind::Int,
                    Some(&[Token::Float(_)]) => ValueKind::Float,
                    Some(&[Token::String(_)]) => ValueKind::String,
                    other => return Err(format_err!("Could not infer type, found '{:?}'", other)),
                }
            }
            other => {
                return Err(format_err!(
                    "Expected type or assignment token, found '{:?}'",
                    other
                ))
            }
        };

        let value = match self.expression()? {
            Some(expr) => *expr,
            None => expected!("expression"),
        };

        let assignment = Assignment::new(ident, variable_type, value);
        debug!("Assignment: {:?}", assignment);
        Ok(Some(assignment))
    }

    fn parameter_list(&mut self) -> Result<ParameterList> {
        trace!("Entered parameter_list");
        let mut params = ParameterList::new();

        while let Some(&[Token::Ident(_)]) = self.peek(1) {
            let ident = match self.eat_token() {
                Token::Ident(ident) => ident,
                _ => unreachable!(),
            };

            match self.eat_token() {
                Token::Assign => {}
                other => return Err(format_err!("Expected Token::Colon, found {:?}", other)),
            }

            let type_ = match self.eat_token() {
                Token::BoolType => ValueKind::Bool,
                Token::FloatType => ValueKind::Float,
                Token::IntType => ValueKind::Int,
                Token::StringType => ValueKind::String,
                other => expected!("type", other),
            };

            if let Some(&[Token::Comma]) = self.peek(1) {
                self.eat_token();
            }


            params.push(Parameter {
                name: ident,
                kind: type_,
            });
        }

        Ok(params)
    }

    fn expression(&mut self) -> Result<Option<Box<Expr>>> {
        trace!("Entered expression");
        let expr = self.logical_expression();
        debug!("Expr: {:?}", expr);
        expr
    }

    fn logical_expression(&mut self) -> Result<Option<Box<Expr>>> {
        trace!("Entered logical_expression");

        if let Some(term) = self.term()? {
            let operator = match self.peek(1) {
                Some(&[Token::Lesser]) => LogicOp::Lesser,
                Some(&[Token::Greater]) => LogicOp::Greater,
                Some(&[Token::GreaterEqual]) => LogicOp::GreaterEqual,
                Some(&[Token::LesserEqual]) => LogicOp::LesserEqual,
                Some(&[Token::Equal]) => LogicOp::Equal,
                _ => return Ok(Some(term)),
            };

            // And the operator
            self.eat_token();

            Ok(Some(Box::new(Expr::LogicalExpr {
                lhs: term,
                rhs: match self.expression()? {
                    Some(expr) => expr,
                    None => {
                        return Err(format_err!(
                            "Expected expression, found '{:?}'",
                            self.eat_token()
                        ))
                    }
                },
                operator,
            })))
        } else {
            Ok(None)
        }
    }

    fn term(&mut self) -> Result<Option<Box<Expr>>> {
        trace!("Entered term");

        if let Some(term) = self.factor()? {
            let operator = match self.peek(1) {
                Some(&[Token::Plus]) => ArithmeticOp::Add,
                Some(&[Token::Minus]) => ArithmeticOp::Sub,
                _ => return Ok(Some(term)),
            };

            // And the operator
            self.eat_token();

            Ok(Some(Box::new(Expr::ArithmeticExpr {
                lhs: term,
                rhs: match self.term()? {
                    Some(expr) => expr,
                    None => {
                        return Err(format_err!(
                            "Expected expression, found '{:?}'",
                            self.eat_token()
                        ))
                    }
                },
                operator,
            })))
        } else {
            Ok(None)
        }

    }

    fn factor(&mut self) -> Result<Option<Box<Expr>>> {
        trace!("Entered factor");

        if let Some(factor) = self.atom()? {
            let operator = match self.peek(1) {
                Some(&[Token::Asterisk]) => ArithmeticOp::Mult,
                Some(&[Token::Slash]) => ArithmeticOp::Div,
                Some(&[Token::Percent]) => ArithmeticOp::Mod,
                _ => return Ok(Some(factor)),
            };

            // And the operator
            self.eat_token();

            Ok(Some(Box::new(Expr::ArithmeticExpr {
                lhs: factor,
                rhs: match self.factor()? {
                    Some(expr) => expr,
                    None => {
                        return Err(format_err!(
                            "Expected expression, found '{:?}'",
                            self.eat_token()
                        ))
                    }
                },
                operator,
            })))
        } else {
            Ok(None)
        }
    }

    fn argument_list(&mut self) -> Result<ArgumentList> {
        trace!("Entered argument_list");
        let mut list = ArgumentList::new();

        while let Some(expr) = self.expression()? {
            list.push(*expr);
            if let Some(&[Token::Comma]) = self.peek(1) {
                self.eat_token();
            }
        }

        Ok(list)
    }

    fn atom(&mut self) -> Result<Option<Box<Expr>>> {
        trace!("Entered atom");

        match self.peek(1) {
            Some(&[Token::OpenParen]) => {
                self.eat_token();
                let expr = self.expression()?;
                match self.eat_token() {
                    Token::CloseParen => Ok(expr),
                    token => Err(format_err!("Expected ')', found '{:?}'", token)),

                }
            }
            Some(&[Token::Ident(_)]) => {
                match self.eat_token() {
                    Token::Ident(name) => Ok(Some(Box::new(Expr::Ident(name)))),
                    _ => unreachable!(),
                }
            }
            Some(&[Token::Integer(value)]) => {
                self.eat_token();
                Ok(Some(Box::new(Expr::Value(Value::Int32(value)))))
            }
            Some(&[Token::Float(value)]) => {
                self.eat_token();
                Ok(Some(Box::new(Expr::Value(Value::Float32(value)))))
            }
            Some(&[Token::Bool(value)]) => {
                self.eat_token();
                Ok(Some(Box::new(Expr::Value(Value::Bool(value)))))
            }
            Some(&[Token::String(_)]) => {
                match self.eat_token() {
                    Token::String(value) => Ok(Some(
                        Box::new(Expr::Value(Value::String(Rc::new(value)))),
                    )),
                    _ => unreachable!(),
                }
            }
            _ => Ok(None),
        }
    }
}
