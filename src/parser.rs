use std::mem;
use std::rc::Rc;

use ast::{AbstractSyntaxTree, ArgumentList, Assignment, ArithmeticOp, Expr, FunctionDeclaration,
          FunctionCall, LogicOp, Parameter, ParameterList, Stmt, StmtList};
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
                None => return Err("Error while parsing".into()),
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

        if let Some(function) = self.function_declaration()? {
            return Ok(Some(Stmt::FunctionDeclaration(function)));
        }

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
                    other => return Err(format!("Expected value type, found '{:?}'", other).into()),
                };
                expect_next!(self, eat_token, Token::Assign);
                type_
            }
            Token::Assign | Token::Initialize => {
                match self.peek(1) {
                    Some(&[Token::Bool(_)]) => ValueKind::Bool,
                    Some(&[Token::Integer(_)]) => ValueKind::Int,
                    Some(&[Token::Float(_)]) => ValueKind::Float,
                    Some(&[Token::String(_)]) => ValueKind::String,
                    other => {
                        return Err(format!("Could not infer type, found '{:?}'", other).into())
                    }
                }
            }
            other => {
                return Err(
                    format!("Expected type or assignment token, found '{:?}'", other).into(),
                )
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

            expect_next!(self, eat_token, Token::Colon);

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

            params.push(Parameter::new(ident, type_));
        }

        Ok(params)
    }

    fn function_declaration(&mut self) -> Result<Option<FunctionDeclaration>> {
        trace!("Entered function_declaration");

        match self.peek(1) {
            Some(&[Token::FunctionDeclaration]) => {}
            _ => return Ok(None),
        }

        expect_next!(self, eat_token, Token::FunctionDeclaration);

        let ident = match self.eat_token() {
            Token::Ident(ident) => ident,
            other => expected!("Identifier", other),
        };

        expect_next!(self, eat_token, Token::OpenParen);
        let params = self.parameter_list()?;
        expect_next!(self, eat_token, Token::CloseParen);

        let return_type = if let Some(&[Token::ReturnDeclaration]) = self.peek(1) {
            self.eat_token();
            let type_ = match self.eat_token() {
                Token::BoolType => ValueKind::Bool,
                Token::FloatType => ValueKind::Float,
                Token::IntType => ValueKind::Int,
                Token::StringType => ValueKind::String,
                other => expected!("Value type", other),
            };
            Some(type_)
        } else {
            None
        };

        expect_next!(self, eat_token, Token::OpenBrace);

        let body = self.statement_list()?.expect("Expected function body");

        expect_next!(self, eat_token, Token::CloseBrace);

        let function = FunctionDeclaration::new(ident, body, params, return_type);
        debug!("FunctionDeclaration: {:#?}", function);
        Ok(Some(function))
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
                        return Err(
                            format!("Expected expression, found '{:?}'", self.eat_token()).into(),
                        )
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
                        return Err(
                            format!("Expected expression, found '{:?}'", self.eat_token()).into(),
                        )
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
                        return Err(
                            format!("Expected expression, found '{:?}'", self.eat_token()).into(),
                        )
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

    fn function_call(&mut self) -> Result<Option<Box<Expr>>> {
        trace!("Entered function_call");

        if let Some(&[Token::Ident(_), Token::OpenParen]) = self.peek(2) {
            let ident = match self.eat_token() {
                Token::Ident(ident) => ident,
                other => expected!("Identifier", other),
            };

            expect_next!(self, eat_token, Token::OpenParen);
            let args = self.argument_list()?;
            expect_next!(self, eat_token, Token::CloseParen);

            let call = Box::new(Expr::FunctionCall(FunctionCall::new(ident, args)));
            debug!("FunctionCall: {:#?}", call);

            Ok(Some(call))

        } else {
            Ok(None)
        }
    }

    fn atom(&mut self) -> Result<Option<Box<Expr>>> {
        trace!("Entered atom");

        if let Some(call) = self.function_call()? {
            return Ok(Some(call));
        }

        match self.peek(1) {
            Some(&[Token::OpenParen]) => {
                self.eat_token();
                let expr = self.expression()?;
                match self.eat_token() {
                    Token::CloseParen => Ok(expr),
                    token => Err(format!("Expected ')', found '{:?}'", token).into()),

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
