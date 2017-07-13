use std::mem;

use ast::{Ast, Assignment, ArithmeticOp, Expr, Function, LogicOp, Parameter, ParameterList, Stmt,
          StmtList};
use builtins::{Value, ValueKind};
use errors::*;
use lexer::Token;

macro_rules! expect_next {
    ( $context:ident, $func:ident, $expected:expr ) => {
        match $context.$func() {
            $expected => {},
            other => return Err(format!("Expected '{:?}', found '{:?}'", $expected, other).into()),
        }
    }
}

macro_rules! expected {
    ( $expected:expr, $got:expr ) => {
        return Err(format!("Expected '{:?}', found '{:?}'", $expected, $got).into())
    };

    ( $expected:expr ) => {
        return Err(format!("Expected '{:?}'", $expected).into())
    }
}

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

    pub fn parse(&mut self) -> Result<Ast> {
        let mut syntax_tree = Ast::new();

        while self.pos < self.tokens.len() {
            syntax_tree.add_stmt(match self.statement()? {
                Some(stmt) => stmt,
                None => return Err("Error while parsing".into()),
            });
        }
        Ok(syntax_tree)
    }

    fn next_token(&mut self) -> Token {
        if self.pos >= self.tokens.len() {
            panic!("Called next_token when no tokens are left")
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
            return Ok(Some(Stmt::Function(function)));
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

        let ident = match self.next_token() {
            Token::Ident(ident) => ident,
            other => expected!("identifier", other),
        };

        let variable_type = match self.next_token() {
            Token::Colon => {
                let type_ = match self.next_token() {
                    Token::BoolType => ValueKind::Bool,
                    Token::IntType => ValueKind::Int,
                    Token::FloatType => ValueKind::Float,
                    Token::StringType => ValueKind::String,
                    other => return Err(format!("Expected value type, found '{:?}'", other).into()),
                };
                expect_next!(self, next_token, Token::Assign);
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

    fn parameter_list(&mut self) -> Result<Option<ParameterList>> {
        trace!("Entered parameter_list");

        let mut params = ParameterList::new();
        loop {
            match self.peek(1) {
                Some(&[Token::Ident(_)]) => {}
                _ => return Ok(Some(params)),
            }

            let ident = match self.next_token() {
                Token::Ident(ident) => ident,
                other => expected!("identifier", other),
            };

            expect_next!(self, next_token, Token::Colon);

            let type_ = match self.next_token() {
                Token::BoolType => ValueKind::Bool,
                Token::FloatType => ValueKind::Float,
                Token::IntType => ValueKind::Int,
                Token::StringType => ValueKind::String,
                other => expected!("type", other),
            };

            if let Some(&[Token::Comma]) = self.peek(1) {
                self.next_token();
            }

            params.push(Parameter::new(ident, type_));
        }
    }

    fn function_declaration(&mut self) -> Result<Option<Function>> {
        trace!("Entered function_declaration");

        match self.peek(1) {
            Some(&[Token::FunctionDeclaration]) => {}
            _ => return Ok(None),
        }

        expect_next!(self, next_token, Token::FunctionDeclaration);

        let ident = match self.next_token() {
            Token::Ident(ident) => ident,
            other => expected!("Identifier", other),
        };

        expect_next!(self, next_token, Token::OpenParen);
        let params = self.parameter_list()?.expect("Expected params");
        expect_next!(self, next_token, Token::CloseParen);

        let return_type = if let Some(&[Token::ReturnDeclaration]) = self.peek(1) {
            self.next_token();
            let type_ = match self.next_token() {
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

        expect_next!(self, next_token, Token::OpenBrace);

        let body = self.statement_list()?.expect("Expected function body");

        expect_next!(self, next_token, Token::CloseBrace);

        let function = Function::new(ident, body, params, return_type);
        debug!("Function: {:#?}", function);
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
            self.next_token();

            Ok(Some(Box::new(Expr::LogicalExpr {
                lhs: term,
                rhs: match self.expression()? {
                    Some(expr) => expr,
                    None => {
                        return Err(
                            format!("Expected expression, found '{:?}'", self.next_token())
                                .into(),
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
            self.next_token();

            Ok(Some(Box::new(Expr::ArithmeticExpr {
                lhs: term,
                rhs: match self.term()? {
                    Some(expr) => expr,
                    None => {
                        return Err(
                            format!("Expected expression, found '{:?}'", self.next_token())
                                .into(),
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
            self.next_token();

            Ok(Some(Box::new(Expr::ArithmeticExpr {
                lhs: factor,
                rhs: match self.factor()? {
                    Some(expr) => expr,
                    None => {
                        return Err(
                            format!("Expected expression, found '{:?}'", self.next_token())
                                .into(),
                        )
                    }
                },
                operator,
            })))
        } else {
            Ok(None)
        }


    }

    fn atom(&mut self) -> Result<Option<Box<Expr>>> {
        trace!("Entered atom");

        if let Some(&[Token::CloseBrace]) = self.peek(1) {
            return Ok(None);
        }

        match self.next_token() {
            Token::OpenParen => {
                let expr = self.expression()?;
                match self.next_token() {
                    Token::CloseParen => Ok(expr),
                    token => Err(format!("Expected ')', found '{:?}'", token).into()),

                }
            }
            Token::Ident(name) => Ok(Some(Box::new(Expr::Ident(name)))),
            Token::Integer(value) => Ok(Some(Box::new(Expr::Value(Value::Int32(value))))),
            Token::Float(value) => Ok(Some(Box::new(Expr::Value(Value::Float32(value))))),
            Token::Bool(value) => Ok(Some(Box::new(Expr::Value(Value::Bool(value))))),
            Token::String(value) => Ok(Some(Box::new(Expr::Value(Value::String(value))))),
            _ => Ok(None),
        }
    }
}
