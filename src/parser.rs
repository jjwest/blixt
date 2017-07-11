use std::mem;

use ast::{Assignment, ArithmeticOp, Expr, Function, LogicOp, ParameterList, Stmt, StmtList,
          ValueType};
use errors::*;
use lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    ast: StmtList,
    pos: usize,
}

macro_rules! expect_next {
    ( $context:ident, $func:ident, $expected:expr ) => {
        match $context.$func() {
            $expected => {},
            other => return Err(format!("Expected '{:?}', found '{:?}'", $expected, other).into()),
        }
    }
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
        if self.pos + len < self.tokens.len() {
            Some(&self.tokens[self.pos..self.pos + len])
        } else {
            None
        }
    }

    fn statement_list(&mut self) -> Result<Option<StmtList>> {
        let mut stmt_list = StmtList::new();
        while let Some(stmt) = self.statement()? {
            stmt_list.add_stmt(stmt);
        }

        Ok(Some(stmt_list))
    }

    fn statement(&mut self) -> Result<Option<Stmt>> {
        trace!("Entered statement");

        if let Some(function) = self.function_declaration()? {
            debug!("function_declaration");
            return Ok(Some(Stmt::Function(function)));
        }

        if let Some(params) = self.parameter_list()? {
            debug!("parameterlist");
            return Ok(Some(Stmt::ParameterList(params)));
        }

        if let Some(assignment) = self.assignment()? {
            debug!("assignment");
            return Ok(Some(Stmt::Assignment(assignment)));
        }
        if let Some(expr) = self.expression()? {
            debug!("expression");
            return Ok(Some(Stmt::Expr(*expr)));
        }

        Ok(None)
    }

    fn assignment(&mut self) -> Result<Option<Assignment>> {
        trace!("Entered assignment");

        match self.peek(2) {
            Some(&[Token::Ident(_), Token::Assign]) => {}
            _ => return Ok(None),
        }

        let ident = match self.next_token() {
            Token::Ident(ident) => ident,
            other => return Err(format!("Expected identifier, found '{:?}'", other).into()),
        };

        expect_next!(self, next_token, Token::Assign);

        let value = match self.expression()? {
            Some(expr) => *expr,
            None => {
                return Err(
                    format!("Expected expression, found '{:?}'", self.next_token()).into(),
                )
            }
        };

        Ok(Some(Assignment::new(ident, value)))
    }

    fn parameter_list(&mut self) -> Result<Option<ParameterList>> {
        trace!("Entered parameter_list");

        let mut params = ParameterList::new();
        loop {
            match self.expression()? {
                Some(expr) => {
                    params.push(*expr);
                    if self.peek(1) == Some(&[Token::Comma]) {
                        self.next_token();
                    } else {
                        return Ok(Some(params));
                    }
                }
                None => return Ok(Some(params)),
            }
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
            other => return Err(format!("Expected identifier, found '{:?}'", other).into()),
        };

        expect_next!(self, next_token, Token::OpenParen);

        let params = self.parameter_list()?.expect("Expected params");

        expect_next!(self, next_token, Token::CloseParen);
        expect_next!(self, next_token, Token::OpenBrace);

        let body = self.statement_list()?.expect("Expected function body");

        expect_next!(self, next_token, Token::CloseBrace);
        Ok(Some(Function::new(ident, body, params)))
    }

    fn expression(&mut self) -> Result<Option<Box<Expr>>> {
        trace!("Entered expression");
        self.logical_expression()
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

        match self.next_token() {
            Token::OpenParen => {
                let expr = self.expression()?;
                match self.next_token() {
                    Token::CloseParen => Ok(expr),
                    token => Err(format!("Expected ')', found '{:?}'", token).into()),

                }
            }
            Token::Ident(name) => Ok(Some(Box::new(Expr::Ident(name)))),
            Token::Integer(value) => Ok(Some(Box::new(Expr::Value(ValueType::Int32(value))))),
            Token::Float(value) => Ok(Some(Box::new(Expr::Value(ValueType::Float32(value))))),
            Token::Bool(value) => Ok(Some(Box::new(Expr::Value(ValueType::Bool(value))))),
            Token::String(value) => Ok(Some(Box::new(Expr::Value(ValueType::String(value))))),
            other => Err(format!("Expected an atom, found '{:?}'", other).into()),
        }
    }
}
