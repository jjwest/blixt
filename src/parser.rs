use ast::{
    ArgList, Assignment, AssignmentKind, Ast, BinaryOp, BinaryOpKind, Decl, Expr, ExprKind, For,
    FunctionCall, FunctionDecl, If, Input, Param, ParamList, Print, Range, Return, Stmt, UnaryOp,
    UnaryOpKind, VarDecl,
};
use context::Context;
use lexer::{Token, TokenKind};
use location::Location;
use primitives::ValueKind;

use failure;

use std::collections::VecDeque;
use std::rc::Rc;

pub fn parse_ast(tokens: VecDeque<Token>, context: &mut Context) -> Result<Ast, failure::Error> {
    let location = match tokens.get(0) {
        Some(token) => token.location,
        None => {
            return Ok(Ast {
                statements: Vec::new(),
            })
        }
    };

    let mut parser = Parser {
        context,
        tokens,
        location,
    };

    let statements = match parser.statement_list() {
        Ok(stmts) => stmts,
        Err(_) => return Err(failure::err_msg("")),
    };

    Ok(Ast { statements })
}

fn join_locations(a: Location, b: Location) -> Location {
    let (mut smaller, larger) = {
        if a.span.start < b.span.start {
            (a, b)
        } else {
            (b, a)
        }
    };

    smaller.span.len +=
        larger.span.start - (smaller.span.start + smaller.span.len) + larger.span.len;

    smaller
}

struct Parser<'a> {
    context: &'a mut Context,
    tokens: VecDeque<Token>,
    location: Location,
}

impl<'a> Parser<'a> {
    fn report_error(&mut self, message: &str) {
        self.context.error(message, self.location);
    }

    fn peek_token(&self, pos: usize) -> Option<&Token> {
        self.tokens.get(pos)
    }

    fn peek_token_kind(&self, pos: usize) -> Option<&TokenKind> {
        self.tokens.get(pos).map(|token| &token.kind)
    }

    fn next_token(&mut self) -> Option<Token> {
        self.tokens.pop_front().map(|token| {
            if let Some(token) = self.peek_token(0) {
                self.location = token.location;
            }
            token
        })
    }

    fn next_token_kind(&mut self) -> Option<TokenKind> {
        self.tokens.pop_front().map(|token| {
            self.location = token.location;
            token.kind
        })
    }

    fn expect_next(&mut self, kind: TokenKind) -> Result<Token, ()> {
        if let Some(token) = self.next_token() {
            if token.kind == kind {
                Ok(token)
            } else {
                self.report_error(&format!("Expected {:?}, found {:?}", kind, token.kind));
                Err(())
            }
        } else {
            self.report_error("Expected {:?}, but no tokens were left");
            Err(())
        }
    }

    fn statement_list(&mut self) -> Result<Vec<Stmt>, ()> {
        trace!("Entered statement_list");

        let mut stmts = Vec::new();
        while let Some(stmt) = self.statement()? {
            debug!("Parsed STMT {:?}", stmt);
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    fn statement(&mut self) -> Result<Option<Stmt>, ()> {
        trace!("Entered statement: {:?}", self.peek_token_kind(0));

        if let Some(print) = self.print()? {
            Ok(Some(print))
        } else if let Some(decl) = self.declaration()? {
            Ok(Some(decl))
        } else if let Some(if_stmt) = self.if_statement()? {
            Ok(Some(if_stmt))
        } else if let Some(for_loop) = self.for_loop()? {
            Ok(Some(for_loop))
        } else if let Some(keyword) = self.keyword()? {
            Ok(Some(keyword))
        } else if let Some(block) = self.block()? {
            Ok(Some(block))
        } else if let Some(assignment) = self.assignment()? {
            Ok(Some(assignment))
        } else if let Some(decl) = self.function_decl()? {
            Ok(Some(decl))
        } else if let Some(expr) = self.expression()? {
            Ok(Some(Stmt::Expr(expr)))
        } else {
            Ok(None)
        }
    }

    fn for_loop(&mut self) -> Result<Option<Stmt>, ()> {
        trace!("Entered for_loop");

        match self.peek_token_kind(0) {
            Some(TokenKind::For) => {}
            _ => return Ok(None),
        }

        self.next_token();

        let ident = self.ident()?;
        self.expect_next(TokenKind::In)?;

        let range = match self.expression()? {
            Some(expr) => match expr.kind {
                ExprKind::Range(Range { start, end }) => Range { start, end },
                _ => {
                    self.report_error("Expected range");
                    return Err(());
                }
            },
            None => {
                self.report_error("Expected range");
                return Err(());
            }
        };

        let block = match self.block()? {
            Some(block) => block,
            None => {
                self.report_error("Expected block");
                return Err(());
            }
        };

        Ok(Some(Stmt::For(For {
            ident,
            range,
            block: match block {
                Stmt::Block(b) => b,
                _ => unreachable!(),
            },
        })))
    }

    fn range(&mut self) -> Result<Option<Expr>, ()> {
        match self.peek_token_kind(0) {
            Some(TokenKind::Range(start, end)) => {
                let start = *start;
                let end = *end;
                let token = self.next_token().unwrap();

                let range = Expr {
                    location: token.location,
                    kind: ExprKind::Range(Range { start, end }),
                };
                Ok(Some(range))
            }
            _ => Ok(None),
        }
    }

    fn declaration(&mut self) -> Result<Option<Stmt>, ()> {
        trace!("Entered declaration");

        match (self.peek_token_kind(0), self.peek_token_kind(1)) {
            (Some(TokenKind::Ident(_)), Some(TokenKind::VarDecl))
            | (Some(TokenKind::Ident(_)), Some(TokenKind::Colon)) => {}
            _ => return Ok(None),
        }

        let ident = match self.next_token_kind() {
            Some(TokenKind::Ident(n)) => n,
            _ => unreachable!(),
        };

        let var_type = match self.next_token_kind() {
            Some(TokenKind::Colon) => match self.next_token_kind() {
                Some(TokenKind::BoolType) => ValueKind::Bool,
                Some(TokenKind::StringType) => ValueKind::String,
                Some(TokenKind::IntType) => ValueKind::Integer,
                Some(TokenKind::FloatType) => ValueKind::Float,
                Some(kind) => {
                    self.report_error(&format!("Expected type, found {:?}", kind));
                    return Err(());
                }
                None => {
                    self.report_error("Expected type");
                    return Err(());
                }
            },
            Some(TokenKind::VarDecl) => ValueKind::Nil,
            other => {
                self.report_error(&format!(
                    "Expected type or declaration operator, found {:?}",
                    other
                ));
                return Err(());
            }
        };

        if var_type != ValueKind::Nil {
            self.expect_next(TokenKind::Assign)?;
        }

        let mut value = match self.expression()? {
            Some(expr) => expr,
            None => {
                self.report_error("Expected expression");
                return Err(());
            }
        };

        if let ValueKind::Float = var_type {
            if let ExprKind::Integer(n) = &value.kind {
                value.kind = ExprKind::Float(*n as f32);
            }
        }

        Ok(Some(Stmt::Decl(Decl::Variable(VarDecl {
            name: ident,
            value,
            kind: var_type,
        }))))
    }

    fn function_decl(&mut self) -> Result<Option<Stmt>, ()> {
        trace!("Entered function_decl");

        match self.peek_token_kind(0) {
            Some(TokenKind::FunctionDecl) => {}
            _ => return Ok(None),
        }

        self.next_token();
        let name = self.ident()?;

        let param_list = self.parameter_list()?;

        let return_type = if let Some(TokenKind::ReturnDecl) = self.peek_token_kind(0) {
            self.next_token();
            match self.next_token_kind() {
                Some(TokenKind::BoolType) => Some(ValueKind::Bool),
                Some(TokenKind::IntType) => Some(ValueKind::Integer),
                Some(TokenKind::FloatType) => Some(ValueKind::Float),
                Some(TokenKind::StringType) => Some(ValueKind::String),
                _ => panic!("Expected type after return decl"),
            }
        } else {
            None
        };

        self.expect_next(TokenKind::OpenBrace)?;
        let body = self.statement_list()?;
        self.expect_next(TokenKind::CloseBrace)?;

        Ok(Some(Stmt::Decl(Decl::Function(FunctionDecl {
            name,
            body,
            params: param_list,
            return_type,
        }))))
    }

    fn parameter_list(&mut self) -> Result<ParamList, ()> {
        self.expect_next(TokenKind::OpenParen)?;

        let mut params = ParamList::new();

        loop {
            match self.peek_token_kind(0) {
                Some(TokenKind::Ident(_)) => {}
                Some(TokenKind::CloseParen) => {
                    self.next_token();
                    break;
                }
                other => {
                    self.report_error(&format!(
                        "Expected identifier or closing parenthesis, found {:?}",
                        other
                    ));
                    return Err(());
                }
            }

            let name = self.ident().expect("Expected ident");
            self.expect_next(TokenKind::Colon)?;

            let kind = match self.next_token() {
                Some(Token { kind, .. }) => match kind {
                    TokenKind::BoolType => ValueKind::Bool,
                    TokenKind::IntType => ValueKind::Integer,
                    TokenKind::FloatType => ValueKind::Float,
                    TokenKind::StringType => ValueKind::String,
                    _ => panic!("Expected type"),
                },
                None => panic!("Sadasdas"),
            };

            params.push(Param { name, kind });

            if let Some(token) = self.next_token() {
                if token.kind == TokenKind::CloseParen {
                    break;
                } else if token.kind != TokenKind::Comma {
                    self.report_error(&format!(
                        "Expected comma or closing parenthesis, found {:?}",
                        token.kind
                    ));
                }
            }
        }

        Ok(params)
    }

    fn function_call(&mut self) -> Result<Option<Expr>, ()> {
        match (self.peek_token_kind(0), self.peek_token_kind(1)) {
            (Some(TokenKind::Ident(_)), Some(TokenKind::OpenParen)) => {}
            _ => return Ok(None),
        }

        let mut location = self.location;
        let name = self.ident()?;
        let args = self.argument_list()?;

        for arg in &args {
            location = join_locations(location, arg.location);
        }

        // Include the closing parenthesis
        location.span.len += 1;

        let funcall = Expr {
            location,
            kind: ExprKind::FunctionCall(FunctionCall { name, args }),
        };

        Ok(Some(funcall))
    }

    fn argument_list(&mut self) -> Result<ArgList, ()> {
        self.expect_next(TokenKind::OpenParen)?;
        let mut args = ArgList::new();

        while let Some(expr) = self.expression()? {
            args.push(expr);

            match self.peek_token_kind(0) {
                Some(TokenKind::Comma) => {
                    self.next_token();
                }
                _ => break,
            }
        }

        self.expect_next(TokenKind::CloseParen)?;
        Ok(args)
    }

    fn keyword(&mut self) -> Result<Option<Stmt>, ()> {
        trace!("Entered keyword");

        if let Some(token) = self.peek_token(0) {
            let location = token.location;
            match &token.kind {
                TokenKind::Bool(n) => {
                    let kind = ExprKind::Bool(*n);
                    self.next_token();
                    return Ok(Some(Stmt::Expr(Expr { location, kind })));
                }
                TokenKind::Return => {
                    self.next_token();
                    let expr = self.expression()?;
                    return Ok(Some(Stmt::Return(Return {
                        value: expr,
                        location,
                    })));
                }
                _ => {}
            }
        }

        Ok(None)
    }

    fn if_statement(&mut self) -> Result<Option<Stmt>, ()> {
        trace!("Entered if statement");

        if let Some(token) = self.peek_token(0) {
            match token.kind {
                TokenKind::If => {}
                _ => return Ok(None),
            }

            self.next_token();

            let cond = self.expression()?.expect("Expected expression after if");
            self.expect_next(TokenKind::OpenBrace)?;
            let body = self.statement_list()?;
            self.expect_next(TokenKind::CloseBrace)?;

            let else_body = match self.peek_token(0) {
                Some(Token { kind, .. }) if *kind == TokenKind::Else => {
                    self.next_token();
                    let stmts = self.statement()?.expect("else stmtlist");
                    Some(Box::new(stmts))
                }
                _ => None,
            };

            Ok(Some(Stmt::If(If {
                cond,
                body,
                else_body,
            })))
        } else {
            Ok(None)
        }
    }

    fn block(&mut self) -> Result<Option<Stmt>, ()> {
        trace!("Entered block");

        if let Some(token) = self.peek_token(0) {
            if token.kind != TokenKind::OpenBrace {
                return Ok(None);
            }

            self.next_token();
            let statements = self.statement_list()?;
            self.expect_next(TokenKind::CloseBrace)?;
            Ok(Some(Stmt::Block(statements)))
        } else {
            Ok(None)
        }
    }

    fn print(&mut self) -> Result<Option<Stmt>, ()> {
        trace!("Entered print");

        match self.peek_token(0) {
            Some(Token { kind, .. }) => {
                if *kind != TokenKind::Ident("print".to_owned()) {
                    return Ok(None);
                }
            }
            _ => return Ok(None),
        }

        self.next_token();
        let args = self.argument_list()?;
        Ok(Some(Stmt::Print(Print { args })))
    }

    fn assignment(&mut self) -> Result<Option<Stmt>, ()> {
        trace!("Entered assignment first: {:?}", self.peek_token(0));

        if let Some(token) = self.peek_token(0) {
            match token.kind {
                TokenKind::Ident(_) => {}
                _ => return Ok(None),
            }
        } else {
            return Ok(None);
        };

        let op = match self.peek_token_kind(1) {
            Some(TokenKind::Assign) => AssignmentKind::Assign,
            Some(TokenKind::AddAssign) => AssignmentKind::Add,
            Some(TokenKind::SubAssign) => AssignmentKind::Sub,
            Some(TokenKind::MulAssign) => AssignmentKind::Mul,
            Some(TokenKind::DivAssign) => AssignmentKind::Div,
            Some(TokenKind::ModAssign) => AssignmentKind::Mod,
            _ => return Ok(None),
        };

        let location = self.location;
        let ident = self.ident()?;
        self.next_token();
        let value = match self.expression()? {
            Some(expr) => expr,
            None => {
                self.report_error("Missing expr after assignment");
                return Err(());
            }
        };

        let location = join_locations(location, value.location);

        Ok(Some(Stmt::Assignment(Assignment {
            ident,
            value,
            op,
            location,
        })))
    }

    fn expression(&mut self) -> Result<Option<Expr>, ()> {
        trace!("Entered expression");

        if let Some(lhs) = self.logical_expr_a()? {
            if let Some(op) = self.peek_token(0) {
                let op = match op.kind {
                    TokenKind::And => BinaryOpKind::And,
                    _ => return Ok(Some(lhs)),
                };

                self.next_token();
                let rhs = self.expression()?.expect("No rhs in expression");

                let mut location = lhs.location;
                location.span.len += rhs.location.span.len;

                let expr = Expr {
                    location,
                    kind: ExprKind::BinaryOp(BinaryOp {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op,
                    }),
                };

                Ok(Some(expr))
            } else {
                Ok(Some(lhs))
            }
        } else {
            Ok(None)
        }
    }

    fn logical_expr_a(&mut self) -> Result<Option<Expr>, ()> {
        trace!("Entered expression");

        if let Some(lhs) = self.logical_expr_b()? {
            if let Some(op) = self.peek_token(0) {
                let op = match op.kind {
                    TokenKind::Or => BinaryOpKind::Or,
                    _ => return Ok(Some(lhs)),
                };

                self.next_token();
                let rhs = self.logical_expr_a()?.expect("No rhs in expression");

                let location = join_locations(lhs.location, rhs.location);

                let expr = Expr {
                    location,
                    kind: ExprKind::BinaryOp(BinaryOp {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op,
                    }),
                };

                Ok(Some(expr))
            } else {
                Ok(Some(lhs))
            }
        } else {
            Ok(None)
        }
    }

    fn logical_expr_b(&mut self) -> Result<Option<Expr>, ()> {
        trace!("Entered expression");

        if let Some(lhs) = self.factor()? {
            if let Some(op) = self.peek_token(0) {
                let op = match op.kind {
                    TokenKind::Equal => BinaryOpKind::Equal,
                    TokenKind::Greater => BinaryOpKind::Greater,
                    TokenKind::GreaterEqual => BinaryOpKind::GreaterEqual,
                    TokenKind::Lesser => BinaryOpKind::Lesser,
                    TokenKind::LesserEqual => BinaryOpKind::LesserEqual,
                    TokenKind::NotEqual => BinaryOpKind::NotEqual,
                    _ => return Ok(Some(lhs)),
                };

                self.next_token();
                let rhs = self.logical_expr_b()?.expect("No rhs in expression");
                let location = join_locations(lhs.location, rhs.location);

                let expr = Expr {
                    location,
                    kind: ExprKind::BinaryOp(BinaryOp {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op,
                    }),
                };

                Ok(Some(expr))
            } else {
                Ok(Some(lhs))
            }
        } else {
            Ok(None)
        }
    }

    fn factor(&mut self) -> Result<Option<Expr>, ()> {
        trace!("Entered factor");

        if let Some(lhs) = self.term()? {
            if let Some(op) = self.peek_token(0) {
                let op = match op.kind {
                    TokenKind::Add => BinaryOpKind::Add,
                    TokenKind::Sub => BinaryOpKind::Sub,
                    _ => return Ok(Some(lhs)),
                };

                self.next_token();
                let rhs = self.factor()?.expect("No rhs in expression");
                let location = join_locations(lhs.location, rhs.location);

                let expr = Expr {
                    location,
                    kind: ExprKind::BinaryOp(BinaryOp {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op,
                    }),
                };

                Ok(Some(expr))
            } else {
                Ok(Some(lhs))
            }
        } else {
            Ok(None)
        }
    }

    fn term(&mut self) -> Result<Option<Expr>, ()> {
        trace!("Entered term");

        if let Some(lhs) = self.atom()? {
            if let Some(op) = self.peek_token(0) {
                let op = match op.kind {
                    TokenKind::Mul => BinaryOpKind::Mul,
                    TokenKind::Div => BinaryOpKind::Div,
                    TokenKind::Mod => BinaryOpKind::Mod,
                    _ => return Ok(Some(lhs)),
                };

                self.next_token();
                let rhs = self.term()?.expect("No rhs in expression");

                let location = join_locations(lhs.location, rhs.location);

                let expr = Expr {
                    location,
                    kind: ExprKind::BinaryOp(BinaryOp {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op,
                    }),
                };

                Ok(Some(expr))
            } else {
                Ok(Some(lhs))
            }
        } else {
            Ok(None)
        }
    }

    fn atom(&mut self) -> Result<Option<Expr>, ()> {
        trace!("Enteted atom");

        if let Some(token) = self.peek_token(0) {
            match token.kind {
                TokenKind::Integer(_)
                | TokenKind::Float(_)
                | TokenKind::String(_)
                | TokenKind::Bool(_)
                | TokenKind::Range(_, _)
                | TokenKind::Ident(_) => {}
                TokenKind::Sub => {
                    let mut location = token.location;
                    self.expect_next(TokenKind::Sub)?;
                    let expr = self.atom()?.expect("expected expression");
                    location.span.len += expr.location.span.len;

                    return Ok(Some(Expr {
                        location,
                        kind: ExprKind::UnaryOp(UnaryOp {
                            value: Box::new(expr),
                            op: UnaryOpKind::Neg,
                        }),
                    }));
                }
                TokenKind::Not => {
                    let mut location = token.location;
                    self.expect_next(TokenKind::Sub)?;
                    let expr = self.expression()?.expect("expected expression");
                    location.span.len += expr.location.span.len;

                    return Ok(Some(Expr {
                        location,
                        kind: ExprKind::UnaryOp(UnaryOp {
                            value: Box::new(expr),
                            op: UnaryOpKind::Not,
                        }),
                    }));
                }
                TokenKind::OpenParen => {
                    self.expect_next(TokenKind::OpenParen)?;
                    let expr = self.expression();
                    self.expect_next(TokenKind::CloseParen)?;
                    return expr;
                }
                _ => return Ok(None),
            }

            if let Some(range) = self.range()? {
                return Ok(Some(range));
            }

            if let Some(input) = self.input()? {
                return Ok(Some(input));
            }

            if let Some(function) = self.function_call()? {
                return Ok(Some(function));
            }

            match self.next_token() {
                Some(token) => {
                    match token.kind {
                        TokenKind::Integer(n) => {
                            return Ok(Some(Expr {
                                location: token.location,
                                kind: ExprKind::Integer(n),
                            }))
                        }
                        TokenKind::Float(n) => {
                            return Ok(Some(Expr {
                                location: token.location,
                                kind: ExprKind::Float(n),
                            }))
                        }
                        TokenKind::String(n) => {
                            return Ok(Some(Expr {
                                location: token.location,
                                kind: ExprKind::StringLiteral(Rc::new(n)),
                            }))
                        }
                        TokenKind::Bool(n) => {
                            return Ok(Some(Expr {
                                location: token.location,
                                kind: ExprKind::Bool(n),
                            }))
                        }
                        TokenKind::Ident(n) => {
                            return Ok(Some(Expr {
                                location: token.location,
                                kind: ExprKind::Ident(n),
                            }))
                        }
                        _ => return Ok(None),
                    };
                }
                None => return Ok(None),
            }
        }

        Ok(None)
    }

    fn ident(&mut self) -> Result<String, ()> {
        let token = match self.next_token() {
            Some(token) => token,
            None => {
                self.report_error("Tried to get an identifier when no tokens are left");
                return Err(());
            }
        };
        match token.kind {
            TokenKind::Ident(n) => Ok(n),
            kind => {
                self.report_error(&format!("Expected identifier, found {:?}", kind));
                Err(())
            }
        }
    }

    fn input(&mut self) -> Result<Option<Expr>, ()> {
        trace!("Entered input, {:?}", self.peek_token(0));

        match self.peek_token(0) {
            Some(Token { kind, .. }) => {
                if *kind != TokenKind::Ident("input".to_owned()) {
                    return Ok(None);
                }
            }
            _ => return Ok(None),
        }

        let token = self.next_token().unwrap();
        let mut location = token.location;
        let message = self.expression()?;

        if let Some(message) = &message {
            location.span.len += message.location.span.len;
        }

        Ok(Some(Expr {
            location,
            kind: ExprKind::Input(Input {
                message: message.map(Box::new),
            }),
        }))
    }
}
