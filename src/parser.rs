use failure;

use ast::{ArgList, Assignment, AssignmentKind, Ast, BinaryOp, BinaryOpKind, Decl, Expr,
          FunctionCall, FunctionDecl, If, Param, ParamList, Stmt, UnaryOp, UnaryOpKind, ValueKind,
          VarDecl};
use lexer::{Token, TokenKind};

use std::collections::VecDeque;
use std::rc::Rc;

pub fn parse_ast(mut tokens: VecDeque<Token>) -> Result<Ast, failure::Error> {
    let statements = statement_list(&mut tokens);

    Ok(Ast { statements })
}

fn expect(tokens: &mut VecDeque<Token>, kind: TokenKind) {
    if let Some(token) = tokens.pop_front() {
        if token.kind != kind {
            panic!("Expected {:?}, but found {:?}", kind, token.kind);
        }
    } else {
        panic!("Tried to use expect when no more tokens are left");
    }
}

fn statement_list(tokens: &mut VecDeque<Token>) -> Vec<Stmt> {
    trace!("Entered statement_list");

    let mut stmts = Vec::new();
    while let Some(stmt) = statement(tokens) {
        debug!("Parsed STMT {:?}", stmt);
        stmts.push(stmt);
    }
    stmts
}

fn statement(tokens: &mut VecDeque<Token>) -> Option<Stmt> {
    trace!("Entered statement: {:?}", tokens.get(0));

    if let Some(decl) = declaration(tokens) {
        Some(decl)
    } else if let Some(if_stmt) = if_statement(tokens) {
        Some(if_stmt)
    } else if let Some(keyword) = keyword(tokens) {
        Some(keyword)
    } else if let Some(block) = block(tokens) {
        Some(block)
    } else if let Some(assignment) = assignment(tokens) {
        Some(assignment)
    } else if let Some(decl) = function_decl(tokens) {
        Some(decl)
    } else if let Some(expr) = expression(tokens) {
        Some(Stmt::Expr(expr))
    } else {
        None
    }
}

fn declaration(tokens: &mut VecDeque<Token>) -> Option<Stmt> {
    trace!("Entered declaration");

    match (
        tokens.get(0).map(|t| &t.kind),
        tokens.get(1).map(|t| &t.kind),
    ) {
        (Some(TokenKind::Ident(_)), Some(TokenKind::VarDecl))
        | (Some(TokenKind::Ident(_)), Some(TokenKind::Colon)) => {}
        _ => return None,
    }

    let ident = match tokens.pop_front().unwrap().kind {
        TokenKind::Ident(n) => n,
        _ => unreachable!(),
    };

    let var_type = match tokens.pop_front().map(|t| t.kind) {
        Some(TokenKind::Colon) => match tokens.pop_front().map(|t| t.kind) {
            Some(TokenKind::BoolType) => ValueKind::Bool,
            Some(TokenKind::StringType) => ValueKind::String,
            Some(TokenKind::IntType) => ValueKind::Integer,
            Some(TokenKind::FloatType) => ValueKind::Float,
            _ => panic!("Expected type"),
        },
        Some(TokenKind::VarDecl) => ValueKind::Undecided,
        _ => panic!("Expected var type"),
    };

    if var_type != ValueKind::Undecided {
        expect(tokens, TokenKind::Assign);
    }

    let value = expression(tokens).expect("Variable decl expected expression");

    Some(Stmt::Decl(Decl::Variable(VarDecl {
        name: ident,
        value,
        kind: var_type,
    })))
}

fn function_decl(tokens: &mut VecDeque<Token>) -> Option<Stmt> {
    trace!("Entered function_decl");

    match tokens.get(0).map(|tok| &tok.kind) {
        Some(TokenKind::FunctionDecl) => {}
        _ => return None,
    }

    tokens.pop_front().unwrap();
    let name = ident(tokens).expect("Expected token after func decl");
    let param_list = parameter_list(tokens);

    let return_type = if let Some(TokenKind::ReturnDecl) = tokens.get(0).map(|tok| &tok.kind) {
        tokens.pop_front().unwrap();
        match tokens.pop_front().map(|tok| tok.kind) {
            Some(TokenKind::BoolType) => Some(ValueKind::Bool),
            Some(TokenKind::IntType) => Some(ValueKind::Integer),
            Some(TokenKind::FloatType) => Some(ValueKind::Float),
            Some(TokenKind::StringType) => Some(ValueKind::String),
            _ => panic!("Expected type after return decl"),
        }
    } else {
        None
    };

    expect(tokens, TokenKind::OpenBrace);
    let body = statement_list(tokens);
    expect(tokens, TokenKind::CloseBrace);

    Some(Stmt::Decl(Decl::Function(FunctionDecl {
        name,
        body,
        params: param_list,
        return_type,
    })))
}

fn parameter_list(tokens: &mut VecDeque<Token>) -> ParamList {
    expect(tokens, TokenKind::OpenParen);

    let mut params = ParamList::new();

    loop {
        match tokens.get(0).map(|tok| &tok.kind) {
            Some(TokenKind::Ident(_)) => {}
            Some(TokenKind::CloseParen) => {
                tokens.pop_front().unwrap();
                break;
            }
            _ => unreachable!(),
        }

        let name = ident(tokens).expect("Expected ident");
        expect(tokens, TokenKind::Colon);

        let kind = match tokens.pop_front() {
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

        if let Some(token) = tokens.pop_front() {
            if token.kind == TokenKind::CloseParen {
                break;
            } else if token.kind != TokenKind::Comma {
                panic!("Expected comma or closeparen in param list");
            }
        }
    }

    params
}

fn function_call(tokens: &mut VecDeque<Token>) -> Option<Expr> {
    match (
        tokens.get(0).map(|t| &t.kind),
        tokens.get(1).map(|t| &t.kind),
    ) {
        (Some(TokenKind::Ident(_)), Some(TokenKind::OpenParen)) => {}
        _ => return None,
    }

    let name = ident(tokens).unwrap();
    expect(tokens, TokenKind::OpenParen);
    let args = argument_list(tokens);
    expect(tokens, TokenKind::CloseParen);

    Some(Expr::FunctionCall(FunctionCall { name, args }))
}

fn argument_list(tokens: &mut VecDeque<Token>) -> ArgList {
    let mut args = ArgList::new();
    while let Some(expr) = expression(tokens) {
        args.push(expr);

        match tokens.get(0).map(|t| &t.kind) {
            Some(TokenKind::Comma) => {
                tokens.pop_front().unwrap();
            }
            _ => break,
        }
    }
    args
}

fn keyword(tokens: &mut VecDeque<Token>) -> Option<Stmt> {
    trace!("Entered keyword");

    if let Some(token) = tokens.get(0) {
        match &token.kind {
            TokenKind::Bool(n) => {
                let value = Some(Stmt::Expr(Expr::Bool(*n)));
                tokens.pop_front().unwrap();
                return value;
            }
            TokenKind::Return => {
                tokens.pop_front().unwrap();
                let expr = expression(tokens);
                return Some(Stmt::Return(expr));
            }
            _ => {}
        }
    }

    None
}

fn if_statement(tokens: &mut VecDeque<Token>) -> Option<Stmt> {
    trace!("Entered if statement");

    if let Some(token) = tokens.get(0) {
        match token.kind {
            TokenKind::If => {}
            _ => return None,
        }

        tokens.pop_front().unwrap();

        let cond = expression(tokens).expect("Expected expression after if");
        expect(tokens, TokenKind::OpenBrace);
        let body = statement_list(tokens);
        expect(tokens, TokenKind::CloseBrace);

        let else_body = match tokens.get(0) {
            Some(Token { kind, .. }) if *kind == TokenKind::Else => {
                tokens.pop_front().unwrap();
                let stmts = statement(tokens).expect("else stmtlist");
                Some(Box::new(stmts))
            }
            _ => None,
        };

        Some(Stmt::If(If {
            cond,
            body,
            else_body,
        }))
    } else {
        None
    }
}

fn block(tokens: &mut VecDeque<Token>) -> Option<Stmt> {
    trace!("Entered block");

    if let Some(token) = tokens.get(0) {
        if token.kind != TokenKind::OpenBrace {
            return None;
        }

        tokens.pop_front().unwrap();
        let statements = statement_list(tokens);
        expect(tokens, TokenKind::CloseBrace);
        Some(Stmt::Block(statements))
    } else {
        None
    }
}

fn assignment(tokens: &mut VecDeque<Token>) -> Option<Stmt> {
    trace!("Entered assignment first: {:?}", tokens.get(0));

    if let Some(token) = tokens.get(0) {
        match token.kind {
            TokenKind::Ident(_) => {}
            _ => return None,
        }
    } else {
        return None;
    };

    let op = match tokens.get(1).map(|tok| &tok.kind) {
        Some(TokenKind::Assign) => AssignmentKind::Regular,
        Some(TokenKind::AddAssign) => AssignmentKind::Add,
        Some(TokenKind::SubAssign) => AssignmentKind::Sub,
        Some(TokenKind::MulAssign) => AssignmentKind::Mul,
        Some(TokenKind::DivAssign) => AssignmentKind::Div,
        Some(TokenKind::ModAssign) => AssignmentKind::Mod,
        _ => return None,
    };

    let ident = ident(tokens).unwrap();
    tokens.pop_front().unwrap();
    let value = expression(tokens).expect("Missing expr after assignment");

    Some(Stmt::Assignment(Assignment { ident, value, op }))
}

fn expression(tokens: &mut VecDeque<Token>) -> Option<Expr> {
    trace!("Entered expression");

    logical_expr_a(tokens).map(|lhs| {
        if let Some(op) = tokens.get(0) {
            let op = match op.kind {
                TokenKind::And => BinaryOpKind::And,
                _ => return lhs,
            };

            tokens.pop_front();

            let rhs = expression(tokens).expect("No rhs in expression");
            Expr::BinaryOp(BinaryOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
            })
        } else {
            lhs
        }
    })
}

fn logical_expr_a(tokens: &mut VecDeque<Token>) -> Option<Expr> {
    trace!("Entered expression");

    logical_expr_b(tokens).map(|lhs| {
        if let Some(op) = tokens.get(0) {
            let op = match op.kind {
                TokenKind::Or => BinaryOpKind::Or,
                _ => return lhs,
            };

            tokens.pop_front();

            let rhs = logical_expr_a(tokens).expect("No rhs in expression");
            Expr::BinaryOp(BinaryOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
            })
        } else {
            lhs
        }
    })
}

fn logical_expr_b(tokens: &mut VecDeque<Token>) -> Option<Expr> {
    trace!("Entered expression");

    factor(tokens).map(|lhs| {
        if let Some(op) = tokens.get(0) {
            let op = match op.kind {
                TokenKind::Equal => BinaryOpKind::Equal,
                TokenKind::Greater => BinaryOpKind::Greater,
                TokenKind::GreaterEqual => BinaryOpKind::GreaterEqual,
                TokenKind::Lesser => BinaryOpKind::Lesser,
                TokenKind::NotEqual => BinaryOpKind::NotEqual,
                _ => return lhs,
            };

            tokens.pop_front();

            let rhs = logical_expr_b(tokens).expect("No rhs in expression");
            Expr::BinaryOp(BinaryOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
            })
        } else {
            lhs
        }
    })
}

fn factor(tokens: &mut VecDeque<Token>) -> Option<Expr> {
    trace!("Entered factor");

    term(tokens).map(|lhs| {
        if let Some(op) = tokens.get(0) {
            let op = match op.kind {
                TokenKind::Add => BinaryOpKind::Add,
                TokenKind::Sub => BinaryOpKind::Sub,
                _ => return lhs,
            };

            tokens.pop_front();

            let rhs = factor(tokens).expect("No rhs in expression");
            Expr::BinaryOp(BinaryOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
            })
        } else {
            lhs
        }
    })
}

fn term(tokens: &mut VecDeque<Token>) -> Option<Expr> {
    trace!("Entered term");

    atom(tokens).map(|lhs| {
        if let Some(op) = tokens.get(0) {
            let op = match op.kind {
                TokenKind::Mul => BinaryOpKind::Mul,
                TokenKind::Div => BinaryOpKind::Div,
                TokenKind::Mod => BinaryOpKind::Mod,
                _ => return lhs,
            };

            tokens.pop_front();

            let rhs = term(tokens).expect("No rhs in expression");
            Expr::BinaryOp(BinaryOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
            })
        } else {
            lhs
        }
    })
}

fn atom(tokens: &mut VecDeque<Token>) -> Option<Expr> {
    trace!("Enteted atom");

    if let Some(token) = tokens.get(0) {
        match token.kind {
            TokenKind::Integer(_)
            | TokenKind::Float(_)
            | TokenKind::String(_)
            | TokenKind::Bool(_)
            | TokenKind::Ident(_) => {}
            TokenKind::Sub => {
                expect(tokens, TokenKind::Sub);
                let expr = expression(tokens).expect("expected expression");
                return Some(Expr::UnaryOp(UnaryOp {
                    expr: Box::new(expr),
                    op: UnaryOpKind::Neg,
                }));
            }
            TokenKind::Not => {
                expect(tokens, TokenKind::Sub);
                let expr = expression(tokens).expect("expected expression");
                return Some(Expr::UnaryOp(UnaryOp {
                    expr: Box::new(expr),
                    op: UnaryOpKind::Not,
                }));
            }
            TokenKind::OpenParen => {
                expect(tokens, TokenKind::OpenParen);
                let expr = expression(tokens);
                expect(tokens, TokenKind::CloseParen);
                return expr;
            }
            _ => return None,
        }

        if let Some(function) = function_call(tokens) {
            return Some(function);
        }

        return match tokens.pop_front().map(|t| t.kind).unwrap() {
            TokenKind::Integer(n) => Some(Expr::Integer(n)),
            TokenKind::Float(n) => Some(Expr::Float(n)),
            TokenKind::String(n) => Some(Expr::StringLiteral(Rc::new(n))),
            TokenKind::Bool(n) => Some(Expr::Bool(n)),
            TokenKind::Ident(n) => Some(Expr::Ident(n)),
            _ => None,
        };
    }

    None
}

fn ident(tokens: &mut VecDeque<Token>) -> Option<String> {
    match tokens.pop_front().map(|tok| tok.kind) {
        Some(TokenKind::Ident(n)) => Some(n),
        _ => None,
    }
}
