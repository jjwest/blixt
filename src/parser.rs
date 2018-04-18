use failure;

use ast::{Ast, BinaryOp, BinaryOpKind, Expr, Stmt, UnaryOp};
use lexer::{Token, TokenKind};

use std::collections::VecDeque;

pub fn parse_ast(mut tokens: VecDeque<Token>) -> Result<Ast, failure::Error> {
    let statements = statement_list(&mut tokens);

    Ok(Ast { statements })
}

fn statement_list(tokens: &mut VecDeque<Token>) -> Vec<Stmt> {
    let mut stmts = Vec::new();
    while let Some(stmt) = statement(tokens) {
        stmts.push(stmt);
    }
    stmts
}

fn statement(tokens: &mut VecDeque<Token>) -> Option<Stmt> {
    trace!("Entered statement");

    if let Some(assignment) = assignment(tokens) {
        Some(assignment)
    } else if let Some(expr) = expression(tokens) {
        Some(Stmt::Expr(expr))
    } else {
        None
    }
}

fn assignment(tokens: &mut VecDeque<Token>) -> Option<Stmt> {
    trace!("Entered assignment first: {:?}", tokens.get(0));

    let ident = if let Some(token) = tokens.get(0) {
        match token.kind {
            TokenKind::Ident(_) => match tokens.pop_front().unwrap().kind {
                TokenKind::Ident(n) => Expr::Ident(n),
                _ => unreachable!(),
            },
            _ => return None,
        }
    } else {
        return None;
    };

    let op = match tokens.get(0).map(|tok| &tok.kind) {
        Some(TokenKind::Assign) => BinaryOpKind::Assign,
        Some(TokenKind::AddAssign) => BinaryOpKind::AddAssign,
        Some(TokenKind::SubAssign) => BinaryOpKind::SubAssign,
        Some(TokenKind::MulAssign) => BinaryOpKind::MulAssign,
        Some(TokenKind::DivAssign) => BinaryOpKind::DivAssign,
        Some(TokenKind::ModAssign) => BinaryOpKind::ModAssign,
        _ => return None,
    };

    debug!("OP: {:?}", op);

    tokens.pop_front();
    let value = expression(tokens).expect("Missing expr after assignment");

    Some(Stmt::Assignment(BinaryOp {
        lhs: Box::new(ident),
        rhs: Box::new(value),
        op,
    }))
}

fn expression(tokens: &mut VecDeque<Token>) -> Option<Expr> {
    trace!("Entered logical expr");

    factor(tokens).map(|lhs| {
        if let Some(op) = tokens.get(0) {
            let op = match op.kind {
                TokenKind::Equal => BinaryOpKind::Equal,
                TokenKind::Greater => BinaryOpKind::Greater,
                TokenKind::GreaterEqual => BinaryOpKind::GreaterEqual,
                TokenKind::Lesser => BinaryOpKind::LesserEqual,
                TokenKind::NotEqual => BinaryOpKind::NotEqual,
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
    if let Some(token) = tokens.pop_front() {
        match token.kind {
            TokenKind::Integer(n) => return Some(Expr::Integer(n)),
            TokenKind::Float(n) => return Some(Expr::Float(n)),
            TokenKind::String(n) => return Some(Expr::StringLiteral(n)),
            TokenKind::Bool(n) => return Some(Expr::Bool(n)),
            TokenKind::Ident(n) => return Some(Expr::Ident(n)),
            _ => {}//tokens.push_front(token),
        }
    }

    None
}

//     None
// }

// fn integer(tokens: VecDeque<Token>) -> Option<Expr> {
//     if let Some(token) = tokens.get(0) {
//         if token.kind == TokenKind::Integer {

//         }
//     }
// }

// fn statement(tokens: &mut VecDeque<Token>) -> Result<Option<Stmt>, failure::Error> {
//     if let Some(expression) = expression(tokens)? {
//         return Ok(Some(Stmt::Expr(expression)));
//     }

//     Ok(None)
// }

// fn arithmetic_expr(tokens: &mut VecDeque<Token>) -> Result<Option<Expr>, failure::Error> {}
