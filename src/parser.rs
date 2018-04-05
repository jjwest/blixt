use failure;

use ast::*;
use lexer::{Token, TokenKind};

use std::collections::VecDeque;

fn parse(tokens: VecDeque<Token>) -> Result<Ast, failure::Error> {
    let ast = Ast {
        statements: Vec::new(),
    };

    Ok(ast)
}

// fn expression(tokens: &mut VecDeque<Token>) -> Result<Option<Expr>, failure::Error> {}

fn integer(tokens: &mut VecDeque<Token>) -> Result<Option<Expr>, failure::Error> {
    if let Some(token) = tokens.pop_front() {
        match token.kind {
            TokenKind::Integer(n) => return Ok(Some(Expr::Integer(n))),
            TokenKind::Float(n) => return Ok(Some(Expr::Float(n))),
            TokenKind::String(n) => return Ok(Some(Expr::String(n))),
            TokenKind::Bool(n) => return Ok(Some(Expr::Bool(n))),
            TokenKind::Ident(n) => return Ok(Some(Expr::Ident(n))),
            _ => eprintln!("NO MATCH"),
        }
    }

    Ok(None)
}

// fn statement(tokens: &mut VecDeque<Token>) -> Result<Option<Stmt>, failure::Error> {
//     if let Some(expression) = expression(tokens)? {
//         return Ok(Some(Stmt::Expr(expression)));
//     }

//     Ok(None)
// }

// fn arithmetic_expr(tokens: &mut VecDeque<Token>) -> Result<Option<Expr>, failure::Error> {}
