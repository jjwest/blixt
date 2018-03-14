use ast::{AbstractSyntaxTree, ArgumentList, ArithmeticOp, Assignment, Expr, LogicOp, Parameter,
          ParameterList, Stmt, StmtList};
use builtins::{Value, ValueKind};
use lexer::Token;

use failure;

use std::collections::VecDeque;

#[derive(Debug, Fail)]
enum Error {
    #[fail(display = "Invalid token")]
    InvalidToken,
    #[fail(display = "Missing token")]
    MissingToken,
}

pub fn parse(tokens: VecDeque<Token>) -> Result<AbstractSyntaxTree, failure::Error> {
    let mut ast = AbstractSyntaxTree::new();

    Ok(ast)
}

fn eat_token(tokens: &mut VecDeque<Token>) -> Result<Token, Error> {
    tokens.pop_front().ok_or(Error::MissingToken)
}

fn parse_statement_list(tokens: &mut VecDeque<Token>) -> Result<StmtList, Error> {
    let mut statements = StmtList::new();

    loop {
        let token = eat_token(tokens)?;
    }

    Ok(statements)
}

// pub struct Parser {
//     tokens: VecDeque<Token>,
//     pos: usize,
// }

// impl Parser {
//     pub fn new(tokens: VecDeque<Token>) -> Self {
//         Parser { tokens, pos: 0 }
//     }

//     pub fn parse(&mut self) -> Result<AbstractSyntaxTree, failure::Error> {
//         let mut syntax_tree = AbstractSyntaxTree::new();

//         // while self.pos < self.tokens.len() {
//         //     syntax_tree.add_stmt(match self.statement()? {
//         //         Some(stmt) => stmt,
//         //         None => return Err(err_msg("Error while parsing")),
//         //     });
//         // }
//         Ok(syntax_tree)
//     }

//     fn eat_token(&mut self) -> Token {
//         self.tokens.pop_front().unwrap()
//     }

//     // fn peek(&mut self, len: usize) -> Option<&[Token]> {
//     //     if self.pos + len <= self.tokens.len() {
//     //         Some(&self.tokens[self.pos..self.pos + len])
//     //     } else {
//     //         None
//     //     }
//     // }

//     // fn statement_list(&mut self) -> Result<Option<StmtList>> {
//     //     let mut stmt_list = StmtList::new();
//     //     while let Some(stmt) = self.statement()? {
//     //         stmt_list.0.push(stmt);
//     //     }

//     //     Ok(Some(stmt_list))
//     // }

//     // fn statement(&mut self) -> Result<Option<Stmt>> {
//     //     trace!("Entered statement");

//     //     // if let Some(assignment) = self.assignment()? {
//     //     //     return Ok(Some(Stmt::Assignment(assignment)));
//     //     // }
//     //     // if let Some(expr) = self.expression()? {
//     //     //     return Ok(Some(Stmt::Expr(*expr)));
//     //     // }

//     //     Ok(None)
//     // }

//     // fn assignment(&mut self) -> Result<Option<Assignment>> {
//     //     trace!("Entered assignment");

//     //     // debug!("Assignment: {:?}", assignment);
//     //     Ok(None)
//     // }

//     // fn parameter_list(&mut self) -> Result<ParameterList> {
//     //     trace!("Entered parameter_list");
//     //     Ok(None)
//     // }

//     // fn expression(&mut self) -> Result<Option<Box<Expr>>> {
//     //     trace!("Entered expression");
//     //     let expr = self.logical_expression();
//     //     debug!("Expr: {:?}", expr);
//     //     expr
//     // }

//     // fn logical_expression(&mut self) -> Result<Option<Box<Expr>>> {
//     //     trace!("Entered logical_expression");
//     //     Ok(None)
//     // }

//     // fn term(&mut self) -> Result<Option<Box<Expr>>> {
//     //     trace!("Entered term");

//     //     Ok(None)
//     // }

//     // fn factor(&mut self) -> Result<Option<Box<Expr>>> {
//     //     trace!("Entered factor");
//     //     Ok(None)
//     // }

//     // fn argument_list(&mut self) -> Result<ArgumentList> {
//     //     trace!("Entered argument_list");

//     //     Ok(None)
//     // }

//     // fn atom(&mut self) -> Result<Option<Box<Expr>>> {
//     //     trace!("Entered atom");
//     //     Ok(None)
//     // }
// }
