use log::trace;

use crate::ast::{
    ArgList, Assignment, AssignmentKind, Ast, AstNodeId, BinaryOp,
    BinaryOpKind, Decl, Expr, ExprKind, For, FunctionCall, FunctionDecl, If,
    Input, ParamList, Print, Return, Stmt, StmtList, UnaryOp,
};
use crate::common::{Context, Symbol};
use crate::location::Location;

use crate::primitives::{Value, ValueKind};
use crate::scope::Scope;

pub fn typecheck(ast: &Ast, context: &mut Context) -> Result<(), ()> {
    let mut checker = Typechecker {
        ast,
        check_passed: true,
        scope: Scope::new(),
        context,
        location: vec![],
        deferred_function_calls: vec![],
        current_function: None,
    };

    checker.check_stmt_list(&ast.statements);

    if checker.check_passed {
        Ok(())
    } else {
        Err(())
    }
}

struct Typechecker<'ctxt> {
    ast: &'ctxt Ast,
    check_passed: bool,
    scope: Scope,
    context: &'ctxt mut Context,
    location: Vec<Location>,
    deferred_function_calls: Vec<FunctionCall>,
    current_function: Option<FunctionDecl>,
}

impl<'ctxt> Typechecker<'ctxt> {
    fn report_error(&mut self, message: &str) {
        self.check_passed = false;
        self.context
            .report_error(message, self.location[self.location.len() - 1]);
    }

    fn check_stmt_list(&mut self, stmts: &StmtList) {
        trace!("stmt_list");

        use Stmt::*;

        for stmt in stmts {
            match &self.ast.arena[*stmt] {
                Assignment(v) => unimplemented!(),
                Block(v) => unimplemented!(),
                Decl(v) => unimplemented!(),
                Expr(v) => self.check_expr(v),
                For(v) => unimplemented!(),
                Print(v) => unimplemented!(),
                If(v) => unimplemented!(),
                Return(v) => unimplemented!(),
                Param(v) => unimplemented!(),
            };
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> ValueKind {
        trace!("Expr");

        use ExprKind::*;

        self.location.push(expr.location);

        let kind = match &expr.kind {
            Bool(_) => ValueKind::Bool,
            Float(_) => ValueKind::Float,
            Integer(_) => ValueKind::Integer,
            StringLiteral(_) => ValueKind::String,
            Ident(v) => self.check_ident(*v),
            Range(_) => unimplemented!(),
            Input(_) => unimplemented!(),
            UnaryOp(_) => unimplemented!(),
            BinaryOp(v) => self.check_binop(v),
            FunctionCall(_) => unimplemented!(),
        };

        self.location.pop();

        kind
    }

    fn check_binop(&mut self, binop: &BinaryOp) -> ValueKind {
        trace!("Binop");

        use BinaryOpKind::*;
        use ValueKind::*;

        let lhs = self.check_expr(self.ast.arena[binop.lhs].expr());
        let rhs = self.check_expr(self.ast.arena[binop.rhs].expr());

        match binop.op {
            And | Or | Equal | Greater | GreaterEqual | Lesser
            | LesserEqual | NotEqual => Bool,

            Field => unimplemented!(),

            Add | Sub | Mul | Div | Mod => match (lhs, rhs) {
                (Bool, Bool) => Bool,
                (Integer, Integer) => Integer,
                (String, String) => String,
                (Float, Float) => Float,
                (Integer, Float) | (Float, Integer) => Float,
                (Nil, other) | (other, Nil) => other,
                (a, b) => {
                    self.report_error(&format!(
                        "Invalid types {:?}, {:?} for operator {:?}",
                        a, b, binop.op
                    ));
                    Nil
                }
            },
        }
    }

    fn check_ident(&mut self, ident: Symbol) -> ValueKind {
        trace!("ident");

        match self.scope.get_variable(ident) {
            Some(var) => var.kind,
            None => {
                self.report_error(&format!(
                    "Variable '{}' is undefined",
                    self.context.interner.get(ident)
                ));
                ValueKind::Nil
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arithmetic_expr() {}
}
