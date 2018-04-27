use ast::{Assignment, AssignmentKind, BinaryOp, BinaryOpKind, Decl, Expr, For, FunctionCall,
          FunctionDecl, If, Input, Print, Stmt, StmtList, UnaryOp, UnaryOpKind, VarDecl};
use builtins::{Value, ValueKind};
use traits::{AstVisitor, Visitable};

use std::collections::HashMap;

pub struct Typecheck {
    checked_vars: HashMap<String, ValueKind>,
}

impl AstVisitor for Typecheck {
    fn visit_stmt_list(&mut self, node: &mut StmtList) -> Value {
        for stmt in node {
            stmt.accept(self);
        }

        Value::Nil
    }

    fn visit_stmt(&mut self, node: &mut Stmt) -> Value {
        node.accept(self)
    }

    fn visit_expr(&mut self, node: &mut Expr) -> Value {
        match node {
            Expr::Bool(n) => Value::Bool(*n),
            Expr::Float(n) => Value::Float(*n),
            Expr::Integer(n) => Value::Int(*n),
            Expr::StringLiteral(n) => Value::String(*n),
            Expr::Ident(n) => self.visit_ident(n),
            Expr::UnaryOp(n) => self.visit_unary_op(n),
            Expr::BinaryOp(n) => self.visit_binary_op(n),
            Expr::FunctionCall(n) => self.visit_funcall(n),
        }
    }

    fn visit_decl(&mut self, node: &mut Decl) -> Value {
        match node {
            Decl::Variable(VarDecl { value, kind, .. }) => {
                if *kind == ValueKind::Undecided {
                    match value.accept(self) {
                        Value::Bool(_) => *kind = ValueKind::Bool,
                        Value::String(_) => *kind = ValueKind::String,
                        Value::Int(_) => *kind = ValueKind::Integer,
                        Value::Float(_) => *kind = ValueKind::Float,
                        _ => unreachable!(),
                    }
                }
            }
            Decl::Function(FunctionDecl {
                params,
                return_type,
                ..
            }) => {}
        }

        Value::Nil
    }

    fn visit_binary_op(&mut self, node: &mut BinaryOp) -> Value {
        Value::Nil
    }

    fn visit_unary_op(&mut self, node: &mut UnaryOp) -> Value {
        Value::Nil
    }

    fn visit_funcall(&mut self, node: &mut FunctionCall) -> Value {
        Value::Nil
    }

    fn visit_if_stmt(&mut self, node: &mut If) -> Value {
        Value::Nil
    }

    fn visit_ident(&mut self, node: &mut String) -> Value {
        Value::Nil
    }

    fn visit_return_stmt(&mut self, node: Option<&mut Expr>) -> Value {
        Value::Nil
    }

    fn visit_block(&mut self, node: &mut StmtList) -> Value {
        Value::Nil
    }

    fn visit_assignment(&mut self, node: &mut Assignment) -> Value {
        Value::Nil
    }

    fn visit_print(&mut self, node: &mut Print) -> Value {
        Value::Nil
    }

    fn visit_input(&mut self, node: &mut Input) -> Value {
        Value::Nil
    }

    fn visit_for(&mut self, node: &mut For) -> Value {
        Value::Nil
    }
}
