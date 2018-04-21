use ast::{Assignment, BinaryOp, Decl, Expr, FunctionCall, If, Stmt, StmtList, UnaryOp};
use builtins::Value;

pub trait AstVisitor {
    fn visit_stmt_list(&mut self, node: &mut StmtList) -> Value;
    fn visit_stmt(&mut self, node: &mut Stmt) -> Value;
    fn visit_expr(&mut self, node: &mut Expr) -> Value;
    fn visit_decl(&mut self, node: &mut Decl) -> Value;
    fn visit_binary_op(&mut self, node: &mut BinaryOp) -> Value;
    fn visit_unary_op(&mut self, node: &mut UnaryOp) -> Value;
    fn visit_funcall(&mut self, node: &mut FunctionCall) -> Value;
    fn visit_if_stmt(&mut self, node: &mut If) -> Value;
    fn visit_ident(&mut self, node: &mut String) -> Value;
    fn visit_return_stmt(&mut self, node: Option<&mut Expr>) -> Value;
    fn visit_block(&mut self, node: &mut StmtList) -> Value;
    fn visit_assignment(&mut self, node: &mut Assignment) -> Value;
}

pub trait Visitable {
    fn accept<V: AstVisitor>(&mut self, visitor: &mut V) -> Value;
}
