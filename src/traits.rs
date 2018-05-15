use ast::{
    Assignment, BinaryOp, Decl, Expr, For, FunctionCall, If, Input, Print, Stmt, StmtList, UnaryOp,
};

pub trait Visitor<'ast> {
    fn visit_stmt_list(&mut self, node: &'ast StmtList);
    fn visit_stmt(&mut self, node: &'ast Stmt);
    fn visit_expr(&mut self, node: &'ast Expr);
    fn visit_decl(&mut self, node: &'ast Decl);
    fn visit_binary_op(&mut self, node: &'ast BinaryOp);
    fn visit_unary_op(&mut self, node: &'ast UnaryOp);
    fn visit_funcall(&mut self, node: &'ast FunctionCall);
    fn visit_if_stmt(&mut self, node: &'ast If);
    fn visit_ident(&mut self, node: &'ast String);
    fn visit_return_stmt(&mut self, node: Option<&'ast Expr>);
    fn visit_block(&mut self, node: &'ast StmtList);
    fn visit_assignment(&mut self, node: &'ast Assignment);
    fn visit_print(&mut self, node: &'ast Print);
    fn visit_input(&mut self, node: &'ast Input);
    fn visit_for(&mut self, node: &'ast For);
}

pub trait Visitable<'a> {
    fn accept<V: Visitor<'a>>(&'a self, visitor: &mut V);
}
