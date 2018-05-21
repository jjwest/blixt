use common::Location;
use primitives::ValueKind;
use traits::{Visitable, Visitor};

use std::rc::Rc;

pub type StmtList = Vec<Stmt>;
pub type ParamList = Vec<Param>;
pub type ArgList = Vec<Expr>;

#[derive(Debug)]
pub struct Ast {
    pub statements: StmtList,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Assignment(Assignment),
    Block(StmtList),
    Decl(Decl),
    Expr(Expr),
    For(For),
    Print(Print),
    If(If),
    Return(Option<Expr>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub location: Location,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Float(f32),
    Integer(i32),
    StringLiteral(Rc<String>),
    Ident(String),
    Range(Range),
    Input(Input),
    Bool(bool),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    FunctionCall(FunctionCall),
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub ident: String,
    pub value: Expr,
    pub op: AssignmentKind,
    pub location: Location,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AssignmentKind {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub value: Box<Expr>,
    pub op: UnaryOpKind,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOpKind {
    Not,
    Neg,
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinaryOpKind,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOpKind {
    And,
    Or,
    Equal,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
    NotEqual,

    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Variable(VarDecl),
    Function(FunctionDecl),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub value: Expr,
    pub kind: ValueKind,
    // pub location: Location,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub body: StmtList,
    pub params: Vec<Param>,
    pub return_type: Option<ValueKind>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub kind: ValueKind,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Expr,
    pub body: StmtList,
    pub else_body: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct For {
    pub ident: String,
    pub range: Range,
    pub block: StmtList,
}

#[derive(Debug, Clone)]
pub struct Range {
    pub start: i32,
    pub end: i32,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Print {
    pub args: ArgList,
}

#[derive(Debug, Clone)]
pub struct Input {
    pub message: Option<Box<Expr>>,
}

impl<'a> Visitable<'a> for Ast {
    fn accept<V: Visitor<'a>>(&'a self, visitor: &mut V) {
        self.statements.accept(visitor)
    }
}

impl<'a> Visitable<'a> for StmtList {
    fn accept<V: Visitor<'a>>(&'a self, visitor: &mut V) {
        visitor.visit_stmt_list(self)
    }
}

impl<'a> Visitable<'a> for Stmt {
    fn accept<V: Visitor<'a>>(&'a self, visitor: &mut V) {
        match &self {
            Stmt::Assignment(a) => visitor.visit_assignment(a),
            Stmt::Block(a) => visitor.visit_block(a),
            Stmt::Print(a) => visitor.visit_print(a),
            Stmt::Decl(a) => visitor.visit_decl(a),
            Stmt::Expr(a) => visitor.visit_expr(a),
            Stmt::For(a) => visitor.visit_for(a),
            Stmt::If(a) => visitor.visit_if_stmt(a),
            Stmt::Return(a) => visitor.visit_return_stmt(a.as_ref()),
        }
    }
}

impl<'a> Visitable<'a> for Expr {
    fn accept<V: Visitor<'a>>(&'a self, visitor: &mut V) {
        visitor.visit_expr(self)
    }
}

impl<'a> Visitable<'a> for BinaryOp {
    fn accept<V: Visitor<'a>>(&'a self, visitor: &mut V) {
        visitor.visit_binary_op(self)
    }
}

impl<'a> Visitable<'a> for UnaryOp {
    fn accept<V: Visitor<'a>>(&'a self, visitor: &mut V) {
        visitor.visit_unary_op(self)
    }
}

impl<'a> Visitable<'a> for Decl {
    fn accept<V: Visitor<'a>>(&'a self, visitor: &mut V) {
        visitor.visit_decl(self)
    }
}

impl<'a> Visitable<'a> for If {
    fn accept<V: Visitor<'a>>(&'a self, visitor: &mut V) {
        visitor.visit_if_stmt(self)
    }
}
