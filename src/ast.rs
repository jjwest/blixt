use builtins::Value;
use traits::{AstVisitor, Visitable};

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
    If(If),
    Return(Option<Expr>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Float(f32),
    Integer(i32),
    StringLiteral(Rc<String>),
    Ident(String),
    Bool(bool),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    FunctionCall(FunctionCall),
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub ident: String,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub expr: Box<Expr>,
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

    Assign,
    AddAssign,
    DivAssign,
    MulAssign,
    SubAssign,
    ModAssign,
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
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub body: StmtList,
    pub params: Vec<Param>,
    pub return_type: Option<ValueKind>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueKind {
    Bool,
    String,
    Integer,
    Float,
    Undecided,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Expr,
    pub body: StmtList,
    pub else_body: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub kind: ValueKind,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expr>,
}

impl Visitable for Ast {
    fn accept<V: AstVisitor>(&mut self, visitor: &mut V) -> Value {
        self.statements.accept(visitor)
    }
}

impl Visitable for StmtList {
    fn accept<V: AstVisitor>(&mut self, visitor: &mut V) -> Value {
        visitor.visit_stmt_list(self)
    }
}

impl Visitable for Stmt {
    fn accept<V: AstVisitor>(&mut self, visitor: &mut V) -> Value {
        match self {
            Stmt::Assignment(a) => visitor.visit_assignment(a),
            Stmt::Block(a) => visitor.visit_block(a),
            Stmt::Decl(a) => visitor.visit_decl(a),
            Stmt::Expr(a) => visitor.visit_expr(a),
            Stmt::If(a) => visitor.visit_if_stmt(a),
            Stmt::Return(a) => visitor.visit_return_stmt(a.as_mut()),
            _ => Value::Nil,
        }
    }
}

impl Visitable for Expr {
    fn accept<V: AstVisitor>(&mut self, visitor: &mut V) -> Value {
        visitor.visit_expr(self)
    }
}

impl Visitable for BinaryOp {
    fn accept<V: AstVisitor>(&mut self, visitor: &mut V) -> Value {
        visitor.visit_binary_op(self)
    }
}

impl Visitable for UnaryOp {
    fn accept<V: AstVisitor>(&mut self, visitor: &mut V) -> Value {
        visitor.visit_unary_op(self)
    }
}

impl Visitable for Decl {
    fn accept<V: AstVisitor>(&mut self, visitor: &mut V) -> Value {
        visitor.visit_decl(self)
    }
}

impl Visitable for FunctionCall {
    fn accept<V: AstVisitor>(&mut self, visitor: &mut V) -> Value {
        visitor.visit_funcall(self)
    }
}

impl Visitable for If {
    fn accept<V: AstVisitor>(&mut self, visitor: &mut V) -> Value {
        visitor.visit_if_stmt(self)
    }
}
