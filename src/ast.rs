use crate::arena::{Arena, Id};
use crate::common::Symbol;
use crate::location::Location;
use crate::primitives::ValueKind;

pub type AstNodeId = Id;

pub type ArgList = Vec<AstNodeId>;
pub type ParamList = Vec<AstNodeId>;
pub type StmtList = Vec<AstNodeId>;

pub struct Ast {
    pub arena: Arena<Stmt>,
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
    Return(Return),
    Param(Param),
}

macro_rules! accessor {
    ($name:ident, $type:ty, $pattern:pat, $value:expr) => {
        impl Stmt {
            pub fn $name(&self) -> &$type {
                match self {
                    $pattern => $value,
                    _ => panic!(concat!("Node not a ", stringify!(name))),
                }
            }
        }
    };
}

accessor!(assignment, Assignment, Stmt::Assignment(a), a);
accessor!(expr, Expr, Stmt::Expr(a), a);

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Option<AstNodeId>,
    pub location: Location,
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
    StringLiteral(Symbol),
    Ident(Symbol),
    Range(Range),
    Input(Input),
    Bool(bool),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    FunctionCall(FunctionCall),
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub ident: Symbol,
    pub value: AstNodeId,
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
    pub value: AstNodeId,
    pub op: UnaryOpKind,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOpKind {
    Not,
    Neg,
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub lhs: AstNodeId,
    pub rhs: AstNodeId,
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

    Field,

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
    Struct(StructDecl),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: Symbol,
    pub value: AstNodeId,
    pub kind: ValueKind,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub name: Symbol,
    pub fields: Vec<AstNodeId>,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: Symbol,
    pub body: StmtList,
    pub params: Vec<AstNodeId>,
    pub return_type: Option<ValueKind>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Symbol,
    pub kind: ValueKind,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: AstNodeId,
    pub body: StmtList,
    pub else_body: Option<StmtList>,
}

#[derive(Debug, Clone)]
pub struct For {
    pub ident: Symbol,
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
    pub name: Symbol,
    pub args: Vec<AstNodeId>,
}

#[derive(Debug, Clone)]
pub struct Print {
    pub args: ArgList,
}

#[derive(Debug, Clone)]
pub struct Input {
    pub message: Option<AstNodeId>,
}
