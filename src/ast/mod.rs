#[derive(Debug)]
pub struct Ast {
    pub statements: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Assignment(BinaryOp),
    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Float(f32),
    Integer(i32),
    StringLiteral(String),
    Ident(String),
    Bool(bool),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
}

#[derive(Debug)]
pub struct UnaryOp {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: UnaryOpKind,
}

#[derive(Debug)]
pub enum UnaryOpKind {
    Not,
    Minus,
}

#[derive(Debug)]
pub struct BinaryOp {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinaryOpKind,
}

#[derive(Debug)]
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
