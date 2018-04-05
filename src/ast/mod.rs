pub struct Ast {
    pub statements: Vec<Stmt>,
}

pub enum Stmt {
    Expr(Expr),
}

pub enum Expr {
    Float(f32),
    Integer(i32),
    String(String),
    Ident(String),
    Bool(bool),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
}

pub enum UnaryOp {
    Not,
    Minus,
}

pub enum BinaryOp {
    And,
    Or,
    Equal,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
    NotEqual,
}
