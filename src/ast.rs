pub type StmtList = Vec<Stmt>;
pub type ParamList = Vec<Param>;
pub type ArgList = Vec<Expr>;

#[derive(Debug)]
pub struct Ast {
    pub statements: StmtList,
}

#[derive(Debug)]
pub enum Stmt {
    Assignment(BinaryOp),
    Block(StmtList),
    Decl(Decl),
    Expr(Expr),
    If(If),
    Return,
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
    FunctionCall(FunctionCall),
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

#[derive(Debug)]
pub enum Decl {
    Variable {
        name: String,
        value: Expr,
        kind: ValueKind,
    },
    Function {
        name: String,
        body: StmtList,
        params: Vec<Param>,
        return_type: Option<ValueKind>,
    },
}

#[derive(Debug, PartialEq)]
pub enum ValueKind {
    Bool,
    String,
    Integer,
    Float,
    Undecided,
}

#[derive(Debug)]
pub struct If {
    pub cond: Expr,
    pub body: StmtList,
    pub else_body: Option<Box<Stmt>>,
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub kind: ValueKind,
}

#[derive(Debug)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expr>,
}
