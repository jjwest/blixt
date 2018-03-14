mod scope;

use failure;

use builtins::{Value, ValueKind};
use self::scope::Scope;

#[derive(Debug)]
pub struct AbstractSyntaxTree {
    scope: Scope,
    statements: StmtList,
}

impl AbstractSyntaxTree {
    pub fn new() -> AbstractSyntaxTree {
        AbstractSyntaxTree {
            scope: Scope::new(),
            statements: StmtList::new(),
        }
    }

    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.statements.0.push(stmt);
    }
}

#[derive(Debug)]
pub struct StmtList(pub Vec<Stmt>);

impl StmtList {
    pub fn new() -> Self {
        StmtList(Vec::new())
    }
}

#[derive(Debug)]
pub enum Stmt {
    Assignment(Assignment),
    Expr(Expr),
    StmtList(StmtList),
    Return(Value),
    ParameterList(ParameterList),
    FunctionDeclaration(FunctionDeclaration),
}

#[derive(Debug)]
pub struct Parameter {
    pub name: String,
    pub kind: ValueKind,
}

impl Parameter {
    pub fn new(name: String, kind: ValueKind) -> Self {
        Self { name, kind }
    }
}

pub type ParameterList = Vec<Parameter>;

#[derive(Debug)]
pub struct FunctionDeclaration {
    name: String,
    body: StmtList,
    params: ParameterList,
    return_type: Option<ValueKind>,
    defined_in_scope_level: u32,
}

impl FunctionDeclaration {
    pub fn new(
        name: String,
        body: StmtList,
        params: ParameterList,
        return_type: Option<ValueKind>,
    ) -> Self {
        FunctionDeclaration {
            name,
            body,
            params,
            return_type,
            defined_in_scope_level: 0,
        }
    }
}

pub type ArgumentList = Vec<Expr>;

#[derive(Debug)]
pub struct FunctionCall {
    name: String,
    args: ArgumentList,
}

#[derive(Debug)]
pub struct Assignment {
    variable: String,
    expr: Expr,
    kind: ValueKind,
}

impl Assignment {
    pub fn new(variable: String, kind: ValueKind, expr: Expr) -> Self {
        Assignment {
            variable,
            kind,
            expr,
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    ArithmeticExpr {
        operator: ArithmeticOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    LogicalExpr {
        operator: LogicOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Ident(String),
    FunctionCall(FunctionCall),
    Value(Value),
}

#[derive(Debug)]
pub struct Variable {
    defined_in_scope_level: u32,
    name: String,
    kind: ValueKind,
    value: Value,
}

#[derive(Debug)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Div,
    Mult,
    Mod,
}

#[derive(Debug)]
pub enum LogicOp {
    Lesser,
    Greater,
    Equal,
    LesserEqual,
    GreaterEqual,
}

#[derive(Debug)]
pub enum UnaryOp {
    Not,
}
