#[derive(Debug)]
pub struct StmtList {
    statements: Vec<Stmt>,
}

impl StmtList {
    pub fn new() -> Self {
        StmtList { statements: Vec::new() }
    }

    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.statements.push(stmt);
    }

    pub fn eval(&mut self) {
        for stmt in &mut self.statements {
            stmt.eval();
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    StmtList(StmtList),
    Return,
}

impl Stmt {
    pub fn eval(&mut self) {}
}

#[derive(Debug)]
pub enum Expr {
    LogicalExpr {
        operator: LogicOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    ArithmeticExpr {
        operator: ArithmeticOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Ident(String),
    FunctionCall(String),
    Value(ValueType),
}


#[derive(Debug)]
pub enum ValueType {
    Bool(bool),
    Int32(i32),
    Float32(f32),
}

struct Variable {
    defined_in_scope_level: u32,
    name: String,
    value: ValueType,
}

struct Scope {
    statements: Vec<Stmt>,
    variables: Vec<Variable>,
    current_scope_level: u32,
}

impl Scope {
    fn push_scope_level(&mut self) {
        self.current_scope_level += 1;
    }

    fn pop_scope_level(&mut self) {
        assert!(self.current_scope_level > 0);

        let current_scope = self.current_scope_level;
        self.variables
            .retain(|var| var.defined_in_scope_level != current_scope);
        self.current_scope_level -= 1;
    }
}

#[derive(Debug)]
enum Numeral {
    Int32(i32),
    Float32(f32),
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
