pub struct StmtList {
    statements: Vec<Stmt>,
}

impl StmtList {
    pub fn new() -> Self {
        StmtList { statements: Vec::new() }
    }

    pub fn add_stmt(&mut self, kind: StmtKind) {
        self.statements
            .push(Stmt { kind });
    }

    pub fn eval(&mut self) {
        for stmt in &mut self.statements {
            stmt.eval();
        }
    }
}

pub struct Stmt {
    kind: StmtKind,
}

impl Stmt {
    pub fn eval(&mut self) {}
}

pub enum StmtKind {
    Expr(Expr),
    StmtList(StmtList),
    Return,
}

pub enum Expr {
    LogicalExpr {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        operator: LogicOp,
    },
    ArithmeticExpr {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        operator: ArithmeticOp,
    },
    Ident(String),
    FunctionCall(String),
    Value(ValueType),
}


pub enum ValueType {
    Bool(bool),
    Int32(i32),
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

enum Numeral {
    Int32(i32),
    Float32(f32),
}

pub enum ArithmeticOp {
    Add,
    Sub,
    Div,
    Mult,
    Mod,
}

pub enum LogicOp {
    Lesser,
    Greater,
    Equal,
    LesserEqual,
    GreaterEqual,
}

pub enum UnaryOp {
    Not,
}
