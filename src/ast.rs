use std::fmt;

use errors::*;

#[derive(Debug)]
pub struct StmtList {
    statements: Vec<Stmt>,
    scope: Scope,
}

impl StmtList {
    pub fn new() -> Self {
        StmtList {
            statements: Vec::new(),
            scope: Scope::new(),
        }
    }

    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.statements.push(stmt);
    }

    pub fn eval(mut self) -> Result<Value> {
        for stmt in self.statements {
            println!("{}", stmt.eval(&mut self.scope)?);
        }
        Ok(Value::Nil)
    }
}

#[derive(Debug)]
pub enum Stmt {
    Assignment(Assignment),
    Expr(Expr),
    StmtList(StmtList),
    Return(Value),
    ParameterList(ParameterList),
    Function(Function),
}

impl Stmt {
    pub fn eval(self, scope: &mut Scope) -> Result<Value> {
        match self {
            Stmt::Assignment(assignment) => assignment.eval(scope),
            Stmt::Expr(expr) => expr.eval(scope),
            Stmt::StmtList(list) => list.eval(),
            Stmt::Return(value) => Ok(value),
            Stmt::ParameterList(params) => {
                println!("Params: {:?}", params);
                Ok(Value::Nil)
            }
            Stmt::Function(_function) => {
                info!("Function!");
                Ok(Value::Nil)
            }
        }
    }
}

pub type ParameterList = Vec<Expr>;

#[derive(Debug)]
pub struct Function {
    name: String,
    body: StmtList,
    params: ParameterList,
}

impl Function {
    pub fn new(name: String, body: StmtList, params: ParameterList) -> Self {
        Function { name, body, params }
    }

    pub fn eval(self) -> Value {
        info!("Evaluating function");
        Value::Nil
    }
}

#[derive(Debug)]
pub struct Assignment {
    variable: String,
    expr: Expr,
    type_: ValueType,
}

impl Assignment {
    pub fn new(variable: String, type_: ValueType, expr: Expr) -> Self {
        Assignment {
            variable,
            type_,
            expr,
        }
    }

    pub fn eval(self, scope: &mut Scope) -> Result<Value> {
        let value = self.expr.eval(scope)?;
        scope.set_variable(self.variable, value, self.type_);
        Ok(Value::Nil)
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
    FunctionCall(String),
    Value(Value),
}

impl Expr {
    pub fn eval(self, scope: &Scope) -> Result<Value> {
        match self {
            Expr::ArithmeticExpr { operator, lhs, rhs } => {
                match operator {
                    ArithmeticOp::Add => lhs.eval(scope)? + rhs.eval(scope)?,
                    ArithmeticOp::Sub => lhs.eval(scope)? - rhs.eval(scope)?,
                    ArithmeticOp::Mult => lhs.eval(scope)? * rhs.eval(scope)?,
                    ArithmeticOp::Div => lhs.eval(scope)? / rhs.eval(scope)?,
                    ArithmeticOp::Mod => lhs.eval(scope)? % rhs.eval(scope)?,
                }
            }
            Expr::Ident(name) => {
                match scope.get_variable(&name) {
                    Some(value) => Ok(value.clone()),
                    None => Err(format!("Variable '{}' is undefined", name).into()),
                }
            }
            Expr::Value(value) => Ok(value),
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Bool(bool),
    Int32(i32),
    Float32(f32),
    String(String),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Nil => write!(f, "nil")?,
            Value::Bool(val) => write!(f, "{}", val)?,
            Value::Int32(val) => write!(f, "{}", val)?,
            Value::Float32(val) => write!(f, "{}", val)?,
            Value::String(ref val) => write!(f, "{}", val)?,
        }

        Ok(())
    }
}

impl ::std::ops::Add for Value {
    type Output = Result<Value>;

    fn add(self, other: Value) -> Self::Output {
        use self::Value::*;

        match (self, other) {
            (Int32(a), Int32(b)) => Ok(Int32(a + b)),
            (Float32(a), Float32(b)) => Ok(Float32(a + b)),
            (Int32(_), Float32(_)) |
            (Float32(_), Int32(_)) => Err("Cannot add an integer with a float".into()),
            (Int32(_), Bool(_)) |
            (Bool(_), Int32(_)) => Err("Cannot add an integer with a bool".into()),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) => Err("Cannot add a float with a bool".into()),
            (Bool(_), Bool(_)) => Err("Cannot add bools".into()),
            _ => Err("Cannot add with nil".into()),
        }
    }
}

impl ::std::ops::Sub for Value {
    type Output = Result<Value>;

    fn sub(self, other: Value) -> Self::Output {
        use self::Value::*;

        match (self, other) {
            (Int32(a), Int32(b)) => Ok(Int32(a - b)),
            (Float32(a), Float32(b)) => Ok(Float32(a - b)),
            (Int32(_), Float32(_)) |
            (Float32(_), Int32(_)) => Err("Cannot subtract an int from a bool".into()),
            (Int32(_), Bool(_)) |
            (Bool(_), Int32(_)) => Err("Cannot subtract an integer with a bool".into()),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) => Err("Cannot subtract a float with a bool".into()),
            (Bool(_), Bool(_)) => Err("Cannot subtract bools".into()),
            _ => Err("Cannot subtract with nil".into()),
        }
    }
}

impl ::std::ops::Mul for Value {
    type Output = Result<Value>;

    fn mul(self, other: Value) -> Self::Output {
        use self::Value::*;

        match (self, other) {
            (Int32(a), Int32(b)) => Ok(Int32(a * b)),
            (Float32(a), Float32(b)) => Ok(Float32(a * b)),
            (Int32(_), Float32(_)) |
            (Float32(_), Int32(_)) => Err("Cannot multiply an int with a float".into()),
            (Int32(_), Bool(_)) |
            (Bool(_), Int32(_)) => Err("Cannot multiply an integer with a bool".into()),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) => Err("Cannot multiply a float with a bool".into()),
            (Bool(_), Bool(_)) => Err("Cannot multiply bools".into()),
            _ => Err("Cannot multiply with nil".into()),
        }
    }
}

impl ::std::ops::Div for Value {
    type Output = Result<Value>;

    fn div(self, other: Value) -> Self::Output {
        use self::Value::*;

        match (self, other) {
            (Int32(a), Int32(b)) => Ok(Int32(a / b)),
            (Float32(a), Float32(b)) => Ok(Float32(a / b)),
            (Int32(_), Float32(_)) |
            (Float32(_), Int32(_)) => Err("Cannot divide an int with a float".into()),
            (Int32(_), Bool(_)) |
            (Bool(_), Int32(_)) => Err("Cannot divide an integer with a bool".into()),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) => Err("Cannot divide a float with a bool".into()),
            (Bool(_), Bool(_)) => Err("Cannot divide bools".into()),
            _ => Err("Cannot divide with nil".into()),
        }
    }
}

impl ::std::ops::Rem for Value {
    type Output = Result<Value>;

    fn rem(self, other: Value) -> Self::Output {
        use self::Value::*;

        match (self, other) {
            (Int32(a), Int32(b)) => Ok(Int32(a % b)),
            (Float32(a), Float32(b)) => Ok(Float32(a % b)),
            (Int32(_), Float32(_)) => Err("Cannot divide a int with an float".into()),
            (Float32(_), Int32(_)) => Err("Cannot divide a float with an int".into()),
            (Int32(_), Bool(_)) => Err("Cannot divide an int with a bool".into()),
            (Bool(_), Int32(_)) => Err("Cannot divide a bool with a float".into()),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) |
            (Bool(_), Bool(_)) => Err("Cannot use modulo with bools".into()),
            _ => Err("Cannot modulo with nil".into()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ValueType {
    Bool,
    Float,
    Int,
    String,
}

#[derive(Debug)]
struct Variable {
    defined_in_scope_level: u32,
    name: String,
    type_: ValueType,
    value: Value,
}

#[derive(Debug)]
pub struct Scope {
    statements: Vec<Stmt>,
    variables: Vec<Variable>,
    current_scope_level: u32,
}

impl Scope {
    fn new() -> Self {
        Scope {
            statements: Vec::new(),
            variables: Vec::new(),
            current_scope_level: 0,
        }
    }

    fn get_variable(&self, variable: &str) -> Option<&Value> {
        for var in self.variables.iter().rev() {
            if var.name == variable {
                return Some(&var.value);
            }
        }
        None
    }

    fn set_variable(&mut self, name: String, value: Value, type_: ValueType) {
        if let Some(pos) = self.variables.iter().rev().position(|var| var.name == name) {
            if self.variables[pos].type_ == type_ {
                self.variables[pos].value = value;
            } else {
                panic!(
                    "Tried setting variable '{:?}' which is of type '{:?}' to a value of type '{:?}'",
                    self.variables[pos].name,
                    self.variables[pos].type_,
                    type_
                );
            }
        } else {
            self.variables.push(Variable {
                defined_in_scope_level: self.current_scope_level,
                name,
                value,
                type_,
            });
        }
    }

    fn push_scope_level(&mut self) {
        self.current_scope_level += 1;
    }

    fn pop_scope_level(&mut self) {
        assert!(self.current_scope_level > 0);

        let current_scope = self.current_scope_level;
        self.variables.retain(|var| {
            var.defined_in_scope_level != current_scope
        });
        self.current_scope_level -= 1;
    }
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
