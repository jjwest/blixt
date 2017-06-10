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

    pub fn eval(mut self) -> Result<ValueType> {
        for stmt in self.statements {
            println!("{}", stmt.eval(&mut self.scope)?);
        }
        Ok(ValueType::Nil)
    }
}

#[derive(Debug)]
pub enum Stmt {
    Assignment { variable: String, expr: Expr },
    Expr(Expr),
    StmtList(StmtList),
    Return(ValueType),
}

impl Stmt {
    pub fn eval(self, scope: &mut Scope) -> Result<ValueType> {
        match self {
            Stmt::Assignment { variable, expr } => {
                let value = expr.eval(scope)?;
                scope.set_variable(variable, value);
                Ok(ValueType::Nil)
            }
            Stmt::Expr(expr) => expr.eval(scope),
            Stmt::StmtList(list) => list.eval(),
            Stmt::Return(value) => Ok(value),
        }
    }
}

#[derive(Debug)]
<<<<<<< variant A
struct Assignment {
    variable: String,
    value: ValueType,
}

impl Assignment {
    fn eval(self, scope: &mut Scope) -> Result<ValueType> {
        scope.set_variable(self.variable, self.value);
        Ok(ValueType::Nil)
    }
}

#[derive(Debug)]
>>>>>>> variant B
======= end
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
    Value(ValueType),
}

impl Expr {
    fn eval(self, scope: &Scope) -> Result<ValueType> {
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
                    None => Err(format!("Variable {} is undefined", name).into()),
                }
            }
            Expr::Value(value) => Ok(value),
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ValueType {
    Bool(bool),
    Int32(i32),
    Float32(f32),
    Nil,
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ValueType::Bool(val) => write!(f, "{}", val)?,
            ValueType::Int32(val) => write!(f, "{}", val)?,
            ValueType::Float32(val) => write!(f, "{}", val)?,
            ValueType::Nil => write!(f, "nil")?,
        }

        Ok(())
    }
}

impl ::std::ops::Add for ValueType {
    type Output = Result<ValueType>;

    fn add(self, other: ValueType) -> Self::Output {
        use self::ValueType::*;

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

impl ::std::ops::Sub for ValueType {
    type Output = Result<ValueType>;

    fn sub(self, other: ValueType) -> Self::Output {
        use self::ValueType::*;

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

impl ::std::ops::Mul for ValueType {
    type Output = Result<ValueType>;

    fn mul(self, other: ValueType) -> Self::Output {
        use self::ValueType::*;

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

impl ::std::ops::Div for ValueType {
    type Output = Result<ValueType>;

    fn div(self, other: ValueType) -> Self::Output {
        use self::ValueType::*;

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

impl ::std::ops::Rem for ValueType {
    type Output = Result<ValueType>;

    fn rem(self, other: ValueType) -> Self::Output {
        use self::ValueType::*;

        match (self, other) {
            (Int32(a), Int32(b)) => Ok(Int32(a % b)),
            (Float32(a), Float32(b)) => Ok(Float32(a % b)),
            (Int32(_), Float32(_)) |
            (Float32(_), Int32(_)) => Err("Cannot divide a float with an int".into()),
            (Int32(_), Bool(_)) |
            (Bool(_), Int32(_)) => Err("Cannot divide a float with a bool".into()),
            (Float32(_), Bool(_)) |
            (Bool(_), Float32(_)) |
            (Bool(_), Bool(_)) => Err("Cannot use modulo with bools".into()),
            _ => Err("Cannot modulo with nil".into()),
        }
    }
}

#[derive(Debug)]
struct Variable {
    defined_in_scope_level: u32,
    name: String,
    value: ValueType,
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

    fn get_variable(&self, variable: &str) -> Option<&ValueType> {
        for var in &self.variables {
            if var.name == variable {
                return Some(&var.value);
            }
        }
        None
    }

    fn set_variable(&mut self, name: String, value: ValueType) {
        if let Some(pos) = self.variables
               .iter()
               .position(|var| var.name == name) {
            self.variables[pos].value = value;
        } else {
            self.variables
                .push(
                    Variable {
                        defined_in_scope_level: self.current_scope_level,
                        name,
                        value,
                    },
                );
        }
    }

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
