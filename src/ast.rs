use builtins::{Value, ValueKind};
use errors::*;

#[derive(Debug)]
pub struct AbstractSyntaxTree {
    global_scope: Scope,
    statements: StmtList,
}

impl AbstractSyntaxTree {
    pub fn new() -> AbstractSyntaxTree {
        AbstractSyntaxTree {
            global_scope: Scope::new(),
            statements: StmtList::new(),
        }
    }

    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.statements.0.push(stmt);
    }

    pub fn eval(mut self) -> Result<()> {
        self.statements.eval(&mut self.global_scope)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct StmtList(pub Vec<Stmt>);

impl StmtList {
    pub fn new() -> Self {
        StmtList(Vec::new())
    }

    pub fn eval(self, scope: &mut Scope) -> Result<Value> {
        for stmt in self.0 {
            println!("{}", stmt.eval(scope)?);
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
            Stmt::StmtList(list) => list.eval(scope),
            Stmt::Return(value) => Ok(value),
            Stmt::ParameterList(params) => {
                println!("Params: {:?}", params);
                Ok(Value::Nil)
            }
            Stmt::Function(_function) => Ok(Value::Nil),
        }
    }
}


#[derive(Debug)]
pub struct Parameter {
    name: String,
    type_: ValueKind,
}

impl Parameter {
    pub fn new(name: String, type_: ValueKind) -> Self {
        Self { name, type_ }
    }
}

pub type ParameterList = Vec<Parameter>;

#[derive(Debug)]
pub struct Function {
    name: String,
    body: StmtList,
    params: ParameterList,
    return_type: Option<ValueKind>,
    scope: Scope,
}

impl Function {
    pub fn new(
        name: String,
        body: StmtList,
        params: ParameterList,
        return_type: Option<ValueKind>,
    ) -> Self {
        Function {
            name,
            body,
            params,
            return_type,
            scope: Scope::new(),
        }
    }

    pub fn eval(self, _scope: &mut Scope) -> Value {
        Value::Nil
    }
}

#[derive(Debug)]
pub struct Assignment {
    variable: String,
    expr: Expr,
    type_: ValueKind,
}

impl Assignment {
    pub fn new(variable: String, type_: ValueKind, expr: Expr) -> Self {
        Assignment {
            variable,
            type_,
            expr,
        }
    }

    pub fn eval(self, scope: &mut Scope) -> Result<Value> {
        let value = self.expr.eval(scope)?;
        scope.set_variable(self.variable, value, self.type_)?;
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
            Expr::Ident(name) => scope.get_variable(&name),
            Expr::Value(value) => Ok(value),
            _ => unimplemented!(),
        }
    }
}


#[derive(Debug)]
pub struct Variable {
    defined_in_scope_level: u32,
    name: String,
    type_: ValueKind,
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

    fn get_variable(&self, variable: &str) -> Result<Value> {
        for var in self.variables.iter().rev() {
            if var.name == variable {
                return Ok(var.value.clone());
            }
        }
        Err(format!("Variable '{}' is undefined", variable).into())
    }

    fn set_variable(&mut self, name: String, value: Value, type_: ValueKind) -> Result<Value> {
        if let Some(pos) = self.variables.iter().rev().position(|var| var.name == name) {
            if self.variables[pos].type_ == type_ {
                self.variables[pos].value = value;
                Ok(Value::Nil)
            } else {
                Err(format!("Tried setting variable '{:?}' which is of type '{:?}' to a value of type '{:?}'",
                            self.variables[pos].name,
                            self.variables[pos].type_,
                            type_).into())
            }
        } else {
            self.variables.push(Variable {
                defined_in_scope_level: self.current_scope_level,
                name,
                value,
                type_,
            });
            Ok(Value::Nil)
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
