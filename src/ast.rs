use std::cell::RefCell;
use std::rc::Rc;

use builtins::{Value, ValueKind};
use errors::*;

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

    pub fn eval(mut self) -> Result<()> {
        self.statements.eval(&mut self.scope)?;
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
            stmt.eval(scope)?;
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
    FunctionDeclaration(FunctionDeclaration),
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
            Stmt::FunctionDeclaration(_function) => Ok(Value::Nil),
        }
    }
}


#[derive(Debug)]
pub struct Parameter {
    name: String,
    kind: ValueKind,
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

    pub fn eval(self, scope: &mut Scope) -> Value {
        scope.add_function(self);
        Value::Nil
    }
}

pub type ArgumentList = Vec<Expr>;

#[derive(Debug)]
pub struct FunctionCall {
    name: String,
    args: ArgumentList,
}

impl FunctionCall {
    pub fn new(name: String, args: ArgumentList) -> Self {
        Self { name, args }
    }

    pub fn eval(self, scope: &mut Scope) -> Result<Value> {
        let FunctionCall { name, args } = self;
        let mut evaluated_args = Vec::with_capacity(args.len());

        for arg in args {
            evaluated_args.push(arg.eval(scope)?);
        }

        if let Some(func) = scope.functions.iter().find(|func| func.name == name) {
            for (param, arg) in func.params.iter().zip(evaluated_args.iter()) {
                match (param.kind, arg) {
                    (ValueKind::Bool, &Value::Bool(_)) |
                    (ValueKind::Float, &Value::Float32(_)) |
                    (ValueKind::Int, &Value::Int32(_)) |
                    (ValueKind::String, &Value::String(_)) => {}
                    _ => return Err(format!("Parameter '{:?}' is of type {:?}, but received an argument of type {:?}",
                                            param.name,
                                            param.kind,
                                            arg).into()),
                }
            }
            Ok(Value::Nil)
        } else {
            Err(
                format!("Tried calling non-defined function '{}'", name).into(),
            )
        }
    }
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

    pub fn eval(self, scope: &mut Scope) -> Result<Value> {
        let value = self.expr.eval(scope)?;
        scope.set_variable(self.variable, value, self.kind)?;
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
    FunctionCall(FunctionCall),
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
    kind: ValueKind,
    value: Value,
}

#[derive(Debug)]
pub struct Scope {
    statements: Vec<Stmt>,
    functions: Vec<FunctionDeclaration>,
    variables: Vec<Variable>,
    current_scope_level: u32,
}

impl Scope {
    fn new() -> Self {
        Scope {
            statements: Vec::new(),
            functions: Vec::new(),
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

    fn set_variable(&mut self, name: String, value: Value, kind: ValueKind) -> Result<Value> {
        if let Some(pos) = self.variables.iter().rev().position(|var| var.name == name) {
            match (self.variables[pos].kind, value) {
                (ValueKind::Bool, val @ Value::Bool(_)) |
                (ValueKind::Int, val @ Value::Int32(_)) |
                (ValueKind::Float, val @ Value::Float32(_)) |
                (ValueKind::String, val @ Value::String(_)) => {
                    self.variables[pos].value = val;
                    Ok(Value::Nil)
                }
                value => Err(
                    format!(
                        "Tried setting variable '{}' which is of type {:?} \
                                  with value {:?} which is of type '{:?}'",
                        name,
                        self.variables[pos].kind,
                        value,
                        kind
                    ).into(),
                ),
            }
        } else {
            self.variables.push(Variable {
                defined_in_scope_level: self.current_scope_level,
                name,
                value,
                kind,
            });
            Ok(Value::Nil)
        }
    }

    fn add_function(&mut self, mut func: FunctionDeclaration) {
        func.defined_in_scope_level = self.current_scope_level
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
