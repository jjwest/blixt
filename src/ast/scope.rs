use ast::{FunctionDeclaration, Stmt, Variable};
use builtins::{Value, ValueKind};
use errors::*;

#[derive(Debug)]
pub struct Scope {
    statements: Vec<Stmt>,
    functions: Vec<FunctionDeclaration>,
    variables: Vec<Variable>,
    current_scope_level: u32,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            statements: Vec::new(),
            functions: Vec::new(),
            variables: Vec::new(),
            current_scope_level: 0,
        }
    }

    pub fn get_variable(&self, variable: &str) -> Result<Value> {
        for var in self.variables.iter().rev() {
            if var.name == variable {
                return Ok(var.value.clone());
            }
        }
        Err(format_err!("Variable '{}' is undefined", variable))
    }

    pub fn set_variable(&mut self, name: String, value: Value, kind: ValueKind) -> Result<Value> {
        if let Some(pos) = self.variables.iter().rev().position(|var| var.name == name) {
            match (self.variables[pos].kind, value) {
                (ValueKind::Bool, val @ Value::Bool(_)) |
                (ValueKind::Int, val @ Value::Int32(_)) |
                (ValueKind::Float, val @ Value::Float32(_)) |
                (ValueKind::String, val @ Value::String(_)) => {
                    self.variables[pos].value = val;
                    Ok(Value::Nil)
                }
                value => Err(format_err!(
                    "Tried setting variable '{}' which is of type {:?} \
                                  with value {:?} which is of type '{:?}'",
                    name,
                    self.variables[pos].kind,
                    value,
                    kind
                )),
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

    pub fn add_function(&mut self, mut func: FunctionDeclaration) {
        func.defined_in_scope_level = self.current_scope_level
    }

    pub fn push_scope_level(&mut self) {
        self.current_scope_level += 1;
    }

    pub fn pop_scope_level(&mut self) {
        assert!(self.current_scope_level > 0);

        let current_scope = self.current_scope_level;
        self.variables.retain(|var| {
            var.defined_in_scope_level != current_scope
        });
        self.current_scope_level -= 1;
    }
}
