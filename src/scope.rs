use ast::FunctionDecl;
use primitives::{Value, ValueKind};

use std::collections::HashMap;

pub type FuncName<'ast> = &'ast str;

pub struct Scope<'ast> {
    scopes: Vec<InnerScope<'ast>>,
    curr_scope: usize,
}

struct InnerScope<'ast> {
    functions: HashMap<FuncName<'ast>, &'ast FunctionDecl>,
    variables: Vec<Variable<'ast>>,
    level: usize,
    parent: Option<usize>,
}

impl<'ast> InnerScope<'ast> {
    pub fn new(parent: Option<usize>) -> InnerScope<'ast> {
        InnerScope {
            functions: HashMap::new(),
            variables: Vec::new(),
            level: 0,
            parent,
        }
    }
}

pub struct Variable<'ast> {
    pub name: &'ast str,
    pub kind: ValueKind,
    pub value: Value,
    pub defined_in_scope_level: usize,
}

impl<'ast> Scope<'ast> {
    pub fn new() -> Scope<'ast> {
        Scope {
            scopes: vec![InnerScope::new(None)],
            curr_scope: 0,
        }
    }

    pub fn new_scope(&mut self) {
        self.scopes.push(InnerScope::new(Some(self.curr_scope)));
        self.curr_scope += 1;
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
        self.curr_scope -= 1;
    }

    pub fn new_scope_level(&mut self) {
        self.scopes[self.curr_scope].level += 1;
    }

    pub fn pop_scope_level(&mut self) {
        let curr_scope = &mut self.scopes[self.curr_scope];
        let curr_level = curr_scope.level;
        curr_scope
            .variables
            .retain(|var| var.defined_in_scope_level != curr_level);
        curr_scope.level -= 1;
    }

    pub fn add_variable(&mut self, name: &'ast str, value: Value, kind: ValueKind) {
        let curr_scope = &mut self.scopes[self.curr_scope];
        curr_scope.variables.push(Variable {
            name,
            value,
            kind,
            defined_in_scope_level: curr_scope.level,
        })
    }

    pub fn get_variable(&mut self, name: &'ast str) -> Option<&Variable> {
        let mut scope = &self.scopes[self.curr_scope];

        loop {
            for var in scope.variables.iter().rev() {
                if var.name == name {
                    return Some(var);
                }
            }

            if let Some(parent) = scope.parent {
                scope = &self.scopes[parent];
            } else {
                break;
            }
        }

        None
    }

    pub fn get_variable_mut(&mut self, name: &str) -> Option<&mut Value> {
        let mut scope = &mut self.scopes[self.curr_scope];

        loop {
            for var in scope.variables.iter_mut().rev() {
                if var.name == name {
                    return Some(&mut var.value);
                }
            }

            if let Some(parent) = scope.parent {
                scope = &mut self.scopes[parent];
            } else {
                break;
            }
        }

        None
    }

    pub fn add_function(&mut self, func: &'ast FunctionDecl) {
        let scope = &mut self.scopes[self.curr_scope];
        scope.functions.insert(func.name.as_str(), func);
    }

    pub fn get_function(&mut self, name: &str) -> Option<&'ast FunctionDecl> {
        let mut scope = &mut self.scopes[self.curr_scope];

        loop {
            if let Some(func) = scope.functions.get(name) {
                return Some(func);
            }

            if let Some(parent) = scope.parent {
                scope = &mut self.scopes[parent];
            } else {
                return None;
            }
        }
    }
}
