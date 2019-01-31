use hashbrown::HashMap;

use crate::ast::AstNodeId;
use crate::common::Symbol;
use crate::primitives::{Value, ValueKind};

pub struct Scope {
    scopes: Vec<InnerScope>,
    curr_scope: usize,
}

struct InnerScope {
    functions: HashMap<Symbol, AstNodeId>,
    user_defined_types: HashMap<Symbol, AstNodeId>,
    variables: Vec<Variable>,
    level: usize,
    parent: Option<usize>,
}

impl InnerScope {
    pub fn new(parent: Option<usize>) -> InnerScope {
        InnerScope {
            functions: HashMap::new(),
            user_defined_types: HashMap::new(),
            variables: Vec::new(),
            level: 0,
            parent,
        }
    }
}

pub struct Variable {
    pub name: Symbol,
    pub kind: ValueKind,
    pub value: Value,
    pub defined_in_scope_level: usize,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            scopes: vec![InnerScope::new(None)],
            curr_scope: 0,
        }
    }

    pub fn push_scope(&mut self) {
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

    pub fn add_variable(
        &mut self,
        name: Symbol,
        value: Value,
        kind: ValueKind,
    ) {
        let curr_scope = &mut self.scopes[self.curr_scope];
        curr_scope.variables.push(Variable {
            name,
            value,
            kind,
            defined_in_scope_level: curr_scope.level,
        })
    }

    pub fn get_variable(&mut self, name: Symbol) -> Option<&Variable> {
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

    pub fn get_variable_mut(&mut self, name: Symbol) -> Option<&mut Variable> {
        for scope in &mut self.scopes {
            for var in scope.variables.iter_mut().rev() {
                if var.name == name {
                    return Some(var);
                }
            }
        }

        None
    }

    pub fn add_function(&mut self, name: Symbol, decl: AstNodeId) {
        let scope = &mut self.scopes[self.curr_scope];
        scope.functions.insert(name, decl);
    }

    pub fn get_function(&mut self, name: Symbol) -> Option<AstNodeId> {
        let mut scope = &mut self.scopes[self.curr_scope];

        loop {
            if let Some(func) = scope.functions.get(&name) {
                return Some(*func);
            }

            if let Some(parent) = scope.parent {
                scope = &mut self.scopes[parent];
            } else {
                return None;
            }
        }
    }

    pub fn add_struct(&mut self, name: Symbol, decl: AstNodeId) {
        let scope = &mut self.scopes[self.curr_scope];
        scope.user_defined_types.insert(name, decl);
    }

    pub fn get_struct(&mut self, name: Symbol) -> Option<AstNodeId> {
        let mut scope = &mut self.scopes[self.curr_scope];

        loop {
            if let Some(structure) = scope.user_defined_types.get(&name) {
                return Some(*structure);
            }

            if let Some(parent) = scope.parent {
                scope = &mut self.scopes[parent];
            } else {
                return None;
            }
        }
    }
}
