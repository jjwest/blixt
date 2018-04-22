use ast::{Assignment, AssignmentKind, BinaryOp, BinaryOpKind, Decl, Expr, For, FunctionCall,
          FunctionDecl, If, Input, Print, Stmt, StmtList, UnaryOp, UnaryOpKind};
use builtins::Value;
use traits::{AstVisitor, Visitable};

use std::collections::HashMap;
use std::io::{self, Write};
use std::rc::Rc;

type FuncName = String;

pub struct Interpreter {
    global_scope: Scope,
    scopes: Vec<Scope>,
    curr_scope: usize,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            global_scope: Scope::new(),
            scopes: vec![Scope::new()],
            curr_scope: 0,
        }
    }

    fn new_scope(&mut self) -> &mut Scope {
        self.scopes.push(Scope::new());
        self.curr_scope += 1;
        &mut self.scopes[self.curr_scope]
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.curr_scope -= 1;
    }

    fn curr_scope(&mut self) -> &mut Scope {
        if self.curr_scope == 0 {
            &mut self.global_scope
        } else {
            &mut self.scopes[self.curr_scope - 1]
        }
    }

    fn add_variable(&mut self, name: String, value: Value) {
        let scope = self.curr_scope();
        scope.variables.push(Variable {
            name,
            value,
            defined_in_scope_level: scope.scope_level,
        })
    }

    fn get_variable(&mut self, name: &str) -> Option<Value> {
        for var in self.curr_scope().variables.iter_mut().rev() {
            if var.name == name {
                return Some(var.value.clone());
            }
        }

        for var in self.global_scope.variables.iter_mut().rev() {
            if var.name == name {
                return Some(var.value.clone());
            }
        }

        None
    }

    fn get_variable_mut(&mut self, name: &str) -> Option<&mut Value> {
        for var in self.curr_scope().variables.iter_mut().rev() {
            if var.name == name {
                return Some(&mut var.value);
            }
        }

        for var in self.global_scope.variables.iter_mut().rev() {
            if var.name == name {
                return Some(&mut var.value);
            }
        }

        None
    }

    fn add_function(&mut self, func: FunctionDecl) {
        let scope = self.curr_scope();
        scope.functions.insert(func.name.clone(), func);
    }

    fn get_function(&mut self, name: &str) -> Option<FunctionDecl> {
        let scope = self.curr_scope();
        match scope.functions.get(name) {
            Some(func) => Some(func.clone()),
            None => self.global_scope.functions.get(name).map(|f| f.clone()),
        }
    }
}

struct Scope {
    functions: HashMap<FuncName, FunctionDecl>,
    variables: Vec<Variable>,
    scope_level: usize,
}

struct Variable {
    name: String,
    value: Value,
    defined_in_scope_level: usize,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            functions: HashMap::new(),
            variables: Vec::new(),
            scope_level: 0,
        }
    }

    fn new_scope_level(&mut self) {
        self.scope_level += 1;
    }

    fn pop_scope_level(&mut self) {
        let curr_level = self.scope_level;
        self.variables
            .retain(|var| var.defined_in_scope_level != curr_level);
        self.scope_level -= 1;
    }
}

impl AstVisitor for Interpreter {
    fn visit_stmt_list(&mut self, node: &mut StmtList) -> Value {
        trace!("Visit stmt_list");

        for stmt in node {
            if let Value::Return(val) = self.visit_stmt(stmt) {
                return *val;
            }
        }

        Value::Nil
    }

    fn visit_stmt(&mut self, node: &mut Stmt) -> Value {
        trace!("Visit stmt");
        node.accept(self)
    }

    fn visit_expr(&mut self, node: &mut Expr) -> Value {
        trace!("Visit expr");

        match node {
            Expr::Bool(v) => Value::Bool(*v),
            Expr::Float(v) => Value::Float(*v),
            Expr::Integer(v) => Value::Int(*v),
            Expr::StringLiteral(v) => Value::String(v.clone()),
            Expr::Ident(v) => self.visit_ident(v),
            Expr::UnaryOp(v) => self.visit_unary_op(v),
            Expr::BinaryOp(v) => self.visit_binary_op(v),
            Expr::FunctionCall(v) => self.visit_funcall(v),
            Expr::Input(v) => self.visit_input(v),
        }
    }

    fn visit_decl(&mut self, node: &mut Decl) -> Value {
        trace!("Visit decl");
        let node = node.clone();

        match node {
            Decl::Function(func) => {
                self.add_function(func);
                Value::Nil
            }
            Decl::Variable(mut var) => {
                let value = var.value.accept(self);
                self.add_variable(var.name.clone(), value);
                Value::Nil
            }
        }
    }

    fn visit_binary_op(&mut self, node: &mut BinaryOp) -> Value {
        trace!("Visit binop");
        match node.op {
            BinaryOpKind::Add => node.lhs.accept(self) + node.rhs.accept(self),
            BinaryOpKind::Sub => node.lhs.accept(self) - node.rhs.accept(self),
            BinaryOpKind::Mul => node.lhs.accept(self) * node.rhs.accept(self),
            BinaryOpKind::Div => node.lhs.accept(self) / node.rhs.accept(self),
            BinaryOpKind::Mod => node.lhs.accept(self) % node.rhs.accept(self),
            BinaryOpKind::Equal => Value::Bool(node.lhs.accept(self) == node.rhs.accept(self)),
            BinaryOpKind::NotEqual => Value::Bool(node.lhs.accept(self) != node.rhs.accept(self)),
            BinaryOpKind::Lesser => Value::Bool(node.lhs.accept(self) < node.rhs.accept(self)),
            BinaryOpKind::LesserEqual => {
                Value::Bool(node.lhs.accept(self) <= node.rhs.accept(self))
            }
            BinaryOpKind::Greater => Value::Bool(node.lhs.accept(self) > node.rhs.accept(self)),
            BinaryOpKind::GreaterEqual => {
                Value::Bool(node.lhs.accept(self) >= node.rhs.accept(self))
            }
            BinaryOpKind::And => match (node.lhs.accept(self), node.rhs.accept(self)) {
                (Value::Bool(a), Value::Bool(b)) => Value::Bool(a && b),
                (a, b) => panic!(
                    "Can only use logical operators with bools AND, found {} {}",
                    a, b
                ),
            },
            BinaryOpKind::Or => match (node.lhs.accept(self), node.rhs.accept(self)) {
                (Value::Bool(a), Value::Bool(b)) => Value::Bool(a || b),
                (a, b) => panic!(
                    "Can only use logical operators with bools OR, found {} {}",
                    a, b
                ),
            },
        }
    }

    fn visit_unary_op(&mut self, node: &mut UnaryOp) -> Value {
        match node.op {
            UnaryOpKind::Not => match node.expr.accept(self) {
                Value::Bool(n) => Value::Bool(!n),
                _ => panic!("Cannot negate non boolean expression"),
            },
            UnaryOpKind::Neg => match node.expr.accept(self) {
                Value::Int(n) => Value::Int(-n),
                Value::Float(n) => Value::Float(-n),
                Value::Bool(_) => panic!("Cannot have negative booleans"),
                Value::String(_) => panic!("Cannot have negative strings"),
                Value::Nil => panic!("Cannot have negative nil"),
                _ => {
                    println!("Tried to negate return value");
                    Value::Nil
                }
            },
        }
    }

    fn visit_funcall(&mut self, node: &mut FunctionCall) -> Value {
        trace!("Visit funcall");

        let mut func = self.get_function(&node.name)
            .expect(&format!("Unknown function {}", node.name));

        assert!(
            func.params.len() == node.args.len(),
            format!(
                "Arg/Param count mismatch. Expected {} args but got {}",
                func.params.len(),
                node.args.len()
            )
        );

        let arg_values: Vec<Value> = node.args.iter_mut().map(|arg| arg.accept(self)).collect();

        self.new_scope();
        for (param, value) in func.params.iter().zip(arg_values) {
            self.add_variable(param.name.clone(), value);
        }

        let result = func.body.accept(self);
        self.pop_scope();
        result
    }

    fn visit_if_stmt(&mut self, node: &mut If) -> Value {
        trace!("Visit if stmt: {:?}", node);

        let cond = node.cond.accept(self);
        if cond == Value::Bool(true) {
            node.body.accept(self)
        } else if let Some(ref mut else_body) = node.else_body {
            else_body.accept(self)
        } else {
            Value::Nil
        }
    }

    fn visit_ident(&mut self, node: &mut String) -> Value {
        trace!("Visit ident");
        self.get_variable(&node)
            .expect(&format!("Unknown ident {}", node))
    }

    fn visit_return_stmt(&mut self, node: Option<&mut Expr>) -> Value {
        trace!("Visit return: {:?}", node);
        match node {
            Some(expr) => Value::Return(Box::new(expr.accept(self))),
            None => Value::Nil,
        }
    }

    fn visit_block(&mut self, node: &mut StmtList) -> Value {
        trace!("Visit block");
        self.curr_scope().new_scope_level();
        let result = self.visit_stmt_list(node);
        self.curr_scope().pop_scope_level();
        result
    }

    fn visit_assignment(&mut self, node: &mut Assignment) -> Value {
        trace!("Visit assignment");

        let value = node.value.accept(self);
        let var = self.get_variable_mut(&node.ident)
            .expect(&format!("Unknown ident {}", node.ident));

        match node.op {
            AssignmentKind::Regular => *var = value,
            AssignmentKind::Add => *var += value,
            AssignmentKind::Sub => *var -= value,
            AssignmentKind::Mul => *var *= value,
            AssignmentKind::Div => *var /= value,
            AssignmentKind::Mod => *var %= value,
        }

        Value::Nil
    }

    fn visit_print(&mut self, node: &mut Print) -> Value {
        trace!("Visit print");

        if let Some(fmt_string) = node.args.get_mut(0) {
            let fmt_string = match fmt_string.accept(self) {
                Value::String(v) => v,
                _ => panic!("Cannot print without format string"),
            };

            let mut num_fmt_args = 0;
            let mut output = String::with_capacity(fmt_string.len());
            let mut escaped = false;

            for ch in fmt_string.chars() {
                if escaped {
                    match ch {
                        '%' => output.push('%'),
                        'n' => output.push('\n'),
                        'r' => output.push('\r'),
                        't' => output.push('\t'),
                        c => output.push(c),
                    }
                    escaped = false;
                } else if ch == '\\' {
                    escaped = true;
                } else if ch == '%' {
                    match node.args
                        .get_mut(num_fmt_args + 1)
                        .map(|arg| arg.accept(self))
                    {
                        Some(Value::String(s)) => {
                            for ch in s.chars() {
                                output.push(ch);
                            }
                        }
                        Some(Value::Int(n)) => output.push_str(&format!("{}", n)),
                        Some(Value::Float(n)) => output.push_str(&format!("{}", n)),
                        Some(Value::Bool(n)) => output.push_str(&format!("{}", n)),
                        Some(Value::Nil) => output.push_str("nil"),
                        Some(Value::Return(_)) => unreachable!(),
                        None => panic!("Expected format arg {}, but none found", num_fmt_args),
                    }
                    num_fmt_args += 1;
                } else {
                    output.push(ch);
                }
            }

            assert!(
                num_fmt_args == node.args.len() - 1,
                format!(
                    "Format string expected {} arguments, found {}",
                    num_fmt_args,
                    node.args.len() - 1
                )
            );
            print!("{}", output);
            io::stdout().flush().expect("Failed to flush stdout");
        }

        Value::Nil
    }

    fn visit_input(&mut self, node: &mut Input) -> Value {
        match node.message.as_mut().map(|m| m.accept(self)) {
            Some(Value::String(v)) => {
                print!("{}", v);
                io::stdout().flush().expect("Failed to flush stdout");
            }
            Some(_) => panic!("Can only print strings"),
            None => {}
        }

        let mut buf = String::new();
        io::stdin()
            .read_line(&mut buf)
            .expect("Failed to read from stdin");

        buf.pop(); // Remove newline

        Value::String(Rc::new(buf))
    }

    fn visit_for(&mut self, node: &mut For) -> Value {
        Value::Nil
    }
}
