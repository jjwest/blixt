use ast::{
    Assignment, AssignmentKind, Ast, BinaryOp, BinaryOpKind, Decl, Expr, ExprKind, For,
    FunctionCall, If, Input, Print, Stmt, StmtList, UnaryOp, UnaryOpKind,
};
use builtins::{Value, ValueKind};
use common::Context;
use scope::Scope;
use traits::{Visitable, Visitor};

use std::fmt::Debug;
use std::io::{self, Write};
use std::rc::Rc;

pub fn interpret(ast: &Ast, context: &mut Context) {
    let mut interp = Interpreter::new(context);
    ast.accept(&mut interp);
}

struct Interpreter<'a, 'ctxt> {
    scope: Scope<'a>,
    values: Vec<Value>,
    context: &'ctxt mut Context,
}

impl<'a, 'ctxt> Interpreter<'a, 'ctxt> {
    pub fn new(context: &'ctxt mut Context) -> Self {
        Self {
            scope: Scope::new(),
            values: Vec::new(),
            context,
        }
    }

    fn value_of<T: Debug + Visitable<'a>>(&mut self, node: &'a T) -> Value {
        node.accept(self);
        self.values
            .pop()
            .expect("Tried to get eval value that does not exist")
    }
}

impl<'a, 'ctxt> Visitor<'a> for Interpreter<'a, 'ctxt> {
    fn visit_stmt_list(&mut self, node: &'a StmtList) {
        trace!("Visit stmt_list");

        for stmt in node {
            stmt.accept(self);
            if let Some(Value::Return(_)) = self.values.get(0) {
                break;
            }
        }
    }

    fn visit_stmt(&mut self, node: &'a Stmt) {
        trace!("Visit stmt");
        node.accept(self)
    }

    fn visit_expr(&mut self, node: &'a Expr) {
        trace!("Visit expr");

        match &node.kind {
            ExprKind::Bool(v) => self.values.push(Value::Bool(*v)),
            ExprKind::Float(v) => self.values.push(Value::Float(*v)),
            ExprKind::Integer(v) => self.values.push(Value::Int(*v)),
            ExprKind::StringLiteral(v) => self.values.push(Value::String(v.clone())),
            ExprKind::Ident(v) => self.visit_ident(v),
            ExprKind::UnaryOp(v) => self.visit_unary_op(v),
            ExprKind::BinaryOp(v) => self.visit_binary_op(v),
            ExprKind::FunctionCall(v) => self.visit_funcall(v),
            ExprKind::Input(v) => self.visit_input(v),
            ExprKind::Range(_v) => unimplemented!(),
        }
    }

    fn visit_decl(&mut self, node: &'a Decl) {
        trace!("Visit decl");

        match node {
            Decl::Function(func) => self.scope.add_function(func),
            Decl::Variable(ref var) => {
                let value = self.value_of(&var.value);
                self.scope.add_variable(&var.name, value, var.kind);
            }
        }
    }

    fn visit_binary_op(&mut self, node: &'a BinaryOp) {
        trace!("Visit binop");

        match node.op {
            BinaryOpKind::Add => {
                let value = self.value_of(&*node.lhs) + self.value_of(&*node.rhs);
                self.values.push(value);
            }
            BinaryOpKind::Sub => {
                let value = self.value_of(&*node.lhs) - self.value_of(&*node.rhs);
                self.values.push(value);
            }
            BinaryOpKind::Mul => {
                let value = self.value_of(&*node.lhs) * self.value_of(&*node.rhs);
                self.values.push(value);
            }
            BinaryOpKind::Div => {
                let value = self.value_of(&*node.lhs) / self.value_of(&*node.rhs);
                self.values.push(value);
            }
            BinaryOpKind::Mod => {
                let value = self.value_of(&*node.lhs) % self.value_of(&*node.rhs);
                self.values.push(value);
            }
            BinaryOpKind::Equal => {
                let value = Value::Bool(self.value_of(&*node.lhs) == self.value_of(&*node.rhs));
                self.values.push(value);
            }
            BinaryOpKind::NotEqual => {
                let value = Value::Bool(self.value_of(&*node.lhs) != self.value_of(&*node.rhs));
                self.values.push(value);
            }
            BinaryOpKind::Lesser => {
                let value = Value::Bool(self.value_of(&*node.lhs) < self.value_of(&*node.rhs));
                self.values.push(value);
            }
            BinaryOpKind::LesserEqual => {
                let value = Value::Bool(self.value_of(&*node.lhs) <= self.value_of(&*node.rhs));
                self.values.push(value);
            }
            BinaryOpKind::Greater => {
                let value = Value::Bool(self.value_of(&*node.lhs) > self.value_of(&*node.rhs));
                self.values.push(value);
            }
            BinaryOpKind::GreaterEqual => {
                let value = Value::Bool(self.value_of(&*node.lhs) >= self.value_of(&*node.rhs));
                self.values.push(value);
            }
            BinaryOpKind::And => match (self.value_of(&*node.lhs), self.value_of(&*node.rhs)) {
                (Value::Bool(a), Value::Bool(b)) => self.values.push(Value::Bool(a && b)),
                (a, b) => panic!(
                    "Can only use logical operators with bools AND, found {} {}",
                    a, b
                ),
            },
            BinaryOpKind::Or => match (self.value_of(&*node.lhs), self.value_of(&*node.rhs)) {
                (Value::Bool(a), Value::Bool(b)) => self.values.push(Value::Bool(a || b)),
                (a, b) => panic!(
                    "Can only use logical operators with bools OR, found {} {}",
                    a, b
                ),
            },
        }
    }

    fn visit_unary_op(&mut self, node: &'a UnaryOp) {
        trace!("Visit unary_op");

        match node.op {
            UnaryOpKind::Not => match self.value_of(&*node.value) {
                Value::Bool(n) => self.values.push(Value::Bool(!n)),
                _ => panic!("Cannot negate non boolean expression"),
            },
            UnaryOpKind::Neg => match self.value_of(&*node.value) {
                Value::Int(n) => self.values.push(Value::Int(-n)),
                Value::Float(n) => self.values.push(Value::Float(-n)),
                Value::Bool(_) => panic!("Cannot have negative booleans"),
                Value::String(_) => panic!("Cannot have negative strings"),
                Value::Nil => panic!("Cannot have negative nil"),
                _ => println!("Tried to negate return value"),
            },
        }
    }

    fn visit_funcall(&mut self, node: &'a FunctionCall) {
        trace!("Visit funcall");

        let func = self.scope
            .get_function(&node.name)
            .expect(&format!("Unknown function {}", node.name));

        assert!(
            func.params.len() == node.args.len(),
            format!(
                "Arg/Param count mismatch. Expected {} args but got {}",
                func.params.len(),
                node.args.len()
            )
        );

        let arg_values: Vec<Value> = node.args.iter().map(|arg| self.value_of(arg)).collect();

        self.scope.new_scope();
        for (param, value) in func.params.iter().zip(arg_values) {
            let kind = match value {
                Value::Bool(_) => ValueKind::Bool,
                Value::Int(_) => ValueKind::Integer,
                Value::Float(_) => ValueKind::Float,
                Value::String(_) => ValueKind::String,
                _ => ValueKind::Undecided,
            };
            self.scope.add_variable(&param.name, value, kind);
        }

        let result = func.body.accept(self);
        self.scope.pop_scope();
        result
    }

    fn visit_if_stmt(&mut self, node: &'a If) {
        trace!("Visit if stmt");

        let cond = self.value_of(&node.cond);
        if cond == Value::Bool(true) {
            node.body.accept(self);
        } else if let Some(ref else_body) = node.else_body {
            else_body.accept(self);
        }
    }

    fn visit_ident(&mut self, node: &'a String) {
        trace!("Visit ident");

        let var = self.scope
            .get_variable(&node)
            .expect(&format!("Unknown ident {}", node));
        self.values.push(var.value.clone());
    }

    fn visit_return_stmt(&mut self, node: Option<&'a Expr>) {
        trace!("Visit return");
        match node {
            Some(expr) => {
                let value = Value::Return(Box::new(self.value_of(expr)));
                self.values.push(value);
            }
            None => self.values.push(Value::Nil),
        }
    }

    fn visit_block(&mut self, node: &'a StmtList) {
        trace!("Visit block");
        self.scope.new_scope_level();
        let result = self.visit_stmt_list(node);
        self.scope.pop_scope_level();
        result
    }

    fn visit_assignment(&mut self, node: &'a Assignment) {
        trace!("Visit assignment");

        let value = self.value_of(&node.value);
        let var = self.scope
            .get_variable_mut(&node.ident)
            .expect(&format!("Unknown ident {}", node.ident));

        match node.op {
            AssignmentKind::Regular => *var = value,
            AssignmentKind::Add => *var += value,
            AssignmentKind::Sub => *var -= value,
            AssignmentKind::Mul => *var *= value,
            AssignmentKind::Div => *var /= value,
            AssignmentKind::Mod => *var %= value,
        }
    }

    fn visit_print(&mut self, node: &'a Print) {
        trace!("Visit print");

        if let Some(fmt_string) = node.args.get(0) {
            let fmt_string = match self.value_of(fmt_string) {
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
                        .get(num_fmt_args + 1)
                        .map(|arg| self.value_of(arg))
                    {
                        Some(Value::String(n)) => output.extend(n.chars()),
                        Some(Value::Int(n)) => output.push_str(&format!("{}", n)),
                        Some(Value::Float(n)) => output.push_str(&format!("{}", n)),
                        Some(Value::Bool(n)) => output.push_str(&format!("{}", n)),
                        Some(Value::Nil) => output.push_str("nil"),
                        Some(Value::Return(val)) => match *val {
                            Value::Int(n) => output.push_str(&format!("{}", n)),
                            Value::Float(n) => output.push_str(&format!("{}", n)),
                            Value::Bool(n) => output.push_str(&format!("{}", n)),
                            Value::Nil => output.push_str("nil"),
                            Value::String(n) => output.extend(n.chars()),
                            _ => unreachable!(),
                        },
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
    }

    fn visit_input(&mut self, node: &'a Input) {
        trace!("Visit input");

        match node.message.as_ref().map(|m| self.value_of(&**m)) {
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

        self.values.push(Value::String(Rc::new(buf)));
    }

    fn visit_for(&mut self, node: &'a For) {
        trace!("Visit for");

        self.scope.new_scope_level();
        self.scope
            .add_variable(&node.ident, Value::Int(0), ValueKind::Integer);

        for i in node.range.start..node.range.end {
            let var = self.scope.get_variable_mut(&node.ident).unwrap();
            *var = Value::Int(i);
            node.block.accept(self);
        }

        self.scope.pop_scope_level();
    }
}
