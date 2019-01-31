use crate::ast::{
    Assignment, AssignmentKind, Ast, AstNode, AstNodeList, BinaryOp, BinaryOpKind, Decl, Expr,
    ExprKind, For, FunctionCall, If, Input, Print, Return, UnaryOp, UnaryOpKind,
};
use crate::common::Context;
use crate::location::Location;
use crate::primitives::{Value, ValueKind};
use crate::scope::Scope;

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
    location: Vec<Location>,
}

impl<'a, 'ctxt> Interpreter<'a, 'ctxt> {
    pub fn new(context: &'ctxt mut Context) -> Self {
        Self {
            scope: Scope::new(),
            values: Vec::new(),
            location: Vec::new(),
            context,
        }
    }

    fn value_of<T: Visitable<'a>>(&mut self, node: &'a T) -> Value {
        node.accept(self);
        self.values.pop().unwrap_or(Value::Nil)
    }

    fn report_error(&mut self, message: &str) {
        self.context
            .error(message, self.location[self.location.len() - 1]);
    }
}

impl<'a, 'ctxt> Visitor<'a> for Interpreter<'a, 'ctxt> {
    fn visit_stmt_list(&mut self, node: &'a AstNodeList) {
        trace!("Visit stmt_list");

        for stmt in node {
            stmt.accept(self);

            if let AstNode::Return(_) = stmt {
                break;
            }
        }
    }

    fn visit_stmt(&mut self, node: &'a AstNode) {
        trace!("Visit stmt");
        node.accept(self)
    }

    fn visit_expr(&mut self, node: &'a Expr) {
        trace!("Visit expr");
        self.location.push(node.location);

        match &node.kind {
            ExprKind::Bool(v) => self.values.push(Value::Bool(*v)),
            ExprKind::Float(v) => self.values.push(Value::Float(*v)),
            ExprKind::Integer(v) => self.values.push(Value::Int(*v)),
            ExprKind::StringLiteral(v) => self.values.push(Value::String(v.clone())),
            ExprKind::Ident(v) => self.visit_ident(v),
            ExprKind::UnaryOp(v) => self.visit_unary_op(v),
            ExprKind::BinaryOp(v) => self.visit_binary_op(v),
            ExprKind::FunctionCall(v) => self.visit_function_call(v),
            ExprKind::Input(v) => self.visit_input(v),
            ExprKind::Range(_v) => unimplemented!(),
        }

        self.location.pop();
    }

    fn visit_decl(&mut self, node: &'a Decl) {
        trace!("Visit decl");

        match node {
            Decl::Function(func) => self.scope.add_function(func),
            Decl::Variable(var) => {
                let value = self.value_of(&var.value);
                self.scope.add_variable(&var.name, value, var.kind.clone());
            }
            Decl::Struct(decl) => self.scope.add_struct(decl),
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
            BinaryOpKind::Field => unimplemented!(),
            BinaryOpKind::And => match (self.value_of(&*node.lhs), self.value_of(&*node.rhs)) {
                (Value::Bool(a), Value::Bool(b)) => self.values.push(Value::Bool(a && b)),
                (a, b) => self.report_error(&format!(
                    "Can only use logical operator AND with bools, found {} {}",
                    a, b
                )),
            },
            BinaryOpKind::Or => match (self.value_of(&*node.lhs), self.value_of(&*node.rhs)) {
                (Value::Bool(a), Value::Bool(b)) => self.values.push(Value::Bool(a || b)),
                (a, b) => self.report_error(&format!(
                    "Can only use logical operators with bools OR, found {} {}",
                    a, b
                )),
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
                Value::Bool(_) => self.report_error("Cannot have negative booleans"),
                Value::String(_) => self.report_error("Cannot have negative strings"),
                Value::Nil => self.report_error("Cannot have negative nil"),
                Value::Struct(_) => self.report_error("Cannot have negative struct"),
            },
        }
    }

    fn visit_function_call(&mut self, node: &'a FunctionCall) {
        trace!("Visit funcall");

        let func = self
            .scope
            .get_function(&node.name)
            .unwrap_or_else(|| panic!("Unknown function {}", node.name));

        assert!(
            func.params.len() == node.args.len(),
            format!(
                "Arg/Param count mismatch. Expected {} args but got {}",
                func.params.len(),
                node.args.len()
            )
        );

        let arg_values: Vec<Value> = node.args.iter().map(|arg| self.value_of(arg)).collect();

        self.scope.push_scope();
        for (param, value) in func.params.iter().zip(arg_values) {
            let kind = match value {
                Value::Bool(_) => ValueKind::Bool,
                Value::Int(_) => ValueKind::Integer,
                Value::Float(_) => ValueKind::Float,
                Value::String(_) => ValueKind::String,
                _ => ValueKind::Nil,
            };
            self.scope.add_variable(&param.name, value, kind);
        }

        func.body.accept(self);
        self.scope.pop_scope();
    }

    fn visit_if_stmt(&mut self, node: &'a If) {
        trace!("Visit if stmt");

        let condition = self.value_of(&node.cond);
        if condition == Value::Bool(true) {
            node.body.accept(self);
        } else if let Some(ref else_body) = node.else_body {
            else_body.accept(self);
        }
    }

    fn visit_ident(&mut self, node: &'a str) {
        trace!("Visit ident");

        let var = self
            .scope
            .get_variable(&node)
            .unwrap_or_else(|| panic!("Unknown ident {}", node));
        self.values.push(var.value.clone());
    }

    fn visit_return(&mut self, node: &'a Return) {
        trace!("Visit return");

        if let Some(ref expr) = node.value {
            let value = self.value_of(expr);
            self.values.push(value);
        }
    }

    fn visit_block(&mut self, node: &'a AstNodeList) {
        trace!("Visit block");
        self.scope.new_scope_level();
        self.visit_stmt_list(node);
        self.scope.pop_scope_level();
    }

    fn visit_assignment(&mut self, node: &'a Assignment) {
        trace!("Visit assignment");

        let value = self.value_of(&node.value);
        let var = self
            .scope
            .get_variable_mut(&node.ident)
            .unwrap_or_else(|| panic!("Unknown ident {}", node.ident));

        match node.op {
            AssignmentKind::Assign => var.value = value,
            AssignmentKind::Add => var.value += value,
            AssignmentKind::Sub => var.value -= value,
            AssignmentKind::Mul => var.value *= value,
            AssignmentKind::Div => var.value /= value,
            AssignmentKind::Mod => var.value %= value,
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
                    match node
                        .args
                        .get(num_fmt_args + 1)
                        .map(|arg| self.value_of(arg))
                    {
                        Some(Value::String(n)) => output.extend(n.chars()),
                        Some(Value::Int(n)) => output.push_str(&format!("{}", n)),
                        Some(Value::Float(n)) => output.push_str(&format!("{}", n)),
                        Some(Value::Bool(n)) => output.push_str(&format!("{}", n)),
                        Some(Value::Nil) => output.push_str("nil"),
                        Some(Value::Struct(_)) => unimplemented!(),
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
            var.value = Value::Int(i);
            node.block.accept(self);
        }

        self.scope.pop_scope_level();
    }
}
