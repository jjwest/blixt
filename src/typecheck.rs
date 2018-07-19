use ast::{
    Assignment, AssignmentKind, Ast, BinaryOp, Decl, Expr, ExprKind, For, FunctionCall,
    FunctionDecl, If, Input, Print, Return, Stmt, StmtList, UnaryOp,
};
use context::Context;
use location::Location;
use primitives::{Value, ValueKind};
use scope::Scope;
use traits::{Visitable, Visitor};

use failure;

pub fn typecheck(ast: &Ast, context: &mut Context) -> Result<(), failure::Error> {
    let mut checker = Typechecker::new(context);
    ast.accept(&mut checker);

    if checker.deferred_funcalls.len() > 0 {
        checker.check_deferred_funcalls();
    }

    if checker.check_passed {
        Ok(())
    } else {
        Err(failure::err_msg(""))
    }
}

pub struct Typechecker<'a, 'ctxt> {
    types: Vec<ValueKind>,
    check_passed: bool,
    scope: Scope<'a>,
    context: &'ctxt mut Context,
    location: Vec<Location>,
    deferred_funcalls: Vec<&'a FunctionCall>,
    current_function: Option<&'a FunctionDecl>,
}

impl<'a, 'ctxt> Typechecker<'a, 'ctxt> {
    pub fn new(context: &'ctxt mut Context) -> Self {
        Self {
            types: Vec::new(),
            check_passed: true,
            scope: Scope::new(),
            location: Vec::new(),
            context,
            deferred_funcalls: Vec::new(),
            current_function: None,
        }
    }

    fn type_of<T: Visitable<'a>>(&mut self, node: &'a T) -> ValueKind {
        node.accept(self);
        self.types
            .pop()
            .expect("Tried calling type_of, but no type found")
    }

    fn report_error(&mut self, message: &str) {
        let location = self.location[self.location.len() - 1];
        self.context.error(message, location);
        self.check_passed = false;
    }

    fn check_deferred_funcalls(&mut self) {
        let funcalls = self.deferred_funcalls.clone();

        for node in funcalls {
            self.scope.push_scope();
            match self.scope.get_function(&node.name) {
                Some(func) => {
                    for param in &func.params {
                        self.scope.add_variable(&param.name, Value::Nil, param.kind);
                    }
                    self.visit_funcall(node);
                }
                None => {
                    self.report_error(&format!("Undefined function '{}'", node.name));
                }
            }
            self.scope.pop_scope();
        }
    }
}

impl<'a, 'ctxt> Visitor<'a> for Typechecker<'a, 'ctxt> {
    fn visit_stmt_list(&mut self, node: &'a StmtList) {
        trace!("Visiting stmt_list");

        for stmt in node {
            stmt.accept(self);
        }
    }

    fn visit_stmt(&mut self, node: &'a Stmt) {
        trace!("Visiting stmt");
        node.accept(self)
    }

    fn visit_expr(&mut self, node: &'a Expr) {
        trace!("Visiting expr {:#?}", node);
        self.location.push(node.location);

        match &node.kind {
            ExprKind::Bool(_) => self.types.push(ValueKind::Bool),
            ExprKind::Float(_) => self.types.push(ValueKind::Float),
            ExprKind::Integer(_) => self.types.push(ValueKind::Integer),
            ExprKind::StringLiteral(_) => self.types.push(ValueKind::String),
            ExprKind::Ident(ident) => self.visit_ident(ident),
            ExprKind::Range(_) => unimplemented!(),
            ExprKind::Input(_) => self.types.push(ValueKind::String),
            ExprKind::UnaryOp(op) => self.visit_unary_op(op),
            ExprKind::BinaryOp(op) => self.visit_binary_op(op),
            ExprKind::FunctionCall(func) => self.visit_funcall(func),
        }

        self.location.pop();
    }

    fn visit_decl(&mut self, node: &'a Decl) {
        trace!("Visiting decl");

        match node {
            Decl::Variable(decl) => {
                if decl.kind == ValueKind::Nil {
                    let kind = self.type_of(&decl.value);
                    self.scope.add_variable(&decl.name, Value::Nil, kind);
                }
            }
            Decl::Function(decl) => {
                self.current_function = Some(decl);
                self.scope.add_function(&decl);
                self.scope.push_scope();

                for param in &decl.params {
                    self.scope.add_variable(&param.name, Value::Nil, param.kind);
                }

                self.visit_stmt_list(&decl.body);
                self.scope.pop_scope();
                self.current_function = None;
            }
        }
    }

    fn visit_binary_op(&mut self, node: &'a BinaryOp) {
        trace!("Visiting binop");

        let left = self.type_of(&*node.lhs);
        let right = self.type_of(&*node.rhs);

        match (left, right) {
            (ValueKind::Bool, ValueKind::Bool) => self.types.push(ValueKind::Bool),
            (ValueKind::Integer, ValueKind::Integer) => self.types.push(ValueKind::Integer),
            (ValueKind::String, ValueKind::String) => self.types.push(ValueKind::String),
            (ValueKind::Float, ValueKind::Float)
            | (ValueKind::Integer, ValueKind::Float)
            | (ValueKind::Float, ValueKind::Integer) => self.types.push(ValueKind::Float),
            (ValueKind::Nil, _) | (_, ValueKind::Nil) => {}
            (a, b) => {
                self.report_error(&format!(
                    "Invalid types {:?}, {:?} for operator {:?}",
                    a, b, node.op
                ));
            }
        }
    }

    fn visit_unary_op(&mut self, node: &'a UnaryOp) {
        trace!("Visiting unaryop");

        let type_ = self.type_of(&*node.value);
        self.types.push(type_);
    }

    fn visit_funcall(&mut self, node: &'a FunctionCall) {
        trace!("Visiting funcall");

        let function = match self.scope.get_function(&node.name) {
            Some(func) => func,
            None => {
                self.deferred_funcalls.push(node);
                return;
            }
        };

        if node.args.len() != function.params.len() {
            self.report_error(&format!(
                "Expected {} arguments, found {}",
                node.args.len(),
                function.params.len()
            ));
        }

        for (arg, param) in node.args.iter().zip(function.params.iter()) {
            let arg_type = self.type_of(arg);
            // If the argument type is still undecided it means
            // the variable was never declared, so no check is done.
            // An error message is printed via type_of -> node.accept -> visit_ident
            if arg_type != ValueKind::Nil && arg_type != param.kind {
                self.location.push(arg.location);
                self.report_error(&format!("Expected {:?}, found {:?}", param.kind, arg_type));
                self.location.pop();
            }
        }

        self.types
            .push(function.return_type.unwrap_or(ValueKind::Nil));
    }

    fn visit_if_stmt(&mut self, _node: &'a If) {
        trace!("Visiting if_stmt");
    }

    fn visit_ident(&mut self, node: &'a String) {
        trace!("Visiting ident");
        match self.scope.get_variable(node.as_str()) {
            Some(var) => self.types.push(var.kind),
            None => {
                self.report_error(&format!("Undeclared variable '{}'", node));
            }
        }
    }

    fn visit_return(&mut self, node: &'a Return) {
        trace!("Visiting return stmt");

        self.location.push(node.location);

        if let Some(func) = self.current_function {
            let return_type = node.value.as_ref().map(|expr| self.type_of(&*expr));
            match (func.return_type, return_type) {
                (None, Some(a)) => {
                    self.report_error(&format!("No return value expected, found {:?}", a));
                }
                (Some(a), None) => {
                    self.report_error(&format!("Expected return value of type {:?}", a));
                }
                (Some(a), Some(b)) if a != b => {
                    self.report_error(&format!("Expected return value {:?}, found {:?}", a, b));
                }
                _ => {}
            }
        } else {
            self.report_error("Cannot use keyword return outside of a function");
        }

        self.location.pop();
    }

    fn visit_block(&mut self, _node: &'a StmtList) {
        trace!("Visiting block");
    }

    fn visit_assignment(&mut self, node: &'a Assignment) {
        trace!("Visiting assignment {} = {:#?}", node.ident, node.value);

        self.location.push(node.location);

        let lhs = self
            .scope
            .get_variable(node.ident.as_str())
            .map(|var| var.kind)
            .unwrap_or_else(|| panic!(format!("{} has not been typechecked", node.ident)));

        let rhs = self.type_of(&node.value);

        match (lhs, rhs) {
            (ValueKind::Bool, ValueKind::Bool) => self.types.push(ValueKind::Bool),
            (ValueKind::Integer, ValueKind::Integer) => self.types.push(ValueKind::Integer),
            (ValueKind::String, ValueKind::String) => self.types.push(ValueKind::String),
            (ValueKind::Float, ValueKind::Float) => self.types.push(ValueKind::Float),
            (ValueKind::Integer, ValueKind::Float) | (ValueKind::Float, ValueKind::Integer) => {
                if node.op != AssignmentKind::Assign {
                    self.report_error(&format!(
                        "Invalid types {:?}, {:?} for operator {:?}",
                        lhs, rhs, node.op
                    ));
                }
            }
            (a, b) => {
                self.report_error(&format!(
                    "Invalid types {:?}, {:?} for operator {:?}",
                    a, b, node.op
                ));
            }
        }

        self.location.pop();
    }

    fn visit_print(&mut self, _node: &'a Print) {
        trace!("Visiting print");
    }

    fn visit_input(&mut self, _node: &'a Input) {
        trace!("Visiting input");
    }

    fn visit_for(&mut self, _node: &'a For) {
        trace!("Visiting for");
    }
}
