use ast::{
    Assignment, AssignmentKind, Ast, BinaryOp, Decl, Expr, ExprKind, For, FunctionCall, If, Input,
    Print, Stmt, StmtList, UnaryOp, VarDecl,
};
use common::{Context, Location};
use primitives::{Value, ValueKind};
use scope::Scope;
use traits::{Visitable, Visitor};

pub fn typecheck(ast: &Ast, context: &mut Context) -> Result<(), ()> {
    let mut checker = Typechecker::new(context);
    ast.accept(&mut checker);

    if checker.check_passed {
        Ok(())
    } else {
        Err(())
    }
}

pub struct Typechecker<'a, 'ctxt> {
    types: Vec<ValueKind>,
    check_passed: bool,
    scope: Scope<'a>,
    context: &'ctxt mut Context,
    location: Location,
}

impl<'a, 'ctxt> Typechecker<'a, 'ctxt> {
    pub fn new(context: &'ctxt mut Context) -> Self {
        Self {
            types: Vec::new(),
            check_passed: true,
            scope: Scope::new(),
            context,
            location: Location::default(),
        }
    }

    fn type_of<T: Visitable<'a>>(&mut self, node: &'a T) -> ValueKind {
        node.accept(self);
        self.types
            .pop()
            .expect("Tried calling type_of, but no types ready")
    }

    fn report_error(&mut self, message: &str) {
        self.context.error(message, self.location);
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
        trace!("Visiting expr");
        self.location = node.location;

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
    }

    fn visit_decl(&mut self, node: &'a Decl) {
        trace!("Visiting decl");

        match node {
            Decl::Variable(VarDecl { name, value, kind }) => {
                if *kind == ValueKind::Undecided {
                    let kind = self.type_of(value);
                    self.scope.add_variable(name, Value::Nil, kind);
                }
            }
            Decl::Function(decl) => {
                self.scope.add_function(&decl);
                self.scope.new_scope();

                for param in &decl.params {
                    self.scope.add_variable(&param.name, Value::Nil, param.kind);
                }

                self.visit_stmt_list(&decl.body);
                self.scope.pop_scope();
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
            (a, b) => {
                self.report_error(&format!(
                    "Invalid types {:?}, {:?} for operator {:?}",
                    a, b, node.op
                ));
                self.check_passed = false;
            }
        }
    }

    fn visit_unary_op(&mut self, node: &'a UnaryOp) {
        trace!("Visiting unaryop");

        let type_ = self.type_of(&*node.value);
        self.types.push(type_);
    }

    fn visit_funcall(&mut self, _node: &'a FunctionCall) {
        trace!("Visiting funcall");
    }

    fn visit_if_stmt(&mut self, _node: &'a If) {
        trace!("Visiting if_stmt");
    }

    fn visit_ident(&mut self, node: &'a String) {
        trace!("Visiting ident");
        match self.scope.get_variable(node.as_str()) {
            Some(var) => self.types.push(var.kind),
            None => {
                self.report_error(&format!("Ident {} has not been typechecked", node));
                self.check_passed = false;
            }
        }
    }

    fn visit_return_stmt(&mut self, _node: Option<&'a Expr>) {
        trace!("Visiting return stmt");
    }

    fn visit_block(&mut self, _node: &'a StmtList) {
        trace!("Visiting block");
    }

    fn visit_assignment(&mut self, node: &'a Assignment) {
        trace!("Visiting assignment {} = {:#?}", node.ident, node.value);

        let lhs = self.scope
            .get_variable(node.ident.as_str())
            .map(|var| var.kind)
            .expect(&format!("{} has not been typechecked", node.ident));

        let rhs = self.type_of(&node.value);

        match (lhs, rhs) {
            (ValueKind::Bool, ValueKind::Bool) => self.types.push(ValueKind::Bool),
            (ValueKind::Integer, ValueKind::Integer) => self.types.push(ValueKind::Integer),
            (ValueKind::String, ValueKind::String) => self.types.push(ValueKind::String),
            (ValueKind::Float, ValueKind::Float) => self.types.push(ValueKind::Float),
            (ValueKind::Integer, ValueKind::Float) | (ValueKind::Float, ValueKind::Integer) => {
                if node.op != AssignmentKind::Regular {
                    self.report_error(&format!(
                        "Invalid types {:?}, {:?} for operator {:?}",
                        lhs, rhs, node.op
                    ));
                    self.types.push(ValueKind::Float);
                    self.check_passed = false;
                }
            }
            (a, b) => {
                self.report_error(&format!(
                    "Invalid types {:?}, {:?} for operator {:?}",
                    a, b, node.op
                ));
                self.types.push(ValueKind::Float);
                self.check_passed = false;
            }
        }
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
