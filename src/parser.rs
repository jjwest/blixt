use ast::{Assignment, ArithmeticOp, Expr, LogicOp, ParameterList, Stmt, StmtList, ValueType};
use errors::*;
use lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    ast: StmtList,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            ast: StmtList::new(),
        }
    }

    pub fn parse(&mut self) -> Result<StmtList> {
        let mut syntax_tree = StmtList::new();

        syntax_tree.add_stmt(match self.statement() {
            Some(stmt) => stmt,
            None => return Err("Error while parsing".into()),
        });

        Ok(syntax_tree)
    }

    fn statement(&mut self) -> Option<Stmt> {
        trace!("Entered statement");

        if let Some(assignment) = self.assignment() {
            return Some(Stmt::Assignment(assignment));
        }
        if let Some(expr) = self.expression() {
            return Some(Stmt::Expr(*expr));
        }

        None
    }

    fn assignment(&mut self) -> Option<Assignment> {
        trace!("Entered assignment");

        let assigment = match (self.tokens.get(0), self.tokens.get(1)) {
            (Some(&Token::Ident(_)), Some(&Token::Assign)) => true,
            _ => false,
        };

        if assigment {
            let ident = match self.tokens.remove(0) {
                Token::Ident(ident) => ident,
                _ => return None,
            };

            self.tokens.remove(0);
            let expr = self.expression().expect("Expression");
            let assigment = Assignment::new(ident, *expr);
            Some(assigment)
        } else {
            None
        }
    }

    fn expression(&mut self) -> Option<Box<Expr>> {
        trace!("Entered expression");

        if let Some(logical_expr) = self.logical_expression() {
            let operator = match self.tokens.get(0) {
                Some(&Token::Lesser) => Some(LogicOp::Lesser),
                Some(&Token::Greater) => Some(LogicOp::Greater),
                Some(&Token::GreaterEqual) => Some(LogicOp::GreaterEqual),
                Some(&Token::LesserEqual) => Some(LogicOp::LesserEqual),
                Some(&Token::Equal) => Some(LogicOp::Equal),
                _ => None,
            };

            if let Some(operator) = operator {
                // We peeked a valid operator earlier, so we remove it
                self.tokens.remove(0);
                Some(Box::new(Expr::LogicalExpr {
                    lhs: logical_expr,
                    rhs: match self.expression() {
                        Some(expr) => expr,
                        None => return None,
                    },
                    operator,
                }))
            } else {
                Some(logical_expr)
            }
        } else {
            None
        }
    }

    fn logical_expression(&mut self) -> Option<Box<Expr>> {
        trace!("Entered logical_expression");

        if let Some(term) = self.term() {
            let operator = match self.tokens.get(0) {
                Some(&Token::Lesser) => Some(LogicOp::Lesser),
                Some(&Token::Greater) => Some(LogicOp::Greater),
                Some(&Token::GreaterEqual) => Some(LogicOp::GreaterEqual),
                Some(&Token::LesserEqual) => Some(LogicOp::LesserEqual),
                Some(&Token::Equal) => Some(LogicOp::Equal),
                _ => None,
            };

            if let Some(operator) = operator {
                // We peeked a valid operator earlier, so we remove it
                self.tokens.remove(0);
                Some(Box::new(Expr::LogicalExpr {
                    lhs: term,
                    rhs: match self.expression() {
                        Some(expr) => expr,
                        None => return None,
                    },
                    operator,
                }))
            } else {
                Some(term)
            }
        } else {
            None
        }
    }

    fn term(&mut self) -> Option<Box<Expr>> {
        trace!("Entered term");

        if let Some(term) = self.factor() {
            let operator = match self.tokens.get(0) {
                Some(&Token::Plus) => Some(ArithmeticOp::Add),
                Some(&Token::Minus) => Some(ArithmeticOp::Sub),
                _ => None,
            };

            if let Some(operator) = operator {
                // We peeked a valid operator earlier, so we remove it
                self.tokens.remove(0);
                Some(Box::new(Expr::ArithmeticExpr {
                    lhs: term,
                    rhs: match self.term() {
                        Some(expr) => expr,
                        None => return None,
                    },
                    operator,
                }))
            } else {
                Some(term)
            }
        } else {
            None
        }

    }

    fn factor(&mut self) -> Option<Box<Expr>> {
        trace!("Entered factor");

        if let Some(factor) = self.atom() {
            let operator = match self.tokens.get(0) {
                Some(&Token::Asterisk) => Some(ArithmeticOp::Mult),
                Some(&Token::Slash) => Some(ArithmeticOp::Div),
                Some(&Token::Percent) => Some(ArithmeticOp::Mod),
                _ => None,
            };

            if let Some(operator) = operator {
                // We peeked a valid operator earlier, so we remove it
                self.tokens.remove(0);
                Some(Box::new(Expr::ArithmeticExpr {
                    lhs: factor,
                    rhs: match self.factor() {
                        Some(expr) => expr,
                        None => return None,
                    },
                    operator,
                }))
            } else {
                Some(factor)
            }
        } else {
            None
        }


    }

    fn atom(&mut self) -> Option<Box<Expr>> {
        trace!("Entered atom");

        match self.tokens.remove(0) {
            Token::OpenParen => {
                let expr = self.expression();
                match self.tokens.remove(0) {
                    Token::CloseParen => expr,
                    _ => None,
                }
            }
            Token::Ident(name) => Some(Box::new(Expr::Ident(name))),
            Token::Integer(value) => Some(Box::new(Expr::Value(ValueType::Int32(value)))),
            Token::Float(value) => Some(Box::new(Expr::Value(ValueType::Float32(value)))),
            Token::Bool(value) => Some(Box::new(Expr::Value(ValueType::Bool(value)))),
            Token::String(value) => Some(Box::new(Expr::Value(ValueType::String(value)))),
            _ => None,
        }
    }
}
