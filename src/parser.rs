use lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    pub fn generate_syntax_tree(&mut self) {}
}

type NodeId = usize;

struct Block {
    stmts: Vec<Stmt>,
}

struct Stmt {
    id: NodeId,
    node: StmtKind,
}

enum StmtKind {
    Assign,
    Expr,
    If,
    Io,
    Loop,
    Return,
}
