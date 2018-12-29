use crate::common::Symbol;
use crate::location::Location;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Location,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    // Logical Operators
    And,
    Or,
    Not,

    // Comparison operators
    Equal,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
    NotEqual,

    // operators
    Assign,
    AddAssign,
    DivAssign,
    MulAssign,
    SubAssign,
    ModAssign,

    // Arithmetic operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Field,

    // Declarations
    FunctionDecl,
    VarDecl,
    StructDecl,

    // Delimiters
    CloseBrace,
    CloseBracket,
    CloseParen,
    Comma,
    SemiColon,
    Colon,

    ReturnDecl,
    OpenBrace,
    OpenBracket,
    OpenParen,

    // Keywords
    If,
    Else,
    For,
    Return,
    While,
    Range(i64, i64),
    In,

    Ident(Symbol),
    Bool(bool),
    Integer(i32),
    Float(f32),
    String(Symbol),

    // Types
    BoolType,
    FloatType,
    IntType,
    StringType,
}
