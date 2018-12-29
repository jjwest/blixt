use crate::common::Symbol;

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub file: Symbol,
    pub line: u32,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: u32,
    pub len: u32,
}
