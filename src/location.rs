pub type InternedString = usize;

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub file: InternedString,
    pub line: usize,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub len: usize,
}

impl Default for Location {
    fn default() -> Location {
        Location {
            file: 0,
            line: 0,
            span: Span { start: 0, len: 0 },
        }
    }
}
