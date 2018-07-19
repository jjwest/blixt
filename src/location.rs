pub type InternedString = usize;

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub file: InternedString,
    pub line: u32,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: u32,
    pub len: u32,
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
