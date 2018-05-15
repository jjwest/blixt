use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

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

pub struct Context {
    pub source_code: HashMap<PathBuf, String>,
    pub interner: StringInterner,
    pub debug_mode: bool,
}

impl Context {
    pub fn new() -> Self {
        Self {
            source_code: HashMap::new(),
            interner: StringInterner::new(),
            debug_mode: false,
        }
    }

    pub fn error(&mut self, message: &str, location: Location) {
        let filename = self.interner.get(location.file);

        let source = match self.source_code.get(Path::new(filename)) {
            Some(source) => source,
            None => {
                let src = fs::read_to_string(filename)
                    .expect(&format!("Failed to open source file {}", filename));
                self.source_code.insert(PathBuf::from(filename), src);
                self.source_code.get(Path::new(filename)).unwrap()
            }
        };

        let line = source.lines().nth(location.line - 1).expect("Invalid line");
        let prelude = format!("{}: Line {}: ", filename, location.line);
        eprintln!("{}{}", prelude, line);
        eprintln!(
            "{:>prelude$}{:^>len$}",
            " ",
            " ",
            prelude = prelude.len(),
            len = location.span.len
        );
        eprintln!("{}\n", message);

        if self.debug_mode {
            panic!();
        }
    }
}

pub struct StringInterner {
    strings: Vec<String>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            strings: Vec::new(),
        }
    }

    pub fn intern(&mut self, string: &str) -> usize {
        if let Some(pos) = self.strings.iter().position(|s| s == string) {
            pos
        } else {
            self.strings.push(string.to_string());
            self.strings.len() - 1
        }
    }

    pub fn get(&self, string: usize) -> &str {
        &self.strings[string]
    }
}
