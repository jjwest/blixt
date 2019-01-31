use copy_arena::Arena;
use hashbrown::HashMap;
use termcolor::{BufferWriter, Color, ColorChoice, ColorSpec, WriteColor};

use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::slice;
use std::str;

use crate::location::Location;

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, Hash, PartialEq)]
pub struct Symbol(u32);

#[cfg(test)]
impl Symbol {
    pub fn new(n: u32) -> Self {
        Symbol(n)
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
            source_code: HashMap::default(),
            interner: StringInterner::new(),
            debug_mode: false,
        }
    }

    pub fn report_error(&mut self, message: &str, location: Location) {
        let filename = self.interner.get(location.file);

        let source = self
            .source_code
            .entry(PathBuf::from(filename))
            .or_insert_with(|| fs::read_to_string(filename).unwrap());

        let stderr = BufferWriter::stderr(ColorChoice::Always);
        let mut buf = stderr.buffer();

        let line = source
            .lines()
            .nth((location.line - 1) as usize)
            .expect("Invalid line");
        let prelude = format!("{}: Line {}: ", filename, location.line);

        writeln!(&mut buf, "       | {}{}", prelude, line).unwrap();

        if buf.supports_color() {
            buf.set_color(
                &ColorSpec::new().set_bold(true).set_fg(Some(Color::Red)),
            )
            .unwrap();
            write!(&mut buf, "Error: ").unwrap();
            buf.reset().unwrap();
        } else {
            write!(&mut buf, "Error: ").unwrap();
        }

        write!(&mut buf, "|").unwrap();

        for _ in 0..(prelude.len() as u32 + location.span.start) {
            write!(&mut buf, " ").unwrap();
        }

        if buf.supports_color() {
            buf.set_color(
                &ColorSpec::new().set_bold(true).set_fg(Some(Color::Blue)),
            )
            .unwrap();

            for _ in 0..location.span.len {
                write!(&mut buf, "^").unwrap();
            }

            buf.reset().unwrap();
        } else {
            for _ in 0..location.span.len {
                write!(&mut buf, "^").unwrap();
            }
        }

        writeln!(&mut buf).unwrap();

        if buf.supports_color() {
            buf.set_color(&ColorSpec::new().set_bold(true)).unwrap();
            writeln!(&mut buf, "       | {}\n\n", message).unwrap();
            buf.reset().unwrap();
        } else {
            writeln!(&mut buf, "       | {}\n\n", message).unwrap();
        }

        stderr.print(&buf).unwrap();

        if self.debug_mode {
            panic!();
        }
    }
}

pub struct StringInterner {
    arena: Arena,
    strings: Vec<&'static str>,
    symbols: HashMap<&'static str, u32>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            arena: Arena::with_capacity(1024 * 1024),
            strings: Vec::new(),
            symbols: HashMap::default(),
        }
    }

    pub fn intern(&mut self, string: &str) -> Symbol {
        if let Some(sym) = self.symbols.get(string) {
            return Symbol(*sym);
        }

        let string: &'static str = unsafe {
            let mut allocator = self.arena.allocator();
            let len = string.len();
            let s = allocator.alloc_slice(string.as_bytes()).as_ptr();
            str::from_utf8_unchecked(slice::from_raw_parts(s, len))
        };

        let sym = self.strings.len() as u32;
        self.strings.push(string);
        self.symbols.insert(string, sym);

        Symbol(sym)
    }

    pub fn get(&self, string: Symbol) -> &str {
        &self.strings[string.0 as usize]
    }
}
