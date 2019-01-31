use std::ops::{Add, AddAssign};

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

fn join_locations(a: &Location, b: &Location) -> Location {
    let (smaller, larger) = {
        if a.span.start < b.span.start {
            (a, b)
        } else {
            (b, a)
        }
    };

    let mut location = *smaller;
    location.span.len += larger.span.start
        - (location.span.start + location.span.len)
        + larger.span.len;

    location
}

impl Add for Location {
    type Output = Location;
    fn add(self, other: Location) -> Location {
        join_locations(&self, &other)
    }
}

impl AddAssign for Location {
    fn add_assign(&mut self, other: Location) {
        *self = join_locations(self, &other);
    }
}
