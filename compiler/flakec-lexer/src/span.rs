//! Span

use std::fmt::Debug;

#[derive(Clone, Copy, Eq, PartialEq, Default)]
pub struct Span{
    pub col: u32,
    pub line: u32,
    pub start: usize,
    pub end: usize
}

impl Span{
    pub fn combine_with(&self, other: Span) -> Span{
        Span { col: self.col, start: self.start, end: other.end, line: self.line }
    }
}

impl Debug for Span{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}..{}", self.start, self.end))
    }
}