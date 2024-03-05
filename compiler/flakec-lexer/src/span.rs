//! Span

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Span{
    pub start: usize,
    pub end: usize
}