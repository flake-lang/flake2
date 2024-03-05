//! Keyword

#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum Keyword{
    Fn,
    Let,
    Construct,
    If,
    For,
    Else,
    While,
    Return,
    Break,
    Continue,
}