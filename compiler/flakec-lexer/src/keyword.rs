//! Keyword

use std::marker::ConstParamTy;

#[derive(Debug, Clone, PartialEq, Eq, ConstParamTy)]
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