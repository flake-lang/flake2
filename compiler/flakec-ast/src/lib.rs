use std::ops::Deref;

use lexer::{span::Span, token::Token};

pub mod expression;
pub mod operator;
pub mod statement;
pub mod value;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    inner: T,
    span: Span,
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> Spanned<T> {
    pub fn with_span(value: T, span: Span) -> Self {
        Self { inner: value, span }
    }

    pub fn combined<U>(&self, other: &Spanned<U>) -> Span {
        Span {
            start: self.span.start,
            end: other.span.end,
        }
    }
}

#[derive(Debug)]
pub enum ParseError<'a, T> {
    UnexpectedToken(Token<'a>),
    UnexpectedEndOfFile,

    Custom(T),
}
