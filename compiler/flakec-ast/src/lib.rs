use std::{convert::Infallible, fmt::Debug, ops::Deref};

use lexer::{span::Span, stream::TokenStream, token::{Token, TokenKind}};

pub mod expression;
pub mod operator;
pub mod statement;
pub mod value;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    inner: T,
    span: Span,
}



pub trait FromTokens<'a>: Sized {
    type Error;

    fn from_tokens(input: &mut TokenStream<'a>) -> Result<Self, ParseError<'a, Self::Error>>;

    fn can_parse(_: &mut TokenStream<'a>) -> bool{
        false
    }
}


pub fn expect_token<'a>(
    input: &mut TokenStream<'a>,
    tok: TokenKind,
) -> Result<(), ParseError<'a, Infallible>> {
    let prev = input.next().ok_or(ParseError::UnexpectedEndOfFile)?;
    if prev.kind() == &tok {
        Ok(())
    } else {
        Err(ParseError::UnexpectedToken(prev))
    }
}

pub trait ToTokens<'a>: Sized {
    type Error;

    fn from_tokens(&'a self) -> Result<Vec<Token<'a>>, ParseError<'a, Self::Error>>;
}

pub fn parse<'a, T: FromTokens<'a>>(input: &mut TokenStream<'a>) -> Result<T, ParseError<'a, T::Error>>{
    T::from_tokens(input)
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
