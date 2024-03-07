use std::{convert::Infallible, fmt::Debug, ops::Deref};

use lexer::{span::Span, stream::TokenStream, token::{Token, TokenKind}};
use pass::{Pass, PassKind};
use statement::Statement;

pub mod expression;
pub mod operator;
pub mod statement;
pub mod value;
pub mod pass;
pub mod token;
pub mod types;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    inner: T,
    span: Span,
}

#[derive(Debug, Clone)]
pub struct AST<'a> {
    pub _nodes: Vec<Statement<'a>>
}

impl<'a> AST<'a>{
    pub fn parse(input: &mut TokenStream<'a>) -> Result<Self, ParseError<'a, Infallible>>{
        let mut nodes: Vec<Statement> = vec![];

        loop {
            if input.peek().is_none() {
                break;
            }

            nodes.push(Statement::from_tokens(input)?);
        }

        Ok(Self { _nodes: nodes })
    }

    pub fn run_pass<T>(&mut self, pass: T) -> Result<(), T::Error>
    where
        T:  Pass<'a>
    {
        let mut pass_local = pass;
        if T::kind() == PassKind::Standard {
            Ok(pass_local.run(self)?)
        }else if T::kind() == PassKind::Check {
            Ok(pass_local.check(self)?)
        } else  {
            unreachable!()
        }
    }
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
            line: self.span.line,
            col: self.span.col
        }
    }
}

macro_rules! ast_struct {
    {
        struct $name:ident { $($sub:ident: $sub_ty:ty), *};
    } => {
        #[allow(unused)]
        #[derive(Debug, Clone)]
        struct $name { $($sub: $sub_ty), *}

        impl<'a> FromTokens<'a> for $name {
            type Error = Infallible;

            fn from_tokens(
                _input: &mut lexer::stream::TokenStream<'a>,
            ) -> Result<Self, crate::ParseError<'a, Self::Error>> {
                Ok(Self {
                    $($sub: crate::parse(_input)?),*
                })
            }
        }
    }
}

#[derive(Debug)]
pub enum ParseError<'a, T> {
    UnexpectedToken(Token<'a>),
    UnexpectedEndOfFile,

    Custom(T),
}
use crate::types::Type;

ast_struct!{
    struct Type2 {
        t1: Type,
        t2: Type
    };
}

#[test]
fn feature() {
    
}