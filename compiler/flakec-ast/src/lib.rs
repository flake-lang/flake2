use std::{convert::Infallible, fmt::Debug, marker::PhantomData, ops::Deref};

use item::{Function, Item};
use lexer::{
    span::Span,
    stream::TokenStream,
    token::{BasicToken, Token, TokenKind},
};
use pass::{Pass, PassKind};
use statement::Statement;

pub mod expression;
pub mod item;
pub mod operator;
pub mod pass;
pub mod statement;
pub mod token;
pub mod types;
pub mod value;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    inner: T,
    span: Span,
}

pub fn restore_on_err<'a, T, F, E>(
    f: F,
    input: &mut TokenStream<'a>,
) -> Result<T, ParseError<'a, E>>
where
    F: Fn(&mut TokenStream<'a>) -> Result<T, ParseError<'a, E>>,
{
    let old_pos = input.pos();

    match f(input) {
        Ok(v) => Ok(v),
        err => {
            input.set_pos(old_pos);
            err
        }
    }
}

#[derive(Debug, Clone)]
pub struct AST<'a> {
    pub _nodes: Vec<Item<'a>>,
}

impl<'a> AST<'a> {
    pub fn parse(input: &mut TokenStream<'a>) -> Result<Self, ParseError<'a, Infallible>> {
        let mut nodes: Vec<Item<'a>> = vec![];

        loop {
            if input.peek().is_none() {
                break;
            }

            nodes.push(Item::from_tokens(input)?);
        }

        Ok(Self { _nodes: nodes })
    }

    pub fn run_pass<T>(&mut self, pass: T) -> Result<(), T::Error>
    where
        T: Pass<'a>,
    {
        let mut pass_local = pass;
        if T::kind() == PassKind::Standard {
            Ok(pass_local.run(self)?)
        } else if T::kind() == PassKind::Check {
            Ok(pass_local.check(self)?)
        } else {
            unreachable!()
        }
    }
}

pub trait FromTokens<'a>: Sized {
    type Error;

    fn from_tokens(input: &mut TokenStream<'a>) -> Result<Self, ParseError<'a, Self::Error>>;

    fn can_parse(_: &mut TokenStream<'a>) -> bool {
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

pub fn parse<'a, T: FromTokens<'a>>(
    input: &mut TokenStream<'a>,
) -> Result<T, ParseError<'a, T::Error>> {
    T::from_tokens(input)
}

#[derive(Debug, Clone)]
/// Something wrapper in a start and end.
pub struct WrappedIn<'a, S, E, T>
where
    S: FromTokens<'a>,
    E: FromTokens<'a>,
    T: FromTokens<'a>,
{
    _start_phatom: PhantomData<S>,
    pub inner: T,
    _end_phantom: PhantomData<E>,
    _phantom: PhantomData<&'a ()>,
}

#[derive(Debug, Clone)]
pub struct Punctuated<T, P>(pub Vec<T>, PhantomData<P>);

impl<'a, T, P> FromTokens<'a> for Punctuated<T, P>
where
    T: FromTokens<'a, Error = Infallible>,
    P: FromTokens<'a, Error = Infallible>,
{
    type Error = Infallible;

    fn from_tokens(input: &mut TokenStream<'a>) -> Result<Self, ParseError<'a, Self::Error>> {
        let mut values: Vec<T> = vec![];

      

        loop {
            values.push(match restore_on_err(T::from_tokens, input){
                Ok(v) => v,
                Err(_) => break
            });

            match restore_on_err(P::from_tokens, input) {
                Ok(_) => {
                   //  _ = P::from_tokens(input);
                    continue;
                },
                Err(_) => {
                   break
                }
            }
        };

        // input.set_pos(input.pos().saturating_sub(1));

        Ok(Self(values, PhantomData))
    }
}

impl<'a, T> FromTokens<'a> for Vec<T>
where
    T: FromTokens<'a>
{
    type Error = T::Error;

    fn from_tokens(input: &mut TokenStream<'a>) -> Result<Self, ParseError<'a, Self::Error>> {
        let mut collected: Vec<T> = vec![];

        loop {
            match restore_on_err(T::from_tokens, input) {
                Ok(v) => collected.push(v),
                Err(_) => break,
            }
        };

        Ok(collected)
    }
}

impl<'a, S: FromTokens<'a>, E, T> Deref for WrappedIn<'a, S, E, T>
where
    S: FromTokens<'a>,
    E: FromTokens<'a>,
    T: FromTokens<'a>,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a, S, E, T> FromTokens<'a> for WrappedIn<'a, S, E, T>
where
    S: FromTokens<'a, Error = Infallible>,
    E: FromTokens<'a, Error = Infallible>,
    T: FromTokens<'a, Error = Infallible>,
{
    type Error = Infallible;

    fn from_tokens(input: &mut TokenStream<'a>) -> Result<Self, ParseError<'a, Self::Error>> {
        _ = S::from_tokens(input)?;

        let res = T::from_tokens(input)?;

        _ = E::from_tokens(input)?;

        Ok(Self {
            _start_phatom: PhantomData,
            inner: res,
            _end_phantom: PhantomData,
            _phantom: PhantomData,
        })
    }
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
            col: self.span.col,
        }
    }
}

#[macro_export]
macro_rules! ast_struct_ty_inner {
    ($input:expr, $t:ty) => {
        crate::parse($input)?
    };
    ($input:expr, Option<$t:ty>) => {
        crate::parse($input).ok()
    };
    ($input:expr, #[on_error($c:expr)] $t:ty) => {
        match crate::parse($input) {
            Ok(v) => v,
            Err(err) => $c,
        }
    };
}

impl<'a, T: FromTokens<'a>> FromTokens<'a> for Option<T> {
    type Error = T::Error;

    fn from_tokens(input: &mut TokenStream<'a>) -> Result<Self, ParseError<'a, Self::Error>> {
        Ok(restore_on_err(T::from_tokens, input).ok())
    }
}

#[derive(Debug, Clone)]
pub struct Identifier(pub String);

impl<'a> FromTokens<'a> for Identifier {
    type Error = Infallible;

    fn from_tokens(input: &mut TokenStream<'a>) -> Result<Identifier, ParseError<'a, Self::Error>> {
        let tok = input.next().ok_or(ParseError::UnexpectedEndOfFile)?;

        match tok.clone().kind().clone() {
            TokenKind::Identifier(ident) => Ok(Self(ident.to_owned())),
            _ => Err(ParseError::UnexpectedToken(tok.clone())),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block<'a>(pub Vec<Statement<'a>>);

impl<'a> FromTokens<'a> for Block<'a> {
    type Error = Infallible;

    fn from_tokens(input: &mut TokenStream<'a>) -> Result<Self, ParseError<'a, Self::Error>> {
        _ = expect_token(input, TokenKind::Basic(BasicToken::LBrace))?;

        let mut toks: Vec<Statement<'a>> = vec![];

        loop {
            let tok = input.peek().ok_or(ParseError::UnexpectedEndOfFile)?;

            if tok.kind() == &TokenKind::Basic(BasicToken::RBrace) {
                break;
            }

            toks.push(parse::<Statement>(input)?)
        }

        _ = expect_token(input, TokenKind::Basic(BasicToken::RBrace))?;

        return Ok(Self(toks));
    }
}

#[macro_export]
macro_rules! ast_struct {
    {
        $(#[doc = $doc_str:literal])?
        $_pub:vis struct $name:ident { $($sub:ident: $sub_ty:ty), *};
    } => {
        #[allow(unused)]
        #[derive(Debug, Clone)]
        $(#[doc = $doc_str])?
        $_pub struct $name<'a> { $(pub $sub: $sub_ty), *, _phantom: core::marker::PhantomData::<&'a ()>}

        impl<'a> crate::FromTokens<'a> for $name<'a> {
            type Error = core::convert::Infallible;

            fn from_tokens(
                _input: &mut lexer::stream::TokenStream<'a>,
            ) -> Result<Self, crate::ParseError<'a, Self::Error>> {
                Ok(Self {
                    $($sub: <$sub_ty>::from_tokens(_input)?),*,
                    _phantom: core::marker::PhantomData
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

ast_struct! {
    struct Type2 {
        t1: Option<Type>,
        t2: Type
    };
}

#[test]
fn feature() {}
