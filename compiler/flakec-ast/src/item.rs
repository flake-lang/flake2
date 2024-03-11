use std::convert::Infallible;

use lexer::{keyword::Keyword, token::TokenKind};

use crate::{
    ast_struct, parse, token, types::Type, value::Value, Block, FromTokens, Identifier, ParseError, Punctuated, WrappedIn
};

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Item<'a> {
    Function(Function<'a>),
    Import(Infallible),
    Module(Infallible),
}

impl<'a> FromTokens<'a> for Item<'a> {
    type Error = Infallible;

    fn from_tokens(input: &mut lexer::stream::TokenStream<'a>) -> Result<Self, crate::ParseError<'a, Self::Error>> {
        let tok = input.peek().ok_or(ParseError::UnexpectedEndOfFile)?;

        Ok(Self::Function(parse(input)?))
    }
}

pub type MarkerParams<'a> =
    WrappedIn<'a, token::LParen, token::RParen, Punctuated<Value, token::Comma>>;

ast_struct! {
    pub struct Marker{
        at_token: token::At,
        name: Identifier,
        params: Option<MarkerParams<'a>>
    };
}

ast_struct! {
    pub struct FnArg{
        name: Identifier,
        colon_token: token::Colon,
        ty: Type
    };
}

ast_struct! {
    pub struct ExplicitRetType{
        colon_token: token::Colon,
        ty: Type
    };
}

ast_struct! {
    /// A [Function] in an [AST].
    pub struct Function {
        markers: Vec<Marker<'a>>,
        fn_token: token::Fn,
        name: Identifier,
        args: WrappedIn<'a, token::LBracket,token::RBracket, Punctuated<FnArg<'a>, token::Comma>>,
        ret: Option<ExplicitRetType<'a>>,
        body: Option<Block<'a>>,
        semicolon_token: token::Semicolon
    };
}
