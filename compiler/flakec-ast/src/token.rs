//! Token

use lexer::keyword::Keyword;
use lexer::token::{BasicToken, TokenKind::*};


macro_rules! ast_token_structs {
    {$($name:ident = $tok:pat),*} => {$(
        #[derive(Debug, Clone, Eq, PartialEq)]
        #[doc = concat!("s", "s")]
        pub struct $name;

        impl<'a> crate::FromTokens<'a> for $name {
            type Error = core::convert::Infallible;

            fn from_tokens(
                input: &mut lexer::stream::TokenStream<'a>,
            ) -> Result<Self, crate::ParseError<'a, Self::Error>> {
                let tok = input.next().ok_or(crate::ParseError::UnexpectedEndOfFile)?;

                match tok.kind(){
                    $tok => Ok($name),
                    _ => Err(crate::ParseError::UnexpectedToken(tok))
                }
            }
        }
    )*}
}

ast_token_structs!{
    Fn = Keyword(Keyword::Fn),
    Let = Keyword(Keyword::Let),
    Return = Keyword(Keyword::Return),
    Eq = Basic(BasicToken::Eq),
    Semicolon = Basic(BasicToken::Semicolon),
    RBracket = Basic(BasicToken::RBracket),
    LParen = Basic(BasicToken::LParen),
    RParen = Basic(BasicToken::RParen),
    LBracket = Basic(BasicToken::LBracket),
    Comma = Basic(BasicToken::Comma),
    Colon = Basic(BasicToken::Colon),
    Dot = Basic(BasicToken::Dot),
    At = Basic(BasicToken::At),
    RevArrow = Basic(BasicToken::RevArrow)
}
