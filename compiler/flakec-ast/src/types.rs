//! [Type]

use std::convert::Infallible;
use lexer::token::TokenKind;
use crate::{FromTokens, ParseError};

/// Flakec Frontend Types.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    /// Signed Int.
    Int { bits: u8 },
    /// Unsigned Int.
    UInt { bits: u8 },
    /// Array with `len` elements of type `elem_ty`.
    Array { elem_ty: Box<Type>, len: u64 },
    /// A Pointer to a value of the type `target_ty`.
    Pointer { target_ty: Box<Type> },
    /// The never type.
    Never,
    /// The Void type
    Void,
    /// Fallback Type...! FIXME!
    Custom(String),
}

impl<'a> FromTokens<'a> for Type {
    type Error = Infallible;

    fn from_tokens(
        input: &mut lexer::stream::TokenStream<'a>,
    ) -> Result<Self, crate::ParseError<'a, Self::Error>> {
        let tok = input.next().ok_or(ParseError::UnexpectedEndOfFile)?;

        use TokenKind::{Identifier, Basic};
        use lexer::token::BasicToken::{Star, ExplMark};

        match tok.kind() {

            // Unsigned Ints
            Identifier("u64") => Ok(Type::UInt { bits: 64 }),
            Identifier("u32") => Ok(Type::UInt { bits: 32 }),
            Identifier("u16") => Ok(Type::UInt { bits: 16 }),
            Identifier("u8") => Ok(Type::UInt { bits: 8 }),

            // Signed Ints
            Identifier("i64") => Ok(Type::Int { bits: 64 }),
            Identifier("i32") => Ok(Type::Int { bits: 32 }),
            Identifier("i16") => Ok(Type::Int { bits: 16 }),
            Identifier("i8") => Ok(Type::Int { bits: 8 }),


            // Void and Never(!)
            Identifier("void") => Ok(Type::Void),
            Basic(ExplMark) => Ok(Type::Never),

            // Pointers
            Basic(Star) => Ok(Type::Pointer {
                target_ty: Self::from_tokens(input)?.into(),
            }),

            // Fallback
            Identifier(ident) => Ok(Type::Custom(ident.to_string())),
            
            // `TokenKind` is `#[non_exhaustive]` and there are other tokens. :)
            _ => Err(ParseError::UnexpectedToken(tok))
        }
    }
}
