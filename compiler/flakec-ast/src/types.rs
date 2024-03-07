//! [Type]

use std::{convert::Infallible, fmt::Display};
use lexer::token::TokenKind;
use crate::{FromTokens, ParseError};

pub const UINT_UNKNOWN: Type = Type::UInt { bits: 0 };
pub const INT_UNKNOWN: Type = Type::Int { bits: 0 };

/// Flakec Frontend Types.
#[derive(Debug, Clone, Eq, PartialEq)]
#[non_exhaustive]
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
    // Bool
    Bool,
    /// Character
    Char,
    /// User-defined
    Custom(String),
    /// Generic User-defined
    GenericCustom {
        name: String,
        args: Vec<Type>
    },

    _Uninitialized(Box<Type>)
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
            Identifier("char") => Ok(Type::Char),
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int { bits: 8 } => f.write_str("i8"),
            Type::Int { bits: 16 } => f.write_str("i16"),
            Type::Int { bits: 32 } => f.write_str("i32"),
            Type::Int { bits: 64 } => f.write_str("i64"),
            Type::Int { .. } => f.write_str("<unknown-width-int>"), 
            Type::UInt { bits: 8 } => f.write_str("u8"),
            Type::UInt { bits: 16 } => f.write_str("u16"),
            Type::UInt { bits: 32 } => f.write_str("u32"),
            Type::UInt { bits: 64 } => f.write_str("u64"),
            Type::UInt { .. } => f.write_str("<unknown-width-uint>"),
            Type::Array { elem_ty, len } => f.write_fmt(format_args!("{}[{}]", elem_ty, len)),
            Type::Pointer { target_ty } => f.write_fmt(format_args!("*[{}]", target_ty)),
            Type::Never => f.write_str("!"),
            Type::Void => f.write_str("void"),
            Type::Bool => f.write_str("bool"),
            Type::Char => f.write_str("char"),
            Type::Custom(s) => f.write_str(s.as_str()),
            Type::GenericCustom { .. } => todo!(),
            Type::_Uninitialized(t) => f.write_str(t.to_string().as_str()),
            _ => Ok(())
        }
    }
}

impl Type{
    pub fn is_pointer(&self) -> bool{
        match self {
            Self::Pointer { .. } => true,
            _ => false
        }
    }

    pub fn is_int<const W: u8>(&self) -> bool{
        match self {
            Self::Int { bits } | Self::UInt { bits } => bits == &W,
            _ => false
        }
    }
} 