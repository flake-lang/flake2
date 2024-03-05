//! Literals

use lexer::{literal::Literal, span::Span};

use crate::{Parse, TokenStreamExt as _};

#[derive(Debug, Clone)]
pub enum Lit {
    String(LitStr),
}

impl Parse for Lit {
    type Output = Self;

    fn parse(input: &mut lexer::stream::TokenStream) -> Option<Self> {
        match input.peek()?.literal()? {
            Literal::String(_) | Literal::OwnedString(_) => {
                input.parse::<LitStr>().map(|s| Self::String(s))
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LitStr {
    _value: String,
    _span: Span,
}

impl Parse for LitStr {
    type Output = Self;

    fn parse(input: &mut lexer::stream::TokenStream) -> Option<Self> {
        let s = input.peek()?.literal()?.string()?;

        Self {
            _value: s,
            _span: input.peek()?.span(),
        }
        .into()
    }
}
