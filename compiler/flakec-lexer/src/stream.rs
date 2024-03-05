//! TokenStream

use std::str::Chars;

use crate::{
    span::Span,
    token::Token,
    Lexer,
};

pub struct TokenStream<'a>
{
    lexer: Lexer<'a, Chars<'a>>,
}

impl<'a> Iterator for TokenStream<'a>
{
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (start, tok, end) = self.lexer.next()?.ok()?;

        Some(Token {
            kind: tok,
            span: Span {
                start: start.byte_idx,
                end: end.byte_idx,
            },
        })
    }
}

impl<'a> TokenStream<'a>
{
    pub fn new(input: &'a str) -> Self{
        Self { lexer: crate::Lexer::new(input) }
    }
}