//! TokenStream

use std::{convert::Infallible, str::Chars};

use lexgen_util::{LexerError, Loc};

use crate::{
    span::Span,
    token::{Token, TokenKind},
    Lexer,
};

pub struct TokenStream<'a> {
    buffer: Vec<Option<Token<'a>>>,
    position: usize,
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.position += 1;

        self.buffer.get(self.position).as_ref().cloned()?.clone() // FIXME
    }
}

fn lexgen_to_token(
    lexgen_tok: Result<(Loc, TokenKind, Loc), LexerError<Infallible>>,
) -> Option<Token> {
    match lexgen_tok {
        Ok((start, tok, end)) => Some(Token {
            kind: tok,
            span: Span {
                start: start.byte_idx,
                end: end.byte_idx,
            },
        }),
        Err(_) => None,
    }
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str) -> Self {
        Self{
            buffer: crate::Lexer::new(input)
            .map(lexgen_to_token)
            .collect::<Vec<_>>(),
            position: 0
        }
    }

    pub fn peek(&mut self) -> Option<&Token<'a>>{
        let tok = self.buffer.get(self.position)?;

        tok.as_ref()
    }

    pub fn far_peek(&mut self, dst: usize) -> Option<&Token<'a>>{
        let tok = self.buffer.get(self.position + dst)?;

        tok.as_ref()
    }

    pub fn backtrack(&mut self, dst: usize) {
        self.position -= dst;
    }
}
