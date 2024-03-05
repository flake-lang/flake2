//! Flakec Parser

#![feature(adt_const_params)]

pub mod lit;

#[macro_use] extern crate __derive;

use lexer::{keyword::{self, Keyword}, stream::TokenStream, token::{BasicToken, ConstToken as ConstTokenKind, Token as LexToken, TokenKind}};

pub trait Parse: Sized{
    type Output;

    fn parse(input: &mut TokenStream) -> Option<Self>;
}

pub trait TokenStreamExt {
    fn parse<T: Parse>(&mut self) -> Option<T>;
}

impl<'a> TokenStreamExt for TokenStream<'a>{
    fn parse<T: Parse>(&mut self) -> Option<T> {
        T::parse(self)
    }
}

pub struct ConstToken<const TOKEN: ConstTokenKind>;

impl<const TOKEN: ConstTokenKind> Parse for ConstToken<TOKEN>{
    type Output = Self;

    fn parse(input: &mut TokenStream) -> Option<Self> {
        if match (TOKEN, input.peek()?.kind()){
          (ConstTokenKind::Basic(cb),  TokenKind::Basic(basic)) => cb == *basic,
          (ConstTokenKind::Keyword(ckw),  TokenKind::Keyword(kw)) => ckw == *kw,
          _ => false
        } {
            Some(Self)
        }else {
            None
        }
    }
}
