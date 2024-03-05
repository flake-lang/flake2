//! Token

use crate::{keyword::Keyword, literal::Literal, span::Span};

#[derive(Debug, Clone, Eq, PartialEq)]
#[non_exhaustive]
pub enum TokenKind<'input>{
    Literal(Literal<'input>),
    Keyword(Keyword),
    Identifier(&'input str),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Hash,
    EqEq,
    TildeEq,
    LtEq,
    GtEq,
    Lt,
    Gt,
    Eq,
    LParen,
    RParen,
    LBrace,
    RBrace,
    RBracket,
    LBracket,
    Semicolon,
    Colon,
    Comma,
    Dot,
    DotDot,
    DotDotDot,
    EOF
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token<'a>{
    pub(crate) kind: TokenKind<'a>,
    pub(crate) span: Span
}

impl<'a> Token<'a>{
    pub fn keyword(&self) -> Option<&Keyword> {
        match &self.kind{
            TokenKind::Keyword(kw) => Some(kw),
            _ => None
        }
    }

    pub fn literal(&self) -> Option<&Literal<'a>> {
        match &self.kind{
            TokenKind::Literal(lit) => Some(lit),
            _ => None
        }
    }
}