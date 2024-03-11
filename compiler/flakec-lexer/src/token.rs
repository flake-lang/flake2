//! Token


use std::marker::ConstParamTy;

use crate::{keyword::Keyword, literal::Literal, span::Span};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
#[non_exhaustive]
pub enum TokenKind<'input>{
    Literal(Literal<'input>),
    Keyword(Keyword),
    Identifier(&'input str),
    Basic(BasicToken),
    EOF,
    
    /// Default
    #[default] _Invalid,
}

#[derive(PartialEq, Eq, ConstParamTy)]
pub enum ConstToken{
    Keyword(Keyword),
    Basic(BasicToken)
}

#[derive(Debug, Clone, Eq, PartialEq, ConstParamTy)]
#[non_exhaustive]
pub enum BasicToken{
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    ExplMark,
    Caret,
    Hash,
    EqEq,
    TildeEq,
    NotEq,
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
    PlusEq,
    RevArrow,
    MinusEq,
    Dot,
    DotDot,
    At,
    Arrow,
    FatArrow,
    QMark,
    Ampersand,
    DotDotDot,
}


#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Token<'a>{
    pub kind: TokenKind<'a>,
    pub span: Span
}

/// A [Token]'s Type.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenType{
    Operator,
    Unary,
    Comparison,
    Assignment,
    Keyword,
    Literal,
    Control,
    Identifier,
    Other,
}

impl BasicToken{
    pub fn get_type(&self) -> TokenType{
        use BasicToken::*;
        match self {
            Plus | Minus | Star | Slash | Percent | ExplMark  | Ampersand => TokenType::Operator,
            EqEq | Lt | Gt | LtEq | GtEq | NotEq => TokenType::Comparison,
            Eq | PlusEq | MinusEq => TokenType::Assignment,
            _ => TokenType::Other
        }
    }
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

    pub fn ident(&'a self) -> Result<&'a str, Self> {
        match self.kind(){ 
            TokenKind::Identifier(ident) => Ok(*ident),
            _ => Err(self.clone())
        } 
    }

    pub fn get_type(&self) -> TokenType{
        match self.kind(){
            TokenKind::Basic(bt) => bt.get_type(),
            TokenKind::Identifier(_) => TokenType::Identifier,
            TokenKind::Keyword(_) => TokenType::Keyword,
            TokenKind::Literal(_) => TokenType::Literal,
            TokenKind::EOF => TokenType::Control,
            _ => TokenType::Other
        }
    }

    pub fn span(&self) -> Span{
        self.span
    }

    pub fn kind(&self) -> &TokenKind{
        &self.kind
    }

}