use std::convert::Infallible;

use lexer::{
    stream::TokenStream,
    token::{BasicToken, Token, TokenKind, TokenType},
};

use crate::{ParseError, Spanned};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Operator_ {
    pub _token: BasicToken,
    pub(self) _is_cmp: bool,
}

pub type Operator = Spanned<Operator_>;

pub fn parse_operator<'a>(
    input: &mut TokenStream<'a>,
) -> Result<Operator, ParseError<'a, Infallible>> {
    let tok = input.next().ok_or(ParseError::UnexpectedEndOfFile)?;

    match tok.kind() {
        TokenKind::Basic(bt) => match bt.get_type() {
            TokenType::Operator => Ok(Operator::with_span(
                Operator_ {
                    _token: bt.clone(),
                    _is_cmp: false,
                },
                tok.span(),
            )),
            TokenType::Comparison => Ok(Operator::with_span(
                Operator_ {
                    _token: bt.clone(),
                    _is_cmp: true,
                },
                tok.span(),
            )),
            _ => Err(ParseError::UnexpectedToken(tok.clone())),
        },
        _ => Err(ParseError::UnexpectedToken(tok.clone())),
    }
}
