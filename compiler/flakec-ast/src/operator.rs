use std::convert::Infallible;

use lexer::{stream::TokenStream, token::{BasicToken, Token, TokenKind, TokenType}};

use crate::ParseError;




#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Operator {
    pub(self) _token: BasicToken
}

pub fn parse_operator<'a>(input: &mut TokenStream<'a>) -> Result<Operator, ParseError<'a, Infallible>>{
    let tok = input.peek().ok_or(ParseError::UnexpectedEndOfFile)?;

    match tok.kind(){
        TokenKind::Basic(bt) => match bt.get_type(){
            TokenType::Operator => Ok(Operator { _token: bt.clone() }),
            _ => Err(ParseError::UnexpectedToken(tok.clone()))
        },
        _ => Err(ParseError::UnexpectedToken(tok.clone()))
    }
}