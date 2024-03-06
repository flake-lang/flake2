use std::convert::Infallible;

use lexer::stream::TokenStream;

use crate::{ParseError, Spanned};

#[derive(Debug, Clone)]
pub enum Value_{
    Str(String),
    Number{
        is_negative: bool,
        value: String
    },
    Boolean(bool),
}

pub type Value = Spanned<Value_>;

pub fn parse_value<'a>(input: &mut TokenStream<'a>) -> Result<Value, ParseError<'a, Infallible>>{
    let prev = input.next().ok_or(ParseError::UnexpectedEndOfFile)?;

    let val = match prev.literal().ok_or(ParseError::UnexpectedToken(prev.clone()))?.clone(){
        lexer::literal::Literal::String(s) => Ok(Value_::Str((*s).to_owned())),
        lexer::literal::Literal::OwnedString(s) => Ok(Value_::Str(s.clone())),
        lexer::literal::Literal::Number(n) => Ok(Value_::Number { is_negative: false, value: (*n).to_owned()}),
        lexer::literal::Literal::SignedNumber(n) => Ok(Value_::Number { is_negative: true, value: (*n).to_owned()}),
        lexer::literal::Literal::Boolean(b) => Ok(Value_::Boolean(b)),
        _ => return Err(ParseError::UnexpectedToken(prev))
    }?;

    Ok(Value::with_span(val, prev.span()))
}