use std::{
    convert::Infallible,
    ops::{Deref, IndexMut},
    path::MAIN_SEPARATOR,
};

use lexer::{
    span::Span,
    stream::TokenStream,
    token::{BasicToken, Token, TokenKind, TokenType},
};

use crate::{
    operator::{parse_operator, Operator, Operator_},
    value::{self, parse_value, Value, Value_},
    ParseError,
};

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Expression {
    // "Hello!"
    Constant(Value),
    // !true
    Unary {
        op: Operator,
        child: Box<Expression>,
        span: Span,
    },
    // 1 + 1
    Binary {
        op: Operator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    // a
    Variable {
        name: String,
        span: Span,
    },
    // test(1, "abc")
    FunctionCall {
        name: String,
        args: Vec<Expression>,
    },
}

pub fn restore_on_err<'a, T, F>(
    f: F,
    input: &mut TokenStream<'a>,
) -> Result<T, ParseError<'a, Infallible>>
where
    F: Fn(&mut TokenStream<'a>) -> Result<T, ParseError<'a, Infallible>>,
{
    let old_pos = input.pos();

    match f(input) {
        Ok(v) => Ok(v),
        err => {
            input.set_pos(old_pos);
            err
        }
    }
}


pub fn parse_unary<'a>(
    input: &mut TokenStream<'a>,
) -> Result<Expression, ParseError<'a, Infallible>> {
    let op = parse_operator(input)?;
    let child = parse_value(input)?;

    let span = op.combined(&child);

    Ok(Expression::Unary {
        op,
        child: Expression::Constant(child).into(),
        span,
    })
}

pub fn parse_value_expr<'a>(
    input: &mut TokenStream<'a>,
) -> Result<Expression, ParseError<'a, Infallible>> {
    let tok = input.peek().ok_or(ParseError::UnexpectedEndOfFile)?.clone();

    match tok.kind() {
        TokenKind::Literal(_) => Ok(Expression::Constant(parse_value(input)?)),
        TokenKind::Identifier(ident) => {
            _ = input.next();

            match input.peek() {
                Some(Token {
                    kind: TokenKind::Basic(BasicToken::LParen),
                    span: _,
                }) => {
                    _ = input.next();

                    let mut args: Vec<Expression> = vec![];

                    loop {
                        if input.peek().ok_or(ParseError::UnexpectedEndOfFile)?.kind()
                            == &TokenKind::Basic(BasicToken::RParen)
                        {
                            break;
                        }

                        args.push(parse_expr(input)?);

                        let tok2 = input.next().ok_or(ParseError::UnexpectedEndOfFile)?;

                        match tok2.kind() {
                            TokenKind::Basic(BasicToken::Comma) => continue,
                            TokenKind::Basic(BasicToken::RParen) => break,
                            _ => return Err(ParseError::UnexpectedToken(tok2))
                        }
                    };

                   //  _ = input.next();

                    Ok(Expression::FunctionCall { name: ident.to_string(), args })
                }
                _ => Ok(Expression::Variable {
                    name: ident.to_string(),
                    span: tok.span(),
                }),
            }
        }
        TokenKind::Basic(BasicToken::LParen) => {
            _ = input.next();
            let res = parse_expr(input)?;

            _ = input.next();

            Ok(res)
        }
        _ => {
            if tok.get_type() == TokenType::Operator {
                parse_unary(input)
            } else {
                Err(ParseError::UnexpectedToken(tok.clone()))
            }
        }
    }
}

pub fn parse_expr<'a>(
    input: &mut TokenStream<'a>,
) -> Result<Expression, ParseError<'a, Infallible>> {
    let expr = parse_value_expr(input)?;

    let tok = match input.peek() {
        Some(t) => t,
        None => return Ok(expr),
    };

    let op = match tok.get_type() {
        TokenType::Operator | TokenType::Comparison => parse_operator(input)?,
        _ => return Ok(expr),
    };

    let right_expr = parse_value_expr(input)?;

    Ok(Expression::Binary {
        op,
        left: expr.into(),
        right: right_expr.into(),
    })
}
