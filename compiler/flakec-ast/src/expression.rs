use std::{
    convert::Infallible,
    ops::{Deref, IndexMut},
    path::MAIN_SEPARATOR,
};

use lexer::{
    span::Span,
    stream::TokenStream,
    token::{self, BasicToken, Token, TokenKind, TokenType},
};

use crate::{
    ast_struct, expect_token, operator::{parse_operator, Operator, Operator_}, parse, types::Type, value::{self, parse_value, Value, Value_}, ParseError
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
    Cast {
        into: Type,
        child: Box<Expression>
    },
}

ast_struct!{
    pub struct CastSource{
        reva_token: crate::token::RevArrow,
        ty: Type
    };
}


pub fn parse_unary<'a>(
    input: &mut TokenStream<'a>,
) -> Result<Expression, ParseError<'a, Infallible>> {
    let op = parse_operator(input)?;
    let child = parse_expr(input)?;


    Ok(Expression::Unary {
        op: op.clone(),
        child: child.into(),
        span: op.span
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
        },
        // Type Cast
        TokenKind::Basic(BasicToken::LBracket) => { // [<target>]<expr>
            _ = input.next();

            let ty = parse::<Type>(input)?;

            _ = expect_token(input, TokenKind::Basic(BasicToken::RBracket))?;

            let res = parse_expr(input)?;

            Ok(Expression::Cast { into: ty, child: res.into() })
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

/// Parses an [Expression].
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
