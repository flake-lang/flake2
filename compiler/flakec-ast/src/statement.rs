use std::convert::Infallible;

use lexer::{
    keyword,
    token::{BasicToken, Token, TokenKind, TokenType},
};

use crate::{
    expect_token,
    expression::{parse_expr, Expression},
    parse, FromTokens, ParseError,
};

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Let(Let),
    Return(Return),
    Assignment(Assignment<'a>),
}

/// Syntax:
/// - return;
/// - return <[expr]>;
#[derive(Debug, Clone)]
pub struct Return {
    pub value: Option<Expression>,
}

/// Syntax:
/// - let <[name]>;
/// - let[<[type]>] <name>; // <--- TODO
/// - let[<[type]?>] <name> = <expr>;
#[derive(Debug, Clone)]
pub struct Let {
    pub var_name: String,
    pub ty_name: Option<String>,
    pub initale_value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct Assignment<'a> {
    pub var: String,
    pub op: Token<'a>,
    pub value: Expression,
}

impl<'a> FromTokens<'a> for Let {
    type Error = Infallible;

    fn from_tokens(
        input: &mut lexer::stream::TokenStream<'a>,
    ) -> Result<Self, crate::ParseError<'a, Self::Error>> {
        _ = expect_token(input, TokenKind::Keyword(keyword::Keyword::Let))?;

        let tok = input.next().ok_or(ParseError::UnexpectedEndOfFile)?;

        let var_name = match tok.kind() {
            TokenKind::Identifier(ident) => *ident,
            _ => return Err(ParseError::UnexpectedToken(tok))
        };

        if input.peek().ok_or(ParseError::UnexpectedEndOfFile)?.kind()
            == &TokenKind::Basic(BasicToken::Semicolon)
        {
            _ = input.next();
            Ok(Self { var_name: var_name.to_owned(), initale_value: None, ty_name: None })
        } else {
            _ = expect_token(input, TokenKind::Basic(BasicToken::Eq))?;

            let val = parse_expr(input)?;

            _ = expect_token(input, TokenKind::Basic(BasicToken::Semicolon))?;

            Ok(Self { var_name: var_name.to_owned(), initale_value: Some(val), ty_name: None })
        }
    }
}

impl<'a> FromTokens<'a> for Return {
    type Error = Infallible;

    fn from_tokens(
        _input: &mut lexer::stream::TokenStream<'a>,
    ) -> Result<Self, crate::ParseError<'a, Self::Error>> {
        _ = expect_token(_input, TokenKind::Keyword(keyword::Keyword::Return))?;

        if _input.peek().ok_or(ParseError::UnexpectedEndOfFile)?.kind()
            == &TokenKind::Basic(BasicToken::Semicolon)
        {
            _ = _input.next();
            Ok(Self { value: None })
        } else {
            let val = parse_expr(_input)?;

            _ = expect_token(_input, TokenKind::Basic(BasicToken::Semicolon))?;

            Ok(Self { value: Some(val) })
        }
    }
}

impl<'a> FromTokens<'a> for Assignment<'a> {
    type Error = Infallible;

    fn from_tokens(
        input: &mut lexer::stream::TokenStream<'a>,
    ) -> Result<Self, crate::ParseError<'a, Self::Error>> {
        let mut tok = input.next().ok_or(ParseError::UnexpectedEndOfFile)?;

        let var = match tok.kind() {
            TokenKind::Identifier(ident) => ident.to_string(),
            _ => return Err(ParseError::UnexpectedToken(tok))
        };

        tok = input.next().ok_or(ParseError::UnexpectedEndOfFile)?;

        let op = match tok.get_type() {
            TokenType::Assignment => tok,
            _ => return Err(ParseError::UnexpectedToken(tok))
        };

        let value = parse_expr(input)?;

        _ = expect_token(input, TokenKind::Basic(BasicToken::Semicolon))?;
        
        Ok(Self { var, op, value })
    }
}

impl<'a> FromTokens<'a> for Statement<'a>{
    type Error = Infallible;

    fn from_tokens(
        _input: &mut lexer::stream::TokenStream<'a>,
    ) -> Result<Self, crate::ParseError<'a, Self::Error>> {
        match _input.peek().ok_or(ParseError::UnexpectedEndOfFile)?.kind() {
            TokenKind::Keyword(keyword::Keyword::Return) => Ok(Self::Return(parse(_input)?)),
            TokenKind::Keyword(keyword::Keyword::Let) => Ok(Self::Let(parse(_input)?)),
            TokenKind::Identifier(_) => Ok(Self::Assignment(parse(_input)?)),
            _ => todo!()
        }
    }
}
