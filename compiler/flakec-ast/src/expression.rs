use std::{ops::IndexMut, path::MAIN_SEPARATOR};

use lexer::{stream::TokenStream, token::{Token, TokenType}};

use crate::{operator::{parse_operator, Operator}, value::{self, parse_value, Value}};

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Expression{
    // "Hello!"
    Constant(Value),
    // !true
    Unary{
        op: Operator,
        child: Box<Expression>
    },
    // 1 + 1
    Binary {
        op: Operator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    // a
    Variable{
        name: String
    },
    // test(1, "abc")
    FunctionCall{
        name: String,
        args: Vec<Expression>
    }
}

