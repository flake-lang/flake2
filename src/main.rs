use std::fmt::Debug;

use ast::{ParseError, AST};
use lexer::stream::TokenStream;


pub fn main(){
    let input = include_str!("../test.fl");
    let mut toks = TokenStream::new(input);
    
    let ast = match AST::parse(&mut toks){
        Ok(v) => v,
        Err(e) => {
            display_error(e);
            return;
        },
    };

    dbg!(ast);
}


pub fn display_error<E: Debug>(e: ParseError<E>) {
    match e{
        ParseError::UnexpectedToken(t) => {

            eprintln!(
            include_str!("../generic.stderr"),
            error = format!("unexpected token {:?}", t.kind()),
            ln = t.span().line,
            cn = t.span().col,
            line = "",
            message = "failed to parse ast."
        )},
        ParseError::UnexpectedEndOfFile => eprintln!(
            include_str!("../generic.stderr"),
            error = format!("unexpected token end of file"),
            ln = usize::MAX,
            cn = 0,
            line = "",
            message = "failed to parse ast."
        ),
        ParseError::Custom(_) => todo!(),
    }
}