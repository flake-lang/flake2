use std::{error::Error, fmt::Debug};

use ast::{ParseError, AST};
use lexer::stream::TokenStream;
use types::TypePass;


pub fn main(){
    let input = include_str!("../test.fl");
    let mut toks = TokenStream::new(input);
    
    let mut ast = match AST::parse(&mut toks){
        Ok(v) => v,
        Err(e) => {
            display_error(e);
            return;
        },
    };

    if let Err(type_err) = ast.run_pass(TypePass::new()){
        eprintln!(
            include_str!("../type.stderr"),
            error = type_err.to_string()
        );

        return;
    }

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