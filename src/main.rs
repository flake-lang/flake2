use std::{error::Error, fmt::Debug};

use ast::{item::Function, FromTokens, ParseError, AST};
use lexer::stream::TokenStream;
use types::TypePass;
use flakec_backend::Backend;

pub fn main(){
    // for testing
    let input = include_str!("../test.fl");

    // token stream
    let mut toks = TokenStream::new(input);
    
// dbg!(Function::from_tokens(&mut toks));

    // raw ast
    let mut ast = match AST::parse(&mut toks){
        Ok(v) => v,
        Err(e) => {
            display_error(e);
            return;
        },
    };

    // types
    if let Err(type_err) = ast.run_pass(TypePass::new()){
        eprint!(
            include_str!("../type.stderr"),
            error = type_err.to_string()
        );

        return;
    }

    // for testing
    dbg!(&ast);

    let mut backend = llvm_backend::LLVMBackend::new();

    dbg!(backend.compile(ast));
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