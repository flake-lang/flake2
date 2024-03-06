use ast::statement::Statement;
use lexer::stream::TokenStream;

pub fn main(){
    let mut toks = TokenStream::new(include_str!("../test.fl"));
    
    loop {
        println!("{:#?}", ast::parse::<Statement>(&mut toks).unwrap());
    }

}
