use lexer::stream::TokenStream;

pub fn main(){
    let mut toks = TokenStream::new(include_str!("../test.fl"));
    
    dbg!(ast::expression::parse_expr(&mut toks, false));

}
