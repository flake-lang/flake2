use lexer::stream::TokenStream;



pub fn main(){
    let toks = TokenStream::new(include_str!("../test.fl"));
    
    for tok in toks{
        eprintln!("{:?}", tok);
    }
}
