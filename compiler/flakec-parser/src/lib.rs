use lexer::stream::TokenStream;


#[derive(Debug)]
#[non_exhaustive]
pub enum ParserState {
    Normal,
}

pub struct Parser<'a>{
    state: ParserState,
    input: &'a mut TokenStream<'a>,
}

impl<'a> Parser<'a>{
    pub fn new(input: &'a mut TokenStream<'a>) -> Self{
        Self{
            state: ParserState::Normal,
            input
        }
    }

}