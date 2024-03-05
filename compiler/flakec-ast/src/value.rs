use lexer::stream::TokenStream;

#[derive(Debug, Clone)]
pub enum Value{
    Str(String),
    Number{
        is_negative: bool,
        value: String
    },
    Boolean(bool)
}

pub fn parse_value<'a>(input: &mut TokenStream<'a>) -> Option<Value>{


    match dbg!(input.next())?.literal()?{
        lexer::literal::Literal::String(s) => Some(Value::Str((*s).to_owned())),
        lexer::literal::Literal::OwnedString(s) => Some(Value::Str(s.clone())),
        lexer::literal::Literal::Number(n) => Some(Value::Number { is_negative: false, value: (*n).to_owned()}),
        lexer::literal::Literal::SignedNumber(n) => Some(Value::Number { is_negative: true, value: (*n).to_owned()}),
        lexer::literal::Literal::Boolean(b) => Some(Value::Boolean(*b)),
        _ => None
    }
}