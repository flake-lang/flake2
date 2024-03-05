//! Literal

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal<'input>{
    String(&'input str),
    OwnedString(String),
    Number(&'input str),
    SignedNumber(&'input str),
    Boolean(bool)
} 

