//! Literal


#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum Literal<'input>{
    String(&'input str),
    OwnedString(String),
    Number(&'input str),
    SignedNumber(&'input str),
    Boolean(bool)
} 

impl<'a> Literal<'a>{
    pub fn string(&self) -> Option<String>{
        match self{
            Self::String(s) => Some((*s).to_owned()),
            Self::OwnedString(s) => Some(s.clone()),
            _ => None,

        }
    }
}