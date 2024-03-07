
use crate::AST;

#[derive(Eq, PartialEq)]
pub enum PassKind{
    Standard,
    Check
}

pub trait Pass<'a> {
    type Error;

    fn kind() -> PassKind;

    fn run(&mut self, _ast: &mut AST<'a>) -> Result<(), Self::Error>{
        unimplemented!()
    }

    fn check(&mut self, _ast: &AST<'a>) -> Result<(), Self::Error>{
        unimplemented!()
    }
}

