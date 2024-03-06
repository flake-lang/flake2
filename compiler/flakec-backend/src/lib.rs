use std::{backtrace, convert::Infallible, error::Error as StdError, marker::PhantomData, ops::Deref};

pub mod types;
pub mod dummy;



pub trait Backend<'a>: 'a{
    fn compile(&'a mut self, _: flakec_middle::ast::AST) -> Result<(), Box<dyn StdError>>;

    #[cold]
    fn name(&'a self) -> &'static str;
    #[cold]
    fn version(&'a self) -> &'a str;
}

pub fn test<'a>(backend: &'a dyn Backend<'a>){
    
}