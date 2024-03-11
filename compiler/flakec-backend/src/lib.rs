use std::error::Error as StdError;

pub mod dummy;
pub mod types;

/// A Flake Compiler [Backend].
pub trait Backend<'a>: 'a {
    /// Compiles an [AST].
    fn compile(&'a mut self, _: flakec_middle::ast::AST) -> Result<(), Box<dyn StdError>>;


    #[cold]
    fn name(&'a self) -> &'a str;
    #[cold]
    fn version(&'a self) -> &'a str;

    fn supports_comptime(&'a self) -> bool {
        false
    }
    
    fn is_compatible_with<T: Backend<'a>>(&'a self, _: T) -> bool
    where
        Self: Sized,
    {
        false
    }
}
