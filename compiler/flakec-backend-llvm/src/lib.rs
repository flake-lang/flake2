pub mod codegen;

use std::{convert::Infallible, marker::PhantomData};

use flakec_backend::Backend;
use inkwell::{builder::Builder, module::Module};

pub enum LLVMError{
    Builder(inkwell::builder::BuilderError),
    UnsupportedFeature(&'static str),
}

pub struct LLVMState<'ctx>{
    pub context: &'ctx mut inkwell::context::Context,
    pub builder: &'ctx mut Builder<'ctx>
}

pub struct LLVMBackend<'a>{
    state: LLVMState<'a>
}

impl<'a> Backend<'a> for LLVMBackend<'a> {

    fn compile(&'a mut self, _: flakec_middle::ast::AST) -> Result<(), Box<(dyn std::error::Error + 'static)>> {
        Ok(())
    }

    fn name(&self) -> &'static str {
        "llvm"
    }

    fn version(&self) -> &'a str {
        "0.0"
    }
}