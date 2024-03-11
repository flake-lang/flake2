//! Code Generation

use inkwell::values::AnyValueEnum;

use crate::LLVMState;

pub trait LLVMBuilder<'ctx>{
    type Output;

    fn build(&mut self, state: &'ctx LLVMState) -> AnyValueEnum;
}