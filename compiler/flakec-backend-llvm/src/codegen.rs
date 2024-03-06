//! Code Generation

use inkwell::values::AnyValueEnum;

use crate::LLVMState;

pub trait Codegen<'ctx>{
    fn build(state: &'ctx LLVMState<'ctx>) -> AnyValueEnum;
}