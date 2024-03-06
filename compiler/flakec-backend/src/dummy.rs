use std::{convert::Infallible, marker::PhantomData};

use crate::Backend;

pub struct DummyBackend<'a>{
    _phantom: PhantomData<&'a ()>
}

impl<'a> Backend<'a> for DummyBackend<'a> {

    fn compile(&'a mut self, _: flakec_middle::ast::AST) -> Result<(), Box<(dyn std::error::Error + 'static)>> {
        Ok(())
    }

    fn name(&self) -> &'static str {
        "flakec-dummy"
    }

    fn version(&self) -> &'a str {
        "1.0"
    }
}