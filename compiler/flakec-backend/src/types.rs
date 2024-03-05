use std::marker::PhantomData;



#[derive(Debug, Clone)]
pub struct PtrValue<'a>{
   pub _addr: usize,
   _phantom: PhantomData<&'a u8>
}

