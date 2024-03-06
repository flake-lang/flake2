//! [Type]

#[derive(Debug, Clone)]
pub enum Type {
    Int { bits: u8 },
    UInt { bits: u8 },
    Array { elem_ty: Box<Type>, len: u64 },
    Pointer { target_ty: Box<Type> },
    
    Custom(String)
}
