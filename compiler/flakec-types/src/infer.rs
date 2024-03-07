use std::{collections::HashMap, ops::Deref};

use flakec_middle::ast::{expression::Expression, operator::Operator_, pass::Pass, types::{Type, INT_UNKNOWN, UINT_UNKNOWN}, value::Value};

#[derive(Debug, Default)]
pub struct TypeManager {
    variables: HashMap<String, Type>,
    functions: HashMap<String, (Type, Vec<Option<Type>>)>
}


#[derive(Debug, Clone)]
pub enum InferType {
    Ast(Type),
    Cast(Type, Box<InferType>),
    Operation(Operator_, Box<InferType>, Box<InferType>) 
}

impl TypeManager {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn infer_expr(&self, expr: Expression) -> Option<Type> {
        match expr {
            Expression::Constant(v) => Some(self.infer_const(v)?),
            Expression::Unary { child, .. } => self.infer_expr(*child),
            Expression::Variable { name, ..} => self.variables.get(name.as_str()).cloned(),
            Expression::FunctionCall { name, args } => Some(self.functions.get(&name)?.clone().0),
            _ => None
        }
    }

    pub fn infer_const(&self, val: Value) -> Option<Type> {
        match val.deref() {
            flakec_middle::ast::value::Value_::Str(str) => Some(Type::Array {
                elem_ty: Type::Char.into(),
                len: str.len() as u64,
            }),
            flakec_middle::ast::value::Value_::Number { is_negative: false, .. } => Some(UINT_UNKNOWN),
            flakec_middle::ast::value::Value_::Number { is_negative: true, .. } => Some(INT_UNKNOWN),
            flakec_middle::ast::value::Value_::Boolean(_) => Some(Type::Bool),
            _ => None
        }
    }
}

