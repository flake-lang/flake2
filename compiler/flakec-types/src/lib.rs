#[macro_use]
extern crate thiserror;

use std::{collections::HashMap, error, fmt::Display, marker::PhantomData, ops::Deref};

use flakec_middle::{
    ast::{
        expression::Expression,
        item::{ExplicitRetType, Function},
        operator::Operator_,
        pass::Pass,
        statement::Statement,
        types::{self, Type, INT_UNKNOWN, UINT_UNKNOWN},
        value::Value,
        Block,
    },
    lexer::token::BasicToken,
};

#[derive(Debug, Default)]
pub struct TypeManager {
    variables: HashMap<String, Type>,
    functions: HashMap<String, (Type, Vec<Type>)>,
}

#[derive(Debug, Clone)]
pub enum InferType {
    Ast(Type),
    Cast(Type, Box<InferType>),
    Operation(Operator_, Box<InferType>, Box<InferType>),
}

impl TypeManager {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn infer_expr(&self, expr: Expression) -> Option<Type> {
        match expr {
            Expression::Constant(v) => Some(self.infer_const(v)?),
            Expression::Unary { child, .. } => self.infer_expr(*child),
            Expression::Variable { name, .. } => self.variables.get(name.as_str()).cloned(),
            Expression::FunctionCall { name, .. } => Some(self.functions.get(&name)?.clone().0),
            Expression::Cast { into, .. } => Some(into),
            Expression::Binary { left, right, .. } => {
                let ty = self.infer_expr(*left)?;

                if ty == self.infer_expr(*right)? {
                    Some(ty)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn infer_const(&self, val: Value) -> Option<Type> {
        match val.deref() {
            flakec_middle::ast::value::Value_::Str(str) => Some(Type::Array {
                elem_ty: Type::Char.into(),
                len: str.len() as u64,
            }),
            flakec_middle::ast::value::Value_::Number {
                is_negative: false, ..
            } => Some(UINT_UNKNOWN),
            flakec_middle::ast::value::Value_::Number {
                is_negative: true, ..
            } => Some(INT_UNKNOWN),
            flakec_middle::ast::value::Value_::Boolean(_) => Some(Type::Bool),
            _ => None,
        }
    }

    pub fn typeof_var(&self, name: String) -> Option<&Type> {
        self.variables.get(&name)
    }

    pub fn register_var(&mut self, name: String, ty: Type) {
        self.variables.insert(name, ty);
    }

    pub fn register_func(&mut self, name: String, args: Vec<Type>, ret_ty: Type) {
        self.functions.insert(name, (ret_ty, args));
    }

    pub fn typeof_func(&self, name: &String) -> Option<&(Type, Vec<Type>)> {
        self.functions.get(name)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Error)]
#[non_exhaustive]
pub enum TypeError {
    #[error("mismatched types `{0}` and `{1}`")]
    TypeMismatch(Type, Type),
    #[error("argument types don't match. expected {1} found {2}.\n = in function call of {0}")]
    MismatchArgumentType(String, Type, Type),
    #[error("the uninitialized variable `{0}` needs a type")]
    UndefinedType(String),
    #[error("expected {0} argument found {1}")]
    MismatchedArgumentCount(usize, usize),
    #[error("expect return type of `{0}` found `{1}`")]
    UnexpectedReturnType(Type, Type),
    #[error("`{0}` isn't valid in this context")]
    InvalidInCurrentContext(&'static str),
    #[error("use of undeclared variable `{0}`")]
    UndeclaredVariable(String),
    #[error("use of undeclared function `{0}`.")]
    UndeclaredFunction(String),
    #[error("use of uninitialised variable `{0}`")]
    UninitialisedVariableUsed(String),
    #[error("casting a `{0}` to a `{1}` is invalid")]
    InvalidCast(Type, Type),
    #[error("casting a `{0}` to a `{1}` is disallowed")]
    DisallowedCast(Type, Type),

    // Internal
    #[error("internal: {0}(...) returned a none value")]
    InternalNoneReturned(&'static str),
}

/// Type Inferring, Checking and more.
pub struct TypePass<'a> {
    _manager: TypeManager,
    _phatom: PhantomData<&'a ()>,
    _expected_ret_ty: Option<Type>,
}

impl<'a> TypePass<'a> {
    pub fn new() -> Self {
        Self {
            _manager: TypeManager::new(),
            _phatom: PhantomData,
            _expected_ret_ty: None,
        }
    }

    pub fn check_fn_args(&mut self, name: &String, args: &Vec<Expression>) -> Result<(), TypeError> {
        let (_, expected_args) = self._manager.typeof_func(name).ok_or(TypeError::UndeclaredFunction(name.to_string()))?.clone();
        
        if args.len() != expected_args.len() {
            return Err(TypeError::MismatchedArgumentCount(expected_args.len(), args.len()));
        }

        for (provided, expected) in args.iter().zip(expected_args.iter()) {
            self.check_expr(provided)?;

            let provided_ty = self._manager.infer_expr(provided.clone()).expect("internal error: failed to infer expression");

            if provided_ty.compare(expected)
            {
                return Err(TypeError::MismatchArgumentType(name.clone(), expected.clone(),provided_ty));
            }
        }

        Ok(())
    }

    /// Checks if a [Expression::Cast] is valid, disallowed or invalid.
    ///
    // Casting Checks (Partially Done)
    ///
    /// ### Valid
    ///
    /// - `*T -> *U`
    /// - `T[N] -> *T`
    /// - `*T -> usize`
    /// - `u8 <--> char`
    ///
    /// ### Unsafe?(Unimplemented)
    /// - `*T -> T[N]`
    /// - `usize -> *T`
    ///
    /// ### Invalid
    /// - `T(!Pointer) -> U(!Pointer)`
    /// - `*T -> U(!usize)`
    /// - `T(!Initialized) -> T(Initialized)`
    pub fn check_cast(&mut self, target: &Type, expr: &Expression) -> Result<(), TypeError> {
        self.check_expr(expr)?;

        let ty = self._manager.infer_expr(expr.clone()).unwrap(); // <-- FIXME: Return an internal error instead.

        if &ty == target {
            return Ok(());
        }

        // note: If we make [TypeError] have have a associated lifetime that last longer than `'a`,
        // we might be able to avoid copying?
        match (&ty, target) {
            (Type::Pointer { .. }, Type::Pointer { .. }) => Ok(()),
            (Type::Int { .. }, Type::Int { .. }) => Ok(()),
            (Type::UInt { .. }, Type::Int { .. }) => Ok(()),
            (Type::Int { .. }, Type::UInt { .. }) => Ok(()),
            (Type::UInt { .. }, Type::UInt { .. }) => Ok(()),
            (Type::Pointer { .. }, Type::UInt { bits: 64 }) => Ok(()),
            (Type::UInt { bits: 64 }, Type::Pointer { .. }) => Ok(()),
            (Type::Array { elem_ty, .. }, Type::Pointer { target_ty: ptr_ty }) => {
                if elem_ty != ptr_ty {
                    Err(TypeError::InvalidCast(ty, target.clone()))
                } else {
                    Ok(())
                }
            }
            (Type::Pointer { .. }, Type::Array { .. }) => {
                Err(TypeError::DisallowedCast(ty, target.clone()))
            }
            /*(Type::UInt { bits: 64 }, Type::Pointer { .. }) => {
                Err(TypeError::DisallowedCast(ty, target.clone()))
            }*/
            _ => Err(TypeError::InvalidCast(ty, target.clone())),
        }
    }

    pub fn check_expr(&mut self, expr: &Expression) -> Result<(), TypeError> {
        match expr {
            Expression::Constant(_) => Ok(()),
            Expression::Unary { child, .. } => self.check_expr(&child),
            Expression::Binary { left, right, .. } => {
                self.check_expr(left.as_ref())?;
                self.check_expr(right.as_ref())?;

                Ok(())
            }
            Expression::Variable { name, .. } => match self._manager.typeof_var(name.to_string()) {
                Some(Type::_Uninitialized(_)) => {
                    Err(TypeError::UninitialisedVariableUsed(name.clone()))
                }
                None => Err(TypeError::UndeclaredVariable(name.clone())),
                _ => Ok(()),
            },
            Expression::FunctionCall { name, args } => self.check_fn_args(name, args),
            Expression::Cast {
                child,
                into: target,
            } => self.check_cast(target, &child),
            _ => todo!(),
        }
    }

    pub fn check_stmt(&mut self, stmt: &Statement<'a>) -> Result<(), TypeError> {
        for expr in stmt.exprs() {
            self.check_expr(expr)?
        }

        match stmt {
            Statement::Let(v) => {
                if v.initale_value.is_none() && v.ty.is_none() {
                    return Err(TypeError::UndefinedType(v.var_name.clone()));
                };

                if let Some(expr) = &v.initale_value {
                    _ = self.check_expr(expr)?;
                }

                let var_ty = match &v.ty {
                    Some(t) => t.clone(),
                    None => self
                        ._manager
                        .infer_expr(v.initale_value.as_ref().unwrap().clone())
                        .ok_or(TypeError::InternalNoneReturned("infer_expr_type"))?,
                };

                self._manager.register_var(v.var_name.clone(), var_ty)
            }
            Statement::Return(ret) => {
                if let Some(expr) = &ret.value {
                    _ = self.check_expr(expr)?;
                }

                if self._expected_ret_ty.is_none() {
                    return Err(TypeError::InvalidInCurrentContext("return"));
                };
                
                if ret.value.is_none(){
                    return Ok(());
                }

               /*  if self._manager.infer_expr(ret.value.as_ref().unwrap().clone()) != self._expected_ret_ty {
                    return Err(TypeError::InternalNoneReturned("_chk_ret_type"));
                }  */
            }
            Statement::Assignment(v) => {
                let var_ty = match self._manager.typeof_var(v.var.clone()) {
                    Some(t) => t,
                    None => return Err(TypeError::UndeclaredVariable(v.var.clone())),
                };

                let val_ty = self
                    ._manager
                    .infer_expr(v.value.clone())
                    .ok_or(TypeError::InternalNoneReturned("infer_expr_type"))?;

                if &val_ty != var_ty {
                    return Err(TypeError::TypeMismatch(val_ty, var_ty.clone()));
                }
            }
        };

        Ok(())
    }

    pub fn handle_function(&mut self, func: &Function<'a>) -> Result<(), TypeError> {
        let args = func
            .args
            .inner
            .0
            .iter()
            .map(|a| ((*a).ty.clone(), a.name.0.clone()))
            .collect::<Vec<_>>();
        let arg_types = args.iter().map(|a| a.0.clone()).collect::<Vec<_>>();

        let ret_ty = match func.ret.clone() {
            Some(ExplicitRetType { ty, .. }) => ty,
            _ => Type::Void,
        };

        self._manager
            .register_func(func.name.0.clone(), arg_types, ret_ty.clone());

        let globals = self._manager.variables.clone();

        for (ty, name) in args {
            self._manager.register_var(name, ty)
        }

        self._expected_ret_ty = Some(ret_ty);

        if let Some(Block(stmts)) = &func.body {
            for stmt in stmts {
                _ = self.check_stmt(&stmt)?;
            }
        }

        self._manager.variables = globals;

        Ok(())
    }
}

impl<'a> Pass<'a> for TypePass<'a> {
    type Error = TypeError;

    fn kind() -> flakec_middle::ast::pass::PassKind {
        flakec_middle::ast::pass::PassKind::Standard
    }

    fn run(&mut self, _ast: &mut flakec_middle::ast::AST<'a>) -> Result<(), Self::Error> {
        for item in _ast._nodes.iter() {
            match item {
                flakec_middle::ast::item::Item::Function(func) => self.handle_function(func)?,
                _ => {}
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use flakec_middle::lexer::stream::TokenStream;

    use crate::TypeManager;

    #[test]
    pub fn test() {
        let mgr = TypeManager::new();
        let mut inp = TokenStream::new("1");

        panic!(
            "{:?}",
            mgr.infer_expr(flakec_middle::ast::expression::parse_expr(&mut inp).unwrap())
        );
    }
}
