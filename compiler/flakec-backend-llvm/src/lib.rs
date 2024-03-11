pub mod codegen;

use std::{
    collections::HashMap,
    convert::Infallible,
    ffi::{CStr, CString},
    marker::PhantomData,
    ops::Deref,
    ptr::null_mut,
};

use flakec_backend::Backend;
use flakec_middle::ast::{
    expression::Expression,
    item::{ExplicitRetType, FnArg, Function, Item},
    statement::{Return, Statement},
    types::Type,
    value::Value,
    AST,
};
use inkwell::{
    builder::Builder,
    context::{AsContextRef, Context},
    llvm_sys::{
        core::{
            LLVMArrayType, LLVMBuildInBoundsGEP2, LLVMBuildIntCast, LLVMBuildIntCast2, LLVMBuildIntToPtr, LLVMBuildNot, LLVMBuildPointerCast, LLVMBuildPtrToInt, LLVMBuildRet, LLVMBuildRetVoid, LLVMBuildStructGEP2, LLVMConstInt, LLVMConstIntOfString, LLVMConstString, LLVMConstStringInContext, LLVMFunctionType, LLVMGetTypeByName, LLVMGetTypeByName2, LLVMGetTypeKind, LLVMInt16Type, LLVMInt16TypeInContext, LLVMInt32Type, LLVMInt32TypeInContext, LLVMInt64Type, LLVMInt64TypeInContext, LLVMInt8Type, LLVMInt8TypeInContext, LLVMPointerType, LLVMTypeOf, LLVMVoidType, LLVMVoidTypeInContext
        },
        prelude::{LLVMBool, LLVMContextRef, LLVMTypeRef},
        LLVMContext, LLVMType, LLVMTypeKind, LLVMValue,
    },
    module::Module,
    types::{
        AnyType, AnyTypeEnum, AsTypeRef, BasicType as _, BasicTypeEnum, FunctionType, IntType,
    },
    values::{ArrayValue, AsValueRef, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};

#[derive(Debug)]
pub enum LLVMError {
    Builder(inkwell::builder::BuilderError),
    UnsupportedFeature(&'static str),
}

pub struct LLVMState {
    pub context: inkwell::context::Context,
}

pub struct LLVMBackend<'a> {
    state: LLVMState,
    _phantom: PhantomData<&'a ()>,
}

impl<'a> LLVMBackend<'a> {
    pub fn new() -> Self {
        Self {
            state: LLVMState {
                context: Context::create(),
            },
            _phantom: PhantomData,
        }
    }
}

impl<'a> Backend<'a> for LLVMBackend<'a> {
    fn compile(
        &'a mut self,
        input: flakec_middle::ast::AST,
    ) -> Result<(), Box<(dyn std::error::Error + 'static)>> {
        let mut ctx = Context::create();
        let mut mod_builder = ModuleBuilder::new(&ctx, "main");

        for item in input._nodes {
            match item {
                Item::Function(func) => mod_builder.comnpile_function(func).unwrap(),
                _ => todo!(),
            }
        }

        mod_builder.llvm_rep.print_to_stderr();

        Ok(())
    }

    fn name(&self) -> &'static str {
        "llvm"
    }

    fn version(&self) -> &'a str {
        "0.0"
    }
}

#[derive(Debug)]
struct ModuleBuilder<'a> {
    context: &'a Context,
    name: &'a str,
    llvm_rep: Module<'a>,
    builder: Builder<'a>,
    exp_ret: LLVMTypeRef,
    locals: HashMap<String, PointerValue<'a>>
}

impl<'a> ModuleBuilder<'a> {
    pub fn new(context: &'a Context, name: &'a str) -> Self {
        Self {
            context,
            llvm_rep: context.create_module(name),
            name,
            builder: context.create_builder(),
            exp_ret: unsafe { LLVMInt32TypeInContext(context.as_ctx_ref()) },
            locals: HashMap::new()
        }
    }

    pub fn comnpile_function(& mut self, func: Function<'a>) -> Result<(), LLVMError> {
        let name = func.name.0.clone();
        let mut llvm_func =
            unsafe { LLVMFunction::from_ast(self.context.as_ctx_ref(), func.clone()) };

        let func_val = self.llvm_rep.add_function(
            name.as_str(),
            unsafe { FunctionType::new(llvm_func.as_type()) },
            None,
        );

        let entry_block = self.context.append_basic_block(func_val, "entry");

        self.builder.position_at_end(entry_block);

        if let Some(v) = func_val.get_type().get_return_type() {
            self.exp_ret = v.as_type_ref();
        }

        for (arg, param) in (func.args.0).iter().zip(func_val.get_params().iter()) {
            let aptr = self.builder.build_alloca(
                unsafe { BasicTypeEnum::new(
                    get_llvm_ty(self.context.as_ctx_ref(), arg.clone().ty)
                ) },
                "__arg"
            ).map_err(|e|LLVMError::Builder(e))?;

            self.builder.build_store(aptr, *param)
                .map_err(|e|LLVMError::Builder(e))?;

            self.locals.insert(arg.name.0.clone(), aptr);
        }

        if let Some(_body) = func.body {
            for stmt in _body.0 {
                self.compile_stmt(stmt)?;
            }
        }

        func_val.verify(true);

        self.locals.clear();

        Ok(())
    }

    pub fn compile_stmt(&mut self, stmt: Statement<'a>) -> Result<(), LLVMError> {
        match stmt {
            Statement::Let(_) => todo!(),
            Statement::Return(Return { value: None }) => unsafe {
                LLVMBuildRetVoid(self.builder.as_mut_ptr());
            },
            Statement::Return(Return { value: Some(val) }) => unsafe {
                LLVMBuildRet(self.builder.as_mut_ptr(), self.compile_expr(val)?);
            },
            Statement::Assignment(_) => todo!(),
        }

        Ok(())
    }

    pub fn compile_expr(&mut self, expr: Expression) -> Result<*mut LLVMValue, LLVMError> {
        match expr {
            Expression::Constant(v) => unsafe {
                Ok(llvm_const(self.context.as_ctx_ref(), v, self.exp_ret))
            },
            Expression::Unary { op, child, span } => todo!(),
            Expression::Binary { op, left, right } => todo!(),
            Expression::Variable { name, span } => {
                let vptr = *self.locals.get(name.as_str()).unwrap();

                match self.builder.build_load(vptr, name.as_str()) {
                    Ok(v) => Ok(v.as_value_ref()),
                    Err(e) => Err(LLVMError::Builder(e))
                }
            },
            Expression::FunctionCall { name, args } => todo!(),
            Expression::Cast { into, child } => unsafe {
                self.compile_cast(into, *child)
            }
            _ => todo!(),
        }
    }

    pub unsafe fn compile_cast(
        &mut self,
        target: Type,
        val_expr: Expression,
    ) -> Result<*mut LLVMValue, LLVMError> {
        let val = self.compile_expr(val_expr)?;
        let val_ty = LLVMGetTypeKind(LLVMTypeOf(val));
        let name = CString::new("").unwrap();

        match (&target, val_ty) {
            (Type::Pointer { target_ty: _t }, LLVMTypeKind::LLVMPointerTypeKind) => {
                Ok(LLVMBuildPointerCast(
                    self.builder.as_mut_ptr(),
                    val,
                    get_llvm_ty(self.context.as_ctx_ref(), target),
                    name.as_ptr(),
                ))
            }
            (
                Type::UInt { .. } | Type::Char | Type::Bool,
                LLVMTypeKind::LLVMIntegerTypeKind,
            ) => Ok(LLVMBuildIntCast2(
                self.builder.as_mut_ptr(),
                val,
                get_llvm_ty(self.context.as_ctx_ref(), target),
                0,
                name.as_ptr(),
            )),
            (
                Type::Int { .. },
                LLVMTypeKind::LLVMIntegerTypeKind,
            ) => Ok(LLVMBuildIntCast2(
                self.builder.as_mut_ptr(),
                val,
                get_llvm_ty(self.context.as_ctx_ref(), target),
                1,
                name.as_ptr(),
            )),
            (
                Type::Pointer { target_ty },
                LLVMTypeKind::LLVMIntegerTypeKind
            ) => Ok(LLVMBuildIntToPtr(
                self.builder.as_mut_ptr(),
                val,
                get_llvm_ty(self.context.as_ctx_ref(), target),
                name.as_ptr()
            )),
            (
                Type::Int { bits: 64 },
                LLVMTypeKind::LLVMPointerTypeKind
            ) => Ok(LLVMBuildPtrToInt(
                self.builder.as_mut_ptr(),
                val,
                get_llvm_ty(self.context.as_ctx_ref(), target),
                name.as_ptr()
            )),
            (
                Type::Pointer { target_ty },
                LLVMTypeKind::LLVMArrayTypeKind
            ) => {
                Ok(LLVMBuildStructGEP2(
                    self.builder.as_mut_ptr(),
                    get_llvm_ty(self.context.as_ctx_ref(), target),
                    val,
                    0,
                    name.as_ptr()

                ))
            },
            (_, _) => todo!()
        }
    }
}

/// Converts a Frontend [Type] into a [LLVMTypeRef].
unsafe fn get_llvm_ty(ctx: *mut LLVMContext, ty: Type) -> *mut LLVMType {
    match ty {
        Type::Int { bits: 8 } | Type::UInt { bits: 8 } => LLVMInt8TypeInContext(ctx),
        Type::Int { bits: 16 } | Type::UInt { bits: 16 } => LLVMInt16TypeInContext(ctx),
        Type::Int { bits: 32 } | Type::UInt { bits: 32 } => LLVMInt32TypeInContext(ctx),
        Type::Int { bits: 64 } | Type::UInt { bits: 64 } => LLVMInt64TypeInContext(ctx),
        Type::Array { elem_ty, len } => LLVMArrayType(get_llvm_ty(ctx, *elem_ty), len as u32),
        Type::Pointer { target_ty } => LLVMPointerType(get_llvm_ty(ctx, *target_ty), 0),
        Type::Never => LLVMVoidTypeInContext(ctx), // <-- Should never happen.
        Type::Void => LLVMVoidTypeInContext(ctx),
        Type::Bool => LLVMInt8TypeInContext(ctx),
        Type::Char => LLVMInt8TypeInContext(ctx),
        Type::Custom(n) => {
            let c_name = CString::new(n).unwrap();
            LLVMGetTypeByName2(ctx, c_name.as_ptr())
        }
        Type::_Uninitialized(t) => get_llvm_ty(ctx, *t),
        _ => unimplemented!(),
    }
}

/// Crates a [LLVMValue] from a constant.
unsafe fn llvm_const(ctx: *mut LLVMContext, val: Value, target: *mut LLVMType) -> *mut LLVMValue {
    match val.deref() {
        flakec_middle::ast::value::Value_::Str(s) => LLVMConstStringInContext(
            ctx,
            CString::new(s.as_str()).unwrap().as_ptr(),
            s.len() as u32,
            0,
        ),
        flakec_middle::ast::value::Value_::Number {
            is_negative: true,
            value,
        } => LLVMConstInt(target, -value.parse::<i64>().unwrap() as u64, 1),
        flakec_middle::ast::value::Value_::Number {
            is_negative: false,
            value,
        } => LLVMConstInt(target, value.parse::<u64>().unwrap(), 0),
        flakec_middle::ast::value::Value_::Boolean(true) => {
            LLVMConstInt(LLVMInt8TypeInContext(ctx), 1, 0)
        }
        flakec_middle::ast::value::Value_::Boolean(false) => {
            LLVMConstInt(LLVMInt8TypeInContext(ctx), 0, 0)
        }
        _ => todo!(),
    }
}

pub struct LLVMFunction<'ctx> {
    ret_ty: *mut LLVMType,
    args: Vec<*mut LLVMType>,
    _phantom: PhantomData<&'ctx ()>,
}

impl<'a> LLVMFunction<'a> {
    pub unsafe fn from_ast(ctx: LLVMContextRef, func: Function<'a>) -> Self {
        let ret_ty = match func.ret {
            None => LLVMVoidType(),
            Some(ExplicitRetType { ty, .. }) => get_llvm_ty(ctx, ty),
        };

        let mut args: Vec<*mut LLVMType> = vec![];

        for arg in func.args.inner.0 {
            args.push(get_llvm_ty(ctx, arg.ty))
        }

        Self {
            ret_ty,
            args,
            _phantom: PhantomData,
        }
    }

    pub fn argc(&self) -> u32 {
        let res = self.args.len();

        res as u32
    }

    pub unsafe fn as_type(&mut self) -> *mut LLVMType {
        LLVMFunctionType(self.ret_ty, self.args.as_mut_ptr(), self.argc(), 0)
    }
}
