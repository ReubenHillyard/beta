//! Traits for functions and implementors.

use crate::ir_gen::values::tags::args::{
    Args, CapturesCloneFnArgs, CapturesDestroyFnArgs, CloneFnArgs, DestroyFnArgs, RawFnArgs,
    RetSizeFnArgs, SizeFnArgs,
};
use crate::ir_gen::values::tags::attributes::{memory_val, MemoryVal};
use crate::ir_gen::values::tags::ret::Ret;
use crate::ir_gen::values::tags::tag::{
    CapturesCloneFn, CapturesDestroyFn, CloneFn, DestroyFn, RawFn, RetSizeFn, SizeFn,
};
use crate::ir_gen::values::tags::{tag, Object};
use crate::ir_gen::Compiler;
use inkwell::types::{FunctionType, PointerType};
use inkwell::values::IntValue;

/// Trait for types which are either a function type or unknown.
pub trait FunctionOrUnknown: Object {}

/// Trait for function types.
pub trait Function: FunctionOrUnknown {
    /// The return type.
    type RetT<'ctx>: Ret<'ctx>;

    /// The arguments type.
    type ArgsT<'ctx>: Args<'ctx>;

    /// The memory permissions.
    const MEMORY_VAL: MemoryVal;

    /// The LLVM IR function type.
    fn fn_type<'ctx>(compiler: &Compiler<'ctx>) -> FunctionType<'ctx>;

    /// The LLVM IR function pointer type.
    ///
    /// Must immediately return `Self::fn_type(compiler).ptr_type(address_space)`, for the
    /// appropriate [`address_space: AddressSpace`](inkwell::AddressSpace).
    fn fn_ptr_type<'ctx>(compiler: &Compiler<'ctx>) -> PointerType<'ctx> {
        Self::fn_type(compiler).ptr_type(Default::default())
    }
}

impl<F: Function> Object for F {}

impl<F: Function> FunctionOrUnknown for F {}

impl FunctionOrUnknown for tag::Unknown {}

impl Function for SizeFn {
    type RetT<'ctx> = IntValue<'ctx>;
    type ArgsT<'ctx> = SizeFnArgs<'ctx>;
    const MEMORY_VAL: MemoryVal = memory_val::NON_ARGMEM_READ_WRITE | memory_val::ARGMEM_READ;
    fn fn_type<'ctx>(compiler: &Compiler<'ctx>) -> FunctionType<'ctx> {
        compiler.usize_type().fn_type(
            &[
                compiler.byte_ptr_type().into(), // captures
                compiler.byte_ptr_type().into(), // argument
            ],
            false,
        )
    }
}

impl Function for CloneFn {
    type RetT<'ctx> = ();
    type ArgsT<'ctx> = CloneFnArgs<'ctx>;
    const MEMORY_VAL: MemoryVal = memory_val::READ_WRITE;
    fn fn_type<'ctx>(compiler: &Compiler<'ctx>) -> FunctionType<'ctx> {
        compiler.void_type().fn_type(
            &[
                compiler.byte_ptr_type().into(), // captures
                compiler.byte_ptr_type().into(), // source
                compiler.byte_ptr_type().into(), // destination
            ],
            false,
        )
    }
}

impl Function for DestroyFn {
    type RetT<'ctx> = ();
    type ArgsT<'ctx> = DestroyFnArgs<'ctx>;
    const MEMORY_VAL: MemoryVal = memory_val::NON_ARGMEM_READ_WRITE | memory_val::ARGMEM_READ;
    fn fn_type<'ctx>(compiler: &Compiler<'ctx>) -> FunctionType<'ctx> {
        compiler.void_type().fn_type(
            &[
                compiler.byte_ptr_type().into(), // captures
                compiler.byte_ptr_type().into(), // argument
            ],
            false,
        )
    }
}

impl Function for CapturesCloneFn {
    type RetT<'ctx> = ();
    type ArgsT<'ctx> = CapturesCloneFnArgs<'ctx>;
    const MEMORY_VAL: MemoryVal = memory_val::READ_WRITE;
    fn fn_type<'ctx>(compiler: &Compiler<'ctx>) -> FunctionType<'ctx> {
        compiler.void_type().fn_type(
            &[
                compiler.byte_ptr_type().into(), // captures
                compiler.byte_ptr_type().into(), // destination
            ],
            false,
        )
    }
}

impl Function for CapturesDestroyFn {
    type RetT<'ctx> = ();
    type ArgsT<'ctx> = CapturesDestroyFnArgs<'ctx>;
    const MEMORY_VAL: MemoryVal = memory_val::NON_ARGMEM_READ_WRITE | memory_val::ARGMEM_READ;
    fn fn_type<'ctx>(compiler: &Compiler<'ctx>) -> FunctionType<'ctx> {
        compiler.void_type().fn_type(
            &[
                compiler.byte_ptr_type().into(), // captures
            ],
            false,
        )
    }
}

impl Function for RawFn {
    type RetT<'ctx> = ();
    type ArgsT<'ctx> = RawFnArgs<'ctx>;
    const MEMORY_VAL: MemoryVal = memory_val::READ_WRITE;
    fn fn_type<'ctx>(compiler: &Compiler<'ctx>) -> FunctionType<'ctx> {
        compiler.void_type().fn_type(
            &[
                compiler.byte_ptr_type().into(), // captures
                compiler.byte_ptr_type().into(), // argument
                compiler.byte_ptr_type().into(), // return address
            ],
            false,
        )
    }
}

impl Function for RetSizeFn {
    type RetT<'ctx> = IntValue<'ctx>;
    type ArgsT<'ctx> = RetSizeFnArgs<'ctx>;
    const MEMORY_VAL: MemoryVal = memory_val::NON_ARGMEM_READ_WRITE | memory_val::ARGMEM_READ;
    fn fn_type<'ctx>(compiler: &Compiler<'ctx>) -> FunctionType<'ctx> {
        compiler.usize_type().fn_type(
            &[
                compiler.byte_ptr_type().into(), // captures
                compiler.byte_ptr_type().into(), // argument
            ],
            false,
        )
    }
}

/// Trait for built-in no-op functions.
pub trait NoopFunction: Function {
    /// The name of the function.
    const NOOP_NAME: &'static str;
}

impl NoopFunction for DestroyFn {
    const NOOP_NAME: &'static str = "noop_destroy";
}

impl NoopFunction for CapturesCloneFn {
    const NOOP_NAME: &'static str = "noop_captures_clone";
}

impl NoopFunction for CapturesDestroyFn {
    const NOOP_NAME: &'static str = "noop_captures_destroy";
}
