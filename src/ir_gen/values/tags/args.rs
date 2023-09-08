//! Trait for arguments to a function, and implementors.

use crate::ir_gen::values::functions::FunctionWithArgs;
use crate::ir_gen::values::tags::flow::{Read, Write};
use crate::ir_gen::values::tags::tag;
use crate::ir_gen::values::PtrValue;
use inkwell::values::{BasicMetadataValueEnum, FunctionValue, IntValue};
use std::borrow::Borrow;

/// Trait for arguments to a function.
pub trait Args<'ctx> {
    /// Should be defined as [`[BasicMetadataValueEnum<'ctx>; N]`](BasicMetadataValueEnum) where
    /// `N` is the number of arguments.
    type ArrayT: Borrow<[BasicMetadataValueEnum<'ctx>]>;

    /// Puts `self` in the correct format for [`inkwell`] to build a call.
    fn as_args(&self) -> Self::ArrayT;

    /// Creates `self` from a function.
    fn from_fn(function: &mut FunctionWithArgs<'ctx, '_, FunctionValue<'ctx>, (), ()>) -> Self;
}

/// The arguments for `memcpy`.
#[derive(Copy, Clone)]
pub struct MemcpyArgs<'ctx> {
    pub dest_ptr: PtrValue<'ctx, Write, tag::Unknown>,
    pub src_ptr: PtrValue<'ctx, Read, tag::Unknown>,
    pub num_bytes: IntValue<'ctx>,
}

/// The arguments for a size function.
#[derive(Copy, Clone)]
pub struct SizeFnArgs<'ctx> {
    pub captures_ptr: PtrValue<'ctx, Read, tag::Captures>,
    pub arg_ptr: PtrValue<'ctx, Read, tag::Unknown>,
}

impl<'ctx> Args<'ctx> for SizeFnArgs<'ctx> {
    type ArrayT = [BasicMetadataValueEnum<'ctx>; 2];
    fn as_args(&self) -> Self::ArrayT {
        [self.captures_ptr.0.into(), self.arg_ptr.0.into()]
    }
    fn from_fn(function: &mut FunctionWithArgs<'ctx, '_, FunctionValue<'ctx>, (), ()>) -> Self {
        let captures_ptr = function.get_nth_param(0, Read, tag::Captures);
        let arg_ptr = function.get_nth_param(1, Read, tag::Unknown);

        SizeFnArgs {
            captures_ptr,
            arg_ptr,
        }
    }
}

/// The arguments for a clone function.
#[derive(Copy, Clone)]
pub struct CloneFnArgs<'ctx> {
    pub captures_ptr: PtrValue<'ctx, Read, tag::Captures>,
    pub src_ptr: PtrValue<'ctx, Read, tag::Unknown>,
    pub dest_ptr: PtrValue<'ctx, Write, tag::Unknown>,
}

impl<'ctx> Args<'ctx> for CloneFnArgs<'ctx> {
    type ArrayT = [BasicMetadataValueEnum<'ctx>; 3];
    fn as_args(&self) -> Self::ArrayT {
        [
            self.captures_ptr.0.into(),
            self.src_ptr.0.into(),
            self.dest_ptr.0.into(),
        ]
    }
    fn from_fn(function: &mut FunctionWithArgs<'ctx, '_, FunctionValue<'ctx>, (), ()>) -> Self {
        let captures_ptr = function.get_nth_param(0, Read, tag::Captures);
        let src_ptr = function.get_nth_param(1, Read, tag::Unknown);
        let dest_ptr = function.get_nth_param(2, Write, tag::Unknown);

        CloneFnArgs {
            captures_ptr,
            src_ptr,
            dest_ptr,
        }
    }
}

/// The arguments for a destroy function.
#[derive(Copy, Clone)]
pub struct DestroyFnArgs<'ctx> {
    pub captures_ptr: PtrValue<'ctx, Read, tag::Captures>,
    pub arg_ptr: PtrValue<'ctx, Read, tag::Unknown>,
}

impl<'ctx> Args<'ctx> for DestroyFnArgs<'ctx> {
    type ArrayT = [BasicMetadataValueEnum<'ctx>; 2];
    fn as_args(&self) -> Self::ArrayT {
        [self.captures_ptr.0.into(), self.arg_ptr.0.into()]
    }
    fn from_fn(function: &mut FunctionWithArgs<'ctx, '_, FunctionValue<'ctx>, (), ()>) -> Self {
        let captures_ptr = function.get_nth_param(0, Read, tag::Captures);
        let arg_ptr = function.get_nth_param(1, Read, tag::Unknown);

        DestroyFnArgs {
            captures_ptr,
            arg_ptr,
        }
    }
}

/// The arguments for a captures clone function.
#[derive(Copy, Clone)]
pub struct CapturesCloneFnArgs<'ctx> {
    pub captures_ptr: PtrValue<'ctx, Read, tag::Captures>,
    pub dest_ptr: PtrValue<'ctx, Write, tag::Captures>,
}

impl<'ctx> Args<'ctx> for CapturesCloneFnArgs<'ctx> {
    type ArrayT = [BasicMetadataValueEnum<'ctx>; 2];
    fn as_args(&self) -> Self::ArrayT {
        [self.captures_ptr.0.into(), self.dest_ptr.0.into()]
    }
    fn from_fn(function: &mut FunctionWithArgs<'ctx, '_, FunctionValue<'ctx>, (), ()>) -> Self {
        let captures_ptr = function.get_nth_param(0, Read, tag::Captures);
        let dest_ptr = function.get_nth_param(1, Write, tag::Captures);

        CapturesCloneFnArgs {
            captures_ptr,
            dest_ptr,
        }
    }
}

/// The arguments for a captures destroy function.
#[derive(Copy, Clone)]
pub struct CapturesDestroyFnArgs<'ctx> {
    pub captures_ptr: PtrValue<'ctx, Read, tag::Captures>,
}

impl<'ctx> Args<'ctx> for CapturesDestroyFnArgs<'ctx> {
    type ArrayT = [BasicMetadataValueEnum<'ctx>; 1];
    fn as_args(&self) -> Self::ArrayT {
        [self.captures_ptr.0.into()]
    }
    fn from_fn(function: &mut FunctionWithArgs<'ctx, '_, FunctionValue<'ctx>, (), ()>) -> Self {
        let captures_ptr = function.get_nth_param(0, Read, tag::Captures);

        CapturesDestroyFnArgs { captures_ptr }
    }
}

/// The arguments for a raw function.
#[derive(Copy, Clone)]
pub struct RawFnArgs<'ctx> {
    pub captures_ptr: PtrValue<'ctx, Read, tag::Captures>,
    pub arg_ptr: PtrValue<'ctx, Read, tag::Unknown>,
    pub ret_ptr: PtrValue<'ctx, Write, tag::Unknown>,
}

impl<'ctx> Args<'ctx> for RawFnArgs<'ctx> {
    type ArrayT = [BasicMetadataValueEnum<'ctx>; 3];
    fn as_args(&self) -> Self::ArrayT {
        [
            self.captures_ptr.0.into(),
            self.arg_ptr.0.into(),
            self.ret_ptr.0.into(),
        ]
    }
    fn from_fn(function: &mut FunctionWithArgs<'ctx, '_, FunctionValue<'ctx>, (), ()>) -> Self {
        let captures_ptr = function.get_nth_param(0, Read, tag::Captures);
        let arg_ptr = function.get_nth_param(1, Read, tag::Unknown);
        let ret_ptr = function.get_nth_param(2, Write, tag::Unknown);

        RawFnArgs {
            captures_ptr,
            arg_ptr,
            ret_ptr,
        }
    }
}

/// The arguments for a return size function.
#[derive(Copy, Clone)]
pub struct RetSizeFnArgs<'ctx> {
    pub captures_ptr: PtrValue<'ctx, Read, tag::Captures>,
    pub arg_ptr: PtrValue<'ctx, Read, tag::Unknown>,
}

impl<'ctx> Args<'ctx> for RetSizeFnArgs<'ctx> {
    type ArrayT = [BasicMetadataValueEnum<'ctx>; 2];
    fn as_args(&self) -> Self::ArrayT {
        [self.captures_ptr.0.into(), self.arg_ptr.0.into()]
    }
    fn from_fn(function: &mut FunctionWithArgs<'ctx, '_, FunctionValue<'ctx>, (), ()>) -> Self {
        let captures_ptr = function.get_nth_param(0, Read, tag::Captures);
        let arg_ptr = function.get_nth_param(1, Read, tag::Unknown);

        RetSizeFnArgs {
            captures_ptr,
            arg_ptr,
        }
    }
}
