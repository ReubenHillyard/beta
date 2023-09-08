//! Types and functions for dealing with LLVM IR values.

use crate::ir_gen::values::tags::flow::Flow;
use crate::ir_gen::values::tags::function::{Function, FunctionOrUnknown};
use crate::ir_gen::values::tags::{tag, Object};
use inkwell::values::PointerValue;
use tags::flow::{Partial, Read, Write};

#[doc(hidden)]
pub mod call;
pub mod captures;
#[doc(hidden)]
pub mod constants;
pub mod destructors;
pub mod functions;
#[doc(hidden)]
pub mod globals;
pub mod low;
#[doc(hidden)]
pub mod members;
#[doc(hidden)]
pub mod metadata;
pub mod names;
#[doc(hidden)]
pub mod special;
pub mod tags;
#[doc(hidden)]
pub mod without_captures;

/// A pointer with given read or write permissions and pointee type.
#[derive(Copy, Clone)]
pub struct PtrValue<'ctx, F: Flow, O: Object>(PointerValue<'ctx>, F, O);

impl<'ctx, O: Object> PtrValue<'ctx, Write, O> {
    /// Obtains a read-only pointer from a write-only pointer.
    ///
    /// Requires the pointee is fully-initialized.
    pub(crate) fn as_read(self) -> PtrValue<'ctx, Read, O> {
        PtrValue(self.0, Read, self.2)
    }
}

impl<'ctx> PtrValue<'ctx, Write, tag::Captures> {
    /// Obtains a pointer tagged as partially-defined from one tagged as write-only.
    pub(crate) fn as_partial(self) -> PtrValue<'ctx, Partial, tag::Captures> {
        PtrValue(self.0, Partial, tag::Captures)
    }
}

impl<'ctx, F: Flow> PtrValue<'ctx, F, tag::Unknown> {
    /// Obtains a pointer tagged as pointing to a given type from one of an unknown type.
    ///
    /// Requires the pointee has the given type.
    pub fn as_ptr_to<O: Object>(self, o: O) -> PtrValue<'ctx, F, O> {
        PtrValue(self.0, self.1, o)
    }
}

impl<'ctx, F: Flow, O: Object> PtrValue<'ctx, F, O> {
    /// Obtains a pointer tagged as pointing to an unknown type from one of a known type.
    pub fn as_byte_ptr(self) -> PtrValue<'ctx, F, tag::Unknown> {
        PtrValue(self.0, self.1, tag::Unknown)
    }
}

/// A function pointer with given pointee type.
#[derive(Copy, Clone)]
pub struct FnValue<'ctx, F: FunctionOrUnknown>(PointerValue<'ctx>, F);

impl<'ctx> FnValue<'ctx, tag::Unknown> {
    /// Obtains a pointer tagged as pointing to a given type from one of an unknown type.
    ///
    /// Requires the pointee has the given type.
    pub fn as_fn_with_type<F: Function>(self, f: F) -> FnValue<'ctx, F> {
        FnValue(self.0, f)
    }
}

/// The location at which a value should be built.
#[derive(Copy, Clone)]
pub enum Location<'ctx> {
    /// Indicates that the value should be built at a specific address.
    Within(PtrValue<'ctx, Write, tag::Unknown>),
    /// Indicates that the value can be built in an `alloca` or refer to an existing value.
    Other,
}
