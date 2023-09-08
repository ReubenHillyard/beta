//! Trait for return values, and implementors.

use crate::ir_gen::values::tags::flow::Read;
use crate::ir_gen::values::tags::tag;
use crate::ir_gen::values::PtrValue;
use inkwell::values::{CallSiteValue, IntValue};

/// Trait for return values.
pub trait Ret<'ctx> {
    /// Create `self` from a [`CallSiteValue`].
    fn from_ret_val(ret_val: CallSiteValue<'ctx>) -> Self;
}

impl<'ctx> Ret<'ctx> for () {
    fn from_ret_val(_ret_val: CallSiteValue<'ctx>) -> Self {}
}

impl<'ctx> Ret<'ctx> for IntValue<'ctx> {
    fn from_ret_val(ret_val: CallSiteValue<'ctx>) -> Self {
        ret_val
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value()
    }
}

impl<'ctx> Ret<'ctx> for PtrValue<'ctx, Read, tag::Unknown> {
    fn from_ret_val(ret_val: CallSiteValue<'ctx>) -> Self {
        PtrValue(
            ret_val
                .try_as_basic_value()
                .left()
                .unwrap()
                .into_pointer_value(),
            Read,
            tag::Unknown,
        )
    }
}
