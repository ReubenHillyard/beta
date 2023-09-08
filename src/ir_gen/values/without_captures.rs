use crate::ir_gen::build::CompilerWithBuilder;
use crate::ir_gen::values::tags::flow::Write;
use crate::ir_gen::values::tags::tag;
use crate::ir_gen::values::{FnValue, PtrValue};
use crate::ir_gen::Compiler;
use inkwell::values::{IntValue, StructValue};

impl<'ctx> Compiler<'ctx> {
    /// Creates an initializer for a global universe constant from the necessary fields.
    ///
    /// Requires the provided fields are valid such fields for a global universe constant.
    pub fn compile_universe_without_captures(
        &self,
        size_fn: FnValue<'ctx, tag::SizeFn>,
        clone_fn: FnValue<'ctx, tag::CloneFn>,
        destroy_fn: FnValue<'ctx, tag::DestroyFn>,
    ) -> StructValue<'ctx> {
        self.context.const_struct(
            &[
                size_fn.0.into(),
                clone_fn.0.into(),
                destroy_fn.0.into(),
                self.size_of_structure(tag::Universe).into(),
                self.get_noop_fn(tag::CapturesCloneFn).0.into(),
                self.get_noop_fn(tag::CapturesDestroyFn).0.into(),
                self.empty_captures_constant().into(),
            ],
            false,
        )
    }

    /// Creates an initializer for a global pi constant from the necessary fields.
    ///
    /// Requires the provided fields are valid such fields for a global pi constant.
    pub fn compile_pi_without_captures(
        &mut self,
        raw_function: FnValue<'ctx, tag::RawFn>,
        ret_size_function: FnValue<'ctx, tag::RetSizeFn>,
    ) -> StructValue<'ctx> {
        self.context.const_struct(
            &[
                raw_function.0.into(),
                ret_size_function.0.into(),
                self.size_of_structure(tag::Pi).into(),
                self.get_noop_fn(tag::CapturesCloneFn).0.into(),
                self.get_noop_fn(tag::CapturesDestroyFn).0.into(),
                self.empty_captures_constant().into(),
            ],
            false,
        )
    }
}

impl<'ctx> CompilerWithBuilder<'ctx, '_> {
    /// Builds a field-wise store of the static fields of a pi value from the necessary fields.
    ///
    /// Requires that the fields are valid such fields for a pi value.
    pub fn build_store_pi_static_part(
        &mut self,
        dest_pi_ptr: PtrValue<'ctx, Write, tag::Pi>,
        raw_function: FnValue<'ctx, tag::RawFn>,
        ret_size_function: FnValue<'ctx, tag::RetSizeFn>,
        total_size: IntValue<'ctx>,
        captures_clone_function: FnValue<'ctx, tag::CapturesCloneFn>,
        captures_destroy_function: FnValue<'ctx, tag::CapturesDestroyFn>,
    ) -> PtrValue<'ctx, Write, tag::Captures> {
        self.build_store_field(dest_pi_ptr, tag::RawFn, raw_function);
        self.build_store_field(dest_pi_ptr, tag::RetSizeFn, ret_size_function);
        self.build_store_field(dest_pi_ptr, tag::TotalSize, total_size);
        self.build_store_field(dest_pi_ptr, tag::CapturesCloneFn, captures_clone_function);
        self.build_store_field(
            dest_pi_ptr,
            tag::CapturesDestroyFn,
            captures_destroy_function,
        );
        self.build_get_captures_ptr(dest_pi_ptr)
    }
}
