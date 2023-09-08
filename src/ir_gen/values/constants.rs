use crate::ir_gen::values::names::{EMPTY_CAPTURES_NAME, MEMCPY_NAME};
use crate::ir_gen::values::tags::builtin_type::{BuiltinType, HasAssociatedFn};
use crate::ir_gen::values::tags::flow::Read;
use crate::ir_gen::values::tags::function::{Function, NoopFunction};
use crate::ir_gen::values::tags::tag;
use crate::ir_gen::values::{FnValue, PtrValue};
use crate::ir_gen::Compiler;
use inkwell::values::{ArrayValue, FunctionValue, StructValue};

impl<'ctx> Compiler<'ctx> {
    /// Obtains built-in function `memcpy`.
    pub fn memcpy(&self) -> FunctionValue<'ctx> {
        self.get_function(MEMCPY_NAME).unwrap()
    }

    /// Obtains a built-in no-op function.
    pub fn get_noop_fn<F: NoopFunction>(&self, f: F) -> FnValue<'ctx, F> {
        self.get_function_ptr(F::NOOP_NAME)
            .unwrap()
            .as_fn_with_type(f)
    }

    /// Creates an empty capture-list.
    pub fn empty_captures_constant(&self) -> ArrayValue<'ctx> {
        self.byte_type().const_array(&[])
    }

    /// Obtains a pointer to the built-in empty capture-list.
    pub fn empty_captures_ptr(&self) -> PtrValue<'ctx, Read, tag::Captures> {
        self.get_global_ptr(EMPTY_CAPTURES_NAME)
            .unwrap()
            .as_ptr_to(tag::Captures)
    }

    /// Obtains a built-in special member.
    pub fn get_builtin_special_member<B: BuiltinType, F: Function>(
        &self,
        _b: B,
        f: F,
    ) -> FnValue<'ctx, F>
        where
            B: HasAssociatedFn<F>,
    {
        self.get_function_ptr(B::get_name_of(f))
            .unwrap()
            .as_fn_with_type(f)
    }

    /// Creates the initializer for a built-in type.
    pub fn compile_builtin_type<B: BuiltinType>(&self, b: B) -> StructValue<'ctx> {
        self.compile_universe_without_captures(
            self.get_builtin_special_member(b, tag::SizeFn),
            self.get_builtin_special_member(b, tag::CloneFn),
            self.get_builtin_special_member(b, tag::DestroyFn),
        )
    }
}
