//! Trait and functions for building low-level instructions.

use crate::ir_gen::build::CompilerWithBuilder;
use crate::ir_gen::values::tags::flow::{Read, Write};
use crate::ir_gen::values::tags::function::Function;
use crate::ir_gen::values::tags::{tag, Object};
use crate::ir_gen::values::{FnValue, PtrValue};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, BasicValueEnum, IntValue};

/// A trait for types that can be loaded and stored.
pub trait LoadStore: Object {
    /// The element type.
    type Elem<'ctx>;
    fn build_load<'ctx>(
        compiler_builder: &mut CompilerWithBuilder<'ctx, '_>,
        ptr: PtrValue<'ctx, Read, Self>,
    ) -> Self::Elem<'ctx>;
    fn build_store<'ctx>(
        compiler_builder: &mut CompilerWithBuilder<'ctx, '_>,
        ptr: PtrValue<'ctx, Write, Self>,
        elem: Self::Elem<'ctx>,
    ) -> PtrValue<'ctx, Read, Self>;
}

impl LoadStore for tag::Usize {
    type Elem<'ctx> = IntValue<'ctx>;
    fn build_load<'ctx>(
        compiler_builder: &mut CompilerWithBuilder<'ctx, '_>,
        ptr: PtrValue<'ctx, Read, Self>,
    ) -> Self::Elem<'ctx> {
        compiler_builder
            .build_load(compiler_builder.compiler.usize_type(), ptr)
            .into_int_value()
    }
    fn build_store<'ctx>(
        compiler_builder: &mut CompilerWithBuilder<'ctx, '_>,
        ptr: PtrValue<'ctx, Write, Self>,
        elem: Self::Elem<'ctx>,
    ) -> PtrValue<'ctx, Read, Self> {
        compiler_builder.build_store(ptr, elem)
    }
}

impl<F: Function> LoadStore for F {
    type Elem<'ctx> = FnValue<'ctx, F>;
    fn build_load<'ctx>(
        compiler_builder: &mut CompilerWithBuilder<'ctx, '_>,
        ptr: PtrValue<'ctx, Read, Self>,
    ) -> Self::Elem<'ctx> {
        FnValue(
            compiler_builder
                .build_load(F::fn_ptr_type(compiler_builder.compiler), ptr)
                .into_pointer_value(),
            F::TAG,
        )
    }
    fn build_store<'ctx>(
        compiler_builder: &mut CompilerWithBuilder<'ctx, '_>,
        ptr: PtrValue<'ctx, Write, Self>,
        elem: Self::Elem<'ctx>,
    ) -> PtrValue<'ctx, Read, Self> {
        compiler_builder.build_store(ptr, elem.0)
    }
}

impl<'ctx> CompilerWithBuilder<'ctx, '_> {
    /// Builds an alloca instruction allocating a given number of bytes.
    ///
    /// Requires `size` is a value of type
    /// [`self.compiler.usize_type()`](crate::ir_gen::Compiler::usize_type).
    pub fn build_alloca(&mut self, size: IntValue<'ctx>) -> PtrValue<'ctx, Write, tag::Unknown> {
        let out = self
            .builder
            .build_array_alloca(self.compiler.byte_type(), size, "");
        let alloca = out.as_instruction_value().unwrap();
        alloca
            .set_alignment(self.compiler.preferred_alignment())
            .unwrap();
        PtrValue(out, Write, tag::Unknown)
    }

    /// Queries whether a type is allowed to be loaded and stored.
    fn is_allowed_load_store_type(type_: BasicTypeEnum<'ctx>) -> bool {
        type_.is_pointer_type() || type_.is_int_type() || type_.is_float_type() || {
            if type_.is_array_type() {
                type_.into_array_type().len() == 0
            } else {
                false
            }
        }
    }

    /// Builds a load instruction.
    ///
    /// Panics if `type_` is not allowed to be loaded.
    ///
    /// Requires `ptr` points to a valid value of type `type_`.
    pub fn build_load<O: Object>(
        &mut self,
        type_: impl BasicType<'ctx>,
        ptr: PtrValue<'ctx, Read, O>,
    ) -> BasicValueEnum<'ctx> {
        assert!(
            CompilerWithBuilder::is_allowed_load_store_type(type_.as_basic_type_enum()),
            "tried to load bad type"
        );
        let out = self.builder.build_load(type_, ptr.0, "");
        let load = out.as_instruction_value().unwrap();
        self.compiler.add_invariant_group_metadata(load);
        out
    }

    /// Builds a store instruction.
    ///
    /// Panics if the type of `value` is not allowed to be stored.
    ///
    /// Requires `ptr` points to storage able to hold `value`.
    pub fn build_store<V: BasicValue<'ctx>, O: Object>(
        &mut self,
        ptr: PtrValue<'ctx, Write, O>,
        value: V,
    ) -> PtrValue<'ctx, Read, O> {
        assert!(
            CompilerWithBuilder::is_allowed_load_store_type(value.as_basic_value_enum().get_type()),
            "tried to store bad type"
        );
        let store = self.builder.build_store(ptr.0, value);
        self.compiler.add_invariant_group_metadata(store);
        PtrValue(ptr.0, Read, ptr.2)
    }
}
