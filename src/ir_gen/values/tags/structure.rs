//! Traits for structure types, and implementors.

use crate::ir_gen::values::tags::function::Function;
use crate::ir_gen::values::tags::{tag, Object, Tag};
use crate::ir_gen::Compiler;
use inkwell::types::StructType;

/// Trait for a built-in structure type.
pub trait Structure: Object {
    /// The name of the global value corresponding to `Self`.
    const NAME: &'static str;

    /// The [`StructType`] corresponding to `Self`.
    fn struct_type<'ctx>(compiler: &Compiler<'ctx>) -> StructType<'ctx>;
}

impl Structure for tag::Universe {
    const NAME: &'static str = "Type";
    fn struct_type<'ctx>(compiler: &Compiler<'ctx>) -> StructType<'ctx> {
        compiler.context.struct_type(
            &[
                tag::SizeFn::fn_ptr_type(compiler).into(),
                tag::CloneFn::fn_ptr_type(compiler).into(),
                tag::DestroyFn::fn_ptr_type(compiler).into(),
                compiler.usize_type().into(),
                tag::CapturesCloneFn::fn_ptr_type(compiler).into(),
                tag::CapturesDestroyFn::fn_ptr_type(compiler).into(),
                compiler.captures_type().into(),
            ],
            false,
        )
    }
}

impl Structure for tag::Pi {
    const NAME: &'static str = "Pi";
    fn struct_type<'ctx>(compiler: &Compiler<'ctx>) -> StructType<'ctx> {
        compiler.context.struct_type(
            &[
                tag::RawFn::fn_ptr_type(compiler).into(),
                tag::RetSizeFn::fn_ptr_type(compiler).into(),
                compiler.usize_type().into(),
                tag::CapturesCloneFn::fn_ptr_type(compiler).into(),
                tag::CapturesDestroyFn::fn_ptr_type(compiler).into(),
                compiler.captures_type().into(),
            ],
            false,
        )
    }
}

/// Trait for fields of a [`Structure`].
pub trait FieldOf<S: Structure>: Tag {
    /// The index of the field within `S`, counting from zero.
    const INDEX: u32;

    /// The type of the field.
    type Ty: Object;
}

impl FieldOf<tag::Universe> for tag::SizeFn {
    const INDEX: u32 = 0;
    type Ty = tag::SizeFn;
}

impl FieldOf<tag::Universe> for tag::CloneFn {
    const INDEX: u32 = 1;
    type Ty = tag::CloneFn;
}

impl FieldOf<tag::Universe> for tag::DestroyFn {
    const INDEX: u32 = 2;
    type Ty = tag::DestroyFn;
}

impl FieldOf<tag::Universe> for tag::TotalSize {
    const INDEX: u32 = 3;
    type Ty = tag::Usize;
}

impl FieldOf<tag::Universe> for tag::CapturesCloneFn {
    const INDEX: u32 = 4;
    type Ty = tag::CapturesCloneFn;
}

impl FieldOf<tag::Universe> for tag::CapturesDestroyFn {
    const INDEX: u32 = 5;
    type Ty = tag::CapturesDestroyFn;
}

impl FieldOf<tag::Universe> for tag::Captures {
    const INDEX: u32 = 6;
    type Ty = tag::Captures;
}

impl FieldOf<tag::Pi> for tag::RawFn {
    const INDEX: u32 = 0;
    type Ty = tag::RawFn;
}

impl FieldOf<tag::Pi> for tag::RetSizeFn {
    const INDEX: u32 = 1;
    type Ty = tag::RetSizeFn;
}

impl FieldOf<tag::Pi> for tag::TotalSize {
    const INDEX: u32 = 2;
    type Ty = tag::Usize;
}

impl FieldOf<tag::Pi> for tag::CapturesCloneFn {
    const INDEX: u32 = 3;
    type Ty = tag::CapturesCloneFn;
}

impl FieldOf<tag::Pi> for tag::CapturesDestroyFn {
    const INDEX: u32 = 4;
    type Ty = tag::CapturesDestroyFn;
}

impl FieldOf<tag::Pi> for tag::Captures {
    const INDEX: u32 = 5;
    type Ty = tag::Captures;
}
