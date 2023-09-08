use crate::ir_gen::build::CompilerWithBuilder;
use crate::ir_gen::values::low::LoadStore;
use crate::ir_gen::values::tags::flow::Read;
use crate::ir_gen::values::tags::flow::{Flow, Write};
use crate::ir_gen::values::tags::structure::{FieldOf, Structure};
use crate::ir_gen::values::tags::{tag, Tag};
use crate::ir_gen::values::PtrValue;

impl<'ctx> CompilerWithBuilder<'ctx, '_> {
    /// Builds a pointer to a field with a given number within a given pointer to a structure.
    ///
    /// Requires `field` is the index of a field within the structure type `S`.
    fn build_get_numbered_field_ptr<F: Flow, S: Structure>(
        &mut self,
        ptr: PtrValue<'ctx, F, S>,
        field: u32,
    ) -> PtrValue<'ctx, F, tag::Unknown> {
        PtrValue(
            self.builder
                .build_struct_gep(S::struct_type(self.compiler), ptr.0, field, "")
                .unwrap(),
            ptr.1,
            tag::Unknown,
        )
    }

    /// Builds a pointer to a given field within a given pointer to a structure.
    pub fn build_get_field_ptr<F: Flow, S: Structure, Field: FieldOf<S>>(
        &mut self,
        ptr: PtrValue<'ctx, F, S>,
        _field: Field,
    ) -> PtrValue<'ctx, F, Field::Ty> {
        PtrValue(
            self.builder
                .build_struct_gep(S::struct_type(self.compiler), ptr.0, Field::INDEX, "")
                .unwrap(),
            ptr.1,
            Field::Ty::TAG,
        )
    }

    /// Builds a pointer to the captures of a given pointer to a structure.
    pub fn build_get_captures_ptr<F: Flow, S: Structure>(
        &mut self,
        ptr: PtrValue<'ctx, F, S>,
    ) -> PtrValue<'ctx, F, tag::Captures>
        where
            tag::Captures: FieldOf<S, Ty=tag::Captures>,
    {
        self.build_get_field_ptr(ptr, tag::Captures)
    }

    /// Builds a load of a given field of a given pointer to a structure.
    pub fn build_load_field<S: Structure, FieldTy: LoadStore, Field: FieldOf<S, Ty=FieldTy>>(
        &mut self,
        ptr: PtrValue<'ctx, Read, S>,
        field: Field,
    ) -> FieldTy::Elem<'ctx> {
        let field_ptr = self.build_get_field_ptr(ptr, field);
        Field::Ty::build_load(self, field_ptr)
    }

    /// Builds a store of a given field of a given pointer to a structure.
    ///
    /// Requires the specified field is only stored to by this one call.
    pub fn build_store_field<S: Structure, FieldTy: LoadStore, Field: FieldOf<S, Ty=FieldTy>>(
        &mut self,
        ptr: PtrValue<'ctx, Write, S>,
        field: Field,
        elem: FieldTy::Elem<'ctx>,
    ) -> PtrValue<'ctx, Read, FieldTy> {
        let field_ptr = self.build_get_field_ptr(ptr, field);
        Field::Ty::build_store(self, field_ptr, elem)
    }

    /// Builds a field-wise copy of the static part of a structure.
    ///
    /// Panics if any field is not allowed to be loaded or stored.
    pub fn build_copy_static_part<S: Structure>(
        &mut self,
        src_ptr: PtrValue<'ctx, Read, S>,
        dest_ptr: PtrValue<'ctx, Write, S>,
    ) -> PtrValue<'ctx, Read, S> {
        let struct_type = S::struct_type(self.compiler);
        for (field, type_) in struct_type.get_field_types().into_iter().enumerate() {
            let src_ptr = self.build_get_numbered_field_ptr(src_ptr, field as u32);
            let dest_ptr = self.build_get_numbered_field_ptr(dest_ptr, field as u32);
            let value = self.build_load(type_, src_ptr);
            self.build_store(dest_ptr, value);
        }
        PtrValue(dest_ptr.0, Read, dest_ptr.2)
    }
}
