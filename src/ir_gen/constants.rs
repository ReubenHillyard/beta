use crate::ir_gen::compile::Compiler;
use inkwell::values::{ArrayValue, PointerValue, StructValue};

impl<'ctx> Compiler<'ctx> {
    pub fn noop_captures_clone(&self) -> PointerValue<'ctx> {
        self.module
            .get_function("noop_captures_clone")
            .unwrap()
            .as_global_value()
            .as_pointer_value()
    }
    pub fn noop_captures_destroy(&self) -> PointerValue<'ctx> {
        self.module
            .get_function("noop_captures_destroy")
            .unwrap()
            .as_global_value()
            .as_pointer_value()
    }
    pub fn empty_captures_constant(&self) -> ArrayValue<'ctx> {
        self.byte_type().const_array(&[])
    }
    pub fn universe_constant(&self) -> StructValue<'ctx> {
        self.context.const_struct(
            &[
                self.module
                    .get_function("universe_size")
                    .unwrap()
                    .as_global_value()
                    .as_pointer_value()
                    .into(),
                self.module
                    .get_function("universe_clone")
                    .unwrap()
                    .as_global_value()
                    .as_pointer_value()
                    .into(),
                self.module
                    .get_function("universe_destroy")
                    .unwrap()
                    .as_global_value()
                    .as_pointer_value()
                    .into(),
                self.universe_type().size_of().unwrap().into(),
                self.noop_captures_clone().into(),
                self.noop_captures_destroy().into(),
                self.empty_captures_constant().into(),
            ],
            false,
        )
    }
    pub fn pi_constant(&self) -> StructValue<'ctx> {
        self.context.const_struct(
            &[
                self.module
                    .get_function("pi_size")
                    .unwrap()
                    .as_global_value()
                    .as_pointer_value()
                    .into(),
                self.module
                    .get_function("pi_clone")
                    .unwrap()
                    .as_global_value()
                    .as_pointer_value()
                    .into(),
                self.module
                    .get_function("pi_destroy")
                    .unwrap()
                    .as_global_value()
                    .as_pointer_value()
                    .into(),
                self.universe_type().size_of().unwrap().into(),
                self.noop_captures_clone().into(),
                self.noop_captures_destroy().into(),
                self.empty_captures_constant().into(),
            ],
            false,
        )
    }
}
