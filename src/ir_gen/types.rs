use crate::ir_gen::values::tags::structure::Structure;
use crate::ir_gen::Compiler;
use inkwell::types::{ArrayType, FunctionType, IntType, PointerType, VoidType};
use inkwell::values::IntValue;

impl<'ctx> Compiler<'ctx> {
    /// Obtains the LLVM IR type returned by procedures.
    ///
    /// This corresponds to the `void` type in C/C++.
    pub fn void_type(&self) -> VoidType<'ctx> {
        self.context.void_type()
    }

    /// Obtains the LLVM IR `bool` type.
    pub fn bool_type(&self) -> IntType<'ctx> {
        self.context.bool_type()
    }

    /// Obtains the LLVM IR byte type.
    ///
    /// Requires the target machine has 8 bits in a byte.
    pub fn byte_type(&self) -> IntType<'ctx> {
        self.context.i8_type()
    }

    /// Obtains the LLVM IR `ptr` type.
    ///
    /// This corresponds to the `void*` type in C/C++.
    pub fn byte_ptr_type(&self) -> PointerType<'ctx> {
        self.byte_type().ptr_type(Default::default())
    }

    /// Obtains the LLVM IR pointer-sized integer type.
    pub fn usize_type(&self) -> IntType<'ctx> {
        self.context
            .ptr_sized_int_type(self.execution_engine.get_target_data(), None)
    }

    /// Obtains the preferred alignment of pointer values.
    pub fn preferred_alignment(&self) -> u32 {
        self.execution_engine
            .get_target_data()
            .get_preferred_alignment(&self.byte_ptr_type())
    }

    /// Obtains the LLVM IR `i32` type.
    pub fn i32_type(&self) -> IntType<'ctx> {
        self.context.i32_type()
    }

    /// Obtains the LLVM IR type that the `main` function should have.
    pub fn main_type(&self) -> FunctionType<'ctx> {
        self.i32_type().fn_type(&[], false)
    }

    /// Obtains the LLVM IR type that `memcpy` should have.
    pub fn memcpy_type(&self) -> FunctionType<'ctx> {
        self.void_type().fn_type(
            &[
                self.byte_ptr_type().into(),
                self.byte_ptr_type().into(),
                self.usize_type().into(),
                self.bool_type().into(),
            ],
            false,
        )
    }

    /// Obtains the LLVM IR type used to represent the location of variable-sized captures.
    pub fn captures_type(&self) -> ArrayType<'ctx> {
        self.byte_type().array_type(0)
    }

    /// Determines the size of a structure.
    pub fn size_of_structure<S: Structure>(&self, _s: S) -> IntValue<'ctx> {
        S::struct_type(self)
            .size_of()
            .unwrap()
            .const_cast(self.usize_type(), false)
    }

    /// Determines the size of an integer type.
    pub fn size_of_int_type(&self, type_: IntType<'ctx>) -> IntValue<'ctx> {
        type_.size_of().const_cast(self.usize_type(), false)
    }
}
