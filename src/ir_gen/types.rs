use crate::ir_gen::compile::Compiler;
use inkwell::types::{ArrayType, FunctionType, IntType, PointerType, StructType, VoidType};

impl<'ctx> Compiler<'ctx> {
    pub fn void_type(&self) -> VoidType<'ctx> {
        self.context.void_type()
    }
    pub fn byte_type(&self) -> IntType<'ctx> {
        self.context.i8_type()
    }
    pub fn byte_ptr_type(&self) -> PointerType<'ctx> {
        self.byte_type().ptr_type(Default::default())
    }
    pub fn usize_type(&self) -> IntType<'ctx> {
        self.context.i64_type()
    }
    pub fn usize_ptr_type(&self) -> PointerType<'ctx> {
        self.usize_type().ptr_type(Default::default())
    }
    pub fn raw_fn_type(&self) -> FunctionType<'ctx> {
        self.void_type().fn_type(
            &[
                self.byte_ptr_type().into(), // captures
                self.byte_ptr_type().into(), // argument
                self.byte_ptr_type().into(), // return address
            ],
            false,
        )
    }
    pub fn raw_fn_ptr_type(&self) -> PointerType<'ctx> {
        self.raw_fn_type().ptr_type(Default::default())
    }
    pub fn size_fn_type(&self) -> FunctionType<'ctx> {
        self.usize_type().fn_type(
            &[
                self.byte_ptr_type().into(), // captures
                self.byte_ptr_type().into(), // argument
            ],
            false,
        )
    }
    pub fn size_fn_ptr_type(&self) -> PointerType<'ctx> {
        self.size_fn_type().ptr_type(Default::default())
    }
    pub fn clone_fn_type(&self) -> FunctionType<'ctx> {
        self.void_type().fn_type(
            &[
                self.byte_ptr_type().into(), // captures
                self.byte_ptr_type().into(), // source
                self.byte_ptr_type().into(), // destination
            ],
            false,
        )
    }
    pub fn clone_fn_ptr_type(&self) -> PointerType<'ctx> {
        self.clone_fn_type().ptr_type(Default::default())
    }
    pub fn destroy_fn_type(&self) -> FunctionType<'ctx> {
        self.void_type().fn_type(
            &[
                self.byte_ptr_type().into(), // captures
                self.byte_ptr_type().into(), // argument
            ],
            false,
        )
    }
    pub fn destroy_fn_ptr_type(&self) -> PointerType<'ctx> {
        self.destroy_fn_type().ptr_type(Default::default())
    }
    pub fn ret_size_fn_type(&self) -> FunctionType<'ctx> {
        self.usize_type().fn_type(
            &[
                self.byte_ptr_type().into(), // captures
                self.byte_ptr_type().into(), // argument
            ],
            false,
        )
    }
    pub fn ret_size_fn_ptr_type(&self) -> PointerType<'ctx> {
        self.ret_size_fn_type().ptr_type(Default::default())
    }
    pub fn captures_clone_fn_type(&self) -> FunctionType<'ctx> {
        self.void_type().fn_type(
            &[
                self.byte_ptr_type().into(), // captures
                self.byte_ptr_type().into(), // destination
            ],
            false,
        )
    }
    pub fn captures_clone_fn_ptr_type(&self) -> PointerType<'ctx> {
        self.captures_clone_fn_type().ptr_type(Default::default())
    }
    pub fn captures_destroy_fn_type(&self) -> FunctionType<'ctx> {
        self.void_type().fn_type(
            &[
                self.byte_ptr_type().into(), // captures
            ],
            false,
        )
    }
    pub fn captures_destroy_fn_ptr_type(&self) -> PointerType<'ctx> {
        self.captures_destroy_fn_type().ptr_type(Default::default())
    }
    pub fn captures_type(&self) -> ArrayType<'ctx> {
        self.byte_type().array_type(0)
    }
    pub fn universe_type(&self) -> StructType<'ctx> {
        self.context.struct_type(
            &[
                self.size_fn_ptr_type().into(),
                self.clone_fn_ptr_type().into(),
                self.destroy_fn_ptr_type().into(),
                self.usize_type().into(),
                self.captures_clone_fn_ptr_type().into(),
                self.captures_destroy_fn_ptr_type().into(),
                self.captures_type().into(),
            ],
            false,
        )
    }
    pub fn universe_ptr_type(&self) -> PointerType<'ctx> {
        self.universe_type().ptr_type(Default::default())
    }
    pub fn pi_type(&self) -> StructType<'ctx> {
        self.context.struct_type(
            &[
                self.raw_fn_ptr_type().into(),
                self.ret_size_fn_ptr_type().into(),
                self.usize_type().into(),
                self.captures_clone_fn_ptr_type().into(),
                self.captures_destroy_fn_ptr_type().into(),
                self.captures_type().into(),
            ],
            false,
        )
    }
    pub fn pi_ptr_type(&self) -> PointerType<'ctx> {
        self.pi_type().ptr_type(Default::default())
    }
}
