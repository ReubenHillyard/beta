use crate::typing::environments::Definitions;
use crate::typing::value::{TypedValue, Value};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{FunctionType, IntType, PointerType, StructType};
use inkwell::values::{GlobalValue, StructValue};

pub mod value;

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

pub struct ConcreteTypedValue<'ctx> {
    pub type_: StructType<'ctx>,
    pub value: StructValue<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Compiler<'ctx> {
        let compiler = Compiler {
            context,
            module: context.create_module(name),
            builder: context.create_builder(),
        };
        compiler.add_type_constant();
        compiler
    }
    fn add_type_constant(&self) {
        let type_size_fn = self
            .module
            .add_function("type_size_fn", self.size_fn_type(), None);
        let entry = self.context.append_basic_block(type_size_fn, "");
        self.builder.position_at_end(entry);

        let type_ = type_size_fn.get_nth_param(1).unwrap().into_pointer_value();
        let type_ = self
            .builder
            .build_pointer_cast(type_, self.type_ptr_type(), "");
        let type_vla = self.builder.build_struct_gep(type_, 1, "").unwrap();
        let type_vla_len = self.builder.build_struct_gep(type_vla, 0, "").unwrap();
        let type_vla_len = self.builder.build_load(type_vla_len, "");
        let result = self.builder.build_int_add(
            self.usize_type().const_int(16, false),
            type_vla_len.into_int_value(),
            "",
        );
        self.builder.build_return(Some(&result));

        let type_ = self.module.add_global(self.type_type(), None, "Type");
        type_.set_initializer(
            &self.context.const_struct(
                &[
                    type_size_fn.as_global_value().as_pointer_value().into(),
                    self.context
                        .const_struct(
                            &[
                                self.usize_type().const_int(0, false).into(),
                                self.byte_type().const_array(&[]).into(),
                            ],
                            false,
                        )
                        .into(),
                ],
                false,
            ),
        );
        type_.set_constant(true);
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
    pub fn byte_vla_type(&self) -> StructType<'ctx> {
        self.context.struct_type(
            &[
                self.usize_type().into(),
                self.byte_type().array_type(0).into(),
            ],
            false,
        )
    }
    pub fn byte_vla_ptr_type(&self) -> PointerType<'ctx> {
        self.byte_vla_type().ptr_type(Default::default())
    }
    pub fn size_fn_type(&self) -> FunctionType<'ctx> {
        self.usize_type().fn_type(
            &[self.byte_vla_ptr_type().into(), self.byte_ptr_type().into()],
            false,
        )
    }
    pub fn size_fn_ptr_type(&self) -> PointerType<'ctx> {
        self.size_fn_type().ptr_type(Default::default())
    }
    pub fn type_type(&self) -> StructType<'ctx> {
        self.context.struct_type(
            &[self.size_fn_ptr_type().into(), self.byte_vla_type().into()],
            false,
        )
    }
    pub fn type_ptr_type(&self) -> PointerType<'ctx> {
        self.type_type().ptr_type(Default::default())
    }

    pub fn type_global(&self) -> GlobalValue<'ctx> {
        self.module.get_global("Type").unwrap()
    }

    fn concrete(
        &self,
        _defs: &Definitions,
        _ctx_len: usize,
        typed_value: &TypedValue,
    ) -> ConcreteTypedValue<'ctx> {
        match typed_value.get_type().wrapped() {
            Value::PiType { .. } => unimplemented!(),
            Value::Universe => unimplemented!(),
            _ => panic!(
                "`{}` is not a type in the empty context",
                typed_value.get_type()
            ),
        }
    }
    pub fn add_global(&self, defs: &Definitions, name: &str, typed_value: &TypedValue) {
        let concrete = self.concrete(defs, 0, typed_value);
        let global = self.module.add_global(concrete.type_, None, name);
        global.set_initializer(&concrete.value);
    }
}

#[cfg(test)]
pub mod tests {
    use crate::ir_gen::Compiler;
    use inkwell::context::Context;
    use std::io::Write;
    use std::path::Path;
    use std::process::Command;

    #[test]
    pub fn test_ir_gen() {
        let context = Context::create();
        let compiler = Compiler::new(&context, "test");

        let i8_type = compiler.context.i8_type();
        let i32_type = compiler.context.i32_type();
        let i64_type = compiler.context.i64_type();
        let str_type = i8_type.ptr_type(Default::default());
        let main_type = i32_type.fn_type(&[], false);
        let puts_type = i32_type.fn_type(&[str_type.into()], false);
        let zero_i32 = i32_type.const_zero();
        let zero_i64 = i64_type.const_zero();

        let hello_world_str = compiler.context.const_string(b"Hello, World!", true);
        let hello_world_type = hello_world_str.get_type();
        let hello_world = compiler
            .module
            .add_global(hello_world_type, None, "hello_world");
        hello_world.set_initializer(&hello_world_str);
        hello_world.set_constant(true);

        let puts = compiler.module.add_function("puts", puts_type, None);

        let main_fn = compiler.module.add_function("main", main_type, None);
        let entry = compiler.context.append_basic_block(main_fn, "");
        compiler.builder.position_at_end(entry);
        let hello_world_ptr = unsafe {
            compiler
                .builder
                .build_gep(hello_world.as_pointer_value(), &[zero_i64, zero_i64], "")
        };
        compiler
            .builder
            .build_call(puts, &[hello_world_ptr.into()], "");
        compiler.builder.build_return(Some(&zero_i32));

        let bitcode_filepath = "test_llvm_ir.bc";
        std::io::stdout()
            .write_all(compiler.module.to_string().as_bytes())
            .unwrap();
        assert!(compiler.module.verify().is_ok());
        compiler
            .module
            .write_bitcode_to_path(Path::new(bitcode_filepath));

        let clang_output = Command::new("clang")
            .args([bitcode_filepath, "-o", "test_llvm_ir"])
            .output()
            .expect("clang not found");
        assert!(clang_output.status.success(), "clang failed");

        let program_output = Command::new("./test_llvm_ir")
            .output()
            .expect("program not found");
        assert!(program_output.status.success(), "program failed");
        std::io::stdout().write_all(&program_output.stdout).unwrap();

        assert_eq!(&program_output.stdout, b"Hello, World!\n");
    }
}
