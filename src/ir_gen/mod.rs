#[cfg(test)]
mod tests {
    use std::io::Write;
    use std::path::Path;
    use std::process::Command;
    use inkwell::context::Context;

    #[test]
    pub fn test_ir_gen() {
        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();

        let i8_type = context.i8_type();
        let i32_type = context.i32_type();
        let i64_type = context.i64_type();
        let str_type = i8_type.ptr_type(Default::default());
        let main_type = i32_type.fn_type(&[], false);
        let puts_type = i32_type.fn_type(&[str_type.into()], false);
        let zero_i32 = i32_type.const_zero();
        let zero_i64 = i64_type.const_zero();

        let hello_world_str = context.const_string(b"Hello, World!", true);
        let hello_world_type = hello_world_str.get_type();
        let hello_world = module.add_global(hello_world_type, None, "hello_world");
        hello_world.set_initializer(&hello_world_str);

        let puts = module.add_function("puts", puts_type, None);

        let main_fn = module.add_function("main", main_type, None);
        let entry = context.append_basic_block(main_fn, "entry");
        builder.position_at_end(entry);
        let hello_world_ptr = unsafe { builder.build_gep(hello_world.as_pointer_value(), &[zero_i64, zero_i64], "a") };
        builder.build_call(puts, &[hello_world_ptr.into()], "b");
        builder.build_return(Some(&zero_i32));

        let bitcode_filepath = "test_llvm_ir.bc";
        println!("{}", module.to_string());
        assert!(module.verify().is_ok());
        module.write_bitcode_to_path(Path::new(bitcode_filepath));

        let clang_output = Command::new("clang").args([bitcode_filepath, "-o", "test_llvm_ir"]).output().expect("clang not found");
        assert!(clang_output.status.success(), "clang failed");

        let program_output = Command::new("./test_llvm_ir").output().expect("program not found");
        assert!(program_output.status.success(), "program failed");
        std::io::stdout().write_all(&program_output.stdout).unwrap();

        assert_eq!(&program_output.stdout, b"Hello, World!\n");
    }
}
