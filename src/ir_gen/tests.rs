use crate::ir_gen::compile::Compiler;
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
    let main_builder = compiler.context.create_builder();
    main_builder.position_at_end(entry);
    let hello_world_ptr = unsafe {
        main_builder.build_gep(hello_world.as_pointer_value(), &[zero_i64, zero_i64], "")
    };

    let test_fn = compiler.module.add_function("test", main_type, None);
    let entry = compiler.context.append_basic_block(test_fn, "");
    let test_builder = compiler.context.create_builder();
    test_builder.position_at_end(entry);
    test_builder.build_int_nuw_add(zero_i64, zero_i64, "");
    test_builder.build_return(Some(&zero_i32));

    main_builder.build_call(puts, &[hello_world_ptr.into()], "");
    main_builder.build_return(Some(&zero_i32));

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
