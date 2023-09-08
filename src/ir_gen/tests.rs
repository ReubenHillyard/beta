use crate::ir_gen::Compiler;
use inkwell::context::Context;
use std::io::Write;
use std::path::Path;
use std::process::Command;

#[test]
pub fn test_ir_gen() {
    let context = Context::create();
    let mut compiler = Compiler::new(&context, "test");

    let i32_type = compiler.context.i32_type();
    let str_type = compiler.byte_ptr_type();
    let puts_type = i32_type.fn_type(&[str_type.into()], false);
    let zero_i32 = i32_type.const_zero();

    let hello_world_str = compiler.context.const_string(b"Hello, World!", true);
    let hello_world = compiler.add_global("hello_world", &hello_world_str);
    let hello_world_ptr = hello_world.as_pointer_value();

    let puts = compiler.declare_function("puts", puts_type);

    let main_fn = compiler.add_main_function();

    main_fn
        .compiler_builder
        .builder
        .build_call(puts, &[hello_world_ptr.into()], "");
    main_fn
        .compiler_builder
        .builder
        .build_return(Some(&zero_i32));

    let bitcode_filepath = "test_llvm_ir.bc";
    std::io::stdout()
        .write_all(compiler.module.to_string().as_bytes())
        .unwrap();
    compiler.module.verify().unwrap();
    compiler
        .module
        .write_bitcode_to_path(Path::new(bitcode_filepath));

    let clang_output = Command::new("clang-16")
        .args([bitcode_filepath, "-o", "test_llvm_ir"])
        .output()
        .expect("clang not found");
    assert!(clang_output.status.success(), "clang-16 failed");

    let program_output = Command::new("./test_llvm_ir")
        .output()
        .expect("program not found");
    assert!(program_output.status.success(), "program failed");
    std::io::stdout().write_all(&program_output.stdout).unwrap();

    assert_eq!(&program_output.stdout, b"Hello, World!\n");
}
