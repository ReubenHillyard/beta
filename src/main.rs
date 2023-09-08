#![crate_name = "beta"]

//! A dependently-typed programming language, aiming to support safe mutable state and a cubical
//! interpretation of univalence.

use crate::ir_gen::values::captures::{Captures, Environment};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use ir_gen::values::tags::structure::Structure;
use ir_gen::values::tags::tag;
use ir_gen::values::Location;
use ir_gen::Compiler;
use itertools::{Either, Itertools};
use lexer::lex;
use std::fs::read_to_string;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use typing::abstraction::abstract_file;
use typing::checking::synth_type;
use typing::environments::Definitions;
use typing::evaluation::Evaluate;
use typing::expression::TypedExpression;
use typing::read_back::read_back_with_ctx_len;

pub mod ir_gen;
pub mod lexer;
pub mod parser;
#[cfg(test)]
pub mod tests;
pub mod typing;

fn main_helper(source: &str) {
    let tokens: Vec<_> = lex(source).collect();
    let (tokens, errors): (Vec<_>, Vec<_>) = tokens.into_iter().partition_map(|t| match t {
        Ok(token) => Either::Left(token),
        Err(error) => Either::Right(error),
    });
    if !errors.is_empty() {
        panic!("lexing errors: {:#?}", errors);
    }
    let file = match parser::parse_as_file(&tokens) {
        Ok(file) => file,
        Err(error) => {
            panic!("parse error: {}", error);
        }
    };
    let file = match abstract_file(&file) {
        Ok(file) => file,
        Err(errors) => {
            panic!("name errors: {:#?}", errors);
        }
    };
    let mut defs = Definitions::default();
    let context = Context::create();
    let mut compiler = Compiler::new(&context, "main");
    for (name, expr) in file.globals {
        let typed_expr = match synth_type(&mut defs.with_empty_ctx(), &expr) {
            Ok(type_) => type_,
            Err(error) => {
                panic!("type error: {:#?}\n\n", error);
            }
        };
        if !defs.all_metas_defined() {
            panic!("could not deduce values for some meta-variables in `{name}`");
        }
        let value = typed_expr.evaluate(defs.with_empty_env());
        let type_expr = read_back_with_ctx_len(&defs, 0, value.get_type());
        let value_expr = read_back_with_ctx_len(&defs, 0, value.get_term());
        println!("{name} = {} as {}\n", value_expr, type_expr);
        let type_ = type_expr.evaluate(defs.with_empty_env());
        let value =
            TypedExpression::create_typed(type_, value_expr).evaluate(defs.with_empty_env());
        compiler.compile_global(&mut defs, name, &value);
        defs.define_global(name, value);
        defs.reset_metas();
    }

    let i32_type = compiler.i32_type();
    let i32_size = compiler.size_of_int_type(i32_type);
    let (size_fn, clone_fn) = compiler.add_trivial_type_special_members(i32_size);
    let i32_universe = compiler.compile_universe_without_captures(
        size_fn,
        clone_fn,
        compiler.get_noop_fn(tag::DestroyFn),
    );
    compiler.add_global("i32", &i32_universe);
    let i32_universe_ptr = compiler
        .get_global_ptr("i32")
        .unwrap()
        .as_ptr_to(tag::Universe);

    let mut increment = compiler.add_function(tag::RawFn, "");
    let lhs = increment
        .compiler_builder
        .build_load(i32_type, increment.args.arg_ptr)
        .into_int_value();
    let value =
        increment
            .compiler_builder
            .builder
            .build_int_nuw_add(lhs, i32_type.const_int(1, false), "");
    increment
        .compiler_builder
        .build_store(increment.args.ret_ptr, value);
    let increment = increment.build_return_void();

    let increment_ret_size_fn = compiler.add_function(tag::RetSizeFn, "");
    let increment_ret_size_fn = increment_ret_size_fn.build_return(Some(&i32_size));

    let increment_pi = compiler.compile_pi_without_captures(increment, increment_ret_size_fn);
    compiler.add_global("increment", &increment_pi);
    let increment_ptr = compiler.get_global_ptr("increment").unwrap();

    let mut main_fn = compiler.add_main_function();

    let pi_universe_ptr = main_fn
        .compiler_builder
        .compiler
        .get_global_ptr(tag::Pi::NAME)
        .unwrap()
        .as_ptr_to(tag::Universe);

    let captures = &Captures::default();
    let captures_ptr = main_fn.compiler_builder.compiler.empty_captures_ptr();
    let arg_ptr = None;
    let mut env = Environment {
        captures,
        captures_ptr,
        arg_ptr,
        dtors: &mut main_fn.dtors,
    };

    let zero_i32_ptr = main_fn.compiler_builder.build_alloca(i32_size);
    let zero_i32_ptr = main_fn
        .compiler_builder
        .build_store(zero_i32_ptr, i32_type.const_zero());

    let twenty_seven = main_fn
        .compiler_builder
        .compiler
        .get_global_ptr("twenty_seven")
        .expect("missing global value `twenty_seven`")
        .as_ptr_to(tag::Pi);

    let twenty_seven_i32 = main_fn
        .compiler_builder
        .build_call_pi(
            &mut env,
            pi_universe_ptr,
            twenty_seven,
            i32_universe_ptr.as_byte_ptr(),
            Location::Other,
        )
        .as_ptr_to(tag::Pi);

    let twenty_seven_increment = main_fn
        .compiler_builder
        .build_call_pi(
            &mut env,
            pi_universe_ptr,
            twenty_seven_i32,
            increment_ptr,
            Location::Other,
        )
        .as_ptr_to(tag::Pi);

    let twenty_seven_zero = main_fn.compiler_builder.build_call_pi(
        &mut env,
        i32_universe_ptr,
        twenty_seven_increment,
        zero_i32_ptr,
        Location::Other,
    );

    let twenty_seven_zero = main_fn
        .compiler_builder
        .build_load(i32_type, twenty_seven_zero)
        .into_int_value();

    main_fn
        .compiler_builder
        .builder
        .build_return(Some(&twenty_seven_zero));

    match compiler.module.verify() {
        Ok(_) => {}
        Err(str) => {
            panic!("{}", str.to_string());
        }
    };

    let global_dce: PassManager<Module> = PassManager::create(());
    global_dce.add_global_dce_pass();
    global_dce.run_on(&compiler.module);

    let common_passes: PassManager<Module> = PassManager::create(());
    common_passes.add_early_cse_mem_ssa_pass();
    common_passes.add_instruction_combining_pass();
    common_passes.add_dead_store_elimination_pass();
    common_passes.add_function_inlining_pass();
    common_passes.add_scalar_repl_aggregates_pass_ssa();

    let rare_passes: PassManager<Module> = PassManager::create(());
    rare_passes.add_global_optimizer_pass();
    rare_passes.add_tail_call_elimination_pass();
    rare_passes.add_new_gvn_pass();

    let common_limit = 20;
    let rare_limit = 10;

    for _ in 0..rare_limit {
        for _ in 0..common_limit {
            if !common_passes.run_on(&compiler.module) {
                break;
            }
        }
        if !rare_passes.run_on(&compiler.module) {
            break;
        }
    }

    let bitcode_filepath = "beta_main.bc";
    std::io::stdout()
        .write_all(compiler.module.to_string().as_bytes())
        .unwrap();
    match compiler.module.verify() {
        Ok(_) => {}
        Err(str) => {
            panic!("{}", str.to_string());
        }
    };
    compiler
        .module
        .write_bitcode_to_path(Path::new(bitcode_filepath));

    let clang_output = Command::new("clang-16")
        .args([bitcode_filepath, "-o", "beta_main"])
        .output()
        .expect("clang not found");
    assert!(clang_output.status.success(), "clang-16 failed");

    let program_output = Command::new("./beta_main")
        .output()
        .expect("program not found");
    assert_eq!(program_output.status.code(), Some(27), "program failed");
}

pub fn main() {
    println!("due to current lack of ability to exhibit side-effects, source files must define a \
        global named `twenty_seven` equal to the Church numeral for twenty-seven, this will be \
        used to evaluate the value `27` at runtime and this will be returned from `main`"
    );
    println!("enter filepath: ");
    let mut path = String::new();
    std::io::stdin().read_line(&mut path).unwrap();
    path.pop(); // remove newline character
    let source = read_to_string(path).unwrap();
    main_helper(&source);
}
