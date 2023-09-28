//! Type and functions for providing the main command.

use crate::commands::Arguments;
use crate::commands::OptLevel;
use crate::ir_gen::values::captures::{Captures, Environment};
use crate::ir_gen::values::tags::structure::Structure;
use crate::ir_gen::values::tags::tag;
use crate::ir_gen::values::Location;
use crate::ir_gen::Compiler;
use crate::parser::cst;
use crate::typing::abstraction::abstract_file;
use crate::typing::ast::File;
use crate::typing::checking::synth_type;
use crate::typing::environments::Definitions;
use crate::typing::evaluation::Evaluate;
use crate::typing::expression::TypedExpression;
use crate::typing::read_back::read_back_with_ctx_len;
use annotate_snippets::snippet::{Slice, SourceAnnotation};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use std::ffi::OsStr;
use std::fs::read_to_string;
use std::path::PathBuf;
use std::process::Command;

mod lex;
mod parse;

/// The arguments to [`main`](crate::main).
pub struct MainArguments {
    pub source: String,
    pub args: Arguments,
}

impl MainArguments {
    /// Creates `self` from the command line arguments by reading source from the input path.
    pub fn new(args: Arguments) -> Result<MainArguments, String> {
        let source = read_to_string(&args.path).map_err(|e| e.to_string())?;
        Ok(MainArguments { source, args })
    }

    /// Runs the compiler on the given arguments.
    ///
    /// This is called from [`main`](crate::main).
    pub fn run(self) -> Result<(), String> {
        let tokens = self.lex()?;

        let file = self.parse(&tokens)?;

        let file = self.abstract_file(&file)?;

        let mut defs = self.type_check(&file)?;

        let context = Context::create();
        let mut compiler = self.compile(&context, &file, &mut defs);

        self.add_main(&mut compiler)?;

        self.emit_llvm(&compiler.module, &self.args.emit_un_opt_llvm)?;

        self.optimize(&compiler.module);

        self.emit_llvm(&compiler.module, &self.args.emit_llvm)?;

        self.compile_to_machine_code(&compiler.module)?;

        verbose_println!(self.args, "succeeded");

        Ok(())
    }

    fn abstract_file<'a>(&self, file: &cst::File<'a>) -> Result<File<'a>, String> {
        let file = match abstract_file(file) {
            Ok(file) => file,
            Err(errors) => {
                return Err(format!("name errors: {:#?}", errors));
            }
        };
        verbose_println!(self.args, "abstracted file");
        Ok(file)
    }
    fn type_check<'a>(&self, file: &File<'a>) -> Result<Definitions<'a>, String> {
        let mut defs = Definitions::default();
        for (name, expr) in &file.globals {
            let typed_expr = match synth_type(&mut defs.with_empty_ctx(), expr) {
                Ok(type_) => type_,
                Err(error) => {
                    return Err(format!("type error: {:#?}\n\n", error));
                }
            };
            if !defs.all_metas_defined() {
                return Err(format!(
                    "could not deduce values for some meta-variables in `{name}`"
                ));
            }
            let value = typed_expr.evaluate(defs.with_empty_env());
            let type_expr = read_back_with_ctx_len(&defs, 0, value.get_type());
            let value_expr = read_back_with_ctx_len(&defs, 0, value.get_term());
            verbose_println!(self.args, "{name} = {} as {}", value_expr, type_expr);
            let type_ = type_expr.evaluate(defs.with_empty_env());
            let value =
                TypedExpression::create_typed(type_, value_expr).evaluate(defs.with_empty_env());
            defs.define_global(name, value);
            defs.reset_metas();
        }
        verbose_println!(self.args, "type-checked file");
        Ok(defs)
    }
    fn compile<'a, 'ctx>(
        &self,
        context: &'ctx Context,
        file: &File<'a>,
        defs: &mut Definitions<'a>,
    ) -> Compiler<'ctx> {
        let mut compiler = Compiler::new(context, "main");
        for (name, _) in &file.globals {
            let typed_value = defs.lookup_global(name).clone();
            compiler.compile_global(defs, name, &typed_value);
        }
        self.verify_module(&compiler.module);
        verbose_println!(self.args, "compiled file to LLVM IR");
        compiler
    }
    fn add_main(&self, compiler: &mut Compiler) -> Result<(), String> {
        let u32_type = compiler.i32_type(); // LLVM IR has i32 = u32
        let u32_size = compiler.size_of_int_type(u32_type);
        let (size_fn, clone_fn) = compiler.add_trivial_type_special_members(u32_size);
        let u32_initializer = compiler.compile_universe_without_captures(
            size_fn,
            clone_fn,
            compiler.get_noop_fn(tag::DestroyFn),
        );
        let u32_universe_ptr = compiler
            .add_global("u32", &u32_initializer)
            .as_ptr_to(tag::Universe);

        let mut increment = compiler.add_function(tag::RawFn, "");
        let lhs = increment
            .compiler_builder
            .build_load(u32_type, increment.args.arg_ptr)
            .into_int_value();
        let value = increment.compiler_builder.builder.build_int_nuw_add(
            lhs,
            u32_type.const_int(1, false),
            "",
        );
        increment
            .compiler_builder
            .build_store(increment.args.ret_ptr, value);
        let increment = increment.build_return_void();

        let increment_ret_size_fn = compiler.add_function(tag::RetSizeFn, "");
        let increment_ret_size_fn = increment_ret_size_fn.build_return(Some(&u32_size));

        let increment_pi = compiler.compile_pi_without_captures(increment, increment_ret_size_fn);
        let increment_ptr = compiler.add_global("increment", &increment_pi);

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
        let dtors = &mut main_fn.dtors;
        let mut env = Environment {
            captures,
            captures_ptr,
            arg_ptr,
            dtors,
        };

        let zero_i32_ptr = main_fn.compiler_builder.build_alloca(u32_size);
        let zero_i32_ptr = main_fn
            .compiler_builder
            .build_store(zero_i32_ptr, u32_type.const_zero());

        let nat_main = main_fn.compiler_builder.compiler.get_global_ptr("nat_main");
        let Some(nat_main) = nat_main else {
            return Err("missing `nat_main`".to_string())
        };
        let nat_main = nat_main.as_ptr_to(tag::Pi);

        let nat_main_i32 = main_fn
            .compiler_builder
            .build_call_pi(
                &mut env,
                pi_universe_ptr,
                nat_main,
                u32_universe_ptr.as_byte_ptr(),
                Location::Other,
            )
            .as_ptr_to(tag::Pi);

        let nat_main_increment = main_fn
            .compiler_builder
            .build_call_pi(
                &mut env,
                pi_universe_ptr,
                nat_main_i32,
                increment_ptr,
                Location::Other,
            )
            .as_ptr_to(tag::Pi);

        let nat_main_zero = main_fn.compiler_builder.build_call_pi(
            &mut env,
            u32_universe_ptr,
            nat_main_increment,
            zero_i32_ptr,
            Location::Other,
        );

        let nat_main = main_fn
            .compiler_builder
            .build_load(u32_type, nat_main_zero)
            .into_int_value();

        main_fn
            .compiler_builder
            .builder
            .build_return(Some(&nat_main));

        self.verify_module(&compiler.module);
        verbose_println!(self.args, "compiled main to LLVM IR");

        Ok(())
    }
    fn optimize(&self, module: &Module) {
        let (common_limit, rare_limit) = match self.args.opt_level {
            OptLevel::Zero => return,
            OptLevel::GlobalDeadCodeElimination => (0, 0),
            OptLevel::One => (5, 1),
            OptLevel::Two => (10, 3),
            OptLevel::Three => (20, 10),
        };

        let global_dce: PassManager<Module> = PassManager::create(());
        global_dce.add_global_dce_pass();
        global_dce.run_on(module);

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

        for _ in 0..rare_limit {
            for _ in 0..common_limit {
                if !common_passes.run_on(module) {
                    break;
                }
            }
            if !rare_passes.run_on(module) {
                break;
            }
        }
        for _ in 0..common_limit {
            if !common_passes.run_on(module) {
                break;
            }
        }

        self.verify_module(module);
        verbose_println!(self.args, "optimized program");
    }
    fn compile_to_machine_code(&self, module: &Module) -> Result<(), String> {
        let bitcode_file = tempfile::Builder::new().suffix(".bc").tempfile().unwrap();
        let bitcode_filepath = bitcode_file.path();
        module.write_bitcode_to_path(bitcode_filepath);
        let output_filepath = &self.args.output;
        let clang_output = Command::new("clang-16")
            .args([
                bitcode_filepath.as_os_str(),
                OsStr::new("-o"),
                output_filepath.as_os_str(),
            ])
            .output();
        let Ok(clang_output) = clang_output else {
            return Err("clang-16 not found".to_string())
        };
        let status = clang_output.status;
        if !status.success() {
            return Err(format!("clang-16 failed with exit status {status}"));
        }
        verbose_println!(self.args, "compiled to machine code");

        Ok(())
    }

    fn emit_llvm(&self, module: &Module, path: &Option<PathBuf>) -> Result<(), String> {
        if let Some(path) = path {
            return module.print_to_file(path).map_err(|s| s.to_string());
        }
        Ok(())
    }
    fn verify_module(&self, module: &Module) {
        module.verify().unwrap()
    }

    fn make_slice<'a>(
        &'a self,
        source: &'a str,
        line_num: usize,
        annotations: Vec<SourceAnnotation<'a>>,
    ) -> Slice<'a> {
        Slice {
            source,
            line_start: line_num + 1,
            origin: Some(self.args.path.to_str().unwrap()),
            annotations,
            fold: false,
        }
    }
}
