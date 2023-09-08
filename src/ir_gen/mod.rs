//! Types and functions for generating LLVM IR.

use crate::ir_gen::build::CompilerWithBuilder;
use inkwell::basic_block::BasicBlock;
use inkwell::context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::targets::TargetMachine;

pub mod build;
#[doc(hidden)]
pub mod compile;
#[doc(hidden)]
mod initialize;
pub mod lambda;
#[cfg(test)]
pub mod tests;
#[doc(hidden)]
pub mod types;
pub mod values;

/// The data required for compilation.
pub struct Compiler<'ctx> {
    pub(crate) context: &'ctx context::Context,
    pub(crate) module: Module<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    /// Creates a [`Compiler`] from a [`context::Context`].
    ///
    /// The [`context::Context`] must be created in a higher scope for lifetime reasons.
    ///
    /// Use like:
    ///
    /// ```
    /// let context = Context::create();
    /// let mut compiler = Compiler::new(&context, "main");
    /// ```
    pub fn new(context: &'ctx context::Context, name: &str) -> Compiler<'ctx> {
        let module = context.create_module(name);

        let default_triple = TargetMachine::get_default_triple();
        let execution_engine = module.create_execution_engine().unwrap();
        let target_data = execution_engine.get_target_data();
        let data_layout = target_data.get_data_layout();
        module.set_triple(&default_triple);
        module.set_data_layout(&data_layout);

        let mut compiler = Compiler {
            context,
            module,
            execution_engine,
        };
        compiler.initialize();
        compiler
    }

    /// Adjoins a [`Builder`](inkwell::builder::Builder) positioned at a given [`BasicBlock`] to
    /// make a [`CompilerWithBuilder`].
    pub fn with_builder_at<'b>(
        &'b self,
        basic_block: BasicBlock<'ctx>,
    ) -> CompilerWithBuilder<'ctx, 'b> {
        let builder = self.context.create_builder();
        builder.position_at_end(basic_block);
        CompilerWithBuilder {
            compiler: self,
            builder,
        }
    }
}
