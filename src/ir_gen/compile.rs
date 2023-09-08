use crate::ir_gen::build::CompilerWithBuilder;
use crate::ir_gen::functions::{FunctionWithArgs, RawFnArgs};
use crate::ir_gen::lambda::Lambda;
use crate::ir_gen::Location;
use crate::typing::environments::{Definitions, DefsWithCtx};
use crate::typing::expression::CoreExpression;
use crate::typing::read_back::read_back_with_ctx_len;
use crate::typing::value::{Closure, Neutral, Principal, Type, TypedValue, VVariable, Value};
use inkwell::context;
use inkwell::module::{Linkage, Module};
use inkwell::values::{FunctionValue, GlobalValue, StructValue};
use std::iter;

pub struct Compiler<'ctx> {
    pub(crate) context: &'ctx context::Context,
    pub(crate) module: Module<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx context::Context, name: &str) -> Compiler<'ctx> {
        let mut compiler = Compiler {
            context,
            module: context.create_module(name),
        };
        compiler.initialize();
        compiler
    }

    pub fn compile_global<'a>(
        &mut self,
        defs: &mut Definitions<'a>,
        name: &str,
        typed_value: &TypedValue<'a>,
    ) {
        let initializer = self.compile_global_initializer(defs, typed_value);
        self.add_global(name, initializer);
    }
    pub(crate) fn add_global(
        &mut self,
        name: &str,
        initializer: StructValue<'ctx>,
    ) {
        let global = self.module.add_global(initializer.get_type(), None, name);
        global.set_initializer(&initializer);
        global.set_linkage(Linkage::Private);
        global.set_constant(true);
    }

    fn compile_global_initializer<'a>(
        &mut self,
        defs: &mut Definitions<'a>,
        typed_value: &TypedValue<'a>,
    ) -> StructValue<'ctx> {
        match typed_value.get_term() {
            Value::PiType { .. } => self.pi_constant(),
            Value::Lambda { closure } => {
                let Lambda {
                    raw_function,
                    ret_size_function,
                    ..
                } = self.compile_lambda(
                    &mut defs.with_empty_ctx(),
                    typed_value.get_type(),
                    closure,
                );
                self.context.const_struct(
                    &[
                        raw_function.as_global_value().as_pointer_value().into(),
                        ret_size_function
                            .as_global_value()
                            .as_pointer_value()
                            .into(),
                        self.pi_type().size_of().unwrap().into(),
                        self.noop_captures_clone().into(),
                        self.noop_captures_destroy().into(),
                        self.empty_captures_constant().into(),
                    ],
                    false,
                )
            }
            Value::Universe => self.universe_constant(),
            Value::Neutral(neu) => {
                let Neutral::Principal(p) = neu else {
                    panic!("global cannot be application")
                };
                let Principal::Variable(var) = p else {
                    panic!("meta-variables should all have been solved")
                };
                let VVariable::Global(name) = var else {
                    panic!("invalid value in empty context")
                };
                self.get_global_initializer(name)
            }
        }
    }

    fn get_global_initializer(&self, name: &str) -> StructValue<'ctx> {
        self.module
            .get_global(name)
            .and_then(GlobalValue::get_initializer)
            .unwrap()
            .into_struct_value()
    }

    pub(crate) fn compile_lambda<'a>(
        &mut self,
        defs_ctx: &mut DefsWithCtx<'a, '_>,
        fn_type: &Type<'a>,
        closure: &Closure<'a, CoreExpression<'a>>,
    ) -> Lambda<'ctx> {
        let Value::PiType { param_type, tclosure } = fn_type.wrapped() else {
            panic!("lambda must have pi type")
        };

        let fresh_var = defs_ctx.ctx.fresh_var();
        let ret_val = closure.call(defs_ctx.defs, &fresh_var);
        let ret_type = tclosure.call(defs_ctx.defs, &fresh_var);

        let ret_expr = read_back_with_ctx_len(defs_ctx.defs, defs_ctx.ctx.len(), &ret_val);
        let mut captures: Vec<bool> = iter::repeat(false).take(defs_ctx.ctx.len()).collect();
        ret_expr.captures_from_ret_val(&mut captures);
        let captures = captures.as_slice();
        let (captures_clone_function, captures_destroy_function) =
            self.compile_captures_functions(defs_ctx, captures);

        let mut function_builder = self.context.create_builder();
        let FunctionWithArgs {
            function,
            args:
            RawFnArgs {
                ret_ptr: function_ret_ptr,
                ..
            },
        } = CompilerWithBuilder {
            compiler: self,
            builder: &mut function_builder,
        }
            .add_raw_function("");

        let mut ret_size_function_builder = self.context.create_builder();
        let FunctionWithArgs {
            function: size_function,
            ..
        } = CompilerWithBuilder {
            compiler: self,
            builder: &mut ret_size_function_builder,
        }
            .add_ret_size_function("");

        let function = Lambda::new(
            function,
            size_function,
            captures_clone_function,
            captures_destroy_function,
            captures,
        );

        let ret_val = TypedValue::create_typed(ret_type, ret_val);

        CompilerWithBuilder {
            compiler: self,
            builder: &mut function_builder,
        }
            .build_value(
                &mut defs_ctx.extend(param_type),
                &function,
                &ret_val,
                Location::Within(function_ret_ptr.0),
            );

        function_builder.build_return(None);

        let size = CompilerWithBuilder {
            compiler: self,
            builder: &mut ret_size_function_builder,
        }
            .build_size_of_value(&mut defs_ctx.extend(param_type), &function, &ret_val);

        ret_size_function_builder.build_return(Some(&size));

        function
    }

    fn compile_captures_functions(
        &self,
        _defs_ctx: &DefsWithCtx,
        _captures: &[bool],
    ) -> (FunctionValue<'ctx>, FunctionValue<'ctx>) {
        todo!()
    }
}
