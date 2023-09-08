//! Type and functions for building lambda functions.

use crate::ir_gen::values::captures::{Captures, Environment};
use crate::ir_gen::values::tags::tag;
use crate::ir_gen::values::FnValue;
use crate::ir_gen::values::Location;
use crate::ir_gen::Compiler;
use crate::typing::checking::type_level;
use crate::typing::environments::DefsWithCtx;
use crate::typing::expression::CoreExpression;
use crate::typing::value::{Closure, Type, TypedValue, Value};

/// The data required to compile or build a lambda.
pub struct Lambda<'ctx> {
    pub raw_function: FnValue<'ctx, tag::RawFn>,
    pub ret_size_function: FnValue<'ctx, tag::RetSizeFn>,
    pub captures_clone_function: FnValue<'ctx, tag::CapturesCloneFn>,
    pub captures_destroy_function: FnValue<'ctx, tag::CapturesDestroyFn>,
    pub captures: Captures,
}

impl<'ctx> Compiler<'ctx> {
    /// Creates a [`Lambda`] with the given body.
    ///
    /// Panics if `fn_type` is not a pi type in context of `defs_ctx`.
    ///
    /// Requires `fn_type` is a valid type in context of `defs_ctx`, and `closure` is a valid body
    /// of a lambda of type `fn_type` in that context.
    pub(crate) fn compile_lambda<'a>(
        &self,
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

        let captures = Captures::from_ret_val(defs_ctx, &ret_val, &ret_type);
        let (captures_clone_function, captures_destroy_function) =
            self.compile_captures_functions(defs_ctx, &captures);

        let mut raw_function = self.add_function(tag::RawFn, "");

        let ret_val = TypedValue::create_typed(ret_type, ret_val);

        raw_function.compiler_builder.build_value(
            &mut defs_ctx.extend(param_type),
            &mut Environment {
                captures: &captures,
                captures_ptr: raw_function.args.captures_ptr,
                arg_ptr: Some(raw_function.args.arg_ptr),
                dtors: &mut raw_function.dtors,
            },
            &ret_val,
            Location::Within(raw_function.args.ret_ptr),
        );

        let raw_function = raw_function.build_return_void();

        let mut ret_size_function = self.add_function(tag::RetSizeFn, "");

        let size = ret_size_function.compiler_builder.build_size_of_value(
            &mut defs_ctx.extend(param_type),
            &mut Environment {
                captures: &captures,
                captures_ptr: ret_size_function.args.captures_ptr,
                arg_ptr: Some(ret_size_function.args.arg_ptr),
                dtors: &mut ret_size_function.dtors,
            },
            &ret_val,
        );

        let ret_size_function = ret_size_function.build_return_usize(size);

        Lambda {
            raw_function,
            ret_size_function,
            captures_clone_function,
            captures_destroy_function,
            captures,
        }
    }

    /// Creates captures functions for a [`Lambda`].
    fn compile_captures_functions(
        &self,
        defs_ctx: &mut DefsWithCtx,
        captures: &Captures,
    ) -> (
        FnValue<'ctx, tag::CapturesCloneFn>,
        FnValue<'ctx, tag::CapturesDestroyFn>,
    ) {
        let mut captures_clone_function = self.add_function(tag::CapturesCloneFn, "");
        let mut captures_destroy_function = self.add_function(tag::CapturesDestroyFn, "");

        let captures_clone_dest_captures_ptr = captures_clone_function
            .compiler_builder
            .build_copy_capture_offsets(
                captures,
                captures_clone_function.args.captures_ptr,
                captures_clone_function.args.dest_ptr,
            );

        for (n, level) in captures.iter().enumerate() {
            let type_ = type_level(defs_ctx, level).into_typed();

            let captures_clone_universe_ptr = captures_clone_function
                .compiler_builder
                .build_value(
                    defs_ctx,
                    &mut Environment {
                        captures,
                        captures_ptr: captures_clone_function.args.captures_ptr,
                        arg_ptr: None,
                        dtors: &mut captures_clone_function.dtors,
                    },
                    &type_,
                    Location::Other,
                )
                .as_ptr_to(tag::Universe);
            let captures_destroy_universe_ptr = captures_destroy_function
                .compiler_builder
                .build_value(
                    defs_ctx,
                    &mut Environment {
                        captures,
                        captures_ptr: captures_destroy_function.args.captures_ptr,
                        arg_ptr: None,
                        dtors: &mut captures_destroy_function.dtors,
                    },
                    &type_,
                    Location::Other,
                )
                .as_ptr_to(tag::Universe);

            let captures_clone_src_ptr = captures_clone_function
                .compiler_builder
                .build_get_nth_capture_ptr(captures_clone_function.args.captures_ptr, n as u64);
            let captures_clone_dest_ptr = captures_clone_function
                .compiler_builder
                .build_get_nth_capture_ptr(captures_clone_dest_captures_ptr, n as u64);
            captures_clone_function.compiler_builder.build_clone(
                captures_clone_universe_ptr,
                captures_clone_src_ptr,
                captures_clone_dest_ptr,
            );

            let captures_destroy_arg_ptr = captures_destroy_function
                .compiler_builder
                .build_get_nth_capture_ptr(captures_destroy_function.args.captures_ptr, n as u64);
            captures_destroy_function
                .compiler_builder
                .build_destroy(captures_destroy_universe_ptr, captures_destroy_arg_ptr);
        }

        let captures_clone_function = captures_clone_function.build_return_void();
        let captures_destroy_function = captures_destroy_function.build_return_void();

        (captures_clone_function, captures_destroy_function)
    }
}
