//! A type for building instructions.

use crate::ir_gen::lambda::Lambda;
use crate::ir_gen::values::captures::{Captures, Environment};
use crate::ir_gen::values::low::LoadStore;
use crate::ir_gen::values::tags::args::RetSizeFnArgs;
use crate::ir_gen::values::tags::flow::Read;
use crate::ir_gen::values::tags::structure::Structure;
use crate::ir_gen::values::tags::tag;
use crate::ir_gen::values::Location;
use crate::ir_gen::values::PtrValue;
use crate::ir_gen::Compiler;
use crate::typing::checking::{type_level, type_vv};
use crate::typing::environments::DefsWithCtx;
use crate::typing::value::{Neutral, Principal, Type, TypedValue, Value};
use inkwell::builder::Builder;
use inkwell::values::IntValue;

/// The data required for building instructions.
pub struct CompilerWithBuilder<'ctx, 'b> {
    pub compiler: &'b Compiler<'ctx>,
    pub builder: Builder<'ctx>,
}

impl<'ctx> CompilerWithBuilder<'ctx, '_> {
    /// Builds the computation of a given value in a given context and environment at a given
    /// address, or an address determined by the function.
    ///
    /// Requires `typed_value` is a valid typed value in context of `defs_ctx` and that `env` is a
    /// valid [`Environment`] for `defs_ctx`.
    pub(crate) fn build_value<'a>(
        &mut self,
        defs_ctx: &mut DefsWithCtx<'a, '_>,
        env: &mut Environment<'ctx, '_>,
        typed_value: &TypedValue<'a>,
        at: Location<'ctx>,
    ) -> PtrValue<'ctx, Read, tag::Unknown> {
        match typed_value.get_term() {
            Value::PiType { .. } => {
                let src_ptr = self.compiler.get_global_ptr(tag::Pi::NAME).unwrap();
                match at {
                    Location::Within(dest_ptr) => self
                        .build_copy_static_part(
                            src_ptr.as_ptr_to(tag::Universe),
                            dest_ptr.as_ptr_to(tag::Universe),
                        )
                        .as_byte_ptr(),
                    Location::Other => src_ptr,
                }
            }
            Value::Lambda { closure } => {
                let Lambda {
                    raw_function,
                    ret_size_function,
                    captures_clone_function,
                    captures_destroy_function,
                    captures,
                } = self
                    .compiler
                    .compile_lambda(defs_ctx, typed_value.get_type(), closure);
                let total_size = self.build_size_of_captures(
                    defs_ctx,
                    env,
                    &captures,
                    self.compiler.size_of_structure(tag::Pi),
                );
                let dest_pi_ptr = match at {
                    Location::Within(ptr) => ptr,
                    Location::Other => {
                        let ptr = self.build_alloca(total_size);
                        env.add_dtor(
                            self.compiler
                                .get_global_ptr(tag::Pi::NAME)
                                .unwrap()
                                .as_ptr_to(tag::Universe),
                            ptr,
                        );
                        ptr
                    }
                }
                .as_ptr_to(tag::Pi);
                let dest_captures_ptr = self.build_store_pi_static_part(
                    dest_pi_ptr,
                    raw_function,
                    ret_size_function,
                    total_size,
                    captures_clone_function,
                    captures_destroy_function,
                );
                let mut offset = self
                    .compiler
                    .size_of_int_type(self.compiler.usize_type())
                    .const_nuw_mul(
                        self.compiler
                            .usize_type()
                            .const_int(captures.number() as u64, false),
                    );
                for (n, level) in captures.iter().enumerate() {
                    let offset_dest_ptr =
                        self.build_get_nth_capture_offset_ptr(dest_captures_ptr, n as u64);
                    LoadStore::build_store(self, offset_dest_ptr, offset);

                    let capture_src_ptr = self.build_level_ptr(env, level);
                    let dest_captures_ptr = dest_captures_ptr.as_partial();
                    let capture_dest_ptr =
                        self.build_get_nth_capture_ptr(dest_captures_ptr, n as u64);

                    let type_ = type_level(defs_ctx, level).into_typed();
                    let universe_ptr = self
                        .build_value(defs_ctx, env, &type_, Location::Other)
                        .as_ptr_to(tag::Universe);

                    self.build_clone(universe_ptr, capture_src_ptr, capture_dest_ptr);

                    let size = self.build_sizeof(universe_ptr, capture_src_ptr);
                    offset = self.builder.build_int_nuw_add(offset, size, "");
                }
                dest_pi_ptr.as_byte_ptr().as_read()
            }
            Value::Universe => {
                let src_ptr = self.compiler.get_global_ptr(tag::Universe::NAME).unwrap();
                match at {
                    Location::Within(dest_ptr) => self
                        .build_copy_static_part(
                            src_ptr.as_ptr_to(tag::Universe),
                            dest_ptr.as_ptr_to(tag::Universe),
                        )
                        .as_byte_ptr(),
                    Location::Other => src_ptr,
                }
            }
            Value::Neutral(neu) => self.build_neutral(defs_ctx, env, neu, at).0,
        }
    }
    /// Builds the computation of a given neutral value in a given context and environment at a
    /// given address, or an address determined by the function.
    ///
    /// Requires `neu` is a valid value in context of `defs_ctx` and that `env` is a valid
    /// [`Environment`] for `defs_ctx`.
    fn build_neutral<'a>(
        &mut self,
        defs_ctx: &mut DefsWithCtx<'a, '_>,
        env: &mut Environment<'ctx, '_>,
        neu: &Neutral<'a>,
        at: Location<'ctx>,
    ) -> (PtrValue<'ctx, Read, tag::Unknown>, Type<'a>) {
        match neu {
            Neutral::Principal(p) => {
                let Principal::Variable(var) = p else {
                    panic!("meta-variables should all have been solved")
                };
                let type_ = type_vv(defs_ctx, *var);
                let src = self.build_var_ptr(env, *var);
                let dest = match at {
                    Location::Within(dest) => {
                        let universe_ptr = self
                            .build_value(
                                defs_ctx,
                                env,
                                &type_.clone().into_typed(),
                                Location::Other,
                            )
                            .as_ptr_to(tag::Universe);
                        self.build_clone(universe_ptr, src, dest)
                    }
                    Location::Other => src,
                };
                (dest, type_)
            }
            Neutral::Application { func, arg } => {
                let (func, func_type) = self.build_neutral(defs_ctx, env, func, Location::Other);
                let Value::PiType { param_type, tclosure } = func_type.wrapped() else {
                    panic!("ill-typed application")
                };
                let arg_ptr = self.build_value(
                    defs_ctx,
                    env,
                    &TypedValue::create_typed((**param_type).clone(), (**arg).clone()),
                    Location::Other,
                );
                let type_ = tclosure.call(defs_ctx.defs, arg);
                let ret_type = self
                    .build_value(defs_ctx, env, &type_.clone().into_typed(), Location::Other)
                    .as_ptr_to(tag::Universe);
                let pi_ptr = func.as_ptr_to(tag::Pi);
                let dest = self.build_call_pi(env, ret_type, pi_ptr, arg_ptr, at);
                (dest, type_)
            }
        }
    }
    /// Builds the computation of the size of a given value in a given context and environment.
    ///
    /// Requires `typed_value` is a valid typed value in context of `defs_ctx` and that `env` is a
    /// valid [`Environment`] for `defs_ctx`.
    pub(crate) fn build_size_of_value<'a>(
        &mut self,
        defs_ctx: &mut DefsWithCtx<'a, '_>,
        env: &mut Environment<'ctx, '_>,
        typed_value: &TypedValue<'a>,
    ) -> IntValue<'ctx> {
        match typed_value.get_term() {
            Value::PiType { .. } | Value::Universe => {
                self.compiler.size_of_structure(tag::Universe)
            }
            Value::Lambda { closure } => {
                let Value::PiType { tclosure, .. } = typed_value.get_type().wrapped() else {
                    panic!("lambda must have pi type")
                };
                let ret_val = closure.call(defs_ctx.defs, &defs_ctx.ctx.fresh_var());
                let ret_type = tclosure.call(defs_ctx.defs, &defs_ctx.ctx.fresh_var());
                let lambda_captures = Captures::from_ret_val(defs_ctx, &ret_val, &ret_type);
                self.build_size_of_captures(
                    defs_ctx,
                    env,
                    &lambda_captures,
                    self.compiler.size_of_structure(tag::Pi),
                )
            }
            Value::Neutral(neu) => self.build_size_of_neutral(defs_ctx, env, neu),
        }
    }
    /// Builds the computation of the size of a given neutral value in a given context and
    /// environment.
    ///
    /// Requires `neu` is a valid value in context of `defs_ctx` and that `env` is a valid
    /// [`Environment`] for `defs_ctx`.
    fn build_size_of_neutral<'a>(
        &mut self,
        defs_ctx: &mut DefsWithCtx<'a, '_>,
        env: &mut Environment<'ctx, '_>,
        neu: &Neutral<'a>,
    ) -> IntValue<'ctx> {
        match neu {
            Neutral::Principal(p) => {
                let Principal::Variable(var) = p else {
                    panic!("meta-variables should all have been solved")
                };
                let type_ = type_vv(defs_ctx, *var);
                let var_ptr = self.build_var_ptr(env, *var);
                let universe_ptr = self
                    .build_value(defs_ctx, env, &type_.clone().into_typed(), Location::Other)
                    .as_ptr_to(tag::Universe);
                self.build_sizeof(universe_ptr, var_ptr)
            }
            Neutral::Application { func, arg } => {
                let (func, func_type) = self.build_neutral(defs_ctx, env, func, Location::Other);
                let Value::PiType { param_type, .. } = func_type.wrapped() else {
                    panic!("ill-typed application")
                };
                let arg_ptr = self.build_value(
                    defs_ctx,
                    env,
                    &TypedValue::create_typed((**param_type).clone(), (**arg).clone()),
                    Location::Other,
                );
                let pi_ptr = func.as_ptr_to(tag::Pi);
                let ret_size_fn = self.build_load_field(pi_ptr, tag::RetSizeFn);
                let captures_ptr = self.build_get_captures_ptr(pi_ptr);
                self.build_call_fn(
                    ret_size_fn,
                    RetSizeFnArgs {
                        captures_ptr,
                        arg_ptr,
                    },
                )
            }
        }
    }
    /// Builds the size required for the given list of captures in a given context and environment,
    /// plus a given constant amount.
    ///
    /// Requires `captures` is a valid capture-list in context of `defs_ctx` and that `env` is a
    /// valid [`Environment`] for `defs_ctx`. Requires `add` is a constant of type
    /// [`self.compiler.usize_type()`](Compiler::usize_type).
    fn build_size_of_captures(
        &mut self,
        defs_ctx: &mut DefsWithCtx,
        env: &mut Environment<'ctx, '_>,
        captures: &Captures,
        add: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        let sizes: Vec<_> = captures
            .iter()
            .map(|level| {
                let type_ = type_level(defs_ctx, level).into_typed();
                let universe_ptr = self
                    .build_value(defs_ctx, env, &type_, Location::Other)
                    .as_ptr_to(tag::Universe);
                let arg_ptr = self.build_level_ptr(env, level);
                self.build_sizeof(universe_ptr, arg_ptr)
            })
            .collect();
        sizes.into_iter().fold(
            add.const_nuw_add(
                self.compiler
                    .size_of_int_type(self.compiler.usize_type())
                    .const_nuw_mul(
                        self.compiler
                            .usize_type()
                            .const_int(captures.number() as u64, false),
                    ),
            ),
            |lhs, rhs| self.builder.build_int_nuw_add(lhs, rhs, ""),
        )
    }
}
