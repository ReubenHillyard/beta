use crate::ir_gen::compile::Compiler;
use crate::ir_gen::functions::{RawFnArgs, RetSizeFnArgs};
use crate::ir_gen::lambda::Lambda;
use crate::ir_gen::values::{BytePtrValue, PiPtrValue};
use crate::ir_gen::Location;
use crate::typing::ast::EVariable;
use crate::typing::checking::type_var;
use crate::typing::environments::{level_to_index_with_ctx_len, DefsWithCtx};
use crate::typing::value::{Neutral, Principal, Type, TypedValue, VVariable, Value};
use inkwell::builder::Builder;
use inkwell::values::{CallableValue, IntValue, PointerValue};

pub struct CompilerWithBuilder<'ctx, 'b> {
    pub compiler: &'b mut Compiler<'ctx>,
    pub builder: &'b mut Builder<'ctx>,
}

impl<'ctx> CompilerWithBuilder<'ctx, '_> {
    pub(crate) fn build_value<'a>(
        &mut self,
        defs_ctx: &mut DefsWithCtx<'a, '_>,
        function: &Lambda<'ctx>,
        typed_value: &TypedValue<'a>,
        at: Location<'ctx>,
    ) -> PointerValue<'ctx> {
        match typed_value.get_term() {
            Value::PiType { .. } => {
                let dest = match at {
                    Location::Within(ptr) => ptr,
                    Location::Alloca => {
                        self.build_alloca(self.compiler.pi_type().size_of().unwrap())
                    }
                    Location::Arg => todo!(),
                };
                let dest = self
                    .builder
                    .build_pointer_cast(dest, self.compiler.pi_ptr_type(), "");
                self.builder.build_store(dest, self.compiler.pi_constant());
                dest
            }
            Value::Lambda { closure } => {
                let Lambda { .. } =
                    self.compiler
                        .compile_lambda(defs_ctx, typed_value.get_type(), closure);
                todo!()
            }
            Value::Universe => {
                let dest = match at {
                    Location::Within(ptr) => ptr,
                    Location::Alloca => {
                        self.build_alloca(self.compiler.universe_type().size_of().unwrap())
                    }
                    Location::Arg => todo!(),
                };
                let dest =
                    self.builder
                        .build_pointer_cast(dest, self.compiler.universe_ptr_type(), "");
                self.builder
                    .build_store(dest, self.compiler.universe_constant());
                dest
            }
            Value::Neutral(neu) => self.build_neutral(defs_ctx, function, neu, at).0,
        }
    }
    fn build_neutral<'a>(
        &mut self,
        defs_ctx: &mut DefsWithCtx<'a, '_>,
        function: &Lambda<'ctx>,
        neu: &Neutral<'a>,
        at: Location<'ctx>,
    ) -> (PointerValue<'ctx>, Type<'a>) {
        match neu {
            Neutral::Principal(p) => {
                let Principal::Variable(var) = p else {
                    panic!("meta-variables should all have been solved")
                };
                let src = self.build_var_ptr(function, *var);
                let var = match var {
                    VVariable::Global(name) => EVariable::Global(name),
                    VVariable::Local(level) => {
                        EVariable::Local(level_to_index_with_ctx_len(defs_ctx.ctx.len(), *level))
                    }
                };
                let type_ = type_var(defs_ctx, var);
                let type_ptr = self.build_value(
                    defs_ctx,
                    function,
                    &type_.clone().into_typed(),
                    Location::Alloca,
                );
                let dest = match at {
                    Location::Within(ptr) => ptr,
                    Location::Alloca => {
                        let size = self.build_sizeof(type_ptr, src);
                        self.build_alloca(size)
                    }
                    Location::Arg => return (src, type_),
                };
                self.build_clone(type_ptr, src, dest);
                (dest, type_)
            }
            Neutral::Application { func, arg } => {
                let (func, func_type) =
                    self.build_neutral(defs_ctx, function, func, Location::Alloca);
                let Value::PiType { param_type, tclosure } = func_type.wrapped() else {
                    panic!("ill-typed application")
                };
                let c_arg = self.build_value(
                    defs_ctx,
                    function,
                    &TypedValue::create_typed((**param_type).clone(), (**arg).clone()),
                    Location::Alloca,
                );
                let func = self
                    .builder
                    .build_pointer_cast(func, self.compiler.pi_ptr_type(), "");
                let func = PiPtrValue(func);
                let raw_fn = self.build_get_pi_raw_function(func);
                let data_ptr = self.build_get_pi_captures_ptr(func);
                let dest = match at {
                    Location::Within(ptr) => ptr,
                    Location::Alloca | Location::Arg => {
                        let ret_size_fn = self.build_get_pi_ret_size_function(func);
                        let size = self.build_call_ret_size_function(
                            &ret_size_fn,
                            RetSizeFnArgs {
                                captures_ptr: data_ptr,
                                arg_ptr: BytePtrValue(c_arg),
                            },
                        );
                        self.build_alloca(size)
                    }
                };
                self.build_call_raw_function(
                    &raw_fn,
                    RawFnArgs {
                        captures_ptr: data_ptr,
                        arg_ptr: BytePtrValue(c_arg),
                        ret_ptr: BytePtrValue(dest),
                    },
                );
                (dest, tclosure.call(defs_ctx.defs, arg))
            }
        }
    }
    fn build_var_ptr(&self, function: &Lambda<'ctx>, var: VVariable) -> PointerValue<'ctx> {
        use VVariable::*;
        match var {
            Global(name) => self
                .compiler
                .module
                .get_global(name)
                .unwrap()
                .as_pointer_value()
                .const_cast(self.compiler.byte_ptr_type()),
            Local(level) => {
                /*let index = index.get_inner();
                if index == 0 {
                    return function
                        .function
                        .get_nth_param(2)
                        .unwrap()
                        .into_pointer_value();
                }
                let index = index - 1;
                let pos_in_captures = function.pos_in_captures(index).unwrap();
                let captures = function
                    .function
                    .get_nth_param(1)
                    .unwrap()
                    .into_pointer_value();
                let captures_offsets = self.builder.build_pointer_cast(
                    captures,
                    self.compiler.usize_type().ptr_type(Default::default()),
                    "",
                );
                let offset_ptr = unsafe {
                    self.builder.build_gep(
                        captures_offsets,
                        &[self
                            .compiler
                            .usize_type()
                            .const_int(pos_in_captures as u64, false)],
                        "",
                    )
                };
                let offset = self.builder.build_load(offset_ptr, "").into_int_value();
                unsafe { self.builder.build_gep(captures, &[offset], "") }*/
                todo!()
            }
        }
    }
    fn build_clone(
        &self,
        type_: PointerValue<'ctx>,
        src: PointerValue<'ctx>,
        dest: PointerValue<'ctx>,
    ) {
        let type_ = self
            .builder
            .build_pointer_cast(type_, self.compiler.universe_ptr_type(), "");
        let table_ptr_ptr = self.builder.build_struct_gep(type_, 0, "").unwrap();
        let data_ptr = self.builder.build_struct_gep(type_, 0, "").unwrap();
        let table_ptr = self
            .builder
            .build_load(table_ptr_ptr, "")
            .into_pointer_value();
        let clone_fn_ptr_ptr = self.builder.build_struct_gep(table_ptr, 1, "").unwrap();
        let clone_fn_ptr = self
            .builder
            .build_load(clone_fn_ptr_ptr, "")
            .into_pointer_value();
        let clone_fn_ptr = CallableValue::try_from(clone_fn_ptr).unwrap();
        self.builder.build_call(
            clone_fn_ptr,
            &[data_ptr.into(), src.into(), dest.into()],
            "",
        );
    }
    fn build_sizeof(&self, type_: PointerValue<'ctx>, val: PointerValue<'ctx>) -> IntValue<'ctx> {
        let type_ = self
            .builder
            .build_pointer_cast(type_, self.compiler.universe_ptr_type(), "");
        let table_ptr_ptr = self.builder.build_struct_gep(type_, 0, "").unwrap();
        let data_ptr = self.builder.build_struct_gep(type_, 0, "").unwrap();
        let table_ptr = self
            .builder
            .build_load(table_ptr_ptr, "")
            .into_pointer_value();
        let size_fn_ptr_ptr = self.builder.build_struct_gep(table_ptr, 0, "").unwrap();
        let size_fn_ptr = self
            .builder
            .build_load(size_fn_ptr_ptr, "")
            .into_pointer_value();
        let size_fn_ptr = CallableValue::try_from(size_fn_ptr).unwrap();
        self.builder
            .build_call(size_fn_ptr, &[data_ptr.into(), val.into()], "")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value()
    }
    fn build_alloca(&self, size: IntValue<'ctx>) -> PointerValue<'ctx> {
        self.builder
            .build_array_alloca(self.compiler.byte_type(), size, "")
    }
    pub(crate) fn build_size_of_value<'a>(
        &mut self,
        defs_ctx: &mut DefsWithCtx<'a, '_>,
        function: &Lambda<'ctx>,
        typed_value: &TypedValue<'a>,
    ) -> IntValue<'ctx> {
        match typed_value.get_term() {
            Value::PiType { .. } => todo!(),      // size of pi-type constant
            Value::Lambda { closure } => todo!(), // constant + size of captures
            Value::Universe => todo!(),           // size of Type constant
            Value::Neutral(neu) => self.build_size_of_neutral(defs_ctx, function, neu),
        }
    }
    fn build_size_of_neutral<'a>(
        &mut self,
        defs_ctx: &mut DefsWithCtx<'a, '_>,
        function: &Lambda<'ctx>,
        neu: &Neutral<'a>,
    ) -> IntValue<'ctx> {
        match neu {
            Neutral::Principal(p) => {
                let Principal::Variable(var) = p else {
                    panic!("meta-variables should all have been solved")
                };
                let src = self.build_var_ptr(function, *var);
                let var = match var {
                    VVariable::Global(name) => EVariable::Global(name),
                    VVariable::Local(level) => {
                        EVariable::Local(level_to_index_with_ctx_len(defs_ctx.ctx.len(), *level))
                    }
                };
                let type_ = type_var(defs_ctx, var);
                let type_ptr = self.build_value(
                    defs_ctx,
                    function,
                    &type_.clone().into_typed(),
                    Location::Alloca,
                );
                self.build_sizeof(type_ptr, src)
            }
            Neutral::Application { func, arg } => {
                let (func, func_type) =
                    self.build_neutral(defs_ctx, function, func, Location::Alloca);
                let Value::PiType { param_type, .. } = func_type.wrapped() else {
                    panic!("ill-typed application")
                };
                let c_arg = self.build_value(
                    defs_ctx,
                    function,
                    &TypedValue::create_typed((**param_type).clone(), (**arg).clone()),
                    Location::Alloca,
                );
                // let func = self.builder.build_pointer_cast(func, ..., "");
                let data_ptr = self.builder.build_struct_gep(func, 3, "").unwrap();
                let size_fn_ptr_ptr = self.builder.build_struct_gep(func, 1, "").unwrap();
                let size_fn_ptr = self
                    .builder
                    .build_load(size_fn_ptr_ptr, "")
                    .into_pointer_value();
                let size_fn_ptr = CallableValue::try_from(size_fn_ptr).unwrap();
                self.builder
                    .build_call(size_fn_ptr, &[data_ptr.into(), c_arg.into()], "")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value()
            }
        }
    }
}
