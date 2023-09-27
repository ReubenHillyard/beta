//! Type and functions for building LLVM IR functions.

use crate::ir_gen::build::CompilerWithBuilder;
use crate::ir_gen::values::destructors::Destructors;
use crate::ir_gen::values::tags::args::{Args, MemcpyArgs};
use crate::ir_gen::values::tags::attributes::enum_attributes::{
    AlwaysInline, Memory, MustProgress, NoCapture, NoFree, NoRecurse, NoSync, NoUndef, NoUnwind,
    NonNull, WillReturn,
};
use crate::ir_gen::values::tags::attributes::memory_val;
use crate::ir_gen::values::tags::flow::Read;
use crate::ir_gen::values::tags::function::Function;
use crate::ir_gen::values::tags::param_flow::ParamFlow;
use crate::ir_gen::values::tags::{tag, Object};
use crate::ir_gen::values::{FnValue, PtrValue};
use crate::ir_gen::Compiler;
use inkwell::attributes::AttributeLoc;
use inkwell::module::Linkage;
use inkwell::types::{BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValue, FunctionValue, IntValue, UnnamedAddress};

/// The data required for building a function body.
pub struct FunctionWithArgs<'ctx, 'b, Func, Args, Dtors> {
    pub compiler_builder: CompilerWithBuilder<'ctx, 'b>,
    function: Func,
    pub args: Args,
    pub dtors: Dtors,
}

impl<'ctx, 'b> FunctionWithArgs<'ctx, 'b, FunctionValue<'ctx>, (), ()> {
    /// Gets the nth parameter of the given function, and adds attributes according to its type.
    pub(crate) fn get_nth_param<F: ParamFlow, O: Object>(
        &mut self,
        nth: u32,
        f: F,
        o: O,
    ) -> PtrValue<'ctx, F, O> {
        self.compiler_builder.compiler.add_enum_attribute(
            self.function,
            AttributeLoc::Param(nth),
            NoFree,
        );
        self.compiler_builder.compiler.add_enum_attribute(
            self.function,
            AttributeLoc::Param(nth),
            NonNull,
        );

        for attribute in F::ATTRIBUTES {
            self.compiler_builder.compiler.add_enum_attribute(
                self.function,
                AttributeLoc::Param(nth),
                attribute,
            )
        }

        PtrValue(
            self.function
                .get_nth_param(nth)
                .unwrap()
                .into_pointer_value(),
            f,
            o,
        )
    }

    /// Promotes a function of unknown type to a given type.
    ///
    /// Requires the function has the given type.
    fn promote<Func: Function>(
        mut self,
        func_type: Func,
    ) -> FunctionWithArgs<'ctx, 'b, FnValue<'ctx, Func>, Func::ArgsT<'ctx>, Destructors<'ctx>> {
        self.compiler_builder.compiler.add_enum_attribute(
            self.function,
            AttributeLoc::Function,
            Memory(Func::MEMORY_VAL),
        );
        let args = Func::ArgsT::from_fn(&mut self);
        FunctionWithArgs {
            compiler_builder: self.compiler_builder,
            function: FnValue(
                self.function.as_global_value().as_pointer_value(),
                func_type,
            ),
            args,
            dtors: Destructors::default(),
        }
    }

    /// Builds a return statement returning a given value.
    pub fn build_return(self, value: Option<&dyn BasicValue<'ctx>>) -> FunctionValue<'ctx> {
        self.compiler_builder.builder.build_return(value);
        self.function
    }
}

impl<'ctx, Func, Args> FunctionWithArgs<'ctx, '_, Func, Args, Destructors<'ctx>> {
    /// Builds a return statement returning a given value, after calling all necessary destructors.
    pub fn build_return(mut self, value: Option<&dyn BasicValue<'ctx>>) -> Func {
        self.dtors.build_destroy(&mut self.compiler_builder);
        self.compiler_builder.builder.build_return(value);
        self.function
    }

    /// Builds a return statement returning void, after calling all necessary destructors.
    pub fn build_return_void(self) -> Func {
        self.build_return(None)
    }

    /// Builds a return statement returning a given `usize`, after calling all necessary destructors.
    pub fn build_return_usize(self, size: IntValue<'ctx>) -> Func {
        self.build_return(Some(&size))
    }

    /// Builds a return statement returning a given `ptr`, after calling all necessary destructors.
    pub fn build_return_ptr(self, ptr: PtrValue<'ctx, Read, tag::Unknown>) -> Func {
        self.build_return(Some(&ptr.0))
    }
}

impl<'ctx> Compiler<'ctx> {
    /// Creates a function with a given type and name, and adjoins its arguments and its
    /// [`Destructors`].
    ///
    ///  Requires `name` is either a unique, valid identifier or `""`.
    pub fn add_function<'b, Func: Function>(
        &'b self,
        func_type: Func,
        name: &str,
    ) -> FunctionWithArgs<'ctx, 'b, FnValue<'ctx, Func>, Func::ArgsT<'ctx>, Destructors<'ctx>> {
        self.add_function_with_type(name, Func::fn_type(self))
            .promote(func_type)
    }

    /// Creates size and clone special members for a trivial type with a given size.
    ///
    /// Requires `size` is a constant of type [`self.compiler.usize_type()`](Compiler::usize_type).
    pub fn add_trivial_type_special_members(
        &mut self,
        size: IntValue<'ctx>,
    ) -> (FnValue<'ctx, tag::SizeFn>, FnValue<'ctx, tag::CloneFn>) {
        let size_fn = self.add_const_function("", tag::SizeFn::fn_type(self), Some(&size));
        let mut clone_fn = self.add_function(tag::CloneFn, "");
        clone_fn.compiler_builder.build_call_memcpy(MemcpyArgs {
            dest_ptr: clone_fn.args.dest_ptr,
            src_ptr: clone_fn.args.src_ptr,
            num_bytes: size,
        });
        let clone_fn = clone_fn.build_return_void();
        (
            FnValue(size_fn.as_global_value().as_pointer_value(), tag::SizeFn),
            clone_fn,
        )
    }

    /// Creates a function of a given type that immediately returns, optionally with a given value.
    ///
    /// Requires `name` is either a unique, valid identifier or `""`.
    pub fn add_const_function(
        &mut self,
        name: &str,
        type_: FunctionType<'ctx>,
        value: Option<&dyn BasicValue<'ctx>>,
    ) -> FunctionValue<'ctx> {
        let function = self.add_function_with_type(name, type_);

        function.compiler_builder.compiler.add_enum_attribute(
            function.function,
            AttributeLoc::Function,
            Memory(memory_val::NONE),
        );
        function.compiler_builder.compiler.add_enum_attribute(
            function.function,
            AttributeLoc::Function,
            NoFree,
        );
        function.compiler_builder.compiler.add_enum_attribute(
            function.function,
            AttributeLoc::Function,
            NoRecurse,
        );

        function.build_return(value)
    }

    /// Declares an externally-defined function.
    ///
    /// Requires `name` is the name of a valid externally-defined function or intrinsic.
    pub fn declare_function(
        &mut self,
        name: &str,
        type_: FunctionType<'ctx>,
    ) -> FunctionValue<'ctx> {
        self.module.add_function(name, type_, None)
    }

    /// Creates a function with a given name and type.
    ///
    /// Requires `name` is either a unique, valid identifier or `""`, and is not `"main"`.
    pub fn add_function_with_type<'b>(
        &'b self,
        name: &str,
        type_: FunctionType<'ctx>,
    ) -> FunctionWithArgs<'ctx, 'b, FunctionValue<'ctx>, (), ()> {
        let function = self
            .module
            .add_function(name, type_, Some(Linkage::Private));
        function
            .as_global_value()
            .set_unnamed_address(UnnamedAddress::Global);
        self.add_enum_attribute(function, AttributeLoc::Function, AlwaysInline);
        self.add_enum_attribute(function, AttributeLoc::Function, WillReturn);
        self.add_enum_attribute(function, AttributeLoc::Function, NoSync);
        self.add_enum_attribute(function, AttributeLoc::Function, NoUnwind);
        self.add_enum_attribute(function, AttributeLoc::Function, MustProgress);

        for nth in 0..function.count_params() {
            self.add_enum_attribute(function, AttributeLoc::Param(nth), NoUndef);
        }

        let ret_type = type_.get_return_type();
        if ret_type.is_some() {
            self.add_enum_attribute(function, AttributeLoc::Return, NoUndef);
        }
        if !ret_type.is_some_and(BasicTypeEnum::is_pointer_type) {
            for nth in 0..function.count_params() {
                self.add_enum_attribute(function, AttributeLoc::Param(nth), NoCapture);
            }
        }

        let entry = self.context.append_basic_block(function, "");
        FunctionWithArgs {
            compiler_builder: self.with_builder_at(entry),
            function,
            args: (),
            dtors: (),
        }
    }

    pub fn add_main_function<'b>(
        &'b self,
    ) -> FunctionWithArgs<'ctx, 'b, FunctionValue<'ctx>, (), Destructors<'ctx>> {
        let function = self.module.add_function("main", self.main_type(), None);
        function
            .as_global_value()
            .set_unnamed_address(UnnamedAddress::Global);

        let entry = self.context.append_basic_block(function, "");
        FunctionWithArgs {
            compiler_builder: self.with_builder_at(entry),
            function,
            args: (),
            dtors: Destructors::default(),
        }
    }
}
