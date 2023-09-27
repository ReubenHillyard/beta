use crate::ir_gen::values::names::{EMPTY_CAPTURES_NAME, MEMCPY_NAME};
use crate::ir_gen::values::tags::args::{CapturesCloneFnArgs, CapturesDestroyFnArgs};
use crate::ir_gen::values::tags::builtin_type::BuiltinType;
use crate::ir_gen::values::tags::function::NoopFunction;
use crate::ir_gen::values::tags::structure::{FieldOf, Structure};
use crate::ir_gen::values::tags::tag;
use crate::ir_gen::Compiler;

impl<'ctx> Compiler<'ctx> {
    /// Initializes a given [`Compiler`] with the appropriate global values.
    pub(super) fn initialize(&mut self) {
        self.declare_function(MEMCPY_NAME, self.memcpy_type());
        self.add_noop_fn(tag::DestroyFn);
        self.add_noop_fn(tag::CapturesCloneFn);
        self.add_noop_fn(tag::CapturesDestroyFn);
        self.add_delegating_builtin(tag::Universe);
        self.add_delegating_builtin(tag::Pi);
        self.add_global(EMPTY_CAPTURES_NAME, &self.empty_captures_constant());
    }

    /// Adds a built-in type whose special members all delegate.
    ///
    /// Requires `b` is the tag of a built-in type whose:
    /// * Size function delegates to a field [`tag::TotalSize`].
    /// * Clone function copies the static part and then delegates to a field
    /// [`tag::CapturesCloneFn`].
    /// * Destroy function delegates to a field [`tag::CapturesDestroyFn`].
    fn add_delegating_builtin<B: BuiltinType>(&mut self, b: B)
    where
        B: Structure,
        tag::TotalSize: FieldOf<B, Ty = tag::Usize>,
        tag::CapturesCloneFn: FieldOf<B, Ty = tag::CapturesCloneFn>,
        tag::CapturesDestroyFn: FieldOf<B, Ty = tag::CapturesDestroyFn>,
        tag::Captures: FieldOf<B, Ty = tag::Captures>,
    {
        self.add_delegating_size_fn(b);
        self.add_delegating_clone_fn(b);
        self.add_delegating_destroy_fn(b);

        self.add_global(B::NAME, &self.compile_builtin_type(b));
    }

    /// Adds a delegating size function for a built-in type.
    ///
    /// Requires `b` is the tag of a built-in type whose size function delegates to a field
    /// [`tag::TotalSize`].
    fn add_delegating_size_fn<B: BuiltinType>(&mut self, b: B)
    where
        B: Structure,
        tag::TotalSize: FieldOf<B, Ty = tag::Usize>,
    {
        let mut function = self.add_function(tag::SizeFn, B::get_name_of(tag::SizeFn));
        let arg_ptr = function.args.arg_ptr.as_ptr_to(b);
        let total_size = function
            .compiler_builder
            .build_load_field(arg_ptr, tag::TotalSize);
        function.build_return_usize(total_size);
    }

    /// Adds a delegating clone function for a built-in type.
    ///
    /// Requires `b` is the tag of a built-in type whose clone function copies the static part and
    /// then delegates to a field [`tag::CapturesCloneFn`].
    fn add_delegating_clone_fn<B: BuiltinType>(&mut self, b: B)
    where
        B: Structure,
        tag::CapturesCloneFn: FieldOf<B, Ty = tag::CapturesCloneFn>,
        tag::Captures: FieldOf<B, Ty = tag::Captures>,
    {
        let mut function = self.add_function(tag::CloneFn, B::get_name_of(tag::CloneFn));
        let src_ptr = function.args.src_ptr.as_ptr_to(b);
        let dest_ptr = function.args.dest_ptr.as_ptr_to(b);

        function
            .compiler_builder
            .build_copy_static_part(src_ptr, dest_ptr);

        let captures_clone_function = function
            .compiler_builder
            .build_load_field(src_ptr, tag::CapturesCloneFn);
        let captures_ptr = function.compiler_builder.build_get_captures_ptr(src_ptr);
        let dest_ptr = function.compiler_builder.build_get_captures_ptr(dest_ptr);

        function.compiler_builder.build_call_fn(
            captures_clone_function,
            CapturesCloneFnArgs {
                captures_ptr,
                dest_ptr,
            },
        );

        function.build_return_void();
    }

    /// Adds a delegating destroy function for a built-in type.
    ///
    /// Requires `b` is the tag of a built-in type whose destroy function delegates to a field
    /// [`tag::CapturesDestroyFn`].
    fn add_delegating_destroy_fn<B: BuiltinType>(&mut self, b: B)
    where
        B: Structure,
        tag::CapturesDestroyFn: FieldOf<B, Ty = tag::CapturesDestroyFn>,
        tag::Captures: FieldOf<B, Ty = tag::Captures>,
    {
        let mut function = self.add_function(tag::DestroyFn, B::get_name_of(tag::DestroyFn));
        let arg_ptr = function.args.arg_ptr.as_ptr_to(b);

        let captures_destroy_function = function
            .compiler_builder
            .build_load_field(arg_ptr, tag::CapturesDestroyFn);
        let captures_ptr = function.compiler_builder.build_get_captures_ptr(arg_ptr);

        function.compiler_builder.build_call_fn(
            captures_destroy_function,
            CapturesDestroyFnArgs { captures_ptr },
        );

        function.build_return_void();
    }

    /// Adds a function that immediately returns.
    fn add_noop_fn<F: NoopFunction>(&mut self, _func_type: F) {
        let type_ = F::fn_type(self);
        self.add_const_function(F::NOOP_NAME, type_, None);
    }
}
