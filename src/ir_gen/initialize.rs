use crate::ir_gen::build::CompilerWithBuilder;
use crate::ir_gen::compile::Compiler;
use crate::ir_gen::functions::{CloneFnArgs, DestroyFnArgs, FunctionWithArgs, SizeFnArgs};
use inkwell::values::CallableValue;

impl<'ctx> Compiler<'ctx> {
    pub(super) fn initialize(&mut self) {
        self.add_noop_captures_clone_fn();
        self.add_noop_captures_destroy_fn();
        self.add_universe();
        self.add_pi();
    }
    fn add_universe(&mut self) {
        self.add_universe_size_fn();
        self.add_universe_clone_fn();
        self.add_universe_destroy_fn();

        self.add_global("Type", self.universe_constant());
    }
    fn add_pi(&mut self) {
        self.add_pi_size_fn();
        self.add_pi_clone_fn();
        self.add_pi_destroy_fn();

        self.add_global("Pi", self.pi_constant());
    }
    fn add_universe_size_fn(&mut self) {
        let mut builder = self.context.create_builder();
        let FunctionWithArgs {
            args: SizeFnArgs { value_ptr, .. },
            ..
        } = CompilerWithBuilder {
            compiler: self,
            builder: &mut builder,
        }
            .add_size_function("universe_size");

        let value_ptr = builder.build_pointer_cast(value_ptr.0, self.universe_ptr_type(), "");
        // get argument pointer of correct type
        let size_ptr = builder.build_struct_gep(value_ptr, 3, "").unwrap();
        // get pointer to size
        let size = builder.build_load(size_ptr, "");
        // get size
        builder.build_return(Some(&size));
        // return size of value
    }
    fn add_universe_clone_fn(&mut self) {
        let mut builder = self.context.create_builder();
        let FunctionWithArgs {
            args: CloneFnArgs {
                src_ptr, dest_ptr, ..
            },
            ..
        } = CompilerWithBuilder {
            compiler: self,
            builder: &mut builder,
        }
            .add_clone_function("universe_clone");

        let src_ptr = builder.build_pointer_cast(src_ptr.0, self.universe_ptr_type(), "");
        // get source pointer of correct type
        let src_captures_clone_ptr = builder.build_struct_gep(src_ptr, 4, "").unwrap();
        let src_captures_clone = builder
            .build_load(src_captures_clone_ptr, "")
            .into_pointer_value();
        let src_captures_clone = CallableValue::try_from(src_captures_clone).unwrap();
        // get pointer to source captures clone function
        let src_captures_ptr = builder.build_struct_gep(src_ptr, 6, "").unwrap();
        let src_captures_ptr =
            builder.build_pointer_cast(src_captures_ptr, self.byte_ptr_type(), "");
        // get pointer to source captures

        let dest_ptr = builder.build_pointer_cast(dest_ptr.0, self.universe_ptr_type(), "");
        // get destination pointer of correct type
        let dest_captures_ptr = builder.build_struct_gep(dest_ptr, 6, "").unwrap();
        let dest_captures_ptr =
            builder.build_pointer_cast(dest_captures_ptr, self.byte_ptr_type(), "");
        // get pointer to destination captures

        let src_without_captures = builder.build_load(src_ptr, "");
        // load non-captures part of source
        builder.build_store(dest_ptr, src_without_captures);
        // store as non-captures part of destination
        builder.build_call(
            src_captures_clone,
            &[src_captures_ptr.into(), dest_captures_ptr.into()],
            "",
        );
        // clone captures

        builder.build_return(None);
    }
    fn add_universe_destroy_fn(&mut self) {
        let mut builder = self.context.create_builder();
        let FunctionWithArgs {
            args: DestroyFnArgs { value_ptr, .. },
            ..
        } = CompilerWithBuilder {
            compiler: self,
            builder: &mut builder,
        }
            .add_destroy_function("universe_destroy");

        let value_ptr = builder.build_pointer_cast(value_ptr.0, self.universe_ptr_type(), "");
        // get argument pointer of correct type
        let arg_captures_destroy_ptr = builder.build_struct_gep(value_ptr, 5, "").unwrap();
        let arg_captures_destroy = builder
            .build_load(arg_captures_destroy_ptr, "")
            .into_pointer_value();
        let arg_captures_destroy = CallableValue::try_from(arg_captures_destroy).unwrap();
        // get pointer to argument captures destroy function
        let arg_captures_ptr = builder.build_struct_gep(value_ptr, 6, "").unwrap();
        let arg_captures_ptr =
            builder.build_pointer_cast(arg_captures_ptr, self.byte_ptr_type(), "");
        // get pointer to argument captures

        builder.build_call(arg_captures_destroy, &[arg_captures_ptr.into()], "");
        // destroy captures

        builder.build_return(None);
    }
    fn add_pi_size_fn(&mut self) {
        let mut builder = self.context.create_builder();
        let FunctionWithArgs {
            args: SizeFnArgs { value_ptr, .. },
            ..
        } = CompilerWithBuilder {
            compiler: self,
            builder: &mut builder,
        }
            .add_size_function("pi_size");

        let value_ptr = builder.build_pointer_cast(value_ptr.0, self.pi_ptr_type(), "");
        // get argument pointer of correct type
        let size_ptr = builder.build_struct_gep(value_ptr, 2, "").unwrap();
        // get pointer to size
        let size = builder.build_load(size_ptr, "");
        // get size
        builder.build_return(Some(&size));
        // return size of value
    }
    fn add_pi_clone_fn(&mut self) {
        let mut builder = self.context.create_builder();
        let FunctionWithArgs {
            args: CloneFnArgs {
                src_ptr, dest_ptr, ..
            },
            ..
        } = CompilerWithBuilder {
            compiler: self,
            builder: &mut builder,
        }
            .add_clone_function("pi_clone");

        let src_ptr = builder.build_pointer_cast(src_ptr.0, self.pi_ptr_type(), "");
        // get source pointer of correct type
        let src_captures_clone_ptr = builder.build_struct_gep(src_ptr, 3, "").unwrap();
        let src_captures_clone = builder
            .build_load(src_captures_clone_ptr, "")
            .into_pointer_value();
        let src_captures_clone = CallableValue::try_from(src_captures_clone).unwrap();
        // get pointer to source captures clone function
        let src_captures_ptr = builder.build_struct_gep(src_ptr, 5, "").unwrap();
        let src_captures_ptr =
            builder.build_pointer_cast(src_captures_ptr, self.byte_ptr_type(), "");
        // get pointer to source captures

        let dest_ptr = builder.build_pointer_cast(dest_ptr.0, self.pi_ptr_type(), "");
        // get destination pointer of correct type
        let dest_captures_ptr = builder.build_struct_gep(dest_ptr, 5, "").unwrap();
        let dest_captures_ptr =
            builder.build_pointer_cast(dest_captures_ptr, self.byte_ptr_type(), "");
        // get pointer to destination captures

        let src_without_captures = builder.build_load(src_ptr, "");
        // load non-captures part of source
        builder.build_store(dest_ptr, src_without_captures);
        // store as non-captures part of destination
        builder.build_call(
            src_captures_clone,
            &[src_captures_ptr.into(), dest_captures_ptr.into()],
            "",
        );
        // clone captures

        builder.build_return(None);
    }
    fn add_pi_destroy_fn(&mut self) {
        let mut builder = self.context.create_builder();
        let FunctionWithArgs {
            args: DestroyFnArgs { value_ptr, .. },
            ..
        } = CompilerWithBuilder {
            compiler: self,
            builder: &mut builder,
        }
            .add_destroy_function("pi_destroy");

        let value_ptr = builder.build_pointer_cast(value_ptr.0, self.pi_ptr_type(), "");
        // get argument pointer of correct type
        let arg_captures_destroy_ptr = builder.build_struct_gep(value_ptr, 4, "").unwrap();
        let arg_captures_destroy = builder
            .build_load(arg_captures_destroy_ptr, "")
            .into_pointer_value();
        let arg_captures_destroy = CallableValue::try_from(arg_captures_destroy).unwrap();
        // get pointer to argument captures destroy function
        let arg_captures_ptr = builder.build_struct_gep(value_ptr, 5, "").unwrap();
        let arg_captures_ptr =
            builder.build_pointer_cast(arg_captures_ptr, self.byte_ptr_type(), "");
        // get pointer to argument captures

        builder.build_call(arg_captures_destroy, &[arg_captures_ptr.into()], "");
        // destroy captures

        builder.build_return(None);
    }
    fn add_noop_captures_clone_fn(&mut self) {
        let type_ = self.captures_clone_fn_type();
        let mut builder = self.context.create_builder();
        CompilerWithBuilder {
            compiler: self,
            builder: &mut builder,
        }
            .add_const_function("noop_captures_clone", type_, None);
    }
    fn add_noop_captures_destroy_fn(&mut self) {
        let type_ = self.captures_destroy_fn_type();
        let mut builder = self.context.create_builder();
        CompilerWithBuilder {
            compiler: self,
            builder: &mut builder,
        }
            .add_const_function("noop_captures_destroy", type_, None);
    }
}
