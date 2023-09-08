use crate::ir_gen::build::CompilerWithBuilder;
use crate::ir_gen::values::{BytePtrValue, CapturesPtrValue};
use inkwell::module::Linkage;
use inkwell::types::FunctionType;
use inkwell::values::{BasicValue, FunctionValue};

#[derive(Copy, Clone)]
pub struct FunctionWithArgs<'ctx, Args> {
    pub function: FunctionValue<'ctx>,
    pub args: Args,
}

#[derive(Copy, Clone)]
pub struct SizeFnArgs<'ctx> {
    pub captures_ptr: CapturesPtrValue<'ctx>,
    pub value_ptr: BytePtrValue<'ctx>,
}

#[derive(Copy, Clone)]
pub struct CloneFnArgs<'ctx> {
    pub captures_ptr: CapturesPtrValue<'ctx>,
    pub src_ptr: BytePtrValue<'ctx>,
    pub dest_ptr: BytePtrValue<'ctx>,
}

#[derive(Copy, Clone)]
pub struct DestroyFnArgs<'ctx> {
    pub captures_ptr: CapturesPtrValue<'ctx>,
    pub value_ptr: BytePtrValue<'ctx>,
}

#[derive(Copy, Clone)]
pub struct CapturesCloneFnArgs<'ctx> {
    pub captures_ptr: CapturesPtrValue<'ctx>,
    pub dest_ptr: BytePtrValue<'ctx>,
}

#[derive(Copy, Clone)]
pub struct CapturesDestroyFnArgs<'ctx> {
    pub captures_ptr: CapturesPtrValue<'ctx>,
}

#[derive(Copy, Clone)]
pub struct RawFnArgs<'ctx> {
    pub captures_ptr: CapturesPtrValue<'ctx>,
    pub arg_ptr: BytePtrValue<'ctx>,
    pub ret_ptr: BytePtrValue<'ctx>,
}

#[derive(Copy, Clone)]
pub struct RetSizeFnArgs<'ctx> {
    pub captures_ptr: CapturesPtrValue<'ctx>,
    pub arg_ptr: BytePtrValue<'ctx>,
}

impl<'ctx> CompilerWithBuilder<'ctx, '_> {
    pub fn add_size_function(&mut self, name: &str) -> FunctionWithArgs<'ctx, SizeFnArgs<'ctx>> {
        let size_fn = self.add_function(name, self.compiler.size_fn_type());

        let captures_ptr = CapturesPtrValue(size_fn.get_nth_param(0).unwrap().into_pointer_value());
        let value_ptr = BytePtrValue(size_fn.get_nth_param(1).unwrap().into_pointer_value());

        FunctionWithArgs {
            function: size_fn,
            args: SizeFnArgs {
                captures_ptr,
                value_ptr,
            },
        }
    }
    pub fn add_clone_function(&mut self, name: &str) -> FunctionWithArgs<'ctx, CloneFnArgs<'ctx>> {
        let size_fn = self.add_function(name, self.compiler.clone_fn_type());

        let captures_ptr = CapturesPtrValue(size_fn.get_nth_param(0).unwrap().into_pointer_value());
        let src_ptr = BytePtrValue(size_fn.get_nth_param(1).unwrap().into_pointer_value());
        let dest_ptr = BytePtrValue(size_fn.get_nth_param(2).unwrap().into_pointer_value());

        FunctionWithArgs {
            function: size_fn,
            args: CloneFnArgs {
                captures_ptr,
                src_ptr,
                dest_ptr,
            },
        }
    }
    pub fn add_destroy_function(
        &mut self,
        name: &str,
    ) -> FunctionWithArgs<'ctx, DestroyFnArgs<'ctx>> {
        let size_fn = self.add_function(name, self.compiler.destroy_fn_type());

        let captures_ptr = CapturesPtrValue(size_fn.get_nth_param(0).unwrap().into_pointer_value());
        let value_ptr = BytePtrValue(size_fn.get_nth_param(1).unwrap().into_pointer_value());

        FunctionWithArgs {
            function: size_fn,
            args: DestroyFnArgs {
                captures_ptr,
                value_ptr,
            },
        }
    }
    pub fn add_captures_clone_function(
        &mut self,
        name: &str,
    ) -> FunctionWithArgs<'ctx, CapturesCloneFnArgs<'ctx>> {
        let size_fn = self.add_function(name, self.compiler.captures_clone_fn_type());

        let captures_ptr = CapturesPtrValue(size_fn.get_nth_param(0).unwrap().into_pointer_value());
        let dest_ptr = BytePtrValue(size_fn.get_nth_param(1).unwrap().into_pointer_value());

        FunctionWithArgs {
            function: size_fn,
            args: CapturesCloneFnArgs {
                captures_ptr,
                dest_ptr,
            },
        }
    }
    pub fn add_captures_destroy_function(
        &mut self,
        name: &str,
    ) -> FunctionWithArgs<'ctx, CapturesDestroyFnArgs<'ctx>> {
        let size_fn = self.add_function(name, self.compiler.captures_destroy_fn_type());

        let captures_ptr = CapturesPtrValue(size_fn.get_nth_param(0).unwrap().into_pointer_value());

        FunctionWithArgs {
            function: size_fn,
            args: CapturesDestroyFnArgs { captures_ptr },
        }
    }
    pub fn add_raw_function(&mut self, name: &str) -> FunctionWithArgs<'ctx, RawFnArgs<'ctx>> {
        let size_fn = self.add_function(name, self.compiler.raw_fn_type());

        let captures_ptr = CapturesPtrValue(size_fn.get_nth_param(0).unwrap().into_pointer_value());
        let arg_ptr = BytePtrValue(size_fn.get_nth_param(1).unwrap().into_pointer_value());
        let ret_ptr = BytePtrValue(size_fn.get_nth_param(2).unwrap().into_pointer_value());

        FunctionWithArgs {
            function: size_fn,
            args: RawFnArgs {
                captures_ptr,
                arg_ptr,
                ret_ptr,
            },
        }
    }
    pub fn add_ret_size_function(
        &mut self,
        name: &str,
    ) -> FunctionWithArgs<'ctx, RetSizeFnArgs<'ctx>> {
        let size_fn = self.add_function(name, self.compiler.ret_size_fn_type());

        let captures_ptr = CapturesPtrValue(size_fn.get_nth_param(0).unwrap().into_pointer_value());
        let arg_ptr = BytePtrValue(size_fn.get_nth_param(1).unwrap().into_pointer_value());

        FunctionWithArgs {
            function: size_fn,
            args: RetSizeFnArgs {
                captures_ptr,
                arg_ptr,
            },
        }
    }
    pub fn add_const_function(
        &mut self,
        name: &str,
        type_: FunctionType<'ctx>,
        value: Option<&dyn BasicValue<'ctx>>,
    ) -> FunctionValue<'ctx> {
        let function = self.add_function(name, type_);
        self.builder.build_return(value);
        function
    }
    fn add_function(&mut self, name: &str, type_: FunctionType<'ctx>) -> FunctionValue<'ctx> {
        let function = self
            .compiler
            .module
            .add_function(name, type_, Some(Linkage::Private));
        let entry = self.compiler.context.append_basic_block(function, "");
        self.builder.position_at_end(entry);
        function
    }
}
