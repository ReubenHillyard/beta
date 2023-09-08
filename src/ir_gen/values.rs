use crate::ir_gen::build::CompilerWithBuilder;
use crate::ir_gen::functions::{
    CapturesCloneFnArgs, CapturesDestroyFnArgs, CloneFnArgs, DestroyFnArgs, RawFnArgs,
    RetSizeFnArgs, SizeFnArgs,
};
use inkwell::values::{CallableValue, IntValue, PointerValue};

#[derive(Copy, Clone)]
pub struct BytePtrValue<'ctx>(pub(crate) PointerValue<'ctx>);

#[derive(Copy, Clone)]
pub struct SizeFnValue<'ctx>(PointerValue<'ctx>);

#[derive(Copy, Clone)]
pub struct CloneFnValue<'ctx>(PointerValue<'ctx>);

#[derive(Copy, Clone)]
pub struct DestroyFnValue<'ctx>(PointerValue<'ctx>);

#[derive(Copy, Clone)]
pub struct CapturesCloneFnValue<'ctx>(PointerValue<'ctx>);

#[derive(Copy, Clone)]
pub struct CapturesDestroyFnValue<'ctx>(PointerValue<'ctx>);

#[derive(Copy, Clone)]
pub struct CapturesPtrValue<'ctx>(pub(crate) PointerValue<'ctx>);

#[derive(Copy, Clone)]
pub struct RawFnValue<'ctx>(PointerValue<'ctx>);

#[derive(Copy, Clone)]
pub struct RetSizeFnValue<'ctx>(PointerValue<'ctx>);

#[derive(Copy, Clone)]
pub struct UniversePtrValue<'ctx>(pub(crate) PointerValue<'ctx>);

#[derive(Copy, Clone)]
pub struct PiPtrValue<'ctx>(pub(crate) PointerValue<'ctx>);

impl<'ctx> CompilerWithBuilder<'ctx, '_> {
    pub fn build_call_size_function(
        &mut self,
        size_function: &SizeFnValue<'ctx>,
        args: SizeFnArgs<'ctx>,
    ) -> IntValue<'ctx> {
        let size_function = CallableValue::try_from(size_function.0).unwrap();
        self.builder
            .build_call(
                size_function,
                &[args.captures_ptr.0.into(), args.value_ptr.0.into()],
                "",
            )
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value()
    }
    pub fn build_call_clone_function(
        &mut self,
        clone_function: &CloneFnValue<'ctx>,
        args: CloneFnArgs<'ctx>,
    ) {
        let clone_function = CallableValue::try_from(clone_function.0).unwrap();
        self.builder.build_call(
            clone_function,
            &[
                args.captures_ptr.0.into(),
                args.src_ptr.0.into(),
                args.dest_ptr.0.into(),
            ],
            "",
        );
    }
    pub fn build_call_destroy_function(
        &mut self,
        destroy_function: &DestroyFnValue<'ctx>,
        args: DestroyFnArgs<'ctx>,
    ) {
        let destroy_function = CallableValue::try_from(destroy_function.0).unwrap();
        self.builder.build_call(
            destroy_function,
            &[args.captures_ptr.0.into(), args.value_ptr.0.into()],
            "",
        );
    }
    pub fn build_call_captures_clone_function(
        &mut self,
        captures_clone_function: &CapturesCloneFnValue<'ctx>,
        args: CapturesCloneFnArgs<'ctx>,
    ) {
        let captures_clone_function = CallableValue::try_from(captures_clone_function.0).unwrap();
        self.builder.build_call(
            captures_clone_function,
            &[args.captures_ptr.0.into(), args.dest_ptr.0.into()],
            "",
        );
    }
    pub fn build_call_captures_destroy_function(
        &mut self,
        captures_destroy_function: &CapturesDestroyFnValue<'ctx>,
        args: CapturesDestroyFnArgs<'ctx>,
    ) {
        let captures_destroy_function =
            CallableValue::try_from(captures_destroy_function.0).unwrap();
        self.builder
            .build_call(captures_destroy_function, &[args.captures_ptr.0.into()], "");
    }
    pub fn build_call_raw_function(
        &mut self,
        raw_function: &RawFnValue<'ctx>,
        args: RawFnArgs<'ctx>,
    ) {
        let raw_function = CallableValue::try_from(raw_function.0).unwrap();
        self.builder.build_call(
            raw_function,
            &[
                args.captures_ptr.0.into(),
                args.arg_ptr.0.into(),
                args.ret_ptr.0.into(),
            ],
            "",
        );
    }
    pub fn build_call_ret_size_function(
        &mut self,
        ret_size_function: &RetSizeFnValue<'ctx>,
        args: RetSizeFnArgs<'ctx>,
    ) -> IntValue<'ctx> {
        let ret_size_function = CallableValue::try_from(ret_size_function.0).unwrap();
        self.builder
            .build_call(
                ret_size_function,
                &[args.captures_ptr.0.into(), args.arg_ptr.0.into()],
                "",
            )
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value()
    }

    pub fn build_get_universe_size_function(
        &mut self,
        universe_ptr: UniversePtrValue<'ctx>,
    ) -> SizeFnValue<'ctx> {
        let size_fn_ptr = self
            .builder
            .build_struct_gep(universe_ptr.0, 0, "")
            .unwrap();
        let size_fn = self
            .builder
            .build_load(size_fn_ptr, "")
            .into_pointer_value();
        SizeFnValue(size_fn)
    }
    pub fn build_get_universe_clone_function(
        &mut self,
        universe_ptr: UniversePtrValue<'ctx>,
    ) -> CloneFnValue<'ctx> {
        let clone_fn_ptr = self
            .builder
            .build_struct_gep(universe_ptr.0, 1, "")
            .unwrap();
        let clone_fn = self
            .builder
            .build_load(clone_fn_ptr, "")
            .into_pointer_value();
        CloneFnValue(clone_fn)
    }
    pub fn build_get_universe_destroy_function(
        &mut self,
        universe_ptr: UniversePtrValue<'ctx>,
    ) -> DestroyFnValue<'ctx> {
        let destroy_fn_ptr = self
            .builder
            .build_struct_gep(universe_ptr.0, 2, "")
            .unwrap();
        let destroy_fn = self
            .builder
            .build_load(destroy_fn_ptr, "")
            .into_pointer_value();
        DestroyFnValue(destroy_fn)
    }
    pub fn build_get_universe_total_size(
        &mut self,
        universe_ptr: UniversePtrValue<'ctx>,
    ) -> IntValue<'ctx> {
        let total_size_ptr = self
            .builder
            .build_struct_gep(universe_ptr.0, 3, "")
            .unwrap();
        self.builder.build_load(total_size_ptr, "").into_int_value()
    }
    pub fn build_get_universe_captures_clone_function(
        &mut self,
        universe_ptr: UniversePtrValue<'ctx>,
    ) -> CapturesCloneFnValue<'ctx> {
        let captures_clone_fn_ptr = self
            .builder
            .build_struct_gep(universe_ptr.0, 4, "")
            .unwrap();
        let captures_clone_fn = self
            .builder
            .build_load(captures_clone_fn_ptr, "")
            .into_pointer_value();
        CapturesCloneFnValue(captures_clone_fn)
    }
    pub fn build_get_universe_captures_destroy_function(
        &mut self,
        universe_ptr: UniversePtrValue<'ctx>,
    ) -> CapturesDestroyFnValue<'ctx> {
        let captures_destroy_fn_ptr = self
            .builder
            .build_struct_gep(universe_ptr.0, 5, "")
            .unwrap();
        let captures_destroy_fn = self
            .builder
            .build_load(captures_destroy_fn_ptr, "")
            .into_pointer_value();
        CapturesDestroyFnValue(captures_destroy_fn)
    }
    pub fn build_get_universe_captures_ptr(
        &mut self,
        universe_ptr: UniversePtrValue<'ctx>,
    ) -> CapturesPtrValue<'ctx> {
        CapturesPtrValue(
            self.builder
                .build_struct_gep(universe_ptr.0, 6, "")
                .unwrap(),
        )
    }

    pub fn build_get_pi_raw_function(&mut self, pi_ptr: PiPtrValue<'ctx>) -> RawFnValue<'ctx> {
        let raw_fn_ptr = self.builder.build_struct_gep(pi_ptr.0, 0, "").unwrap();
        let raw_fn = self.builder.build_load(raw_fn_ptr, "").into_pointer_value();
        RawFnValue(raw_fn)
    }
    pub fn build_get_pi_ret_size_function(
        &mut self,
        pi_ptr: PiPtrValue<'ctx>,
    ) -> RetSizeFnValue<'ctx> {
        let ret_size_fn_ptr = self.builder.build_struct_gep(pi_ptr.0, 1, "").unwrap();
        let ret_size_fn = self
            .builder
            .build_load(ret_size_fn_ptr, "")
            .into_pointer_value();
        RetSizeFnValue(ret_size_fn)
    }
    pub fn build_get_pi_total_size(&mut self, pi_ptr: PiPtrValue<'ctx>) -> IntValue<'ctx> {
        let total_size_ptr = self.builder.build_struct_gep(pi_ptr.0, 2, "").unwrap();
        self.builder.build_load(total_size_ptr, "").into_int_value()
    }
    pub fn build_get_pi_captures_clone_function(
        &mut self,
        pi_ptr: PiPtrValue<'ctx>,
    ) -> CapturesCloneFnValue<'ctx> {
        let captures_clone_fn_ptr = self.builder.build_struct_gep(pi_ptr.0, 3, "").unwrap();
        let captures_clone_fn = self
            .builder
            .build_load(captures_clone_fn_ptr, "")
            .into_pointer_value();
        CapturesCloneFnValue(captures_clone_fn)
    }
    pub fn build_get_pi_captures_destroy_function(
        &mut self,
        pi_ptr: PiPtrValue<'ctx>,
    ) -> CapturesDestroyFnValue<'ctx> {
        let captures_destroy_fn_ptr = self.builder.build_struct_gep(pi_ptr.0, 4, "").unwrap();
        let captures_destroy_fn = self
            .builder
            .build_load(captures_destroy_fn_ptr, "")
            .into_pointer_value();
        CapturesDestroyFnValue(captures_destroy_fn)
    }
    pub fn build_get_pi_captures_ptr(
        &mut self,
        pi_ptr: PiPtrValue<'ctx>,
    ) -> CapturesPtrValue<'ctx> {
        CapturesPtrValue(self.builder.build_struct_gep(pi_ptr.0, 5, "").unwrap())
    }
}
