use crate::ir_gen::build::CompilerWithBuilder;
use crate::ir_gen::values::tags::args::{Args, MemcpyArgs};
use crate::ir_gen::values::tags::function::Function;
use crate::ir_gen::values::tags::ret::Ret;
use crate::ir_gen::values::FnValue;
use std::borrow::Borrow;

impl<'ctx> CompilerWithBuilder<'ctx, '_> {
    /// Builds a call to `memcpy`.
    pub fn build_call_memcpy(&mut self, args: MemcpyArgs<'ctx>) {
        self.builder.build_call(
            self.compiler.memcpy(),
            &[
                args.dest_ptr.0.into(),
                args.src_ptr.0.into(),
                args.num_bytes.into(),
                self.compiler.bool_type().const_zero().into(), // not volatile
            ],
            "",
        );
    }

    /// Builds a call to a function pointer.
    pub fn build_call_fn<F: Function>(
        &mut self,
        function: FnValue<'ctx, F>,
        args: F::ArgsT<'ctx>,
    ) -> F::RetT<'ctx> {
        let ret_val = self.builder.build_indirect_call(
            F::fn_type(self.compiler),
            function.0,
            args.as_args().borrow(),
            "",
        );
        F::RetT::from_ret_val(ret_val)
    }
}
