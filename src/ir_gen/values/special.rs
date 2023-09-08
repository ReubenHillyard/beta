use crate::ir_gen::build::CompilerWithBuilder;
use crate::ir_gen::values::captures::Environment;
use crate::ir_gen::values::tags::args::{
    CloneFnArgs, DestroyFnArgs, RawFnArgs, RetSizeFnArgs, SizeFnArgs,
};
use crate::ir_gen::values::tags::flow::{Read, Write};
use crate::ir_gen::values::tags::tag;
use crate::ir_gen::values::Location;
use crate::ir_gen::values::PtrValue;
use inkwell::values::IntValue;

impl<'ctx> CompilerWithBuilder<'ctx, '_> {
    /// Builds computation of the size of a given object of a given type.
    ///
    /// Requires `arg_ptr` points to an object of the type pointed to by `universe_ptr`.
    pub fn build_sizeof(
        &mut self,
        universe_ptr: PtrValue<'ctx, Read, tag::Universe>,
        arg_ptr: PtrValue<'ctx, Read, tag::Unknown>,
    ) -> IntValue<'ctx> {
        let size_fn = self.build_load_field(universe_ptr, tag::SizeFn);
        let captures_ptr = self.build_get_captures_ptr(universe_ptr);
        self.build_call_fn(
            size_fn,
            SizeFnArgs {
                captures_ptr,
                arg_ptr,
            },
        )
    }

    /// Builds a clone of a given object of a given type in given storage.
    ///
    /// Requires `src_ptr` points to an object of the type pointed to by `universe_ptr` and that
    /// `dest_ptr` points to storage that can hold the value of that object.
    pub fn build_clone(
        &mut self,
        universe_ptr: PtrValue<'ctx, Read, tag::Universe>,
        src_ptr: PtrValue<'ctx, Read, tag::Unknown>,
        dest_ptr: PtrValue<'ctx, Write, tag::Unknown>,
    ) -> PtrValue<'ctx, Read, tag::Unknown> {
        let clone_fn = self.build_load_field(universe_ptr, tag::CloneFn);
        let captures_ptr = self.build_get_captures_ptr(universe_ptr);
        self.build_call_fn(
            clone_fn,
            CloneFnArgs {
                captures_ptr,
                src_ptr,
                dest_ptr,
            },
        );
        PtrValue(dest_ptr.0, Read, tag::Unknown)
    }

    /// Builds a destructor-call for a given object of a given type.
    ///
    /// Requires `arg_ptr` points to an object of the type pointed to by `universe_ptr`.
    pub fn build_destroy(
        &mut self,
        universe_ptr: PtrValue<'ctx, Read, tag::Universe>,
        arg_ptr: PtrValue<'ctx, Read, tag::Unknown>,
    ) {
        let destroy_fn = self.build_load_field(universe_ptr, tag::DestroyFn);
        let captures_ptr = self.build_get_captures_ptr(universe_ptr);
        self.build_call_fn(
            destroy_fn,
            DestroyFnArgs {
                captures_ptr,
                arg_ptr,
            },
        );
    }

    /// Builds a call of a language function, optionally placing the return value at a given
    /// address.
    ///
    /// Requires:
    /// * `pi_ptr` points to a valid function, in environment `env`.
    /// * `arg_ptr` points to a valid argument for that function, in environment `env`.
    /// * `ret_type` points to the result type of calling that function with that argument.
    pub fn build_call_pi(
        &mut self,
        env: &mut Environment<'ctx, '_>,
        ret_type: PtrValue<'ctx, Read, tag::Universe>,
        pi_ptr: PtrValue<'ctx, Read, tag::Pi>,
        arg_ptr: PtrValue<'ctx, Read, tag::Unknown>,
        at: Location<'ctx>,
    ) -> PtrValue<'ctx, Read, tag::Unknown> {
        let raw_fn = self.build_load_field(pi_ptr, tag::RawFn);
        let captures_ptr = self.build_get_captures_ptr(pi_ptr);
        let ret_ptr = match at {
            Location::Within(ptr) => ptr,
            Location::Other => {
                let ret_size_fn = self.build_load_field(pi_ptr, tag::RetSizeFn);
                let size = self.build_call_fn(
                    ret_size_fn,
                    RetSizeFnArgs {
                        captures_ptr,
                        arg_ptr,
                    },
                );
                let ptr = self.build_alloca(size);
                env.add_dtor(ret_type, ptr);
                ptr
            }
        };
        self.build_call_fn(
            raw_fn,
            RawFnArgs {
                captures_ptr,
                arg_ptr,
                ret_ptr,
            },
        );
        ret_ptr.as_read()
    }
}
