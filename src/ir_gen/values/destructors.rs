//! Type and functions for destroying objects on scope-exit.

use crate::ir_gen::build::CompilerWithBuilder;
use crate::ir_gen::values::tags::flow::{Read, Write};
use crate::ir_gen::values::tags::tag;
use crate::ir_gen::values::PtrValue;

/// A register of objects that need to be destroyed on scope-exit.
#[derive(Default)]
pub struct Destructors<'ctx> {
    dtors: Vec<(
        PtrValue<'ctx, Read, tag::Universe>,
        PtrValue<'ctx, Write, tag::Unknown>,
    )>,
}

impl<'ctx> Destructors<'ctx> {
    /// Registers an object to be destroyed on scope-exit.
    ///
    /// Requires:
    /// * `arg_ptr` points to storage for an object of the type pointed to by `universe_ptr`.
    /// * the storage pointed to by `arg_ptr` is local to the function being built by the function
    /// `add_dtor` is called from. In particular, `arg_ptr` may not be the return address.
    /// * `arg_ptr` has not already had its destructor-call scheduled.
    pub fn add_dtor(
        &mut self,
        universe_ptr: PtrValue<'ctx, Read, tag::Universe>,
        arg_ptr: PtrValue<'ctx, Write, tag::Unknown>,
    ) {
        self.dtors.push((universe_ptr, arg_ptr));
    }

    /// Builds destructor-calls of all the registered objects.
    pub fn build_destroy(self, compiler_builder: &mut CompilerWithBuilder<'ctx, '_>) {
        for (universe_ptr, arg_ptr) in self.dtors.into_iter().rev() {
            let arg_ptr = PtrValue(arg_ptr.0, Read, arg_ptr.2);
            // by now the variable has been initialized; so can be read
            compiler_builder.build_destroy(universe_ptr, arg_ptr);
        }
    }
}
