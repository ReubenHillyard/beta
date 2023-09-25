use crate::ir_gen::values::tags::flow::Read;
use crate::ir_gen::values::tags::tag;
use crate::ir_gen::values::{FnValue, PtrValue};
use crate::ir_gen::Compiler;
use inkwell::module::Linkage;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, GlobalValue, UnnamedAddress};

impl<'ctx> Compiler<'ctx> {
    /// Creates a global constant with a given initializer.
    ///
    /// Requires `name` is either a unique, valid identifier or `""`. Requires `initializer` is a
    /// constant value.
    pub fn add_global(
        &mut self,
        name: &str,
        initializer: &impl BasicValue<'ctx>,
    ) -> PtrValue<'ctx, Read, tag::Unknown> {
        let global =
            self.module
                .add_global(initializer.as_basic_value_enum().get_type(), None, name);
        global.set_initializer(initializer);
        global.set_linkage(Linkage::Private);
        global.set_unnamed_address(UnnamedAddress::Global);
        global.set_constant(true);
        PtrValue(global.as_pointer_value(), Read, tag::Unknown)
    }

    /// Obtains the initializer of an existing global value or fails.
    pub fn get_global_initializer(&self, name: &str) -> Option<BasicValueEnum<'ctx>> {
        self.get_global(name).and_then(GlobalValue::get_initializer)
    }

    /// Obtains an existing global value or fails.
    pub fn get_global(&self, name: &str) -> Option<GlobalValue<'ctx>> {
        self.module.get_global(name)
    }

    /// Obtains a pointer to an existing global value or fails.
    pub fn get_global_ptr(&self, name: &str) -> Option<PtrValue<'ctx, Read, tag::Unknown>> {
        Some(PtrValue(
            self.get_global(name)?.as_pointer_value(),
            Read,
            tag::Unknown,
        ))
    }

    /// Obtains an existing global function or fails.
    pub fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    /// Obtains a pointer to an existing global function or fails.
    pub fn get_function_ptr(&self, name: &str) -> Option<FnValue<'ctx, tag::Unknown>> {
        Some(FnValue(
            self.get_function(name)?
                .as_global_value()
                .as_pointer_value(),
            tag::Unknown,
        ))
    }
}
