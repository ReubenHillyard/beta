use crate::ir_gen::lambda::Lambda;
use crate::ir_gen::values::tags::tag;
use crate::ir_gen::Compiler;
use crate::typing::environments::Definitions;
use crate::typing::value::{Neutral, Principal, TypedValue, VVariable, Value};
use inkwell::values::{GlobalValue, StructValue};

impl<'ctx> Compiler<'ctx> {
    /// Creates a new global value.
    ///
    /// Requires `typed_value` is a valid typed value in the empty
    /// [`Context`](crate::typing::environments::Context). Requires `name` is either a unique, valid
    /// identifier or `""`.
    pub fn compile_global<'a>(
        &mut self,
        defs: &mut Definitions<'a>,
        name: &str,
        typed_value: &TypedValue<'a>,
    ) -> GlobalValue<'ctx> {
        let initializer = self.compile_global_initializer(defs, typed_value);
        self.add_global(name, &initializer)
    }

    /// Creates an initializer for a global value.
    ///
    /// Requires `typed_value` is a valid typed value in the empty
    /// [`Context`](crate::typing::environments::Context).
    fn compile_global_initializer<'a>(
        &mut self,
        defs: &mut Definitions<'a>,
        typed_value: &TypedValue<'a>,
    ) -> StructValue<'ctx> {
        match typed_value.get_term() {
            Value::PiType { .. } => self.compile_builtin_type(tag::Pi),
            Value::Lambda { closure } => {
                let Lambda {
                    raw_function,
                    ret_size_function,
                    ..
                } = self.compile_lambda(
                    &mut defs.with_empty_ctx(),
                    typed_value.get_type(),
                    closure,
                );
                self.compile_pi_without_captures(raw_function, ret_size_function)
            }
            Value::Universe => self.compile_builtin_type(tag::Universe),
            Value::Neutral(neu) => {
                let Neutral::Principal(p) = neu else {
                    panic!("global cannot be application")
                };
                let Principal::Variable(var) = p else {
                    panic!("meta-variables should all have been solved")
                };
                let VVariable::Global(name) = var else {
                    panic!("invalid value in empty context")
                };
                self.get_global_initializer(name)
                    .unwrap()
                    .into_struct_value()
            }
        }
    }
}
