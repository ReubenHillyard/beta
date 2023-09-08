//! A trait for evaluating expressions to values.

use crate::typing::definitions::{Definitions, DefsWithEnv};
use crate::typing::environment::evaluate_ev;
use crate::typing::expression::CoreExpression;
use crate::typing::type_wrapper::Term;
use crate::typing::value::{Closure, Neutral, Value};

/// A trait for expressions that can be evaluated to values.
pub trait Evaluate<'a> {
    type ValueT: Term;

    /// Evaluates `self` in environment of `defs_env`.
    ///
    /// Requires `self` is a valid expression in environment of `defs_env`. Particularly, `self`
    /// must be well-typed.
    fn evaluate(&self, defs_env: DefsWithEnv<'a, '_>) -> Self::ValueT;
}

impl<'a> Evaluate<'a> for CoreExpression<'a> {
    type ValueT = Value<'a>;
    fn evaluate(&self, defs_env: DefsWithEnv<'a, '_>) -> Self::ValueT {
        use crate::typing::expression::CoreExpression::*;
        match self {
            MetaVariable(mv) => defs_env.defs.lookup_meta(*mv),
            Variable(ev) => evaluate_ev(defs_env, ev),
            PiType {
                tparam_type,
                ret_type,
            } => Value::PiType {
                param_type: Box::new(tparam_type.evaluate(defs_env)),
                tclosure: Box::new(Closure::new_in_env(defs_env.env, (**ret_type).clone())),
            },
            Lambda { ret_val } => Value::Lambda {
                closure: Box::new(Closure::new_in_env(defs_env.env, (**ret_val).clone())),
            },
            Application { func, arg } => do_apply(
                defs_env.defs,
                &func.evaluate(defs_env),
                &arg.evaluate(defs_env),
            ),
            Universe => Value::Universe,
        }
    }
}

/// Call a function with an argument.
///
/// Requires `func` and `arg` are valid values in the context from which `do_apply` is called,
/// and that, in that context, it is well-typed for `func` can be called with `arg`.
pub(crate) fn do_apply<'a>(defs: &Definitions<'a>, func: &Value<'a>, arg: &Value<'a>) -> Value<'a> {
    match func {
        Value::Lambda { closure } => closure.call(defs, arg),
        Value::Neutral(neu) => Value::Neutral(Neutral::Application {
            func: Box::new(neu.clone()),
            arg: Box::new(arg.clone()),
        }),
        _ => panic!("Cannot call `{func}` because it is not a function."),
    }
}
