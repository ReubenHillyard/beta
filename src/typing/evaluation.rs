//! Functions for evaluating [`TypedExpression`]s to [`TypedValue`]s.

use crate::typing::definitions::Definitions;
use crate::typing::environment::{evaluate_ev, Environment};
use crate::typing::expression::CoreExpression;
use crate::typing::type_wrapper::Term;
use crate::typing::value::{Closure, Neutral, Value};

pub trait Evaluate<'a> {
    type ValueT: Term;
    fn evaluate(&self, defs: &Definitions<'a>, env: &Environment<'a, '_>) -> Self::ValueT;
}

impl<'a> Evaluate<'a> for CoreExpression<'a> {
    type ValueT = Value<'a>;
    fn evaluate(&self, defs: &Definitions<'a>, env: &Environment<'a, '_>) -> Self::ValueT {
        use crate::typing::expression::CoreExpression::*;
        match self {
            MetaVariable(mv) => defs.lookup_meta(*mv),
            Variable(ev) => evaluate_ev(defs, env, ev),
            PiType {
                tparam_type,
                ret_type,
            } => Value::PiType {
                param_type: Box::new(tparam_type.evaluate(defs, env)),
                tclosure: Box::new(Closure::new_in_env(env, (**ret_type).clone())),
            },
            Lambda { ret_val } => Value::Lambda {
                closure: Box::new(Closure::new_in_env(env, (**ret_val).clone())),
            },
            Application { func, arg } => {
                do_apply(defs, &func.evaluate(defs, env), &arg.evaluate(defs, env))
            }
            Universe => Value::Universe,
        }
    }
}

/// Call a function with an argument.
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
