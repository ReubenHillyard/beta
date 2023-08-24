//! Functions for evaluating [`TypedExpression`]s to [`TypedValue`]s.

use crate::typing::checking::{CoreExpression, TypedExpression};
use crate::typing::definitions::Definitions;
use crate::typing::environment::{evaluate_ev, Environment};
use crate::typing::type_wrapper::Term;
use crate::typing::value::{Closure, Neutral, TypedValue, Value};

pub trait Evaluate {
    type ValueT<'b>: Term
        where
            Self: 'b;
    fn evaluate<'b>(&self, defs: &Definitions<'b>, env: &Environment<'b, '_>) -> Self::ValueT<'b>
        where
            Self: 'b;
}

impl<'a> Evaluate for CoreExpression<'a> {
    type ValueT<'b> = Value<'b> where Self: 'b;
    fn evaluate<'b>(&self, defs: &Definitions<'b>, env: &Environment<'b, '_>) -> Self::ValueT<'b>
        where
            Self: 'b,
    {
        use CoreExpression::*;
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

impl<'a> Evaluate for TypedExpression<'a> {
    type ValueT<'b> = TypedValue<'b> where Self: 'b;
    fn evaluate<'b>(&self, defs: &Definitions<'b>, env: &Environment<'b, '_>) -> Self::ValueT<'b>
        where
            Self: 'b,
    {
        TypedValue::create_typed_value(self.get_type().clone(), self.get_expr().evaluate(defs, env))
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
        Value::MetaNeutral(neu) => Value::MetaNeutral(Neutral::Application {
            func: Box::new(neu.clone()),
            arg: Box::new(arg.clone()),
        }),
        _ => panic!("Cannot call `{}` because it is not a function.", func),
    }
}
