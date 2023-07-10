use crate::typing::ast::Expression;
use crate::typing::definitions::Definitions;
use crate::typing::environment::{EVariable, Environment};
use crate::typing::value::{Closure, Neutral, Type, Value};

pub use crate::typing::environment::evaluate_var;

/// Evaluates an expression to a value.
pub fn evaluate<'a>(
    defs: &Definitions<'a>,
    env: &Environment<'a, '_>,
    expr: &Expression<'a>,
) -> Value<'a> {
    use Expression::*;
    match expr {
        Variable(ev) => evaluate_var(defs, env, ev),
        PiType {
            tparam_type,
            ret_type,
        } => Value::PiType {
            param_type: Box::new(Type(evaluate(defs, env, tparam_type))),
            tclosure: Closure::new_in_env(env, (**ret_type).clone()),
        },
        Lambda { ret_val, .. } => Value::Lambda {
            closure: Closure::new_in_env(env, (**ret_val).clone()),
        },
        Application { func, arg } => {
            do_apply(defs, &evaluate(defs, env, func), &evaluate(defs, env, arg))
        }
        Universe => Value::Universe,
        Annotation { expr, .. } => evaluate(defs, env, expr),
    }
}

pub(crate) fn do_apply<'a>(defs: &Definitions<'a>, func: &Value<'a>, arg: &Value<'a>) -> Value<'a> {
    match func {
        Value::Lambda { closure } => closure.call(defs, arg),
        Value::Neutral { neu } => Value::Neutral {
            neu: Neutral::Application {
                func: Box::new(neu.clone()),
                arg: Box::new(arg.clone()),
            },
        },
        _ => panic!("Cannot call `{:?}` because it is not a function.", func),
    }
}
