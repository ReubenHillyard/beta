//! Functions for evaluating [`TypedExpression`]s to [`TypedValue`]s.

use crate::typing::checking::TypedExpression;
use crate::typing::definitions::Definitions;
use crate::typing::environment::Environment;
use crate::typing::value::{Neutral, TypedValue, Value};

/// Evaluates an [`TypedExpression`] to a [`TypedValue`].
pub fn evaluate<'a>(
    defs: &Definitions<'a>,
    env: &Environment<'a, '_>,
    expr: &TypedExpression<'a>,
) -> TypedValue<'a> {
    TypedValue::create_typed_value(
        expr.get_type().clone(),
        detail::evaluate(defs, env, expr.get_expr()),
    )
}

pub(crate) mod detail {
    use crate::typing::checking::CoreExpression;
    use crate::typing::environment::{evaluate_ev, Closure, Environment};
    use crate::typing::environments::Definitions;
    use crate::typing::evaluation::do_apply;
    use crate::typing::value::{Type, TypedValue, Value};

    pub(crate) fn evaluate<'a>(
        defs: &Definitions<'a>,
        env: &Environment<'a, '_>,
        expr: &CoreExpression<'a>,
    ) -> Value<'a> {
        use CoreExpression::*;
        match expr {
            MetaVariable(mv) => defs.lookup_meta(*mv),
            Variable(ev) => evaluate_ev(defs, env, ev),
            PiType {
                tparam_type,
                ret_type,
            } => Value::PiType {
                param_type: Box::new(Type::create_type_from_value(
                    TypedValue::create_typed_value(
                        Type::UNIVERSE,
                        evaluate(defs, env, tparam_type),
                    ),
                )),
                tclosure: Box::new(Closure::new_in_env(env, (**ret_type).clone())),
            },
            Lambda { ret_val, .. } => Value::Lambda {
                closure: Box::new(Closure::new_in_env(env, (**ret_val).clone())),
            },
            Application { func, arg, .. } => {
                do_apply(defs, &evaluate(defs, env, func), &evaluate(defs, env, arg))
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
        Value::MetaNeutral(neu) => Value::MetaNeutral(Neutral::Application {
            func: Box::new(neu.clone()),
            arg: Box::new(arg.clone()),
        }),
        _ => panic!("Cannot call `{}` because it is not a function.", func),
    }
}
