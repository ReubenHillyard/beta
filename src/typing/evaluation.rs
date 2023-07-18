//! Functions for evaluating [`Expression`]s to [`Value`]s.

use crate::typing::checking::TypedExpression;
use crate::typing::definitions::Definitions;
use crate::typing::environments::Environment;
use crate::typing::value::{Neutral, Type, TypedValue, Value};

pub(crate) use crate::typing::environment::context::evaluate_ev;

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
    use crate::typing::ast::Expression;
    use crate::typing::environments::{Definitions, Environment};
    use crate::typing::evaluation::{do_apply, evaluate_ev};
    use crate::typing::value::{Closure, Type, Value};

    pub(crate) fn evaluate<'a>(
        defs: &Definitions<'a>,
        env: &Environment<'a, '_>,
        expr: &Expression<'a>,
    ) -> Value<'a> {
        use Expression::*;
        match expr {
            Variable(ev) => evaluate_ev(defs, env, ev),
            PiType {
                tparam_type,
                ret_type,
            } => Value::PiType {
                param_type: Box::new(Type::create_type_from_value(evaluate(
                    defs,
                    env,
                    tparam_type,
                ))),
                tclosure: Box::new(Closure::new_in_env(env, (**ret_type).clone())),
            },
            Lambda { ret_val, .. } => Value::Lambda {
                closure: Box::new(Closure::new_in_env(env, (**ret_val).clone())),
            },
            Application { func, arg, .. } => {
                do_apply(defs, &evaluate(defs, env, func), &evaluate(defs, env, arg))
            }
            Universe => Value::Universe,
            Annotation { expr, .. } => evaluate(defs, env, expr),
        }
    }
}

/// Call a function with an argument.
pub(crate) fn do_apply<'a>(defs: &Definitions<'a>, func: &Value<'a>, arg: &Value<'a>) -> Value<'a> {
    match func {
        Value::Lambda { closure } => closure.call(defs, arg),
        Value::Neutral { type_, neu } => {
            let Value::PiType { param_type, tclosure } = type_.as_value() else {
                panic!("Cannot call `{}` because it has non-function type `{}`.", func, type_.as_value())
            };
            let ret_type = Type::create_type_from_value(tclosure.call(defs, arg));
            Value::Neutral {
                type_: Box::new(ret_type),
                neu: Neutral::Application {
                    func: Box::new(neu.clone()),
                    arg: Box::new(TypedValue::create_typed_value(
                        (**param_type).clone(),
                        arg.clone(),
                    )),
                },
            }
        }
        _ => panic!("Cannot call `{}` because it is not a function.", func),
    }
}
