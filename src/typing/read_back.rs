//! Functions for reading back [`Value`]s into [`Expression`]s in normal form.

use crate::typing::ast::Expression;
use crate::typing::environments::{Context, Definitions};
use crate::typing::value::{Neutral, Type, Value};

pub use crate::typing::environment::context::vv_to_ev;
use crate::typing::evaluation::do_apply;

/// Reads back a [`Type`]d [`Value`] to an [`Expression`] in beta-normal, eta-long form.
pub fn read_back_typed<'a>(
    defs: &Definitions<'a>,
    ctx: &Context<'a, '_>,
    val: &Value<'a>,
    type_: &Type<'a>,
) -> Expression<'a> {
    match type_.as_value() {
        Value::PiType {
            param_type,
            tclosure,
        } => {
            let fresh_var = ctx.fresh_var();
            let fresh_var = Value::Neutral {
                type_: param_type.clone(),
                neu: Neutral::Variable(fresh_var),
            };
            let ret_val = read_back_typed(
                defs,
                &ctx.extend(param_type),
                &do_apply(defs, val, &fresh_var),
                &Type::create_type_from_value(tclosure.call(defs, &fresh_var)),
            );
            Expression::Lambda {
                param_type: None,
                ret_val: Box::new(ret_val),
            }
        }
        Value::Universe => match val {
            Value::PiType {
                param_type,
                tclosure,
            } => {
                let fresh_var = ctx.fresh_var();
                let fresh_var = Value::Neutral {
                    type_: param_type.clone(),
                    neu: Neutral::Variable(fresh_var),
                };
                let ret_type = read_back_typed(
                    defs,
                    &ctx.extend(param_type),
                    &tclosure.call(defs, &fresh_var),
                    &Type::UNIVERSE,
                );
                let tparam_type =
                    read_back_typed(defs, ctx, param_type.as_value(), &Type::UNIVERSE);
                Expression::PiType {
                    tparam_type: Box::new(tparam_type),
                    ret_type: Box::new(ret_type),
                }
            }
            Value::Universe => Expression::Universe,
            Value::Neutral { neu, .. } => read_back_neutral(defs, ctx, neu),
            _ => panic!("Cannot read back `{:?}` as a type.", val),
        },
        Value::Neutral { .. } => {
            let Value::Neutral { neu ,..} = val else {
                panic!(
                    "Cannot read back `{:?}` as a `{:?}` because it is not of that type.",
                    val, type_
                )
            };
            read_back_neutral(defs, ctx, neu)
        }
        _ => panic!(
            "Cannot read back as `{:?}` because it is not a type.",
            type_.as_value()
        ),
    }
}

/// Reads back a [`Neutral`] value to an [`Expression`] in beta-normal, eta-long form.
pub fn read_back_neutral<'a>(
    defs: &Definitions<'a>,
    ctx: &Context<'a, '_>,
    neu: &Neutral<'a>,
) -> Expression<'a> {
    match neu {
        Neutral::Variable(vv) => Expression::Variable(vv_to_ev(ctx, *vv)),
        Neutral::Application { func, arg } => Expression::Application {
            func: Box::new(read_back_neutral(defs, ctx, func)),
            arg: Box::new(read_back_typed(defs, ctx, arg.get_value(), arg.get_type())),
        },
    }
}
