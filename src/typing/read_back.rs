//! Functions for reading back [`Value`]s into [`Expression`]s in normal form.

use crate::typing::checking::TypedExpression;
use crate::typing::environments::{Context, Definitions};
use crate::typing::value::TypedValue;

pub use crate::typing::environment::context::vv_to_ev;

/// Reads back a [`Type`]d [`Value`] to an [`TypedExpression`] in beta-normal, eta-long form.
pub fn read_back_typed<'a>(
    defs: &Definitions<'a>,
    ctx: &Context<'a, '_>,
    val: &TypedValue<'a>,
) -> TypedExpression<'a> {
    TypedExpression::create_typed_expression(
        detail::read_back_typed(defs, ctx, val),
        val.get_type().clone(),
    )
}

mod detail {
    use crate::typing::ast::Expression;
    use crate::typing::environments::{Context, Definitions};
    use crate::typing::evaluation::do_apply;
    use crate::typing::read_back::vv_to_ev;
    use crate::typing::value::{Neutral, Type, TypedValue, Value};

    pub fn read_back_typed<'a>(
        defs: &Definitions<'a>,
        ctx: &Context<'a, '_>,
        val: &TypedValue<'a>,
    ) -> Expression<'a> {
        let out = match val.get_type().as_value() {
            Value::PiType {
                param_type,
                tclosure,
            } => {
                let fresh_var = ctx.fresh_var((**param_type).clone());
                let ret_val = do_apply(defs, val.get_value(), &fresh_var);
                let ret_type = Type::create_type_from_value(tclosure.call(defs, &fresh_var));
                let ret_val = read_back_typed(
                    defs,
                    &ctx.extend(param_type),
                    &TypedValue::create_typed_value(ret_type, ret_val),
                );
                Expression::Lambda {
                    param_type: None,
                    ret_val: Box::new(ret_val),
                }
            }
            Value::Universe => match val.get_value() {
                Value::PiType {
                    param_type,
                    tclosure,
                } => {
                    let fresh_var = ctx.fresh_var((**param_type).clone());
                    let ret_type = Type::create_type_from_value(tclosure.call(defs, &fresh_var));
                    let ret_type = read_back_typed(
                        defs,
                        &ctx.extend(param_type),
                        &ret_type.into_typed_value(),
                    );
                    let tparam_type =
                        read_back_typed(defs, ctx, &param_type.clone().into_typed_value());
                    Expression::PiType {
                        tparam_type: Box::new(tparam_type),
                        ret_type: Box::new(ret_type),
                    }
                }
                Value::Universe => Expression::Universe,
                Value::Neutral { neu, .. } => read_back_neutral(defs, ctx, neu),
                _ => panic!("Cannot read back `{}` as a type.", val.get_value()),
            },
            Value::Neutral { .. } => {
                let Value::Neutral { neu,.. } = val.get_value() else {
                    panic!(
                        "Cannot read back `{}` as a `{}` because it is not of that type.",
                        val.get_value(), val.get_type().as_value()
                    )
                };
                read_back_neutral(defs, ctx, neu)
            }
            _ => panic!(
                "Cannot read back as `{}` because it is not a type.",
                val.get_type().as_value()
            ),
        };
        out
    }

    /// Reads back a [`Neutral`] value to an [`TypedExpression`] in beta-normal, eta-long form.
    fn read_back_neutral<'a>(
        defs: &Definitions<'a>,
        ctx: &Context<'a, '_>,
        neu: &Neutral<'a>,
    ) -> Expression<'a> {
        match neu {
            Neutral::Variable(vv) => Expression::Variable(vv_to_ev(ctx, *vv)),
            Neutral::Application { func, arg } => Expression::Application {
                func: Box::new(read_back_neutral(defs, ctx, func)),
                arg: Box::new(read_back_typed(defs, ctx, arg)),
            },
        }
    }
}
