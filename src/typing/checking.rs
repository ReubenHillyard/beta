//! Functions for type-checking and type-synthesis.

use crate::typing::ast::Expression;
use crate::typing::environments::{Context, Definitions};
use crate::typing::value::Type;
use derivative::Derivative;
use std::fmt;

pub use crate::typing::environment::context::type_var;

/// A hereditarily-typed expression.
#[derive(Clone, Debug, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct TypedExpression<'a> {
    expr: Expression<'a>,
    #[derivative(PartialEq = "ignore")]
    type_: Type<'a>,
}

impl fmt::Display for TypedExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} as {}", self.expr, self.type_.as_value())
    }
}

impl<'a> TypedExpression<'a> {
    pub const UNIVERSE: TypedExpression<'static> = TypedExpression {
        expr: Expression::Universe,
        type_: Type::UNIVERSE,
    };
    pub fn get_expr(&self) -> &Expression<'a> {
        &self.expr
    }
    pub fn get_type(&self) -> &Type<'a> {
        &self.type_
    }
    pub(crate) fn create_typed_expression(
        expr: Expression<'a>,
        type_: Type<'a>,
    ) -> TypedExpression<'a> {
        TypedExpression { expr, type_ }
    }
}

/// Checks that an expression has a given type, and forms a [`TypedExpression`] from them.
pub fn check_type<'a>(
    defs: &Definitions<'a>,
    ctx: &Context<'a, '_>,
    expr: &Expression<'a>,
    type_: &Type<'a>,
) -> super::Result<'a, TypedExpression<'a>> {
    detail::check_type(defs, ctx, expr, type_)?;
    Ok(TypedExpression::create_typed_expression(
        expr.clone(),
        type_.clone(),
    ))
}

/// Synthesizes a type for an expression, and forms a [`TypedExpression`] from them.
pub fn synth_type<'a>(
    defs: &Definitions<'a>,
    ctx: &Context<'a, '_>,
    expr: &Expression<'a>,
) -> super::Result<'a, TypedExpression<'a>> {
    let type_ = detail::synth_type(defs, ctx, expr)?;
    Ok(TypedExpression::create_typed_expression(
        expr.clone(),
        type_,
    ))
}

mod detail {
    use crate::typing::ast::Expression;
    use crate::typing::checking::type_var;
    use crate::typing::environments::{Context, Definitions, Environment};
    use crate::typing::equivalence::judgmentally_equal;
    use crate::typing::evaluation::detail::evaluate;
    use crate::typing::read_back::read_back_typed;
    use crate::typing::value::{Closure, Type, Value};
    use crate::typing::TypeError;

    pub fn check_type<'a>(
        defs: &Definitions<'a>,
        ctx: &Context<'a, '_>,
        expr: &Expression<'a>,
        type_: &Type<'a>,
    ) -> super::super::Result<'a, ()> {
        use Expression::*;
        match expr {
            Lambda {
                param_type,
                ret_val,
            } => {
                if let Some(_given_param_type) = param_type {
                    unimplemented!("cannot yet check type of lambda with given parameter type")
                };
                let Value::PiType { param_type, tclosure } = type_.as_value() else {
                    return Err(TypeError::UsedLambdaAsNonPiType(expr.clone(), type_.clone()));
                };
                let fresh_var = ctx.fresh_var((**param_type).clone());
                let ret_type = Type::create_type_from_value(tclosure.call(defs, &fresh_var));
                check_type(defs, &ctx.extend(param_type), ret_val, &ret_type)
            }
            _ => {
                let syn_type_expr = synth_type(defs, ctx, expr)?;
                judgmentally_equal(
                    defs,
                    ctx,
                    &type_.clone().into_typed_value(),
                    &syn_type_expr.into_typed_value(),
                )
            }
        }
    }

    pub fn synth_type<'a>(
        defs: &Definitions<'a>,
        ctx: &Context<'a, '_>,
        expr: &Expression<'a>,
    ) -> super::super::Result<'a, Type<'a>> {
        use Expression::*;
        match expr {
            Variable(ev) => Ok(type_var(defs, ctx, *ev)),
            PiType {
                tparam_type,
                ret_type,
            } => {
                check_type(defs, ctx, tparam_type, &Type::UNIVERSE)?;
                let tparam_type = Type::create_type_from_value(evaluate(
                    defs,
                    &Environment::from_context(ctx),
                    tparam_type,
                ));
                check_type(defs, &ctx.extend(&tparam_type), ret_type, &Type::UNIVERSE)?;
                Ok(Type::UNIVERSE)
            }
            Lambda {
                param_type,
                ret_val,
            } => {
                let Some(param_type) = param_type else {
                    return Err(TypeError::CantDeduceLambdaParamType(expr.clone()))
                };
                check_type(defs, ctx, param_type, &Type::UNIVERSE)?;
                let param_type = Type::create_type_from_value(evaluate(
                    defs,
                    &Environment::from_context(ctx),
                    param_type,
                ));
                let ret_val_type = synth_type(defs, &ctx.extend(&param_type), ret_val)?;
                let ret_type = read_back_typed(
                    defs,
                    &ctx.extend(&param_type),
                    &ret_val_type.into_typed_value(),
                );
                Ok(Type::create_type_from_value(Value::PiType {
                    param_type: Box::new(param_type),
                    tclosure: Box::new(Closure::new_in_ctx(ctx, ret_type.get_expr().clone())),
                }))
            }
            Application { func, arg } => {
                let func_type = synth_type(defs, ctx, func)?;
                let Value::PiType { param_type, tclosure } = func_type.as_value() else {
                    return Err(TypeError::CantCallNonFunction((**func).clone()));
                };
                check_type(defs, ctx, arg, param_type)?;
                let arg = evaluate(defs, &Environment::from_context(ctx), arg);
                let type_ = Type::create_type_from_value(tclosure.call(defs, &arg));
                Ok(type_)
            }
            Universe => Ok(Type::UNIVERSE),
            Annotation { expr, type_ } => {
                check_type(defs, ctx, type_, &Type::UNIVERSE)?;
                let type_ = Type::create_type_from_value(evaluate(
                    defs,
                    &Environment::from_context(ctx),
                    type_,
                ));
                check_type(defs, ctx, expr, &type_)?;
                Ok(type_)
            }
        }
    }
}
