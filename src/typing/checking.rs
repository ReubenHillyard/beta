//! Functions for type-checking and type-synthesis.

use crate::typing::ast::Expression;
use crate::typing::environment::{Closure, Context, Environment};
use crate::typing::environments::Definitions;
use crate::typing::evaluation::Evaluate;
use crate::typing::expression::{CoreExpression, TypeExpression, TypedExpression};
use crate::typing::read_back::read_back_with_ctx_len;
use crate::typing::type_wrapper;
use crate::typing::type_wrapper::Term;
use crate::typing::unification::unify_types;
use crate::typing::value::{Type, Value};

pub use crate::typing::environment::type_var;

/// Checks that an expression has a given type, and forms a [`TypedExpression`] from them.
pub fn check_type<'a>(
    defs: &mut Definitions<'a>,
    ctx: &Context<'a, '_>,
    expr: &Expression<'a>,
    type_: &Type<'a>,
) -> super::Result<'a, TypedExpression<'a>> {
    use Expression::*;
    match (expr, type_.wrapped()) {
        (Underscore, _) => Ok(defs.add_meta(ctx, type_.clone())),
        (
            Lambda {
                param_type: given_param_type,
                ret_val,
            },
            Value::PiType {
                param_type,
                tclosure,
            },
        ) => {
            let given_param_type = check_is_type(defs, ctx, given_param_type)?;
            let given_param_type = given_param_type.evaluate(defs, &Environment::from_context(ctx));
            unify_types(defs, ctx, param_type, &given_param_type)?;
            let fresh_var = ctx.fresh_var();
            let ret_type = tclosure.call(defs, &fresh_var);
            let ret_val = check_type(defs, &ctx.extend(param_type), ret_val, &ret_type)?;
            Ok(TypedExpression::create_typed(
                type_.clone(),
                CoreExpression::Lambda {
                    ret_val: Box::new(ret_val.into_wrapped()),
                },
            ))
        }
        _ => {
            let syn_type_expr = synth_type(defs, ctx, expr)?;
            unify_types(defs, ctx, type_, syn_type_expr.get_type())?;
            Ok(syn_type_expr)
        }
    }
}

pub fn check_is_type<'a>(
    defs: &mut Definitions<'a>,
    ctx: &Context<'a, '_>,
    expr: &Expression<'a>,
) -> super::Result<'a, TypeExpression<'a>> {
    check_type(defs, ctx, expr, &Type::UNIVERSE).map(type_wrapper::Typed::into_type)
}

/// Synthesizes a type for an expression, and forms a [`TypedExpression`] from them.
pub fn synth_type<'a>(
    defs: &mut Definitions<'a>,
    ctx: &Context<'a, '_>,
    expr: &Expression<'a>,
) -> super::Result<'a, TypedExpression<'a>> {
    use Expression::*;
    match expr {
        Underscore => {
            let type_ = defs.add_meta_type(ctx);
            let type_ = type_.evaluate(defs, &Environment::from_context(ctx));
            Ok(defs.add_meta(ctx, type_))
        }
        Variable(ev) => Ok(type_var(defs, ctx, *ev)),
        PiType {
            tparam_type,
            ret_type,
        } => {
            let tparam_type_expr = check_is_type(defs, ctx, tparam_type)?;
            let tparam_type = tparam_type_expr.evaluate(defs, &Environment::from_context(ctx));
            let ret_type = check_is_type(defs, &ctx.extend(&tparam_type), ret_type)?;
            Ok(TypedExpression::create_typed(
                Type::UNIVERSE,
                CoreExpression::PiType {
                    tparam_type: Box::new(tparam_type_expr),
                    ret_type: Box::new(ret_type),
                },
            ))
        }
        Lambda {
            param_type,
            ret_val,
        } => {
            let param_type_expr = check_is_type(defs, ctx, param_type)?;
            let param_type = param_type_expr.evaluate(defs, &Environment::from_context(ctx));
            let ret_val_expr = synth_type(defs, &ctx.extend(&param_type), ret_val)?;
            let ret_type = read_back_with_ctx_len(defs, ctx.len() + 1, ret_val_expr.get_type());
            Ok(TypedExpression::create_typed(
                Type::pi_type(param_type, Closure::new_in_ctx(ctx, ret_type)),
                CoreExpression::Lambda {
                    ret_val: Box::new(ret_val_expr.into_wrapped()),
                },
            ))
        }
        Application { func, arg } => {
            let arg_expr = synth_type(defs, ctx, arg)?;
            let candidate_ret_type = defs.add_meta_type(&ctx.extend(arg_expr.get_type()));
            let tclosure = Closure::new_in_ctx(ctx, candidate_ret_type);
            let candidate_func_type = Type::pi_type(arg_expr.get_type().clone(), tclosure.clone());
            let func = check_type(defs, ctx, func, &candidate_func_type)?;
            let arg = arg_expr.evaluate(defs, &Environment::from_context(ctx));
            let type_ = tclosure.call(defs, arg.get_term());
            Ok(TypedExpression::create_typed(
                type_,
                CoreExpression::Application {
                    func: Box::new(func.into_wrapped()),
                    arg: Box::new(arg_expr.into_wrapped()),
                },
            ))
        }
        Universe => Ok(TypedExpression::UNIVERSE),
        Annotation { expr, type_ } => {
            let type_ = check_is_type(defs, ctx, type_)?;
            let type_ = type_.evaluate(defs, &Environment::from_context(ctx));
            check_type(defs, ctx, expr, &type_)
        }
    }
}
