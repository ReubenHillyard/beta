//! Functions for type-checking and type-synthesis.

use crate::typing::ast::Expression;
use crate::typing::environments::DefsWithCtx;
use crate::typing::expression::{CoreExpression, TypeExpression, TypedExpression};
use crate::typing::read_back::read_back_with_ctx_len;
use crate::typing::type_wrapper;
use crate::typing::type_wrapper::Term;
use crate::typing::unification::unify_types;
use crate::typing::value::{Closure, Type, Value};

pub use crate::typing::environment::type_var;

/// Checks that an expression has a given type, and forms a [`TypedExpression`] from them.
pub fn check_type<'a>(
    defs_ctx: &mut DefsWithCtx<'a, '_>,
    expr: &Expression<'a>,
    type_: &Type<'a>,
) -> super::Result<'a, TypedExpression<'a>> {
    use Expression::*;
    match (expr, type_.wrapped()) {
        (Underscore, _) => Ok(defs_ctx.add_meta(type_.clone())),
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
            let given_param_type = check_is_type(defs_ctx, given_param_type)?;
            let given_param_type = defs_ctx.evaluate(&given_param_type);
            unify_types(defs_ctx, param_type, &given_param_type)?;
            let fresh_var = defs_ctx.ctx.fresh_var();
            let ret_type = tclosure.call(defs_ctx.defs, &fresh_var);
            let ret_val = check_type(&mut defs_ctx.extend(param_type), ret_val, &ret_type)?;
            Ok(TypedExpression::create_typed(
                type_.clone(),
                CoreExpression::Lambda {
                    ret_val: Box::new(ret_val.into_wrapped()),
                },
            ))
        }
        _ => {
            let syn_type_expr = synth_type(defs_ctx, expr)?;
            unify_types(defs_ctx, type_, syn_type_expr.get_type())?;
            Ok(syn_type_expr)
        }
    }
}

pub fn check_is_type<'a>(
    defs_ctx: &mut DefsWithCtx<'a, '_>,
    expr: &Expression<'a>,
) -> super::Result<'a, TypeExpression<'a>> {
    check_type(defs_ctx, expr, &Type::UNIVERSE).map(type_wrapper::Typed::term_into_type)
}

/// Synthesizes a type for an expression, and forms a [`TypedExpression`] from them.
pub fn synth_type<'a>(
    defs_ctx: &mut DefsWithCtx<'a, '_>,
    expr: &Expression<'a>,
) -> super::Result<'a, TypedExpression<'a>> {
    use Expression::*;
    match expr {
        Underscore => {
            let type_ = defs_ctx.add_meta_type();
            let type_ = defs_ctx.evaluate(&type_);
            Ok(defs_ctx.add_meta(type_))
        }
        Variable(ev) => Ok(TypedExpression::create_typed(
            type_var(defs_ctx, *ev),
            CoreExpression::Variable(*ev),
        )),
        PiType {
            tparam_type,
            ret_type,
        } => {
            let tparam_type_expr = check_is_type(defs_ctx, tparam_type)?;
            let tparam_type = defs_ctx.evaluate(&tparam_type_expr);
            let ret_type = check_is_type(&mut defs_ctx.extend(&tparam_type), ret_type)?;
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
            let param_type_expr = check_is_type(defs_ctx, param_type)?;
            let param_type = defs_ctx.evaluate(&param_type_expr);
            let ret_val_expr = synth_type(&mut defs_ctx.extend(&param_type), ret_val)?;
            let ret_type = read_back_with_ctx_len(
                defs_ctx.defs,
                defs_ctx.ctx.len() + 1,
                ret_val_expr.get_type(),
            );
            Ok(TypedExpression::create_typed(
                Type::pi_type(param_type, Closure::new_in_ctx(&defs_ctx.ctx, ret_type)),
                CoreExpression::Lambda {
                    ret_val: Box::new(ret_val_expr.into_wrapped()),
                },
            ))
        }
        Application { func, arg } => {
            let arg_expr = synth_type(defs_ctx, arg)?;
            let candidate_ret_type = defs_ctx.extend(arg_expr.get_type()).add_meta_type();
            let tclosure = Closure::new_in_ctx(&defs_ctx.ctx, candidate_ret_type);
            let candidate_func_type = Type::pi_type(arg_expr.get_type().clone(), tclosure.clone());
            let func = check_type(defs_ctx, func, &candidate_func_type)?;
            let arg = defs_ctx.evaluate(&arg_expr);
            let type_ = tclosure.call(defs_ctx.defs, arg.get_term());
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
            let type_ = check_is_type(defs_ctx, type_)?;
            let type_ = defs_ctx.evaluate(&type_);
            check_type(defs_ctx, expr, &type_)
        }
    }
}
