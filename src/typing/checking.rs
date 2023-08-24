//! Functions for type-checking and type-synthesis.

use crate::typing::ast::{EVariable, Expression};
use crate::typing::definitions::MetaVar;
use crate::typing::environment::{Closure, Context, Environment};
use crate::typing::environments::Definitions;
use crate::typing::evaluation::Evaluate;
use crate::typing::unification::unify_types;
use crate::typing::value::{Type, Value};
use std::fmt;
use std::fmt::{Display, Formatter};

pub use crate::typing::environment::type_var;
use crate::typing::read_back::read_back_type;
use crate::typing::type_wrapper;

#[derive(Clone, Debug)]
pub enum CoreExpression<'a> {
    MetaVariable(MetaVar),
    Variable(EVariable<'a>),
    PiType {
        tparam_type: Box<TypeExpression<'a>>,
        ret_type: Box<TypeExpression<'a>>,
    },
    Lambda {
        ret_val: Box<CoreExpression<'a>>,
    },
    Application {
        func: Box<CoreExpression<'a>>,
        arg: Box<CoreExpression<'a>>,
    },
    Universe,
}

impl<'a> Display for CoreExpression<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use CoreExpression::*;
        match self {
            MetaVariable(mv) => write!(f, "{mv}"),
            Variable(ev) => write!(f, "{ev}"),
            PiType {
                tparam_type,
                ret_type,
            } => write!(f, "($ : {tparam_type}) -> {ret_type}"),
            Lambda { ret_val } => write!(f, "$ => {ret_val}"),
            Application { func, arg } => write!(f, "({func})({arg})"),
            Universe => write!(f, "Type"),
        }
    }
}

pub fn lambdas_to<'a>(defs: &Definitions<'a>, n: usize, mut expr: CoreExpression<'a>) -> Value<'a> {
    for _ in 0..n {
        expr = CoreExpression::Lambda {
            ret_val: Box::new(expr),
        }
    }
    expr.evaluate(defs, &Environment::EMPTY)
}

/// A typed expression.
#[derive(Clone, Debug)]
pub struct TypedExpression<'a> {
    expr: CoreExpression<'a>,
    type_: Type<'a>,
}

impl Display for TypedExpression<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} as {}", self.expr, self.type_)
    }
}

impl<'a> TypedExpression<'a> {
    pub const UNIVERSE: TypedExpression<'static> = TypedExpression {
        expr: CoreExpression::Universe,
        type_: Type::UNIVERSE,
    };
    pub fn get_expr(&self) -> &CoreExpression<'a> {
        &self.expr
    }
    pub fn get_type(&self) -> &Type<'a> {
        &self.type_
    }
    pub(crate) fn create_typed_expression(
        expr: CoreExpression<'a>,
        type_: Type<'a>,
    ) -> TypedExpression<'a> {
        TypedExpression { expr, type_ }
    }
    pub(crate) fn into_type_expr(self) -> TypeExpression<'a> {
        TypeExpression::create_type(self.expr)
    }
}

pub type TypeExpression<'a> = type_wrapper::Type<CoreExpression<'a>>;

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
            Ok(TypedExpression::create_typed_expression(
                CoreExpression::Lambda {
                    ret_val: Box::new(ret_val.expr),
                },
                type_.clone(),
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
    check_type(defs, ctx, expr, &Type::UNIVERSE).map(|te| TypeExpression::create_type(te.expr))
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
            Ok(TypedExpression::create_typed_expression(
                CoreExpression::PiType {
                    tparam_type: Box::new(tparam_type_expr),
                    ret_type: Box::new(ret_type),
                },
                Type::UNIVERSE,
            ))
        }
        Lambda {
            param_type,
            ret_val,
        } => {
            let param_type_expr = check_is_type(defs, ctx, param_type)?;
            let param_type = param_type_expr.evaluate(defs, &Environment::from_context(ctx));
            let ret_val_expr = synth_type(defs, &ctx.extend(&param_type), ret_val)?;
            let ret_type = read_back_type(defs, ctx.len() + 1, ret_val_expr.get_type());
            Ok(TypedExpression::create_typed_expression(
                CoreExpression::Lambda {
                    ret_val: Box::new(ret_val_expr.expr),
                },
                Type::create_type(Value::PiType {
                    param_type: Box::new(param_type),
                    tclosure: Box::new(Closure::new_in_ctx(ctx, ret_type)),
                }),
            ))
        }
        Application { func, arg } => {
            let arg_expr = synth_type(defs, ctx, arg)?;
            let candidate_ret_type = defs.add_meta_type(&ctx.extend(arg_expr.get_type()));
            let tclosure = Closure::new_in_ctx(ctx, candidate_ret_type);
            let candidate_func_type = Value::PiType {
                param_type: Box::new(arg_expr.get_type().clone()),
                tclosure: Box::new(tclosure.clone()),
            };
            let candidate_func_type = Type::create_type(candidate_func_type);
            let func = check_type(defs, ctx, func, &candidate_func_type)?;
            let arg = arg_expr.evaluate(defs, &Environment::from_context(ctx));
            let type_ = tclosure.call(defs, arg.get_value());
            Ok(TypedExpression::create_typed_expression(
                CoreExpression::Application {
                    func: Box::new(func.expr),
                    arg: Box::new(arg_expr.expr),
                },
                type_,
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
