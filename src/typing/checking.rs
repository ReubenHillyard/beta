//! Functions for type-checking and type-synthesis.

use crate::typing::ast::{EVariable, Expression};
use crate::typing::definitions::MetaVar;
use crate::typing::environments::{Context, Definitions, Environment};
use crate::typing::evaluation::{detail, evaluate};
use crate::typing::unification::unify;
use crate::typing::value::{Closure, Type, TypedValue, Value};
use derivative::Derivative;
use std::fmt;
use std::fmt::{Display, Formatter};

pub use crate::typing::environment::context::type_var;
use crate::typing::read_back::read_back_value;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CoreExpression<'a> {
    MetaVariable(MetaVar),
    Variable(EVariable<'a>),
    PiType {
        tparam_type: Box<CoreExpression<'a>>,
        ret_type: Box<CoreExpression<'a>>,
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
    detail::evaluate(defs, &Environment::EMPTY, &expr)
}

/// A typed expression.
#[derive(Clone, Debug, Derivative)]
#[derivative(Eq, PartialEq)]
pub struct TypedExpression<'a> {
    expr: CoreExpression<'a>,
    #[derivative(PartialEq = "ignore")]
    type_: Type<'a>,
}

impl Display for TypedExpression<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} as {}", self.expr, self.type_.as_value())
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
}

/// Checks that an expression has a given type, and forms a [`TypedExpression`] from them.
pub fn check_type<'a>(
    defs: &mut Definitions<'a>,
    ctx: &Context<'a, '_>,
    expr: &Expression<'a>,
    type_: &Type<'a>,
) -> super::Result<'a, TypedExpression<'a>> {
    use Expression::*;
    match (expr, type_.as_value()) {
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
            let given_param_type = check_type(defs, ctx, given_param_type, &Type::UNIVERSE)?;
            let given_param_type =
                evaluate(defs, &Environment::from_context(ctx), &given_param_type);
            unify(
                defs,
                ctx,
                &param_type.clone().into_typed_value(),
                &given_param_type,
            )?;
            let fresh_var = ctx.fresh_var();
            let ret_type = Type::create_type_from_value(TypedValue::create_typed_value(
                Type::UNIVERSE,
                tclosure.call(defs, &fresh_var),
            ));
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
            unify(
                defs,
                ctx,
                &type_.clone().into_typed_value(),
                &syn_type_expr.get_type().clone().into_typed_value(),
            )?;
            Ok(syn_type_expr)
        }
    }
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
            let type_ = defs.add_meta(ctx, Type::UNIVERSE);
            let type_ = evaluate(defs, &Environment::from_context(ctx), &type_);
            let type_ = Type::create_type_from_value(type_);
            Ok(defs.add_meta(ctx, type_))
        }
        Variable(ev) => Ok(type_var(defs, ctx, *ev)),
        PiType {
            tparam_type,
            ret_type,
        } => {
            let tparam_type_expr = check_type(defs, ctx, tparam_type, &Type::UNIVERSE)?;
            let tparam_type = Type::create_type_from_value(evaluate(
                defs,
                &Environment::from_context(ctx),
                &tparam_type_expr,
            ));
            let ret_type = check_type(defs, &ctx.extend(&tparam_type), ret_type, &Type::UNIVERSE)?;
            Ok(TypedExpression::create_typed_expression(
                CoreExpression::PiType {
                    tparam_type: Box::new(tparam_type_expr.expr),
                    ret_type: Box::new(ret_type.expr),
                },
                Type::UNIVERSE,
            ))
        }
        Lambda {
            param_type,
            ret_val,
        } => {
            let param_type_expr = check_type(defs, ctx, param_type, &Type::UNIVERSE)?;
            let param_type = Type::create_type_from_value(evaluate(
                defs,
                &Environment::from_context(ctx),
                &param_type_expr,
            ));
            let ret_val_expr = synth_type(defs, &ctx.extend(&param_type), ret_val)?;
            let ret_type = read_back_value(defs, ctx.len() + 1, ret_val_expr.get_type().as_value());
            Ok(TypedExpression::create_typed_expression(
                CoreExpression::Lambda {
                    ret_val: Box::new(ret_val_expr.expr),
                },
                Type::create_type_from_value(TypedValue::create_typed_value(
                    Type::UNIVERSE,
                    Value::PiType {
                        param_type: Box::new(param_type),
                        tclosure: Box::new(Closure::new_in_ctx(ctx, ret_type)),
                    },
                )),
            ))
        }
        Application { func, arg } => {
            let arg_expr = synth_type(defs, ctx, arg)?;
            let candidate_ret_type =
                defs.add_meta(&ctx.extend(arg_expr.get_type()), Type::UNIVERSE);
            let tclosure = Closure::new_in_ctx(ctx, candidate_ret_type.expr);
            let candidate_func_type = Value::PiType {
                param_type: Box::new(arg_expr.get_type().clone()),
                tclosure: Box::new(tclosure.clone()),
            };
            let candidate_func_type = Type::create_type_from_value(TypedValue::create_typed_value(
                Type::UNIVERSE,
                candidate_func_type,
            ));
            let func = check_type(defs, ctx, func, &candidate_func_type)?;
            let arg = evaluate(defs, &Environment::from_context(ctx), &arg_expr);
            let type_ = Type::create_type_from_value(TypedValue::create_typed_value(
                Type::UNIVERSE,
                tclosure.call(defs, arg.get_value()),
            ));
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
            let type_ = check_type(defs, ctx, type_, &Type::UNIVERSE)?;
            let type_ = Type::create_type_from_value(evaluate(
                defs,
                &Environment::from_context(ctx),
                &type_,
            ));
            check_type(defs, ctx, expr, &type_)
        }
    }
}
