//! Types for convenient expressions.

use crate::typing::ast::EVariable;
use crate::typing::definitions::{Definitions, MetaVar};
use crate::typing::evaluation::Evaluate;
use crate::typing::type_wrapper;
use crate::typing::value::Value;
use std::fmt;
use std::fmt::{Display, Formatter};

/// An expression elaborated to the most convenient form for evaluation.
#[derive(Clone, Debug, Eq, PartialEq)]
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

/// Wraps a given [`CoreExpression`] in a given number of lambdas.
pub fn lambdas_to<'a>(defs: &Definitions<'a>, n: usize, mut expr: CoreExpression<'a>) -> Value<'a> {
    for _ in 0..n {
        expr = CoreExpression::Lambda {
            ret_val: Box::new(expr),
        }
    }
    expr.evaluate(defs.with_empty_env())
}

/// An expression which is known to be a type.
pub type TypeExpression<'a> = type_wrapper::Type<CoreExpression<'a>>;

/// A pair of a type and an expression of that type.
pub type TypedExpression<'a> = type_wrapper::Typed<Value<'a>, CoreExpression<'a>>;
