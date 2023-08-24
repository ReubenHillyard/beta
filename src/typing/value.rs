//! Types for representing values.

use crate::typing::checking::{CoreExpression, TypeExpression};
use crate::typing::definitions::{Definitions, MetaVar};
pub use crate::typing::environment::Closure;
pub use crate::typing::environment::{Level, VVariable};
use crate::typing::evaluation::do_apply;
use crate::typing::type_wrapper;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

/// The result of a computation.
#[derive(Clone, Debug)]
pub enum Value<'a> {
    PiType {
        param_type: Box<Type<'a>>,
        tclosure: Box<Closure<'a, TypeExpression<'a>>>,
    },
    Lambda {
        closure: Box<Closure<'a, CoreExpression<'a>>>,
    },
    Universe,
    Neutral(Neutral<'a, VVariable<'a>>),
    MetaNeutral(Neutral<'a, MetaVar>),
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::PiType {
                param_type,
                tclosure,
            } => write!(f, "(£ : {}) -> {}", param_type, tclosure),
            Value::Lambda { closure } => write!(f, "£ => {}", closure),
            Value::Universe => write!(f, "Type"),
            Value::Neutral(neu) => write!(f, "{neu}"),
            Value::MetaNeutral(neu) => write!(f, "{neu}"),
        }
    }
}

pub trait Force<'a> {
    fn force(&self, defs: &Definitions<'a>) -> Self;
}

impl<'a> Force<'a> for Value<'a> {
    fn force(&self, defs: &Definitions<'a>) -> Self {
        match self {
            Value::MetaNeutral(neu) => neu.force(defs),
            _ => self.clone(),
        }
    }
}

/// The record of elimination forms applied to a free variable.
#[derive(Clone, Debug)]
pub enum Neutral<'a, VarT> {
    Variable(VarT),
    Application {
        func: Box<Neutral<'a, VarT>>,
        arg: Box<Value<'a>>,
    },
}

impl<VarT: Display> Display for Neutral<'_, VarT> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Neutral::*;
        match self {
            Variable(var) => write!(f, "{var}"),
            Application { func, arg, .. } => write!(f, "({func})({arg})"),
        }
    }
}

impl<VarT> Neutral<'_, VarT> {
    pub fn get_principal(&self) -> &VarT {
        use Neutral::*;
        match self {
            Variable(var) => var,
            Application { func, .. } => func.get_principal(),
        }
    }
}

impl<'a> Neutral<'a, MetaVar> {
    fn force(&self, defs: &Definitions<'a>) -> Value<'a> {
        use Neutral::*;
        match self {
            Variable(mv) => defs.lookup_meta(*mv),
            Application { func, arg } => {
                let func = func.force(defs);
                let arg = arg.force(defs);
                do_apply(defs, &func, &arg)
            }
        }
    }
}

/// A value which is known to be a type.
pub type Type<'a> = type_wrapper::Type<Value<'a>>;

/// A pair of a type and a value of that type.
#[derive(Clone, Debug)]
pub struct TypedValue<'a> {
    type_: Type<'a>,
    value: Value<'a>,
}

impl<'a> TypedValue<'a> {
    pub(crate) const fn create_typed_value(type_: Type<'a>, value: Value<'a>) -> TypedValue<'a> {
        TypedValue { type_, value }
    }
    pub fn get_type(&self) -> &Type<'a> {
        &self.type_
    }
    pub fn get_value(&self) -> &Value<'a> {
        &self.value
    }
}
