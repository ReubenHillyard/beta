//! Types for representing values.

use crate::typing::definitions::{Definitions, MetaVar};
use crate::typing::evaluation::do_apply;
use crate::typing::expression::{CoreExpression, TypeExpression};
use crate::typing::type_wrapper;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

pub use crate::typing::environment::Closure;
pub use crate::typing::environment::Level;

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
    Neutral(Neutral<'a>),
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::PiType {
                param_type,
                tclosure,
            } => write!(f, "(£ : {param_type}) -> {tclosure}"),
            Value::Lambda { closure } => write!(f, "£ => {closure}"),
            Value::Universe => write!(f, "Type"),
            Value::Neutral(neu) => write!(f, "{neu}"),
        }
    }
}

/// The record of elimination forms applied to a free variable.
#[derive(Clone, Debug)]
pub enum Neutral<'a> {
    Principal(Principal<'a>),
    Application {
        func: Box<Neutral<'a>>,
        arg: Box<Value<'a>>,
    },
}

impl Display for Neutral<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Neutral::*;
        match self {
            Principal(p) => write!(f, "{p}"),
            Application { func, arg, .. } => write!(f, "({func})({arg})"),
        }
    }
}

impl<'a> Neutral<'a> {
    /// Obtains the unknown that is blocking computation.
    pub fn principal(&self) -> Principal<'a> {
        use Neutral::*;
        match self {
            Principal(p) => *p,
            Application { func, .. } => func.principal(),
        }
    }
}

/// A variable or meta-variable.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Principal<'a> {
    Variable(VVariable<'a>),
    MetaVariable(MetaVar),
}

impl<'a> Display for Principal<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Principal::*;
        match self {
            Variable(var) => write!(f, "{var}"),
            MetaVariable(mv) => write!(f, "{mv}"),
        }
    }
}

impl<'a> Principal<'a> {
    /// Queries whether a [`Principal`] is a meta-variable.
    pub fn is_meta(&self) -> bool {
        matches!(self, Principal::MetaVariable(_))
    }
}

/// A variable that may appear in a value.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum VVariable<'a> {
    Global(&'a str),
    Local(Level),
}

impl<'a> Display for VVariable<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use VVariable::*;
        match self {
            Global(name) => write!(f, "{name}"),
            Local(level) => write!(f, "{level}"),
        }
    }
}

/// A value which is known to be a type.
pub type Type<'a> = type_wrapper::Type<Value<'a>>;

/// A pair of a type and a value of that type.
pub type TypedValue<'a> = type_wrapper::Typed<Value<'a>, Value<'a>>;

pub(crate) trait Force<'a> {
    fn force(&self, defs: &Definitions<'a>) -> Self;
}

impl<'a> Force<'a> for Value<'a> {
    fn force(&self, defs: &Definitions<'a>) -> Self {
        match self {
            Value::Neutral(neu) => neu.force(defs),
            _ => self.clone(),
        }
    }
}

impl<'a> Neutral<'a> {
    fn force(&self, defs: &Definitions<'a>) -> Value<'a> {
        use Neutral::*;
        match self {
            Principal(p) => p.force(defs),
            Application { func, arg } => {
                let func = func.force(defs);
                let arg = arg.force(defs);
                do_apply(defs, &func, &arg)
            }
        }
    }
}

impl<'a> Principal<'a> {
    fn force(&self, defs: &Definitions<'a>) -> Value<'a> {
        use Principal::*;
        match self {
            Variable(var) => Value::Neutral(Neutral::Principal(Variable(*var))),
            MetaVariable(mv) => defs.lookup_meta(*mv),
        }
    }
}
