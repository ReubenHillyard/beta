//! Types for representing values.

use crate::typing::definitions::{Definitions, MetaVar};
pub use crate::typing::environment::Closure;
pub use crate::typing::environment::{Level, VVariable};
use crate::typing::evaluation::do_apply;
use std::fmt;
use std::fmt::Debug;

/// The result of a computation.
#[derive(Clone, Debug)]
pub enum Value<'a> {
    PiType {
        param_type: Box<Type<'a>>,
        tclosure: Box<Closure<'a>>,
    },
    Lambda {
        closure: Box<Closure<'a>>,
    },
    Universe,
    Neutral(Neutral<'a, VVariable<'a>>),
    MetaNeutral(Neutral<'a, MetaVar>),
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::PiType {
                param_type,
                tclosure,
            } => write!(f, "(£ : {}) -> {}", param_type.as_value(), tclosure),
            Value::Lambda { closure } => write!(f, "£ => {}", closure),
            Value::Universe => write!(f, "Type"),
            Value::Neutral(neu) => write!(f, "{neu}"),
            Value::MetaNeutral(neu) => write!(f, "{neu}"),
        }
    }
}

impl<'a> Value<'a> {
    pub(crate) fn force(&self, defs: &Definitions<'a>) -> Value<'a> {
        match self {
            Value::MetaNeutral(neu) => neu.force(defs),
            _ => self.clone(),
        }
    }
    pub fn as_str(&self) -> String {
        self.to_string()
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

impl<VarT: fmt::Display> fmt::Display for Neutral<'_, VarT> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
#[derive(Clone, Debug)]
pub struct Type<'a>(Value<'a>);

impl<'a> Type<'a> {
    pub const UNIVERSE: Type<'static> = Type(Value::Universe);
    pub(crate) fn create_type_from_value(value: TypedValue<'a>) -> Type<'a> {
        Type(value.value)
    }
    pub fn as_value(&self) -> &Value<'a> {
        &self.0
    }
    pub fn into_typed_value(self) -> TypedValue<'a> {
        TypedValue {
            type_: Type::UNIVERSE,
            value: self.0,
        }
    }
    pub fn force(&self, defs: &Definitions<'a>) -> Type<'a> {
        Type(self.0.force(defs))
    }
}

/// A pair of a type and a value of that type.
#[derive(Clone, Debug)]
pub struct TypedValue<'a> {
    type_: Type<'a>,
    value: Value<'a>,
}

impl<'a> TypedValue<'a> {
    pub(crate) fn create_typed_value(type_: Type<'a>, value: Value<'a>) -> TypedValue<'a> {
        TypedValue { type_, value }
    }
    pub fn get_type(&self) -> &Type<'a> {
        &self.type_
    }
    pub fn get_value(&self) -> &Value<'a> {
        &self.value
    }
}
