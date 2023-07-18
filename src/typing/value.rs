//! Types for representing values.

pub use crate::typing::environment::context::Closure;
pub use crate::typing::environment::{Level, VVariable};
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
    Neutral {
        type_: Box<Type<'a>>,
        neu: Neutral<'a>,
    },
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
            Value::Neutral { neu, .. } => <Neutral as fmt::Display>::fmt(neu, f),
        }
    }
}

impl Value<'_> {
    pub fn as_str(&self) -> String {
        self.to_string()
    }
}

/// The record of elimination forms applied to a free variable.
#[derive(Clone, Debug)]
pub enum Neutral<'a> {
    Variable(VVariable<'a>),
    Application {
        func: Box<Neutral<'a>>,
        arg: Box<TypedValue<'a>>,
    },
}

impl fmt::Display for Neutral<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Neutral::*;
        match self {
            Variable(vv) => write!(f, "{}", vv),
            Application { func, arg, .. } => write!(f, "({})({})", func, arg.get_value()),
        }
    }
}

/// A value which is known to be a type.
#[derive(Clone, Debug)]
pub struct Type<'a>(Value<'a>);

impl<'a> Type<'a> {
    pub const UNIVERSE: Type<'static> = Type(Value::Universe);
    pub(crate) fn create_type_from_value(value: Value<'a>) -> Type<'a> {
        Type(value)
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
