//! Types for representing values.

pub use crate::typing::environment::context::Closure;
pub use crate::typing::environment::{Level, VVariable};

/// The result of a computation.
#[derive(Clone, Debug)]
pub enum Value<'a> {
    PiType {
        param_type: Box<Type<'a>>,
        tclosure: Closure<'a>,
    },
    Lambda {
        closure: Closure<'a>,
    },
    Universe,
    Neutral {
        type_: Box<Type<'a>>,
        neu: Neutral<'a>,
    },
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
