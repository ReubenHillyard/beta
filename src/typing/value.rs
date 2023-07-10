//! Types for representing values.

pub use crate::typing::environment::{Closure, Level, VVariable};
use std::fmt;

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
        neu: Neutral<'a>,
    },
}

/// The record of elimination forms applied to a free variable.
#[derive(Clone, Debug)]
pub enum Neutral<'a> {
    Variable(VVariable<'a>),
    Application {
        func: Box<Neutral<'a>>,
        arg: Box<Value<'a>>,
    },
}

/// A value which is known to be a type.
#[derive(Clone, Debug)]
pub struct Type<'a>(pub(crate) Value<'a>);

/// A pair of a type and a value of that type.
pub struct TypedValue<'a> {
    type_: Type<'a>,
    value: Value<'a>,
}

impl<'a> TypedValue<'a> {
    pub fn get_type(&self) -> &Type<'a> {
        &self.type_
    }
    pub fn get_value(&self) -> &Value<'a> {
        &self.value
    }
}
