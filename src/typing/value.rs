//! Types for representing values.

use crate::typing::environment::Closure;

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Level {
    level: usize,
}

/// A variable that may appear in a value.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum VVariable<'a> {
    Global(&'a str),
    Local(Level),
}

#[derive(Clone)]
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

#[derive(Clone)]
pub enum Neutral<'a> {
    Variable(VVariable<'a>),
    Application {
        func: Box<Neutral<'a>>,
        arg: Box<Value<'a>>,
    },
}

#[derive(Clone)]
pub struct Type<'a>(Value<'a>);

pub struct TypedValue<'a> {
    type_: Type<'a>,
    value: Value<'a>,
}
