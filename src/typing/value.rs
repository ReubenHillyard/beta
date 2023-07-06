//! Types for representing values.

use crate::typing::environment::Closure;

/// The position of a bound variable, counting from the left of the context.
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

/// The result of a computation.
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

/// The record of elimination forms applied to a free variable.
#[derive(Clone)]
pub enum Neutral<'a> {
    Variable(VVariable<'a>),
    Application {
        func: Box<Neutral<'a>>,
        arg: Box<Value<'a>>,
    },
}

/// A value which is known to be a type.
#[derive(Clone)]
pub struct Type<'a>(Value<'a>);

/// A pair of a type and a value of that type.
pub struct TypedValue<'a> {
    type_: Type<'a>,
    value: Value<'a>,
}
