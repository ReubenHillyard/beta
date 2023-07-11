//! Implements various things `use`d elsewhere.

use std::fmt;

/// The position of a bound variable, counting from the right of the context.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Index {
    index: usize,
}

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.index)
    }
}

/// The position of a bound variable, counting from the left of the context.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Level {
    level: usize,
}

/// A variable that may appear in an expression.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum EVariable<'a> {
    Global(&'a str),
    Local(Index),
}

impl<'a> fmt::Display for EVariable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use EVariable::*;
        match self {
            Global(name) => name.fmt(f),
            Local(index) => index.fmt(f),
        }
    }
}

/// A variable that may appear in a value.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum VVariable<'a> {
    Global(&'a str),
    Local(Level),
}

pub(crate) mod abstraction;

pub(crate) mod context;
