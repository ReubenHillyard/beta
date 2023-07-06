//! Types for representing values.

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


