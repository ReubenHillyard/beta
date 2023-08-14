//! Types and functions for type-checking.

pub mod ast;
pub mod checking;
#[doc(hidden)]
mod definitions;
mod environment;
pub mod evaluation;
pub mod read_back;
#[doc(hidden)]
mod type_error;
pub mod unification;
pub mod value;

pub use type_error::*;

/// Types representing the environments where typing or evaluation take place.
pub mod environments {
    pub use super::definitions::*;
    pub use crate::typing::environment::Context;
    pub use crate::typing::environment::Environment;
}

pub(crate) mod abstraction;

#[cfg(test)]
#[doc(hidden)]
mod tests;
