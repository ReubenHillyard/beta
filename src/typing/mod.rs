//! Types and functions for type-checking.

pub mod abstraction;
pub mod ast;
pub mod checking;
#[doc(hidden)]
mod definitions;
mod environment;
pub mod evaluation;
pub mod expression;
pub mod read_back;
pub(crate) mod renaming;
#[doc(hidden)]
mod type_error;
pub mod type_wrapper;
pub mod unification;
pub mod value;

pub use type_error::*;

/// Types representing the environments where typing or evaluation take place.
pub mod environments {
    pub use super::definitions::*;
    pub use crate::typing::environment::Context;
    pub use crate::typing::environment::Environment;
}

#[cfg(test)]
#[doc(hidden)]
pub(crate) mod tests;
