//! Types and functions for type-checking.

pub mod ast;
pub mod checking;
#[doc(hidden)]
mod definitions;
pub(crate) mod environment;
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
    pub use super::environment::context::{Context, Environment};
}

#[cfg(test)]
#[doc(hidden)]
mod tests;
