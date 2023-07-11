//! Types and functions for type-checking.

pub mod ast;
#[doc(hidden)]
mod definitions;
pub(crate) mod environment;
pub mod evaluation;
pub mod read_back;
pub mod value;

/// Types representing the environments where typing or evaluation take place.
pub mod environments {
    pub use super::definitions::*;
    pub use super::environment::context::{Context, Environment};
}
