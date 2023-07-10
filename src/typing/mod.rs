//! Types and functions for type-checking.

pub mod ast;
mod definitions;
mod environment;
pub mod evaluation;
pub mod value;

pub mod environments {
    pub use super::definitions::*;
    pub(crate) use super::environment::test_evaluate;
    pub use super::environment::{Context, Environment};
}
