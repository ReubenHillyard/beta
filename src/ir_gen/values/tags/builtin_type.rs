//! Traits for built-in types.

use crate::ir_gen::values::tags::function::Function;
use crate::ir_gen::values::tags::{tag, Object};

/// A trait for types with an associated function of a given type.
pub trait HasAssociatedFn<F: Function>: Object {
    /// The name of the function.
    const NAMED: &'static str;

    /// Returns the name of the function.
    ///
    /// Must immediately return [`Self::NAMED`].
    fn get_name_of(_f: F) -> &'static str {
        Self::NAMED
    }
}

/// A trait for built-in types.
///
/// Built-in types must have associated special members: size, clone, and destroy functions.
pub trait BuiltinType:
HasAssociatedFn<tag::SizeFn> + HasAssociatedFn<tag::CloneFn> + HasAssociatedFn<tag::DestroyFn>
{}

impl HasAssociatedFn<tag::SizeFn> for tag::Universe {
    const NAMED: &'static str = "universe_size";
}

impl HasAssociatedFn<tag::CloneFn> for tag::Universe {
    const NAMED: &'static str = "universe_clone";
}

impl HasAssociatedFn<tag::DestroyFn> for tag::Universe {
    const NAMED: &'static str = "universe_destroy";
}

impl BuiltinType for tag::Universe {}

impl HasAssociatedFn<tag::SizeFn> for tag::Pi {
    const NAMED: &'static str = "pi_size";
}

impl HasAssociatedFn<tag::CloneFn> for tag::Pi {
    const NAMED: &'static str = "pi_clone";
}

impl HasAssociatedFn<tag::DestroyFn> for tag::Pi {
    const NAMED: &'static str = "pi_destroy";
}

impl BuiltinType for tag::Pi {}
