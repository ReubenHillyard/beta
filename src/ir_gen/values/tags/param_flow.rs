//! Trait for read-write permissions of a parameter, and implementors.

use crate::ir_gen::values::tags::attributes::enum_attributes::{NoAlias, ReadOnly, WriteOnly};
use crate::ir_gen::values::tags::attributes::EnumAttribute;
use crate::ir_gen::values::tags::flow::{Flow, Read, Write};

/// Slice of attributes.
type Attributes = &'static [&'static dyn EnumAttribute];

/// Trait for read-write permissions of a parameter.
pub trait ParamFlow: Flow {
    /// Slice of attributes to be applied to the parameter.
    const ATTRIBUTES: Attributes;
}

impl ParamFlow for Read {
    const ATTRIBUTES: Attributes = &[&ReadOnly];
}

impl ParamFlow for Write {
    const ATTRIBUTES: Attributes = &[&WriteOnly, &NoAlias];
}
