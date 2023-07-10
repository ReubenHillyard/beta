use crate::typing::value::TypedValue;
use std::collections::HashMap;

/// A record of all global values.
pub struct Definitions<'a>(pub(crate) HashMap<&'a str, TypedValue<'a>>);

impl<'a> Definitions<'a> {
    pub fn lookup(&self, name: &'a str) -> &TypedValue<'a> {
        &self.0[name]
    }
}
