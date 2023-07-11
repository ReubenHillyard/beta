use crate::typing::value::TypedValue;
use std::collections::HashMap;

/// A record of all global values.
#[derive(Default)]
pub struct Definitions<'a>(HashMap<&'a str, TypedValue<'a>>);

impl<'a> Definitions<'a> {
    pub fn lookup(&self, name: &'a str) -> &TypedValue<'a> {
        &self.0[name]
    }
}
