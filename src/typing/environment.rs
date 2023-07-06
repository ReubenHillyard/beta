//! Types representing the environments where typing or evaluation take place.

use crate::typing::ast::Expression;
use crate::typing::value::{Type, TypedValue, Value};
use std::collections::HashMap;

/// A record of all global values.
pub struct Definitions<'a>(HashMap<&'a str, TypedValue<'a>>);

enum ContextInner<'a> {
    Empty,
    Extend {
        parent: &'a ContextInner<'a>,
        val: &'a Type<'a>,
    },
}

/// A typing context.
pub struct Context<'a>(ContextInner<'a>);

#[derive(Clone)]
struct FlatEnvironment<'a>(Vec<Value<'a>>);

enum EnvironmentInner<'a> {
    From(&'a Context<'a>),
    Extend {
        parent: &'a FlatEnvironment<'a>,
        val: &'a Value<'a>,
    },
}

/// An evaluation environment.
pub struct Environment<'a>(EnvironmentInner<'a>);

/// A value which depends on arguments.
#[derive(Clone)]
pub struct Closure<'a> {
    env: FlatEnvironment<'a>,
    body: Expression<'a>,
}
