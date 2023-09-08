use crate::typing::environment::{Context, Environment};
use crate::typing::evaluation::Evaluate;
use crate::typing::expression::{CoreExpression, TypeExpression, TypedExpression};
use crate::typing::type_wrapper::Term;
use crate::typing::value::{Force, Neutral, Principal, Type, TypedValue, Value};
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Display, Formatter};

/// A meta-variable representing an unknown to be solved for.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub struct MetaVar {
    id: usize,
}

impl Display for MetaVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "?{}", self.id)
    }
}

/// The [`Type`] of a meta-variable, and its [`Value`] if solved-for.
#[derive(Debug)]
struct MetaVarInfo<'a> {
    type_: Type<'a>,
    value: Option<Value<'a>>,
}

/// A record of all global values and meta-variables.
#[derive(Default, Debug)]
pub struct Definitions<'a> {
    globals: HashMap<&'a str, TypedValue<'a>>,
    metas: Vec<MetaVarInfo<'a>>,
}

impl<'a> Definitions<'a> {
    /// Obtains a reference to a global value.
    ///
    /// Panics if `name` does not name a global value in scope.
    pub fn lookup_global(&self, name: &'a str) -> &TypedValue<'a> {
        &self.globals[name]
    }

    /// Defines a global value.
    ///
    /// Panics if `name` names an already-defined global value.
    pub fn define_global(&mut self, name: &'a str, typed_value: TypedValue<'a>) {
        let old = self.globals.insert(name, typed_value);
        assert!(old.is_none());
    }

    /// Inserts a new meta-variable with a given [`Type`].
    ///
    /// Requires `type_` is a valid [`Type`] in the empty [`Context`].
    fn insert_meta(&mut self, type_: Type<'a>) -> MetaVar {
        let id = self.metas.len();
        self.metas.push(MetaVarInfo { type_, value: None });
        MetaVar { id }
    }

    /// Provides a definition for a meta-variable.
    ///
    /// Requires `value` is a valid [`Value`] of the type of `mv` in the empty [`Context`].
    ///
    /// Panics if `mv` is not in scope, or is already defined.
    pub fn define_meta(&mut self, mv: MetaVar, value: Value<'a>) {
        let old = self.metas[mv.id].value.replace(value);
        assert!(old.is_none());
    }

    /// Obtains the value of a meta-variable if defined, or else returns the meta-variable.
    ///
    /// Panics if `mv` is not in scope.
    pub fn lookup_meta(&self, mv: MetaVar) -> Value<'a> {
        match &self.metas[mv.id].value {
            Some(value) => value.force(self),
            _ => Value::Neutral(Neutral::Principal(Principal::MetaVariable(mv))),
        }
    }

    /// Obtains the type of a meta-variable.
    ///
    /// Panics if `mv` is not in scope.
    pub fn type_meta(&self, mv: MetaVar) -> Type<'a> {
        self.metas[mv.id].type_.force(self)
    }

    /// Discards all meta-variables.
    pub fn reset_metas(&mut self) {
        self.metas.clear()
    }

    /// Queries whether all meta-variables have been solved-for.
    pub fn all_metas_defined(&self) -> bool {
        self.metas.iter().all(|meta| meta.value.is_some())
    }

    /// Adjoins the empty [`Context`] to make a [`DefsWithCtx`].
    pub fn with_empty_ctx<'b>(&'b mut self) -> DefsWithCtx<'a, 'b> {
        DefsWithCtx {
            defs: self,
            ctx: Context::EMPTY,
        }
    }

    /// Adjoins the empty [`Environment`] to make a [`DefsWithEnv`].
    pub fn with_empty_env<'b>(&'b self) -> DefsWithEnv<'a, 'b> {
        DefsWithEnv {
            defs: self,
            env: &Environment::EMPTY,
        }
    }
}

/// A pair of [`Definitions`] and [`Context`].
pub struct DefsWithCtx<'a, 'b> {
    pub defs: &'b mut Definitions<'a>,
    pub ctx: Context<'a, 'b>,
}

/// A pair of [`Definitions`] and [`Environment`].
#[derive(Copy, Clone)]
pub struct DefsWithEnv<'a, 'b> {
    pub defs: &'b Definitions<'a>,
    pub env: &'b Environment<'a, 'b>,
}

impl<'a> DefsWithCtx<'a, '_> {
    /// Extends the [`Context`] with a variable of given [`Type`].
    ///
    /// Requires `type_` is a valid [`Type`] in context of `self`.
    pub fn extend<'b>(&'b mut self, type_: &'b Type<'a>) -> DefsWithCtx<'a, 'b> {
        DefsWithCtx {
            defs: self.defs,
            ctx: self.ctx.extend(type_),
        }
    }

    /// Creates a new meta-variable with a given [`Type`].
    ///
    /// Requires `type_` is a valid [`Type`] in context of `self`.
    pub fn add_meta(&mut self, type_: Type<'a>) -> TypedExpression<'a> {
        let mv = self.defs.insert_meta(self.func_to(&type_));
        TypedExpression::create_typed(type_, self.ctx.call(CoreExpression::MetaVariable(mv)))
    }

    /// Creates a new type-valued meta-variable.
    pub fn add_meta_type(&mut self) -> TypeExpression<'a> {
        self.add_meta(Type::UNIVERSE).term_into_type()
    }

    /// Evaluates an expression in the environment associated to the context.
    ///
    /// Requires `e` is a valid term in context of `self`.
    pub fn evaluate<E: Evaluate<'a>>(&self, e: &E) -> E::ValueT {
        e.evaluate(DefsWithEnv {
            defs: self.defs,
            env: &Environment::from_context(&self.ctx),
        })
    }
}
