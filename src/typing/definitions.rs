use crate::typing::environment::{Context, Environment};
use crate::typing::evaluation::Evaluate;
use crate::typing::expression::{CoreExpression, TypeExpression, TypedExpression};
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

#[derive(Debug)]
struct MetaVarInfo<'a> {
    type_: Type<'a>,
    value: Option<Value<'a>>,
}

/// A record of all global values.
#[derive(Default, Debug)]
pub struct Definitions<'a> {
    globals: HashMap<&'a str, TypedValue<'a>>,
    metas: Vec<MetaVarInfo<'a>>,
}

impl<'a> Definitions<'a> {
    pub fn lookup_global(&self, name: &'a str) -> &TypedValue<'a> {
        &self.globals[name]
    }
    pub fn insert_global(&mut self, name: &'a str, typed_value: TypedValue<'a>) {
        self.globals.insert(name, typed_value);
    }
    fn insert_meta(&mut self, type_: Type<'a>) -> MetaVar {
        let id = self.metas.len();
        self.metas.push(MetaVarInfo { type_, value: None });
        MetaVar { id }
    }
    pub fn define_meta(&mut self, mv: MetaVar, value: Value<'a>) {
        let old = self.metas[mv.id].value.replace(value);
        assert!(old.is_none());
    }
    pub fn lookup_meta(&self, mv: MetaVar) -> Value<'a> {
        match &self.metas[mv.id].value {
            Some(value) => value.force(self),
            _ => Value::Neutral(Neutral::Principal(Principal::MetaVariable(mv))),
        }
    }
    pub fn type_meta(&self, mv: MetaVar) -> Type<'a> {
        self.metas[mv.id].type_.force(self)
    }
    pub fn reset_metas(&mut self) {
        self.metas.clear()
    }
    pub fn all_metas_defined(&self) -> bool {
        self.metas.iter().all(|meta| meta.value.is_some())
    }
    pub fn with_empty_ctx<'b>(&'b mut self) -> DefsWithCtx<'a, 'b> {
        DefsWithCtx {
            defs: self,
            ctx: Context::EMPTY,
        }
    }
    pub fn with_empty_env<'b>(&'b mut self) -> DefsWithEnv<'a, 'b> {
        DefsWithEnv {
            defs: self,
            env: &Environment::EMPTY,
        }
    }
}

pub struct DefsWithCtx<'a, 'b> {
    pub defs: &'b mut Definitions<'a>,
    pub ctx: Context<'a, 'b>,
}

#[derive(Copy, Clone)]
pub struct DefsWithEnv<'a, 'b> {
    pub defs: &'b Definitions<'a>,
    pub env: &'b Environment<'a, 'b>,
}

impl<'a> DefsWithCtx<'a, '_> {
    pub fn extend<'b>(&'b mut self, type_: &'b Type<'a>) -> DefsWithCtx<'a, 'b> {
        DefsWithCtx {
            defs: self.defs,
            ctx: self.ctx.extend(type_),
        }
    }
    pub fn add_meta(&mut self, type_: Type<'a>) -> TypedExpression<'a> {
        let mv = self.defs.insert_meta(self.func_to(&type_));
        TypedExpression::create_typed(type_, self.ctx.call(CoreExpression::MetaVariable(mv)))
    }
    pub fn add_meta_type(&mut self) -> TypeExpression<'a> {
        self.add_meta(Type::UNIVERSE).term_into_type()
    }
    pub fn evaluate<E: Evaluate<'a>>(&self, e: &E) -> E::ValueT {
        e.evaluate(DefsWithEnv {
            defs: self.defs,
            env: &Environment::from_context(&self.ctx),
        })
    }
}
