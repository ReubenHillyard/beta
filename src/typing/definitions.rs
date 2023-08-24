use crate::typing::checking::{CoreExpression, TypeExpression, TypedExpression};
use crate::typing::environment::Context;
use crate::typing::value::{Force, Neutral, Type, TypedValue, Value};
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
    pub fn add_meta(&mut self, ctx: &Context<'a, '_>, type_: Type<'a>) -> TypedExpression<'a> {
        let mv = self.insert_meta(ctx.func_to(self, type_.clone()));
        TypedExpression::create_typed_expression(ctx.call(CoreExpression::MetaVariable(mv)), type_)
    }
    pub fn add_meta_type(&mut self, ctx: &Context<'a, '_>) -> TypeExpression<'a> {
        self.add_meta(ctx, Type::UNIVERSE).into_type_expr()
    }
    pub fn define_meta(&mut self, mv: MetaVar, value: Value<'a>) {
        let old = self.metas[mv.id].value.replace(value);
        assert!(old.is_none());
    }
    pub fn lookup_meta(&self, mv: MetaVar) -> Value<'a> {
        match &self.metas[mv.id].value {
            Some(value) => value.force(self),
            _ => Value::MetaNeutral(Neutral::Variable(mv)),
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
}
