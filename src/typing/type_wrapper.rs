use crate::typing::checking::CoreExpression;
use crate::typing::definitions::Definitions;
use crate::typing::environments::Environment;
use crate::typing::evaluation::Evaluate;
use crate::typing::value::{Force, TypedValue, Value};
use std::fmt;
use std::fmt::{Display, Formatter};

pub trait Term {
    const UNIVERSE: Self;
}

impl<'a> Term for CoreExpression<'a> {
    const UNIVERSE: Self = CoreExpression::Universe;
}

impl<'a> Term for Value<'a> {
    const UNIVERSE: Self = Value::Universe;
}

impl<'a> Term for TypedValue<'a> {
    const UNIVERSE: Self = TypedValue::create_typed_value(Type::UNIVERSE, Value::UNIVERSE);
}

impl<M: Term> Term for Type<M> {
    const UNIVERSE: Self = Type(M::UNIVERSE);
}

#[derive(Clone, Debug)]
pub struct Type<M: Term>(M);

impl<M: Term + Display> Display for Type<M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<M: Term> Type<M> {
    pub const UNIVERSE: Type<M> = Type(M::UNIVERSE);
    pub(crate) fn create_type(m: M) -> Type<M> {
        Type(m)
    }
    pub fn wrapped(&self) -> &M {
        &self.0
    }
}

impl<'a, M: Term + Force<'a>> Force<'a> for Type<M> {
    fn force(&self, defs: &Definitions<'a>) -> Self {
        Type(self.0.force(defs))
    }
}

impl<M: Term + Evaluate> Evaluate for Type<M> {
    type ValueT<'b> = Type<M::ValueT<'b>> where Self: 'b;
    fn evaluate<'b>(&self, defs: &Definitions<'b>, env: &Environment<'b, '_>) -> Self::ValueT<'b>
        where
            Self: 'b,
    {
        Type::create_type(self.wrapped().evaluate(defs, env))
    }
}
