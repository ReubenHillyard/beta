use crate::typing::definitions::Definitions;
use crate::typing::environments::Environment;
use crate::typing::evaluation::Evaluate;
use crate::typing::expression::CoreExpression;
use crate::typing::read_back::{ReadBack, Rename};
use crate::typing::value::{Closure, Force, Value};
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

impl<M: Term> Term for Type<M> {
    const UNIVERSE: Self = Type(Term::UNIVERSE);
}

impl<Ty: Term, Te: Term> Term for Typed<Ty, Te> {
    const UNIVERSE: Self = Typed {
        type_: Term::UNIVERSE,
        term: Term::UNIVERSE,
    };
}

#[derive(Clone, Debug)]
pub struct Type<Te: Term>(Te);

impl<Te: Term + Display> Display for Type<Te> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<Te: Term> Type<Te> {
    pub const UNIVERSE: Type<Te> = Type(Te::UNIVERSE);
    pub fn wrapped(&self) -> &Te {
        &self.0
    }
}

impl<'a, Te: Term + Force<'a>> Force<'a> for Type<Te> {
    fn force(&self, defs: &Definitions<'a>) -> Self {
        Type(self.0.force(defs))
    }
}

impl<'a, Te: Term + Evaluate<'a>> Evaluate<'a> for Type<Te> {
    type ValueT = Type<Te::ValueT>;
    fn evaluate(&self, defs: &Definitions<'a>, env: &Environment<'a, '_>) -> Self::ValueT {
        Type(self.0.evaluate(defs, env))
    }
}

impl<'a, Te: Term + ReadBack<'a>> ReadBack<'a> for Type<Te> {
    type ExprT = Type<Te::ExprT>;
    fn read_back<Ren: Rename<'a>>(
        &self,
        defs: &Definitions<'a>,
        ren: Ren,
    ) -> Result<Self::ExprT, Ren::Err> {
        self.0.read_back(defs, ren).map(Type)
    }
}

impl<'a> Type<CoreExpression<'a>> {
    pub fn pi_type(
        tparam_type: Type<CoreExpression<'a>>,
        ret_type: Type<CoreExpression<'a>>,
    ) -> Type<CoreExpression<'a>> {
        Type(CoreExpression::PiType {
            tparam_type: Box::new(tparam_type),
            ret_type: Box::new(ret_type),
        })
    }
}

impl<'a> Type<Value<'a>> {
    pub fn pi_type(
        param_type: Type<Value<'a>>,
        tclosure: Closure<'a, Type<CoreExpression<'a>>>,
    ) -> Type<Value<'a>> {
        Type(Value::PiType {
            param_type: Box::new(param_type),
            tclosure: Box::new(tclosure),
        })
    }
}

#[derive(Clone, Debug)]
pub struct Typed<Ty: Term, Te: Term> {
    type_: Type<Ty>,
    term: Te,
}

impl<Ty: Term + Display, Te: Term + Display> Display for Typed<Ty, Te> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} as {}", self.term, self.type_)
    }
}

impl<Ty: Term, Te: Term> Typed<Ty, Te> {
    pub(crate) fn create_typed(type_: Type<Ty>, term: Te) -> Typed<Ty, Te> {
        Typed { type_, term }
    }
    pub fn get_type(&self) -> &Type<Ty> {
        &self.type_
    }
    pub fn get_term(&self) -> &Te {
        &self.term
    }
    pub fn into_wrapped(self) -> Te {
        self.term
    }
    pub(crate) fn into_type(self) -> Type<Te> {
        Type(self.term)
    }
}

impl<'a, Ty: Term + Clone, Te: Term + Evaluate<'a>> Evaluate<'a> for Typed<Ty, Te> {
    type ValueT = Typed<Ty, Te::ValueT>;
    fn evaluate(&self, defs: &Definitions<'a>, env: &Environment<'a, '_>) -> Self::ValueT {
        Typed {
            type_: self.type_.clone(),
            term: self.term.evaluate(defs, env),
        }
    }
}
