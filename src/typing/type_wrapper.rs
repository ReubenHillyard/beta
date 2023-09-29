//! Wrappers for terms either of known type, or known to be types.

use crate::typing::definitions::Definitions;
use crate::typing::environments::DefsWithEnv;
use crate::typing::evaluation::Evaluate;
use crate::typing::expression::CoreExpression;
use crate::typing::read_back::{ReadBack, Rename};
use crate::typing::value::{Closure, Force, Value};
use std::fmt;
use std::fmt::{Display, Formatter};

/// A trait for terms.
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

/// A term known to be a type.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Type<Te: Term>(Te);

impl<Te: Term + Display> Display for Type<Te> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<Te: Term> Type<Te> {
    /// Obtains a reference to the underlying term.
    pub fn wrapped(&self) -> &Te {
        &self.0
    }

    /// Demotes a [`Type`] to a [`Typed`].
    pub fn into_typed<Ty: Term>(self) -> Typed<Ty, Te> {
        Typed {
            type_: Term::UNIVERSE,
            term: self.0,
        }
    }
}

impl<'a, Te: Term + Force<'a>> Force<'a> for Type<Te> {
    fn force(&self, defs: &Definitions<'a>) -> Self {
        Type(self.0.force(defs))
    }
}

impl<'a, Te: Term + Evaluate<'a>> Evaluate<'a> for Type<Te> {
    type ValueT = Type<Te::ValueT>;
    fn evaluate(&self, defs_env: DefsWithEnv<'a, '_>) -> Self::ValueT {
        Type(self.0.evaluate(defs_env))
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
    /// Builds a pi type.
    ///
    /// Requires that `tparam_type` is a valid type in the context from which `pi_type` is called,
    /// and that `ret_type` is a valid type in that context extended with a variable of type
    /// `tparam_type`.
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
    /// Builds a pi type.
    ///
    /// Requires that `tparam_type` is a valid type in the context from which `pi_type` is called,
    /// and that `tclosure` returns a valid type in that context extended with a variable of type
    /// `tparam_type`, when called with that variable.
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

/// A term of a known type.
#[derive(Clone, Debug, Eq, PartialEq)]
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
    /// Creates a [`Typed`] from a type and a term of that type.
    ///
    /// Requires that `type_` is a valid type in the context from which `create_typed` is called,
    /// and that `term` is a valid term of that type in that context.
    pub(crate) fn create_typed(type_: Type<Ty>, term: Te) -> Typed<Ty, Te> {
        Typed { type_, term }
    }

    /// Obtains a reference to the type.
    pub fn get_type(&self) -> &Type<Ty> {
        &self.type_
    }

    /// Obtains a reference to the term.
    pub fn get_term(&self) -> &Te {
        &self.term
    }

    /// Obtains the wrapped term.
    pub fn into_wrapped(self) -> Te {
        self.term
    }

    /// Obtains the wrapped term as a [`Type`].
    ///
    /// Requires that the term denotes a type in the context from which `term_into_type` is called.
    pub(crate) fn term_into_type(self) -> Type<Te> {
        Type(self.term)
    }

    /// Obtains the type.
    pub fn type_into_type(self) -> Type<Ty> {
        self.type_
    }
}

impl<'a, Ty: Term + Clone, Te: Term + Evaluate<'a>> Evaluate<'a> for Typed<Ty, Te> {
    type ValueT = Typed<Ty, Te::ValueT>;
    fn evaluate(&self, defs_env: DefsWithEnv<'a, '_>) -> Self::ValueT {
        Typed {
            type_: self.type_.clone(),
            term: self.term.evaluate(defs_env),
        }
    }
}
