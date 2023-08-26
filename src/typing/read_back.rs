//! A function for reading back [`Value`]s into [`CoreExpression`]s.

use crate::typing::ast::{EVariable, Index};
use crate::typing::environment::level_to_index_with_ctx_len;
use crate::typing::environments::{Context, Definitions, MetaVar};
use crate::typing::expression::CoreExpression;
use crate::typing::type_wrapper::Term;
use crate::typing::value::{Level, Neutral, Principal, VVariable, Value};

#[derive(Debug)]
pub(crate) enum Void {}

pub(crate) fn read_back_with_ctx_len<'a, RB: ReadBack<'a>>(
    defs: &Definitions<'a>,
    ctx_len: usize,
    rb: &RB,
) -> RB::ExprT {
    match rb.read_back(defs, ctx_len) {
        Ok(expr) => expr,
        Err(err) => match err {},
    }
}

pub(crate) trait ReadBack<'a> {
    type ExprT: Term;
    fn read_back<Ren: Rename<'a>>(
        &self,
        defs: &Definitions<'a>,
        ren: Ren,
    ) -> Result<Self::ExprT, Ren::Err>;
}

impl<'a> ReadBack<'a> for Value<'a> {
    type ExprT = CoreExpression<'a>;
    fn read_back<Ren: Rename<'a>>(
        &self,
        defs: &Definitions<'a>,
        ren: Ren,
    ) -> Result<Self::ExprT, Ren::Err> {
        use Value::*;
        match self {
            PiType {
                param_type,
                tclosure,
            } => {
                let fresh_var = ren.fresh_var();
                let ret_type = tclosure.call(defs, &fresh_var);
                Ok(CoreExpression::PiType {
                    tparam_type: Box::new(param_type.read_back(defs, ren)?),
                    ret_type: Box::new(ret_type.read_back(defs, ren.lift())?),
                })
            }
            Lambda { closure } => {
                let fresh_var = ren.fresh_var();
                let ret_val = closure.call(defs, &fresh_var);
                Ok(CoreExpression::Lambda {
                    ret_val: Box::new(ret_val.read_back(defs, ren.lift())?),
                })
            }
            Universe => Ok(CoreExpression::Universe),
            Neutral(neu) => neu.read_back(defs, ren),
        }
    }
}

impl<'a> ReadBack<'a> for Neutral<'a> {
    type ExprT = CoreExpression<'a>;
    fn read_back<Ren: Rename<'a>>(
        &self,
        defs: &Definitions<'a>,
        ren: Ren,
    ) -> Result<Self::ExprT, Ren::Err> {
        match self {
            Neutral::Principal(p) => match p {
                Principal::Variable(var) => Ok(CoreExpression::Variable(match var {
                    VVariable::Local(level) => EVariable::Local(ren.rename_level(*level)?),
                    VVariable::Global(name) => EVariable::Global(name),
                })),
                Principal::MetaVariable(mv) => ren.rename_meta(defs, *mv),
            },
            Neutral::Application { func, arg } => Ok(CoreExpression::Application {
                func: Box::new(func.read_back(defs, ren)?),
                arg: Box::new(arg.read_back(defs, ren)?),
            }),
        }
    }
}

pub(crate) trait Rename<'a>: Copy {
    type Err;
    fn fresh_var(self) -> Value<'a>;
    fn lift(self) -> Self;
    fn rename_level(self, level: Level) -> Result<Index, Self::Err>;
    fn rename_meta(
        self,
        defs: &Definitions<'a>,
        mv: MetaVar,
    ) -> Result<CoreExpression<'a>, Self::Err>;
}

impl<'a> Rename<'a> for usize {
    type Err = Void;
    fn fresh_var(self) -> Value<'a> {
        Context::fresh_var_from_ctx_len(self)
    }
    fn lift(self) -> Self {
        self + 1
    }
    fn rename_level(self, level: Level) -> Result<Index, Self::Err> {
        Ok(level_to_index_with_ctx_len(self, level))
    }
    fn rename_meta(
        self,
        defs: &Definitions<'a>,
        mv: MetaVar,
    ) -> Result<CoreExpression<'a>, Self::Err> {
        let out = defs.lookup_meta(mv);
        if let Value::Neutral(Neutral::Principal(Principal::MetaVariable(mv_))) = out {
            if mv_ == mv {
                return Ok(CoreExpression::MetaVariable(mv));
            }
        }
        out.read_back(defs, self)
    }
}
