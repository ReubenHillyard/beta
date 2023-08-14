//! A function for reading back [`Value`]s into [`CoreExpression`]s.

use crate::typing::checking::CoreExpression;
use crate::typing::definitions::MetaVar;
use crate::typing::environment::Level;
use crate::typing::environments::Definitions;
use crate::typing::value::Value;
use crate::typing::TypeError;
use std::collections::HashMap;

mod detail {
    use crate::typing::ast::Index;
    use crate::typing::checking::CoreExpression;
    use crate::typing::definitions::{Definitions, MetaVar};
    use crate::typing::environment::{level_to_index_with_ctx_len, EVariable};
    use crate::typing::environments::Context;
    use crate::typing::read_back::RenamingAvoiding;
    use crate::typing::value::{Level, Neutral, VVariable, Value};
    use crate::typing::TypeError;

    #[derive(Debug)]
    pub enum Void {}

    pub trait Rename<'a>: Copy {
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
            if let Value::MetaNeutral(Neutral::Variable(mv_)) = out {
                if mv_ == mv {
                    return Ok(CoreExpression::MetaVariable(mv));
                }
            }
            read_back_value(defs, self, &out)
        }
    }

    impl<'a, 'r> Rename<'a> for RenamingAvoiding<'r> {
        type Err = TypeError<'a>;

        fn fresh_var(self) -> Value<'a> {
            Context::fresh_var_from_ctx_len(self.domain_len())
        }

        fn lift(self) -> Self {
            self.lift_renaming()
        }

        fn rename_level(self, level: Level) -> Result<Index, Self::Err> {
            Ok(level_to_index_with_ctx_len(
                self.range_len(),
                self.do_rename_level(level)?,
            ))
        }

        fn rename_meta(
            self,
            defs: &Definitions<'a>,
            mv: MetaVar,
        ) -> Result<CoreExpression<'a>, Self::Err> {
            if mv == self.avoiding {
                return Err(TypeError::FailedOccursCheck(mv));
            }
            let out = defs.lookup_meta(mv);
            if let Value::MetaNeutral(Neutral::Variable(mv_)) = out {
                if mv_ == mv {
                    return Ok(CoreExpression::MetaVariable(mv));
                }
            }
            read_back_value(defs, self, &out)
        }
    }

    pub trait ValidVarT<'a>: Copy {
        fn rename<Ren: Rename<'a>>(
            self,
            defs: &Definitions<'a>,
            ren: Ren,
        ) -> Result<CoreExpression<'a>, Ren::Err>;
    }

    impl<'a> ValidVarT<'a> for VVariable<'a> {
        fn rename<Ren: Rename<'a>>(
            self,
            _defs: &Definitions<'a>,
            ren: Ren,
        ) -> Result<CoreExpression<'a>, Ren::Err> {
            Ok(CoreExpression::Variable(match self {
                VVariable::Local(level) => EVariable::Local(ren.rename_level(level)?),
                VVariable::Global(name) => EVariable::Global(name),
            }))
        }
    }

    impl<'a> ValidVarT<'a> for MetaVar {
        fn rename<Ren: Rename<'a>>(
            self,
            defs: &Definitions<'a>,
            ren: Ren,
        ) -> Result<CoreExpression<'a>, Ren::Err> {
            ren.rename_meta(defs, self)
        }
    }

    pub fn read_back_value<'a, Ren: Rename<'a>>(
        defs: &Definitions<'a>,
        ren: Ren,
        value: &Value<'a>,
    ) -> Result<CoreExpression<'a>, Ren::Err> {
        use Value::*;
        match value {
            PiType {
                param_type,
                tclosure,
            } => {
                let fresh_var = ren.fresh_var();
                let ret_type = tclosure.call(defs, &fresh_var);
                Ok(CoreExpression::PiType {
                    tparam_type: Box::new(read_back_value(defs, ren, param_type.as_value())?),
                    ret_type: Box::new(read_back_value(defs, ren.lift(), &ret_type)?),
                })
            }
            Lambda { closure } => {
                let fresh_var = ren.fresh_var();
                let ret_val = closure.call(defs, &fresh_var);
                Ok(CoreExpression::Lambda {
                    ret_val: Box::new(read_back_value(defs, ren.lift(), &ret_val)?),
                })
            }
            Universe => Ok(CoreExpression::Universe),
            Neutral(neu) => read_back_neutral(defs, ren, neu),
            MetaNeutral(neu) => read_back_neutral(defs, ren, neu),
        }
    }

    fn read_back_neutral<'a, Ren: Rename<'a>, VarT: ValidVarT<'a>>(
        defs: &Definitions<'a>,
        ren: Ren,
        neu: &Neutral<'a, VarT>,
    ) -> Result<CoreExpression<'a>, Ren::Err> {
        use Neutral::*;
        match neu {
            Variable(var) => var.rename(defs, ren),
            Application { func, arg } => Ok(CoreExpression::Application {
                func: Box::new(read_back_neutral(defs, ren, func)?),
                arg: Box::new(read_back_value(defs, ren, arg)?),
            }),
        }
    }
}

pub fn read_back_value<'a>(
    defs: &Definitions<'a>,
    ctx_len: usize,
    value: &Value<'a>,
) -> CoreExpression<'a> {
    match detail::read_back_value(defs, ctx_len, value) {
        Ok(expr) => expr,
        Err(err) => match err {},
    }
}

#[derive(Debug)]
pub(crate) struct Renaming {
    domain_len: usize,
    range_len: usize,
    map: HashMap<Level, Level>,
}

#[derive(Copy, Clone)]
pub(crate) struct RenamingAvoiding<'r> {
    pub renaming: &'r Renaming,
    pub avoiding: MetaVar,
    pub extended: usize,
}

impl Renaming {
    pub(crate) fn create_renaming<'a>(
        ctx_len: usize,
        vvs: &[Level],
    ) -> super::Result<'a, Renaming> {
        let mut out = HashMap::new();
        for (count, vv) in vvs.iter().enumerate() {
            if out.insert(*vv, Level::create_level(count)).is_some() {
                return Err(TypeError::RepeatLevelInSpine(*vv));
            }
        }
        Ok(Renaming {
            domain_len: ctx_len,
            range_len: vvs.len(),
            map: out,
        })
    }
}

impl<'r> RenamingAvoiding<'r> {
    pub(crate) fn new(renaming: &'r Renaming, avoiding: MetaVar) -> RenamingAvoiding<'r> {
        RenamingAvoiding {
            renaming,
            avoiding,
            extended: 0,
        }
    }
    fn lift_renaming(self) -> RenamingAvoiding<'r> {
        RenamingAvoiding {
            extended: self.extended + 1,
            ..self
        }
    }
    fn domain_len(&self) -> usize {
        self.renaming.domain_len + self.extended
    }
    fn range_len(&self) -> usize {
        self.renaming.range_len + self.extended
    }
    fn do_rename_level(&self, level: Level) -> super::Result<'static, Level> {
        if level.get_inner() < self.renaming.domain_len {
            match self.renaming.map.get(&level) {
                Some(level) => Ok(*level),
                None => Err(TypeError::LevelOutOfScope(level)),
            }
        } else {
            assert!(level.get_inner() < self.domain_len());
            Ok(Level::create_level(
                (level.get_inner() - self.renaming.domain_len) + self.renaming.range_len,
            ))
        }
    }
    pub(crate) fn rename_value_avoiding<'a>(
        &self,
        defs: &Definitions<'a>,
        value: &Value<'a>,
    ) -> super::Result<'a, CoreExpression<'a>> {
        detail::read_back_value(defs, *self, value)
    }
}
