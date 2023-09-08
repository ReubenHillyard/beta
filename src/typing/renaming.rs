//! An implementor of trait [`Rename`].

use crate::typing::ast::Index;
use crate::typing::definitions::{Definitions, MetaVar};
use crate::typing::environment::{level_to_index_with_ctx_len, Level};
use crate::typing::environments::Context;
use crate::typing::expression::CoreExpression;
use crate::typing::read_back::{ReadBack, Rename};
use crate::typing::value::{Neutral, Principal, Value};
use crate::typing::TypeError;
use std::collections::HashMap;

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
        value.read_back(defs, *self)
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
        if let Value::Neutral(Neutral::Principal(Principal::MetaVariable(mv_))) = out {
            if mv_ == mv {
                return Ok(CoreExpression::MetaVariable(mv));
            }
        }
        out.read_back(defs, self)
    }
}
