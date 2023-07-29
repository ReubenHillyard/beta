//! Implements various things `use`d elsewhere.

use crate::typing::checking::CoreExpression;
use crate::typing::definitions::{Definitions, MetaVar};
use crate::typing::environments::Context;
use crate::typing::value::{Neutral, Value};
use crate::typing::TypeError;
use std::collections::HashMap;
use std::fmt;

/// The position of a bound variable, counting from the right of the context.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Index {
    index: usize,
}

#[cfg(test)]
impl Index {
    pub(crate) fn create_index(index: usize) -> Index {
        Index { index }
    }
}

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.index)
    }
}

/// The position of a bound variable, counting from the left of the context.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Level {
    level: usize,
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "£{}", self.level)
    }
}

/// A variable that may appear in an expression.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum EVariable<'a> {
    Global(&'a str),
    Local(Index),
}

impl<'a> fmt::Display for EVariable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use EVariable::*;
        match self {
            Global(name) => write!(f, "{}", name),
            Local(index) => write!(f, "{}", index),
        }
    }
}

/// A variable that may appear in a value.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum VVariable<'a> {
    Global(&'a str),
    Local(Level),
}

impl<'a> fmt::Display for VVariable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use VVariable::*;
        match self {
            Global(name) => write!(f, "{}", name),
            Local(level) => write!(f, "{}", level),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Renaming {
    domain_len: usize,
    range_len: usize,
    map: HashMap<Level, Level>,
}

mod detail {
    use crate::typing::checking::CoreExpression;
    use crate::typing::environment::context::vv_to_ev_with_ctx_size;
    use crate::typing::environment::Renaming;
    use crate::typing::environments::MetaVar;
    use crate::typing::value::VVariable;
    use crate::typing::TypeError;

    pub(super) trait ValidVarT<'a> {
        fn rename_avoiding(
            &self,
            renaming: &Renaming,
            mv: MetaVar,
        ) -> super::super::Result<'a, CoreExpression<'a>>;
    }

    impl<'a> ValidVarT<'a> for MetaVar {
        fn rename_avoiding(
            &self,
            _renaming: &Renaming,
            mv: MetaVar,
        ) -> crate::typing::Result<'a, CoreExpression<'a>> {
            if *self == mv {
                Err(TypeError::FailedOccursCheck(mv))
            } else {
                Ok(CoreExpression::MetaVariable(*self))
            }
        }
    }

    impl<'a> ValidVarT<'a> for VVariable<'a> {
        fn rename_avoiding(
            &self,
            renaming: &Renaming,
            _mv: MetaVar,
        ) -> crate::typing::Result<'a, CoreExpression<'a>> {
            use VVariable::*;
            let vv = match self {
                Local(level) => Local(renaming.rename_level(*level)?),
                _ => *self,
            };
            Ok(CoreExpression::Variable(vv_to_ev_with_ctx_size(
                renaming.range_len,
                vv,
            )))
        }
    }
}

impl Renaming {
    pub(crate) fn create_renaming<'a>(
        ctx_size: usize,
        vvs: &[Level],
    ) -> super::Result<'a, Renaming> {
        let mut out = HashMap::new();
        for (count, vv) in vvs.iter().enumerate() {
            if out.insert(*vv, Level { level: count }).is_some() {
                return Err(TypeError::RepeatLevelInSpine(*vv));
            }
        }
        Ok(Renaming {
            domain_len: ctx_size,
            range_len: vvs.len(),
            map: out,
        })
    }
    pub(crate) fn lift_renaming(&self) -> Renaming {
        Renaming {
            domain_len: self.domain_len + 1,
            range_len: self.range_len + 1,
            map: {
                let mut map = HashMap::with_capacity(self.map.len() + 1);
                map.clone_from(&self.map);
                let old = map.insert(
                    Level {
                        level: self.domain_len,
                    },
                    Level {
                        level: self.range_len,
                    },
                );
                assert!(old.is_none());
                map
            },
        }
    }
    pub(crate) fn rename_level(&self, level: Level) -> super::Result<'static, Level> {
        match self.map.get(&level) {
            Some(level) => Ok(*level),
            None => Err(TypeError::LevelOutOfScope(level)),
        }
    }
    pub(crate) fn rename_value_avoiding<'a>(
        &self,
        defs: &Definitions<'a>,
        value: &Value<'a>,
        mv: MetaVar,
    ) -> super::Result<'a, CoreExpression<'a>> {
        use Value::*;
        match value {
            PiType {
                param_type,
                tclosure,
            } => {
                let fresh_var = Context::fresh_var_from_ctx_size(self.domain_len);
                let ret_type = tclosure.call(defs, &fresh_var);
                Ok(CoreExpression::PiType {
                    tparam_type: Box::new(self.rename_value_avoiding(
                        defs,
                        param_type.as_value(),
                        mv,
                    )?),
                    ret_type: Box::new(
                        self.lift_renaming()
                            .rename_value_avoiding(defs, &ret_type, mv)?,
                    ),
                })
            }
            Lambda { closure } => {
                let fresh_var = Context::fresh_var_from_ctx_size(self.domain_len);
                let ret_val = closure.call(defs, &fresh_var);
                Ok(CoreExpression::Lambda {
                    ret_val: Box::new(
                        self.lift_renaming()
                            .rename_value_avoiding(defs, &ret_val, mv)?,
                    ),
                })
            }
            Universe => Ok(CoreExpression::Universe),
            Neutral(neu) => self.rename_neutral_avoiding(defs, neu, mv),
            MetaNeutral(neu) => self.rename_neutral_avoiding(defs, neu, mv),
        }
    }
    fn rename_neutral_avoiding<'a, VarT: detail::ValidVarT<'a>>(
        &self,
        defs: &Definitions<'a>,
        neu: &Neutral<'a, VarT>,
        mv: MetaVar,
    ) -> super::Result<'a, CoreExpression<'a>> {
        use Neutral::*;
        match neu {
            Variable(var) => var.rename_avoiding(self, mv),
            Application { func, arg } => Ok(CoreExpression::Application {
                func: Box::new(self.rename_neutral_avoiding(defs, func, mv)?),
                arg: Box::new(self.rename_value_avoiding(defs, arg, mv)?),
            }),
        }
    }
}

pub(crate) mod abstraction;

pub(crate) mod context;
