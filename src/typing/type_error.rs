use crate::typing::definitions::MetaVar;
use crate::typing::value::{Level, Value};

/// The error type for type-synthesis and type-checking.
#[derive(Debug)]
pub enum TypeError<'a> {
    FailedOccursCheck(MetaVar),
    NonLevelInSpine(Value<'a>),
    LevelOutOfScope(Level),
    CantUnifyDifferentHeads,
    RepeatLevelInSpine(Level),
}

/// A specialised result type for type-synthesis and type-checking.
pub type Result<'a, T> = std::result::Result<T, TypeError<'a>>;
