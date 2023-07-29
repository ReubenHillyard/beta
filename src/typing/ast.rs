//! Types for representing abstract syntax.

pub use crate::typing::environment::abstraction::abstract_file;
pub use crate::typing::environment::{EVariable, Index};
use std::fmt;

/// The abstract syntax of a file.
#[derive(Debug)]
pub struct File<'a> {
    pub globals: Vec<(&'a str, Expression<'a>)>,
}

/// The abstract syntax of an expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression<'a> {
    Underscore,
    Variable(EVariable<'a>),
    PiType {
        tparam_type: Box<Expression<'a>>,
        ret_type: Box<Expression<'a>>,
    },
    Lambda {
        param_type: Box<Expression<'a>>,
        ret_val: Box<Expression<'a>>,
    },
    Application {
        func: Box<Expression<'a>>,
        arg: Box<Expression<'a>>,
    },
    Universe,
    Annotation {
        expr: Box<Expression<'a>>,
        type_: Box<Expression<'a>>,
    },
}

impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;
        match self {
            Underscore => write!(f, "?"),
            Variable(id) => id.fmt(f),
            PiType {
                tparam_type,
                ret_type,
            } => write!(f, "($ : {}) -> {}", tparam_type, ret_type),
            Lambda {
                param_type,
                ret_val,
            } => write!(f, "($ : {}) => {}", param_type, ret_val),
            Application { func, arg } => write!(f, "({})({})", func, arg),
            Universe => write!(f, "Type"),
            Annotation { expr, type_ } => write!(f, "({} as {})", expr, type_),
        }
    }
}

/// An error regarding the use of concrete names.
#[derive(Debug)]
pub enum NameError<'a> {
    DuplicateGlobal(&'a str, &'a str),
    NotFound(&'a str),
}
