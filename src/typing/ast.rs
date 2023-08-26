//! Types for representing abstract syntax.

use std::fmt;
use std::fmt::{Display, Formatter};

pub use crate::typing::environment::Index;

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

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Expression::*;
        match self {
            Underscore => write!(f, "?"),
            Variable(id) => write!(f, "{id}"),
            PiType {
                tparam_type,
                ret_type,
            } => write!(f, "($ : {tparam_type}) -> {ret_type}"),
            Lambda {
                param_type,
                ret_val,
            } => write!(f, "($ : {param_type}) => {ret_val}"),
            Application { func, arg } => write!(f, "({func})({arg})"),
            Universe => write!(f, "Type"),
            Annotation { expr, type_ } => write!(f, "({expr} as {type_})"),
        }
    }
}

/// A variable that may appear in an expression.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum EVariable<'a> {
    Global(&'a str),
    Local(Index),
}

impl<'a> Display for EVariable<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use EVariable::*;
        match self {
            Global(name) => write!(f, "{name}"),
            Local(index) => write!(f, "{index}"),
        }
    }
}
