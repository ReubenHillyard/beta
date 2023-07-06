//! Types for representing abstract syntax.

/// The position of a bound variable, counting from the right of the context.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Index {
    index: usize,
}

/// A variable that may appear in an expression.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum EVariable<'a> {
    Global(&'a str),
    Local(Index),
}

/// The abstract syntax of an expression.
#[derive(Clone)]
pub enum Expression<'a> {
    Variable(EVariable<'a>),
    PiType {
        tparam_type: Box<Expression<'a>>,
        ret_type: Box<Expression<'a>>,
    },
    Lambda {
        param_type: Option<Box<Expression<'a>>>,
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
