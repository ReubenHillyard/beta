//! Functions for parsing [`Token`]s into concrete syntax.

use crate::lexer::Token;
use crate::parser::cst::*;
use peg::error::{ExpectedSet, ParseError};

/// Types for concrete syntax tree elements.
pub mod cst {
    use std::fmt;

    /// Represents the concrete syntax of a file.
    #[derive(Debug)]
    pub struct File<'a> {
        pub declarations: Vec<Declaration<'a>>,
    }

    /// Represents the concrete syntax of a declaration.
    #[derive(Debug)]
    pub enum Declaration<'a> {
        Definition {
            name: &'a str,
            value: Expression<'a>,
        },
    }

    /// The concrete syntax of an expression.
    #[derive(Debug)]
    pub enum Expression<'a> {
        Underscore,
        Variable(&'a str),
        PiType {
            tparam: Option<&'a str>,
            tparam_type: Box<Expression<'a>>,
            ret_type: Box<Expression<'a>>,
        },
        Lambda {
            param: Option<&'a str>,
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

    impl fmt::Display for Expression<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            use Expression::*;
            match self {
                Underscore => write!(f, "_"),
                Variable(id) => write!(f, "{id}"),
                PiType {
                    tparam,
                    tparam_type,
                    ret_type,
                } => {
                    let tparam = tparam.unwrap_or("_");
                    write!(f, "({tparam} : {tparam_type}) -> {ret_type}")
                }
                Lambda {
                    param,
                    param_type,
                    ret_val,
                } => {
                    let param = param.unwrap_or("_");
                    match param_type {
                        Some(param_type) => {
                            write!(f, "({param} : {param_type}) => {ret_val}")
                        }
                        None => write!(f, "{param} => {ret_val}"),
                    }
                }
                Application { func, arg } => write!(f, "({func})({arg})"),
                Universe => write!(f, "Type"),
                Annotation { expr, type_ } => write!(f, "({expr} as {type_})"),
            }
        }
    }
}

#[doc(hidden)]
mod grammar {
    use super::cst::*;
    use crate::lexer::Token;

    peg::parser! {
        pub grammar parser<'a>() for [Token<'a>] {
            rule opt_id() -> Option<&'a str>
                = [Token::Identifier(id)] { Some(id) }
                / [Token::Underscore] { None }

            rule expr() -> Expression<'a>
                = (param:opt_id() [Token::DoubleArrow] ret_val:expr() {
                    Expression::Lambda {
                        param,
                        param_type: None,
                        ret_val: Box::new(ret_val)
                    }
                })
                / ([Token::LParen] param:opt_id() [Token::Colon]
                    param_type:expr() [Token::RParen] [Token::DoubleArrow] ret_val:expr()
                {
                    Expression::Lambda {
                        param,
                        param_type: Some(Box::new(param_type)),
                        ret_val: Box::new(ret_val)
                    }
                })
                / ([Token::LParen] tparam:opt_id() [Token::Colon]
                    tparam_type:expr() [Token::RParen] [Token::SingleArrow] ret_type:expr()
                {
                    Expression::PiType {
                        tparam,
                        tparam_type: Box::new(tparam_type),
                        ret_type: Box::new(ret_type)
                    }
                })
                / core_expr()

            rule core_expr() -> Expression<'a> = precedence!{
                func:@ [Token::LParen] arg:expr() [Token::RParen] {
                    Expression::Application {
                        func: Box::new(func),
                        arg: Box::new(arg),
                    }
                }
                dom:@ [Token::SingleArrow] ran:expr() {
                    Expression::PiType {
                        tparam: None,
                        tparam_type: Box::new(dom),
                        ret_type: Box::new(ran),
                    }
                }
                expr:@ [Token::As] type_:expr() {
                    Expression::Annotation {
                        expr: Box::new(expr),
                        type_: Box::new(type_),
                    }
                }
                [Token::LParen] e:expr() [Token::RParen] { e }
                [Token::Type] { Expression::Universe }
                [Token::Underscore] { Expression::Underscore }
                [Token::Identifier(id)] { Expression::Variable(id) }
            }

            pub rule expression() -> Expression<'a>
                = expr()

            rule decl() -> Declaration<'a>
                = [Token::Define] [Token::Identifier(name)] [Token::Equals]
                    value:expr() [Token::Semicolon]
                {
                    Declaration::Definition {
                        name,
                        value
                    }
                }

            pub rule file() -> File<'a>
                = declarations:decl()* {
                    File {
                        declarations
                    }
                }

        }
    }
}

/// Parses a slice of [`Token`]s into an [`Expression`] or fails.
pub fn parse_as_expression<'a>(tokens: &[Token<'a>]) -> Result<Expression<'a>, ParseError<usize>> {
    grammar::parser::expression(tokens)
}

/// Parses a slice of [`Token`]s into an [`File`] or fails.
pub fn parse_as_file<'a>(tokens: &[Token<'a>]) -> Result<File<'a>, ParseError<usize>> {
    grammar::parser::file(tokens)
}

/// The names of the expected tokens as used in error messages.
pub(crate) fn token_names(expected_set: &ExpectedSet) -> impl Iterator<Item = &'static str> + '_ {
    expected_set.tokens().map(token_name)
}

/// Gets the name of a token from the name given by [`peg`].
fn token_name(name: &str) -> &'static str {
    if name == "EOF" {
        return "EOF";
    }
    let core_name = name
        .strip_prefix("[Token :: ")
        .and_then(|name| name.split_once(|c: char| !c.is_ascii_alphanumeric()))
        .map(|(name, _)| name);
    match core_name {
        Some("Underscore") => Token::UNDERSCORE_NAME,
        Some("As") => Token::AS_NAME,
        Some("Define") => Token::DEFINE_NAME,
        Some("Type") => Token::TYPE_NAME,
        Some("Identifier") => Token::IDENTIFIER_NAME,
        Some("LParen") => Token::L_PAREN_NAME,
        Some("RParen") => Token::R_PAREN_NAME,
        Some("Equals") => Token::EQUALS_NAME,
        Some("Comma") => Token::COMMA_NAME,
        Some("Colon") => Token::COLON_NAME,
        Some("Semicolon") => Token::SEMICOLON_NAME,
        Some("SingleArrow") => Token::SINGLE_ARROW_NAME,
        Some("DoubleArrow") => Token::DOUBLE_ARROW_NAME,
        _ => panic!("unrecognised token name `{name:?}`"),
    }
}
