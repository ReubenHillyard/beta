//! Functions for parsing [`Token`]s into concrete syntax tree elements.

use crate::lexer::Token;
use crate::parser::cst::Expression;
use itertools::{Either, Itertools};
use std::string::ParseError;

/// Types for concrete syntax tree elements.
pub mod cst {
    use std::fmt;

    /// Represents the concrete syntax of an expression.
    #[derive(Debug)]
    pub enum Expression<'a> {
        Variable(&'a str),
        PiType {
            tparam: &'a str,
            tparam_type: Box<Expression<'a>>,
            ret_type: Box<Expression<'a>>,
        },
        Lambda {
            param: &'a str,
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
                Variable(id) => id.fmt(f),
                PiType {
                    tparam,
                    tparam_type,
                    ret_type,
                } => write!(f, "({} : {}) -> {}", tparam, tparam_type, ret_type),
                Lambda {
                    param,
                    param_type,
                    ret_val,
                } => match param_type {
                    Some(param_type) => write!(f, "({} : {}) => {}", param, param_type, ret_val),
                    None => write!(f, "{} => {}", param, ret_val),
                },
                Application { func, arg } => write!(f, "({})({})", func, arg),
                Universe => write!(f, "Type"),
                Annotation { expr, type_ } => write!(f, "({} as {})", expr, type_),
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
            rule expr() -> Expression<'a>
                = ([Token::Identifier(param)] [Token::DoubleArrow] ret_val:expr() {
                    Expression::Lambda {
                        param,
                        param_type: None,
                        ret_val: Box::new(ret_val)
                    }
                })
                / ([Token::LParen] [Token::Identifier(param)] [Token::Colon] param_type:expr() [Token::RParen] [Token::DoubleArrow] ret_val:expr() {
                    Expression::Lambda {
                        param,
                        param_type: Some(Box::new(param_type)),
                        ret_val: Box::new(ret_val)
                    }
                })
                / ([Token::LParen] [Token::Identifier(tparam)] [Token::Colon] tparam_type:expr() [Token::RParen] [Token::SingleArrow] ret_type:expr() {
                    Expression::PiType {
                        tparam,
                        tparam_type: Box::new(tparam_type),
                        ret_type: Box::new(ret_type)
                    }
                })
                / (e:core_expr() [Token::As] t:core_expr() {
                    Expression::Annotation {
                        expr: Box::new(e),
                        type_: Box::new(t),
                    }
                })
                / (e:core_expr() { e })

            rule core_expr() -> Expression<'a> = precedence!{
                func:@ [Token::LParen] arg:expr() [Token::RParen] {
                    Expression::Application {
                        func: Box::new(func),
                        arg: Box::new(arg),
                    }
                }
                --
                [Token::LParen] e:expr() [Token::RParen] { e }
                [Token::Type] { Expression::Universe }
                [Token::Identifier(id)] { Expression::Variable(id) }
            }

            pub rule expression() -> Expression<'a>
                = expr()
        }
    }
}

/// Parses a slice of [`Token`]s into an [`Expression`] or fails.
pub fn parse_as_expression<'a>(
    tokens: &'a [Token<'a>],
) -> Result<Expression<'a>, peg::error::ParseError<usize>> {
    grammar::parser::expression(tokens)
}

/// Allows the user to enter a line of text, and prints the result of parsing it.
#[doc(hidden)]
pub(crate) fn test_parser() {
    use crate::lexer::lex;
    let mut line = String::new();
    loop {
        println!("\n\n");
        std::io::stdin().read_line(&mut line).unwrap();
        let tokens = lex(&line).collect::<Vec<_>>();
        let (tokens, errors): (Vec<_>, Vec<_>) = tokens.into_iter().partition_map(|t| match t {
            Ok(token) => Either::Left(token),
            Err(error) => Either::Right(error),
        });
        if errors.is_empty() {
            let expr = parse_as_expression(&tokens);
            match expr {
                Ok(expr) => println!("expression: {}", expr),
                Err(error) => println!("parse error: {}", error),
            }
        } else {
            println!("lexing errors: {:?}", errors);
        }
        line.clear();
    }
}
