//! Implements various things `use`d elsewhere.

use crate::typing::environment::abstraction::abstract_expression_empty;
use crate::typing::environments::Definitions;
use crate::typing::environments::Environment;
use crate::typing::evaluation::evaluate;
use itertools::{Either, Itertools};
use std::fmt;

/// The position of a bound variable, counting from the right of the context.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Index {
    index: usize,
}

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.index)
    }
}

/// The position of a bound variable, counting from the left of the context.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Level {
    level: usize,
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
            Global(name) => name.fmt(f),
            Local(index) => index.fmt(f),
        }
    }
}

/// A variable that may appear in a value.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum VVariable<'a> {
    Global(&'a str),
    Local(Level),
}

pub(crate) mod abstraction;

pub(crate) mod context;

/// Allows the user to enter a line of text, and prints its abstract syntax.
#[doc(hidden)]
pub(crate) fn test_evaluate() {
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
            let expr = crate::parser::parse_as_expression(&tokens);
            match expr {
                Ok(expr) => {
                    println!("expression: {}\n", expr);
                    let expr = abstract_expression_empty(&expr);
                    match expr {
                        Ok(expr) => {
                            println!("abstracted: {}\n", expr);
                            println!(
                                "evaluated: {:?}\n",
                                evaluate(&Definitions::default(), &Environment::EMPTY, &expr)
                            );
                        }
                        Err(errors) => println!("name errors: {:?}", errors),
                    }
                }
                Err(error) => println!("parse error: {}", error),
            }
        } else {
            println!("lexing errors: {:?}", errors);
        }
        line.clear();
    }
}
