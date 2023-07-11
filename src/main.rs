#![crate_name = "beta"]
#![allow(unused)]

//! A dependently-typed programming language, aiming to support safe mutable state and a cubical
//! interpretation of univalence.

use crate::lexer::lex;
use crate::typing::ast::abstract_file;
use crate::typing::environments::{Context, Definitions, Environment};
use crate::typing::evaluation::evaluate;
use crate::typing::read_back::read_back_typed;
use crate::typing::value::{Type, TypedValue};
use itertools::{Either, Itertools};
use std::fs::read_to_string;

pub mod ir_gen;
pub mod lexer;
pub mod parser;
pub mod typing;

pub fn main() {
    println!("for now files may only declare types.\n");
    println!("enter filepath: ");
    let mut path = String::new();
    std::io::stdin().read_line(&mut path).unwrap();
    path.pop(); // remove newline character
    let source = read_to_string(path).unwrap();
    let tokens: Vec<_> = lex(&source).collect();
    let (tokens, errors): (Vec<_>, Vec<_>) = tokens.into_iter().partition_map(|t| match t {
        Ok(token) => Either::Left(token),
        Err(error) => Either::Right(error),
    });
    if !errors.is_empty() {
        println!("lexing errors: {:?}", errors);
        return;
    }
    let file = match parser::parse_as_file(&tokens) {
        Ok(file) => file,
        Err(error) => {
            println!("parse error: {}", error);
            return;
        }
    };
    let file = match abstract_file(&file) {
        Ok(file) => file,
        Err(errors) => {
            println!("name errors: {:?}", errors);
            return;
        }
    };
    let mut defs = Definitions::default();
    let env = Environment::EMPTY;
    for (name, expr) in file.globals {
        let value = evaluate(&defs, &env, &expr);
        println!(
            "{name} = {}\n\n",
            read_back_typed(&defs, &Context::EMPTY, &value, &Type::UNIVERSE)
        );
        defs.insert(name, TypedValue::create_typed_value(Type::UNIVERSE, value));
    }
}
