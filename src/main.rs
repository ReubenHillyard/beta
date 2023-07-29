#![crate_name = "beta"]

//! A dependently-typed programming language, aiming to support safe mutable state and a cubical
//! interpretation of univalence.

use crate::lexer::lex;
use crate::typing::ast::abstract_file;
use crate::typing::checking::{synth_type, TypedExpression};
use crate::typing::environments::{Context, Definitions, Environment};
use crate::typing::evaluation::evaluate;
use crate::typing::read_back::read_back_value;
use crate::typing::value::Type;
use itertools::{Either, Itertools};
use std::fs::read_to_string;

pub mod ir_gen;
pub mod lexer;
pub mod parser;
pub mod typing;

pub fn main() {
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
        println!("lexing errors: {:#?}", errors);
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
            println!("name errors: {:#?}", errors);
            return;
        }
    };
    let mut defs = Definitions::default();
    let env = Environment::EMPTY;
    for (name, expr) in file.globals {
        let typed_expr = match synth_type(&mut defs, &Context::EMPTY, &expr) {
            Ok(type_) => type_,
            Err(error) => {
                println!("type error: {:#?}\n\n", error);
                println!("{defs:#?}");
                return;
            }
        };
        if !defs.all_metas_defined() {
            println!("could not deduce values for some meta-variables in {name}");
            return;
        }
        let value = evaluate(&defs, &env, &typed_expr);
        let type_expr = read_back_value(&defs, 0, value.get_type().as_value());
        let value_expr = read_back_value(&defs, 0, value.get_value());
        println!("{name} = {} as {}\n", value_expr, type_expr);
        let type_ = evaluate(
            &defs,
            &env,
            &TypedExpression::create_typed_expression(type_expr, Type::UNIVERSE),
        );
        let value = evaluate(
            &defs,
            &env,
            &TypedExpression::create_typed_expression(
                value_expr,
                Type::create_type_from_value(type_),
            ),
        );
        defs.insert_global(name, value);
        defs.reset_metas();
    }
}
