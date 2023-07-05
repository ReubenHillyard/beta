#![crate_name = "beta"]
#![allow(unused)]

//! A dependently-typed programming language, aiming to support safe mutable state and a cubical
//! interpretation of univalence.

pub mod ir_gen;
pub mod lexer;
pub mod parser;
pub mod typing;

pub fn main() {
    lexer::test_lexer()
}
