#![crate_name = "beta"]
#![allow(unused)]

//! A dependently-typed programming language, aiming to support safe mutable state and a cubical
//! interpretation of univalence.

pub mod ir_gen;
pub mod lexer;
pub mod parser;
pub mod typing;

fn test_lexer() {
    let mut line = String::new();
    loop {
        println!("\n\n");
        line.clear();
        std::io::stdin().read_line(&mut line).unwrap();
        let tokens = lexer::lex(&line).collect::<Result<Vec<_>, _>>();
        println!("{:?}", tokens);
    }
}

pub fn main() {
    test_lexer()
}
