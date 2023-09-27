#![crate_name = "beta"]

//! A dependently-typed programming language, aiming to support safe mutable state and a cubical
//! interpretation of univalence.

use clap::Parser;
use commands::main_command::MainArguments;
use commands::Arguments;
use std::process;

pub mod commands;
pub mod ir_gen;
pub mod lexer;
pub mod parser;
pub mod typing;
pub mod utility;

/// Parses command line arguments and runs the compiler on them.
pub fn main() {
    let args = Arguments::parse();
    let args = MainArguments::new(args);
    if let Err(msg) = args.and_then(MainArguments::run) {
        eprintln!("{msg}");
        process::exit(1);
    };
}
