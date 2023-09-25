//! Types and functions for providing the command line interface.

macro_rules! verbose_println {
    ($args:expr, $($msg:tt),*) => {
        if $args.verbose {
            println!($($msg,)*)
        }
    }
}

pub mod main_command;
#[cfg(test)]
pub mod tests;

#[doc(hidden)]
mod arguments {
    use clap::Parser;
    use clap::ValueEnum;
    use std::path::PathBuf;

    macro_rules! about {
        () => {
            "beta language compiler"
        };
    }
    macro_rules! extended {
        () => {
            "documentation at https://reubenhillyard.github.io/beta/"
        };
    }

    const ABOUT: &str = about!();
    const LONG_ABOUT: &str = concat!(about!(), "\n\n", extended!());

    /// The record of command line arguments passed to [`main`](crate::main).
    #[derive(Parser)]
    #[command(version, about = ABOUT, long_about = LONG_ABOUT)]
    pub struct Arguments {
        /// The input file
        pub path: PathBuf,

        /// Where to output
        #[arg(short, value_name = "PATH")]
        pub output: PathBuf,

        /// Optimization level
        #[arg(short = 'O', default_value = "g")]
        pub opt_level: OptLevel,

        /// Produce verbose output
        #[arg(long)]
        pub verbose: bool,

        /// Emit unoptimized LLVM IR
        #[arg(long, value_name = "PATH")]
        pub emit_un_opt_llvm: Option<PathBuf>,

        /// Emit LLVM IR
        #[arg(long, value_name = "PATH")]
        pub emit_llvm: Option<PathBuf>,
    }

    /// How to optimize the input.
    #[derive(ValueEnum, Copy, Clone)]
    pub enum OptLevel {
        /// Perform no optimization
        #[clap(name = "0")]
        Zero,
        /// Optimize only by discarding unused globals
        #[clap(name = "g")]
        GlobalDeadCodeElimination,
        #[clap(name = "1")]
        One,
        #[clap(name = "2")]
        Two,
        #[clap(name = "3")]
        Three,
    }
}

pub use arguments::Arguments;
pub use arguments::OptLevel;
