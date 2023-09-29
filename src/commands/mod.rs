//! Types and functions for providing the command line interface.

use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet};
use itertools::Itertools;

#[doc(hidden)]
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

pub const FORMAT_OPTIONS: FormatOptions = FormatOptions {
    color: true,
    anonymized_line_numbers: false,
    margin: None,
};

/// Makes an error snippet with a given title and slices.
pub fn make_error_snippet<'a>(title: &'a str, slices: Vec<Slice<'a>>) -> Snippet<'a> {
    Snippet {
        title: Some(Annotation {
            id: None,
            label: Some(title),
            annotation_type: AnnotationType::Error,
        }),
        footer: vec![],
        slices,
        opt: FORMAT_OPTIONS,
    }
}

fn format_snippets(snippets: Vec<Snippet>) -> String {
    let num_errors: usize = snippets
        .iter()
        .filter_map(|snippet| {
            snippet.title.as_ref().and_then(|title| {
                if title.annotation_type == AnnotationType::Error {
                    Some(snippet.slices.len())
                } else {
                    None
                }
            })
        })
        .sum();
    let footer_label_inner = if num_errors < 2 {
        None
    } else {
        Some(format!("aborting due to {num_errors} previous errors"))
    };
    let footer_label = match footer_label_inner.as_ref() {
        None => {
            if num_errors == 0 {
                None
            } else {
                Some("aborting due to previous error")
            }
        }
        Some(footer_label) => Some(footer_label.as_str()),
    };
    let footer = footer_label.map(|label| make_error_snippet(label, vec![]));
    let body = snippets
        .into_iter()
        .chain(footer.into_iter())
        .map(DisplayList::from)
        .map(|dl| dl.to_string())
        .format("\n\n");
    format!("{body}")
}
