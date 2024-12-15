//! Generate a minimized version of a given input, by removing parts of it.
//! This is useful for debugging, when you have a large input that causes a failure,
//! and you want to find the smallest input that still causes the failure.
//!
//! Typical usage:
//! `cargo run --release --bin minimize -- full <file_that_triggers_parsing_bug>`
//!
//! This tool will churn on that for a while, and eventually print out a minimized version
//! of the input that still triggers the bug.
//!
//! Note that `--release` is important, as this tool is very slow in debug mode.

use test_syntax::{minimize::print_minimizations, test_helpers::InputKind};

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() != 3 {
        eprintln!("Usage: {} [expr|full|moduledefs|header] <input>", args[0]);
        std::process::exit(1);
    }

    let kind = match args[1].as_str() {
        "expr" => InputKind::Expr,
        "full" => InputKind::Full,
        "moduledefs" => InputKind::ModuleDefs,
        "header" => InputKind::Header,
        _ => {
            eprintln!("Invalid input kind: {}", args[1]);
            std::process::exit(1);
        }
    };

    let text = std::fs::read_to_string(&args[2]).unwrap();
    let found_error = print_minimizations(&text, kind);
    std::process::exit(if found_error { 0 } else { 1 });
}
