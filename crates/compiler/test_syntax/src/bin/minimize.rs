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

use std::io::Read;

use test_syntax::{
    minimize::{print_minimizations, Options},
    test_helpers::InputKind,
};

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() < 3 || args.len() > 5 {
        eprintln!("Usage: {} [--minimize-full-error] [--minimize-initial-parse-error] [expr|full|moduledefs|header|pattern] <input>", args[0]);
        std::process::exit(1);
    }

    let mut options = Options {
        kind: InputKind::Expr,
        minimize_full_error: false,
        minimize_initial_parse_error: false,
    };

    let mut index = 1;
    while index < args.len() - 2 {
        match args[index].as_str() {
            "--minimize-full-error" => options.minimize_full_error = true,
            "--minimize-initial-parse-error" => options.minimize_initial_parse_error = true,
            _ => {
                eprintln!("Invalid option: {}", args[index]);
                std::process::exit(1);
            }
        }
        index += 1;
    }

    options.kind = match args[index].as_str() {
        "expr" => InputKind::Expr,
        "full" => InputKind::Full,
        "moduledefs" => InputKind::ModuleDefs,
        "header" => InputKind::Header,
        "pattern" => InputKind::Pattern,
        _ => {
            eprintln!("Invalid input kind: {}", args[index]);
            std::process::exit(1);
        }
    };
    let input = &args[index + 1];
    let mut buf = String::new();
    let text = if input == "-" {
        std::io::stdin().read_to_string(&mut buf).unwrap();
        buf.to_string()
    } else {
        std::fs::read_to_string(input).unwrap()
    };

    let found_error = print_minimizations(&text, options);
    std::process::exit(if found_error { 0 } else { 1 });
}
