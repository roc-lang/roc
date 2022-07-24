use clap::Parser;
use roc_bindgen::bindgen_rs;
use roc_bindgen::load::load_types;
use roc_load::Threading;
use std::ffi::OsStr;
use std::fs::File;
use std::io::{ErrorKind, Write};
use std::path::PathBuf;
use std::process;

/// Printed in error messages if you try to use an unsupported extension.
const SUPPORTED_EXTENSIONS: &str = ".c, .rs, .zig, and .json";

// TODO add an option for --targets so that you can specify
// e.g. 64-bit, 32-bit, *and* 16-bit (which can matter for alignment because of pointers)
#[derive(Debug, Parser)]
#[clap(about)]
struct Opts {
    /// The path to the `platform` module .roc file
    platform_module: PathBuf,

    /// The output file, e.g. `test.rs`
    dest: PathBuf,
}

enum OutputType {
    Rust,
    C,
    Zig,
    Json,
}

pub fn main() {
    let opts = Opts::parse();
    let input_path = opts.platform_module;
    let output_path = opts.dest;
    let output_type = match output_path.extension().and_then(OsStr::to_str) {
        Some("rs") => OutputType::Rust,
        Some("c") => OutputType::C,
        Some("zig") => OutputType::Zig,
        Some("json") => OutputType::Json,
        Some(other) => {
            eprintln!(
                "Unsupported output file extension: \".{}\" - currently supported extensions are {}",
                other,
                SUPPORTED_EXTENSIONS
            );

            process::exit(1);
        }
        None => {
            eprintln!("The output file path needs to have a file extension in order to tell what output format to use. Currently supported extensions are {}", SUPPORTED_EXTENSIONS);

            process::exit(1);
        }
    };

    match load_types(input_path.clone(), Threading::AllAvailable) {
        Ok(types_and_targets) => {
            let mut file = File::create(output_path.clone()).unwrap_or_else(|err| {
                eprintln!(
                    "Unable to create output file {} - {:?}",
                    output_path.display(),
                    err
                );

                process::exit(1);
            });

            let mut buf;
            match output_type {
                OutputType::Rust => {
                    buf = std::str::from_utf8(bindgen_rs::HEADER).unwrap().to_string();
                    let body = bindgen_rs::emit(&types_and_targets);

                    buf.push_str(&body);
                }
                OutputType::C => todo!("TODO: Generate bindings for C"),
                OutputType::Zig => todo!("TODO: Generate bindings for Zig"),
                OutputType::Json => todo!("TODO: Generate bindings for JSON"),
            };

            file.write_all(buf.as_bytes()).unwrap_or_else(|err| {
                eprintln!(
                    "Unable to write bindings to output file {} - {:?}",
                    output_path.display(),
                    err
                );

                process::exit(1);
            });

            println!(
                "🎉 Generated type declarations in:\n\n\t{}",
                output_path.display()
            );
        }
        Err(err) => match err.kind() {
            ErrorKind::NotFound => {
                eprintln!("Platform module file not found: {}", input_path.display());
                process::exit(1);
            }
            error => {
                eprintln!(
                    "Error loading platform module file {} - {:?}",
                    input_path.display(),
                    error
                );
                process::exit(1);
            }
        },
    }
}
