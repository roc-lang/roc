use clap::{ArgEnum, Parser};
use std::io::ErrorKind;
use std::path::PathBuf;
use std::{fs, process};

#[derive(Clone, Debug, ArgEnum)]
enum TargetFormat {
    Rust,
    Json,
    C,
    Zig,
}

// TODO add an option for --targets so that you can specify
// e.g. 64-bit, 32-bit, *and* 16-bit (which can matter for alignment because of pointers)
#[derive(Debug, Parser)]
#[clap(about)]
struct Opts {
    /// The output format
    #[clap(arg_enum)]
    target: TargetFormat,

    /// The path to the platform's Package-Config.roc file
    platform_module: PathBuf,
}

pub fn main() {
    let opts = Opts::parse();
    let path = opts.platform_module;

    match fs::read_to_string(&path) {
        Ok(src) => {
            println!("Got this source: {:?}", src);
        }
        Err(err) => match err.kind() {
            ErrorKind::NotFound => {
                eprintln!("Platform module file not found: {}", path.display());
                process::exit(1);
            }
            error => {
                eprintln!(
                    "Error loading platform module file {} - {:?}",
                    path.display(),
                    error
                );
                process::exit(1);
            }
        },
    }
}
