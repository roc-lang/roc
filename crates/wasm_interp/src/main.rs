use bumpalo::Bump;
use clap::{Arg, Command};
use roc_wasm_interp::Action;
use std::ffi::OsString;
use std::fs;
use std::io;
use std::process;

use roc_wasm_interp::ExecutionState;
use roc_wasm_module::WasmModule;

pub const FLAG_FUNCTION: &str = "function";
pub const WASM_FILE: &str = "WASM_FILE";

fn main() -> io::Result<()> {
    // Define the command line arguments

    let flag_function = Arg::new(FLAG_FUNCTION)
        .long(FLAG_FUNCTION)
        .help("Call a specific function exported from the WebAssembly module")
        .default_value("_start")
        .required(false);

    let wasm_file_to_run = Arg::new(WASM_FILE)
        .help("The .wasm file to run")
        .allow_invalid_utf8(true)
        .required(true);

    let app = Command::new("roc_wasm_interp")
        .about("Run the given .wasm file")
        .arg(flag_function)
        .arg(wasm_file_to_run);

    // Parse the command line arguments

    let matches = app.get_matches();
    let start_fn_name = matches.get_one::<String>(FLAG_FUNCTION).unwrap();

    // Load the WebAssembly binary file

    let wasm_path = matches.get_one::<OsString>(WASM_FILE).unwrap();
    let module_bytes = fs::read(wasm_path)?;

    // Parse the binary data

    let arena = Bump::new();
    let require_relocatable = false;
    let module = match WasmModule::preload(&arena, &module_bytes, require_relocatable) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("I couldn't parse this WebAssembly module! There's something wrong at byte offset 0x{}.",  e.offset);
            eprintln!("{}", e.message);
            eprintln!("If you think this could be a code generation problem in the Roc compiler, see crates/compiler/gen_wasm/README.md for debugging tips.");
            process::exit(1);
        }
    };

    // Initialise the execution state

    let mut state = match ExecutionState::for_module(&arena, &module, start_fn_name) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}", e);
            process::exit(2);
        }
    };

    // Run

    while let Action::Continue = state.execute_next_instruction(&module) {}

    Ok(())
}
