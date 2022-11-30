use bumpalo::Bump;
use clap::ArgAction;
use clap::{Arg, Command};
use roc_wasm_interp::Action;
use std::ffi::OsString;
use std::fs;
use std::io;
use std::process;

use roc_wasm_interp::Instance;
use roc_wasm_module::WasmModule;

pub const FLAG_FUNCTION: &str = "function";
pub const FLAG_DEBUG: &str = "debug";
pub const FLAG_HEX: &str = "hex";
pub const WASM_FILE: &str = "WASM_FILE";
pub const ARGS_FOR_APP: &str = "ARGS_FOR_APP";

fn main() -> io::Result<()> {
    // Define the command line arguments

    let flag_function = Arg::new(FLAG_FUNCTION)
        .long(FLAG_FUNCTION)
        .help("Call a specific function exported from the WebAssembly module")
        .default_value("_start")
        .required(false);

    let flag_debug = Arg::new(FLAG_DEBUG)
        .long(FLAG_DEBUG)
        .help("Print a log of every instruction executed, for debugging purposes.")
        .action(ArgAction::SetTrue)
        .required(false);

    let flag_hex = Arg::new(FLAG_HEX)
        .long(FLAG_HEX)
        .help("If the called function returns a value, print it in hexadecimal format.")
        .action(ArgAction::SetTrue)
        .required(false);

    let wasm_file_to_run = Arg::new(WASM_FILE)
        .help("The .wasm file to run")
        .allow_invalid_utf8(true)
        .required(true);

    let args_for_app = Arg::new(ARGS_FOR_APP)
        .help("Arguments to pass into the WebAssembly app\ne.g. `roc_wasm_interp app.wasm -- 123 123.45`")
        .multiple_values(true)
        .takes_value(true)
        .last(true);

    let app = Command::new("roc_wasm_interp")
        .about("Run the given .wasm file")
        .arg(flag_function)
        .arg(flag_debug)
        .arg(flag_hex)
        .arg(wasm_file_to_run)
        .trailing_var_arg(true)
        .arg(args_for_app);

    // Parse the command line arguments

    let matches = app.get_matches();
    let start_fn_name = matches.get_one::<String>(FLAG_FUNCTION).unwrap();
    let is_debug_mode = matches.get_flag(FLAG_DEBUG);
    let is_hex_format = matches.get_flag(FLAG_HEX);
    let start_arg_strings = matches
        .get_many::<String>(ARGS_FOR_APP)
        .unwrap_or_default()
        .map(|s| s.as_str());

    // Load the WebAssembly binary file

    let wasm_path = matches.get_one::<OsString>(WASM_FILE).unwrap();
    let module_bytes = fs::read(wasm_path)?;

    // Parse the binary data

    let arena = Bump::new();
    let require_relocatable = false;
    let module = match WasmModule::preload(&arena, &module_bytes, require_relocatable) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("I couldn't parse this WebAssembly module! There's something wrong at byte offset {:#x}.",  e.offset);
            eprintln!("{}", e.message);
            eprintln!("If you think this could be a code generation problem in the Roc compiler, see crates/compiler/gen_wasm/README.md for debugging tips.");
            process::exit(1);
        }
    };

    // Initialise the execution state

    let mut state = Instance::for_module(
        &arena,
        &module,
        start_fn_name,
        is_debug_mode,
        start_arg_strings,
    )
    .unwrap_or_else(|e| {
        eprintln!("{}", e);
        process::exit(2);
    });

    // Run

    while let Action::Continue = state.execute_next_instruction(&module) {}

    // Print out return value(s), if any

    match state.value_stack.len() {
        0 => {}
        1 => {
            if is_hex_format {
                println!("{:#x?}", state.value_stack.pop())
            } else {
                println!("{:?}", state.value_stack.pop())
            }
        }
        _ => {
            if is_hex_format {
                println!("{:#x?}", &state.value_stack)
            } else {
                println!("{:?}", &state.value_stack)
            }
        }
    }

    Ok(())
}
