use bumpalo::{collections::Vec, Bump};
use clap::ArgAction;
use clap::{Arg, Command};
use std::fs;
use std::io;
use std::iter::once;
use std::process;

use roc_wasm_interp::{DefaultImportDispatcher, Instance};
use roc_wasm_module::WasmModule;

pub const FLAG_FUNCTION: &str = "function";
pub const FLAG_DEBUG: &str = "debug";
pub const FLAG_HEX: &str = "hex";
pub const WASM_FILE: &str = "WASM_FILE";
pub const ARGS_FOR_APP: &str = "ARGS_FOR_APP";

fn main() -> io::Result<()> {
    let arena = Bump::new();

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
        .required(true);

    let args_for_app = Arg::new(ARGS_FOR_APP)
        .help("Arguments to pass into the WebAssembly app\ne.g. `roc_wasm_interp app.wasm 123 123.45`")
        .num_args(0..);

    let app = Command::new("roc_wasm_interp")
        .about("Run the given .wasm file")
        .arg(flag_function)
        .arg(flag_debug)
        .arg(flag_hex)
        .arg(wasm_file_to_run)
        .arg(args_for_app);

    // Parse the command line arguments

    let matches = app.get_matches();
    let start_fn_name = matches.get_one::<String>(FLAG_FUNCTION).unwrap();
    let is_debug_mode = matches.get_flag(FLAG_DEBUG);
    let is_hex_format = matches.get_flag(FLAG_HEX);
    let start_arg_strings = matches.get_many::<String>(ARGS_FOR_APP).unwrap_or_default();
    let wasm_path = matches.get_one::<String>(WASM_FILE).unwrap();
    // WASI expects the .wasm file to be argv[0]
    let wasi_argv_iter = once(wasm_path)
        .chain(start_arg_strings)
        .map(|s| s.as_bytes());
    let wasi_argv = Vec::from_iter_in(wasi_argv_iter, &arena);

    // Load the WebAssembly binary file

    let module_bytes = fs::read(wasm_path)?;

    // Parse the binary data

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

    // Create an execution instance

    let dispatcher = DefaultImportDispatcher::new(&wasi_argv);
    let mut inst =
        Instance::for_module(&arena, &module, dispatcher, is_debug_mode).unwrap_or_else(|e| {
            eprintln!("{e}");
            process::exit(2);
        });

    // Run

    let result = inst.call_export_from_cli(&module, start_fn_name, &wasi_argv);

    // Print out return value, if any

    match result {
        Ok(Some(val)) => {
            if is_hex_format {
                println!("{val:#x?}")
            } else {
                println!("{val:?}")
            }
        }
        Ok(None) => {}
        Err(e) => {
            eprintln!("{e}");
            process::exit(3);
        }
    }

    Ok(())
}
