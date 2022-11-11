use std::{
    cell::RefCell,
    fs,
    ops::{Deref, DerefMut},
    path::Path,
    sync::Mutex,
    thread_local,
};
use wasmer::{
    imports, ChainableNamedResolver, Function, ImportObject, Instance, Module, Store, Value,
};
use wasmer_wasi::WasiState;

const WASM_REPL_COMPILER_PATH: &str = "../../target/wasm32-wasi/release/roc_repl_wasm.wasm";

thread_local! {
    static REPL_STATE: RefCell<Option<ReplState>> = RefCell::new(None)
}

// The compiler Wasm instance.
// This takes several *seconds* to initialise, so we only want to do it once for all tests.
// Every test mutates compiler memory in `unsafe` ways, so we run them sequentially using a Mutex.
// Even if Cargo uses many threads, these tests won't go any faster. But that's fine, they're quick.
lazy_static! {
    static ref COMPILER: Instance = init_compiler();
}

static TEST_MUTEX: Mutex<()> = Mutex::new(());

/// Load the compiler .wasm file and get it ready to execute
/// THIS FUNCTION TAKES 4 SECONDS TO RUN
fn init_compiler() -> Instance {
    let path = Path::new(WASM_REPL_COMPILER_PATH);
    let wasm_module_bytes = match fs::read(&path) {
        Ok(bytes) => bytes,
        Err(e) => panic!("{}", format_compiler_load_error(e)),
    };
    println!("loaded Roc compiler bytes");

    let store = Store::default();

    // This is the slow line. Skipping validation checks reduces module compilation time from 5s to 4s.
    // Safety: We trust rustc to produce a valid module.
    let wasmer_module =
        unsafe { Module::from_binary_unchecked(&store, &wasm_module_bytes).unwrap() };

    // Specify the external functions the Wasm module needs to link to
    // We only use WASI so that we can debug test failures more easily with println!(), dbg!(), etc.
    let mut wasi_env = WasiState::new("compiler").finalize().unwrap();
    let wasi_import_obj = wasi_env
        .import_object(&wasmer_module)
        .unwrap_or_else(|_| ImportObject::new());
    let repl_import_obj = imports! {
        "env" => {
            "wasmer_create_app" => Function::new_native(&store, wasmer_create_app),
            "wasmer_run_app" => Function::new_native(&store, wasmer_run_app),
            "wasmer_get_result_and_memory" => Function::new_native(&store, wasmer_get_result_and_memory),
            "wasmer_copy_input_string" => Function::new_native(&store, wasmer_copy_input_string),
            "wasmer_copy_output_string" => Function::new_native(&store, wasmer_copy_output_string),
            "now" => Function::new_native(&store, dummy_system_time_now),
        }
    };
    // "Chain" the import objects together. Wasmer will look up the REPL object first, then the WASI object
    let import_object = wasi_import_obj.chain_front(repl_import_obj);

    println!("Instantiating Roc compiler");

    // Make a fully-linked instance with its own block of memory
    let inst = Instance::new(&wasmer_module, &import_object).unwrap();

    println!("Instantiated Roc compiler");

    inst
}

struct ReplState {
    src: &'static str,
    app: Option<Instance>,
    result_addr: Option<u32>,
    output: Option<String>,
}

fn wasmer_create_app(app_bytes_ptr: u32, app_bytes_len: u32) -> u32 {
    let app: Instance = {
        let memory = COMPILER.exports.get_memory("memory").unwrap();
        let memory_bytes: &[u8] = unsafe { memory.data_unchecked() };

        // Find the slice of bytes for the compiled Roc app
        let ptr = app_bytes_ptr as usize;
        let len = app_bytes_len as usize;
        let app_module_bytes: &[u8] = &memory_bytes[ptr..][..len];

        // Parse the bytes into a Wasmer module
        let store = Store::default();
        let wasmer_module = match Module::new(&store, app_module_bytes) {
            Ok(m) => m,
            Err(e) => {
                println!("Failed to create Wasm module\n{:?}", e);
                if false {
                    let path = std::env::temp_dir().join("roc_repl_test_invalid_app.wasm");
                    fs::write(&path, app_module_bytes).unwrap();
                    println!("Wrote invalid wasm to {:?}", path);
                }
                return false.into();
            }
        };

        // Get the WASI imports for the app
        let mut wasi_env = WasiState::new("app").finalize().unwrap();
        let import_object = wasi_env
            .import_object(&wasmer_module)
            .unwrap_or_else(|_| imports!());

        // Create an executable instance
        match Instance::new(&wasmer_module, &import_object) {
            Ok(instance) => instance,
            Err(e) => {
                println!("Failed to create Wasm instance {:?}", e);
                return false.into();
            }
        }
    };

    REPL_STATE.with(|f| {
        if let Some(state) = f.borrow_mut().deref_mut() {
            state.app = Some(app)
        } else {
            unreachable!()
        }
    });

    return true.into();
}

fn wasmer_run_app() -> u32 {
    REPL_STATE.with(|f| {
        if let Some(state) = f.borrow_mut().deref_mut() {
            if let Some(app) = &state.app {
                let wrapper = app.exports.get_function("wrapper").unwrap();

                let result_addr: i32 = match wrapper.call(&[]) {
                    Err(e) => panic!("{:?}", e),
                    Ok(result) => result[0].unwrap_i32(),
                };
                state.result_addr = Some(result_addr as u32);

                let memory = app.exports.get_memory("memory").unwrap();
                memory.size().bytes().0 as u32
            } else {
                unreachable!()
            }
        } else {
            unreachable!()
        }
    })
}

fn wasmer_get_result_and_memory(buffer_alloc_addr: u32) -> u32 {
    REPL_STATE.with(|f| {
        if let Some(state) = f.borrow().deref() {
            if let Some(app) = &state.app {
                let app_memory = app.exports.get_memory("memory").unwrap();
                let result_addr = state.result_addr.unwrap();

                let app_memory_bytes: &[u8] = unsafe { app_memory.data_unchecked() };

                let buf_addr = buffer_alloc_addr as usize;
                let len = app_memory_bytes.len();

                let memory = COMPILER.exports.get_memory("memory").unwrap();
                let compiler_memory_bytes: &mut [u8] = unsafe { memory.data_unchecked_mut() };
                compiler_memory_bytes[buf_addr..][..len].copy_from_slice(app_memory_bytes);

                result_addr
            } else {
                panic!("REPL app not found")
            }
        } else {
            panic!("REPL state not found")
        }
    })
}

fn wasmer_copy_input_string(src_buffer_addr: u32) {
    let src = REPL_STATE.with(|rs| {
        if let Some(state) = rs.borrow_mut().deref_mut() {
            std::mem::take(&mut state.src)
        } else {
            unreachable!()
        }
    });

    let memory = COMPILER.exports.get_memory("memory").unwrap();
    let memory_bytes: &mut [u8] = unsafe { memory.data_unchecked_mut() };

    let buf_addr = src_buffer_addr as usize;
    let len = src.len();
    memory_bytes[buf_addr..][..len].copy_from_slice(src.as_bytes());
}

fn wasmer_copy_output_string(output_ptr: u32, output_len: u32) {
    let output: String = {
        let memory = COMPILER.exports.get_memory("memory").unwrap();
        let memory_bytes: &[u8] = unsafe { memory.data_unchecked() };

        // Find the slice of bytes for the output string
        let ptr = output_ptr as usize;
        let len = output_len as usize;
        let output_bytes: &[u8] = &memory_bytes[ptr..][..len];

        // Copy it out of the Wasm module
        let copied_bytes = output_bytes.to_vec();
        unsafe { String::from_utf8_unchecked(copied_bytes) }
    };

    REPL_STATE.with(|f| {
        if let Some(state) = f.borrow_mut().deref_mut() {
            state.output = Some(output)
        }
    })
}

fn format_compiler_load_error(e: std::io::Error) -> String {
    if matches!(e.kind(), std::io::ErrorKind::NotFound) {
        format!(
            "\n\n    {}\n\n",
            [
                "ROC COMPILER WASM BINARY NOT FOUND",
                "Please run these tests using repl_test/run_wasm.sh!",
                "It will build a .wasm binary for the compiler, and a native binary for the tests themselves",
            ]
            .join("\n    ")
        )
    } else {
        format!("{:?}", e)
    }
}

fn dummy_system_time_now() -> f64 {
    0.0
}

fn run(src: &'static str) -> (bool, String) {
    println!("run");
    REPL_STATE.with(|rs| {
        *rs.borrow_mut().deref_mut() = Some(ReplState {
            src,
            app: None,
            result_addr: None,
            output: None,
        });
    });

    let ok = if let Ok(_guard) = TEST_MUTEX.lock() {
        let entrypoint = COMPILER
            .exports
            .get_function("entrypoint_from_test")
            .unwrap();

        let src_len = Value::I32(src.len() as i32);
        let wasm_ok: i32 = entrypoint.call(&[src_len]).unwrap().deref()[0].unwrap_i32();
        wasm_ok != 0
    } else {
        panic!(
            "Failed to acquire test mutex! A previous test must have panicked while holding it, running Wasm"
        )
    };

    let final_state: ReplState = REPL_STATE.with(|rs| rs.take()).unwrap();
    let output: String = final_state.output.unwrap();

    (ok, output)
}

#[allow(dead_code)]
pub fn expect_success(input: &'static str, expected: &str) {
    let (ok, output) = run(input);
    if !ok {
        panic!("\n{}\n", output);
    }
    assert_eq!(output, expected);
}

#[allow(dead_code)]
pub fn expect_failure(input: &'static str, expected: &str) {
    let (ok, output) = run(input);
    assert_eq!(ok, false);
    assert_eq!(output, expected);
}
