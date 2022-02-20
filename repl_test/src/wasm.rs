use std::{
    cell::RefCell,
    fs,
    ops::{Deref, DerefMut},
    path::Path,
    thread_local,
};
use wasmer::{imports, Function, Instance, Module, Store, Value};
use wasmer_wasi::WasiState;

const WASM_REPL_COMPILER_PATH: &str = "../target/wasm32-unknown-unknown/release/roc_repl_wasm.wasm";

thread_local! {
    static REPL_STATE: RefCell<Option<ReplState>> = RefCell::new(None)
}

struct ReplState {
    src: &'static str,
    compiler: Instance,
    app: Option<Instance>,
    result_addr: Option<u32>,
    output: Option<String>,
}

fn wasmer_create_app(app_bytes_ptr: u32, app_bytes_len: u32) {
    REPL_STATE.with(|f| {
        if let Some(state) = f.borrow_mut().deref_mut() {
            let compiler_memory = state.compiler.exports.get_memory("memory").unwrap();
            let compiler_memory_bytes: &mut [u8] = unsafe { compiler_memory.data_unchecked_mut() };

            // Find the slice of bytes for the compiled Roc app
            let ptr = app_bytes_ptr as usize;
            let len = app_bytes_len as usize;
            let app_module_bytes: &[u8] = &compiler_memory_bytes[ptr..][..len];

            // Parse the bytes into a Wasmer module
            let store = Store::default();
            let wasmer_module = Module::new(&store, app_module_bytes).unwrap();

            // Get the WASI imports for the app
            let mut wasi_env = WasiState::new("hello").finalize().unwrap();
            let import_object = wasi_env
                .import_object(&wasmer_module)
                .unwrap_or_else(|_| imports!());

            // Create an executable instance (give it a stack & heap, etc. For ELF, this would be the OS's job.)
            let instance = Instance::new(&wasmer_module, &import_object).unwrap();
            state.app = Some(instance)
        } else {
            panic!("REPL state not found")
        }
    })
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
                panic!("App not found")
            }
        } else {
            panic!("REPL state not found")
        }
    })
}

fn wasmer_get_result_and_memory(buffer_alloc_addr: u32) -> u32 {
    REPL_STATE.with(|f| {
        if let Some(state) = f.borrow().deref() {
            if let Some(app) = &state.app {
                let app_memory = app.exports.get_memory("memory").unwrap();
                let compiler_memory = state.compiler.exports.get_memory("memory").unwrap();
                let result_addr = state.result_addr.unwrap();

                let compiler_memory_bytes: &mut [u8] =
                    unsafe { compiler_memory.data_unchecked_mut() };

                let app_memory_bytes: &[u8] = unsafe { app_memory.data_unchecked() };

                let buf_addr = buffer_alloc_addr as usize;
                let len = app_memory_bytes.len();
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
    REPL_STATE.with(|f| {
        if let Some(state) = f.borrow().deref() {
            let compiler_memory = state.compiler.exports.get_memory("memory").unwrap();
            let compiler_memory_bytes: &mut [u8] = unsafe { compiler_memory.data_unchecked_mut() };

            let buf_addr = src_buffer_addr as usize;
            let len = state.src.len();
            compiler_memory_bytes[buf_addr..][..len].copy_from_slice(state.src.as_bytes());
        }
    })
}

fn wasmer_copy_output_string(output_ptr: u32, output_len: u32) {
    REPL_STATE.with(|f| {
        if let Some(state) = f.borrow_mut().deref_mut() {
            let compiler_memory = state.compiler.exports.get_memory("memory").unwrap();
            let compiler_memory_bytes: &mut [u8] = unsafe { compiler_memory.data_unchecked_mut() };

            // Find the slice of bytes for the compiled Roc app
            let ptr = output_ptr as usize;
            let len = output_len as usize;
            let output_bytes: &[u8] = &compiler_memory_bytes[ptr..][..len];

            // Copy it out of the Wasm module
            let copied_bytes = output_bytes.to_vec();
            let output = unsafe { String::from_utf8_unchecked(copied_bytes) };

            state.output = Some(output)
        }
    })
}

fn dummy_system_time_now() -> f64 {
    0.0
}

fn init_compiler() -> Instance {
    let path = Path::new(WASM_REPL_COMPILER_PATH);
    let wasm_module_bytes = match fs::read(&path) {
        Ok(bytes) => bytes,
        Err(e) => panic!("{}", format_compiler_load_error(e)),
    };

    let store = Store::default();
    let wasmer_module = Module::new(&store, &wasm_module_bytes).unwrap();

    let import_object = imports! {
        "env" => {
            "wasmer_create_app" => Function::new_native(&store, wasmer_create_app),
            "wasmer_run_app" => Function::new_native(&store, wasmer_run_app),
            "wasmer_get_result_and_memory" => Function::new_native(&store, wasmer_get_result_and_memory),
            "wasmer_copy_input_string" => Function::new_native(&store, wasmer_copy_input_string),
            "wasmer_copy_output_string" => Function::new_native(&store, wasmer_copy_output_string),
            "now" => Function::new_native(&store, dummy_system_time_now),
        }
    };

    Instance::new(&wasmer_module, &import_object).unwrap()
}

fn format_compiler_load_error(e: std::io::Error) -> String {
    if matches!(e.kind(), std::io::ErrorKind::NotFound) {
        format!(
            "\n\n    {}\n\n",
            [
                "CANNOT BUILD WASM REPL TESTS",
                "Please run these tests using repl_test/run_wasm.sh!",
                "",
                "These tests combine two builds for two different targets,",
                "which Cargo doesn't handle very well.",
                "It probably requires a second target directory to avoid locks.",
                "We'll get to it eventually but it's not done yet.",
            ]
            .join("\n    ")
        )
    } else {
        format!("{:?}", e)
    }
}

fn run(src: &'static str) -> (bool, String) {
    REPL_STATE.with(|f| {
        let compiler = init_compiler();

        let new_state = ReplState {
            src,
            compiler,
            app: None,
            result_addr: None,
            output: None,
        };

        {
            *f.borrow_mut().deref_mut() = Some(new_state);
        }

        if let Some(state) = f.borrow().deref() {
            let entrypoint = state
                .compiler
                .exports
                .get_function("entrypoint_from_test")
                .unwrap();

            let src_len = Value::I32(src.len() as i32);
            let wasm_ok: i32 = entrypoint.call(&[src_len]).unwrap().deref()[0].unwrap_i32();
            let ok = wasm_ok != 0;

            let final_state = f.take().unwrap();

            (ok, final_state.output.unwrap())
        } else {
            panic!()
        }
    })
}

pub fn expect_success(input: &'static str, expected: &str) {
    let (ok, output) = run(input);
    assert_eq!(ok, true);
    assert_eq!(output, expected);
}

pub fn expect_failure(input: &'static str, expected: &str) {
    let (ok, output) = run(input);
    assert_eq!(ok, false);
    assert_eq!(output, expected);
}
