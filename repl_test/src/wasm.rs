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
    static COMPILER: RefCell<Option<Instance>> = RefCell::new(None)
}

thread_local! {
    static REPL_STATE: RefCell<Option<ReplState>> = RefCell::new(None)
}

struct ReplState {
    src: &'static str,
    app: Option<Instance>,
    result_addr: Option<u32>,
    output: Option<String>,
}

fn wasmer_create_app(app_bytes_ptr: u32, app_bytes_len: u32) {
    let app = COMPILER.with(|f| {
        if let Some(compiler) = f.borrow().deref() {
            let memory = compiler.exports.get_memory("memory").unwrap();
            let memory_bytes: &[u8] = unsafe { memory.data_unchecked() };

            // Find the slice of bytes for the compiled Roc app
            let ptr = app_bytes_ptr as usize;
            let len = app_bytes_len as usize;
            let app_module_bytes: &[u8] = &memory_bytes[ptr..][..len];

            // Parse the bytes into a Wasmer module
            let store = Store::default();
            let wasmer_module = Module::new(&store, app_module_bytes).unwrap();

            // Get the WASI imports for the app
            let mut wasi_env = WasiState::new("hello").finalize().unwrap();
            let import_object = wasi_env
                .import_object(&wasmer_module)
                .unwrap_or_else(|_| imports!());

            // Create an executable instance. (Give it a stack & heap, etc. If this was ELF, it would be the OS's job.)
            Instance::new(&wasmer_module, &import_object).unwrap()
        } else {
            unreachable!()
        }
    });

    REPL_STATE.with(|f| {
        if let Some(state) = f.borrow_mut().deref_mut() {
            state.app = Some(app)
        } else {
            unreachable!()
        }
    });
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

                COMPILER.with(|f| {
                    if let Some(compiler) = f.borrow().deref() {
                        let memory = compiler.exports.get_memory("memory").unwrap();
                        let compiler_memory_bytes: &mut [u8] =
                            unsafe { memory.data_unchecked_mut() };
                        compiler_memory_bytes[buf_addr..][..len].copy_from_slice(app_memory_bytes);
                    } else {
                        unreachable!()
                    }
                });

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

    COMPILER.with(|c| {
        if let Some(compiler) = c.borrow().deref() {
            let memory = compiler.exports.get_memory("memory").unwrap();
            let memory_bytes: &mut [u8] = unsafe { memory.data_unchecked_mut() };

            let buf_addr = src_buffer_addr as usize;
            let len = src.len();
            memory_bytes[buf_addr..][..len].copy_from_slice(src.as_bytes());
        } else {
            unreachable!()
        }
    })
}

fn wasmer_copy_output_string(output_ptr: u32, output_len: u32) {
    let output: String = COMPILER.with(|c| {
        if let Some(compiler) = c.borrow().deref() {
            let memory = compiler.exports.get_memory("memory").unwrap();
            let memory_bytes: &[u8] = unsafe { memory.data_unchecked() };

            // Find the slice of bytes for the output string
            let ptr = output_ptr as usize;
            let len = output_len as usize;
            let output_bytes: &[u8] = &memory_bytes[ptr..][..len];

            // Copy it out of the Wasm module
            let copied_bytes = output_bytes.to_vec();
            unsafe { String::from_utf8_unchecked(copied_bytes) }
        } else {
            unreachable!()
        }
    });

    REPL_STATE.with(|f| {
        if let Some(state) = f.borrow_mut().deref_mut() {
            state.output = Some(output)
        }
    })
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

fn run(src: &'static str) -> (bool, String) {
    REPL_STATE.with(|rs| {
        *rs.borrow_mut().deref_mut() = Some(ReplState {
            src,
            app: None,
            result_addr: None,
            output: None,
        });
    });

    let ok = COMPILER.with(|c| {
        *c.borrow_mut().deref_mut() = Some(init_compiler());

        if let Some(compiler) = c.borrow().deref() {
            let entrypoint = compiler
                .exports
                .get_function("entrypoint_from_test")
                .unwrap();

            let src_len = Value::I32(src.len() as i32);
            let wasm_ok: i32 = entrypoint.call(&[src_len]).unwrap().deref()[0].unwrap_i32();
            wasm_ok != 0
        } else {
            unreachable!()
        }
    });

    let final_state: ReplState = REPL_STATE.with(|rs| rs.take()).unwrap();
    let output: String = final_state.output.unwrap();

    (ok, output)
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

fn dummy_system_time_now() -> f64 {
    0.0
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
