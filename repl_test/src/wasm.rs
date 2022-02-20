use std::{
    cell::RefCell,
    env, fs,
    ops::{Deref, DerefMut},
    path::Path,
    thread_local,
};
use wasmer::{imports, Function, Instance, Module, Store};
use wasmer_wasi::WasiState;

const COMPILER_PATH_ENV_VAR: &str = "COMPILER_PATH";

thread_local! {
    static REPL_STATE: RefCell<Option<Env<'static>>> = RefCell::new(None)
}

struct Env {
    src: &'static str,
    compiler: Instance,
    app: Option<Instance>,
    result_addr: Option<u32>,
}

fn wasmer_create_app(app_bytes: &[u8]) {
    let store = Store::default();
    let wasmer_module = Module::new(&store, &app_bytes).unwrap();

    // First, we create the `WasiEnv`
    let mut wasi_env = WasiState::new("hello").finalize().unwrap();

    // Then, we get the import object related to our WASI
    // and attach it to the Wasm instance.
    let import_object = wasi_env
        .import_object(&wasmer_module)
        .unwrap_or_else(|_| imports!());

    let instance = Instance::new(&wasmer_module, &import_object).unwrap();

    REPL_STATE.with(|f| {
        if let Some(state) = f.borrow_mut().deref_mut() {
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

fn init_compiler() -> Vec<u8> {
    let path_str = env::var(COMPILER_PATH_ENV_VAR).unwrap();
    let path = Path::new(&path_str);
    let wasm_module_bytes = fs::read(&path).unwrap();

    let store = Store::default();
    let wasmer_module = Module::new(&store, &wasm_module_bytes).unwrap();

    let import_object = imports! {
        "env" => {
            "wasmer_create_app" => Function::new_native(&store, wasmer_create_app),
            "wasmer_run_app" => Function::new_native(&store, wasmer_run_app),
            "wasmer_get_result_and_memory" => Function::new_native(&store, wasmer_get_result_and_memory),
            "wasmer_copy_input_string" => Function::new_native(&store, wasmer_copy_input_string),
        }
    };

    Instance::new(&wasmer_module, &import_object).unwrap()
}

fn run(src: &str) -> String {
    let compiler = init_compiler();

    let entrypoint = compiler
        .exports
        .get_function("entrypoint_from_test")
        .unwrap();

    REPL_STATE.with(|f| {
        let new_state = Env {
            src,
            compiler,
            app: None,
            result_addr: None,
        };
        let current_state = f.borrow_mut().deref_mut();
        assert_eq!(current_state, None);
        *current_state = Some(new_state);
    });

    let actual = String::new();
    actual
}

pub fn expect_success(input: &str, expected: &str) {
    let actual = run(input);
    assert_eq!(actual, expected);
}

pub fn expect_failure(input: &str, expected: &str) {
    let actual = run(input);
    assert_eq!(actual, expected);
}
