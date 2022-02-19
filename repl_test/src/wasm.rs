use std::{
    cell::RefCell,
    ops::{Deref, DerefMut},
    thread_local,
};
use wasmer::Instance;

thread_local! {
    static REPL_STATE: RefCell<Option<ReplState>> = RefCell::new(None)
}

struct ReplState {
    compiler: Instance,
    app: Option<Instance>,
    result_addr: Option<u32>,
}

fn instantiate_compiler(wasm_module_bytes: &[u8]) {
    use wasmer::{Module, Store};
    use wasmer_wasi::WasiState;

    let store = Store::default();
    let wasmer_module = Module::new(&store, &wasm_module_bytes).unwrap();

    // First, we create the `WasiEnv`
    let mut wasi_env = WasiState::new("hello").finalize().unwrap();

    // Then, we get the import object related to our WASI
    // and attach it to the Wasm instance.
    let import_object = wasi_env
        .import_object(&wasmer_module)
        .unwrap_or_else(|_| wasmer::imports!());

    let instance = wasmer::Instance::new(&wasmer_module, &import_object).unwrap();
    REPL_STATE.with(|f| {
        if let Some(state) = f.borrow_mut().deref_mut() {
            state.app = Some(instance)
        } else {
            panic!("REPL state not found")
        }
    })
}

fn wasmer_create_app(wasm_module_bytes: &[u8]) {
    use wasmer::{Module, Store};
    use wasmer_wasi::WasiState;

    let store = Store::default();
    let wasmer_module = Module::new(&store, &wasm_module_bytes).unwrap();

    // First, we create the `WasiEnv`
    let mut wasi_env = WasiState::new("hello").finalize().unwrap();

    // Then, we get the import object related to our WASI
    // and attach it to the Wasm instance.
    let import_object = wasi_env
        .import_object(&wasmer_module)
        .unwrap_or_else(|_| wasmer::imports!());

    let instance = wasmer::Instance::new(&wasmer_module, &import_object).unwrap();
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
                    Ok(result) => match result[0] {
                        wasmer::Value::I32(a) => a,
                        _ => panic!("Expected an i32 address, got {:?}", result),
                    },
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

pub fn expect_success(input: &str, expected: &str) {
    todo!()
}

pub fn expect_failure(input: &str, expected: &str) {
    todo!()
}
