use bumpalo::Bump;
use roc_wasm_interp::{wasi, DefaultImportDispatcher, ImportDispatcher, Instance, WasiDispatcher};
use roc_wasm_module::{Value, WasmModule};

const COMPILER_BYTES: &[u8] =
    include_bytes!("../../../.ignore/repl_test_target/wasm32-wasi/release/roc_repl_wasm.wasm");

struct CompilerDispatcher<'a> {
    arena: &'a Bump,
    src: &'a str,
    answer: String,
    wasi: WasiDispatcher<'a>,
    app: Option<(WasmModule<'a>, Instance<'a, DefaultImportDispatcher<'a>>)>,
    result_addr: Option<i32>,
}

impl<'a> ImportDispatcher for CompilerDispatcher<'a> {
    fn dispatch(
        &mut self,
        module_name: &str,
        function_name: &str,
        arguments: &[Value],
        compiler_memory: &mut [u8],
    ) -> Option<Value> {
        println!("{}.{}{:?}", module_name, function_name, arguments);

        let unknown = || {
            panic!(
                "I could not find an implementation for import {}.{}",
                module_name, function_name
            )
        };

        if module_name == wasi::MODULE_NAME {
            self.wasi
                .dispatch(function_name, arguments, compiler_memory)
        } else if module_name == "env" {
            match function_name {
                "test_create_app" => {
                    // Get some bytes from the compiler Wasm instance and create the app Wasm instance
                    // fn test_create_app(app_bytes_ptr: *const u8, app_bytes_len: usize) -> u32;
                    assert_eq!(arguments.len(), 2);
                    let app_bytes_ptr = arguments[0].expect_i32().unwrap() as usize;
                    let app_bytes_len = arguments[1].expect_i32().unwrap() as usize;
                    let app_bytes = &compiler_memory[app_bytes_ptr..][..app_bytes_len];

                    let require_reloc = false;
                    let module = WasmModule::preload(self.arena, app_bytes, require_reloc).unwrap();

                    let is_debug_mode = false;
                    let instance = Instance::for_module(
                        self.arena,
                        &module,
                        DefaultImportDispatcher::default(),
                        is_debug_mode,
                    )
                    .unwrap();

                    self.app = Some((module, instance));
                    let ok = Value::I32(true as i32);
                    Some(ok)
                }
                "test_run_app" => {
                    // fn test_run_app() -> usize;
                    assert_eq!(arguments.len(), 0);
                    match &mut self.app {
                        Some((module, instance)) => {
                            let result_addr = instance
                                .call_export(module, "wrapper", [])
                                .unwrap()
                                .expect("No return address from wrapper")
                                .expect_i32()
                                .unwrap();
                            self.result_addr = Some(result_addr);
                            let memory_size = instance.memory.len();
                            Some(Value::I32(memory_size as i32))
                        }
                        None => panic!("Trying to run the app but it hasn't been created"),
                    }
                }
                "test_get_result_and_memory" => {
                    // Copy the app's entire memory buffer into the compiler's memory,
                    // and return the location in that buffer where we can find the app result.
                    // fn test_get_result_and_memory(buffer_alloc_addr: *mut u8) -> usize;
                    assert_eq!(arguments.len(), 1);
                    let buffer_alloc_addr = arguments[0].expect_i32().unwrap() as usize;
                    match &self.app {
                        Some((_, instance)) => {
                            let len = instance.memory.len();
                            compiler_memory[buffer_alloc_addr..][..len]
                                .copy_from_slice(&instance.memory);
                            self.result_addr.map(Value::I32)
                        }
                        None => panic!("Trying to get result and memory but there is no app"),
                    }
                }
                "test_copy_input_string" => {
                    // Copy the Roc source code from the test into the compiler Wasm instance
                    // fn test_copy_input_string(src_buffer_addr: *mut u8);
                    assert_eq!(arguments.len(), 1);
                    let src_buffer_addr = arguments[0].expect_i32().unwrap() as usize;
                    let len = self.src.len();
                    compiler_memory[src_buffer_addr..][..len].copy_from_slice(self.src.as_bytes());
                    None
                }
                "test_copy_output_string" => {
                    // The REPL now has a string representing the answer. Make it available to the test code.
                    // fn test_copy_output_string(output_ptr: *const u8, output_len: usize);
                    assert_eq!(arguments.len(), 2);
                    let output_ptr = arguments[0].expect_i32().unwrap() as usize;
                    let output_len = arguments[1].expect_i32().unwrap() as usize;
                    match std::str::from_utf8(&compiler_memory[output_ptr..][..output_len]) {
                        Ok(answer) => {
                            self.answer = answer.into();
                        }
                        Err(e) => panic!("{:?}", e),
                    }
                    None
                }
                "now" => Some(Value::F64(0.0)),
                _ => unknown(),
            }
        } else {
            unknown()
        }
    }
}

fn run(src: &'static str) -> Result<String, String> {
    let arena = Bump::new();

    let require_reloc = false;
    let module = WasmModule::preload(&arena, COMPILER_BYTES, require_reloc).unwrap();

    let mut instance = {
        let dispatcher = CompilerDispatcher {
            arena: &arena,
            src,
            answer: String::new(),
            wasi: WasiDispatcher::default(),
            app: None,
            result_addr: None,
        };

        let is_debug_mode = false; // logs every instruction!
        Instance::for_module(&arena, &module, dispatcher, is_debug_mode).unwrap()
    };

    let len = Value::I32(src.len() as i32);
    let wasm_ok: i32 = instance
        .call_export(&module, "entrypoint_from_test", [len])
        .unwrap()
        .unwrap()
        .expect_i32()
        .unwrap();
    let answer_str = instance.import_dispatcher.answer.to_owned();

    if wasm_ok == 0 {
        Err(answer_str)
    } else {
        Ok(answer_str)
    }
}

#[allow(dead_code)]
pub fn expect_success(input: &'static str, expected: &str) {
    assert_eq!(run(input), Ok(expected.into()));
}

#[allow(dead_code)]
pub fn expect_failure(input: &'static str, expected: &str) {
    assert_eq!(run(input), Err(expected.into()));
}
