use futures::executor;

extern "C" {
    fn test_create_app(app_bytes_ptr: *const u8, app_bytes_len: usize) -> u32;
    fn test_run_app() -> usize;
    fn test_get_result_and_memory(buffer_alloc_addr: *mut u8) -> usize;
    fn test_copy_input_string(src_buffer_addr: *mut u8);
    fn test_copy_output_string(output_ptr: *const u8, output_len: usize);
}

/// Async wrapper to match the equivalent JS function
pub async fn js_create_app(wasm_module_bytes: &[u8]) -> Result<(), String> {
    let ok = unsafe { test_create_app(wasm_module_bytes.as_ptr(), wasm_module_bytes.len()) } != 0;
    if ok {
        Ok(())
    } else {
        Err("Compiler generated an invalid Wasm module".to_string())
    }
}

pub fn js_run_app() -> usize {
    unsafe { test_run_app() }
}

pub fn js_get_result_and_memory(buffer_alloc_addr: *mut u8) -> usize {
    unsafe { test_get_result_and_memory(buffer_alloc_addr) }
}

/// Entrypoint for tests using WASI and a CLI interpreter
/// - Synchronous API, to avoid the need to run an async executor across the Wasm/native boundary.
/// - Uses an extra callback to allocate & copy the input string (in the browser version, wasm_bindgen does this)
#[no_mangle]
pub extern "C" fn entrypoint_from_test(src_len: usize) {
    let mut src_buffer = std::vec![0; src_len];
    let src = unsafe {
        test_copy_input_string(src_buffer.as_mut_ptr());
        String::from_utf8_unchecked(src_buffer)
    };
    let result_async = crate::repl::entrypoint_from_js(src);
    let output = executor::block_on(result_async);

    unsafe { test_copy_output_string(output.as_ptr(), output.len()) }
}
