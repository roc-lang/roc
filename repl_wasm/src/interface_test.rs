extern "C" {
    fn wasmer_create_app(wasm_module_bytes: &[u8]);
    fn wasmer_run_app() -> usize;
    fn wasmer_get_result_and_memory(buffer_alloc_addr: *mut u8) -> usize;
    fn wasmer_copy_input_string(src_buffer_addr: *mut u8);
}

/// Async wrapper to match the equivalent JS function
pub async fn js_create_app(wasm_module_bytes: &[u8]) -> Result<(), String> {
    unsafe {
        wasmer_create_app(wasm_module_bytes);
    }
    Ok(())
}

pub fn js_run_app() -> usize {
    unsafe { wasmer_run_app() }
}

pub fn js_get_result_and_memory(buffer_alloc_addr: *mut u8) -> usize {
    unsafe { wasmer_get_result_and_memory(buffer_alloc_addr) }
}

/// Entrypoint for Wasmer tests
/// - Synchronous API, to avoid the need to manage async calls across the Wasm/native boundary.
///   In the JS version, wasm_bindgen deals with this, as well as translating Rust Future <-> JS Promise.
/// - Manually copy the source bytes and convert to String. Again, wasm_bindgen does this for JS.
pub fn entrypoint_from_test(src_len: usize) -> Result<String, String> {
    let mut src_buffer = std::vec![0; src_len];
    let src = unsafe {
        wasmer_copy_input_string(src_buffer.as_mut_ptr());
        String::from_utf8_unchecked(src_buffer)
    };
    let result_async = crate::repl::entrypoint_from_js(src);
    executor::block_on(result_async)
}
