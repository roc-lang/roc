extern "C" {
    fn wasmer_create_app(wasm_module_bytes: &[u8]);
    fn wasmer_run_app() -> usize;
    fn wasmer_get_result_and_memory(buffer_alloc_addr: *mut u8) -> usize;
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

/// Synchronous entrypoint for Wasmer tests
/// In tests we avoid the complexity of async calls across the Wasm/native boundary
pub fn entrypoint_from_test(src: String) -> Result<String, String> {
    let result_async = crate::repl::entrypoint_from_js(src);
    executor::block_on(result_async)
}
