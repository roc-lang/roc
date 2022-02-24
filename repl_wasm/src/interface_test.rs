use futures::executor;

extern "C" {
    fn wasmer_create_app(app_bytes_ptr: *const u8, app_bytes_len: usize);
    fn wasmer_run_app() -> usize;
    fn wasmer_get_result_and_memory(buffer_alloc_addr: *mut u8) -> usize;
    fn wasmer_copy_input_string(src_buffer_addr: *mut u8);
    fn wasmer_copy_output_string(output_ptr: *const u8, output_len: usize);
}

/// Async wrapper to match the equivalent JS function
pub async fn js_create_app(wasm_module_bytes: &[u8]) -> Result<(), String> {
    unsafe {
        wasmer_create_app(wasm_module_bytes.as_ptr(), wasm_module_bytes.len());
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
/// - Synchronous API, to avoid the need to run an async executor across the Wasm/native boundary.
///   (wasmer has a sync API for creating an Instance, whereas browsers don't)
/// - Uses an extra callback to allocate & copy the input string (wasm_bindgen does this for JS)
#[no_mangle]
pub extern "C" fn entrypoint_from_test(src_len: usize) -> bool {
    let mut src_buffer = std::vec![0; src_len];
    let src = unsafe {
        wasmer_copy_input_string(src_buffer.as_mut_ptr());
        String::from_utf8_unchecked(src_buffer)
    };
    let result_async = crate::repl::entrypoint_from_js(src);
    let result = executor::block_on(result_async);
    let ok = result.is_ok();

    let output = result.unwrap_or_else(|s| s);

    unsafe { wasmer_copy_output_string(output.as_ptr(), output.len()) }

    ok
}
