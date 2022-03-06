// wasm_bindgen procedural macro breaks this clippy rule
// https://github.com/rustwasm/wasm-bindgen/issues/2774
#![allow(clippy::unused_unit)]

use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(catch)]
    pub async fn js_create_app(wasm_module_bytes: &[u8]) -> Result<(), JsValue>;

    pub fn js_run_app() -> usize;

    pub fn js_get_result_and_memory(buffer_alloc_addr: *mut u8) -> usize;
}

/// Async entrypoint for the browser
/// The browser only has an async API to generate a Wasm module from bytes
/// wasm_bindgen manages the interaction between Rust Futures and JS Promises
#[wasm_bindgen]
pub async fn entrypoint_from_js(src: String) -> Result<String, String> {
    crate::repl::entrypoint_from_js(src).await
}
