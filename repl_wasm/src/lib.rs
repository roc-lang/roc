mod repl;

//
// Interface with external JS in the browser
//
#[cfg(not(feature = "wasmer"))]
mod externs_js;
#[cfg(not(feature = "wasmer"))]
pub use externs_js::{entrypoint_from_js, js_create_app, js_get_result_and_memory, js_run_app};

//
// Interface with test code outside the Wasm module
//
#[cfg(feature = "wasmer")]
mod externs_test;
#[cfg(feature = "wasmer")]
pub use externs_test::{entrypoint_from_test, js_create_app, js_get_result_and_memory, js_run_app};
