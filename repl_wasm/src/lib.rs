mod repl;

//
// Interface with external JS in the browser
//
#[cfg(not(feature = "wasmer"))]
mod interface_js;
#[cfg(not(feature = "wasmer"))]
pub use interface_js::{entrypoint_from_js, js_create_app, js_get_result_and_memory, js_run_app};

//
// Interface with test code outside the Wasm module
//
#[cfg(feature = "wasmer")]
mod interface_test;
#[cfg(feature = "wasmer")]
pub use interface_test::{
    entrypoint_from_test, js_create_app, js_get_result_and_memory, js_run_app,
};
