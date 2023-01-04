//! Provides a build of the REPL for the Roc website using WebAssembly.
mod repl;

//
// Interface with external JS in the browser
//
#[cfg(feature = "console_error_panic_hook")]
extern crate console_error_panic_hook;
#[cfg(not(feature = "wasi_test"))]
mod externs_js;
#[cfg(not(feature = "wasi_test"))]
pub use externs_js::{entrypoint_from_js, js_create_app, js_get_result_and_memory, js_run_app};

//
// Interface with test code outside the Wasm module
//
#[cfg(feature = "wasi_test")]
mod externs_test;
#[cfg(feature = "wasi_test")]
pub use externs_test::{entrypoint_from_test, js_create_app, js_get_result_and_memory, js_run_app};
