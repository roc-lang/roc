mod repl;

//
// Interface with external JS in the browser
//
#[cfg(not(test))]
mod interface_js;
#[cfg(not(test))]
pub use interface_js::{entrypoint_from_js, js_create_app, js_get_result_and_memory, js_run_app};

//
// Interface with test code outside the Wasm module
//
#[cfg(test)]
mod interface_test;
#[cfg(test)]
pub use interface_test::{
    entrypoint_from_test, js_create_app, js_get_result_and_memory, js_run_app,
};
