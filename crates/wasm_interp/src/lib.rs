mod call_stack;
mod instance;
pub mod test_utils;
mod value_stack;
pub mod wasi;

// Main external interface
pub use instance::Instance;

// Exposed for testing only. Should eventually become private.
pub use call_stack::CallStack;
pub use instance::Action;
pub use value_stack::ValueStack;

use roc_wasm_module::Value;

pub trait ImportDispatcher {
    /// Dispatch a call from WebAssembly to your own code, based on module and function name.
    fn dispatch(
        &mut self,
        module_name: &str,
        function_name: &str,
        arguments: &[Value],
        memory: &mut [u8],
    ) -> Option<Value>;
}

pub const DEFAULT_IMPORTS: DefaultImportDispatcher = DefaultImportDispatcher {};

pub struct DefaultImportDispatcher {}

impl ImportDispatcher for DefaultImportDispatcher {
    fn dispatch(
        &mut self,
        module_name: &str,
        function_name: &str,
        arguments: &[Value],
        memory: &mut [u8],
    ) -> Option<Value> {
        if module_name == wasi::MODULE_NAME {
            wasi::dispatch(function_name, arguments, memory)
        } else {
            panic!(
                "DefaultImportDispatcher does not implement {}.{}",
                module_name, function_name
            );
        }
    }
}
