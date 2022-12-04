mod call_stack;
mod instance;
mod tests;
mod value_stack;
pub mod wasi;

// Main external interface
pub use instance::Instance;

use roc_wasm_module::{Value, ValueType};
use value_stack::ValueStack;
use wasi::WasiDispatcher;

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

pub const DEFAULT_IMPORTS: DefaultImportDispatcher = DefaultImportDispatcher {
    wasi: WasiDispatcher { args: &[] },
};

pub struct DefaultImportDispatcher<'a> {
    wasi: WasiDispatcher<'a>,
}

impl<'a> DefaultImportDispatcher<'a> {
    pub fn new(args: &'a [&'a String]) -> Self {
        DefaultImportDispatcher {
            wasi: WasiDispatcher { args },
        }
    }
}

impl<'a> ImportDispatcher for DefaultImportDispatcher<'a> {
    fn dispatch(
        &mut self,
        module_name: &str,
        function_name: &str,
        arguments: &[Value],
        memory: &mut [u8],
    ) -> Option<Value> {
        if module_name == wasi::MODULE_NAME {
            self.wasi.dispatch(function_name, arguments, memory)
        } else {
            panic!(
                "DefaultImportDispatcher does not implement {}.{}",
                module_name, function_name
            );
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Error {
    ValueStackType(ValueType, ValueType),
    ValueStackEmpty,
}

impl Error {
    fn value_stack_type(expected: ValueType, is_float: bool, is_64: bool) -> Self {
        let ty = match (is_float, is_64) {
            (false, false) => ValueType::I32,
            (false, true) => ValueType::I64,
            (true, false) => ValueType::F32,
            (true, true) => ValueType::F64,
        };
        Error::ValueStackType(expected, ty)
    }
}
