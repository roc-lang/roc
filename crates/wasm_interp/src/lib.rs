mod call_stack;
mod instance;
mod tests;
mod value_stack;
pub mod wasi;

// Main external interface
pub use instance::Instance;
pub use wasi::WasiDispatcher;

use rand::prelude::*;
use roc_wasm_module::{Value, ValueType, WasmModule};
use value_stack::ValueStack;

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

impl Default for DefaultImportDispatcher<'_> {
    fn default() -> Self {
        DefaultImportDispatcher {
            wasi: WasiDispatcher {
                args: &[],
                rng: thread_rng(),
            },
        }
    }
}

pub struct DefaultImportDispatcher<'a> {
    wasi: WasiDispatcher<'a>,
}

impl<'a> DefaultImportDispatcher<'a> {
    pub fn new(args: &'a [&'a String]) -> Self {
        DefaultImportDispatcher {
            wasi: WasiDispatcher {
                args,
                rng: thread_rng(),
            },
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

/// Errors that can happen while interpreting the program
/// All of these cause a WebAssembly stack trace to be dumped
#[derive(Debug, PartialEq)]
pub(crate) enum Error {
    ValueStackType(ValueType, ValueType),
    ValueStackEmpty,
    UnreachableOp,
}

impl Error {
    pub fn to_string_at(&self, file_offset: usize) -> String {
        match self {
            Error::ValueStackType(expected, actual) => {
                format!(
                    "ERROR: I found a type mismatch in the Value Stack at file offset {:#x}. Expected {:?}, but found {:?}.\n", 
                    file_offset, expected, actual
                )
            }
            Error::ValueStackEmpty => {
                format!(
                    "ERROR: I tried to pop a value from the Value Stack at file offset {:#x}, but it was empty.\n",
                    file_offset
                )
            }
            Error::UnreachableOp => {
                format!(
                    "WebAssembly `unreachable` instruction at file offset {:#x}.\n",
                    file_offset
                )
            }
        }
    }
}

impl From<(ValueType, ValueType)> for Error {
    fn from((expected, actual): (ValueType, ValueType)) -> Self {
        Error::ValueStackType(expected, actual)
    }
}

// Determine which function the program counter is in
pub(crate) fn pc_to_fn_index(program_counter: usize, module: &WasmModule<'_>) -> usize {
    if module.code.function_offsets.is_empty() {
        0
    } else {
        // Find the first function that starts *after* the given program counter
        let next_internal_fn_index = module
            .code
            .function_offsets
            .iter()
            .position(|o| *o as usize > program_counter)
            .unwrap_or(module.code.function_offsets.len());
        // Go back 1
        let internal_fn_index = next_internal_fn_index - 1;
        // Adjust for imports, whose indices come before the code section
        module.import.imports.len() + internal_fn_index
    }
}
