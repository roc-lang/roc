mod call_stack;
mod instance;
pub mod test_utils;
mod value_stack;
mod wasi;

// Exposed for testing only. Should eventually become private.
pub use call_stack::CallStack;
pub use instance::{Action, Instance};
pub use value_stack::ValueStack;
