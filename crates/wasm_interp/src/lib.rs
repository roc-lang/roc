mod call_stack;
mod execute;
mod value_stack;

// Exposed for testing only. Should eventually become private.
pub use call_stack::CallStack;
pub use value_stack::ValueStack;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}
