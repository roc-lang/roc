use roc_wasm_module::{parse::Parse, Value, ValueType};
use std::iter::repeat;

use crate::value_stack::ValueStack;

#[derive(Debug)]
pub struct Frame {
    /// The function this frame belongs to
    pub fn_index: usize,
    /// Address in the code section where this frame returns to
    pub return_addr: usize,
    /// Depth of the "function body block" for this frame
    pub body_block_index: usize,
    /// Offset in the ValueStack where the args & locals begin
    pub locals_start: usize,
    /// Number of args & locals in the frame
    pub locals_count: usize,
    /// Expected return type, if any
    pub return_type: Option<ValueType>,
}

impl Frame {
    pub fn new() -> Self {
        Frame {
            fn_index: 0,
            return_addr: 0,
            body_block_index: 0,
            locals_start: 0,
            locals_count: 0,
            return_type: None,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn enter(
        fn_index: usize,
        return_addr: usize,
        body_block_index: usize,
        n_args: usize,
        return_type: Option<ValueType>,
        code_bytes: &[u8],
        value_stack: &mut ValueStack<'_>,
        pc: &mut usize,
    ) -> Self {
        let locals_start = value_stack.depth() - n_args;

        // Parse local variable declarations in the function header. They're grouped by type.
        let local_group_count = u32::parse((), code_bytes, pc).unwrap();
        for _ in 0..local_group_count {
            let (group_size, ty) = <(u32, ValueType)>::parse((), code_bytes, pc).unwrap();
            let n = group_size as usize;
            let zero = match ty {
                ValueType::I32 => Value::I32(0),
                ValueType::I64 => Value::I64(0),
                ValueType::F32 => Value::F32(0.0),
                ValueType::F64 => Value::F64(0.0),
            };
            value_stack.extend(repeat(zero).take(n));
        }

        let locals_count = value_stack.depth() - locals_start;

        Frame {
            fn_index,
            return_addr,
            body_block_index,
            locals_start,
            locals_count,
            return_type,
        }
    }

    pub fn get_local(&self, values: &ValueStack<'_>, index: u32) -> Value {
        debug_assert!((index as usize) < self.locals_count);
        *values.get(self.locals_start + index as usize).unwrap()
    }

    pub fn set_local(&self, values: &mut ValueStack<'_>, index: u32, value: Value) {
        debug_assert!((index as usize) < self.locals_count);
        values.set(self.locals_start + index as usize, value)
    }
}
