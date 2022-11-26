use bitvec::vec::BitVec;
use bumpalo::{collections::Vec, Bump};
use roc_wasm_module::{parse::Parse, Value, ValueType};
use std::iter::repeat;

use crate::ValueStack;

/// Struct-of-Arrays storage for the call stack.
/// Type info is packed to avoid wasting space on padding.
/// However we store 64 bits for every local, even 32-bit values, for easy random access.
#[derive(Debug)]
pub struct CallStack<'a> {
    /// return addresses and nested block depths (one entry per frame)
    return_addrs_and_block_depths: Vec<'a, (u32, u32)>,
    /// frame offsets into the `locals`, `is_float`, and `is_64` vectors (one entry per frame)
    frame_offsets: Vec<'a, u32>,
    /// base size of the value stack before executing (one entry per frame)
    value_stack_bases: Vec<'a, u32>,
    /// binary data for local variables (one entry per local)
    locals_data: Vec<'a, u64>,
    /// int/float type info (one entry per local)
    is_float: BitVec,
    /// bitwidth type info (one entry per local)
    is_64: BitVec,
}

/*
TODO, maybe?
Store data as `Vec<u8>` and a current frame offset.
To find a local by index, take a slice of `is_64` starting at current frame offset,
and use count_ones to know how many of the locals in-between are 64-bit vs 32.
Big size reduction, since most locals are i32. And we're loading that word from is_64 anyway.
When pushing/popping frames, move the current frame offset using a similar calculation.
Not clear if this would be better! Stack access pattern is pretty cache-friendly anyway.
*/

impl<'a> CallStack<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        CallStack {
            return_addrs_and_block_depths: Vec::with_capacity_in(256, arena),
            frame_offsets: Vec::with_capacity_in(256, arena),
            value_stack_bases: Vec::with_capacity_in(256, arena),
            locals_data: Vec::with_capacity_in(16 * 256, arena),
            is_float: BitVec::with_capacity(256),
            is_64: BitVec::with_capacity(256),
        }
    }

    /// On entering a Wasm call, save the return address, and make space for locals
    pub fn push_frame(
        &mut self,
        return_addr: u32,
        return_block_depth: u32,
        arg_type_bytes: &[u8],
        value_stack: &mut ValueStack<'a>,
        code_bytes: &[u8],
        pc: &mut usize,
    ) {
        self.return_addrs_and_block_depths
            .push((return_addr, return_block_depth));
        let frame_offset = self.is_64.len();
        self.frame_offsets.push(frame_offset as u32);
        let mut total = 0;

        // Make space for arguments
        let n_args = arg_type_bytes.len();
        self.is_64.extend(repeat(false).take(n_args));
        self.is_float.extend(repeat(false).take(n_args));
        self.locals_data.extend(repeat(0).take(n_args));

        // Pop arguments off the value stack and into locals
        for (i, type_byte) in arg_type_bytes.iter().copied().enumerate().rev() {
            let arg = value_stack.pop();
            assert_eq!(ValueType::from(arg), ValueType::from(type_byte));
            self.set_local_help(i as u32, arg);
        }

        self.value_stack_bases.push(value_stack.len() as u32);

        // Parse local variable declarations in the function header. They're grouped by type.
        let local_group_count = u32::parse((), code_bytes, pc).unwrap();
        for _ in 0..local_group_count {
            let (group_size, ty) = <(u32, ValueType)>::parse((), code_bytes, pc).unwrap();
            let n = group_size as usize;
            total += n;
            self.is_64
                .extend(repeat(matches!(ty, ValueType::I64 | ValueType::F64)).take(n));
            self.is_float
                .extend(repeat(matches!(ty, ValueType::F32 | ValueType::F64)).take(n));
        }
        self.locals_data.extend(repeat(0).take(total));
    }

    /// On returning from a Wasm call, drop its locals and retrieve the return address
    pub fn pop_frame(&mut self) -> Option<(u32, u32)> {
        let frame_offset = self.frame_offsets.pop()? as usize;
        self.value_stack_bases.pop()?;
        self.locals_data.truncate(frame_offset);
        self.is_64.truncate(frame_offset);
        self.is_64.truncate(frame_offset);
        self.return_addrs_and_block_depths.pop()
    }

    pub fn get_local(&self, local_index: u32) -> Value {
        let frame_offset = self.frame_offsets.last().unwrap();
        let index = (*frame_offset + local_index) as usize;
        let data64 = self.locals_data[index];
        let is_float = self.is_float[index];
        let is_64 = self.is_64[index];
        if is_64 {
            if is_float {
                Value::F64(f64::from_ne_bytes(data64.to_ne_bytes()))
            } else {
                Value::I64(i64::from_ne_bytes(data64.to_ne_bytes()))
            }
        } else {
            let data32 = data64 as u32;
            if is_float {
                Value::F32(f32::from_ne_bytes(data32.to_ne_bytes()))
            } else {
                Value::I32(i32::from_ne_bytes(data32.to_ne_bytes()))
            }
        }
    }

    pub fn set_local(&mut self, local_index: u32, value: Value) {
        let type_check_ok = self.set_local_help(local_index, value);
        debug_assert!(type_check_ok);
    }

    fn set_local_help(&mut self, local_index: u32, value: Value) -> bool {
        let frame_offset = *self.frame_offsets.last().unwrap();
        let index = (frame_offset + local_index) as usize;
        match value {
            Value::I32(x) => {
                self.locals_data[index] = u64::from_ne_bytes((x as i64).to_ne_bytes());
                !self.is_64[index] && !self.is_float[index]
            }
            Value::I64(x) => {
                self.locals_data[index] = u64::from_ne_bytes((x).to_ne_bytes());
                !self.is_float[index] && self.is_64[index]
            }
            Value::F32(x) => {
                self.locals_data[index] = x.to_bits() as u64;
                self.is_float[index] && !self.is_64[index]
            }
            Value::F64(x) => {
                self.locals_data[index] = x.to_bits();
                self.is_float[index] && self.is_64[index]
            }
        }
    }

    pub fn value_stack_base(&self) -> u32 {
        *self.value_stack_bases.last().unwrap_or(&0)
    }

    pub fn is_empty(&self) -> bool {
        self.is_64.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use roc_wasm_module::Serialize;

    use super::*;

    const RETURN_ADDR: u32 = 0x12345;

    fn test_get_set(call_stack: &mut CallStack<'_>, index: u32, value: Value) {
        call_stack.set_local(index, value);
        assert_eq!(call_stack.get_local(index), value);
    }

    fn setup<'a>(arena: &'a Bump, call_stack: &mut CallStack<'a>) {
        let mut buffer = vec![];
        let mut cursor = 0;
        let mut vs = ValueStack::new(arena);

        // Push a other few frames before the test frame, just to make the scenario more typical.
        [(1u32, ValueType::I32)].serialize(&mut buffer);
        call_stack.push_frame(0x11111, 0, &[], &mut vs, &buffer, &mut cursor);

        [(2u32, ValueType::I32)].serialize(&mut buffer);
        call_stack.push_frame(0x22222, 0, &[], &mut vs, &buffer, &mut cursor);

        [(3u32, ValueType::I32)].serialize(&mut buffer);
        call_stack.push_frame(0x33333, 0, &[], &mut vs, &buffer, &mut cursor);

        // Create a test call frame with local variables of every type
        [
            (8u32, ValueType::I32),
            (4u32, ValueType::I64),
            (2u32, ValueType::F32),
            (1u32, ValueType::F64),
        ]
        .serialize(&mut buffer);
        call_stack.push_frame(RETURN_ADDR, 0, &[], &mut vs, &buffer, &mut cursor);
    }

    #[test]
    fn test_all() {
        let arena = Bump::new();
        let mut call_stack = CallStack::new(&arena);

        setup(&arena, &mut call_stack);

        test_get_set(&mut call_stack, 0, Value::I32(123));
        test_get_set(&mut call_stack, 8, Value::I64(123456));
        test_get_set(&mut call_stack, 12, Value::F32(1.01));
        test_get_set(&mut call_stack, 14, Value::F64(-1.1));

        test_get_set(&mut call_stack, 0, Value::I32(i32::MIN));
        test_get_set(&mut call_stack, 0, Value::I32(i32::MAX));

        test_get_set(&mut call_stack, 8, Value::I64(i64::MIN));
        test_get_set(&mut call_stack, 8, Value::I64(i64::MAX));

        test_get_set(&mut call_stack, 12, Value::F32(f32::MIN));
        test_get_set(&mut call_stack, 12, Value::F32(f32::MAX));

        test_get_set(&mut call_stack, 14, Value::F64(f64::MIN));
        test_get_set(&mut call_stack, 14, Value::F64(f64::MAX));

        assert_eq!(call_stack.pop_frame(), Some((RETURN_ADDR, 0)));
    }

    #[test]
    #[should_panic]
    fn test_type_error_i32() {
        let arena = Bump::new();
        let mut call_stack = CallStack::new(&arena);
        setup(&arena, &mut call_stack);
        test_get_set(&mut call_stack, 0, Value::F32(1.01));
    }

    #[test]
    #[should_panic]
    fn test_type_error_i64() {
        let arena = Bump::new();
        let mut call_stack = CallStack::new(&arena);
        setup(&arena, &mut call_stack);
        test_get_set(&mut call_stack, 8, Value::F32(1.01));
    }

    #[test]
    #[should_panic]
    fn test_type_error_f32() {
        let arena = Bump::new();
        let mut call_stack = CallStack::new(&arena);
        setup(&arena, &mut call_stack);
        test_get_set(&mut call_stack, 12, Value::I32(123));
    }

    #[test]
    #[should_panic]
    fn test_type_error_f64() {
        let arena = Bump::new();
        let mut call_stack = CallStack::new(&arena);
        setup(&arena, &mut call_stack);
        test_get_set(&mut call_stack, 14, Value::I32(123));
    }
}
