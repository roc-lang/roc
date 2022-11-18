use bitvec::vec::BitVec;
use bumpalo::collections::Vec;
use roc_wasm_module::ValueType;
use std::iter::repeat;

use crate::Value;

/// Struct-of-Arrays storage for the call stack.
/// Type info is packed to avoid wasting space on padding.
/// However we store 64 bits for every local, even 32-bit values, for easy random access.
pub struct CallStack<'a> {
    /// return addresses (one entry per frame)
    return_addrs: Vec<'a, u32>,
    /// frame offsets into the `locals`, `is_float`, and `is_64` vectors (one entry per frame)
    frame_offsets: Vec<'a, u32>,
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
    /// On entering a Wasm call, save the return address, and make space for locals
    pub fn push_frame(&mut self, return_addr: u32, local_groups: &[(u32, ValueType)]) {
        self.return_addrs.push(return_addr);
        let frame_offset = self.is_64.len();
        self.frame_offsets.push(frame_offset as u32);
        let mut total = 0;
        for (num_locals, ty) in local_groups {
            let n = *num_locals as usize;
            total += n;
            self.is_64
                .extend(repeat(matches!(ty, ValueType::I64 | ValueType::F64)).take(n));
            self.is_float
                .extend(repeat(matches!(ty, ValueType::F32 | ValueType::F64)).take(n));
        }
        self.locals_data.extend(repeat(0).take(total));
    }

    /// On returning from a Wasm call, drop its locals and retrieve the return address
    pub fn pop_frame(&mut self) -> u32 {
        let frame_offset = self.frame_offsets.pop().unwrap() as usize;
        self.locals_data.truncate(frame_offset);
        self.is_64.truncate(frame_offset);
        self.is_64.truncate(frame_offset);
        self.return_addrs.pop().unwrap()
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
        let frame_offset = *self.frame_offsets.last().unwrap();
        let index = (frame_offset + local_index) as usize;
        match value {
            Value::I32(x) => {
                self.locals_data[index] = unsafe { std::mem::transmute(x as i64) };
                self.is_float.set(index, false);
                self.is_64.set(index, false);
            }
            Value::I64(x) => {
                self.locals_data[index] = unsafe { std::mem::transmute(x) };
                self.is_float.set(index, false);
                self.is_64.set(index, true);
            }
            Value::F32(x) => {
                self.locals_data[index] = x.to_bits() as u64;
                self.is_float.set(index, true);
                self.is_64.set(index, false);
            }
            Value::F64(x) => {
                self.locals_data[index] = x.to_bits();
                self.is_float.set(index, true);
                self.is_64.set(index, true);
            }
        }
    }
}
