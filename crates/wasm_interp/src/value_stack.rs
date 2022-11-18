use bitvec::vec::BitVec;
use bumpalo::collections::Vec;
use std::fmt::Debug;

use crate::Value;

/// Memory-efficient Struct-of-Arrays storage for the value stack.
/// Pack the values and their types as densely as possible,
/// to get better cache usage, at the expense of some extra logic.
struct ValueStack<'a> {
    bytes: Vec<'a, u8>,
    is_float: BitVec,
    is_64: BitVec,
}

impl<'a> ValueStack<'a> {
    pub fn push(&mut self, value: Value) {
        match value {
            Value::I32(x) => {
                self.bytes.extend_from_slice(&x.to_ne_bytes());
                self.is_float.push(false);
                self.is_64.push(false);
            }
            Value::I64(x) => {
                self.bytes.extend_from_slice(&x.to_ne_bytes());
                self.is_float.push(false);
                self.is_64.push(true);
            }
            Value::F32(x) => {
                self.bytes.extend_from_slice(&x.to_ne_bytes());
                self.is_float.push(true);
                self.is_64.push(false);
            }
            Value::F64(x) => {
                self.bytes.extend_from_slice(&x.to_ne_bytes());
                self.is_float.push(true);
                self.is_64.push(true);
            }
        }
    }

    pub fn pop(&mut self) -> Value {
        let is_64 = self.is_64.pop().unwrap();
        let is_float = self.is_float.pop().unwrap();
        let size = if is_64 { 8 } else { 4 };
        let bytes_idx = self.bytes.len() - size;
        let value = self.get(is_64, is_float, bytes_idx);
        self.bytes.truncate(size);
        value
    }

    fn get(&self, is_64: bool, is_float: bool, bytes_idx: usize) -> Value {
        if is_64 {
            let mut b = [0; 8];
            b.copy_from_slice(&self.bytes[bytes_idx..][..8]);
            if is_float {
                Value::F64(f64::from_ne_bytes(b))
            } else {
                Value::I64(i64::from_ne_bytes(b))
            }
        } else {
            let mut b = [0; 4];
            b.copy_from_slice(&self.bytes[bytes_idx..][..4]);
            if is_float {
                Value::F32(f32::from_ne_bytes(b))
            } else {
                Value::I32(i32::from_ne_bytes(b))
            }
        }
    }
}

impl Debug for ValueStack<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let mut index = 0;
        assert_eq!(self.is_64.len(), self.is_float.len());
        let iter_64 = self.is_64.iter().by_vals();
        let iter_float = self.is_float.iter().by_vals();
        for (is_64, is_float) in iter_64.zip(iter_float) {
            let value = self.get(is_64, is_float, index);
            index += if is_64 { 8 } else { 4 };
            value.fmt(f)?;
            write!(f, ", ")?;
        }
        write!(f, "]")
    }
}
