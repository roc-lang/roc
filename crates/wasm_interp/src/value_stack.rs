use bitvec::vec::BitVec;
use bumpalo::{collections::Vec, Bump};
use roc_wasm_module::{Value, ValueType};
use std::{fmt::Debug, mem::size_of};

use crate::Error;

/// Memory-efficient Struct-of-Arrays storage for the value stack.
/// Pack the values and their types as densely as possible,
/// to get better cache usage, at the expense of some extra logic.
pub struct ValueStack<'a> {
    bytes: Vec<'a, u8>,
    is_float: BitVec,
    is_64: BitVec,
}

macro_rules! pop_bytes {
    ($ty: ty, $bytes: expr) => {{
        const SIZE: usize = size_of::<$ty>();
        if $bytes.len() < SIZE {
            Err(Error::ValueStackEmpty)
        } else {
            let bytes_idx = $bytes.len() - SIZE;
            let mut b = [0; SIZE];
            b.copy_from_slice(&$bytes[bytes_idx..][..SIZE]);
            $bytes.truncate(bytes_idx);
            Ok(<$ty>::from_ne_bytes(b))
        }
    }};
}

impl<'a> ValueStack<'a> {
    pub(crate) fn new(arena: &'a Bump) -> Self {
        ValueStack {
            bytes: Vec::with_capacity_in(1024, arena),
            is_float: BitVec::with_capacity(1024),
            is_64: BitVec::with_capacity(1024),
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.is_64.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.is_64.is_empty()
    }

    pub(crate) fn push(&mut self, value: Value) {
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

    pub(crate) fn pop(&mut self) -> Value {
        let is_64 = self.is_64.pop().unwrap();
        let is_float = self.is_float.pop().unwrap();
        let size = if is_64 { 8 } else { 4 };
        let bytes_idx = self.bytes.len() - size;
        let value = self.get(is_64, is_float, bytes_idx);
        self.bytes.truncate(bytes_idx);
        value
    }

    pub(crate) fn peek(&self) -> Value {
        let is_64 = *self.is_64.last().unwrap();
        let is_float = *self.is_float.last().unwrap();
        let size = if is_64 { 8 } else { 4 };
        let bytes_idx = self.bytes.len() - size;
        self.get(is_64, is_float, bytes_idx)
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

    /// Memory addresses etc
    pub(crate) fn pop_u32(&mut self) -> Result<u32, Error> {
        match (self.is_float.pop(), self.is_64.pop()) {
            (Some(false), Some(false)) => pop_bytes!(u32, self.bytes),
            (Some(is_float), Some(is_64)) => {
                Err(Error::value_stack_type(ValueType::I32, is_float, is_64))
            }
            _ => Err(Error::ValueStackEmpty),
        }
    }

    pub(crate) fn pop_i32(&mut self) -> Result<i32, Error> {
        match (self.is_float.pop(), self.is_64.pop()) {
            (Some(false), Some(false)) => pop_bytes!(i32, self.bytes),
            (Some(is_float), Some(is_64)) => {
                Err(Error::value_stack_type(ValueType::I32, is_float, is_64))
            }
            _ => Err(Error::ValueStackEmpty),
        }
    }

    pub(crate) fn pop_u64(&mut self) -> Result<u64, Error> {
        match (self.is_float.pop(), self.is_64.pop()) {
            (Some(false), Some(true)) => pop_bytes!(u64, self.bytes),
            (Some(is_float), Some(is_64)) => {
                Err(Error::value_stack_type(ValueType::I64, is_float, is_64))
            }
            _ => Err(Error::ValueStackEmpty),
        }
    }

    pub(crate) fn pop_i64(&mut self) -> Result<i64, Error> {
        match (self.is_float.pop(), self.is_64.pop()) {
            (Some(false), Some(true)) => pop_bytes!(i64, self.bytes),
            (Some(is_float), Some(is_64)) => {
                Err(Error::value_stack_type(ValueType::I64, is_float, is_64))
            }
            _ => Err(Error::ValueStackEmpty),
        }
    }

    pub(crate) fn pop_f32(&mut self) -> Result<f32, Error> {
        match (self.is_float.pop(), self.is_64.pop()) {
            (Some(true), Some(false)) => pop_bytes!(f32, self.bytes),
            (Some(is_float), Some(is_64)) => {
                Err(Error::value_stack_type(ValueType::F32, is_float, is_64))
            }
            _ => Err(Error::ValueStackEmpty),
        }
    }

    pub(crate) fn pop_f64(&mut self) -> Result<f64, Error> {
        match (self.is_float.pop(), self.is_64.pop()) {
            (Some(true), Some(true)) => pop_bytes!(f64, self.bytes),
            (Some(is_float), Some(is_64)) => {
                Err(Error::value_stack_type(ValueType::F64, is_float, is_64))
            }
            _ => Err(Error::ValueStackEmpty),
        }
    }

    fn fmt_from_index(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        from_index: usize,
    ) -> std::fmt::Result {
        write!(f, "[")?;
        let mut bytes_index = 0;
        assert_eq!(self.is_64.len(), self.is_float.len());
        if from_index < self.is_64.len() {
            let iter_64 = self.is_64.iter().by_vals();
            let iter_float = self.is_float.iter().by_vals();
            for (i, (is_64, is_float)) in iter_64.zip(iter_float).enumerate() {
                if i < from_index {
                    continue;
                }
                let value = self.get(is_64, is_float, bytes_index);
                bytes_index += if is_64 { 8 } else { 4 };
                value.fmt(f)?;
                if i < self.is_64.len() - 1 {
                    write!(f, ", ")?;
                }
            }
        }
        write!(f, "]")
    }

    pub(crate) fn get_slice<'b>(&'b self, index: usize) -> ValueStackSlice<'a, 'b> {
        ValueStackSlice { stack: self, index }
    }
}

impl Debug for ValueStack<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_from_index(f, 0)
    }
}

pub struct ValueStackSlice<'a, 'b> {
    stack: &'b ValueStack<'a>,
    index: usize,
}

impl Debug for ValueStackSlice<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.stack.fmt_from_index(f, self.index)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const VALUES: [Value; 4] = [
        Value::I32(123),
        Value::I64(123456),
        Value::F32(1.01),
        Value::F64(-1.1),
    ];

    #[test]
    fn test_push_pop() {
        let arena = Bump::new();
        let mut stack = ValueStack::new(&arena);

        for val in VALUES {
            stack.push(val);
        }

        for val in VALUES.iter().rev() {
            let popped = stack.pop();
            assert_eq!(popped, *val);
        }
    }

    #[test]
    fn test_debug_fmt() {
        let arena = Bump::new();
        let mut stack = ValueStack::new(&arena);

        for val in VALUES {
            stack.push(val);
        }

        assert_eq!(format!("{:?}", VALUES), format!("{:?}", stack));
    }
}
