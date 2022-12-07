use bumpalo::{collections::Vec, Bump};
use roc_wasm_module::{Value, ValueType};
use std::fmt::Debug;

use crate::Error;

/// Memory-efficient Struct-of-Arrays storage for the value stack.
/// Pack the values and their types as densely as possible,
/// to get better cache usage, at the expense of some extra logic.
#[derive(Debug)]
pub struct ValueStack<'a> {
    value_count: usize,
    bytes: Vec<'a, u8>,
}

impl<'a> ValueStack<'a> {
    pub(crate) fn new(arena: &'a Bump) -> Self {
        ValueStack {
            value_count: 0,
            bytes: Vec::with_capacity_in(8 * 1024, arena),
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.value_count
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.value_count == 0
    }

    pub(crate) fn push(&mut self, value: Value) {
        self.value_count += 1;
        match value {
            Value::I32(x) => {
                self.bytes.extend_from_slice(&x.to_ne_bytes());
                // TODO:
                //  check if this byte matches the Rust discriminant for Value, and if it matters.
                //  make an enum out of it
                //  Try using ValueType, as it would be nice semantically
                self.bytes.push(0);
            }
            Value::I64(x) => {
                self.bytes.extend_from_slice(&x.to_ne_bytes());
                self.bytes.push(1);
            }
            Value::F32(x) => {
                self.bytes.extend_from_slice(&x.to_ne_bytes());
                self.bytes.push(2);
            }
            Value::F64(x) => {
                self.bytes.extend_from_slice(&x.to_ne_bytes());
                self.bytes.push(3);
            }
        }
    }

    pub(crate) fn pop(&mut self) -> Value {
        self.value_count -= 1;
        match self.bytes.pop().unwrap() {
            0 => {
                let mut b = [0; 4];
                let end = self.bytes.len();
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                self.bytes.truncate(start);
                Value::I32(i32::from_ne_bytes(b))
            }
            1 => {
                let mut b = [0; 8];
                let end = self.bytes.len();
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                self.bytes.truncate(start);
                Value::I64(i64::from_ne_bytes(b))
            }
            2 => {
                let mut b = [0; 4];
                let end = self.bytes.len();
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                self.bytes.truncate(start);
                Value::F32(f32::from_ne_bytes(b))
            }
            3 => {
                let mut b = [0; 8];
                let end = self.bytes.len();
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                self.bytes.truncate(start);
                Value::F64(f64::from_ne_bytes(b))
            }
            _ => unreachable!(),
        }
    }

    // TODO: share some goddamn code, jeez!

    pub(crate) fn peek(&self) -> Value {
        let len = self.bytes.len();
        match self.bytes[len - 1] {
            0 => {
                let mut b = [0; 4];
                let end = len - 1;
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                Value::I32(i32::from_ne_bytes(b))
            }
            1 => {
                let mut b = [0; 8];
                let end = len - 1;
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                Value::I64(i64::from_ne_bytes(b))
            }
            2 => {
                let mut b = [0; 4];
                let end = len - 1;
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                Value::F32(f32::from_ne_bytes(b))
            }
            3 => {
                let mut b = [0; 8];
                let end = len - 1;
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                Value::F64(f64::from_ne_bytes(b))
            }
            _ => unreachable!(),
        }
    }

    /// Memory addresses etc
    pub(crate) fn pop_u32(&mut self) -> Result<u32, Error> {
        self.value_count -= 1;
        let type_byte = self.bytes.pop().unwrap();
        match type_byte {
            0 => {
                let mut b = [0; 4];
                let end = self.bytes.len();
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                self.bytes.truncate(start);
                Ok(u32::from_ne_bytes(b))
            }
            _ => todo!(),
        }
    }

    pub(crate) fn pop_i32(&mut self) -> Result<i32, Error> {
        self.value_count -= 1;
        let type_byte = self.bytes.pop().unwrap();
        match type_byte {
            0 => {
                let mut b = [0; 4];
                let end = self.bytes.len();
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                self.bytes.truncate(start);
                Ok(i32::from_ne_bytes(b))
            }
            _ => todo!(),
        }
    }

    pub(crate) fn pop_u64(&mut self) -> Result<u64, Error> {
        self.value_count -= 1;
        let type_byte = self.bytes.pop().unwrap();
        match type_byte {
            1 => {
                let mut b = [0; 8];
                let end = self.bytes.len();
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                self.bytes.truncate(start);
                Ok(u64::from_ne_bytes(b))
            }
            _ => todo!(),
        }
    }

    pub(crate) fn pop_i64(&mut self) -> Result<i64, Error> {
        self.value_count -= 1;
        let type_byte = self.bytes.pop().unwrap();
        match type_byte {
            1 => {
                let mut b = [0; 8];
                let end = self.bytes.len();
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                self.bytes.truncate(start);
                Ok(i64::from_ne_bytes(b))
            }
            _ => todo!(),
        }
    }

    pub(crate) fn pop_f32(&mut self) -> Result<f32, Error> {
        self.value_count -= 1;
        let type_byte = self.bytes.pop().unwrap();
        match type_byte {
            2 => {
                let mut b = [0; 4];
                let end = self.bytes.len();
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                self.bytes.truncate(start);
                Ok(f32::from_ne_bytes(b))
            }
            _ => todo!(),
        }
    }

    pub(crate) fn pop_f64(&mut self) -> Result<f64, Error> {
        self.value_count -= 1;
        let type_byte = self.bytes.pop().unwrap();
        match type_byte {
            3 => {
                let mut b = [0; 8];
                let end = self.bytes.len();
                let start = end - b.len();
                b.copy_from_slice(&self.bytes[start..end]);
                self.bytes.truncate(start);
                Ok(f64::from_ne_bytes(b))
            }
            _ => todo!(),
        }
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
    #[ignore = "Debug fmt broken while doing perf experiments"]
    fn test_debug_fmt() {
        let arena = Bump::new();
        let mut stack = ValueStack::new(&arena);

        for val in VALUES {
            stack.push(val);
        }

        assert_eq!(format!("{:?}", VALUES), format!("{:?}", stack));
    }
}
