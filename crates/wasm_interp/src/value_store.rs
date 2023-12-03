use bumpalo::{collections::Vec, Bump};
use roc_wasm_module::{Value, ValueType};
use std::fmt::Debug;

use crate::Error;

/// Combined storage for the Wasm stack machine and local variables.
///
/// All values are mixed together so that on function calls, "moving"
/// arguments from the stack machine to local variables is a no-op
/// (or rather, just a matter of recording block metadata in the Instance).
///
/// We use a simple Vec. When we tried more densely-packed SoA structures,
/// they were slower due to more logic, and harder to debug.
pub struct ValueStore<'a> {
    values: Vec<'a, Value>,
}

impl<'a> ValueStore<'a> {
    pub(crate) fn new(arena: &'a Bump) -> Self {
        ValueStore {
            values: Vec::with_capacity_in(1024, arena),
        }
    }

    pub(crate) fn depth(&self) -> usize {
        self.values.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub(crate) fn push(&mut self, value: Value) {
        self.values.push(value);
    }

    pub(crate) fn pop(&mut self) -> Value {
        self.values.pop().unwrap()
    }

    pub(crate) fn peek(&self) -> Value {
        *self.values.last().unwrap()
    }

    pub(crate) fn get(&self, index: usize) -> Option<&Value> {
        self.values.get(index)
    }

    pub(crate) fn set(&mut self, index: usize, value: Value) {
        self.values[index] = value;
    }

    pub(crate) fn extend<I: Iterator<Item = Value>>(&mut self, values: I) {
        self.values.extend(values)
    }

    /// Memory addresses etc
    pub(crate) fn pop_u32(&mut self) -> Result<u32, Error> {
        match self.values.pop() {
            Some(Value::I32(x)) => Ok(u32::from_ne_bytes(x.to_ne_bytes())),
            Some(bad) => Err(Error::Type(ValueType::I32, ValueType::from(bad))),
            None => Err(Error::StackEmpty),
        }
    }

    pub(crate) fn pop_i32(&mut self) -> Result<i32, Error> {
        match self.values.pop() {
            Some(Value::I32(x)) => Ok(x),
            Some(bad) => Err(Error::Type(ValueType::I32, ValueType::from(bad))),
            None => Err(Error::StackEmpty),
        }
    }

    pub(crate) fn pop_u64(&mut self) -> Result<u64, Error> {
        match self.values.pop() {
            Some(Value::I64(x)) => Ok(u64::from_ne_bytes(x.to_ne_bytes())),
            Some(bad) => Err(Error::Type(ValueType::I64, ValueType::from(bad))),
            None => Err(Error::StackEmpty),
        }
    }

    pub(crate) fn pop_i64(&mut self) -> Result<i64, Error> {
        match self.values.pop() {
            Some(Value::I64(x)) => Ok(x),
            Some(bad) => Err(Error::Type(ValueType::I64, ValueType::from(bad))),
            None => Err(Error::StackEmpty),
        }
    }

    pub(crate) fn pop_f32(&mut self) -> Result<f32, Error> {
        match self.values.pop() {
            Some(Value::F32(x)) => Ok(x),
            Some(bad) => Err(Error::Type(ValueType::F32, ValueType::from(bad))),
            None => Err(Error::StackEmpty),
        }
    }

    pub(crate) fn pop_f64(&mut self) -> Result<f64, Error> {
        match self.values.pop() {
            Some(Value::F64(x)) => Ok(x),
            Some(bad) => Err(Error::Type(ValueType::F64, ValueType::from(bad))),
            None => Err(Error::StackEmpty),
        }
    }

    pub(crate) fn iter(&self) -> std::slice::Iter<Value> {
        self.values.iter()
    }

    pub(crate) fn truncate(&mut self, depth: usize) {
        self.values.truncate(depth)
    }

    pub(crate) fn get_slice(&mut self, from: usize) -> &[Value] {
        &self.values[from..]
    }
}

impl Debug for ValueStore<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self.values)
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
        let mut stack = ValueStore::new(&arena);

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
        let mut stack = ValueStore::new(&arena);

        for val in VALUES {
            stack.push(val);
        }

        assert_eq!(format!("{VALUES:?}"), format!("{stack:?}"));
    }
}
