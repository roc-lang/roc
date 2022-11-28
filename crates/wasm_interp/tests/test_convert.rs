#![cfg(test)]

use roc_wasm_interp::test_utils::test_op_example;
use roc_wasm_module::{opcodes::OpCode::*, Value};

// #[test]
// fn test_i32wrapi64() {}

// #[test]
// fn test_i32truncsf32() {}

// #[test]
// fn test_i32truncuf32() {}

// #[test]
// fn test_i32truncsf64() {}

// #[test]
// fn test_i32truncuf64() {}

// #[test]
// fn test_i64extendsi32() {}

// #[test]
// fn test_i64extendui32() {}

// #[test]
// fn test_i64truncsf32() {}

// #[test]
// fn test_i64truncuf32() {}

// #[test]
// fn test_i64truncsf64() {}

// #[test]
// fn test_i64truncuf64() {}

// #[test]
// fn test_f32convertsi32() {}

// #[test]
// fn test_f32convertui32() {}

// #[test]
// fn test_f32convertsi64() {}

// #[test]
// fn test_f32convertui64() {}

// #[test]
// fn test_f32demotef64() {}

// #[test]
// fn test_f64convertsi32() {}

// #[test]
// fn test_f64convertui32() {}

// #[test]
// fn test_f64convertsi64() {}

// #[test]
// fn test_f64convertui64() {}

// #[test]
// fn test_f64promotef32() {}

#[test]
fn test_i32reinterpretf32() {
    test_op_example(
        I32REINTERPRETF32,
        [Value::F32(12.375)],
        Value::I32(0x4146_0000),
    );
}

#[test]
fn test_i64reinterpretf64() {
    test_op_example(
        I64REINTERPRETF64,
        [Value::F64(0.01171875)],
        Value::from(0x3F88_0000_0000_0000u64),
    );
}

#[test]
fn test_f32reinterpreti32() {
    test_op_example(
        F32REINTERPRETI32,
        [Value::I32(0x4146_0000)],
        Value::F32(12.375),
    );
}

#[test]
fn test_f64reinterpreti64() {
    test_op_example(
        F64REINTERPRETI64,
        [Value::from(0x3F88_0000_0000_0000u64)],
        Value::F64(0.01171875),
    );
}
