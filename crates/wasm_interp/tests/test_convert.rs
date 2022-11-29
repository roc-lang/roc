#![cfg(test)]

use roc_wasm_interp::test_utils::test_op_example;
use roc_wasm_module::{opcodes::OpCode::*, Value};

#[test]
fn test_i32wrapi64() {
    test_op_example(
        I32WRAPI64,
        [Value::I64(0x123456789)],
        Value::I32(0x23456789),
    );
}

// #[test]
// fn test_i32truncsf32() {}

// #[test]
// fn test_i32truncuf32() {}

// #[test]
// fn test_i32truncsf64() {}

// #[test]
// fn test_i32truncuf64() {}

#[test]
fn test_i64extendsi32() {
    test_op_example(I64EXTENDSI32, [Value::I32(-1)], Value::I64(-1));
}

#[test]
fn test_i64extendui32() {
    test_op_example(I64EXTENDUI32, [Value::I32(-1)], Value::I64(0xffff_ffff));
}

// #[test]
// fn test_i64truncsf32() {}

// #[test]
// fn test_i64truncuf32() {}

// #[test]
// fn test_i64truncsf64() {}

// #[test]
// fn test_i64truncuf64() {}

#[test]
fn test_f32demotef64() {
    test_op_example(F32DEMOTEF64, [Value::F64(12.375)], Value::F32(12.375));
}

#[test]
fn test_f64convertsi32() {
    test_op_example(F64CONVERTSI32, [Value::I32(-1)], Value::F64(-1.0));
}

#[test]
fn test_f64convertui32() {
    test_op_example(F64CONVERTUI32, [Value::I32(-1)], Value::F64(4294967295.0));
}

#[test]
fn test_f64convertsi64() {
    test_op_example(F64CONVERTSI64, [Value::I64(-1)], Value::F64(-1.0));
}

#[test]
fn test_f64convertui64() {
    test_op_example(
        F64CONVERTUI64,
        [Value::I64(-1)],
        Value::F64(1.8446744073709552e19),
    );
}

#[test]
fn test_f64promotef32() {
    test_op_example(F64PROMOTEF32, [Value::F32(12.375)], Value::F64(12.375));
}

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
