#![cfg(test)]

use roc_wasm_interp::test_utils::test_op_example;
use roc_wasm_module::{opcodes::OpCode, opcodes::OpCode::*, Value};

fn test_f32_comparison(op: OpCode, arg1: f32, arg2: f32, expected: bool) {
    test_op_example(
        op,
        [Value::F32(arg1), Value::F32(arg2)],
        Value::I32(expected as i32),
    )
}

fn test_f32_binop(op: OpCode, arg1: f32, arg2: f32, expected: f32) {
    test_op_example(
        op,
        [Value::F32(arg1), Value::F32(arg2)],
        Value::F32(expected),
    )
}

fn test_f32_unop(op: OpCode, arg: f32, expected: f32) {
    test_op_example(op, [Value::F32(arg)], Value::F32(expected))
}

#[test]
fn test_f32eq() {
    let op = F32EQ;
    test_f32_comparison(op, 1.1, 1.1, true);
    test_f32_comparison(op, 1.1, -1.1, false);
}

#[test]
fn test_f32ne() {
    let op = F32NE;
    test_f32_comparison(op, 1.1, 1.1, false);
    test_f32_comparison(op, 1.1, -1.1, true);
}

#[test]
fn test_f32lt() {
    let op = F32LT;
    test_f32_comparison(op, 1.1, 1.1, false);
    test_f32_comparison(op, 1.1, -1.1, false);
    test_f32_comparison(op, -1.1, 1.1, true);
}

#[test]
fn test_f32gt() {
    let op = F32GT;
    test_f32_comparison(op, 1.1, 1.1, false);
    test_f32_comparison(op, 1.1, -1.1, true);
    test_f32_comparison(op, -1.1, 1.1, false);
}

#[test]
fn test_f32le() {
    let op = F32LE;
    test_f32_comparison(op, 1.1, 1.1, true);
    test_f32_comparison(op, 1.1, -1.1, false);
    test_f32_comparison(op, -1.1, 1.1, true);
}

#[test]
fn test_f32ge() {
    let op = F32GE;
    test_f32_comparison(op, 1.1, 1.1, true);
    test_f32_comparison(op, 1.1, -1.1, true);
    test_f32_comparison(op, -1.1, 1.1, false);
}

#[test]
fn test_f32abs() {
    let op = F32ABS;
    test_f32_unop(op, 0.0, 0.0);
    test_f32_unop(op, 1.1, 1.1);
    test_f32_unop(op, -1.1, 1.1);
}

#[test]
fn test_f32neg() {
    let op = F32NEG;
    test_f32_unop(op, 0.0, 0.0);
    test_f32_unop(op, 1.1, -1.1);
    test_f32_unop(op, -1.1, 1.1);
}

#[test]
fn test_f32ceil() {
    let op = F32CEIL;
    test_f32_unop(op, 1.1, 2.0);
    test_f32_unop(op, -1.1, -1.0);
}

#[test]
fn test_f32floor() {
    let op = F32FLOOR;
    test_f32_unop(op, 1.1, 1.0);
    test_f32_unop(op, -1.1, -2.0);
}

#[test]
fn test_f32trunc() {
    let op = F32TRUNC;
    test_f32_unop(op, 1.1, 1.0);
    test_f32_unop(op, -1.1, -1.0);
}

#[test]
fn test_f32nearest() {
    let op = F32NEAREST;
    test_f32_unop(op, 1.4, 1.0);
    test_f32_unop(op, 1.6, 2.0);
    test_f32_unop(op, -1.4, -1.0);
    test_f32_unop(op, -1.6, -2.0);
    test_f32_unop(op, 1.5, 2.0);
    test_f32_unop(op, 2.5, 2.0);
}

#[test]
fn test_f32sqrt() {
    let op = F32SQRT;
    test_f32_unop(op, 4.0, 2.0);
}

#[test]
fn test_f32add() {
    let op = F32ADD;
    test_f32_binop(op, 0.0, 0.0, 0.0);
    test_f32_binop(op, -1.1, -1.1, -2.2);
    test_f32_binop(op, 1.1, 1.1, 2.2);
    test_f32_binop(op, 1.1, -1.1, 0.0);
}

#[test]
fn test_f32sub() {
    let op = F32SUB;
    test_f32_binop(op, 0.0, 0.0, 0.0);
    test_f32_binop(op, -1.1, -1.1, 0.0);
    test_f32_binop(op, 1.1, -1.1, 2.2);
}

#[test]
fn test_f32mul() {
    let op = F32MUL;
    test_f32_binop(op, 1.1, 0.0, 0.0);
    test_f32_binop(op, -1.5, -1.5, 2.25);
    test_f32_binop(op, 1.5, 1.5, 2.25);
    test_f32_binop(op, 1.5, -1.5, -2.25);
}

#[test]
fn test_f32div() {
    let op = F32DIV;
    test_f32_binop(op, -1.1, -1.1, 1.0);
    test_f32_binop(op, 1.1, -1.1, -1.0);
    test_f32_binop(op, 5.0, 2.0, 2.5);

    test_f32_binop(op, 1.0, 0.0, f32::INFINITY);
    test_f32_binop(op, -1.0, 0.0, f32::NEG_INFINITY);
    // test_f32_binop(op, 0.0, 0.0, f32::NAN); // can't check NaN for equality! LOL
}

#[test]
fn test_f32min() {
    let op = F32MIN;
    test_f32_binop(op, 1.1, 2.2, 1.1);
    test_f32_binop(op, -1.1, -2.2, -2.2);
}

#[test]
fn test_f32max() {
    let op = F32MAX;
    test_f32_binop(op, 1.1, 2.2, 2.2);
    test_f32_binop(op, -1.1, -2.2, -1.1);
}

#[test]
fn test_f32copysign() {
    let op = F32COPYSIGN;
    test_f32_binop(op, 1.1, 2.2, 1.1);
    test_f32_binop(op, -1.1, -2.2, -1.1);
    test_f32_binop(op, -1.1, 1.1, 1.1);
    test_f32_binop(op, 1.1, -1.1, -1.1);
}
