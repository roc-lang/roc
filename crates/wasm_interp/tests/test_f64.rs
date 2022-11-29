#![cfg(test)]

use roc_wasm_interp::test_utils::test_op_example;
use roc_wasm_module::{opcodes::OpCode, opcodes::OpCode::*, Value};

fn test_f64_comparison(op: OpCode, arg1: f64, arg2: f64, expected: bool) {
    test_op_example(
        op,
        [Value::F64(arg1), Value::F64(arg2)],
        Value::I32(expected as i32),
    )
}

fn test_f64_binop(op: OpCode, arg1: f64, arg2: f64, expected: f64) {
    test_op_example(
        op,
        [Value::F64(arg1), Value::F64(arg2)],
        Value::F64(expected),
    )
}

fn test_f64_unop(op: OpCode, arg: f64, expected: f64) {
    test_op_example(op, [Value::F64(arg)], Value::F64(expected))
}

#[test]
fn test_f64eq() {
    let op = F64EQ;
    test_f64_comparison(op, 1.1, 1.1, true);
    test_f64_comparison(op, 1.1, -1.1, false);
}

#[test]
fn test_f64ne() {
    let op = F64NE;
    test_f64_comparison(op, 1.1, 1.1, false);
    test_f64_comparison(op, 1.1, -1.1, true);
}

#[test]
fn test_f64lt() {
    let op = F64LT;
    test_f64_comparison(op, 1.1, 1.1, false);
    test_f64_comparison(op, 1.1, -1.1, false);
    test_f64_comparison(op, -1.1, 1.1, true);
}

#[test]
fn test_f64gt() {
    let op = F64GT;
    test_f64_comparison(op, 1.1, 1.1, false);
    test_f64_comparison(op, 1.1, -1.1, true);
    test_f64_comparison(op, -1.1, 1.1, false);
}

#[test]
fn test_f64le() {
    let op = F64LE;
    test_f64_comparison(op, 1.1, 1.1, true);
    test_f64_comparison(op, 1.1, -1.1, false);
    test_f64_comparison(op, -1.1, 1.1, true);
}

#[test]
fn test_f64ge() {
    let op = F64GE;
    test_f64_comparison(op, 1.1, 1.1, true);
    test_f64_comparison(op, 1.1, -1.1, true);
    test_f64_comparison(op, -1.1, 1.1, false);
}

#[test]
fn test_f64abs() {
    let op = F64ABS;
    test_f64_unop(op, 0.0, 0.0);
    test_f64_unop(op, 1.1, 1.1);
    test_f64_unop(op, -1.1, 1.1);
}

#[test]
fn test_f64neg() {
    let op = F64NEG;
    test_f64_unop(op, 0.0, 0.0);
    test_f64_unop(op, 1.1, -1.1);
    test_f64_unop(op, -1.1, 1.1);
}

#[test]
fn test_f64ceil() {
    let op = F64CEIL;
    test_f64_unop(op, 1.1, 2.0);
    test_f64_unop(op, -1.1, -1.0);
}

#[test]
fn test_f64floor() {
    let op = F64FLOOR;
    test_f64_unop(op, 1.1, 1.0);
    test_f64_unop(op, -1.1, -2.0);
}

#[test]
fn test_f64trunc() {
    let op = F64TRUNC;
    test_f64_unop(op, 1.1, 1.0);
    test_f64_unop(op, -1.1, -1.0);
}

#[test]
fn test_f64nearest() {
    let op = F64NEAREST;
    test_f64_unop(op, 1.4, 1.0);
    test_f64_unop(op, 1.6, 2.0);
    test_f64_unop(op, -1.4, -1.0);
    test_f64_unop(op, -1.6, -2.0);
    test_f64_unop(op, 1.5, 2.0);
    test_f64_unop(op, 2.5, 2.0);
}

#[test]
fn test_f64sqrt() {
    let op = F64SQRT;
    test_f64_unop(op, 4.0, 2.0);
}

#[test]
fn test_f64add() {
    let op = F64ADD;
    test_f64_binop(op, 0.0, 0.0, 0.0);
    test_f64_binop(op, -1.1, -1.1, -2.2);
    test_f64_binop(op, 1.1, 1.1, 2.2);
    test_f64_binop(op, 1.1, -1.1, 0.0);
}

#[test]
fn test_f64sub() {
    let op = F64SUB;
    test_f64_binop(op, 0.0, 0.0, 0.0);
    test_f64_binop(op, -1.1, -1.1, 0.0);
    test_f64_binop(op, 1.1, -1.1, 2.2);
}

#[test]
fn test_f64mul() {
    let op = F64MUL;
    test_f64_binop(op, 1.1, 0.0, 0.0);
    test_f64_binop(op, -1.5, -1.5, 2.25);
    test_f64_binop(op, 1.5, 1.5, 2.25);
    test_f64_binop(op, 1.5, -1.5, -2.25);
}

#[test]
fn test_f64div() {
    let op = F64DIV;
    test_f64_binop(op, -1.1, -1.1, 1.0);
    test_f64_binop(op, 1.1, -1.1, -1.0);
    test_f64_binop(op, 5.0, 2.0, 2.5);

    test_f64_binop(op, 1.0, 0.0, f64::INFINITY);
    test_f64_binop(op, -1.0, 0.0, f64::NEG_INFINITY);
    // to-probably-never-do: check NaN. It needs its own special test setup.
}

#[test]
fn test_f64min() {
    let op = F64MIN;
    test_f64_binop(op, 1.1, 2.2, 1.1);
    test_f64_binop(op, -1.1, -2.2, -2.2);
}

#[test]
fn test_f64max() {
    let op = F64MAX;
    test_f64_binop(op, 1.1, 2.2, 2.2);
    test_f64_binop(op, -1.1, -2.2, -1.1);
}

#[test]
fn test_f64copysign() {
    let op = F64COPYSIGN;
    test_f64_binop(op, 1.1, 2.2, 1.1);
    test_f64_binop(op, -1.1, -2.2, -1.1);
    test_f64_binop(op, -1.1, 1.1, 1.1);
    test_f64_binop(op, 1.1, -1.1, -1.1);
}
