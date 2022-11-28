#![cfg(test)]

use roc_wasm_interp::test_utils::test_op_example;
use roc_wasm_module::{opcodes::OpCode, opcodes::OpCode::*, Value};

fn test_i32_binop(op: OpCode, arg1: i32, arg2: i32, expected: i32) {
    test_op_example(
        op,
        [Value::from(arg1), Value::from(arg2)],
        Value::from(expected),
    )
}

fn test_u32_binop(op: OpCode, arg1: u32, arg2: u32, expected: u32) {
    test_op_example(
        op,
        [Value::from(arg1), Value::from(arg2)],
        Value::from(expected),
    )
}

fn test_i32_unop(op: OpCode, arg: i32, expected: i32) {
    test_op_example(op, [Value::from(arg)], Value::from(expected))
}

#[test]
fn test_i32eqz() {
    let op = I32EQZ;
    test_i32_unop(op, 0, 1);
    test_i32_unop(op, i32::MIN, 0);
    test_i32_unop(op, i32::MAX, 0);
}

#[test]
fn test_i32eq() {
    let op = I32EQ;
    test_i32_binop(op, 0, 0, 1);
    test_i32_binop(op, i32::MIN, i32::MIN, 1);
    test_i32_binop(op, i32::MAX, i32::MAX, 1);
    test_i32_binop(op, i32::MIN, i32::MAX, 0);
    test_i32_binop(op, i32::MAX, i32::MIN, 0);
}

#[test]
fn test_i32ne() {
    let op = I32NE;
    test_i32_binop(op, 0, 0, 0);
    test_i32_binop(op, i32::MIN, i32::MIN, 0);
    test_i32_binop(op, i32::MAX, i32::MAX, 0);
    test_i32_binop(op, i32::MIN, i32::MAX, 1);
    test_i32_binop(op, i32::MAX, i32::MIN, 1);
}

#[test]
fn test_i32lts() {
    let op = I32LTS;
    test_i32_binop(op, 0, 0, 0);
    test_i32_binop(op, i32::MIN, i32::MIN, 0);
    test_i32_binop(op, i32::MAX, i32::MAX, 0);
    test_i32_binop(op, i32::MIN, i32::MAX, 1);
    test_i32_binop(op, i32::MAX, i32::MIN, 0);
}

#[test]
fn test_i32ltu() {
    let op = I32LTU;
    test_u32_binop(op, 0, 0, 0);
    test_u32_binop(op, u32::MIN, u32::MIN, 0);
    test_u32_binop(op, u32::MAX, u32::MAX, 0);
    test_u32_binop(op, u32::MIN, u32::MAX, 1);
    test_u32_binop(op, u32::MAX, u32::MIN, 0);
}

#[test]
fn test_i32gts() {
    let op = I32GTS;
    test_i32_binop(op, 0, 0, 0);
    test_i32_binop(op, i32::MIN, i32::MIN, 0);
    test_i32_binop(op, i32::MAX, i32::MAX, 0);
    test_i32_binop(op, i32::MIN, i32::MAX, 0);
    test_i32_binop(op, i32::MAX, i32::MIN, 1);
}

#[test]
fn test_i32gtu() {
    let op = I32GTU;
    test_u32_binop(op, 0, 0, 0);
    test_u32_binop(op, u32::MIN, u32::MIN, 0);
    test_u32_binop(op, u32::MAX, u32::MAX, 0);
    test_u32_binop(op, u32::MIN, u32::MAX, 0);
    test_u32_binop(op, u32::MAX, u32::MIN, 1);
}

#[test]
fn test_i32les() {
    let op = I32LES;
    test_i32_binop(op, 0, 0, 1);
    test_i32_binop(op, i32::MIN, i32::MIN, 1);
    test_i32_binop(op, i32::MAX, i32::MAX, 1);
    test_i32_binop(op, i32::MIN, i32::MAX, 1);
    test_i32_binop(op, i32::MAX, i32::MIN, 0);
}

#[test]
fn test_i32leu() {
    let op = I32LEU;
    test_u32_binop(op, 0, 0, 1);
    test_u32_binop(op, u32::MIN, u32::MIN, 1);
    test_u32_binop(op, u32::MAX, u32::MAX, 1);
    test_u32_binop(op, u32::MIN, u32::MAX, 1);
    test_u32_binop(op, u32::MAX, u32::MIN, 0);
}

#[test]
fn test_i32ges() {
    let op = I32GES;
    test_i32_binop(op, 0, 0, 1);
    test_i32_binop(op, i32::MIN, i32::MIN, 1);
    test_i32_binop(op, i32::MAX, i32::MAX, 1);
    test_i32_binop(op, i32::MIN, i32::MAX, 0);
    test_i32_binop(op, i32::MAX, i32::MIN, 1);
}

#[test]
fn test_i32geu() {
    let op = I32GEU;
    test_u32_binop(op, 0, 0, 1);
    test_u32_binop(op, u32::MIN, u32::MIN, 1);
    test_u32_binop(op, u32::MAX, u32::MAX, 1);
    test_u32_binop(op, u32::MIN, u32::MAX, 0);
    test_u32_binop(op, u32::MAX, u32::MIN, 1);
}

#[test]
fn test_i32clz() {
    let op = I32CLZ;
    test_i32_unop(op, 0, 32);
    test_i32_unop(op, -1, 0);
    test_i32_unop(op, 1, 31);
    test_i32_unop(op, 1024, 21);
}

#[test]
fn test_i32ctz() {
    let op = I32CTZ;
    test_i32_unop(op, 0, 32);
    test_i32_unop(op, -1, 0);
    test_i32_unop(op, 2, 1);
    test_i32_unop(op, 1024, 10);
}

#[test]
fn test_i32popcnt() {
    let op = I32POPCNT;
    test_i32_unop(op, 0, 0);
    test_i32_unop(op, -1, 32);
    test_i32_unop(op, 2, 1);
    test_i32_unop(op, 96, 2);
}

#[test]
fn test_i32add() {
    let op = I32ADD;
    test_i32_binop(op, 0, 0, 0);
    test_i32_binop(op, -1, -1, -2);
    test_i32_binop(op, 1, 1, 2);
    test_i32_binop(op, i32::MAX, 1, i32::MIN);
}

#[test]
fn test_i32sub() {
    let op = I32SUB;
    test_i32_binop(op, 0, 0, 0);
    test_i32_binop(op, -1, 1, -2);
    test_i32_binop(op, 1, 1, 0);
    test_i32_binop(op, i32::MIN, 1, i32::MAX);
}

#[test]
fn test_i32mul() {
    let op = I32MUL;
    test_i32_binop(op, 0, 0, 0);
    test_i32_binop(op, -1, -1, 1);
    test_i32_binop(op, 2, 3, 6);
    test_i32_binop(op, i32::MAX, 2, -2);
}

#[test]
fn test_i32divs() {
    let op = I32DIVS;
    test_i32_binop(op, -1, -1, 1);
    test_i32_binop(op, 6, 3, 2);
    test_i32_binop(op, i32::MIN, -1, i32::MIN);
}

#[test]
#[should_panic(expected = "divide by zero")]
fn test_i32divs_zero() {
    test_i32_binop(I32DIVS, 1, 0, -1);
}

#[test]
fn test_i32divu() {
    let op = I32DIVU;
    test_u32_binop(op, 1, 1, 1);
    test_u32_binop(op, 6, 3, 2);
}

#[test]
#[should_panic(expected = "divide by zero")]
fn test_i32divu_zero() {
    test_i32_binop(I32DIVU, 1, 0, -1);
}

#[test]
fn test_i32rems() {
    let op = I32REMS;
    test_i32_binop(op, 5, 2, 1);
    // test_i32_binop(op, 5, -2, 1); // TODO: we don't match Wasmer, we get 0
    test_i32_binop(op, -5, 2, -1);
    // test_i32_binop(op, -5, -2, -1); // TODO: we don't match Wasmer, we get 0
}

#[test]
#[should_panic(expected = "divisor of zero")]
fn test_i32rems_zero() {
    test_i32_binop(I32REMS, 1, 0, -1);
}

#[test]
fn test_i32remu() {
    let op = I32REMU;
    test_i32_binop(op, 5, 2, 1);
}

#[test]
#[should_panic(expected = "divisor of zero")]
fn test_i32remu_zero() {
    test_i32_binop(I32REMU, 1, 0, -1);
}

#[test]
fn test_i32and() {
    test_u32_binop(I32AND, 0x0000_ffff, 0x00ff_00ff, 0x0000_00ff);
}

#[test]
fn test_i32or() {
    test_u32_binop(I32OR, 0x0000_ffff, 0x00ff_00ff, 0x00ff_ffff);
}

#[test]
fn test_i32xor() {
    test_u32_binop(I32XOR, 0x0000_ffff, 0x00ff_00ff, 0x00ff_ff00);
}

#[test]
fn test_i32shl() {
    test_u32_binop(I32SHL, 0xffff_ffff, 8, 0xffff_ff00);
    test_u32_binop(I32SHL, 0xffff_ffff, 40, 0xffff_ff00);
}

#[test]
fn test_i32shrs() {
    test_u32_binop(I32SHRS, 0xffff_0000, 8, 0xffff_ff00);
    test_u32_binop(I32SHRS, 0xffff_0000, 40, 0xffff_ff00);
}

#[test]
fn test_i32shru() {
    test_u32_binop(I32SHRU, 0xffff_0000, 8, 0x00ff_ff00);
    test_u32_binop(I32SHRU, 0xffff_0000, 40, 0x00ff_ff00);
}

#[test]
fn test_i32rotl() {
    test_u32_binop(I32ROTL, 0xff00_0000, 4, 0xf000_000f);
    test_u32_binop(I32ROTL, 0xff00_0000, 36, 0xf000_000f);
}

#[test]
fn test_i32rotr() {
    test_u32_binop(I32ROTR, 0x0000_00ff, 4, 0xf000_000f);
    test_u32_binop(I32ROTR, 0x0000_00ff, 36, 0xf000_000f);
}
