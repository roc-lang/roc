#![cfg(test)]

use roc_wasm_interp::test_utils::test_op_example;
use roc_wasm_module::{opcodes::OpCode, opcodes::OpCode::*, Value};

fn test_i64_comparison(op: OpCode, arg1: i64, arg2: i64, expected: bool) {
    test_op_example(
        op,
        [Value::from(arg1), Value::from(arg2)],
        Value::I32(expected as i32),
    )
}

fn test_u64_comparison(op: OpCode, arg1: u64, arg2: u64, expected: bool) {
    test_op_example(
        op,
        [Value::from(arg1), Value::from(arg2)],
        Value::I32(expected as i32),
    )
}

fn test_i64_binop(op: OpCode, arg1: i64, arg2: i64, expected: i64) {
    test_op_example(
        op,
        [Value::from(arg1), Value::from(arg2)],
        Value::from(expected),
    )
}

fn test_u64_binop(op: OpCode, arg1: u64, arg2: u64, expected: u64) {
    test_op_example(
        op,
        [Value::from(arg1), Value::from(arg2)],
        Value::from(expected),
    )
}

fn test_i64_unop(op: OpCode, arg: i64, expected: i64) {
    test_op_example(op, [Value::from(arg)], Value::from(expected))
}

#[test]
fn test_i64eqz() {
    let op = I64EQZ;
    test_op_example(op, [Value::I64(0)], Value::I32(true as i32));
    test_op_example(op, [Value::I64(i64::MIN)], Value::I32(false as i32));
    test_op_example(op, [Value::I64(i64::MAX)], Value::I32(false as i32));
}

#[test]
fn test_i64eq() {
    let op = I64EQ;
    test_i64_comparison(op, 0, 0, true);
    test_i64_comparison(op, i64::MIN, i64::MIN, true);
    test_i64_comparison(op, i64::MAX, i64::MAX, true);
    test_i64_comparison(op, i64::MIN, i64::MAX, false);
    test_i64_comparison(op, i64::MAX, i64::MIN, false);
}

#[test]
fn test_i64ne() {
    let op = I64NE;
    test_i64_comparison(op, 0, 0, false);
    test_i64_comparison(op, i64::MIN, i64::MIN, false);
    test_i64_comparison(op, i64::MAX, i64::MAX, false);
    test_i64_comparison(op, i64::MIN, i64::MAX, true);
    test_i64_comparison(op, i64::MAX, i64::MIN, true);
}

#[test]
fn test_i64lts() {
    let op = I64LTS;
    test_i64_comparison(op, 0, 0, false);
    test_i64_comparison(op, i64::MIN, i64::MIN, false);
    test_i64_comparison(op, i64::MAX, i64::MAX, false);
    test_i64_comparison(op, i64::MIN, i64::MAX, true);
    test_i64_comparison(op, i64::MAX, i64::MIN, false);
}

#[test]
fn test_i64ltu() {
    let op = I64LTU;
    test_u64_comparison(op, 0, 0, false);
    test_u64_comparison(op, u64::MIN, u64::MIN, false);
    test_u64_comparison(op, u64::MAX, u64::MAX, false);
    test_u64_comparison(op, u64::MIN, u64::MAX, true);
    test_u64_comparison(op, u64::MAX, u64::MIN, false);
}

#[test]
fn test_i64gts() {
    let op = I64GTS;
    test_i64_comparison(op, 0, 0, false);
    test_i64_comparison(op, i64::MIN, i64::MIN, false);
    test_i64_comparison(op, i64::MAX, i64::MAX, false);
    test_i64_comparison(op, i64::MIN, i64::MAX, false);
    test_i64_comparison(op, i64::MAX, i64::MIN, true);
}

#[test]
fn test_i64gtu() {
    let op = I64GTU;
    test_u64_comparison(op, 0, 0, false);
    test_u64_comparison(op, u64::MIN, u64::MIN, false);
    test_u64_comparison(op, u64::MAX, u64::MAX, false);
    test_u64_comparison(op, u64::MIN, u64::MAX, false);
    test_u64_comparison(op, u64::MAX, u64::MIN, true);
}

#[test]
fn test_i64les() {
    let op = I64LES;
    test_i64_comparison(op, 0, 0, true);
    test_i64_comparison(op, i64::MIN, i64::MIN, true);
    test_i64_comparison(op, i64::MAX, i64::MAX, true);
    test_i64_comparison(op, i64::MIN, i64::MAX, true);
    test_i64_comparison(op, i64::MAX, i64::MIN, false);
}

#[test]
fn test_i64leu() {
    let op = I64LEU;
    test_u64_comparison(op, 0, 0, true);
    test_u64_comparison(op, u64::MIN, u64::MIN, true);
    test_u64_comparison(op, u64::MAX, u64::MAX, true);
    test_u64_comparison(op, u64::MIN, u64::MAX, true);
    test_u64_comparison(op, u64::MAX, u64::MIN, false);
}

#[test]
fn test_i64ges() {
    let op = I64GES;
    test_i64_comparison(op, 0, 0, true);
    test_i64_comparison(op, i64::MIN, i64::MIN, true);
    test_i64_comparison(op, i64::MAX, i64::MAX, true);
    test_i64_comparison(op, i64::MIN, i64::MAX, false);
    test_i64_comparison(op, i64::MAX, i64::MIN, true);
}

#[test]
fn test_i64geu() {
    let op = I64GEU;
    test_u64_comparison(op, 0, 0, true);
    test_u64_comparison(op, u64::MIN, u64::MIN, true);
    test_u64_comparison(op, u64::MAX, u64::MAX, true);
    test_u64_comparison(op, u64::MIN, u64::MAX, false);
    test_u64_comparison(op, u64::MAX, u64::MIN, true);
}

#[test]
fn test_i64clz() {
    let op = I64CLZ;
    test_i64_unop(op, 0, 64);
    test_i64_unop(op, -1, 0);
    test_i64_unop(op, 1, 63);
    test_i64_unop(op, 1024, 53);
}

#[test]
fn test_i64ctz() {
    let op = I64CTZ;
    test_i64_unop(op, 0, 64);
    test_i64_unop(op, -1, 0);
    test_i64_unop(op, 2, 1);
    test_i64_unop(op, 1024, 10);
}

#[test]
fn test_i64popcnt() {
    let op = I64POPCNT;
    test_i64_unop(op, 0, 0);
    test_i64_unop(op, -1, 64);
    test_i64_unop(op, 2, 1);
    test_i64_unop(op, 96, 2);
}

#[test]
fn test_i64add() {
    let op = I64ADD;
    test_i64_binop(op, 0, 0, 0);
    test_i64_binop(op, -1, -1, -2);
    test_i64_binop(op, 1, 1, 2);
    test_i64_binop(op, i64::MAX, 1, i64::MIN);
}

#[test]
fn test_i64sub() {
    let op = I64SUB;
    test_i64_binop(op, 0, 0, 0);
    test_i64_binop(op, -1, 1, -2);
    test_i64_binop(op, 1, 1, 0);
    test_i64_binop(op, i64::MIN, 1, i64::MAX);
}

#[test]
fn test_i64mul() {
    let op = I64MUL;
    test_i64_binop(op, 0, 0, 0);
    test_i64_binop(op, -1, -1, 1);
    test_i64_binop(op, 2, 3, 6);
    test_i64_binop(op, i64::MAX, 2, -2);
}

#[test]
fn test_i64divs() {
    let op = I64DIVS;
    test_i64_binop(op, -1, -1, 1);
    test_i64_binop(op, 6, 3, 2);
    test_i64_binop(op, i64::MIN, -1, i64::MIN);
}

#[test]
#[should_panic(expected = "divide by zero")]
fn test_i64divs_zero() {
    test_i64_binop(I64DIVS, 1, 0, -1);
}

#[test]
fn test_i64divu() {
    let op = I64DIVU;
    test_u64_binop(op, 1, 1, 1);
    test_u64_binop(op, 6, 3, 2);
}

#[test]
#[should_panic(expected = "divide by zero")]
fn test_i64divu_zero() {
    test_i64_binop(I64DIVU, 1, 0, -1);
}

#[test]
fn test_i64rems() {
    let op = I64REMS;
    test_i64_binop(op, 5, 2, 1);
    // test_i64_binop(op, 5, -2, 1); // TODO: we don't match Wasmer, we get 0
    test_i64_binop(op, -5, 2, -1);
    // test_i64_binop(op, -5, -2, -1); // TODO: we don't match Wasmer, we get 0
}

#[test]
#[should_panic(expected = "divisor of zero")]
fn test_i64rems_zero() {
    test_i64_binop(I64REMS, 1, 0, -1);
}

#[test]
fn test_i64remu() {
    let op = I64REMU;
    test_i64_binop(op, 5, 2, 1);
}

#[test]
#[should_panic(expected = "divisor of zero")]
fn test_i64remu_zero() {
    test_i64_binop(I64REMU, 1, 0, -1);
}

#[test]
fn test_i64and() {
    test_u64_binop(
        I64AND,
        0x0000_0000_ffff_ffff,
        0x0000_ffff_0000_ffff,
        0x0000_0000_0000_ffff,
    );
}

#[test]
fn test_i64or() {
    test_u64_binop(
        I64OR,
        0x0000_0000_ffff_ffff,
        0x0000_ffff_0000_ffff,
        0x0000_ffff_ffff_ffff,
    );
}

#[test]
fn test_i64xor() {
    test_u64_binop(
        I64XOR,
        0x0000_0000_ffff_ffff,
        0x0000_ffff_0000_ffff,
        0x0000_ffff_ffff_0000,
    );
}

#[test]
fn test_i64shl() {
    test_u64_binop(I64SHL, 0xffff_ffff_ffff_ffff, 8, 0xffff_ffff_ffff_ff00);
    test_u64_binop(I64SHL, 0xffff_ffff_ffff_ffff, 72, 0xffff_ffff_ffff_ff00);
}

#[test]
fn test_i64shrs() {
    test_u64_binop(I64SHRS, 0xffff_ffff_0000_0000, 8, 0xffff_ffff_ff00_0000);
    test_u64_binop(I64SHRS, 0xffff_ffff_0000_0000, 72, 0xffff_ffff_ff00_0000);
}

#[test]
fn test_i64shru() {
    test_u64_binop(I64SHRU, 0xffff_ffff_0000_0000, 8, 0x00ff_ffff_ff00_0000);
    test_u64_binop(I64SHRU, 0xffff_ffff_0000_0000, 72, 0x00ff_ffff_ff00_0000);
}

#[test]
fn test_i64rotl() {
    test_u64_binop(I64ROTL, 0xff00_0000_0000_0000, 4, 0xf000_0000_0000_000f);
    test_u64_binop(I64ROTL, 0xff00_0000_0000_0000, 68, 0xf000_0000_0000_000f);
}

#[test]
fn test_i64rotr() {
    test_u64_binop(I64ROTR, 0x0000_0000_0000_00ff, 4, 0xf000_0000_0000_000f);
    test_u64_binop(I64ROTR, 0x0000_0000_0000_00ff, 68, 0xf000_0000_0000_000f);
}
