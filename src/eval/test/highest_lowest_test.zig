//! Unit tests for `highest` and `lowest` constants on every numeric type
//! defined in Builtin.roc.
//!
//! Every test name is prefixed with `highest_lowest:` so the whole suite can
//! be run via:
//!
//!     zig build test -- --test-filter "highest_lowest"
const helpers = @import("helpers.zig");
const runExpectI64 = helpers.runExpectI64;
const runExpectF32 = helpers.runExpectF32;
const runExpectF64 = helpers.runExpectF64;
const runExpectDec = helpers.runExpectDec;
const runExpectBool = helpers.runExpectBool;

// U8

test "highest_lowest: U8.highest" {
    try runExpectI64("U8.highest", 255, .no_trace);
}

test "highest_lowest: U8.lowest" {
    try runExpectI64("U8.lowest", 0, .no_trace);
}

test "highest_lowest: U8.from_str at highest boundary" {
    try runExpectBool("U8.from_str(\"255\").is_ok()", true, .no_trace);
}

test "highest_lowest: U8.from_str past highest boundary" {
    try runExpectBool("U8.from_str(\"256\").is_err()", true, .no_trace);
}

test "highest_lowest: U8.from_str negative rejected" {
    try runExpectBool("U8.from_str(\"-1\").is_err()", true, .no_trace);
}

// I8

test "highest_lowest: I8.highest" {
    try runExpectI64("I8.highest", 127, .no_trace);
}

test "highest_lowest: I8.lowest" {
    try runExpectI64("I8.lowest", -128, .no_trace);
}

test "highest_lowest: I8.from_str at highest boundary" {
    try runExpectBool("I8.from_str(\"127\").is_ok()", true, .no_trace);
}

test "highest_lowest: I8.from_str past highest boundary" {
    try runExpectBool("I8.from_str(\"128\").is_err()", true, .no_trace);
}

test "highest_lowest: I8.from_str at lowest boundary" {
    try runExpectBool("I8.from_str(\"-128\").is_ok()", true, .no_trace);
}

test "highest_lowest: I8.from_str past lowest boundary" {
    try runExpectBool("I8.from_str(\"-129\").is_err()", true, .no_trace);
}

// U16

test "highest_lowest: U16.highest" {
    try runExpectI64("U16.highest", 65535, .no_trace);
}

test "highest_lowest: U16.lowest" {
    try runExpectI64("U16.lowest", 0, .no_trace);
}

test "highest_lowest: U16.from_str at highest boundary" {
    try runExpectBool("U16.from_str(\"65535\").is_ok()", true, .no_trace);
}

test "highest_lowest: U16.from_str past highest boundary" {
    try runExpectBool("U16.from_str(\"65536\").is_err()", true, .no_trace);
}

test "highest_lowest: U16.from_str negative rejected" {
    try runExpectBool("U16.from_str(\"-1\").is_err()", true, .no_trace);
}

// I16

test "highest_lowest: I16.highest" {
    try runExpectI64("I16.highest", 32767, .no_trace);
}

test "highest_lowest: I16.lowest" {
    try runExpectI64("I16.lowest", -32768, .no_trace);
}

test "highest_lowest: I16.from_str at highest boundary" {
    try runExpectBool("I16.from_str(\"32767\").is_ok()", true, .no_trace);
}

test "highest_lowest: I16.from_str past highest boundary" {
    try runExpectBool("I16.from_str(\"32768\").is_err()", true, .no_trace);
}

test "highest_lowest: I16.from_str at lowest boundary" {
    try runExpectBool("I16.from_str(\"-32768\").is_ok()", true, .no_trace);
}

test "highest_lowest: I16.from_str past lowest boundary" {
    try runExpectBool("I16.from_str(\"-32769\").is_err()", true, .no_trace);
}

// U32

test "highest_lowest: U32.highest" {
    try runExpectI64("U32.highest", 4_294_967_295, .no_trace);
}

test "highest_lowest: U32.lowest" {
    try runExpectI64("U32.lowest", 0, .no_trace);
}

test "highest_lowest: U32.from_str at highest boundary" {
    try runExpectBool("U32.from_str(\"4294967295\").is_ok()", true, .no_trace);
}

test "highest_lowest: U32.from_str past highest boundary" {
    try runExpectBool("U32.from_str(\"4294967296\").is_err()", true, .no_trace);
}

test "highest_lowest: U32.from_str negative rejected" {
    try runExpectBool("U32.from_str(\"-1\").is_err()", true, .no_trace);
}

// I32

test "highest_lowest: I32.highest" {
    try runExpectI64("I32.highest", 2_147_483_647, .no_trace);
}

test "highest_lowest: I32.lowest" {
    try runExpectI64("I32.lowest", -2_147_483_648, .no_trace);
}

test "highest_lowest: I32.from_str at highest boundary" {
    try runExpectBool("I32.from_str(\"2147483647\").is_ok()", true, .no_trace);
}

test "highest_lowest: I32.from_str past highest boundary" {
    try runExpectBool("I32.from_str(\"2147483648\").is_err()", true, .no_trace);
}

test "highest_lowest: I32.from_str at lowest boundary" {
    try runExpectBool("I32.from_str(\"-2147483648\").is_ok()", true, .no_trace);
}

test "highest_lowest: I32.from_str past lowest boundary" {
    try runExpectBool("I32.from_str(\"-2147483649\").is_err()", true, .no_trace);
}

// U64

test "highest_lowest: U64.highest" {
    try runExpectI64("U64.highest", 18_446_744_073_709_551_615, .no_trace);
}

test "highest_lowest: U64.lowest" {
    try runExpectI64("U64.lowest", 0, .no_trace);
}

test "highest_lowest: U64.from_str at highest boundary" {
    try runExpectBool("U64.from_str(\"18446744073709551615\").is_ok()", true, .no_trace);
}

test "highest_lowest: U64.from_str past highest boundary" {
    try runExpectBool("U64.from_str(\"18446744073709551616\").is_err()", true, .no_trace);
}

test "highest_lowest: U64.from_str negative rejected" {
    try runExpectBool("U64.from_str(\"-1\").is_err()", true, .no_trace);
}

// I64

test "highest_lowest: I64.highest" {
    try runExpectI64("I64.highest", 9_223_372_036_854_775_807, .no_trace);
}

test "highest_lowest: I64.lowest" {
    try runExpectI64("I64.lowest", -9_223_372_036_854_775_808, .no_trace);
}

test "highest_lowest: I64.from_str at highest boundary" {
    try runExpectBool("I64.from_str(\"9223372036854775807\").is_ok()", true, .no_trace);
}

test "highest_lowest: I64.from_str past highest boundary" {
    try runExpectBool("I64.from_str(\"9223372036854775808\").is_err()", true, .no_trace);
}

test "highest_lowest: I64.from_str at lowest boundary" {
    try runExpectBool("I64.from_str(\"-9223372036854775808\").is_ok()", true, .no_trace);
}

test "highest_lowest: I64.from_str past lowest boundary" {
    try runExpectBool("I64.from_str(\"-9223372036854775809\").is_err()", true, .no_trace);
}

// U128 — value exceeds i128, so highest is verified via to_str round-trip

test "highest_lowest: U128.highest" {
    try runExpectBool(
        "U128.to_str(U128.highest) == \"340282366920938463463374607431768211455\"",
        true,
        .no_trace,
    );
}

test "highest_lowest: U128.lowest" {
    try runExpectI64("U128.lowest", 0, .no_trace);
}

test "highest_lowest: U128.from_str at highest boundary" {
    try runExpectBool(
        "U128.from_str(\"340282366920938463463374607431768211455\").is_ok()",
        true,
        .no_trace,
    );
}

test "highest_lowest: U128.from_str past highest boundary" {
    try runExpectBool(
        "U128.from_str(\"340282366920938463463374607431768211456\").is_err()",
        true,
        .no_trace,
    );
}

test "highest_lowest: U128.from_str negative rejected" {
    try runExpectBool("U128.from_str(\"-1\").is_err()", true, .no_trace);
}

// I128

test "highest_lowest: I128.highest" {
    try runExpectI64("I128.highest", 170141183460469231731687303715884105727, .no_trace);
}

test "highest_lowest: I128.lowest" {
    try runExpectI64("I128.lowest", -170141183460469231731687303715884105728, .no_trace);
}

test "highest_lowest: I128.from_str at highest boundary" {
    try runExpectBool(
        "I128.from_str(\"170141183460469231731687303715884105727\").is_ok()",
        true,
        .no_trace,
    );
}

test "highest_lowest: I128.from_str past highest boundary" {
    try runExpectBool(
        "I128.from_str(\"170141183460469231731687303715884105728\").is_err()",
        true,
        .no_trace,
    );
}

test "highest_lowest: I128.from_str at lowest boundary" {
    try runExpectBool(
        "I128.from_str(\"-170141183460469231731687303715884105728\").is_ok()",
        true,
        .no_trace,
    );
}

test "highest_lowest: I128.from_str past lowest boundary" {
    try runExpectBool(
        "I128.from_str(\"-170141183460469231731687303715884105729\").is_err()",
        true,
        .no_trace,
    );
}

// Dec — fixed-point i128 scaled by 10^18.
// `runExpectDec` compares the raw i128 storage.

test "highest_lowest: Dec.highest" {
    // Dec is i128-backed, scaled by 10^18.
    // Dec.highest == 170141183460469231731.687303715884105727
    // raw i128 storage == 170141183460469231731687303715884105727 (= 2^127 - 1)
    try runExpectDec("Dec.highest", 170141183460469231731687303715884105727, .no_trace);
}

test "highest_lowest: Dec.lowest" {
    // Dec.lowest == -170141183460469231731.687303715884105728
    // raw i128 storage == -170141183460469231731687303715884105728 (= -2^127)
    try runExpectDec("Dec.lowest", -170141183460469231731687303715884105728, .no_trace);
}

test "highest_lowest: Dec.from_str at highest boundary" {
    try runExpectBool(
        "Dec.from_str(\"170141183460469231731.687303715884105727\").is_ok()",
        true,
        .no_trace,
    );
}

test "highest_lowest: Dec.from_str past highest boundary" {
    try runExpectBool(
        "Dec.from_str(\"170141183460469231731.687303715884105728\").is_err()",
        true,
        .no_trace,
    );
}

test "highest_lowest: Dec.from_str at lowest boundary" {
    try runExpectBool(
        "Dec.from_str(\"-170141183460469231731.687303715884105728\").is_ok()",
        true,
        .no_trace,
    );
}

test "highest_lowest: Dec.from_str past lowest boundary" {
    try runExpectBool(
        "Dec.from_str(\"-170141183460469231731.687303715884105729\").is_err()",
        true,
        .no_trace,
    );
}

// F32 — IEEE 754 finite max ≈ 3.40282347e38

test "highest_lowest: F32.highest" {
    try runExpectF32("F32.highest", 3.40282347e38, .no_trace);
}

test "highest_lowest: F32.lowest" {
    try runExpectF32("F32.lowest", -3.40282347e38, .no_trace);
}

test "highest_lowest: F32.from_str at highest boundary" {
    try runExpectBool("F32.from_str(\"3.40282347e38\").is_ok()", true, .no_trace);
}

test "highest_lowest: F32.from_str at lowest boundary" {
    try runExpectBool("F32.from_str(\"-3.40282347e38\").is_ok()", true, .no_trace);
}

// F64 — IEEE 754 finite max ≈ 1.7976931348623157e308

test "highest_lowest: F64.highest" {
    try runExpectF64("F64.highest", 1.7976931348623157e308, .no_trace);
}

test "highest_lowest: F64.lowest" {
    try runExpectF64("F64.lowest", -1.7976931348623157e308, .no_trace);
}

test "highest_lowest: F64.from_str at highest boundary" {
    try runExpectBool("F64.from_str(\"1.7976931348623157e308\").is_ok()", true, .no_trace);
}

test "highest_lowest: F64.from_str at lowest boundary" {
    try runExpectBool("F64.from_str(\"-1.7976931348623157e308\").is_ok()", true, .no_trace);
}
