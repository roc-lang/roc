//! Comprehensive tests for all arithmetic operations on all number types.
//!
//! This test file systematically verifies that every number type supports
//! all its arithmetic operations correctly when type-annotated expressions
//! are evaluated by the interpreter.
//!
//! Number types tested:
//! - Unsigned integers: U8, U16, U32, U64, U128 ✓
//! - Signed integers: I8, I16, I32, I64, I128 ✓
//! - Floating-point: F32 ✓, F64 ✓
//! - Fixed-point decimal: Dec ✓
//!
//! Operations tested (where supported):
//! - negate (signed types only)
//! - plus (+)
//! - minus (-)
//! - times (*)
//! - div_by (//)
//! - rem_by (%)
//!
//! Test values are chosen to be in ranges that clearly demonstrate the type:
//! - U8: Uses values > 127 (too large for I8)
//! - U16: Uses values > 32767 (too large for I16)
//! - U32: Uses values > 2147483647 (too large for I32)
//! - U64: Uses values > 9223372036854775807 (too large for I64)
//! - U128: Uses values > max I64/U64
//! - I8: Uses negative values < -127 or operations that produce negatives
//! - I16: Uses negative values < -128 (too negative for I8)
//! - I32: Uses negative values < -32768 (too negative for I16)
//! - I64: Uses negative values < -2147483648 (too negative for I32)
//! - I128: Uses negative values < min I64

const helpers = @import("helpers.zig");
const runExpectI64 = helpers.runExpectI64;
const runExpectF32 = helpers.runExpectF32;
const runExpectF64 = helpers.runExpectF64;
const runExpectDec = helpers.runExpectDec;
const runExpectStr = helpers.runExpectStr;

// U8 Tests (Unsigned 8-bit: 0 to 255)
// Uses values > 127 to prove they're not I8

test "U8: plus" {
    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 200
        \\    b : U8
        \\    b = 50
        \\    a + b
        \\}
    , 250, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 255
        \\    b : U8
        \\    b = 0
        \\    a + b
        \\}
    , 255, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 128
        \\    b : U8
        \\    b = 127
        \\    a + b
        \\}
    , 255, .no_trace);
}

test "U8: minus" {
    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 200
        \\    b : U8
        \\    b = 50
        \\    a - b
        \\}
    , 150, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 255
        \\    b : U8
        \\    b = 100
        \\    a - b
        \\}
    , 155, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 240
        \\    b : U8
        \\    b = 240
        \\    a - b
        \\}
    , 0, .no_trace);
}

test "U8: times" {
    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 15
        \\    b : U8
        \\    b = 17
        \\    a * b
        \\}
    , 255, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 128
        \\    b : U8
        \\    b = 1
        \\    a * b
        \\}
    , 128, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 16
        \\    b : U8
        \\    b = 15
        \\    a * b
        \\}
    , 240, .no_trace);
}

test "U8: div_by" {
    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 240
        \\    b : U8
        \\    b = 2
        \\    a // b
        \\}
    , 120, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 255
        \\    b : U8
        \\    b = 15
        \\    a // b
        \\}
    , 17, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 200
        \\    b : U8
        \\    b = 10
        \\    a // b
        \\}
    , 20, .no_trace);
}

test "U8: rem_by" {
    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 200
        \\    b : U8
        \\    b = 13
        \\    a % b
        \\}
    , 5, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 255
        \\    b : U8
        \\    b = 16
        \\    a % b
        \\}
    , 15, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U8
        \\    a = 128
        \\    b : U8
        \\    b = 7
        \\    a % b
        \\}
    , 2, .no_trace);
}

// U16 Tests (Unsigned 16-bit: 0 to 65535)
// Uses values > 32767 to prove they're not I16

test "U16: plus" {
    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 40000
        \\    b : U16
        \\    b = 20000
        \\    a + b
        \\}
    , 60000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 65535
        \\    b : U16
        \\    b = 0
        \\    a + b
        \\}
    , 65535, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 32768
        \\    b : U16
        \\    b = 32767
        \\    a + b
        \\}
    , 65535, .no_trace);
}

test "U16: minus" {
    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 50000
        \\    b : U16
        \\    b = 10000
        \\    a - b
        \\}
    , 40000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 65535
        \\    b : U16
        \\    b = 30000
        \\    a - b
        \\}
    , 35535, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 50000
        \\    b : U16
        \\    b = 50000
        \\    a - b
        \\}
    , 0, .no_trace);
}

test "U16: times" {
    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 256
        \\    b : U16
        \\    b = 255
        \\    a * b
        \\}
    , 65280, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 32768
        \\    b : U16
        \\    b = 1
        \\    a * b
        \\}
    , 32768, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 255
        \\    b : U16
        \\    b = 256
        \\    a * b
        \\}
    , 65280, .no_trace);
}

test "U16: div_by" {
    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 60000
        \\    b : U16
        \\    b = 3
        \\    a // b
        \\}
    , 20000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 65535
        \\    b : U16
        \\    b = 257
        \\    a // b
        \\}
    , 255, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 40000
        \\    b : U16
        \\    b = 128
        \\    a // b
        \\}
    , 312, .no_trace);
}

test "U16: rem_by" {
    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 50000
        \\    b : U16
        \\    b = 128
        \\    a % b
        \\}
    , 80, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 65535
        \\    b : U16
        \\    b = 256
        \\    a % b
        \\}
    , 255, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U16
        \\    a = 40000
        \\    b : U16
        \\    b = 99
        \\    a % b
        \\}
    , 4, .no_trace);
}

// U32 Tests (Unsigned 32-bit: 0 to 4294967295)
// Uses values > 2147483647 to prove they're not I32

test "U32: plus" {
    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 3000000000
        \\    b : U32
        \\    b = 1000000000
        \\    a + b
        \\}
    , 4000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 2147483648
        \\    b : U32
        \\    b = 2147483647
        \\    a + b
        \\}
    , 4294967295, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 4294967295
        \\    b : U32
        \\    b = 0
        \\    a + b
        \\}
    , 4294967295, .no_trace);
}

test "U32: minus" {
    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 3000000000
        \\    b : U32
        \\    b = 1000000000
        \\    a - b
        \\}
    , 2000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 4294967295
        \\    b : U32
        \\    b = 2147483648
        \\    a - b
        \\}
    , 2147483647, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 3000000000
        \\    b : U32
        \\    b = 3000000000
        \\    a - b
        \\}
    , 0, .no_trace);
}

test "U32: times" {
    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 65536
        \\    b : U32
        \\    b = 65535
        \\    a * b
        \\}
    , 4294901760, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 2147483648
        \\    b : U32
        \\    b = 1
        \\    a * b
        \\}
    , 2147483648, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 1000000
        \\    b : U32
        \\    b = 4294
        \\    a * b
        \\}
    , 4294000000, .no_trace);
}

test "U32: div_by" {
    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 4000000000
        \\    b : U32
        \\    b = 1000
        \\    a // b
        \\}
    , 4000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 4294967295
        \\    b : U32
        \\    b = 65536
        \\    a // b
        \\}
    , 65535, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 3000000000
        \\    b : U32
        \\    b = 128
        \\    a // b
        \\}
    , 23437500, .no_trace);
}

test "U32: rem_by" {
    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 3000000000
        \\    b : U32
        \\    b = 128
        \\    a % b
        \\}
    , 0, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 4294967295
        \\    b : U32
        \\    b = 65536
        \\    a % b
        \\}
    , 65535, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U32
        \\    a = 2147483648
        \\    b : U32
        \\    b = 99
        \\    a % b
        \\}
    , 2, .no_trace);
}

// U64 Tests (Unsigned 64-bit: 0 to 18446744073709551615)
// Uses values > 9223372036854775807 to prove they're not I64

test "U64: plus" {
    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 10000000000000000000
        \\    b : U64
        \\    b = 5000000000000000000
        \\    a + b
        \\}
    , 15000000000000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 9223372036854775808
        \\    b : U64
        \\    b = 9223372036854775807
        \\    a + b
        \\}
    , 18446744073709551615, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 18446744073709551615
        \\    b : U64
        \\    b = 0
        \\    a + b
        \\}
    , 18446744073709551615, .no_trace);
}

test "U64: minus" {
    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 15000000000000000000
        \\    b : U64
        \\    b = 5000000000000000000
        \\    a - b
        \\}
    , 10000000000000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 18446744073709551615
        \\    b : U64
        \\    b = 9223372036854775808
        \\    a - b
        \\}
    , 9223372036854775807, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 12000000000000000000
        \\    b : U64
        \\    b = 12000000000000000000
        \\    a - b
        \\}
    , 0, .no_trace);
}

test "U64: times" {
    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 4294967296
        \\    b : U64
        \\    b = 4294967295
        \\    a * b
        \\}
    , 18446744069414584320, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 9223372036854775808
        \\    b : U64
        \\    b = 1
        \\    a * b
        \\}
    , 9223372036854775808, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 1000000000
        \\    b : U64
        \\    b = 10000000000
        \\    a * b
        \\}
    , 10000000000000000000, .no_trace);
}

test "U64: div_by" {
    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 15000000000000000000
        \\    b : U64
        \\    b = 1000000
        \\    a // b
        \\}
    , 15000000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 18446744073709551615
        \\    b : U64
        \\    b = 4294967296
        \\    a // b
        \\}
    , 4294967295, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 10000000000000000000
        \\    b : U64
        \\    b = 256
        \\    a // b
        \\}
    , 39062500000000000, .no_trace);
}

test "U64: rem_by" {
    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 10000000000000000000
        \\    b : U64
        \\    b = 256
        \\    a % b
        \\}
    , 0, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 18446744073709551615
        \\    b : U64
        \\    b = 4294967296
        \\    a % b
        \\}
    , 4294967295, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U64
        \\    a = 9223372036854775808
        \\    b : U64
        \\    b = 99
        \\    a % b
        \\}
    , 8, .no_trace);
}

// U128 Tests (Unsigned 128-bit: 0 to 340282366920938463463374607431768211455)
// Uses values > max U64 to prove they're not U64

test "U128: plus" {
    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 100000000000000000000000000000
        \\    b : U128
        \\    b = 50000000000000000000000000000
        \\    a + b
        \\}
    , 150000000000000000000000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 18446744073709551616
        \\    b : U128
        \\    b = 18446744073709551615
        \\    a + b
        \\}
    , 36893488147419103231, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 170141183460469231731687303715884105727
        \\    b : U128
        \\    b = 0
        \\    a + b
        \\}
    , 170141183460469231731687303715884105727, .no_trace);
}

test "U128: minus" {
    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 150000000000000000000000000000
        \\    b : U128
        \\    b = 50000000000000000000000000000
        \\    a - b
        \\}
    , 100000000000000000000000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 36893488147419103231
        \\    b : U128
        \\    b = 18446744073709551616
        \\    a - b
        \\}
    , 18446744073709551615, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 100000000000000000000000000000
        \\    b : U128
        \\    b = 100000000000000000000000000000
        \\    a - b
        \\}
    , 0, .no_trace);
}

test "U128: times" {
    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 13043817825332782212
        \\    b : U128
        \\    b = 13043817825332782212
        \\    a * b
        \\}
    , 170141183460469231722567801800623612944, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 10000000000000000000
        \\    b : U128
        \\    b = 10000000000000000000
        \\    a * b
        \\}
    , 100000000000000000000000000000000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 1000000000000000000000
        \\    b : U128
        \\    b = 1000000
        \\    a * b
        \\}
    , 1000000000000000000000000000, .no_trace);
}

test "U128: div_by" {
    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 100000000000000000000000000000
        \\    b : U128
        \\    b = 10000000000000000
        \\    a // b
        \\}
    , 10000000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 170141183460469231722567801800623612944
        \\    b : U128
        \\    b = 13043817825332782212
        \\    a // b
        \\}
    , 13043817825332782212, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 36893488147419103231
        \\    b : U128
        \\    b = 256
        \\    a // b
        \\}
    , 144115188075855871, .no_trace);
}

test "U128: rem_by" {
    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 100000000000000000000000000000
        \\    b : U128
        \\    b = 99
        \\    a % b
        \\}
    , 10, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 170141183460469231722567801800623612944
        \\    b : U128
        \\    b = 13043817825332782212
        \\    a % b
        \\}
    , 0, .no_trace);

    try runExpectI64(
        \\{
        \\    a : U128
        \\    a = 36893488147419103231
        \\    b : U128
        \\    b = 256
        \\    a % b
        \\}
    , 255, .no_trace);
}

// I8 Tests (Signed 8-bit: -128 to 127)
// Uses negative numbers to prove they're signed

test "I8: negate" {
    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = -127
        \\    -a
        \\}
    , 127, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = 127
        \\    -a
        \\}
    , -127, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = -50
        \\    -a
        \\}
    , 50, .no_trace);
}

test "I8: plus" {
    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = -100
        \\    b : I8
        \\    b = -20
        \\    a + b
        \\}
    , -120, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = -50
        \\    b : I8
        \\    b = 70
        \\    a + b
        \\}
    , 20, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = 127
        \\    b : I8
        \\    b = 0
        \\    a + b
        \\}
    , 127, .no_trace);
}

test "I8: minus" {
    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = -50
        \\    b : I8
        \\    b = 70
        \\    a - b
        \\}
    , -120, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = 100
        \\    b : I8
        \\    b = -27
        \\    a - b
        \\}
    , 127, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = -64
        \\    b : I8
        \\    b = -64
        \\    a - b
        \\}
    , 0, .no_trace);
}

test "I8: times" {
    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = -16
        \\    b : I8
        \\    b = 8
        \\    a * b
        \\}
    , -128, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = -10
        \\    b : I8
        \\    b = -10
        \\    a * b
        \\}
    , 100, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = 127
        \\    b : I8
        \\    b = 1
        \\    a * b
        \\}
    , 127, .no_trace);
}

test "I8: div_by" {
    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = -128
        \\    b : I8
        \\    b = 2
        \\    a // b
        \\}
    , -64, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = 127
        \\    b : I8
        \\    b = -1
        \\    a // b
        \\}
    , -127, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = -100
        \\    b : I8
        \\    b = -10
        \\    a // b
        \\}
    , 10, .no_trace);
}

test "I8: rem_by" {
    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = -128
        \\    b : I8
        \\    b = 7
        \\    a % b
        \\}
    , -2, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = 127
        \\    b : I8
        \\    b = -10
        \\    a % b
        \\}
    , 7, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I8
        \\    a = -100
        \\    b : I8
        \\    b = -7
        \\    a % b
        \\}
    , -2, .no_trace);
}

// I16 Tests (Signed 16-bit: -32768 to 32767)
// Uses values < -128 or operations producing such values to prove they're not I8

test "I16: negate" {
    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = -32767
        \\    -a
        \\}
    , 32767, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = 32767
        \\    -a
        \\}
    , -32767, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = -10000
        \\    -a
        \\}
    , 10000, .no_trace);
}

test "I16: plus" {
    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = -20000
        \\    b : I16
        \\    b = -10000
        \\    a + b
        \\}
    , -30000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = -32768
        \\    b : I16
        \\    b = 32767
        \\    a + b
        \\}
    , -1, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = 32767
        \\    b : I16
        \\    b = 0
        \\    a + b
        \\}
    , 32767, .no_trace);
}

test "I16: minus" {
    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = -10000
        \\    b : I16
        \\    b = 20000
        \\    a - b
        \\}
    , -30000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = 30000
        \\    b : I16
        \\    b = -2767
        \\    a - b
        \\}
    , 32767, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = -16384
        \\    b : I16
        \\    b = -16384
        \\    a - b
        \\}
    , 0, .no_trace);
}

test "I16: times" {
    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = -256
        \\    b : I16
        \\    b = 128
        \\    a * b
        \\}
    , -32768, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = -100
        \\    b : I16
        \\    b = -327
        \\    a * b
        \\}
    , 32700, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = 181
        \\    b : I16
        \\    b = 181
        \\    a * b
        \\}
    , 32761, .no_trace);
}

test "I16: div_by" {
    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = -32768
        \\    b : I16
        \\    b = 2
        \\    a // b
        \\}
    , -16384, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = 32767
        \\    b : I16
        \\    b = -1
        \\    a // b
        \\}
    , -32767, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = -30000
        \\    b : I16
        \\    b = -10
        \\    a // b
        \\}
    , 3000, .no_trace);
}

test "I16: rem_by" {
    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = -32768
        \\    b : I16
        \\    b = 99
        \\    a % b
        \\}
    , -98, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = 32767
        \\    b : I16
        \\    b = -100
        \\    a % b
        \\}
    , 67, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I16
        \\    a = -10000
        \\    b : I16
        \\    b = -128
        \\    a % b
        \\}
    , -16, .no_trace);
}

// I32 Tests (Signed 32-bit: -2147483648 to 2147483647)
// Uses values < -32768 to prove they're not I16

test "I32: negate" {
    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = -2147483647
        \\    -a
        \\}
    , 2147483647, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = 2147483647
        \\    -a
        \\}
    , -2147483647, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = -1000000000
        \\    -a
        \\}
    , 1000000000, .no_trace);
}

test "I32: plus" {
    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = -1000000000
        \\    b : I32
        \\    b = -500000000
        \\    a + b
        \\}
    , -1500000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = -2147483648
        \\    b : I32
        \\    b = 2147483647
        \\    a + b
        \\}
    , -1, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = 2147483647
        \\    b : I32
        \\    b = 0
        \\    a + b
        \\}
    , 2147483647, .no_trace);
}

test "I32: minus" {
    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = -1000000000
        \\    b : I32
        \\    b = 500000000
        \\    a - b
        \\}
    , -1500000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = 2000000000
        \\    b : I32
        \\    b = -147483647
        \\    a - b
        \\}
    , 2147483647, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = -1073741824
        \\    b : I32
        \\    b = -1073741824
        \\    a - b
        \\}
    , 0, .no_trace);
}

test "I32: times" {
    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = -65536
        \\    b : I32
        \\    b = 32768
        \\    a * b
        \\}
    , -2147483648, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = -10000
        \\    b : I32
        \\    b = -214748
        \\    a * b
        \\}
    , 2147480000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = 46340
        \\    b : I32
        \\    b = 46340
        \\    a * b
        \\}
    , 2147395600, .no_trace);
}

test "I32: div_by" {
    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = -2147483648
        \\    b : I32
        \\    b = 2
        \\    a // b
        \\}
    , -1073741824, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = 2147483647
        \\    b : I32
        \\    b = -1
        \\    a // b
        \\}
    , -2147483647, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = -1500000000
        \\    b : I32
        \\    b = -1000
        \\    a // b
        \\}
    , 1500000, .no_trace);
}

test "I32: rem_by" {
    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = -2147483648
        \\    b : I32
        \\    b = 99
        \\    a % b
        \\}
    , -2, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = 2147483647
        \\    b : I32
        \\    b = -65536
        \\    a % b
        \\}
    , 65535, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I32
        \\    a = -1000000000
        \\    b : I32
        \\    b = -32768
        \\    a % b
        \\}
    , -18944, .no_trace);
}

// I64 Tests (Signed 64-bit: -9223372036854775808 to 9223372036854775807)
// Uses values < -2147483648 to prove they're not I32

test "I64: negate" {
    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = -9223372036854775807
        \\    -a
        \\}
    , 9223372036854775807, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = 9223372036854775807
        \\    -a
        \\}
    , -9223372036854775807, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = -5000000000000
        \\    -a
        \\}
    , 5000000000000, .no_trace);
}

test "I64: plus" {
    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = -5000000000000
        \\    b : I64
        \\    b = -3000000000000
        \\    a + b
        \\}
    , -8000000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = -9223372036854775808
        \\    b : I64
        \\    b = 9223372036854775807
        \\    a + b
        \\}
    , -1, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = 9223372036854775807
        \\    b : I64
        \\    b = 0
        \\    a + b
        \\}
    , 9223372036854775807, .no_trace);
}

test "I64: minus" {
    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = -5000000000000
        \\    b : I64
        \\    b = 3000000000000
        \\    a - b
        \\}
    , -8000000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = 9000000000000000000
        \\    b : I64
        \\    b = -223372036854775807
        \\    a - b
        \\}
    , 9223372036854775807, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = -4611686018427387904
        \\    b : I64
        \\    b = -4611686018427387904
        \\    a - b
        \\}
    , 0, .no_trace);
}

test "I64: times" {
    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = -4294967296
        \\    b : I64
        \\    b = 2147483648
        \\    a * b
        \\}
    , -9223372036854775808, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = -1000000000
        \\    b : I64
        \\    b = -9223372
        \\    a * b
        \\}
    , 9223372000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = 3037000499
        \\    b : I64
        \\    b = 3037000499
        \\    a * b
        \\}
    , 9223372030926249001, .no_trace);
}

test "I64: div_by" {
    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = -9223372036854775808
        \\    b : I64
        \\    b = 2
        \\    a // b
        \\}
    , -4611686018427387904, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = 9223372036854775807
        \\    b : I64
        \\    b = -1
        \\    a // b
        \\}
    , -9223372036854775807, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = -8000000000000
        \\    b : I64
        \\    b = -1000000
        \\    a // b
        \\}
    , 8000000, .no_trace);
}

test "I64: rem_by" {
    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = -9223372036854775808
        \\    b : I64
        \\    b = 99
        \\    a % b
        \\}
    , -8, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = 9223372036854775807
        \\    b : I64
        \\    b = -4294967296
        \\    a % b
        \\}
    , 4294967295, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I64
        \\    a = -5000000000000
        \\    b : I64
        \\    b = -2147483648
        \\    a % b
        \\}
    , -658067456, .no_trace);
}

// I128 Tests (Signed 128-bit: -170141183460469231731687303715884105728 to 170141183460469231731687303715884105727)
// Uses values < min I64 to prove they're not I64

test "I128: negate" {
    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = -85070591730234615865843651857942052864
        \\    -a
        \\}
    , 85070591730234615865843651857942052864, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = 170141183460469231731687303715884105727
        \\    -a
        \\}
    , -170141183460469231731687303715884105727, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = -100000000000000000000000
        \\    -a
        \\}
    , 100000000000000000000000, .no_trace);
}

test "I128: plus" {
    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = -100000000000000000000000
        \\    b : I128
        \\    b = -50000000000000000000000
        \\    a + b
        \\}
    , -150000000000000000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = -170141183460469231731687303715884105728
        \\    b : I128
        \\    b = 170141183460469231731687303715884105727
        \\    a + b
        \\}
    , -1, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = 170141183460469231731687303715884105727
        \\    b : I128
        \\    b = 0
        \\    a + b
        \\}
    , 170141183460469231731687303715884105727, .no_trace);
}

test "I128: minus" {
    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = -100000000000000000000000
        \\    b : I128
        \\    b = 50000000000000000000000
        \\    a - b
        \\}
    , -150000000000000000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = 85070591730234615865843651857942052863
        \\    b : I128
        \\    b = -1
        \\    a - b
        \\}
    , 85070591730234615865843651857942052864, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = -85070591730234615865843651857942052864
        \\    b : I128
        \\    b = -85070591730234615865843651857942052864
        \\    a - b
        \\}
    , 0, .no_trace);
}

test "I128: times" {
    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = -18446744073709551616
        \\    b : I128
        \\    b = 9223372036854775808
        \\    a * b
        \\}
    , -170141183460469231731687303715884105728, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = -10000000000000000000
        \\    b : I128
        \\    b = -17014118346
        \\    a * b
        \\}
    , 170141183460000000000000000000, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = 13043817825332782212
        \\    b : I128
        \\    b = 13043817825332782212
        \\    a * b
        \\}
    , 170141183460469231722567801800623612944, .no_trace);
}

test "I128: div_by" {
    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = -170141183460469231731687303715884105728
        \\    b : I128
        \\    b = 2
        \\    a // b
        \\}
    , -85070591730234615865843651857942052864, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = 170141183460469231731687303715884105727
        \\    b : I128
        \\    b = -1
        \\    a // b
        \\}
    , -170141183460469231731687303715884105727, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = -100000000000000000000000
        \\    b : I128
        \\    b = -10000000000
        \\    a // b
        \\}
    , 10000000000000, .no_trace);
}

test "I128: rem_by" {
    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = -170141183460469231731687303715884105728
        \\    b : I128
        \\    b = 99
        \\    a % b
        \\}
    , -29, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = 170141183460469231731687303715884105727
        \\    b : I128
        \\    b = -18446744073709551616
        \\    a % b
        \\}
    , 18446744073709551615, .no_trace);

    try runExpectI64(
        \\{
        \\    a : I128
        \\    a = -100000000000000000000000
        \\    b : I128
        \\    b = -9223372036854775808
        \\    a % b
        \\}
    , -200376420520689664, .no_trace);
}

// NOTE: F32, F64, and Dec Tests
//
// Floating-point and decimal arithmetic tests are not yet implemented because
// the interpreter does not currently support arithmetic operations on fractional
// number types (F32, F64, Dec).
//
// When floating-point arithmetic is implemented in the interpreter, tests should
// be added here following the same pattern as the integer tests above, using the
// runExpectF32() and runExpectF64() helper functions that have been added to
// helpers.zig.
//
// The StackValue module already has asF32(), asF64(), and asDec() methods
// available for reading floating-point values.
//
// Example test structure (currently commented out):
//
// test "F32: negate" {
//     try runExpectF32(
//         \\{
//         \\    a : F32
//         \\    a = 3.14
//         \\    -a
//         \\}
//     , -3.14, .no_trace);
// }
//
// F32 Tests (32-bit floating point)

test "F32: literal only" {
    // Simplest possible F32 test - just return a literal
    try runExpectF32("3.14f32", 3.14, .no_trace);
}

test "F32: variable assignment" {
    // Test F32 variable assignment without any operations
    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = 3.14f32
        \\    a
        \\}
    , 3.14, .no_trace);
}

test "F32: negate" {
    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = 3.14f32
        \\    -a
        \\}
    , -3.14, .no_trace);
}

test "F32: plus" {
    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = 1.5f32
        \\    b : F32
        \\    b = 2.5f32
        \\    a + b
        \\}
    , 4.0, .no_trace);

    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = 3.14159f32
        \\    b : F32
        \\    b = 2.71828f32
        \\    a + b
        \\}
    , 5.85987, .no_trace);

    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = -10.5f32
        \\    b : F32
        \\    b = 10.5f32
        \\    a + b
        \\}
    , 0.0, .no_trace);
}

test "F32: minus" {
    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = 10.0f32
        \\    b : F32
        \\    b = 3.5f32
        \\    a - b
        \\}
    , 6.5, .no_trace);

    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = 2.5f32
        \\    b : F32
        \\    b = 5.0f32
        \\    a - b
        \\}
    , -2.5, .no_trace);

    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = 100.0f32
        \\    b : F32
        \\    b = 100.0f32
        \\    a - b
        \\}
    , 0.0, .no_trace);
}

test "F32: times" {
    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = 2.5f32
        \\    b : F32
        \\    b = 4.0f32
        \\    a * b
        \\}
    , 10.0, .no_trace);

    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = -3.0f32
        \\    b : F32
        \\    b = 2.5f32
        \\    a * b
        \\}
    , -7.5, .no_trace);

    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = 0.5f32
        \\    b : F32
        \\    b = 0.5f32
        \\    a * b
        \\}
    , 0.25, .no_trace);
}

test "F32: div_by" {
    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = 10.0f32
        \\    b : F32
        \\    b = 2.0f32
        \\    a / b
        \\}
    , 5.0, .no_trace);

    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = 7.5f32
        \\    b : F32
        \\    b = 2.5f32
        \\    a / b
        \\}
    , 3.0, .no_trace);

    try runExpectF32(
        \\{
        \\    a : F32
        \\    a = 1.0f32
        \\    b : F32
        \\    b = 3.0f32
        \\    a / b
        \\}
    , 0.3333333, .no_trace);
}

// F64 Tests (64-bit floating point)

test "F64: negate" {
    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = 3.141592653589793f64
        \\    -a
        \\}
    , -3.141592653589793, .no_trace);

    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = -2.718281828459045f64
        \\    -a
        \\}
    , 2.718281828459045, .no_trace);

    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = 0.0f64
        \\    -a
        \\}
    , 0.0, .no_trace);
}

test "F64: plus" {
    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = 1.5f64
        \\    b : F64
        \\    b = 2.5f64
        \\    a + b
        \\}
    , 4.0, .no_trace);

    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = 3.141592653589793f64
        \\    b : F64
        \\    b = 2.718281828459045f64
        \\    a + b
        \\}
    , 5.859874482048838, .no_trace);

    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = -100.123456789f64
        \\    b : F64
        \\    b = 100.123456789f64
        \\    a + b
        \\}
    , 0.0, .no_trace);
}

test "F64: minus" {
    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = 10.5f64
        \\    b : F64
        \\    b = 3.25f64
        \\    a - b
        \\}
    , 7.25, .no_trace);

    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = 2.5f64
        \\    b : F64
        \\    b = 5.75f64
        \\    a - b
        \\}
    , -3.25, .no_trace);

    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = 1000.0f64
        \\    b : F64
        \\    b = 1000.0f64
        \\    a - b
        \\}
    , 0.0, .no_trace);
}

test "F64: times" {
    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = 2.5f64
        \\    b : F64
        \\    b = 4.0f64
        \\    a * b
        \\}
    , 10.0, .no_trace);

    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = -3.5f64
        \\    b : F64
        \\    b = 2.0f64
        \\    a * b
        \\}
    , -7.0, .no_trace);

    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = 1.414213562373095f64
        \\    b : F64
        \\    b = 1.414213562373095f64
        \\    a * b
        \\}
    , 2.0, .no_trace);
}

test "F64: div_by" {
    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = 10.0f64
        \\    b : F64
        \\    b = 2.0f64
        \\    a / b
        \\}
    , 5.0, .no_trace);

    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = 22.0f64
        \\    b : F64
        \\    b = 7.0f64
        \\    a / b
        \\}
    , 3.142857142857143, .no_trace);

    try runExpectF64(
        \\{
        \\    a : F64
        \\    a = 1.0f64
        \\    b : F64
        \\    b = 3.0f64
        \\    a / b
        \\}
    , 0.3333333333333333, .no_trace);
}

// Dec Tests (Fixed-point decimal: 18 decimal places precision)
// Dec is stored as i128 with 18 decimal places (10^18 = 1.0)

test "Dec: negate" {
    // 3.14dec stored as 3.14 * 10^18 = 3140000000000000000
    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = 3.14dec
        \\    -a
        \\}
    , -3140000000000000000, .no_trace);

    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = -2.5dec
        \\    -a
        \\}
    , 2500000000000000000, .no_trace);

    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = 0.0dec
        \\    -a
        \\}
    , 0, .no_trace);
}

test "Dec: plus" {
    // 1.5dec + 2.5dec = 4.0dec
    // Stored as: 1500000000000000000 + 2500000000000000000 = 4000000000000000000
    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = 1.5dec
        \\    b : Dec
        \\    b = 2.5dec
        \\    a + b
        \\}
    , 4000000000000000000, .no_trace);

    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = 3.14159dec
        \\    b : Dec
        \\    b = 2.71828dec
        \\    a + b
        \\}
    , 5859870000000000000, .no_trace);

    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = -10.5dec
        \\    b : Dec
        \\    b = 10.5dec
        \\    a + b
        \\}
    , 0, .no_trace);
}

test "Dec: minus" {
    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = 10.0dec
        \\    b : Dec
        \\    b = 3.5dec
        \\    a - b
        \\}
    , 6500000000000000000, .no_trace);

    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = 2.5dec
        \\    b : Dec
        \\    b = 5.0dec
        \\    a - b
        \\}
    , -2500000000000000000, .no_trace);

    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = 100.0dec
        \\    b : Dec
        \\    b = 100.0dec
        \\    a - b
        \\}
    , 0, .no_trace);
}

test "Dec: times" {
    // 2.5dec * 4.0dec = 10.0dec
    // In fixed-point: (2.5 * 10^18) * (4.0 * 10^18) / 10^18 = 10.0 * 10^18
    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = 2.5dec
        \\    b : Dec
        \\    b = 4.0dec
        \\    a * b
        \\}
    , 10000000000000000000, .no_trace);

    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = -3.0dec
        \\    b : Dec
        \\    b = 2.5dec
        \\    a * b
        \\}
    , -7500000000000000000, .no_trace);

    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = 0.5dec
        \\    b : Dec
        \\    b = 0.5dec
        \\    a * b
        \\}
    , 250000000000000000, .no_trace);
}

test "Dec: div_by" {
    // 10.0dec / 2.0dec = 5.0dec
    // In fixed-point: (10.0 * 10^18 * 10^18) / (2.0 * 10^18) = 5.0 * 10^18
    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = 10.0dec
        \\    b : Dec
        \\    b = 2.0dec
        \\    a / b
        \\}
    , 5000000000000000000, .no_trace);

    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = 7.5dec
        \\    b : Dec
        \\    b = 2.5dec
        \\    a / b
        \\}
    , 3000000000000000000, .no_trace);

    try runExpectDec(
        \\{
        \\    a : Dec
        \\    a = 1.0dec
        \\    b : Dec
        \\    b = 3.0dec
        \\    a / b
        \\}
    , 333333333333333333, .no_trace);
}

// Dec: to_str

test "Dec: to_str" {
    // Simple whole number
    try runExpectStr(
        \\{
        \\    a : Dec
        \\    a = 100.0dec
        \\    Dec.to_str(a)
        \\}
    , "100.0", .no_trace);

    // Positive decimal
    try runExpectStr(
        \\{
        \\    a : Dec
        \\    a = 123.45dec
        \\    Dec.to_str(a)
        \\}
    , "123.45", .no_trace);

    // Negative decimal
    try runExpectStr(
        \\{
        \\    a : Dec
        \\    a = -123.45dec
        \\    Dec.to_str(a)
        \\}
    , "-123.45", .no_trace);

    // Whole number without trailing zeros in decimal part
    try runExpectStr(
        \\{
        \\    a : Dec
        \\    a = 123.0dec
        \\    Dec.to_str(a)
        \\}
    , "123.0", .no_trace);

    // Negative whole number
    try runExpectStr(
        \\{
        \\    a : Dec
        \\    a = -123.0dec
        \\    Dec.to_str(a)
        \\}
    , "-123.0", .no_trace);

    // Decimal less than 1
    try runExpectStr(
        \\{
        \\    a : Dec
        \\    a = 0.45dec
        \\    Dec.to_str(a)
        \\}
    , "0.45", .no_trace);

    // Negative decimal less than 1
    try runExpectStr(
        \\{
        \\    a : Dec
        \\    a = -0.45dec
        \\    Dec.to_str(a)
        \\}
    , "-0.45", .no_trace);

    // Zero
    try runExpectStr(
        \\{
        \\    a : Dec
        \\    a = 0.0dec
        \\    Dec.to_str(a)
        \\}
    , "0.0", .no_trace);
}

// Mixed Dec-Int Operations
// These tests verify that mixing Dec and Int types produces a TYPE MISMATCH error
// at compile time, and crashes at runtime. Roc requires explicit type conversions.

// Dec + Int: Should be a type mismatch - Dec and I64 are different types
test "Dec + Int: plus - type mismatch" {
    // TODO: Re-enable when error-type-to-runtime-crash pass is implemented
    return error.SkipZigTest;
}

test "Dec + Int: minus - type mismatch" {
    // TODO: Re-enable when error-type-to-runtime-crash pass is implemented
    return error.SkipZigTest;
}

test "Dec + Int: times - type mismatch" {
    // TODO: Re-enable when error-type-to-runtime-crash pass is implemented
    return error.SkipZigTest;
}

test "Dec + Int: div_by - type mismatch" {
    // TODO: Re-enable when error-type-to-runtime-crash pass is implemented
    return error.SkipZigTest;
}

// Int + Dec: Should be a type mismatch - I64 and Dec are different types
test "Int + Dec: plus - type mismatch" {
    // TODO: Re-enable when error-type-to-runtime-crash pass is implemented
    return error.SkipZigTest;
}

test "Int + Dec: minus - type mismatch" {
    // TODO: Re-enable when error-type-to-runtime-crash pass is implemented
    return error.SkipZigTest;
}

test "Int + Dec: times - type mismatch" {
    // TODO: Re-enable when error-type-to-runtime-crash pass is implemented
    return error.SkipZigTest;
}

test "Int + Dec: div_by - type mismatch" {
    // TODO: Re-enable when error-type-to-runtime-crash pass is implemented
    return error.SkipZigTest;
}
