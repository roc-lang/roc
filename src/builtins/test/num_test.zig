//! Unit tests for numeric builtin functions

const std = @import("std");
const builtins = @import("builtins");

const TestEnv = builtins.utils.TestEnv;
const NumParseResult = builtins.num.NumParseResult;
const addWithOverflow = builtins.num.addWithOverflow;

test "parseIntFromStr decimal parsing" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test successful decimal parsing
    const valid_str = builtins.str.RocStr.fromSlice("42", test_env.getOps());
    defer valid_str.decref(test_env.getOps());

    const result = builtins.num.parseIntFromStr(i32, valid_str);
    try std.testing.expectEqual(@as(i32, 42), result.value);
    try std.testing.expectEqual(@as(u8, 0), result.errorcode);

    // Test negative number
    const neg_str = builtins.str.RocStr.fromSlice("-123", test_env.getOps());
    defer neg_str.decref(test_env.getOps());

    const neg_result = builtins.num.parseIntFromStr(i32, neg_str);
    try std.testing.expectEqual(@as(i32, -123), neg_result.value);
    try std.testing.expectEqual(@as(u8, 0), neg_result.errorcode);

    // Test zero
    const zero_str = builtins.str.RocStr.fromSlice("0", test_env.getOps());
    defer zero_str.decref(test_env.getOps());

    const zero_result = builtins.num.parseIntFromStr(i32, zero_str);
    try std.testing.expectEqual(@as(i32, 0), zero_result.value);
    try std.testing.expectEqual(@as(u8, 0), zero_result.errorcode);
}

test "parseIntFromStr hex and binary parsing" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test hexadecimal parsing
    const hex_str = builtins.str.RocStr.fromSlice("0xFF", test_env.getOps());
    defer hex_str.decref(test_env.getOps());

    const hex_result = builtins.num.parseIntFromStr(i32, hex_str);
    try std.testing.expectEqual(@as(i32, 255), hex_result.value);
    try std.testing.expectEqual(@as(u8, 0), hex_result.errorcode);

    // Test binary parsing
    const bin_str = builtins.str.RocStr.fromSlice("0b1010", test_env.getOps());
    defer bin_str.decref(test_env.getOps());

    const bin_result = builtins.num.parseIntFromStr(i32, bin_str);
    try std.testing.expectEqual(@as(i32, 10), bin_result.value);
    try std.testing.expectEqual(@as(u8, 0), bin_result.errorcode);

    // Test octal parsing
    const oct_str = builtins.str.RocStr.fromSlice("0o755", test_env.getOps());
    defer oct_str.decref(test_env.getOps());

    const oct_result = builtins.num.parseIntFromStr(i32, oct_str);
    try std.testing.expectEqual(@as(i32, 493), oct_result.value); // 7*64 + 5*8 + 5 = 493
    try std.testing.expectEqual(@as(u8, 0), oct_result.errorcode);
}

test "parseIntFromStr error cases" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test invalid string
    const invalid_str = builtins.str.RocStr.fromSlice("not_a_number", test_env.getOps());
    defer invalid_str.decref(test_env.getOps());

    const invalid_result = builtins.num.parseIntFromStr(i32, invalid_str);
    try std.testing.expectEqual(@as(i32, 0), invalid_result.value);
    try std.testing.expectEqual(@as(u8, 1), invalid_result.errorcode);

    // Test empty string
    const empty_str = builtins.str.RocStr.fromSlice("", test_env.getOps());
    defer empty_str.decref(test_env.getOps());

    const empty_result = builtins.num.parseIntFromStr(i32, empty_str);
    try std.testing.expectEqual(@as(i32, 0), empty_result.value);
    try std.testing.expectEqual(@as(u8, 1), empty_result.errorcode);

    // Test overflow (for i8)
    const overflow_str = builtins.str.RocStr.fromSlice("1000", test_env.getOps());
    defer overflow_str.decref(test_env.getOps());

    const overflow_result = builtins.num.parseIntFromStr(i8, overflow_str);
    try std.testing.expectEqual(@as(i8, 0), overflow_result.value);
    try std.testing.expectEqual(@as(u8, 1), overflow_result.errorcode);
}

test "parseFloatFromStr basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test successful float parsing
    const valid_str = builtins.str.RocStr.fromSlice("3.14159", test_env.getOps());
    defer valid_str.decref(test_env.getOps());

    const result = builtins.num.parseFloatFromStr(f32, valid_str);
    try std.testing.expectApproxEqRel(@as(f32, 3.14159), result.value, 0.00001);
    try std.testing.expectEqual(@as(u8, 0), result.errorcode);

    // Test negative float
    const neg_str = builtins.str.RocStr.fromSlice("-42.5", test_env.getOps());
    defer neg_str.decref(test_env.getOps());

    const neg_result = builtins.num.parseFloatFromStr(f64, neg_str);
    try std.testing.expectEqual(@as(f64, -42.5), neg_result.value);
    try std.testing.expectEqual(@as(u8, 0), neg_result.errorcode);

    // Test scientific notation
    const sci_str = builtins.str.RocStr.fromSlice("1.5e2", test_env.getOps());
    defer sci_str.decref(test_env.getOps());

    const sci_result = builtins.num.parseFloatFromStr(f32, sci_str);
    try std.testing.expectEqual(@as(f32, 150.0), sci_result.value);
    try std.testing.expectEqual(@as(u8, 0), sci_result.errorcode);
}

test "parseFloatFromStr error cases" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test invalid string
    const invalid_str = builtins.str.RocStr.fromSlice("not_a_float", test_env.getOps());
    defer invalid_str.decref(test_env.getOps());

    const invalid_result = builtins.num.parseFloatFromStr(f32, invalid_str);
    try std.testing.expectEqual(@as(f32, 0.0), invalid_result.value);
    try std.testing.expectEqual(@as(u8, 1), invalid_result.errorcode);

    // Test empty string
    const empty_str = builtins.str.RocStr.fromSlice("", test_env.getOps());
    defer empty_str.decref(test_env.getOps());

    const empty_result = builtins.num.parseFloatFromStr(f32, empty_str);
    try std.testing.expectEqual(@as(f32, 0.0), empty_result.value);
    try std.testing.expectEqual(@as(u8, 1), empty_result.errorcode);

    // Test malformed decimal
    const malformed_str = builtins.str.RocStr.fromSlice("3.14.15", test_env.getOps());
    defer malformed_str.decref(test_env.getOps());

    const malformed_result = builtins.num.parseFloatFromStr(f32, malformed_str);
    try std.testing.expectEqual(@as(f32, 0.0), malformed_result.value);
    try std.testing.expectEqual(@as(u8, 1), malformed_result.errorcode);
}

test "parseFloatFromStr special values" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Test infinity
    const inf_str = builtins.str.RocStr.fromSlice("inf", test_env.getOps());
    defer inf_str.decref(test_env.getOps());

    const inf_result = builtins.num.parseFloatFromStr(f32, inf_str);
    try std.testing.expect(std.math.isInf(inf_result.value));
    try std.testing.expectEqual(@as(u8, 0), inf_result.errorcode);

    // Test negative infinity
    const neg_inf_str = builtins.str.RocStr.fromSlice("-inf", test_env.getOps());
    defer neg_inf_str.decref(test_env.getOps());

    const neg_inf_result = builtins.num.parseFloatFromStr(f32, neg_inf_str);
    try std.testing.expect(std.math.isNegativeInf(neg_inf_result.value));
    try std.testing.expectEqual(@as(u8, 0), neg_inf_result.errorcode);

    // Test NaN
    const nan_str = builtins.str.RocStr.fromSlice("nan", test_env.getOps());
    defer nan_str.decref(test_env.getOps());

    const nan_result = builtins.num.parseFloatFromStr(f32, nan_str);
    try std.testing.expect(std.math.isNan(nan_result.value));
    try std.testing.expectEqual(@as(u8, 0), nan_result.errorcode);
}

test "addWithOverflow basic functionality" {
    // Test normal addition without overflow
    const result1 = addWithOverflow(i32, 10, 20);
    try std.testing.expectEqual(@as(i32, 30), result1.value);
    try std.testing.expectEqual(false, result1.has_overflowed);

    // Test addition that would overflow
    const result2 = addWithOverflow(i8, 127, 1);
    try std.testing.expectEqual(@as(i8, -128), result2.value); // wraps around
    try std.testing.expectEqual(true, result2.has_overflowed);
}

test "addWithOverflow with floating point" {
    // Test normal floating point addition
    const result1 = addWithOverflow(f32, 1.5, 2.5);
    try std.testing.expectEqual(@as(f32, 4.0), result1.value);
    try std.testing.expectEqual(false, result1.has_overflowed);

    // Test infinite result
    const result2 = addWithOverflow(f32, std.math.floatMax(f32), std.math.floatMax(f32));
    try std.testing.expectEqual(true, result2.has_overflowed);
}

test "subWithOverflow basic functionality" {
    // Test normal subtraction without overflow
    const result1 = builtins.num.subWithOverflow(i32, 30, 10);
    try std.testing.expectEqual(@as(i32, 20), result1.value);
    try std.testing.expectEqual(false, result1.has_overflowed);

    // Test subtraction that would underflow
    const result2 = builtins.num.subWithOverflow(i8, -128, 1);
    try std.testing.expectEqual(@as(i8, 127), result2.value); // wraps around
    try std.testing.expectEqual(true, result2.has_overflowed);
}

test "mulWithOverflow basic functionality" {
    // Test normal multiplication without overflow
    const result1 = builtins.num.mulWithOverflow(i32, 6, 7);
    try std.testing.expectEqual(@as(i32, 42), result1.value);
    try std.testing.expectEqual(false, result1.has_overflowed);

    // Test multiplication that would overflow
    const result2 = builtins.num.mulWithOverflow(i8, 100, 2);
    try std.testing.expectEqual(@as(i8, -56), result2.value); // wraps around
    try std.testing.expectEqual(true, result2.has_overflowed);
}

test "isMultipleOf functionality" {
    // Test basic multiples
    try std.testing.expect(builtins.num.isMultipleOf(i32, 10, 5));
    try std.testing.expect(builtins.num.isMultipleOf(i32, 15, 3));
    try std.testing.expect(!builtins.num.isMultipleOf(i32, 10, 3));

    // Test edge cases
    try std.testing.expect(builtins.num.isMultipleOf(i32, 0, 5)); // 0 is multiple of anything
    try std.testing.expect(builtins.num.isMultipleOf(i32, 5, -1)); // anything is multiple of -1
    try std.testing.expect(builtins.num.isMultipleOf(i32, 0, 0)); // 0 is multiple of 0
    try std.testing.expect(!builtins.num.isMultipleOf(i32, 5, 0)); // 5 is not multiple of 0
}

test "compareI128 functionality" {
    const a: i128 = 1000000000000000000;
    const b: i128 = 2000000000000000000;
    const c: i128 = 1000000000000000000;

    try std.testing.expectEqual(builtins.utils.Ordering.LT, builtins.num.compareI128(a, b));
    try std.testing.expectEqual(builtins.utils.Ordering.GT, builtins.num.compareI128(b, a));
    try std.testing.expectEqual(builtins.utils.Ordering.EQ, builtins.num.compareI128(a, c));
}

test "compareU128 functionality" {
    const a: u128 = 1000000000000000000;
    const b: u128 = 2000000000000000000;
    const c: u128 = 1000000000000000000;

    try std.testing.expectEqual(builtins.utils.Ordering.LT, builtins.num.compareU128(a, b));
    try std.testing.expectEqual(builtins.utils.Ordering.GT, builtins.num.compareU128(b, a));
    try std.testing.expectEqual(builtins.utils.Ordering.EQ, builtins.num.compareU128(a, c));
}

test "128-bit comparison functions" {
    const small: i128 = 100;
    const large: i128 = 200;

    try std.testing.expect(builtins.num.lessThanI128(small, large));
    try std.testing.expect(!builtins.num.lessThanI128(large, small));
    try std.testing.expect(builtins.num.lessThanOrEqualI128(small, large));
    try std.testing.expect(builtins.num.lessThanOrEqualI128(small, small));
    try std.testing.expect(builtins.num.greaterThanI128(large, small));
    try std.testing.expect(!builtins.num.greaterThanI128(small, large));
    try std.testing.expect(builtins.num.greaterThanOrEqualI128(large, small));
    try std.testing.expect(builtins.num.greaterThanOrEqualI128(small, small));
}

test "mul_u128 basic functionality" {
    const a: u128 = 1000000;
    const b: u128 = 2000000;
    const result = builtins.num.mul_u128(a, b);

    // 1000000 * 2000000 = 2000000000000, which fits in u128
    try std.testing.expectEqual(@as(u128, 0), result.hi);
    try std.testing.expectEqual(@as(u128, 2000000000000), result.lo);
}

test "f32ToParts and f32FromParts roundtrip" {
    const values = [_]f32{ 0.0, 1.0, -1.0, 3.14159, -42.5, std.math.inf(f32), -std.math.inf(f32) };

    for (values) |val| {
        if (!std.math.isNan(val)) { // Skip NaN since NaN != NaN
            const parts = builtins.num.f32ToParts(val);
            const reconstructed = builtins.num.f32FromParts(parts);
            try std.testing.expectEqual(val, reconstructed);
        }
    }
}

test "f64ToParts and f64FromParts roundtrip" {
    const values = [_]f64{ 0.0, 1.0, -1.0, 3.141592653589793, -42.5, std.math.inf(f64), -std.math.inf(f64) };

    for (values) |val| {
        if (!std.math.isNan(val)) { // Skip NaN since NaN != NaN
            const parts = builtins.num.f64ToParts(val);
            const reconstructed = builtins.num.f64FromParts(parts);
            try std.testing.expectEqual(val, reconstructed);
        }
    }
}

test "f32ToBits and f32FromBits roundtrip" {
    const values = [_]f32{ 0.0, 1.0, -1.0, 3.14159, -42.5 };

    for (values) |val| {
        const bits = builtins.num.f32ToBits(val);
        const reconstructed = builtins.num.f32FromBits(bits);
        try std.testing.expectEqual(val, reconstructed);
    }
}

test "f64ToBits and f64FromBits roundtrip" {
    const values = [_]f64{ 0.0, 1.0, -1.0, 3.141592653589793, -42.5 };

    for (values) |val| {
        const bits = builtins.num.f64ToBits(val);
        const reconstructed = builtins.num.f64FromBits(bits);
        try std.testing.expectEqual(val, reconstructed);
    }
}

test "f32ToParts specific values" {
    // Test zero
    const zero_parts = builtins.num.f32ToParts(0.0);
    try std.testing.expectEqual(@as(u32, 0), zero_parts.fraction);
    try std.testing.expectEqual(@as(u8, 0), zero_parts.exponent);
    try std.testing.expectEqual(false, zero_parts.sign);

    // Test negative zero
    const neg_zero_parts = builtins.num.f32ToParts(-0.0);
    try std.testing.expectEqual(@as(u32, 0), neg_zero_parts.fraction);
    try std.testing.expectEqual(@as(u8, 0), neg_zero_parts.exponent);
    try std.testing.expectEqual(true, neg_zero_parts.sign);

    // Test 1.0
    const one_parts = builtins.num.f32ToParts(1.0);
    try std.testing.expectEqual(@as(u32, 0), one_parts.fraction);
    try std.testing.expectEqual(@as(u8, 127), one_parts.exponent); // bias is 127
    try std.testing.expectEqual(false, one_parts.sign);
}

test "shiftRightZeroFillI128 basic functionality" {
    // Test normal shift
    const value: i128 = 0x1000;
    const result1 = builtins.num.shiftRightZeroFillI128(value, 4);
    try std.testing.expectEqual(@as(i128, 0x100), result1);

    // Test shift by 0
    const result2 = builtins.num.shiftRightZeroFillI128(value, 0);
    try std.testing.expectEqual(value, result2);

    // Test large shift (should return 0)
    const result3 = builtins.num.shiftRightZeroFillI128(value, 128);
    try std.testing.expectEqual(@as(i128, 0), result3);

    // Test negative value (arithmetic right shift preserves sign bit)
    const neg_value: i128 = -1;
    const result4 = builtins.num.shiftRightZeroFillI128(neg_value, 1);
    try std.testing.expectEqual(@as(i128, -1), result4);
}

test "shiftRightZeroFillU128 basic functionality" {
    // Test normal shift
    const value: u128 = 0x1000;
    const result1 = builtins.num.shiftRightZeroFillU128(value, 4);
    try std.testing.expectEqual(@as(u128, 0x100), result1);

    // Test shift by 0
    const result2 = builtins.num.shiftRightZeroFillU128(value, 0);
    try std.testing.expectEqual(value, result2);

    // Test large shift (should return 0)
    const result3 = builtins.num.shiftRightZeroFillU128(value, 128);
    try std.testing.expectEqual(@as(u128, 0), result3);

    // Test max value
    const max_value: u128 = std.math.maxInt(u128);
    const result4 = builtins.num.shiftRightZeroFillU128(max_value, 1);
    try std.testing.expectEqual(@as(u128, 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF), result4);
}

test "addWithOverflow edge cases" {
    // Test adding to max value
    const result1 = builtins.num.addWithOverflow(i8, std.math.maxInt(i8), 1);
    try std.testing.expectEqual(@as(i8, std.math.minInt(i8)), result1.value);
    try std.testing.expectEqual(true, result1.has_overflowed);

    // Test adding zero (should never overflow)
    const result2 = builtins.num.addWithOverflow(i8, std.math.maxInt(i8), 0);
    try std.testing.expectEqual(@as(i8, std.math.maxInt(i8)), result2.value);
    try std.testing.expectEqual(false, result2.has_overflowed);

    // Test boundary case
    const result3 = builtins.num.addWithOverflow(i8, std.math.maxInt(i8) - 1, 1);
    try std.testing.expectEqual(@as(i8, std.math.maxInt(i8)), result3.value);
    try std.testing.expectEqual(false, result3.has_overflowed);
}

test "subWithOverflow edge cases" {
    // Test subtracting from min value
    const result1 = builtins.num.subWithOverflow(i8, std.math.minInt(i8), 1);
    try std.testing.expectEqual(@as(i8, std.math.maxInt(i8)), result1.value);
    try std.testing.expectEqual(true, result1.has_overflowed);

    // Test subtracting zero (should never overflow)
    const result2 = builtins.num.subWithOverflow(i8, std.math.minInt(i8), 0);
    try std.testing.expectEqual(@as(i8, std.math.minInt(i8)), result2.value);
    try std.testing.expectEqual(false, result2.has_overflowed);

    // Test boundary case
    const result3 = builtins.num.subWithOverflow(i8, std.math.minInt(i8) + 1, 1);
    try std.testing.expectEqual(@as(i8, std.math.minInt(i8)), result3.value);
    try std.testing.expectEqual(false, result3.has_overflowed);
}

test "mulWithOverflow edge cases" {
    // Test multiplying by zero (should never overflow)
    const result1 = builtins.num.mulWithOverflow(i8, std.math.maxInt(i8), 0);
    try std.testing.expectEqual(@as(i8, 0), result1.value);
    try std.testing.expectEqual(false, result1.has_overflowed);

    // Test multiplying by one (should never overflow)
    const result2 = builtins.num.mulWithOverflow(i8, std.math.maxInt(i8), 1);
    try std.testing.expectEqual(@as(i8, std.math.maxInt(i8)), result2.value);
    try std.testing.expectEqual(false, result2.has_overflowed);

    // Test multiplying by -1
    const result3 = builtins.num.mulWithOverflow(i8, std.math.maxInt(i8), -1);
    try std.testing.expectEqual(@as(i8, -std.math.maxInt(i8)), result3.value);
    try std.testing.expectEqual(false, result3.has_overflowed);

    // Test multiplying min value by -1 (should overflow)
    const result4 = builtins.num.mulWithOverflow(i8, std.math.minInt(i8), -1);
    try std.testing.expectEqual(@as(i8, std.math.minInt(i8)), result4.value);
    try std.testing.expectEqual(true, result4.has_overflowed);
}

test "mul_u128 large values" {
    // Test multiplication that would overflow into high bits
    const large1: u128 = 0xFFFFFFFFFFFFFFFF; // max u64
    const large2: u128 = 0xFFFFFFFFFFFFFFFF;
    const result = builtins.num.mul_u128(large1, large2);

    // Verify the actual result values
    try std.testing.expectEqual(@as(u128, 0), result.hi);
    try std.testing.expectEqual(@as(u128, 0xfffffffffffffffe0000000000000001), result.lo);
}

test "mul_u128 overflow into high bits" {
    // Test multiplication that overflows into high bits
    const large1: u128 = 0x10000000000000000; // 2^64
    const large2: u128 = 0x10000000000000000; // 2^64
    const result = builtins.num.mul_u128(large1, large2);

    // 2^64 * 2^64 = 2^128, which should give hi = 1, lo = 0
    try std.testing.expectEqual(@as(u128, 1), result.hi);
    try std.testing.expectEqual(@as(u128, 0), result.lo);
}
