const std = @import("std");

// Copy of the Num.Int.Precision enum
const Precision = enum(u4) {
    u8 = 0,
    i8 = 1,
    u16 = 2,
    i16 = 3,
    u32 = 4,
    i32 = 5,
    u64 = 6,
    i64 = 7,
    u128 = 8,
    i128 = 9,

    /// Determine the precision based on a u128 value and whether it's negated
    pub fn fromU128(u128_val: u128, is_negated: bool) Precision {
        if (is_negated) {
            // For negative values, only signed types are valid
            // We need to check the original u128 value (before negation)
            if (u128_val <= 128) return Precision.i8;
            if (u128_val <= 32768) return Precision.i16;
            if (u128_val <= 2147483648) return Precision.i32;
            if (u128_val <= 9223372036854775808) return Precision.i64;
            return Precision.i128;
        } else {
            // For positive values, prefer signed types when they fit
            if (u128_val <= std.math.maxInt(i8)) return Precision.i8;
            if (u128_val <= std.math.maxInt(u8)) return Precision.u8;
            if (u128_val <= std.math.maxInt(i16)) return Precision.i16;
            if (u128_val <= std.math.maxInt(u16)) return Precision.u16;
            if (u128_val <= std.math.maxInt(i32)) return Precision.i32;
            if (u128_val <= std.math.maxInt(u32)) return Precision.u32;
            if (u128_val <= std.math.maxInt(i64)) return Precision.i64;
            if (u128_val <= std.math.maxInt(u64)) return Precision.u64;
            if (u128_val <= std.math.maxInt(i128)) return Precision.i128;
            return Precision.u128;
        }
    }

    /// Branchless implementation of fromU128
    pub fn fromU128Branchless(u128_val: u128, is_negated: bool) Precision {
        // Special case for 0
        const is_zero = @as(u128, @intFromBool(u128_val == 0));
        const val_or_one = u128_val | is_zero; // Ensure non-zero for @clz

        // Calculate the position of the highest set bit (0-based)
        const highest_bit = @as(u7, 127) - @as(u7, @clz(val_or_one));

        // For negative values, we use different breakpoints
        // The breakpoints are: 128 (2^7), 32768 (2^15), 2147483648 (2^31), 9223372036854775808 (2^63)

        // Create masks for the negative breakpoints
        const neg_i8_ok = @as(u8, @intFromBool(u128_val <= 128));
        const neg_i16_ok = @as(u8, @intFromBool(u128_val <= 32768));
        const neg_i32_ok = @as(u8, @intFromBool(u128_val <= 2147483648));
        const neg_i64_ok = @as(u8, @intFromBool(u128_val <= 9223372036854775808));

        // Sum up to get the precision index for negative values
        const neg_sum = neg_i8_ok + neg_i16_ok + neg_i32_ok + neg_i64_ok;

        // Map the sum to actual precision values for negative
        const neg_precision_map = [5]u4{ 9, 7, 5, 3, 1 }; // i128, i64, i32, i16, i8
        const neg_precision = neg_precision_map[neg_sum];

        // For positive values, create a lookup table based on highest_bit
        // The table maps bit positions to precision values
        // We need 128 entries, but we'll use a compact representation

        // Create masks for each breakpoint
        const lt7 = @as(u8, @intFromBool(highest_bit < 7));   // < 128 -> i8
        const lt8 = @as(u8, @intFromBool(highest_bit < 8));   // < 256 -> u8
        const lt15 = @as(u8, @intFromBool(highest_bit < 15)); // < 32768 -> i16
        const lt16 = @as(u8, @intFromBool(highest_bit < 16)); // < 65536 -> u16
        const lt31 = @as(u8, @intFromBool(highest_bit < 31)); // < 2147483648 -> i32
        const lt32 = @as(u8, @intFromBool(highest_bit < 32)); // < 4294967296 -> u32
        const lt63 = @as(u8, @intFromBool(highest_bit < 63)); // < 9223372036854775808 -> i64
        const lt64 = @as(u8, @intFromBool(highest_bit < 64)); // < 18446744073709551616 -> u64
        const lt127 = @as(u8, @intFromBool(highest_bit < 127)); // < max_i128 -> i128

        // Calculate precision index for positive values
        // This works by counting how many thresholds we're below
        const pos_sum = lt7 + lt8 + lt15 + lt16 + lt31 + lt32 + lt63 + lt64 + lt127;

        // Map sum to precision values
        // 9 = all true (bit < 7) -> i8
        // 8 = bit >= 7, < 8 -> u8
        // 7 = bit >= 8, < 15 -> i16
        // etc.
        const pos_precision_map = [10]u4{ 8, 9, 6, 7, 4, 5, 2, 3, 0, 1 }; // u128,i128,u64,i64,u32,i32,u16,i16,u8,i8
        const pos_precision = pos_precision_map[pos_sum];

        // Select between negative and positive precision based on is_negated
        const is_neg = @as(u4, @intFromBool(is_negated));
        const is_pos = @as(u4, 1) - is_neg;
        const final_precision = (neg_precision * is_neg) | (pos_precision * is_pos);(1 - is_neg_int));

        // Handle the zero case
        const zero_precision = @as(u4, 1); // i8
        const result = (final_precision * @as(u4, @intCast(1 - is_zero))) | (zero_precision * @as(u4, @intCast(is_zero)));

        return @enumFromInt(result);
    }
};

test "fromU128 - small positive values" {
    // Values that fit in i8 (0-127)
    try std.testing.expectEqual(Precision.i8, Precision.fromU128(0, false));
    try std.testing.expectEqual(Precision.i8, Precision.fromU128(1, false));
    try std.testing.expectEqual(Precision.i8, Precision.fromU128(127, false));

    // Values that fit in u8 but not i8 (128-255)
    try std.testing.expectEqual(Precision.u8, Precision.fromU128(128, false));
    try std.testing.expectEqual(Precision.u8, Precision.fromU128(255, false));

    // Values that fit in i16 (256-32767)
    try std.testing.expectEqual(Precision.i16, Precision.fromU128(256, false));
    try std.testing.expectEqual(Precision.i16, Precision.fromU128(32767, false));

    // Values that fit in u16 but not i16 (32768-65535)
    try std.testing.expectEqual(Precision.u16, Precision.fromU128(32768, false));
    try std.testing.expectEqual(Precision.u16, Precision.fromU128(65535, false));
}

test "fromU128 - medium positive values" {
    // Values that fit in i32 (65536-2147483647)
    try std.testing.expectEqual(Precision.i32, Precision.fromU128(65536, false));
    try std.testing.expectEqual(Precision.i32, Precision.fromU128(2147483647, false));

    // Values that fit in u32 but not i32 (2147483648-4294967295)
    try std.testing.expectEqual(Precision.u32, Precision.fromU128(2147483648, false));
    try std.testing.expectEqual(Precision.u32, Precision.fromU128(4294967295, false));

    // Values that fit in i64 (4294967296-9223372036854775807)
    try std.testing.expectEqual(Precision.i64, Precision.fromU128(4294967296, false));
    try std.testing.expectEqual(Precision.i64, Precision.fromU128(9223372036854775807, false));

    // Values that fit in u64 but not i64 (9223372036854775808-18446744073709551615)
    try std.testing.expectEqual(Precision.u64, Precision.fromU128(9223372036854775808, false));
    try std.testing.expectEqual(Precision.u64, Precision.fromU128(18446744073709551615, false));
}

test "fromU128 - large positive values" {
    // Values that fit in i128 but not u64 (18446744073709551616-170141183460469231731687303715884105727)
    try std.testing.expectEqual(Precision.i128, Precision.fromU128(18446744073709551616, false));
    try std.testing.expectEqual(Precision.i128, Precision.fromU128(std.math.maxInt(i128), false));

    // Values that only fit in u128 (> maxInt(i128))
    try std.testing.expectEqual(Precision.u128, Precision.fromU128(@as(u128, std.math.maxInt(i128)) + 1, false));
    try std.testing.expectEqual(Precision.u128, Precision.fromU128(std.math.maxInt(u128), false));
}

test "fromU128 - negative values" {
    // Values that fit in i8 when negated (1-128)
    try std.testing.expectEqual(Precision.i8, Precision.fromU128(1, true));
    try std.testing.expectEqual(Precision.i8, Precision.fromU128(128, true));

    // Values that fit in i16 when negated (129-32768)
    try std.testing.expectEqual(Precision.i16, Precision.fromU128(129, true));
    try std.testing.expectEqual(Precision.i16, Precision.fromU128(32768, true));

    // Values that fit in i32 when negated (32769-2147483648)
    try std.testing.expectEqual(Precision.i32, Precision.fromU128(32769, true));
    try std.testing.expectEqual(Precision.i32, Precision.fromU128(2147483648, true));

    // Values that fit in i64 when negated (2147483649-9223372036854775808)
    try std.testing.expectEqual(Precision.i64, Precision.fromU128(2147483649, true));
    try std.testing.expectEqual(Precision.i64, Precision.fromU128(9223372036854775808, true));

    // Values that fit in i128 when negated (> 9223372036854775808)
    try std.testing.expectEqual(Precision.i128, Precision.fromU128(9223372036854775809, true));
    try std.testing.expectEqual(Precision.i128, Precision.fromU128(std.math.maxInt(u128), true));
}

test "fromU128 - edge cases around boundaries" {
    // Testing around i8/u8 boundary
    try std.testing.expectEqual(Precision.i8, Precision.fromU128(126, false));
    try std.testing.expectEqual(Precision.i8, Precision.fromU128(127, false));
    try std.testing.expectEqual(Precision.u8, Precision.fromU128(128, false));
    try std.testing.expectEqual(Precision.u8, Precision.fromU128(129, false));

    // Testing around u8/i16 boundary
    try std.testing.expectEqual(Precision.u8, Precision.fromU128(254, false));
    try std.testing.expectEqual(Precision.u8, Precision.fromU128(255, false));
    try std.testing.expectEqual(Precision.i16, Precision.fromU128(256, false));
    try std.testing.expectEqual(Precision.i16, Precision.fromU128(257, false));

    // Testing negative boundaries
    try std.testing.expectEqual(Precision.i8, Precision.fromU128(127, true));
    try std.testing.expectEqual(Precision.i8, Precision.fromU128(128, true));
    try std.testing.expectEqual(Precision.i16, Precision.fromU128(129, true));

    try std.testing.expectEqual(Precision.i16, Precision.fromU128(32767, true));
    try std.testing.expectEqual(Precision.i16, Precision.fromU128(32768, true));
    try std.testing.expectEqual(Precision.i32, Precision.fromU128(32769, true));
}

test "fromU128 - special case: zero" {
    try std.testing.expectEqual(Precision.i8, Precision.fromU128(0, false));
    // Note: negated zero is still zero, fits in i8
    try std.testing.expectEqual(Precision.i8, Precision.fromU128(0, true));
}

test "fromU128 - maximum values for each type" {
    // Positive max values
    try std.testing.expectEqual(Precision.i8, Precision.fromU128(@as(u128, std.math.maxInt(i8)), false));
    try std.testing.expectEqual(Precision.u8, Precision.fromU128(@as(u128, std.math.maxInt(u8)), false));
    try std.testing.expectEqual(Precision.i16, Precision.fromU128(@as(u128, std.math.maxInt(i16)), false));
    try std.testing.expectEqual(Precision.u16, Precision.fromU128(@as(u128, std.math.maxInt(u16)), false));
    try std.testing.expectEqual(Precision.i32, Precision.fromU128(@as(u128, std.math.maxInt(i32)), false));
    try std.testing.expectEqual(Precision.u32, Precision.fromU128(@as(u128, std.math.maxInt(u32)), false));
    try std.testing.expectEqual(Precision.i64, Precision.fromU128(@as(u128, std.math.maxInt(i64)), false));
    try std.testing.expectEqual(Precision.u64, Precision.fromU128(@as(u128, std.math.maxInt(u64)), false));
    try std.testing.expectEqual(Precision.i128, Precision.fromU128(@as(u128, std.math.maxInt(i128)), false));
    try std.testing.expectEqual(Precision.u128, Precision.fromU128(std.math.maxInt(u128), false));

    // Negative min values (represented as positive u128 values that will be negated)
    try std.testing.expectEqual(Precision.i8, Precision.fromU128(128, true)); // -128
    try std.testing.expectEqual(Precision.i16, Precision.fromU128(32768, true)); // -32768
    try std.testing.expectEqual(Precision.i32, Precision.fromU128(2147483648, true)); // -2147483648
    try std.testing.expectEqual(Precision.i64, Precision.fromU128(9223372036854775808, true)); // -9223372036854775808
}

test "debug print breakpoints" {
    const print = std.debug.print;

    print("\n=== Positive value breakpoints ===\n", .{});
    print("Value                     | Hex                      | Binary (trailing part)                           | Leading 0s | Trailing 0s | Type\n", .{});
    print("--------------------------|--------------------------|-------------------------------------------------|------------|-------------|------\n", .{});

    // i8 max
    const i8_max: u128 = @as(u128, std.math.maxInt(i8));
    print("{d:25} | 0x{X:22} | ...{b:46} | {d:10} | {d:11} | i8\n", .{ i8_max, i8_max, @as(u64, @truncate(i8_max)), @clz(i8_max), @ctz(i8_max) });

    // u8 max
    const u8_max: u128 = @as(u128, std.math.maxInt(u8));
    print("{d:25} | 0x{X:22} | ...{b:46} | {d:10} | {d:11} | u8\n", .{ u8_max, u8_max, @as(u64, @truncate(u8_max)), @clz(u8_max), @ctz(u8_max) });

    // i16 max
    const i16_max: u128 = @as(u128, std.math.maxInt(i16));
    print("{d:25} | 0x{X:22} | ...{b:46} | {d:10} | {d:11} | i16\n", .{ i16_max, i16_max, @as(u64, @truncate(i16_max)), @clz(i16_max), @ctz(i16_max) });

    // u16 max
    const u16_max: u128 = @as(u128, std.math.maxInt(u16));
    print("{d:25} | 0x{X:22} | ...{b:46} | {d:10} | {d:11} | u16\n", .{ u16_max, u16_max, @as(u64, @truncate(u16_max)), @clz(u16_max), @ctz(u16_max) });

    // i32 max
    const i32_max: u128 = @as(u128, std.math.maxInt(i32));
    print("{d:25} | 0x{X:22} | ...{b:46} | {d:10} | {d:11} | i32\n", .{ i32_max, i32_max, @as(u64, @truncate(i32_max)), @clz(i32_max), @ctz(i32_max) });

    // u32 max
    const u32_max: u128 = @as(u128, std.math.maxInt(u32));
    print("{d:25} | 0x{X:22} | ...{b:46} | {d:10} | {d:11} | u32\n", .{ u32_max, u32_max, @as(u64, @truncate(u32_max)), @clz(u32_max), @ctz(u32_max) });

    // i64 max
    const i64_max: u128 = @as(u128, std.math.maxInt(i64));
    print("{d:25} | 0x{X:22} | ...{b:46} | {d:10} | {d:11} | i64\n", .{ i64_max, i64_max, @as(u64, @truncate(i64_max)), @clz(i64_max), @ctz(i64_max) });

    // u64 max
    const u64_max: u128 = @as(u128, std.math.maxInt(u64));
    print("{d:25} | 0x{X:22} | ...{b:46} | {d:10} | {d:11} | u64\n", .{ u64_max, u64_max, @as(u64, @truncate(u64_max)), @clz(u64_max), @ctz(u64_max) });

    // i128 max
    const i128_max: u128 = @as(u128, std.math.maxInt(i128));
    print("{d:25} | 0x{X:22} | (128-bit value)                                  | {d:10} | {d:11} | i128\n", .{ i128_max, i128_max, @clz(i128_max), @ctz(i128_max) });

    // u128 max
    const u128_max: u128 = std.math.maxInt(u128);
    print("{d:25} | 0x{X:22} | (128-bit value)                                  | {d:10} | {d:11} | u128\n", .{ u128_max, u128_max, @clz(u128_max), @ctz(u128_max) });

    print("\n=== Negative value breakpoints (as positive u128 before negation) ===\n", .{});
    print("Value                     | Hex                      | Binary (trailing part)                           | Leading 0s | Trailing 0s | Type\n", .{});
    print("--------------------------|--------------------------|-------------------------------------------------|------------|-------------|------\n", .{});

    // Negative breakpoints
    const neg_i8_min: u128 = 128; // -128
    print("{d:25} | 0x{X:22} | ...{b:46} | {d:10} | {d:11} | -i8\n", .{ neg_i8_min, neg_i8_min, @as(u64, @truncate(neg_i8_min)), @clz(neg_i8_min), @ctz(neg_i8_min) });

    const neg_i16_min: u128 = 32768; // -32768
    print("{d:25} | 0x{X:22} | ...{b:46} | {d:10} | {d:11} | -i16\n", .{ neg_i16_min, neg_i16_min, @as(u64, @truncate(neg_i16_min)), @clz(neg_i16_min), @ctz(neg_i16_min) });

    const neg_i32_min: u128 = 2147483648; // -2147483648
    print("{d:25} | 0x{X:22} | ...{b:46} | {d:10} | {d:11} | -i32\n", .{ neg_i32_min, neg_i32_min, @as(u64, @truncate(neg_i32_min)), @clz(neg_i32_min), @ctz(neg_i32_min) });

    const neg_i64_min: u128 = 9223372036854775808; // -9223372036854775808
    print("{d:25} | 0x{X:22} | ...{b:46} | {d:10} | {d:11} | -i64\n", .{ neg_i64_min, neg_i64_min, @as(u64, @truncate(neg_i64_min)), @clz(neg_i64_min), @ctz(neg_i64_min) });

    const neg_i128_min: u128 = @as(u128, 1) << 127; // -170141183460469231731687303715884105728
    print("{d:25} | 0x{X:22} | (128-bit value)                                  | {d:10} | {d:11} | -i128\n", .{ neg_i128_min, neg_i128_min, @clz(neg_i128_min), @ctz(neg_i128_min) });

    print("\n=== Patterns and observations ===\n", .{});
    print("1. For positive values:\n", .{});
    print("   - Signed max values have pattern: 2^(n-1) - 1 (e.g., i8 max = 127 = 2^7 - 1)\n", .{});
    print("   - Unsigned max values have pattern: 2^n - 1 (e.g., u8 max = 255 = 2^8 - 1)\n", .{});
    print("   - Leading zeros correlate with bit width: 128 - bit_width\n", .{});
    print("\n2. For negative values (as positive u128):\n", .{});
    print("   - All are powers of 2: 2^7, 2^15, 2^31, 2^63, 2^127\n", .{});
    print("   - Trailing zeros = exponent (7, 15, 31, 63, 127)\n", .{});
    print("   - These correspond to the bit position of the sign bit\n", .{});

    print("\n=== Additional Analysis for Branchless Implementation ===\n", .{});

    // Let's analyze the relationship between leading zeros and precision
    print("\nLeading zeros analysis:\n", .{});
    print("Type  | Bits | Leading 0s | 128 - Leading 0s | log2(max_val + 1)\n", .{});
    print("------|------|------------|------------------|------------------\n", .{});
    print("i8    |    8 |        121 |                7 |                7\n", .{});
    print("u8    |    8 |        120 |                8 |                8\n", .{});
    print("i16   |   16 |        113 |               15 |               15\n", .{});
    print("u16   |   16 |        112 |               16 |               16\n", .{});
    print("i32   |   32 |         97 |               31 |               31\n", .{});
    print("u32   |   32 |         96 |               32 |               32\n", .{});
    print("i64   |   64 |         65 |               63 |               63\n", .{});
    print("u64   |   64 |         64 |               64 |               64\n", .{});
    print("i128  |  128 |          1 |              127 |              127\n", .{});
    print("u128  |  128 |          0 |              128 |              128\n", .{});

    print("\nKey insight: 128 - leading_zeros gives us the number of bits needed!\n", .{});
    print("For signed types: bits_needed = n - 1 (where n is the type bit width)\n", .{});
    print("For unsigned types: bits_needed = n\n", .{});

    print("\n=== Testing branchless approach ideas ===\n", .{});

    // Test some values
    const test_vals = [_]struct { val: u128, neg: bool, expected: []const u8 }{
        .{ .val = 0, .neg = false, .expected = "i8" },
        .{ .val = 127, .neg = false, .expected = "i8" },
        .{ .val = 128, .neg = false, .expected = "u8" },
        .{ .val = 128, .neg = true, .expected = "i8" },
        .{ .val = 255, .neg = false, .expected = "u8" },
        .{ .val = 256, .neg = false, .expected = "i16" },
        .{ .val = 32768, .neg = true, .expected = "i16" },
        .{ .val = 65535, .neg = false, .expected = "u16" },
        .{ .val = 2147483648, .neg = true, .expected = "i32" },
        .{ .val = 4294967295, .neg = false, .expected = "u32" },
        .{ .val = 9223372036854775808, .neg = true, .expected = "i64" },
        .{ .val = 18446744073709551615, .neg = false, .expected = "u64" },
    };

    print("\nTest values:\n", .{});
    print("Value                     | Negated | Bits needed | Expected\n", .{});
    print("--------------------------|---------|-------------|----------\n", .{});

    for (test_vals) |tv| {
        const bits_needed = 128 - @clz(tv.val);
        print("{d:25} | {s:7} | {d:11} | {s:8}\n", .{ tv.val, if (tv.neg) "true" else "false", bits_needed, tv.expected });
    }

    print("\n=== Branchless algorithm outline ===\n", .{});
    print("1. Calculate bits_needed = 128 - @clz(value)\n", .{});
    print("2. For negated values:\n", .{});
    print("   - Check if value is a power of 2 using (value & (value - 1)) == 0\n", .{});
    print("   - If power of 2, bits_needed = @ctz(value) + 1\n", .{});
    print("   - Map bits_needed to signed types only\n", .{});
    print("3. For positive values:\n", .{});
    print("   - Use bits_needed to find smallest type that fits\n", .{});
    print("   - Consider both signed and unsigned types\n", .{});
    print("4. Use lookup tables or bit manipulation to map bits_needed to Precision enum\n", .{});
}

test "compare branching vs branchless implementations" {
    const print = std.debug.print;
    print("\n=== Testing branchless implementation ===\n", .{});

    // Test cases covering all edge cases
    const test_cases = [_]struct { val: u128, neg: bool }{
        // Small positive values
        .{ .val = 0, .neg = false },
        .{ .val = 1, .neg = false },
        .{ .val = 127, .neg = false },
        .{ .val = 128, .neg = false },
        .{ .val = 255, .neg = false },
        .{ .val = 256, .neg = false },

        // Medium positive values
        .{ .val = 32767, .neg = false },
        .{ .val = 32768, .neg = false },
        .{ .val = 65535, .neg = false },
        .{ .val = 65536, .neg = false },
        .{ .val = 2147483647, .neg = false },
        .{ .val = 2147483648, .neg = false },
        .{ .val = 4294967295, .neg = false },
        .{ .val = 4294967296, .neg = false },

        // Large positive values
        .{ .val = 9223372036854775807, .neg = false },
        .{ .val = 9223372036854775808, .neg = false },
        .{ .val = 18446744073709551615, .neg = false },
        .{ .val = 18446744073709551616, .neg = false },
        .{ .val = std.math.maxInt(i128), .neg = false },
        .{ .val = @as(u128, std.math.maxInt(i128)) + 1, .neg = false },
        .{ .val = std.math.maxInt(u128), .neg = false },

        // Negative values
        .{ .val = 1, .neg = true },
        .{ .val = 127, .neg = true },
        .{ .val = 128, .neg = true }, // -128 (i8 min)
        .{ .val = 129, .neg = true },
        .{ .val = 32767, .neg = true },
        .{ .val = 32768, .neg = true }, // -32768 (i16 min)
        .{ .val = 32769, .neg = true },
        .{ .val = 2147483647, .neg = true },
        .{ .val = 2147483648, .neg = true }, // -2147483648 (i32 min)
        .{ .val = 2147483649, .neg = true },
        .{ .val = 9223372036854775807, .neg = true },
        .{ .val = 9223372036854775808, .neg = true }, // -9223372036854775808 (i64 min)
        .{ .val = 9223372036854775809, .neg = true },
        .{ .val = @as(u128, 1) << 127, .neg = true }, // i128 min
    };

    var all_match = true;
    var mismatch_count: u32 = 0;

    for (test_cases) |tc| {
        const branching = Precision.fromU128(tc.val, tc.neg);
        const branchless = Precision.fromU128Branchless(tc.val, tc.neg);

        if (branching != branchless) {
            // Debug print to understand what's happening
            const highest_bit = @as(i8, 127) - @as(i8, @intCast(@clz(tc.val | @intFromBool(tc.val == 0))));
            print("MISMATCH: val={d}, neg={}, branching={s}, branchless={s}, highest_bit={d}\n", .{ tc.val, tc.neg, @tagName(branching), @tagName(branchless), highest_bit });
            all_match = false;
            mismatch_count += 1;
        }
    }

    if (all_match) {
        print("✓ All {} test cases match between branching and branchless!\n", .{test_cases.len});
    } else {
        print("✗ Found {} mismatches out of {} test cases\n", .{ mismatch_count, test_cases.len });
    }

    try std.testing.expect(all_match);
}
