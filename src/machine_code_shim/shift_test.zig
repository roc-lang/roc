//! Compare the shim's upstream shift implementations with normal language shifts.
const std = @import("std");
const shift = @import("private_compiler_rt").shift;

test "toolchain shifts match language shifts at both runtime widths" {
    inline for (.{ i64, i128 }) |T| {
        const U = @Int(.unsigned, @bitSizeOf(T));
        const bits = @bitSizeOf(T);
        const left = if (T == i64) shift.__ashldi3 else shift.__ashlti3;
        const arithmetic = if (T == i64) shift.__ashrdi3 else shift.__ashrti3;
        const logical = if (T == i64) shift.__lshrdi3 else shift.__lshrti3;
        const values = [_]T{ 0, 1, -1, std.math.minInt(T), std.math.maxInt(T), @bitCast(@as(U, 0x87654321) << (bits / 2) | 0xfedcba98) };
        for (values) |a| {
            for ([_]u8{ 0, 1, bits / 2 - 1, bits / 2, bits / 2 + 1, bits - 1 }) |b| {
                const count: std.math.Log2Int(T) = @intCast(b);
                try std.testing.expectEqual(a << count, left(a, b));
                try std.testing.expectEqual(a >> count, arithmetic(a, b));
                try std.testing.expectEqual(@as(T, @bitCast(@as(U, @bitCast(a)) >> count)), logical(a, b));
            }
        }
        var prng = std.Random.DefaultPrng.init(0x736869667473);
        const random = prng.random();
        for (0..1000) |_| {
            const a = random.int(T);
            const count = random.int(std.math.Log2Int(T));
            try std.testing.expectEqual(a << count, left(a, count));
            try std.testing.expectEqual(a >> count, arithmetic(a, count));
            try std.testing.expectEqual(@as(T, @bitCast(@as(U, @bitCast(a)) >> count)), logical(a, count));
        }
    }
}
