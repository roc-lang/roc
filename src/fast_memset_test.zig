//! Tests for `fast_memset`, compiled from the module as the compiler
//! executable compiles it: without builtin recognition.

const std = @import("std");
const memset = @import("fast_memset").memset;

test "memset fills exactly the requested bytes at every length and alignment" {
    const guard = 0xA5;
    var buffer: [16 + 200 + 16]u8 align(16) = undefined;
    for (0..16) |offset| {
        for (0..200) |len| {
            @memset(&buffer, guard);
            const fill: u8 = @intCast(len % 251);
            const result = memset(buffer[16 + offset ..].ptr, fill, len);
            try std.testing.expectEqual(@as(?[*]u8, buffer[16 + offset ..].ptr), result);
            for (buffer, 0..) |byte, index| {
                const inside = index >= 16 + offset and index < 16 + offset + len;
                try std.testing.expectEqual(if (inside) fill else guard, byte);
            }
        }
    }
}

test "memset of zero bytes accepts a null destination" {
    try std.testing.expectEqual(@as(?[*]u8, null), memset(null, 0, 0));
}
