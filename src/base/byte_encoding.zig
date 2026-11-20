//! Canonical byte encodings shared across compiler stages.

const std = @import("std");

/// Writes one integer using its canonical little-endian representation.
pub fn writeIntLittle(comptime T: type, out: []u8, value: T) void {
    std.debug.assert(out.len == @sizeOf(T));
    std.mem.writeInt(T, out[0..@sizeOf(T)], value, .little);
}

/// Whether two 256-bit digests hold the same bytes: four word compares folded
/// into one test.
pub fn digestsEql(a: *const [32]u8, b: *const [32]u8) bool {
    var difference: u64 = 0;
    inline for (0..4) |word| {
        difference |= std.mem.readInt(u64, a[word * 8 ..][0..8], .little) ^ std.mem.readInt(u64, b[word * 8 ..][0..8], .little);
    }
    return difference == 0;
}

test "256-bit digests are equal exactly when every byte is" {
    var a: [32]u8 = undefined;
    for (&a, 0..) |*byte, index| byte.* = @intCast(index * 7 + 3);
    var b = a;
    try std.testing.expect(digestsEql(&a, &b));
    for (0..32) |index| {
        b[index] ^= 0x80;
        try std.testing.expect(!digestsEql(&a, &b));
        try std.testing.expect(!digestsEql(&b, &a));
        b[index] ^= 0x81;
        try std.testing.expect(!digestsEql(&a, &b));
        b[index] = a[index];
    }
    try std.testing.expect(digestsEql(&a, &b));
}

test "write integer little endian" {
    var bytes: [4]u8 = undefined;
    writeIntLittle(u32, &bytes, 0x12345678);
    try std.testing.expectEqualSlices(u8, &.{ 0x78, 0x56, 0x34, 0x12 }, &bytes);
}
