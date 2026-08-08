//! Canonical byte encodings shared across compiler stages.

const std = @import("std");

/// Writes one integer using its canonical little-endian representation.
pub fn writeIntLittle(comptime T: type, out: []u8, value: T) void {
    std.debug.assert(out.len == @sizeOf(T));
    std.mem.writeInt(T, out[0..@sizeOf(T)], value, .little);
}

test "write integer little endian" {
    var bytes: [4]u8 = undefined;
    writeIntLittle(u32, &bytes, 0x12345678);
    try std.testing.expectEqualSlices(u8, &.{ 0x78, 0x56, 0x34, 0x12 }, &bytes);
}
