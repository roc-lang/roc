//! SIMD narrowing of ASCII prefixes in UTF-16 and UTF-32 input.

const std = @import("std");
const builtin = @import("builtin");

/// Writes the initial ASCII prefix, limited by the destination length, and
/// returns the number of input units consumed (equal to bytes written).
/// Full 128-bit input vectors are checked before narrowing; partial vectors
/// and the first vector containing a non-ASCII unit are handled lane by lane.
pub fn encodePrefix(comptime Unit: type, units: []const Unit, output: []u8) usize {
    comptime std.debug.assert(Unit == u16 or Unit == u32);
    const lanes = 16 / @sizeOf(Unit);
    const Vector = @Vector(lanes, Unit);
    const limit = @min(units.len, output.len);
    var index: usize = 0;
    while (limit - index >= lanes) : (index += lanes) {
        const wide: Vector = units[index..][0..lanes].*;
        if (@reduce(.Or, wide > @as(Vector, @splat(0x7f)))) break;
        // Keep narrowing a full-width shuffle: truncating to a short vector
        // makes LLVM scalarize the stores on wasm32.
        const byte_indices = comptime blk: {
            var indices: [16]i32 = undefined;
            const low_byte = if (builtin.cpu.arch.endian() == .little) 0 else @sizeOf(Unit) - 1;
            for (&indices, 0..) |*lane, i| lane.* = @intCast((i % lanes) * @sizeOf(Unit) + low_byte);
            break :blk indices;
        };
        const narrow = @shuffle(u8, @as(@Vector(16, u8), @bitCast(wide)), undefined, byte_indices);
        const Packed = std.meta.Int(.unsigned, lanes * 8);
        const packed_lanes: @Vector(16 / lanes, Packed) = @bitCast(narrow);
        @as(*align(1) Packed, @ptrCast(output[index..].ptr)).* = packed_lanes[0];
    }
    while (index < limit and units[index] <= 0x7f) : (index += 1) {
        output[index] = @intCast(units[index]);
    }
    return index;
}

test "ASCII prefix handles all lengths, offsets, destination limits, and stop lanes" {
    inline for (.{ u16, u32 }) |Unit| {
        var input: [65]Unit = undefined;
        for (&input, 0..) |*unit, i| unit.* = @intCast((i * 37) % 128);
        var output: [67]u8 = undefined;
        for (0..4) |offset| {
            for (0..input.len - offset + 1) |length| {
                const units = input[offset..][0..length];
                for (0..length + 2) |capacity| {
                    @memset(&output, 0xaa);
                    const destination = output[1..][0..capacity];
                    const written = encodePrefix(Unit, units, destination);
                    try std.testing.expectEqual(@min(length, capacity), written);
                    for (units[0..written], output[1..][0..written]) |unit, byte| {
                        try std.testing.expectEqual(@as(u8, @intCast(unit)), byte);
                    }
                    try std.testing.expectEqual(@as(u8, 0xaa), output[0]);
                    for (output[written + 1 ..]) |byte| try std.testing.expectEqual(@as(u8, 0xaa), byte);
                }
            }
        }
        // Test each possible first non-ASCII lane, including values whose low
        // byte is ASCII: checking narrowed bytes would silently accept these.
        const non_ascii_values = if (Unit == u16)
            .{ 0x80, 0x100, 0xd800, 0xffff }
        else
            .{ 0x80, 0x100, 0xd800, 0x10000, 0x80000000, 0xffffffff };
        inline for (non_ascii_values) |non_ascii| {
            for (0..input.len) |stop| {
                const previous = input[stop];
                input[stop] = non_ascii;
                @memset(&output, 0xaa);
                try std.testing.expectEqual(stop, encodePrefix(Unit, &input, &output));
                for (input[0..stop], output[0..stop]) |unit, byte| {
                    try std.testing.expectEqual(@as(u8, @intCast(unit)), byte);
                }
                for (output[stop..]) |byte| try std.testing.expectEqual(@as(u8, 0xaa), byte);
                input[stop] = previous;
            }
        }
    }
}
