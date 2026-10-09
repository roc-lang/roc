//! `memset` for the compiler executable on musl targets.
//!
//! musl leaves `memset` to compiler_rt, whose implementation stores one byte
//! per loop iteration, so every `@memset` and every zero-initialized aggregate
//! the compiler writes costs a loop iteration per byte. This one stores 16
//! bytes at a time. It is built with `no_builtin`, so its own store loops are
//! never turned back into calls to `memset`, and it uses no string
//! instructions.

const std = @import("std");
const builtin = @import("builtin");

comptime {
    if (builtin.abi.isMusl() and !builtin.is_test) {
        @export(&memset, .{ .name = "memset", .linkage = .strong });
    }
}

const Block = @Vector(16, u8);

/// Fill `len` bytes at `dest` with `c`.
pub fn memset(dest: ?[*]u8, c: u8, len: usize) callconv(.c) ?[*]u8 {
    @setRuntimeSafety(false);
    if (len == 0) return dest;
    const d = dest.?;
    if (len < 16) {
        fillShort(d, c, len);
        return dest;
    }
    const block: Block = @splat(c);
    // The first and last 16 bytes are stored unaligned; everything between
    // them is stored in aligned 32-byte steps, and the stores overlap rather
    // than branching on the remainder.
    storeUnaligned(d, block);
    const end = d + len;
    if (len > 32) {
        var p = d + (16 - @intFromPtr(d) % 16);
        while (@intFromPtr(end) - @intFromPtr(p) > 32) : (p += 32) {
            storeAligned(p, block);
            storeAligned(p + 16, block);
        }
        storeUnaligned(end - 32, block);
    }
    storeUnaligned(end - 16, block);
    return dest;
}

/// Fill `len` bytes, `1 <= len < 16`, with two overlapping stores of the
/// widest integer that fits.
inline fn fillShort(d: [*]u8, c: u8, len: usize) void {
    if (len >= 8) {
        const word: u64 = @as(u64, c) * 0x0101_0101_0101_0101;
        storeInt(u64, d, word);
        storeInt(u64, d + len - 8, word);
    } else if (len >= 4) {
        const word: u32 = @as(u32, c) * 0x0101_0101;
        storeInt(u32, d, word);
        storeInt(u32, d + len - 4, word);
    } else {
        d[0] = c;
        d[len / 2] = c;
        d[len - 1] = c;
    }
}

inline fn storeInt(comptime T: type, p: [*]u8, value: T) void {
    @as(*align(1) T, @ptrCast(p)).* = value;
}

inline fn storeUnaligned(p: [*]u8, block: Block) void {
    @as(*align(1) Block, @ptrCast(p)).* = block;
}

inline fn storeAligned(p: [*]u8, block: Block) void {
    @as(*align(16) Block, @ptrCast(@alignCast(p))).* = block;
}

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
