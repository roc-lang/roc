//! FuzzReader - Direct byte consumption for coverage-guided fuzzing
//!
//! Instead of using a PRNG seeded by input bytes, this reader consumes bytes
//! directly from the fuzzer input. This gives AFL++ direct control over each
//! decision in the code generator, enabling much better coverage exploration.
//!
//! When bytes are exhausted, methods return minimal values (0, false, min)
//! to produce valid but minimal code. This means:
//! - Short inputs produce simple, valid programs
//! - The fuzzer discovers that longer inputs create more complex code
//!
//! Reference implementations:
//! - Rust: https://github.com/rust-fuzz/arbitrary
//! - Roc: https://github.com/bhansconnect/roc-fuzz-internal

const Self = @This();

/// The fuzzer input data
data: []const u8,

/// Current read position
position: usize,

/// Initialize a new FuzzReader with the given input data
pub fn init(data: []const u8) Self {
    return .{
        .data = data,
        .position = 0,
    };
}

/// Read a single byte from the input.
/// Returns 0 if input is exhausted.
pub fn readByte(self: *Self) u8 {
    if (self.position >= self.data.len) {
        return 0;
    }
    const byte = self.data[self.position];
    self.position += 1;
    return byte;
}

/// Read multiple bytes from the input.
/// Returns 0 for any bytes beyond the input length.
pub fn readBytes(self: *Self, comptime N: usize) [N]u8 {
    var result: [N]u8 = undefined;
    for (0..N) |i| {
        result[i] = self.readByte();
    }
    return result;
}

/// Generate a boolean value.
/// Consumes 1 byte. Returns false if exhausted.
pub fn boolean(self: *Self) bool {
    return (self.readByte() & 1) == 1;
}

/// Generate an integer in the range [min, max] (inclusive).
/// Uses only as many bytes as needed to represent the range.
/// Returns min if exhausted.
pub fn intRangeAtMost(self: *Self, comptime T: type, min: T, max: T) T {
    if (min >= max) return min;

    // Calculate range size, handling both signed and unsigned types
    const type_info = @typeInfo(T);
    const range: u64 = switch (type_info) {
        .int => |info| blk: {
            if (info.signedness == .signed) {
                // For signed types, cast to i128 first to handle the subtraction
                const diff: i128 = @as(i128, max) - @as(i128, min);
                break :blk @intCast(diff + 1);
            } else {
                // For unsigned types, direct calculation
                break :blk @as(u64, max - min) + 1;
            }
        },
        else => @compileError("intRangeAtMost only supports integer types"),
    };

    // Determine how many bytes we need for this range
    const bytes_needed = bytesForRange(range);

    // Read the minimum bytes needed
    var value: u64 = 0;
    for (0..bytes_needed) |i| {
        const byte = self.readByte();
        value |= @as(u64, byte) << @intCast(i * 8);
    }

    // Map to the range using modulo
    const offset = value % range;
    // For signed types, we need to be careful about the addition
    return switch (type_info) {
        .int => |info| blk: {
            if (info.signedness == .signed) {
                const signed_offset: T = @intCast(offset);
                break :blk min + signed_offset;
            } else {
                break :blk min + @as(T, @intCast(offset));
            }
        },
        else => unreachable,
    };
}

/// Generate an integer in the range [min, max) (exclusive upper bound).
/// Uses only as many bytes as needed to represent the range.
/// Returns min if exhausted or if min >= max.
pub fn intRangeLessThan(self: *Self, comptime T: type, min: T, max: T) T {
    if (min >= max) return min;
    // [min, max) is equivalent to [min, max-1]
    return self.intRangeAtMost(T, min, max - 1);
}

/// Calculate the minimum number of bytes needed to represent a range
fn bytesForRange(range: u64) usize {
    if (range <= 0x100) return 1;
    if (range <= 0x10000) return 2;
    if (range <= 0x1000000) return 3;
    if (range <= 0x100000000) return 4;
    if (range <= 0x10000000000) return 5;
    if (range <= 0x1000000000000) return 6;
    if (range <= 0x100000000000000) return 7;
    return 8;
}

// Tests
const std = @import("std");
const testing = std.testing;

test "readByte returns 0 when exhausted" {
    var reader = Self.init(&[_]u8{});
    try testing.expectEqual(@as(u8, 0), reader.readByte());
    try testing.expectEqual(@as(u8, 0), reader.readByte());
}

test "readByte returns bytes in order" {
    var reader = Self.init(&[_]u8{ 0x12, 0x34, 0x56 });
    try testing.expectEqual(@as(u8, 0x12), reader.readByte());
    try testing.expectEqual(@as(u8, 0x34), reader.readByte());
    try testing.expectEqual(@as(u8, 0x56), reader.readByte());
    try testing.expectEqual(@as(u8, 0), reader.readByte()); // exhausted
}

test "boolean uses low bit" {
    var reader = Self.init(&[_]u8{ 0x00, 0x01, 0x02, 0x03 });
    try testing.expectEqual(false, reader.boolean()); // 0x00 & 1 = 0
    try testing.expectEqual(true, reader.boolean()); // 0x01 & 1 = 1
    try testing.expectEqual(false, reader.boolean()); // 0x02 & 1 = 0
    try testing.expectEqual(true, reader.boolean()); // 0x03 & 1 = 1
}

test "intRangeAtMost handles small ranges with 1 byte" {
    var reader = Self.init(&[_]u8{ 0, 1, 2, 3, 255 });
    // Range [0, 2] has size 3
    try testing.expectEqual(@as(u8, 0), reader.intRangeAtMost(u8, 0, 2)); // 0 % 3 = 0
    try testing.expectEqual(@as(u8, 1), reader.intRangeAtMost(u8, 0, 2)); // 1 % 3 = 1
    try testing.expectEqual(@as(u8, 2), reader.intRangeAtMost(u8, 0, 2)); // 2 % 3 = 2
    try testing.expectEqual(@as(u8, 0), reader.intRangeAtMost(u8, 0, 2)); // 3 % 3 = 0
    try testing.expectEqual(@as(u8, 0), reader.intRangeAtMost(u8, 0, 2)); // 255 % 3 = 0
}

test "intRangeAtMost with min offset" {
    var reader = Self.init(&[_]u8{ 0, 1, 2 });
    // Range [10, 12] has size 3
    try testing.expectEqual(@as(u8, 10), reader.intRangeAtMost(u8, 10, 12)); // 10 + 0
    try testing.expectEqual(@as(u8, 11), reader.intRangeAtMost(u8, 10, 12)); // 10 + 1
    try testing.expectEqual(@as(u8, 12), reader.intRangeAtMost(u8, 10, 12)); // 10 + 2
}

test "intRangeLessThan excludes max" {
    var reader = Self.init(&[_]u8{ 0, 1, 2, 3 });
    // Range [0, 3) = [0, 2] has size 3
    try testing.expectEqual(@as(u8, 0), reader.intRangeLessThan(u8, 0, 3));
    try testing.expectEqual(@as(u8, 1), reader.intRangeLessThan(u8, 0, 3));
    try testing.expectEqual(@as(u8, 2), reader.intRangeLessThan(u8, 0, 3));
    try testing.expectEqual(@as(u8, 0), reader.intRangeLessThan(u8, 0, 3)); // 3 % 3 = 0
}

test "intRangeAtMost returns min when exhausted" {
    var reader = Self.init(&[_]u8{});
    try testing.expectEqual(@as(u8, 5), reader.intRangeAtMost(u8, 5, 10));
}

test "intRangeAtMost handles equal min and max" {
    var reader = Self.init(&[_]u8{ 0xFF, 0xFF });
    try testing.expectEqual(@as(u8, 7), reader.intRangeAtMost(u8, 7, 7));
    try testing.expectEqual(@as(u8, 7), reader.intRangeAtMost(u8, 7, 7));
}

test "intRangeAtMost works with usize" {
    var reader = Self.init(&[_]u8{ 5, 0, 0, 0, 0, 0, 0, 0 });
    const result = reader.intRangeAtMost(usize, 0, 10);
    try testing.expectEqual(@as(usize, 5), result);
}

test "position tracks bytes consumed" {
    var reader = Self.init(&[_]u8{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });
    try testing.expectEqual(@as(usize, 0), reader.position);

    _ = reader.readByte();
    try testing.expectEqual(@as(usize, 1), reader.position);

    _ = reader.boolean();
    try testing.expectEqual(@as(usize, 2), reader.position);

    _ = reader.intRangeAtMost(u8, 0, 255); // uses 1 byte for range 256
    try testing.expectEqual(@as(usize, 3), reader.position);
}
