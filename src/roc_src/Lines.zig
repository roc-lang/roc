//! A compact representation of where the newlines are in a given Roc source file.

const std = @import("std");
const Allocator = std.mem.Allocator;
pub const Self = @This();

bitmasks: []u64,

pub fn initWithCapacity(gpa: Allocator, capacity: usize) Allocator.Error!Self {
    return .{ .bitmasks = try gpa.alloc(u64, capacity) };
}

pub fn deinit(self: Self, gpa: Allocator) void {
    gpa.free(self.bitmasks);
}

/// Returns the absolute index of the nearest '\n' at or after the given `pos`.
pub fn newlineAfter(self: *const Self, pos: usize) error{NoNewlinesAfter}!usize {
    const bits_per_mask = @bitSizeOf(u64);
    var bitmask_idx = pos / bits_per_mask;

    if (bitmask_idx >= self.bitmasks.len) {
        return error.NoNewlinesAfter;
    }

    const offset_in_bitmask = pos % bits_per_mask;

    // Mask out bits before the current position to only find newlines at or after it.
    const mask = ~@as(u64, 0) >> @as(u6, @intCast(offset_in_bitmask));
    const masked_value = self.bitmasks[bitmask_idx] & mask;

    if (masked_value != 0) {
        const leading_zeros = @clz(masked_value);
        return (bitmask_idx * bits_per_mask) + leading_zeros;
    }

    // If no newline was found in the current mask, check the subsequent ones.
    bitmask_idx += 1;
    while (bitmask_idx < self.bitmasks.len) {
        const value = self.bitmasks[bitmask_idx];
        if (value != 0) {
            const leading_zeros = @clz(value);
            return (bitmask_idx * bits_per_mask) + leading_zeros;
        }
        bitmask_idx += 1;
    }

    return error.NoNewlinesAfter;
}

/// Returns the absolute index of the nearest '\n' *before* the given `pos`.
pub fn newlineBefore(self: *const Self, pos: usize) error{NoNewlinesBefore}!usize {
    if (pos == 0 or self.bitmasks.len == 0) return error.NoNewlinesBefore;

    const mask_type = u64;
    const bits_per_mask = @bitSizeOf(mask_type);
    const last_bitmask_idx = self.bitmasks.len - 1;
    var bitmask_idx = (pos - 1) / bits_per_mask;

    // If pos is beyond our bitmasks, start searching from the very last bitmask.
    bitmask_idx = if (bitmask_idx >= self.bitmasks.len) last_bitmask_idx else bitmask_idx;

    const chunk_start = bitmask_idx * bits_per_mask;
    const mask = ~@as(mask_type, 0) << @intCast(chunk_start + bits_per_mask -| pos);
    const masked_value = self.bitmasks[bitmask_idx] & mask;

    if (masked_value != 0) {
        // Find the rightmost set bit (highest position before pos)
        const trailing_zeros = @ctz(masked_value);
        return (bitmask_idx * bits_per_mask) + (bits_per_mask - trailing_zeros - 1);
    }

    // If no newline was found, check previous masks.
    while (bitmask_idx > 0) {
        bitmask_idx -= 1;
        const value = self.bitmasks[bitmask_idx];
        if (value != 0) {
            return (bitmask_idx * bits_per_mask) + @clz(value);
        }
    }

    return error.NoNewlinesBefore;
}

test "initWithCapacity and deinit" {
    const allocator = std.testing.allocator;

    const lines = try Self.initWithCapacity(allocator, 10);
    defer lines.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 10), lines.bitmasks.len);
}

test "newlineAfter with no bitmasks" {
    const lines = Self{ .bitmasks = &[_]u64{} };

    const result = lines.newlineAfter(0);
    try std.testing.expectError(error.NoNewlinesAfter, result);
}

test "newlineAfter with single newline in first mask" {
    var bitmasks = [_]u64{1 << 63}; // Newline at position 0
    const lines = Self{ .bitmasks = &bitmasks };

    const pos = try lines.newlineAfter(0);
    try std.testing.expectEqual(@as(usize, 0), pos);
}

test "newlineAfter finds newlines from given position" {
    // Set bits at positions 10, 20, 30 (from MSB)
    var bitmasks = [_]u64{(1 << (63 - 10)) | (1 << (63 - 20)) | (1 << (63 - 30))};
    const lines = Self{ .bitmasks = &bitmasks };

    // Looking after position 0 should find the newline at position 10
    const pos1 = try lines.newlineAfter(0);
    try std.testing.expectEqual(@as(usize, 10), pos1);

    // Looking from position 10 finds the newline at position 10
    const pos2 = try lines.newlineAfter(10);
    try std.testing.expectEqual(@as(usize, 10), pos2); // Newline is at position 10

    // Looking from position 20 finds the newline at position 20
    const pos3 = try lines.newlineAfter(20);
    try std.testing.expectEqual(@as(usize, 20), pos3); // Newline is at position 20
}

test "newlineAfter crosses mask boundaries" {
    var bitmasks = [_]u64{
        0, // First mask has no newlines
        1 << 63, // Second mask has newline at position 0
    };
    const lines = Self{ .bitmasks = &bitmasks };

    // Looking in the first mask should find newline in second mask
    const pos = try lines.newlineAfter(0);
    try std.testing.expectEqual(@as(usize, 64), pos); // Found at position 64 (start of second mask)
}

test "newlineAfter with all zeros returns error" {
    var bitmasks = [_]u64{ 0, 0, 0 };
    const lines = Self{ .bitmasks = &bitmasks };

    const result = lines.newlineAfter(0);
    try std.testing.expectError(error.NoNewlinesAfter, result);
}

test "newlineAfter starting from middle of bitmask" {
    // Newlines at positions 5, 15, 25, 35, 45, 55
    var bitmask: u64 = 0;
    bitmask |= 1 << (63 - 5);
    bitmask |= 1 << (63 - 15);
    bitmask |= 1 << (63 - 25);
    bitmask |= 1 << (63 - 35);
    bitmask |= 1 << (63 - 45);
    bitmask |= 1 << (63 - 55);

    var bitmasks = [_]u64{bitmask};
    const lines = Self{ .bitmasks = &bitmasks };

    // Starting after position 30 should find newline at 35
    const pos = try lines.newlineAfter(30);
    try std.testing.expectEqual(@as(usize, 35), pos); // Found at position 35
}

test "newlineAfter at exact bitmask boundary" {
    var bitmasks = [_]u64{
        0, // No newlines in first mask
        1 << 63, // Newline at position 0 of second mask
    };
    const lines = Self{ .bitmasks = &bitmasks };

    // Starting at position 64 (start of second mask)
    const pos = try lines.newlineAfter(64);
    try std.testing.expectEqual(@as(usize, 64), pos);
}

test "newlineAfter near end of bitmasks array" {
    var bitmasks = [_]u64{
        0, // No newlines
        0, // No newlines
        1 << 60, // Newline at position 3
    };
    const lines = Self{ .bitmasks = &bitmasks };

    // Starting at position 128 (third mask)
    const pos = try lines.newlineAfter(128);
    try std.testing.expectEqual(@as(usize, 131), pos); // 128 + 3
}

test "newlineAfter with position beyond last mask" {
    var bitmasks = [_]u64{1 << 63};
    const lines = Self{ .bitmasks = &bitmasks };

    // Position 100 is beyond the single 64-bit mask
    const result = lines.newlineAfter(100);
    try std.testing.expectError(error.NoNewlinesAfter, result);
}

test "newlineAfter finds next newline when starting just before one" {
    // Newlines at positions 10 and 20
    var bitmasks = [_]u64{(1 << (63 - 10)) | (1 << (63 - 20))};
    const lines = Self{ .bitmasks = &bitmasks };

    // Looking from position 9 should find newline at position 10
    const pos1 = try lines.newlineAfter(9);
    try std.testing.expectEqual(@as(usize, 10), pos1);

    // Looking from position 19 should find newline at position 20
    const pos2 = try lines.newlineAfter(19);
    try std.testing.expectEqual(@as(usize, 20), pos2);
}

test "newlineAfter crosses multiple mask boundaries" {
    var bitmasks = [_]u64{
        0, // First mask: no newlines
        0, // Second mask: no newlines
        0, // Third mask: no newlines
        1 << (63 - 15), // Fourth mask: newline at position 15
    };
    const lines = Self{ .bitmasks = &bitmasks };

    // Starting at position 10 (in first mask) should find newline in fourth mask
    // The newline is at position 15 of the fourth mask (mask index 3)
    // From position 10 in first mask, should find newline at position 207 (3*64 + 15)
    const pos = try lines.newlineAfter(10);
    try std.testing.expectEqual(@as(usize, 207), pos); // Found at position 207
}

test "newlineAfter fails when all newlines are before given position" {
    // Newlines at positions 5, 10, 15, 20
    var bitmasks = [_]u64{
        (1 << (63 - 5)) | (1 << (63 - 10)) | (1 << (63 - 15)) | (1 << (63 - 20)),
    };
    const lines = Self{ .bitmasks = &bitmasks };

    // Looking from position 21 onwards - all newlines are before this
    const result1 = lines.newlineAfter(21);
    try std.testing.expectError(error.NoNewlinesAfter, result1);

    // Looking from position 50 - all newlines are before this
    const result2 = lines.newlineAfter(50);
    try std.testing.expectError(error.NoNewlinesAfter, result2);

    // Edge case: looking from position 63 (last bit of mask)
    const result3 = lines.newlineAfter(63);
    try std.testing.expectError(error.NoNewlinesAfter, result3);
}

test "newlineAfter behavior at exact newline position" {
    // Newlines at positions 10, 20, 30
    var bitmasks = [_]u64{
        (1 << (63 - 10)) | (1 << (63 - 20)) | (1 << (63 - 30)),
    };
    const lines = Self{ .bitmasks = &bitmasks };

    // When we're AT position 10 (which has a newline), it returns that position
    const at_newline = try lines.newlineAfter(10);
    try std.testing.expectEqual(@as(usize, 10), at_newline);

    // If we want the NEXT newline after 10, we need to start at position 11
    const after_newline = try lines.newlineAfter(11);
    try std.testing.expectEqual(@as(usize, 20), after_newline); // Next newline is at 20
}

test "newlineBefore with no bitmasks" {
    const lines = Self{ .bitmasks = &[_]u64{} };

    const result = lines.newlineBefore(10);
    try std.testing.expectError(error.NoNewlinesBefore, result);
}

test "newlineBefore at position 0" {
    var bitmasks = [_]u64{1 << 63}; // Newline at position 0
    const lines = Self{ .bitmasks = &bitmasks };

    const result = lines.newlineBefore(0);
    try std.testing.expectError(error.NoNewlinesBefore, result);
}

test "newlineBefore with single newline" {
    var bitmasks = [_]u64{1 << (63 - 10)}; // Newline at position 10
    const lines = Self{ .bitmasks = &bitmasks };

    // Looking back from position 20 should find newline at position 10
    const idx1 = try lines.newlineBefore(20);
    try std.testing.expectEqual(@as(usize, 10), idx1); // Found at position 10

    // Looking back from position 11 should find newline at position 10
    const idx2 = try lines.newlineBefore(11);
    try std.testing.expectEqual(@as(usize, 10), idx2); // Found at position 10

    // Looking back from position 5 should fail (no newlines before it)
    const result = lines.newlineBefore(5);
    try std.testing.expectError(error.NoNewlinesBefore, result);
}

test "newlineBefore with multiple newlines" {
    // Newlines at positions 10, 20, 30
    var bitmasks = [_]u64{
        (1 << (63 - 10)) | (1 << (63 - 20)) | (1 << (63 - 30)),
    };
    const lines = Self{ .bitmasks = &bitmasks };

    // From position 25, should find newline at 20
    const idx1 = try lines.newlineBefore(25);
    try std.testing.expectEqual(@as(usize, 20), idx1);

    // From position 20, should find newline at 10
    const idx2 = try lines.newlineBefore(20);
    try std.testing.expectEqual(@as(usize, 10), idx2);

    // From position 10, should fail (no newlines before position 10)
    const result = lines.newlineBefore(10);
    try std.testing.expectError(error.NoNewlinesBefore, result);
}

test "newlineBefore crosses mask boundary" {
    var bitmasks = [_]u64{
        1 << (63 - 50), // First mask: newline at position 50
        1 << (63 - 10), // Second mask: newline at position 74 (64 + 10)
    };
    const lines = Self{ .bitmasks = &bitmasks };

    // From position 70 (in second mask), should find newline at 50
    const idx = try lines.newlineBefore(70);
    try std.testing.expectEqual(@as(usize, 50), idx);
}

test "newlineBefore crosses multiple mask boundaries" {
    var bitmasks = [_]u64{
        1 << (63 - 10), // First mask: newline at position 10
        0, // Second mask: no newlines
        0, // Third mask: no newlines
        1 << (63 - 20), // Fourth mask: newline at position 212 (192 + 20)
    };
    const lines = Self{ .bitmasks = &bitmasks };

    // From position 200 (in fourth mask), should find newline at 10
    const idx = try lines.newlineBefore(200);
    try std.testing.expectEqual(@as(usize, 10), idx);
}

test "newlineBefore with position beyond last mask" {
    var bitmasks = [_]u64{
        1 << (63 - 10), // Newline at position 10
    };
    const lines = Self{ .bitmasks = &bitmasks };

    // Position 100 is beyond our single mask, but should still find newline at 10
    const idx = try lines.newlineBefore(100);
    try std.testing.expectEqual(@as(usize, 10), idx); // Found at position 10
}

test "newlineBefore fails when all newlines are after position" {
    // Newlines at positions 30, 40, 50
    var bitmasks = [_]u64{
        (1 << (63 - 30)) | (1 << (63 - 40)) | (1 << (63 - 50)),
    };
    const lines = Self{ .bitmasks = &bitmasks };

    // Looking back from position 29 - all newlines are after this
    const result1 = lines.newlineBefore(29);
    try std.testing.expectError(error.NoNewlinesBefore, result1);

    // Looking back from position 10 - all newlines are after this
    const result2 = lines.newlineBefore(10);
    try std.testing.expectError(error.NoNewlinesBefore, result2);
}

test "newlineBefore at exact mask boundary" {
    var bitmasks = [_]u64{
        1 << (63 - 63), // First mask: newline at position 63 (last position)
        1 << (63 - 0), // Second mask: newline at position 64 (first position)
    };
    const lines = Self{ .bitmasks = &bitmasks };

    // From position 64 (start of second mask), should find newline at 63
    const idx1 = try lines.newlineBefore(64);
    try std.testing.expectEqual(@as(usize, 63), idx1);

    // From position 65, should find newline at 64
    const idx2 = try lines.newlineBefore(65);
    try std.testing.expectEqual(@as(usize, 64), idx2);
}

test "newlineBefore just after a newline" {
    // Newlines at positions 10, 20, 30
    var bitmasks = [_]u64{
        (1 << (63 - 10)) | (1 << (63 - 20)) | (1 << (63 - 30)),
    };
    const lines = Self{ .bitmasks = &bitmasks };

    // From position 21 (just after newline at 20), should find newline at 20
    const idx1 = try lines.newlineBefore(21);
    try std.testing.expectEqual(@as(usize, 20), idx1);

    // From position 31 (just after newline at 30), should find newline at 30
    const idx2 = try lines.newlineBefore(31);
    try std.testing.expectEqual(@as(usize, 30), idx2);
}
