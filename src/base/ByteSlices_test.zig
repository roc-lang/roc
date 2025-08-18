const std = @import("std");
const testing = std.testing;
const ByteSlices = @import("ByteSlices.zig");
const collections = @import("collections");

test "ByteSlices: store and retrieve single slice" {
    const allocator = testing.allocator;

    var slices = ByteSlices{ .entries = .{} };
    defer slices.entries.deinit(allocator);

    const data = "Hello, World!";
    const idx = try slices.append(allocator, data);

    const retrieved = slices.slice(idx);
    try testing.expectEqualSlices(u8, data, retrieved);
}

test "ByteSlices: store and retrieve multiple slices" {
    const allocator = testing.allocator;

    var slices = ByteSlices{ .entries = .{} };
    defer slices.entries.deinit(allocator);

    const data1 = "First";
    const data2 = "Second string";
    const data3 = "Third!";

    const idx1 = try slices.append(allocator, data1);
    const idx2 = try slices.append(allocator, data2);
    const idx3 = try slices.append(allocator, data3);

    try testing.expectEqualSlices(u8, data1, slices.slice(idx1));
    try testing.expectEqualSlices(u8, data2, slices.slice(idx2));
    try testing.expectEqualSlices(u8, data3, slices.slice(idx3));
}

test "ByteSlices: various lengths requiring different padding" {
    const allocator = testing.allocator;

    var slices = ByteSlices{ .entries = .{} };
    defer slices.entries.deinit(allocator);

    // Test strings of various lengths that will require different padding amounts
    const test_cases = [_][]const u8{
        "", // Empty string
        "a", // 1 byte
        "ab", // 2 bytes
        "abc", // 3 bytes
        "abcd", // 4 bytes (aligns perfectly after u32 length)
        "abcde", // 5 bytes
        "Hello!", // 6 bytes
        "1234567", // 7 bytes
        "12345678", // 8 bytes (2x alignment)
        "123456789", // 9 bytes
    };

    var indices: [test_cases.len]ByteSlices.Idx = undefined;

    // Store all strings
    for (test_cases, 0..) |data, i| {
        indices[i] = try slices.append(allocator, data);
    }

    // Verify all can be retrieved correctly
    for (test_cases, 0..) |expected, i| {
        const retrieved = slices.slice(indices[i]);
        try testing.expectEqualSlices(u8, expected, retrieved);
    }
}

test "ByteSlices: variable length encoding layout" {
    const allocator = testing.allocator;

    var slices = ByteSlices{ .entries = .{} };
    defer slices.entries.deinit(allocator);

    // Start with a known state
    const first = "x"; // 1 byte
    const idx1 = try slices.append(allocator, first);

    // After storing 1 byte length + 1 byte data, we have 2 bytes total
    const second = "yy"; // 2 bytes
    const idx2 = try slices.append(allocator, second);

    // Verify both can still be retrieved
    try testing.expectEqualSlices(u8, first, slices.slice(idx1));
    try testing.expectEqualSlices(u8, second, slices.slice(idx2));

    // Check the actual buffer layout with variable-length encoding
    const buffer = slices.entries.items.items;

    // First entry: 1 byte for length (value 1) + 1 byte data = 2 bytes
    try testing.expectEqual(@as(u8, 1), buffer[0]); // Length encoded as single byte
    try testing.expectEqual(@as(u8, 'x'), buffer[1]);

    // Second entry starts at index 2: 1 byte for length (value 2) + 2 bytes data
    try testing.expectEqual(@as(u8, 2), buffer[2]); // Length encoded as single byte
    try testing.expectEqual(@as(u8, 'y'), buffer[3]);
    try testing.expectEqual(@as(u8, 'y'), buffer[4]);
}

test "ByteSlices: large strings" {
    const allocator = testing.allocator;

    var slices = ByteSlices{ .entries = .{} };
    defer slices.entries.deinit(allocator);

    // Create a large string
    var large_data: [1000]u8 = undefined;
    for (&large_data, 0..) |*byte, i| {
        byte.* = @as(u8, @intCast(i % 256));
    }

    const idx = try slices.append(allocator, &large_data);
    const retrieved = slices.slice(idx);

    try testing.expectEqualSlices(u8, &large_data, retrieved);
}

test "ByteSlices: interleaved small and large" {
    const allocator = testing.allocator;

    var slices = ByteSlices{ .entries = .{} };
    defer slices.entries.deinit(allocator);

    const small1 = "small";
    var large: [500]u8 = undefined;
    @memset(&large, 'L');
    const small2 = "tiny";

    const idx1 = try slices.append(allocator, small1);
    const idx2 = try slices.append(allocator, &large);
    const idx3 = try slices.append(allocator, small2);

    try testing.expectEqualSlices(u8, small1, slices.slice(idx1));
    try testing.expectEqualSlices(u8, &large, slices.slice(idx2));
    try testing.expectEqualSlices(u8, small2, slices.slice(idx3));
}
