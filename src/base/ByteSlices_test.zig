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

test "ByteSlices: alignment padding calculation" {
    const allocator = testing.allocator;

    var slices = ByteSlices{ .entries = .{} };
    defer slices.entries.deinit(allocator);

    // Start with a known state
    const first = "x"; // 1 byte
    const idx1 = try slices.append(allocator, first);

    // After storing u32 length (4 bytes) + 1 byte data, we have 5 bytes total
    // Next u32 should need 3 bytes of padding to align to 8
    const second = "yy"; // 2 bytes
    const idx2 = try slices.append(allocator, second);

    // Verify both can still be retrieved
    try testing.expectEqualSlices(u8, first, slices.slice(idx1));
    try testing.expectEqualSlices(u8, second, slices.slice(idx2));

    // Check the actual buffer layout
    const buffer = slices.entries.items.items;

    // First entry: 4 bytes for length + 1 byte data = 5 bytes
    try testing.expectEqual(@as(u32, 1), @as(*const u32, @ptrCast(@alignCast(buffer.ptr))).*);
    try testing.expectEqual(@as(u8, 'x'), buffer[4]);

    // Should have padding at indices 5, 6, 7
    // Then second length at index 8
    const second_len_ptr = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + 8)));
    try testing.expectEqual(@as(u32, 2), second_len_ptr.*);
    try testing.expectEqual(@as(u8, 'y'), buffer[12]);
    try testing.expectEqual(@as(u8, 'y'), buffer[13]);
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
