const std = @import("std");
const testing = std.testing;

test "simple deterministic serialization" {
    const allocator = testing.allocator;

    // Create a simple struct to test
    const TestStruct = struct {
        a: u32,
        b: u64,
        c: [4]u8,
    };

    // Create test data
    const test_data = TestStruct{
        .a = 42,
        .b = 1234567890,
        .c = [4]u8{ 1, 2, 3, 4 },
    };

    // Serialize to two different buffers
    const size = @sizeOf(TestStruct);
    const buffer1 = try allocator.alloc(u8, size);
    defer allocator.free(buffer1);
    const buffer2 = try allocator.alloc(u8, size);
    defer allocator.free(buffer2);

    // Copy the struct bytes to both buffers
    @memcpy(buffer1, std.mem.asBytes(&test_data));
    @memcpy(buffer2, std.mem.asBytes(&test_data));

    // Both buffers should be identical
    try testing.expectEqualSlices(u8, buffer1, buffer2);
}

test "zeroed allocator determinism" {
    // Test that std.mem.zeroes creates deterministic output
    const TestAllocator = std.mem.Allocator;

    const alloc1 = std.mem.zeroes(TestAllocator);
    const alloc2 = std.mem.zeroes(TestAllocator);

    // Convert to bytes and compare
    const bytes1 = std.mem.asBytes(&alloc1);
    const bytes2 = std.mem.asBytes(&alloc2);

    try testing.expectEqualSlices(u8, bytes1, bytes2);
}

test "pointer to zero determinism" {
    // Test different ways of creating zero pointers
    const ptr1: [*]allowzero u8 = @ptrFromInt(0);
    const ptr2: [*]allowzero u8 = @ptrFromInt(0);

    try testing.expectEqual(ptr1, ptr2);

    // Test with slices
    const slice1 = ptr1[0..0];
    const slice2 = ptr2[0..0];

    try testing.expectEqualSlices(u8, slice1, slice2);
}

test "undefined replacement strategies" {
    const TestStruct = struct {
        ptr: ?*u32,
        value: u64,
    };

    // Instead of undefined, use explicit zero values
    const test1 = TestStruct{
        .ptr = null, // Instead of undefined
        .value = 0, // Instead of undefined
    };

    const test2 = TestStruct{
        .ptr = null,
        .value = 0,
    };

    const bytes1 = std.mem.asBytes(&test1);
    const bytes2 = std.mem.asBytes(&test2);

    try testing.expectEqualSlices(u8, bytes1, bytes2);
}
