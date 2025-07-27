//! Tests for CompactWriter, verifying its scatter-gather I/O functionality,
//! alignment handling, and serialization capabilities.
//! These tests ensure that CompactWriter correctly manages iovecs, handles
//! padding for different data alignments, and maintains data integrity
//! when serializing various data types and slices.

const std = @import("std");
const CompactWriter = @import("../CompactWriter.zig").CompactWriter;
const testing = std.testing;

test "CompactWriter basic functionality" {
    const allocator = testing.allocator;

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(allocator);

    // Test appendAlloc
    const test_struct = struct {
        value: u32,
        flag: bool,
    };

    const ptr = try writer.appendAlloc(allocator, test_struct);
    ptr.* = .{
        .value = 42,
        .flag = true,
    };

    // Verify the writer has tracked the allocation
    try testing.expect(writer.iovecs.items.len > 0);
    try testing.expect(writer.total_bytes >= @sizeOf(test_struct));

    // Test that writer tracked the allocation correctly
    try testing.expect(writer.total_bytes >= @sizeOf(test_struct));
}

test "CompactWriter appendSlice" {
    const allocator = testing.allocator;

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(allocator);

    // Create some test data
    const data = [_]u32{ 1, 2, 3, 4, 5 };

    const offset = try writer.appendSlice(allocator, &data, data.len);

    // Verify the writer has tracked the slice
    try testing.expect(writer.iovecs.items.len > 0);
    try testing.expect(offset > 0);
    try testing.expect(writer.total_bytes >= @sizeOf(u32) * data.len);

    // Test that writer tracked the slice correctly
    try testing.expect(writer.total_bytes >= @sizeOf(u32) * data.len);
}

test "CompactWriter alignment padding" {
    const allocator = testing.allocator;

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(allocator);

    // First append a u8 (1-byte aligned)
    const byte_ptr = try writer.appendAlloc(allocator, u8);
    byte_ptr.* = 42;

    // Should be at 1 byte total
    try testing.expectEqual(@as(usize, 1), writer.total_bytes);

    // Now append a u32 (4-byte aligned)
    const u32_ptr = try writer.appendAlloc(allocator, u32);
    u32_ptr.* = 1337;

    // Should have padded to align u32, so we're at 4 + 4 = 8 bytes
    try testing.expectEqual(@as(usize, 8), writer.total_bytes);

    // Should have 3 iovecs: u8 data, padding, u32 data
    try testing.expectEqual(@as(usize, 3), writer.iovecs.items.len);
}

test "CompactWriter multiple allocations" {
    const allocator = testing.allocator;

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(allocator);

    // Append multiple items
    const item1 = try writer.appendAlloc(allocator, u64);
    item1.* = 100;

    const item2 = try writer.appendAlloc(allocator, u32);
    item2.* = 200;

    const item3 = try writer.appendAlloc(allocator, u16);
    item3.* = 300;

    // Verify all items were tracked
    try testing.expect(writer.iovecs.items.len >= 3);
    try testing.expect(writer.total_bytes >= @sizeOf(u64) + @sizeOf(u32) + @sizeOf(u16));

    // Verify values are correct
    try testing.expectEqual(@as(u64, 100), item1.*);
    try testing.expectEqual(@as(u32, 200), item2.*);
    try testing.expectEqual(@as(u16, 300), item3.*);
}

test "CompactWriter up-front padding with various alignments" {
    const allocator = testing.allocator;

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(allocator);

    // Start with u8 (1-byte aligned)
    _ = try writer.appendAlloc(allocator, u8);
    try testing.expectEqual(@as(usize, 1), writer.total_bytes);

    // Add u16 (2-byte aligned) - should pad 1 byte first
    _ = try writer.appendAlloc(allocator, u16);
    try testing.expectEqual(@as(usize, 4), writer.total_bytes); // 1 + 1 padding + 2

    // Add u32 (4-byte aligned) - already at 4, no padding needed
    _ = try writer.appendAlloc(allocator, u32);
    try testing.expectEqual(@as(usize, 8), writer.total_bytes); // 4 + 4

    // Add u8 again - no padding needed
    _ = try writer.appendAlloc(allocator, u8);
    try testing.expectEqual(@as(usize, 9), writer.total_bytes); // 8 + 1

    // Add u64 (8-byte aligned) - should pad to 16
    _ = try writer.appendAlloc(allocator, u64);
    try testing.expectEqual(@as(usize, 24), writer.total_bytes); // 9 + 7 padding + 8

    // Test with a struct that has specific alignment
    const AlignedStruct = struct {
        value: u32,
        data: u64 align(8),
    };
    _ = try writer.appendAlloc(allocator, AlignedStruct);
    // Already at 24 (divisible by 8), so no padding needed
    try testing.expectEqual(@as(usize, 24 + @sizeOf(AlignedStruct)), writer.total_bytes);
}

test "CompactWriter slice alignment" {
    const allocator = testing.allocator;

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(allocator);

    // Start with a u8
    _ = try writer.appendAlloc(allocator, u8);
    try testing.expectEqual(@as(usize, 1), writer.total_bytes);

    // Add a slice of u32s - should pad to 4-byte alignment first
    const data = [_]u32{ 10, 20, 30 };
    _ = try writer.appendSlice(allocator, &data, data.len);
    // 1 + 3 padding + (3 * 4) = 16
    try testing.expectEqual(@as(usize, 16), writer.total_bytes);

    // Add a slice of u64s - already at 16 (divisible by 8), no padding
    const data64 = [_]u64{ 100, 200 };
    _ = try writer.appendSlice(allocator, &data64, data64.len);
    // 16 + (2 * 8) = 32
    try testing.expectEqual(@as(usize, 32), writer.total_bytes);
}

test "CompactWriter brute-force appendSlice alignment" {
    const allocator = testing.allocator;

    // Test all slice lengths from 0 to 8 for different types
    // Verifies alignment padding works correctly for all cases
    const test_types = .{
        u8, // 1-byte alignment
        u16, // 2-byte alignment
        u32, // 4-byte alignment
        u64, // 8-byte alignment
    };

    inline for (test_types) |T| {
        var length: usize = 0;
        while (length <= 8) : (length += 1) {
            var writer = CompactWriter{
                .iovecs = .{},
                .total_bytes = 0,
            };
            defer writer.iovecs.deinit(allocator);

            // Create test data
            var data: [8]T = undefined;
            for (data, 0..) |*item, i| {
                item.* = @as(T, @intCast(i + 1));
            }

            // Add a u8 first to create misalignment
            const misalign_ptr = try writer.appendAlloc(allocator, u8);
            misalign_ptr.* = 99;

            const start_bytes = writer.total_bytes;
            try testing.expectEqual(@as(usize, 1), start_bytes);

            // Append the slice
            if (length > 0) {
                const offset = try writer.appendSlice(allocator, &data, length);

                // Verify padding was added correctly
                const expected_padding = std.mem.alignForward(usize, start_bytes, @alignOf(T)) - start_bytes;
                const actual_bytes_written = writer.total_bytes - start_bytes;
                const expected_bytes = expected_padding + @sizeOf(T) * length;
                try testing.expectEqual(expected_bytes, actual_bytes_written);

                // Verify offset is correct
                try testing.expectEqual(writer.total_bytes, offset);
            }

            // Write to buffer and verify data integrity
            var buffer: [256]u8 align(16) = undefined;
            const written = try writer.writeToBuffer(&buffer);

            // Verify first byte
            try testing.expectEqual(@as(u8, 99), written[0]);

            // Verify slice data (if any)
            if (length > 0) {
                const data_start = std.mem.alignForward(usize, 1, @alignOf(T));
                const data_ptr = @as([*]const T, @ptrCast(@alignCast(written.ptr + data_start)));

                var i: usize = 0;
                while (i < length) : (i += 1) {
                    try testing.expectEqual(@as(T, @intCast(i + 1)), data_ptr[i]);
                }
            }
        }
    }
}
