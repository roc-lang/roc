//! Tests for the stack implementation.
const std = @import("std");
const collections = @import("collections");
const Stack = @import("../stack.zig").Stack;
const StackOverflow = @import("../stack.zig").StackOverflow;

test "Stack.initCapacity and deinit" {
    var stack = try Stack.initCapacity(std.testing.allocator, 1024);
    defer stack.deinit();

    try std.testing.expectEqual(@as(u32, 1024), stack.capacity);
    try std.testing.expectEqual(@as(u32, 0), stack.used);
    try std.testing.expect(stack.isEmpty());
}

test "Stack.alloca basic allocation" {
    var stack = try Stack.initCapacity(std.testing.allocator, 1024);
    defer stack.deinit();

    const ptr1 = try stack.alloca(10, @enumFromInt(0));
    try std.testing.expectEqual(@as(u32, 10), stack.used);

    const ptr2 = try stack.alloca(20, @enumFromInt(0));
    try std.testing.expectEqual(@as(u32, 30), stack.used);

    // The pointers should be different
    try std.testing.expect(@intFromPtr(ptr1) != @intFromPtr(ptr2));
    try std.testing.expectEqual(@as(usize, 10), @intFromPtr(ptr2) - @intFromPtr(ptr1));
}

test "Stack.alloca with alignment" {
    var stack = try Stack.initCapacity(std.testing.allocator, 4096);
    defer stack.deinit();

    // Test alignments from 1 to 16
    const alignments = [_]u32{ 1, 2, 4, 8, 16 };

    // Test different misalignments
    for (0..17) |misalign| {
        // Reset stack for each misalignment test
        stack.used = 0;

        // Create initial misalignment
        if (misalign > 0) {
            _ = try stack.alloca(@intCast(misalign), @enumFromInt(0));
        }

        // Test each alignment with the current misalignment
        for (alignments) |alignment| {
            const start_used = stack.used;
            const allocation_size: u32 = 32; // Use a consistent size for testing

            const aligned_ptr = try stack.alloca(allocation_size, @enumFromInt(std.math.log2_int(u32, alignment)));

            // Verify the pointer is properly aligned
            try std.testing.expectEqual(@as(usize, 0), @intFromPtr(aligned_ptr) % alignment);

            // Calculate expected padding
            const start_addr = @intFromPtr(stack.start) + start_used;
            const aligned_addr = std.mem.alignForward(usize, start_addr, alignment);
            const expected_padding = @as(u32, @intCast(aligned_addr - start_addr));

            // Verify the stack used the expected amount of space
            const expected_used = start_used + expected_padding + allocation_size;
            try std.testing.expectEqual(expected_used, stack.used);
        }
    }

    // Additional test: allocate with various alignments in sequence
    stack.used = 0;
    for (alignments) |alignment| {
        // Create some misalignment
        _ = try stack.alloca(3, @enumFromInt(0));

        const before_used = stack.used;
        const ptr = try stack.alloca(alignment * 2, @enumFromInt(std.math.log2_int(u32, alignment)));

        // Verify alignment
        try std.testing.expectEqual(@as(usize, 0), @intFromPtr(ptr) % alignment);

        // Verify we used at least the requested amount
        try std.testing.expect(stack.used >= before_used + alignment * 2);
    }
}

test "Stack.alloca overflow" {
    var stack = try Stack.initCapacity(std.testing.allocator, 100);
    defer stack.deinit();

    // This should succeed
    _ = try stack.alloca(50, @enumFromInt(0));

    // This should fail (would total 150 bytes)
    try std.testing.expectError(StackOverflow.StackOverflow, stack.alloca(100, @enumFromInt(0)));

    // Stack should still be in valid state
    try std.testing.expectEqual(@as(u32, 50), stack.used);
}

test "Stack.restore" {
    var stack = try Stack.initCapacity(std.testing.allocator, 1024);
    defer stack.deinit();

    const checkpoint = stack.next();
    _ = try stack.alloca(100, @enumFromInt(0));
    try std.testing.expectEqual(@as(u32, 100), stack.used);

    stack.restore(checkpoint);
    try std.testing.expectEqual(@as(u32, 0), stack.used);

    // Allocate again after restore
    const ptr1 = try stack.alloca(50, @enumFromInt(0));
    try std.testing.expectEqual(@intFromPtr(checkpoint), @intFromPtr(ptr1));
}

test "Stack.isEmpty" {
    var stack = try Stack.initCapacity(std.testing.allocator, 100);
    defer stack.deinit();

    try std.testing.expect(stack.isEmpty());
    try std.testing.expectEqual(@as(u32, 100), stack.available());

    _ = try stack.alloca(30, @enumFromInt(0));
    try std.testing.expect(!stack.isEmpty());
    try std.testing.expectEqual(@as(u32, 70), stack.available());
}

test "Stack zero-size allocation" {
    var stack = try Stack.initCapacity(std.testing.allocator, 100);
    defer stack.deinit();

    const ptr1 = try stack.alloca(0, @enumFromInt(0));
    const ptr2 = try stack.alloca(0, @enumFromInt(0));

    // Zero-size allocations should return the same pointer
    try std.testing.expectEqual(@intFromPtr(ptr1), @intFromPtr(ptr2));
    try std.testing.expectEqual(@as(u32, 0), stack.used);
}

test "Stack memory is aligned to max_roc_alignment" {
    var stack = try Stack.initCapacity(std.testing.allocator, 1024);
    defer stack.deinit();

    // Check that the start pointer is aligned to max_roc_alignment
    const start_addr = @intFromPtr(stack.start);
    const max_alignment_value = collections.max_roc_alignment.toByteUnits();
    try std.testing.expectEqual(@as(usize, 0), start_addr % max_alignment_value);

    // Also verify after some allocations
    _ = try stack.alloca(100, @enumFromInt(0));
    _ = try stack.alloca(200, @enumFromInt(0));

    // The start pointer should still be aligned
    try std.testing.expectEqual(@as(usize, 0), start_addr % max_alignment_value);
}
