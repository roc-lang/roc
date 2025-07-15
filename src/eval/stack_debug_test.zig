const std = @import("std");
const testing = std.testing;
const stack = @import("stack.zig");

test "debug stack allocation" {
    const allocator = testing.allocator;

    // Create a simple stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    std.debug.print("\n=== Stack test ===\n", .{});
    std.debug.print("Stack initial state: start={*}, capacity={}\n", .{ eval_stack.start, eval_stack.capacity });

    // Try allocating some memory
    const ptr1 = try eval_stack.alloca(16, .@"8");
    std.debug.print("Allocated 16 bytes: {*}\n", .{ptr1});

    const ptr2 = try eval_stack.alloca(32, .@"16");
    std.debug.print("Allocated 32 bytes: {*}\n", .{ptr2});

    // Write and read back
    const test_ptr = @as(*i128, @ptrCast(@alignCast(ptr1)));
    test_ptr.* = 42;
    const read_value = test_ptr.*;
    std.debug.print("Write/read test: wrote 42, read {}\n", .{read_value});
    try testing.expectEqual(@as(i128, 42), read_value);
}

test "stack alignment" {
    const allocator = testing.allocator;
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Test various alignments
    const ptr1 = try eval_stack.alloca(1, .@"1");
    const ptr2 = try eval_stack.alloca(2, .@"2");
    const ptr4 = try eval_stack.alloca(4, .@"4");
    const ptr8 = try eval_stack.alloca(8, .@"8");
    const ptr16 = try eval_stack.alloca(16, .@"16");

    // Check alignment
    try testing.expect(@intFromPtr(ptr1) % 1 == 0);
    try testing.expect(@intFromPtr(ptr2) % 2 == 0);
    try testing.expect(@intFromPtr(ptr4) % 4 == 0);
    try testing.expect(@intFromPtr(ptr8) % 8 == 0);
    try testing.expect(@intFromPtr(ptr16) % 16 == 0);
}
