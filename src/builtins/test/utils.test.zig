//! Unit tests for utils functions

const std = @import("std");
const builtins = @import("builtins");

const TestEnv = @import("../utils.zig").TestEnv;
const calculateCapacity = @import("../utils.zig").calculateCapacity;
const allocateWithRefcount = @import("../utils.zig").allocateWithRefcount;

test "increfC, refcounted data" {
    var mock_rc: isize = 17;
    const ptr_to_refcount: *isize = &mock_rc;
    @import("../utils.zig").increfRcPtrC(ptr_to_refcount, 2);
    try std.testing.expectEqual(mock_rc, 19);
}

test "increfC, static data" {
    var mock_rc: isize = @import("../utils.zig").REFCOUNT_STATIC_DATA;
    const ptr_to_refcount: *isize = &mock_rc;
    @import("../utils.zig").increfRcPtrC(ptr_to_refcount, 2);
    try std.testing.expectEqual(mock_rc, @import("../utils.zig").REFCOUNT_STATIC_DATA);
}

test "decrefC, refcounted data" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var mock_rc: isize = 17;
    const ptr_to_refcount: *isize = &mock_rc;
    @import("../utils.zig").decrefRcPtrC(@ptrCast(ptr_to_refcount), 8, false, test_env.getOps());
    try std.testing.expectEqual(mock_rc, 16);
}

test "decrefC, static data" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var mock_rc: isize = @import("../utils.zig").REFCOUNT_STATIC_DATA;
    const ptr_to_refcount: *isize = &mock_rc;
    @import("../utils.zig").decrefRcPtrC(@ptrCast(ptr_to_refcount), 8, false, test_env.getOps());
    try std.testing.expectEqual(mock_rc, @import("../utils.zig").REFCOUNT_STATIC_DATA);
}

test "TestEnv basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Should start with no allocations
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());

    // Get ops should work
    const ops = test_env.getOps();
    // Function pointers are non-null by design, just verify we can get ops
    _ = ops;
}

test "TestEnv allocation tracking" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const ops = test_env.getOps();

    // Test allocation
    var alloc_request = @import("host_abi.zig").RocAlloc{
        .alignment = 8,
        .length = 32,
        .answer = undefined,
    };

    ops.roc_alloc(&alloc_request, ops.env);
    try std.testing.expectEqual(@as(usize, 1), test_env.getAllocationCount());

    // Test deallocation
    var dealloc_request = @import("host_abi.zig").RocDealloc{
        .alignment = 8,
        .ptr = alloc_request.answer,
    };

    ops.roc_dealloc(&dealloc_request, ops.env);
    try std.testing.expectEqual(@as(usize, 0), test_env.getAllocationCount());
}

test "calculateCapacity with various inputs" {
    // Test zero capacity
    try std.testing.expectEqual(@as(usize, 0), calculateCapacity(0, 0, 1));

    // Test basic growth
    try std.testing.expectEqual(@as(usize, 6), calculateCapacity(4, 6, 1));

    // Test with larger element sizes
    try std.testing.expectEqual(@as(usize, 20), calculateCapacity(16, 20, 1));

    // Test that it rounds up appropriately
    try std.testing.expectEqual(@as(usize, 10), calculateCapacity(8, 10, 1));

    // Test growth logic when requesting exactly old_capacity + 1
    try std.testing.expectEqual(@as(usize, 8), calculateCapacity(4, 5, 1));
}

test "allocateWithRefcount basic functionality" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const ops = test_env.getOps();

    // Allocate memory with refcount
    const ptr = allocateWithRefcount(64, 8, false, ops);
    _ = ptr; // Just verify it doesn't crash

    // Should have tracked the allocation
    try std.testing.expectEqual(@as(usize, 1), test_env.getAllocationCount());
}

test "isUnique with different scenarios" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const ops = test_env.getOps();

    // Test with null (should return true)
    try std.testing.expect(@import("../utils.zig").isUnique(null));

    // Test with allocated memory
    const ptr = allocateWithRefcount(64, 8, false, ops);
    try std.testing.expect(@import("../utils.zig").isUnique(ptr));
}

test "rcNone function" {
    // rcNone should be safe to call with any pointer
    @import("../utils.zig").rcNone(null);

    var dummy: u8 = 42;
    @import("../utils.zig").rcNone(@as(?[*]u8, @ptrCast(&dummy)));

    // If we get here without crashing, the test passed
    try std.testing.expect(true);
}
