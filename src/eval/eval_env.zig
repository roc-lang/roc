const std = @import("std");
const testing = std.testing;
const host = @import("../host_abi.zig");
const _ = @import("./stack.zig");

pub const Env = struct {
    allocator: std.mem.Allocator,
    log: std.ArrayListUnmanaged(LogEntry),
};

pub const LogEntry = union(enum) {
    dbg: []u8,
    expect_failed: []u8,
    crashed: []u8,
};

pub const EvalRocOps = host.RocOps(Env, struct {});

fn testingRocDbg(arg: *const host.RocDbg, env: *Env) void {
    env.log.append(env.allocator, .{ .dbg = arg.toString() }) catch {
        @panic("roc_dbg ran out of memory trying to append to the testing output log.");
    };
}

fn testingRocExpectFailed(arg: *const host.RocExpectFailed, env: *Env) void {
    env.log.append(env.allocator, .{ .expect_failed = arg.toString() }) catch {
        @panic("roc_expect_failed ran out of memory trying to append to the testing output log.");
    };
}

fn testingRocCrashed(arg: *const host.RocCrashed, env: *Env) void {
    env.log.append(env.allocator, .{ .crashed = arg.toString() }) catch {
        @panic("roc_crashed ran out of memory trying to append to the testing output log.");
    };
}

pub fn testingEvalRocOps(allocator: std.mem.Allocator) EvalRocOps {
    const env: Env = .{
        .allocator = allocator,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };

    return .{
        .env = env,
        .roc_alloc = testingRocAlloc,
        .roc_dealloc = testingRocDealloc,
        .roc_realloc = testingRocRealloc,
        .roc_dbg = testingRocDbg,
        .roc_expect_failed = testingRocExpectFailed,
        .roc_crashed = testingRocCrashed,
        .host_fns = .{},
    };
}

fn testingRocAlloc(arg: *host.RocAlloc, env: *const Env) void {
    const alignment = std.mem.Alignment.fromByteUnits(arg.alignment);

    // Calculate offset needed for metadata
    // First, we need enough room for a usize
    const usize_size = @sizeOf(usize);

    // Calculate how much space we need before the user data to:
    // 1. Have enough space for the metadata (total allocation size)
    // 2. Ensure the user data starts at the required alignment
    const metadata_space = usize_size;
    // Calculate how much padding we need after the metadata to ensure user data alignment
    const alignment_value = alignment.toByteUnits();
    const padding = (alignment_value - (metadata_space % alignment_value)) % alignment_value;
    const offset = metadata_space + padding;

    // Allocate memory with extra space for metadata
    const total_size = offset + arg.len;

    // Allocate with at least usize alignment to ensure we can safely store the size
    const min_alignment = @max(alignment.toByteUnits(), @alignOf(usize));
    const ptr = env.allocator.rawAlloc(total_size, std.mem.Alignment.fromByteUnits(min_alignment), @returnAddress()) orelse {
        @panic("Allocation failed inside testing implementation of roc_alloc");
    };

    // Store the total allocation size at the beginning
    const meta_ptr = @as(*usize, @ptrCast(@alignCast(ptr)));
    meta_ptr.* = total_size;

    // Calculate user data pointer (properly aligned)
    const user_ptr = @as([*]u8, @ptrCast(ptr)) + offset;

    // DEBUG: Verify alignment of the user pointer
    std.debug.assert(@intFromPtr(user_ptr) % alignment.toByteUnits() == 0);

    arg.answer = user_ptr;
}

/// Reallocates memory according to Roc's memory management requirements.
/// This function handles three cases:
/// 1. If the pointer is null, it behaves like malloc (allocates new memory)
/// 2. If in-place reallocation is possible, it resizes the existing allocation
/// 3. If in-place reallocation fails, it allocates new memory, copies data, and frees the old memory
///
/// The function maintains proper alignment and preserves existing data up to the minimum
/// of the old and new sizes. It also handles the metadata stored before the user data
/// that tracks the total allocation size.
fn testingRocRealloc(arg: *host.RocRealloc, env: *const Env) void {
    // If the new length is 0, treat it as a minimum size (1 byte)
    if (arg.new_len == 0) {
        arg.new_len = 1;
    }

    // If old pointer is null, just allocate new memory
    if (@intFromPtr(arg.ptr) == 0) {
        var alloc_arg = host.RocAlloc{
            .alignment = arg.alignment,
            .len = arg.new_len,
            .answer = undefined,
        };
        testingRocAlloc(&alloc_arg, env);
        arg.answer = alloc_arg.answer;
        return;
    }

    const old_ptr = @as([*]u8, @ptrCast(arg.ptr));
    const original_size = getOriginalSize(old_ptr, arg.alignment);

    // Calculate the offset to get to the original allocation
    const metadata_space = @sizeOf(usize);
    const alignment_value = arg.alignment;
    const padding = (alignment_value - (metadata_space % alignment_value)) % alignment_value;
    const offset = metadata_space + padding;

    // Get pointer to the start of the allocation
    const original_ptr = old_ptr - offset;

    // Calculate total size needed for new allocation (including metadata)
    const new_total_size = offset + arg.new_len;

    // Compute minimum alignment required (at least usize alignment for the metadata)
    const min_alignment = @max(arg.alignment, @alignOf(usize));

    // Try to resize in place
    const maybe_resized = env.allocator.rawRemap(
        original_ptr[0..original_size],
        std.mem.Alignment.fromByteUnits(min_alignment),
        new_total_size,
        0,
    );

    if (maybe_resized) |resized_ptr| {
        // In-place reallocation succeeded

        // Update the size metadata
        const meta_ptr = @as(*usize, @ptrCast(@alignCast(resized_ptr)));
        meta_ptr.* = new_total_size;

        // Calculate the new user-facing pointer
        const user_ptr = @as([*]u8, @ptrCast(resized_ptr)) + offset;

        // DEBUG: Verify alignment of the user pointer
        std.debug.assert(@intFromPtr(user_ptr) % arg.alignment == 0);

        arg.answer = user_ptr;
        return;
    }

    // If we get here, in-place reallocation failed, fall back to allocate/copy/free

    // Allocate new memory
    var alloc_arg = host.RocAlloc{
        .alignment = arg.alignment,
        .len = arg.new_len,
        .answer = undefined,
    };
    testingRocAlloc(&alloc_arg, env);

    // Copy data from old to new (up to the smaller of the two sizes)
    const old_user_size = original_size - offset;
    const copy_size = @min(old_user_size, arg.new_len);
    @memcpy(alloc_arg.answer[0..copy_size], old_ptr[0..copy_size]);

    // Free the old memory
    const dealloc_arg = host.RocDealloc{
        .alignment = arg.alignment,
        .ptr = old_ptr,
    };
    testingRocDealloc(&dealloc_arg, env);

    arg.answer = alloc_arg.answer;
}

// Helper function to get the original allocation size from a user pointer
fn getOriginalSize(user_ptr: [*]u8, alignment_size: usize) usize {
    // Safety check - handle null pointer
    if (@intFromPtr(user_ptr) == 0) return 0;

    const alignment = std.mem.Alignment.fromByteUnits(alignment_size);

    // Calculate the same offset as in allocation to find the metadata
    const metadata_space = @sizeOf(usize);
    const alignment_value = alignment.toByteUnits();
    const padding = (alignment_value - (metadata_space % alignment_value)) % alignment_value;
    const offset = metadata_space + padding;

    // Calculate pointer to the beginning of the allocation
    const original_ptr = user_ptr - offset;

    // Read the total allocated size (ensuring proper alignment)
    const meta_ptr = @as(*usize, @ptrCast(@alignCast(original_ptr)));
    return meta_ptr.*;
}

fn testingRocDealloc(arg: *const host.RocDealloc, env: *const Env) void {
    const user_ptr = arg.ptr;
    // Check for null pointer
    if (@intFromPtr(user_ptr) == 0) return;

    const alignment = std.mem.Alignment.fromByteUnits(arg.alignment);

    // Calculate the same offset as in allocation to find the metadata
    const metadata_space = @sizeOf(usize);
    const alignment_value = alignment.toByteUnits();
    const padding = (alignment_value - (metadata_space % alignment_value)) % alignment_value;
    const offset = metadata_space + padding;

    // Calculate pointer to the beginning of the allocation
    const original_ptr = user_ptr - offset;

    // Read the total allocated size
    const meta_ptr = @as(*usize, @ptrCast(@alignCast(original_ptr)));
    const total_size = meta_ptr.*;

    // Compute minimum alignment required (at least usize alignment for the metadata)
    const min_alignment = @max(alignment.toByteUnits(), @alignOf(usize));

    // Free the memory using the original pointer and size
    env.allocator.rawFree(original_ptr[0..total_size], std.mem.Alignment.fromByteUnits(min_alignment), @returnAddress());
}

test "Memory management - alloc then dealloc" {
    const scratch = testing.allocator;
    const env: Env = .{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };

    const alignment: usize = 8;
    const size: usize = 16;
    var alloc_arg = host.RocAlloc{
        .alignment = alignment,
        .len = size,
        .answer = undefined,
    };

    testingRocAlloc(&alloc_arg, &env);

    // The pointer returned should be aligned as requested.
    try testing.expect(@intFromPtr(alloc_arg.answer) % alignment == 0);

    // Write to the memory to confirm it's usable.
    @memset(alloc_arg.answer[0..size], 0xAB);

    const dealloc_arg = host.RocDealloc{
        .alignment = alignment,
        .ptr = alloc_arg.answer,
    };

    testingRocDealloc(&dealloc_arg, &env);
}

test "Memory management - realloc stress test with alignments" {
    const scratch = testing.allocator;
    var env = Env{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };
    defer env.log.deinit(scratch);

    // Test various alignment and size combinations
    const test_cases = [_]struct {
        initial_align: usize,
        initial_size: usize,
        new_align: usize,
        new_size: usize,
    }{
        // Same alignment, different sizes
        .{ .initial_align = 1, .initial_size = 7, .new_align = 1, .new_size = 13 },
        .{ .initial_align = 4, .initial_size = 16, .new_align = 4, .new_size = 64 },
        .{ .initial_align = 8, .initial_size = 32, .new_align = 8, .new_size = 8 },
        .{ .initial_align = 16, .initial_size = 128, .new_align = 16, .new_size = 256 },
        .{ .initial_align = 32, .initial_size = 64, .new_align = 32, .new_size = 32 },
        .{ .initial_align = 64, .initial_size = 512, .new_align = 64, .new_size = 1024 },
        .{ .initial_align = 128, .initial_size = 256, .new_align = 128, .new_size = 128 },
    };

    for (test_cases, 0..) |tc, i| {
        // Initial allocation
        var alloc_arg = host.RocAlloc{
            .alignment = tc.initial_align,
            .len = tc.initial_size,
            .answer = undefined,
        };
        testingRocAlloc(&alloc_arg, &env);

        // Fill with a unique pattern based on test case index
        const pattern = @as(u8, @intCast((i * 17 + 1) & 0xFF));
        @memset(alloc_arg.answer[0..tc.initial_size], pattern);

        // Reallocate with same alignment (which is what realloc should maintain)
        var realloc_arg = host.RocRealloc{
            .ptr = alloc_arg.answer,
            .alignment = tc.new_align,
            .new_len = tc.new_size,
            .answer = undefined,
        };
        testingRocRealloc(&realloc_arg, &env);

        // Verify alignment
        try testing.expect(@intFromPtr(realloc_arg.answer) % tc.new_align == 0);

        // Verify data preservation
        const preserved_size = @min(tc.initial_size, tc.new_size);
        for (0..preserved_size) |j| {
            try testing.expectEqual(pattern, realloc_arg.answer[j]);
        }

        // If we expanded, write to the new space to ensure it's valid
        if (tc.new_size > tc.initial_size) {
            @memset(realloc_arg.answer[tc.initial_size..tc.new_size], pattern + 1);
        }

        // Cleanup
        const dealloc_arg = host.RocDealloc{
            .alignment = tc.new_align,
            .ptr = realloc_arg.answer,
        };
        testingRocDealloc(&dealloc_arg, &env);
    }
}

test "Memory management - unusual alignment" {
    const scratch = testing.allocator;
    const env: Env = .{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };

    // Test with unusual alignment values
    const alignments = [_]usize{ 1, 2, 4, 8, 16, 32, 64, 128 };

    for (alignments) |alignment| {
        // Allocate memory with this alignment
        const size: usize = 24;
        var alloc_arg = host.RocAlloc{
            .alignment = alignment,
            .len = size,
            .answer = undefined,
        };

        testingRocAlloc(&alloc_arg, &env);

        // Verify memory is properly aligned
        try testing.expect(@intFromPtr(alloc_arg.answer) % alignment == 0);

        // Write some data
        @memset(alloc_arg.answer[0..size], 0xAA);

        // Verify we can read it back
        for (0..size) |i| {
            try testing.expectEqual(@as(u8, 0xAA), alloc_arg.answer[i]);
        }

        // Cleanup
        const dealloc_arg = host.RocDealloc{
            .alignment = alignment,
            .ptr = alloc_arg.answer,
        };

        testingRocDealloc(&dealloc_arg, &env);
    }
}

test "Memory management - small allocation" {
    const scratch = testing.allocator;
    const env: Env = .{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };

    // Test small-sized allocation with different alignments
    const alignments = [_]usize{ 1, 8, 16 };

    for (alignments) |alignment| {
        // Allocate a small amount of memory
        var alloc_arg = host.RocAlloc{
            .alignment = alignment,
            .len = 8,
            .answer = undefined,
        };

        testingRocAlloc(&alloc_arg, &env);

        // Verify we got a non-null pointer
        try testing.expect(@intFromPtr(alloc_arg.answer) != 0);

        // Verify we can use it
        @memset(alloc_arg.answer[0..8], 0xAA);
        for (0..8) |i| {
            try testing.expectEqual(@as(u8, 0xAA), alloc_arg.answer[i]);
        }

        // Cleanup
        const dealloc_arg = host.RocDealloc{
            .alignment = alignment,
            .ptr = alloc_arg.answer,
        };

        testingRocDealloc(&dealloc_arg, &env);
    }
}

test "Memory management - allocation sequence" {
    const scratch = testing.allocator;
    const env: Env = .{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };

    const alignment: usize = 8;
    const size: usize = 32;
    var pointers: [5][*]u8 = undefined;

    // Allocate multiple times in sequence
    for (0..5) |i| {
        var alloc_arg = host.RocAlloc{
            .alignment = alignment,
            .len = size,
            .answer = undefined,
        };
        testingRocAlloc(&alloc_arg, &env);
        pointers[i] = alloc_arg.answer;

        // Write a unique pattern to this allocation
        const pattern = @as(u8, @intCast(0xA0 + i));
        @memset(alloc_arg.answer[0..size], pattern);
    }

    // Verify all allocated memory has the correct patterns and is still usable
    for (0..5) |i| {
        const pattern = @as(u8, @intCast(0xA0 + i));
        for (0..size) |j| {
            try testing.expectEqual(pattern, pointers[i][j]);
        }
    }

    // Deallocate all the memory
    for (0..5) |i| {
        const dealloc_arg = host.RocDealloc{
            .alignment = alignment,
            .ptr = pointers[i],
        };
        testingRocDealloc(&dealloc_arg, &env);
    }
}

test "Memory management - allocation from scratch" {
    const scratch = testing.allocator;
    const env: Env = .{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };

    // Use a simple allocation pattern
    const alignment = 8;
    const size = 16;

    // Perform direct allocation
    var alloc_arg = host.RocAlloc{
        .alignment = alignment,
        .len = size,
        .answer = undefined,
    };

    testingRocAlloc(&alloc_arg, &env);

    // Verify we got a valid pointer
    try testing.expect(@intFromPtr(alloc_arg.answer) != 0);

    // Test that we can use the memory
    @memset(alloc_arg.answer[0..size], 0xCC);

    for (0..size) |i| {
        try testing.expectEqual(@as(u8, 0xCC), alloc_arg.answer[i]);
    }

    // Clean up
    const cleanup_dealloc = host.RocDealloc{
        .alignment = alignment,
        .ptr = alloc_arg.answer,
    };

    testingRocDealloc(&cleanup_dealloc, &env);
}

test "Memory management - large allocations" {
    const scratch = testing.allocator;
    const env: Env = .{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };

    // Test various large allocation sizes
    const sizes = [_]usize{ 1024, 4096, 16384, 65536 };
    const alignment: usize = 16;

    for (sizes) |size| {
        var alloc_arg = host.RocAlloc{
            .alignment = alignment,
            .len = size,
            .answer = undefined,
        };

        testingRocAlloc(&alloc_arg, &env);

        // Verify memory is properly aligned
        try testing.expect(@intFromPtr(alloc_arg.answer) % alignment == 0);

        // Test that we can use the entire allocated space
        @memset(alloc_arg.answer[0..size], 0xFA);

        // Verify we can read back the whole space
        for (0..size) |i| {
            try testing.expectEqual(@as(u8, 0xFA), alloc_arg.answer[i]);
        }

        // Cleanup
        const dealloc_arg = host.RocDealloc{
            .alignment = alignment,
            .ptr = alloc_arg.answer,
        };

        testingRocDealloc(&dealloc_arg, &env);
    }
}

test "Memory management - variable alignments" {
    const scratch = testing.allocator;
    const env: Env = .{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };

    // Test with various power-of-two alignments
    const alignments = [_]usize{ 1, 2, 4, 8, 16, 32, 64 };

    for (alignments) |requested_alignment| {
        // Allocate with this alignment
        var alloc_arg = host.RocAlloc{
            .alignment = requested_alignment,
            .len = 32,
            .answer = undefined,
        };

        testingRocAlloc(&alloc_arg, &env);

        // Verify the actual alignment meets our expectations
        try testing.expect(@intFromPtr(alloc_arg.answer) % requested_alignment == 0);

        // Fill memory and check it's usable
        @memset(alloc_arg.answer[0..32], 0xBB);
        for (0..32) |i| {
            try testing.expectEqual(@as(u8, 0xBB), alloc_arg.answer[i]);
        }

        // Cleanup
        const dealloc_arg = host.RocDealloc{
            .alignment = requested_alignment,
            .ptr = alloc_arg.answer,
        };

        testingRocDealloc(&dealloc_arg, &env);
    }
}

test "Memory management - reallocation size transitions" {
    const scratch = testing.allocator;
    var env = Env{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };
    defer env.log.deinit(scratch);

    const alignment: usize = 8;

    // 1. Test small to larger allocation
    // First create a small allocation
    var alloc_arg = host.RocAlloc{
        .alignment = alignment,
        .len = 4,
        .answer = undefined,
    };
    testingRocAlloc(&alloc_arg, &env);

    // Write a pattern to the memory
    @memset(alloc_arg.answer[0..4], 0xAB);

    // Now reallocate to a larger size
    var realloc_arg = host.RocRealloc{
        .ptr = alloc_arg.answer,
        .alignment = alignment,
        .new_len = 32,
        .answer = undefined,
    };
    testingRocRealloc(&realloc_arg, &env);

    // Verify we got a valid pointer and the data was preserved
    try testing.expect(@intFromPtr(realloc_arg.answer) != 0);
    for (0..4) |i| {
        try testing.expectEqual(@as(u8, 0xAB), realloc_arg.answer[i]);
    }

    // 2. Now test larger to smaller reallocation
    var realloc_to_small = host.RocRealloc{
        .ptr = realloc_arg.answer,
        .alignment = alignment,
        .new_len = 2,
        .answer = undefined,
    };
    testingRocRealloc(&realloc_to_small, &env);

    // Verify we got a valid pointer and data was preserved
    try testing.expect(@intFromPtr(realloc_to_small.answer) != 0);
    for (0..2) |i| {
        try testing.expectEqual(@as(u8, 0xAB), realloc_to_small.answer[i]);
    }

    // Clean up
    const dealloc_arg = host.RocDealloc{
        .alignment = alignment,
        .ptr = realloc_to_small.answer,
    };
    testingRocDealloc(&dealloc_arg, &env);
}

test "Memory management - multiple reallocations" {
    const scratch = testing.allocator;
    var env = Env{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };
    defer env.log.deinit(scratch);

    const alignment: usize = 8;
    const initial_size: usize = 16;

    // Initial allocation
    var alloc_arg = host.RocAlloc{
        .alignment = alignment,
        .len = initial_size,
        .answer = undefined,
    };
    testingRocAlloc(&alloc_arg, &env);

    // Fill with pattern
    @memset(alloc_arg.answer[0..initial_size], 0x11);

    // Series of reallocations with different sizes
    const sizes = [_]usize{ 32, 8, 64, 24, 128 };
    var current_ptr = alloc_arg.answer;
    var current_size = initial_size;

    for (sizes) |new_size| {
        // Reallocate to new size
        var realloc_arg = host.RocRealloc{
            .ptr = current_ptr,
            .alignment = alignment,
            .new_len = new_size,
            .answer = undefined,
        };
        testingRocRealloc(&realloc_arg, &env);

        // Verify alignment
        try testing.expect(@intFromPtr(realloc_arg.answer) % alignment == 0);

        // Check that the content was preserved
        const preserved_size = @min(current_size, new_size);
        for (0..preserved_size) |i| {
            try testing.expectEqual(@as(u8, 0x11), realloc_arg.answer[i]);
        }

        // Fill new space with pattern
        if (new_size > current_size) {
            @memset(realloc_arg.answer[current_size..new_size], 0x11);
        }

        // Update for next iteration
        current_ptr = realloc_arg.answer;
        current_size = new_size;
    }

    // Cleanup
    const dealloc_arg = host.RocDealloc{
        .alignment = alignment,
        .ptr = current_ptr,
    };
    testingRocDealloc(&dealloc_arg, &env);
}

test "Memory management - realloc smaller" {
    const scratch = testing.allocator;
    var env = Env{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };
    defer env.log.deinit(scratch);

    // 1. Test allocation
    const alignment: usize = 8;
    const initial_size: usize = 32;
    var alloc_arg = host.RocAlloc{
        .alignment = alignment,
        .len = initial_size,
        .answer = undefined,
    };

    testingRocAlloc(&alloc_arg, &env);

    // Fill memory with a pattern
    @memset(alloc_arg.answer[0..initial_size], 0xCD);

    // 2. Test reallocation to smaller size
    const new_size: usize = 16;
    var realloc_arg = host.RocRealloc{
        .ptr = alloc_arg.answer,
        .alignment = alignment,
        .new_len = new_size,
        .answer = undefined,
    };

    testingRocRealloc(&realloc_arg, &env);

    // Verify memory is properly aligned
    try testing.expect(@intFromPtr(realloc_arg.answer) % alignment == 0);

    // Verify data was copied correctly
    for (0..new_size) |i| {
        try testing.expectEqual(@as(u8, 0xCD), realloc_arg.answer[i]);
    }

    // Cleanup
    const dealloc_arg = host.RocDealloc{
        .alignment = alignment,
        .ptr = realloc_arg.answer,
    };

    testingRocDealloc(&dealloc_arg, &env);
}

test "Memory management - realloc larger" {
    const scratch = testing.allocator;
    var env = Env{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };
    defer env.log.deinit(scratch);

    // 1. Test allocation
    const alignment: usize = 16; // Test with a larger alignment
    const initial_size: usize = 8;
    var alloc_arg = host.RocAlloc{
        .alignment = alignment,
        .len = initial_size,
        .answer = undefined,
    };

    testingRocAlloc(&alloc_arg, &env);

    // Fill memory with a pattern
    @memset(alloc_arg.answer[0..initial_size], 0xEF);

    // 2. Test reallocation to larger size
    const new_size: usize = 64;
    var realloc_arg = host.RocRealloc{
        .ptr = alloc_arg.answer,
        .alignment = alignment,
        .new_len = new_size,
        .answer = undefined,
    };

    testingRocRealloc(&realloc_arg, &env);

    // Verify memory is properly aligned
    try testing.expect(@intFromPtr(realloc_arg.answer) % alignment == 0);

    // Verify initial data was copied correctly
    for (0..initial_size) |i| {
        try testing.expectEqual(@as(u8, 0xEF), realloc_arg.answer[i]);
    }

    // Use the additional space
    for (initial_size..new_size) |i| {
        realloc_arg.answer[i] = 0x42;
    }

    // Cleanup
    const dealloc_arg = host.RocDealloc{
        .alignment = alignment,
        .ptr = realloc_arg.answer,
    };

    testingRocDealloc(&dealloc_arg, &env);
}

test "Memory management - realloc zero size" {
    const scratch = testing.allocator;
    var env = Env{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };
    defer env.log.deinit(scratch);

    // First allocate some memory
    var alloc_arg = host.RocAlloc{
        .alignment = 8,
        .len = 16,
        .answer = undefined,
    };
    testingRocAlloc(&alloc_arg, &env);

    // Fill with pattern
    @memset(alloc_arg.answer[0..16], 0x77);

    // Reallocate to zero size (should be treated as size 1)
    var realloc_arg = host.RocRealloc{
        .ptr = alloc_arg.answer,
        .alignment = 8,
        .new_len = 0,
        .answer = undefined,
    };

    testingRocRealloc(&realloc_arg, &env);

    // Should still have a valid pointer
    try testing.expect(@intFromPtr(realloc_arg.answer) != 0);
    try testing.expect(@intFromPtr(realloc_arg.answer) % 8 == 0);

    // Should be able to write at least 1 byte
    realloc_arg.answer[0] = 0x88;

    // Cleanup
    const dealloc_arg = host.RocDealloc{
        .alignment = 8,
        .ptr = realloc_arg.answer,
    };
    testingRocDealloc(&dealloc_arg, &env);
}

test "Memory management - realloc edge cases" {
    const scratch = testing.allocator;
    var env = Env{
        .allocator = scratch,
        .log = std.ArrayListUnmanaged(LogEntry).empty,
    };
    defer env.log.deinit(scratch);

    // Test reallocating to the same size
    var alloc_arg = host.RocAlloc{
        .alignment = 16,
        .len = 32,
        .answer = undefined,
    };
    testingRocAlloc(&alloc_arg, &env);

    // Fill with pattern
    for (0..32) |i| {
        alloc_arg.answer[i] = @as(u8, @intCast(i & 0xFF));
    }

    // Reallocate to same size
    var realloc_arg = host.RocRealloc{
        .ptr = alloc_arg.answer,
        .alignment = 16,
        .new_len = 32,
        .answer = undefined,
    };
    testingRocRealloc(&realloc_arg, &env);

    // Verify alignment
    try testing.expect(@intFromPtr(realloc_arg.answer) % 16 == 0);

    // Verify data preservation
    for (0..32) |i| {
        try testing.expectEqual(@as(u8, @intCast(i & 0xFF)), realloc_arg.answer[i]);
    }

    // Cleanup
    const dealloc_arg = host.RocDealloc{
        .alignment = 16,
        .ptr = realloc_arg.answer,
    };
    testingRocDealloc(&dealloc_arg, &env);
}
