//! An implementation of RocOps for testing purposes.
//!
//! This module also provides allocation tracking for detecting memory errors:
//! - Leaks: allocations without corresponding frees
//! - Double-frees: freeing the same pointer twice
//! - Use-after-free: detected via POISON_VALUE written to refcount slot

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const eval_mod = @import("../mod.zig");

const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;

const CrashContext = eval_mod.CrashContext;
const CrashState = eval_mod.CrashState;

const TestEnv = @This();

/// Poison value written to refcount slot on free for use-after-free detection.
/// Matches the value used in src/builtins/utils.zig.
const POISON_VALUE: isize = @bitCast(if (@sizeOf(usize) == 8)
    @as(usize, 0xDEADBEEFDEADBEEF)
else
    @as(usize, 0xDEADBEEF));

/// Information about an active allocation, used for leak detection.
const AllocationInfo = struct {
    size: usize,
    alignment: usize,
};

allocator: std.mem.Allocator,
crash: CrashContext,
roc_ops: ?RocOps,
/// Tracks active allocations for leak/double-free detection.
/// Key is the user-visible pointer (after size metadata).
allocation_tracker: std.AutoHashMap(usize, AllocationInfo),

pub fn init(allocator: std.mem.Allocator) TestEnv {
    return TestEnv{
        .allocator = allocator,
        .crash = CrashContext.init(allocator),
        .roc_ops = null,
        .allocation_tracker = std.AutoHashMap(usize, AllocationInfo).init(allocator),
    };
}

pub fn deinit(self: *TestEnv) void {
    self.allocation_tracker.deinit();
    self.crash.deinit();
}

/// Check for memory leaks. Panics if any allocations were not freed.
/// Call this at the end of tests to verify all memory was properly released.
pub fn checkForLeaks(self: *TestEnv) void {
    const leak_count = self.allocation_tracker.count();
    if (leak_count > 0) {
        std.debug.print("\n=== MEMORY LEAK DETECTED ===\n", .{});
        std.debug.print("Found {} leaked allocation(s):\n", .{leak_count});

        var iter = self.allocation_tracker.iterator();
        var i: usize = 0;
        while (iter.next()) |entry| : (i += 1) {
            std.debug.print("  [{d}] ptr=0x{x}, size={d}, alignment={d}\n", .{
                i,
                entry.key_ptr.*,
                entry.value_ptr.size,
                entry.value_ptr.alignment,
            });
        }
        std.debug.print("============================\n", .{});
        @panic("Memory leak detected in test");
    }
}

/// Reset allocation tracking state (useful between test runs).
pub fn resetAllocationTracker(self: *TestEnv) void {
    self.allocation_tracker.clearRetainingCapacity();
}

/// Get the RocOps instance for this test environment, initializing it if needed
pub fn get_ops(self: *TestEnv) *RocOps {
    if (self.roc_ops == null) {
        self.roc_ops = RocOps{
            .env = @ptrCast(self),
            .roc_alloc = testRocAlloc,
            .roc_dealloc = testRocDealloc,
            .roc_realloc = testRocRealloc,
            .roc_dbg = testRocDbg,
            .roc_expect_failed = testRocExpectFailed,
            .roc_crashed = testRocCrashed,
            .hosted_fns = .{ .count = 0, .fns = undefined }, // Not used in tests
        };
    }
    self.crash.reset();
    return &(self.roc_ops.?);
}

/// Expose the current crash state for assertions in tests.
pub fn crashState(self: *TestEnv) CrashState {
    return self.crash.state;
}

fn testRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));

    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alloc_args.alignment)));

    // Calculate additional bytes needed to store the size
    const size_storage_bytes = @max(alloc_args.alignment, @alignOf(usize));
    const total_size = alloc_args.length + size_storage_bytes;

    // Allocate memory including space for size metadata
    const result = test_env.allocator.rawAlloc(total_size, align_enum, @returnAddress());

    const base_ptr = result orelse {
        std.debug.panic("Out of memory during testRocAlloc", .{});
    };

    // Store the total size (including metadata) right before the user data
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    // Return pointer to the user data (after the size metadata)
    const user_ptr: usize = @intFromPtr(base_ptr) + size_storage_bytes;
    alloc_args.answer = @ptrFromInt(user_ptr);

    // Track this allocation for leak detection
    test_env.allocation_tracker.put(user_ptr, .{
        .size = alloc_args.length,
        .alignment = alloc_args.alignment,
    }) catch {
        std.debug.panic("Failed to track allocation in TestEnv", .{});
    };
}

fn testRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));
    const user_ptr: usize = @intFromPtr(dealloc_args.ptr);

    // Check for double-free
    if (!test_env.allocation_tracker.remove(user_ptr)) {
        std.debug.print("\n=== DOUBLE-FREE DETECTED ===\n", .{});
        std.debug.print("Attempted to free ptr=0x{x} which was not allocated or already freed\n", .{user_ptr});
        std.debug.print("============================\n", .{});
        @panic("Double-free detected in test");
    }

    // Calculate where the size metadata is stored
    const size_storage_bytes = @max(dealloc_args.alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - @sizeOf(usize));

    // Read the total size from metadata
    const total_size = size_ptr.*;

    // Calculate the base pointer (start of actual allocation)
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - size_storage_bytes);

    // Write POISON_VALUE to the refcount slot for use-after-free detection.
    // The refcount is stored at offset -8 from the data pointer (just before the data).
    // For Roc allocations, the layout is: [refcount:isize][data...]
    // The dealloc_args.ptr points to the refcount location (not the data).
    const refcount_ptr: *isize = @ptrCast(@alignCast(dealloc_args.ptr));
    refcount_ptr.* = POISON_VALUE;

    // Calculate alignment
    const log2_align = std.math.log2_int(u32, @intCast(dealloc_args.alignment));
    const align_enum: std.mem.Alignment = @enumFromInt(log2_align);

    // Free the memory (including the size metadata)
    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    test_env.allocator.rawFree(slice, align_enum, @returnAddress());
}

fn testRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));
    const old_user_ptr: usize = @intFromPtr(realloc_args.answer);

    // Check that the old pointer was actually allocated
    if (!test_env.allocation_tracker.remove(old_user_ptr)) {
        std.debug.print("\n=== REALLOC OF UNTRACKED MEMORY ===\n", .{});
        std.debug.print("Attempted to realloc ptr=0x{x} which was not allocated or already freed\n", .{old_user_ptr});
        std.debug.print("===================================\n", .{});
        @panic("Realloc of untracked memory detected in test");
    }

    // Calculate where the size metadata is stored for the old allocation
    const size_storage_bytes = @max(realloc_args.alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(realloc_args.answer) - @sizeOf(usize));

    // Read the old total size from metadata
    const old_total_size = old_size_ptr.*;

    // Calculate the old base pointer (start of actual allocation)
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(realloc_args.answer) - size_storage_bytes);

    // Calculate new total size needed
    const new_total_size = realloc_args.new_length + size_storage_bytes;

    // Get the alignment enum from the passed alignment
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(realloc_args.alignment)));

    // Perform reallocation using rawFree + rawAlloc to handle alignment correctly
    // (Zig's realloc doesn't let us specify alignment for the old slice)
    const new_result = test_env.allocator.rawAlloc(new_total_size, align_enum, @returnAddress());
    const new_base_ptr = new_result orelse {
        std.debug.panic("Out of memory during testRocRealloc", .{});
    };

    // Copy the old data to the new allocation
    const copy_size = @min(old_total_size, new_total_size);
    @memcpy(new_base_ptr[0..copy_size], old_base_ptr[0..copy_size]);

    // Free the old allocation
    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
    test_env.allocator.rawFree(old_slice, align_enum, @returnAddress());

    // Store the new total size in the metadata
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_base_ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    // Return pointer to the user data (after the size metadata)
    const new_user_ptr: usize = @intFromPtr(new_base_ptr) + size_storage_bytes;
    realloc_args.answer = @ptrFromInt(new_user_ptr);

    // Track the new allocation
    test_env.allocation_tracker.put(new_user_ptr, .{
        .size = realloc_args.new_length,
        .alignment = realloc_args.alignment,
    }) catch {
        std.debug.panic("Failed to track reallocation in TestEnv", .{});
    };
}

fn testRocDbg(_: *const RocDbg, _: *anyopaque) callconv(.c) void {
    @panic("testRocDbg not implemented yet");
}

fn testRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));
    const source_bytes = expect_args.utf8_bytes[0..expect_args.len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    // Format and record the message
    const formatted = std.fmt.allocPrint(test_env.allocator, "Expect failed: {s}", .{trimmed}) catch {
        std.debug.panic("failed to allocate expect failure message in test env", .{});
    };
    test_env.crash.recordCrash(formatted) catch |err| {
        test_env.allocator.free(formatted);
        std.debug.panic("failed to store expect failure in test env: {}", .{err});
    };
}

fn testRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));
    const msg_slice = crashed_args.utf8_bytes[0..crashed_args.len];
    test_env.crash.recordCrash(msg_slice) catch |err| {
        std.debug.panic("failed to store crash message in test env: {}", .{err});
    };
}
