//! An implementation of RocOps for testing purposes.
//!
//! This module also provides allocation tracking for detecting memory errors:
//! - Leaks: allocations without corresponding frees
//! - Double-frees: freeing the same pointer twice
//! - Use-after-free: detected via POISON_VALUE written to refcount slot

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const crash_context = @import("../crash_context.zig");

/// Diagnostic print that is a no-op on freestanding (WASM) where
/// std.debug.print is unavailable due to missing OS thread/file primitives.
fn debugPrint(comptime fmt: []const u8, args: anytype) void {
    if (comptime builtin.os.tag != .freestanding) {
        std.debug.print(fmt, args);
    }
}

const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;

const CrashContext = crash_context.CrashContext;
const CrashState = crash_context.CrashState;

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
/// Key is the allocation pointer returned by `roc_alloc`.
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

/// Error set for memory leak detection in tests.
pub const LeakError = error{MemoryLeak};

/// Check for memory leaks. Returns error.MemoryLeak if any allocations were not freed.
/// Call this at the end of tests to verify all memory was properly released.
/// Leak details are silent by default — use trace-refcount flags to diagnose.
pub fn checkForLeaks(self: *TestEnv) LeakError!void {
    if (self.allocation_tracker.count() > 0) {
        return error.MemoryLeak;
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
            .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
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
    const alloc_ptr = allocateTrackedBytes(test_env.allocator, alloc_args.length, alloc_args.alignment);
    alloc_args.answer = @ptrCast(alloc_ptr);
    test_env.allocation_tracker.put(@intFromPtr(alloc_ptr), .{
        .size = alloc_args.length,
        .alignment = alloc_args.alignment,
    }) catch {
        std.debug.panic("Failed to track allocation in TestEnv", .{});
    };
}

fn testRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));
    const alloc_ptr: usize = @intFromPtr(dealloc_args.ptr);

    // Check for double-free
    const alloc_info = test_env.allocation_tracker.fetchRemove(alloc_ptr) orelse {
        debugPrint("\n=== DOUBLE-FREE DETECTED ===\n", .{});
        debugPrint("Attempted to free ptr=0x{x} which was not allocated or already freed\n", .{alloc_ptr});
        debugPrint("============================\n", .{});
        @panic("Double-free detected in test");
    };

    if (alloc_info.value.size >= @sizeOf(isize)) {
        const refcount_ptr: *isize = @ptrCast(@alignCast(dealloc_args.ptr));
        refcount_ptr.* = POISON_VALUE;
    }
    freeTrackedBytes(test_env.allocator, dealloc_args.ptr, alloc_info.value);
}

fn testRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));
    const old_alloc_ptr: usize = @intFromPtr(realloc_args.answer);

    // Check that the old pointer was actually allocated
    const old_info = test_env.allocation_tracker.fetchRemove(old_alloc_ptr) orelse {
        debugPrint("\n=== REALLOC OF UNTRACKED MEMORY ===\n", .{});
        debugPrint("Attempted to realloc ptr=0x{x} which was not allocated or already freed\n", .{old_alloc_ptr});
        debugPrint("===================================\n", .{});
        @panic("Realloc of untracked memory detected in test");
    };

    const new_base_ptr = allocateTrackedBytes(test_env.allocator, realloc_args.new_length, realloc_args.alignment);

    const old_bytes: [*]u8 = @ptrCast(@alignCast(realloc_args.answer));
    const copy_size = @min(old_info.value.size, realloc_args.new_length);
    @memcpy(new_base_ptr[0..copy_size], old_bytes[0..copy_size]);

    freeTrackedBytes(test_env.allocator, realloc_args.answer, old_info.value);
    realloc_args.answer = @ptrCast(new_base_ptr);

    // Track the new allocation
    test_env.allocation_tracker.put(@intFromPtr(new_base_ptr), .{
        .size = realloc_args.new_length,
        .alignment = realloc_args.alignment,
    }) catch {
        std.debug.panic("Failed to track reallocation in TestEnv", .{});
    };
}

fn testRocDbg(dbg_args: *const RocDbg, _: *anyopaque) callconv(.c) void {
    const msg = dbg_args.utf8_bytes[0..dbg_args.len];
    debugPrint("[dbg] {s}\n", .{msg});
}

fn testRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));
    const source_bytes = expect_args.utf8_bytes[0..expect_args.len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    // Format and record the message
    const formatted = std.fmt.allocPrint(test_env.allocator, "Expect failed: {s}", .{trimmed}) catch {
        std.debug.panic("failed to allocate expect failure message in test env", .{});
    };
    defer test_env.allocator.free(formatted);
    test_env.crash.recordCrash(formatted) catch |err| {
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

fn allocateTrackedBytes(allocator: std.mem.Allocator, len: usize, alignment: usize) [*]u8 {
    return switch (alignment) {
        1 => (allocator.alignedAlloc(u8, .@"1", len) catch oom("testRocAlloc")).ptr,
        2 => (allocator.alignedAlloc(u8, .@"2", len) catch oom("testRocAlloc")).ptr,
        4 => (allocator.alignedAlloc(u8, .@"4", len) catch oom("testRocAlloc")).ptr,
        8 => (allocator.alignedAlloc(u8, .@"8", len) catch oom("testRocAlloc")).ptr,
        16 => (allocator.alignedAlloc(u8, .@"16", len) catch oom("testRocAlloc")).ptr,
        else => std.debug.panic("Unsupported alignment in test env: {d}", .{alignment}),
    };
}

fn freeTrackedBytes(allocator: std.mem.Allocator, ptr: *anyopaque, alloc_info: AllocationInfo) void {
    const bytes: [*]u8 = @ptrCast(@alignCast(ptr));
    switch (alloc_info.alignment) {
        1 => allocator.free(bytes[0..alloc_info.size]),
        2 => allocator.free((@as([*]align(2) u8, @alignCast(bytes)))[0..alloc_info.size]),
        4 => allocator.free((@as([*]align(4) u8, @alignCast(bytes)))[0..alloc_info.size]),
        8 => allocator.free((@as([*]align(8) u8, @alignCast(bytes)))[0..alloc_info.size]),
        16 => allocator.free((@as([*]align(16) u8, @alignCast(bytes)))[0..alloc_info.size]),
        else => std.debug.panic("Unsupported free alignment in test env: {d}", .{alloc_info.alignment}),
    }
}

fn oom(comptime context: []const u8) noreturn {
    std.debug.panic("Out of memory during {s}", .{context});
}

test "crash message storage and retrieval - host-managed context" {
    const testing = std.testing;
    const test_message = "Direct API test message";

    var test_env_instance = TestEnv.init(std.heap.smp_allocator);
    defer test_env_instance.deinit();

    try testing.expect(test_env_instance.crashState() == .did_not_crash);

    const crash_args = RocCrashed{
        .utf8_bytes = @constCast(test_message.ptr),
        .len = test_message.len,
    };

    const ops = test_env_instance.get_ops();
    ops.roc_crashed(&crash_args, ops.env);

    switch (test_env_instance.crashState()) {
        .did_not_crash => return error.TestUnexpectedResult,
        .crashed => |msg| try testing.expectEqualStrings(test_message, msg),
    }
}
