//! An implementation of RocOps for testing purposes.

const std = @import("std");
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

/// Fixed size for storing allocation metadata (size).
/// Must be at least max(16, @alignOf(usize)) to ensure proper alignment for all Roc types.
/// We use 16 because that's Roc's max alignment (for Dec/i128/u128).
const SIZE_STORAGE_BYTES: usize = 16;

const TestEnv = @This();

allocator: std.mem.Allocator,
crash: CrashContext,
roc_ops: ?RocOps,

pub fn init(allocator: std.mem.Allocator) TestEnv {
    return TestEnv{
        .allocator = allocator,
        .crash = CrashContext.init(allocator),
        .roc_ops = null,
    };
}

pub fn deinit(self: *TestEnv) void {
    self.crash.deinit();
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

    // Use a fixed alignment that's at least SIZE_STORAGE_BYTES to ensure consistency
    // across alloc/dealloc/realloc calls regardless of the requested alignment.
    const effective_alignment = @max(alloc_args.alignment, SIZE_STORAGE_BYTES);
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(effective_alignment)));

    const total_size = alloc_args.length + SIZE_STORAGE_BYTES;

    // Allocate memory including space for size metadata
    const result = test_env.allocator.rawAlloc(total_size, align_enum, @returnAddress());

    const base_ptr = result orelse {
        std.debug.panic("Out of memory during testRocAlloc", .{});
    };

    // Store the total size (including metadata) right before the user data
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + SIZE_STORAGE_BYTES - @sizeOf(usize));
    size_ptr.* = total_size;

    // Return pointer to the user data (after the size metadata)
    alloc_args.answer = @ptrFromInt(@intFromPtr(base_ptr) + SIZE_STORAGE_BYTES);
}

fn testRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));

    // Use the fixed SIZE_STORAGE_BYTES - this must match what testRocAlloc used
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - @sizeOf(usize));

    // Read the total size from metadata
    const total_size = size_ptr.*;

    // Calculate the base pointer (start of actual allocation)
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - SIZE_STORAGE_BYTES);

    // Use the same effective alignment as testRocAlloc
    const effective_alignment = @max(dealloc_args.alignment, SIZE_STORAGE_BYTES);
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(effective_alignment)));

    // Free the memory (including the size metadata)
    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    test_env.allocator.rawFree(slice, align_enum, @returnAddress());
}

fn testRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));

    // Use the fixed SIZE_STORAGE_BYTES - must match what testRocAlloc used
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(realloc_args.answer) - @sizeOf(usize));

    // Read the old total size from metadata
    const old_total_size = old_size_ptr.*;

    // Calculate the old base pointer (start of actual allocation)
    const old_base_ptr: [*]align(SIZE_STORAGE_BYTES) u8 = @ptrFromInt(@intFromPtr(realloc_args.answer) - SIZE_STORAGE_BYTES);

    // Calculate new total size needed
    const new_total_size = realloc_args.new_length + SIZE_STORAGE_BYTES;

    // Perform reallocation - use align(SIZE_STORAGE_BYTES) slice to match original allocation
    const old_slice: []align(SIZE_STORAGE_BYTES) u8 = old_base_ptr[0..old_total_size];
    const new_slice = test_env.allocator.realloc(old_slice, new_total_size) catch {
        std.debug.panic("Out of memory during testRocRealloc", .{});
    };

    // Store the new total size in the metadata
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + SIZE_STORAGE_BYTES - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    // Return pointer to the user data (after the size metadata)
    realloc_args.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + SIZE_STORAGE_BYTES);
}

fn testRocDbg(dbg_args: *const RocDbg, env: *anyopaque) callconv(.c) void {
    _ = dbg_args;
    _ = env;
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
