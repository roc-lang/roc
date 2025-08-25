//! Simple platform host that calls into a simplified Roc entrypoint with two integers and prints the result.

const std = @import("std");
const builtins = @import("builtins");

/// Host environment - contains our arena allocator
const HostEnv = struct {
    arena: std.heap.ArenaAllocator,
};

/// Roc allocation function
fn rocAllocFn(roc_alloc: *builtins.host_abi.RocAlloc, env: *anyopaque) callconv(.C) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.arena.allocator();

    const log2_align = std.math.log2_int(u32, @intCast(roc_alloc.alignment));
    const align_enum: std.mem.Alignment = @enumFromInt(log2_align);

    const result = allocator.rawAlloc(roc_alloc.length, align_enum, @returnAddress());

    roc_alloc.answer = result orelse {
        @panic("Host allocation failed");
    };
}

/// Roc deallocation function
fn rocDeallocFn(roc_dealloc: *builtins.host_abi.RocDealloc, env: *anyopaque) callconv(.C) void {
    _ = roc_dealloc;
    _ = env;
    // NoOp as our arena frees all memory at once
}

/// Roc reallocation function
fn rocReallocFn(roc_realloc: *builtins.host_abi.RocRealloc, env: *anyopaque) callconv(.C) void {
    _ = roc_realloc;
    _ = env;
    @panic("Realloc not implemented in this example");
}

/// Roc debug function
fn rocDbgFn(roc_dbg: *const builtins.host_abi.RocDbg, env: *anyopaque) callconv(.C) void {
    _ = env;
    const message = roc_dbg.utf8_bytes[0..roc_dbg.len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

/// Roc expect failed function
fn rocExpectFailedFn(roc_expect: *const builtins.host_abi.RocExpectFailed, env: *anyopaque) callconv(.C) void {
    _ = env;
    const message = roc_expect.utf8_bytes[0..roc_expect.len];
    std.debug.print("ROC EXPECT FAILED: {s}\n", .{message});
}

/// Roc crashed function
fn rocCrashedFn(roc_crashed: *const builtins.host_abi.RocCrashed, env: *anyopaque) callconv(.C) noreturn {
    _ = env;
    const message = roc_crashed.utf8_bytes[0..roc_crashed.len];
    @panic(message);
}

// External symbols provided by the Roc runtime object file
// Follows RocCall ABI: ops, ret_ptr, then argument pointers
extern fn roc__addInts(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void;
extern fn roc__multiplyInts(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void;

// OS-specific entry point handling
comptime {
    const builtin = @import("builtin");
    if (builtin.os.tag == .windows) {
        // Windows needs __main for MinGW-style initialization
        @export(__main, .{ .name = "__main" });
    } else {
        // On Unix-like systems, export main to be called by C runtime
        @export(&main, .{ .name = "main" });
    }
}

// stub for Windows
fn __main() void {}

// C compatible main for runtime
fn main(argc: c_int, argv: [*][*:0]u8) callconv(.C) c_int {
    _ = argc;
    _ = argv;
    platform_main() catch |err| {
        std.io.getStdErr().writer().print("HOST ERROR: {?}", .{err}) catch unreachable;
        return 1;
    };
    return 0;
}

/// Platform host entrypoint -- this is where the roc application starts and does platform things
/// before the platform calls into Roc to do application-specific things.
fn platform_main() !void {
    var host_env = HostEnv{
        .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
    };
    defer host_env.arena.deinit(); // Clean up all allocations on exit

    const stdout = std.io.getStdOut().writer();

    // Create the RocOps struct
    var roc_ops = builtins.host_abi.RocOps{
        .env = @as(*anyopaque, @ptrCast(&host_env)),
        .roc_alloc = rocAllocFn,
        .roc_dealloc = rocDeallocFn,
        .roc_realloc = rocReallocFn,
        .roc_dbg = rocDbgFn,
        .roc_expect_failed = rocExpectFailedFn,
        .roc_crashed = rocCrashedFn,
        .host_fns = undefined, // No host functions for this simple example
    };

    // Generate random integers using current timestamp as seed
    var rand = std.Random.DefaultPrng.init(@intCast(std.time.timestamp()));
    const a = rand.random().intRangeAtMost(i64, 0, 100);
    const b = rand.random().intRangeAtMost(i64, 0, 100);

    // Arguments struct for passing two integers to Roc as a tuple
    const Args = extern struct { a: i64, b: i64 };
    var args = Args{ .a = a, .b = b };

    try stdout.print("Generated numbers: a = {}, b = {}\n", .{ a, b });

    // Test first entrypoint: addInts (entry_idx = 0)
    try stdout.print("\n=== Testing addInts (entry_idx = 0) ===\n", .{});

    var add_result: i64 = undefined;
    roc__addInts(&roc_ops, @as(*anyopaque, @ptrCast(&add_result)), @as(*anyopaque, @ptrCast(&args)));

    const expected_add = a +% b; // Use wrapping addition to match Roc behavior
    try stdout.print("Expected add result: {}\n", .{expected_add});
    try stdout.print("Roc computed add: {}\n", .{add_result});

    var success_count: u32 = 0;
    if (add_result == expected_add) {
        try stdout.print("\x1b[32mSUCCESS\x1b[0m: addInts results match!\n", .{});
        success_count += 1;
    } else {
        try stdout.print("\x1b[31mFAIL\x1b[0m: addInts results differ!\n", .{});
    }

    // Test second entrypoint: multiplyInts (entry_idx = 1)
    try stdout.print("\n=== Testing multiplyInts (entry_idx = 1) ===\n", .{});

    var multiply_result: i64 = undefined;
    roc__multiplyInts(&roc_ops, @as(*anyopaque, @ptrCast(&multiply_result)), @as(*anyopaque, @ptrCast(&args)));

    const expected_multiply = a *% b; // Use wrapping multiplication to match Roc behavior
    try stdout.print("Expected multiply result: {}\n", .{expected_multiply});
    try stdout.print("Roc computed multiply: {}\n", .{multiply_result});

    if (multiply_result == expected_multiply) {
        try stdout.print("\x1b[32mSUCCESS\x1b[0m: multiplyInts results match!\n", .{});
        success_count += 1;
    } else {
        try stdout.print("\x1b[31mFAIL\x1b[0m: multiplyInts results differ!\n", .{});
    }

    // Final summary
    try stdout.print("\n=== FINAL RESULT ===\n", .{});
    if (success_count == 2) {
        try stdout.print("\x1b[32mALL TESTS PASSED\x1b[0m: Both entrypoints work correctly!\n", .{});
    } else {
        try stdout.print("\x1b[31mSOME TESTS FAILED\x1b[0m: {}/2 tests passed\n", .{success_count});
        std.process.exit(1);
    }
}
