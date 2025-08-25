//! Simple platform host that calls into a simplified Roc entrypoint and prints a string result.

const std = @import("std");
const builtins = @import("builtins");

const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocCrashed = builtins.host_abi.RocCrashed;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocOps = builtins.host_abi.RocOps;
const RocDbg = builtins.host_abi.RocDbg;
const RocStr = builtins.str.RocStr;

/// Host environment - contains our arena allocator
const HostEnv = struct {
    arena: std.heap.ArenaAllocator,
};

/// Roc allocation function
fn rocAllocFn(roc_alloc: *RocAlloc, env: *anyopaque) callconv(.C) void {
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
fn rocDeallocFn(roc_dealloc: *RocDealloc, env: *anyopaque) callconv(.C) void {
    _ = roc_dealloc;
    _ = env;
    // NoOp as our arena frees all memory at once
}

/// Roc reallocation function
fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.C) void {
    _ = roc_realloc;
    _ = env;
    @panic("Realloc not implemented in this example");
}

/// Roc debug function
fn rocDbgFn(roc_dbg: *const RocDbg, env: *anyopaque) callconv(.C) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.arena.allocator();

    const message = roc_dbg.utf8_bytes[0..roc_dbg.len];
    const bytes = std.fmt.allocPrint(allocator, "ROC DBG: {s}\n", .{message}) catch |err| {
        std.log.err("Failed to allocate debug message: {s}", .{@errorName(err)});
        return;
    };
    defer allocator.free(bytes);

    std.io.getStdErr().writeAll(bytes) catch |err| {
        std.log.err("Failed to write debug message to stderr: {s}", .{@errorName(err)});
        return;
    };
}

/// Roc expect failed function
fn rocExpectFailedFn(roc_expect: *const RocExpectFailed, env: *anyopaque) callconv(.C) void {
    _ = env;
    const message = roc_expect.utf8_bytes[0..roc_expect.len];
    std.debug.print("ROC EXPECT FAILED: {s}\n", .{message});
}

/// Roc crashed function
fn rocCrashedFn(roc_crashed: *const RocCrashed, env: *anyopaque) callconv(.C) noreturn {
    _ = env;
    const message = roc_crashed.utf8_bytes[0..roc_crashed.len];
    @panic(message);
}

// External symbol provided by the Roc runtime object file
// Follows RocCall ABI: ops, ret_ptr, then argument pointers
extern fn roc__processString(ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void;

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
    var roc_ops = RocOps{
        .env = @as(*anyopaque, @ptrCast(&host_env)),
        .roc_alloc = rocAllocFn,
        .roc_dealloc = rocDeallocFn,
        .roc_realloc = rocReallocFn,
        .roc_dbg = rocDbgFn,
        .roc_expect_failed = rocExpectFailedFn,
        .roc_crashed = rocCrashedFn,
        .host_fns = undefined, // No host functions for this simple example
    };

    // Create the input string for the Roc function (if it's a function)
    const input_string = "string from host";
    var input_roc_str = RocStr.fromSlice(input_string, &roc_ops);
    defer input_roc_str.decref(&roc_ops);

    // Arguments struct for single string parameter - consistent with struct-based approach
    // `extern struct` has well-defined in-memory layout matching the C ABI for the target
    const Args = extern struct { str: RocStr };
    var args = Args{ .str = input_roc_str };

    // Call the Roc entrypoint - pass argument pointer for functions, null for values
    var roc_str: RocStr = undefined;
    roc__processString(&roc_ops, @as(*anyopaque, @ptrCast(&roc_str)), @as(*anyopaque, @ptrCast(&args)));
    defer roc_str.decref(&roc_ops);

    // Get the string as a slice and print it
    const result_slice = roc_str.asSlice();
    try stdout.print("{s}", .{result_slice});

    // Verify the result contains the expected input
    const expected_substring = "Got the following from the host: string from host";
    if (std.mem.indexOf(u8, result_slice, expected_substring) != null) {
        try stdout.print("\n\x1b[32mSUCCESS\x1b[0m: Result contains expected substring!\n", .{});
    } else {
        try stdout.print("\n\x1b[31mFAIL\x1b[0m: Result does not contain expected substring!\n", .{});
    }
}
