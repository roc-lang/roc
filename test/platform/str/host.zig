//! Simple platform host that calls into a simplified Roc entrypoint and prints a string result.

const std = @import("std");
const builtins = @import("builtins");

const RocStr = builtins.str.RocStr;

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

// External symbol provided by the Roc runtime object file
// Follows RocCall ABI: ops, ret_ptr, then argument pointers
extern fn roc_entrypoint(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.C) void;

/// Platform host entrypoint -- this is where the roc application starts and does platform things
/// before the platform calls into Roc to do application-specific things.
pub fn main() !void {
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

    // Create the input string for the Roc function (if it's a function)
    const input_string = "string from host";
    var input_roc_str = RocStr.fromSlice(input_string, &roc_ops);
    defer input_roc_str.decref(&roc_ops);

    // Arguments struct for single string parameter - consistent with struct-based approach
    const Args = struct {
        str: RocStr,
    };
    
    var args = Args{
        .str = input_roc_str,
    };

    // Call the Roc entrypoint - pass argument pointer for functions, null for values
    var roc_str: RocStr = undefined;
    roc_entrypoint(&roc_ops, @as(*anyopaque, @ptrCast(&roc_str)), @as(*anyopaque, @ptrCast(&args)));
    defer roc_str.decref(&roc_ops);

    // Get the string as a slice and print it
    const result_slice = roc_str.asSlice();
    try stdout.print("{s}", .{result_slice});
    
    // Verify the result contains the expected input
    const expected_substring = "Got the following from the host: string from host";
    if (std.mem.indexOf(u8, result_slice, expected_substring) == null) {
        std.process.exit(1);
    }
}
