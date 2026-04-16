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

/// Roc allocation function with size-tracking metadata
fn rocAllocFn(roc_alloc: *RocAlloc, env: *anyopaque) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.arena.allocator();

    const min_alignment: usize = @max(roc_alloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    // Prepend size metadata so realloc can know the old size
    const size_storage_bytes = @max(roc_alloc.alignment, @alignOf(usize));
    const total_size = roc_alloc.length + size_storage_bytes;

    const base_ptr = allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse {
        @panic("Host allocation failed");
    };

    // Store total size right before the user data
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    roc_alloc.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
}

/// Roc deallocation function
fn rocDeallocFn(roc_dealloc: *RocDealloc, env: *anyopaque) callconv(.c) void {
    _ = roc_dealloc;
    _ = env;
    // NoOp as our arena frees all memory at once
}

/// Roc reallocation function
fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.arena.allocator();

    const min_alignment: usize = @max(roc_realloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    const size_storage_bytes = @max(roc_realloc.alignment, @alignOf(usize));

    // Read old size from metadata
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(roc_realloc.answer) - @sizeOf(usize));
    const old_total_size = old_size_ptr.*;

    // Allocate new block
    const new_total_size = roc_realloc.new_length + size_storage_bytes;
    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse {
        @panic("Host reallocation failed");
    };

    // Copy old data to new location
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(roc_realloc.answer) - size_storage_bytes);
    const copy_size = @min(old_total_size, new_total_size);
    @memcpy(new_ptr[0..copy_size], old_base_ptr[0..copy_size]);

    // Store new size in metadata
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    roc_realloc.answer = @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes);
}

/// Roc debug function
fn rocDbgFn(roc_dbg: *const RocDbg, env: *anyopaque) callconv(.c) void {
    _ = env;
    const message = roc_dbg.utf8_bytes[0..roc_dbg.len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

/// Roc expect failed function
fn rocExpectFailedFn(roc_expect: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    _ = env;
    const source_bytes = roc_expect.utf8_bytes[0..roc_expect.len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

/// Roc crashed function
fn rocCrashedFn(roc_crashed: *const RocCrashed, env: *anyopaque) callconv(.c) noreturn {
    _ = env;
    const message = roc_crashed.utf8_bytes[0..roc_crashed.len];
    @panic(message);
}

// External symbol provided by the Roc runtime object file
// Follows RocCall ABI: ops, ret_ptr, then argument pointers
extern fn roc__process_string(ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;

// OS-specific entry point handling
comptime {
    // Export main for all platforms
    @export(&main, .{ .name = "main" });

    // Windows MinGW/MSVCRT compatibility: export __main stub
    if (@import("builtin").os.tag == .windows) {
        @export(&__main, .{ .name = "__main" });
    }
}

// Windows MinGW/MSVCRT compatibility stub
// The C runtime on Windows calls __main from main for constructor initialization
fn __main() callconv(.c) void {}

// C compatible main for runtime
fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    _ = argc;
    _ = argv;
    platform_main() catch |err| {
        std.debug.print("HOST ERROR: {s}", .{@errorName(err)});
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

    // Create the RocOps struct
    var roc_ops = RocOps{
        .env = @as(*anyopaque, @ptrCast(&host_env)),
        .roc_alloc = rocAllocFn,
        .roc_dealloc = rocDeallocFn,
        .roc_realloc = rocReallocFn,
        .roc_dbg = rocDbgFn,
        .roc_expect_failed = rocExpectFailedFn,
        .roc_crashed = rocCrashedFn,
        .hosted_fns = .{ .count = 0, .fns = undefined }, // No host functions for this simple example
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
    roc__process_string(&roc_ops, @as(*anyopaque, @ptrCast(&roc_str)), @as(*anyopaque, @ptrCast(&args)));
    defer roc_str.decref(&roc_ops);

    // Get the string as a slice and print it
    const result_slice = roc_str.asSlice();
    std.debug.print("{s}", .{result_slice});

    // Verify the result contains the expected input
    const expected_substring = "Got the following from the host: string from host";
    if (std.mem.find(u8, result_slice, expected_substring) != null) {
        std.debug.print("\n\x1b[32mSUCCESS\x1b[0m: Result contains expected substring!\n", .{});
    } else {
        std.debug.print("\n\x1b[31mFAIL\x1b[0m: Result does not contain expected substring!\n", .{});
        return error.TestFailed;
    }
}
