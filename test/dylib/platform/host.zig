//! Host for the shared-library test platform. Unlike executable platform hosts,
//! this code is linked INTO the shared library that `roc build` produces, and it
//! exposes the library's outward-facing C API (`roc_run_app`). A separate loader
//! process (test/dylib/loader.zig) dlopens the library and calls that API, which
//! drives the Roc app through the platform ABI.

const std = @import("std");
const shim_io = @import("shim_io");
const builtins = @import("builtins");

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
pub const std_options_debug_io = shim_io.io();
pub const std_options_debug_threaded_io = null;
// See `shim_io.std_options_no_stack_tracing` for why stack tracing is disabled.
pub const std_options = shim_io.std_options_no_stack_tracing;

/// Host environment - contains our arena allocator
const HostEnv = struct {
    arena: std.heap.ArenaAllocator,
};

/// Roc allocation function with size-tracking metadata
fn rocAllocFn(roc_alloc: *builtins.host_abi.RocAlloc, env: *anyopaque) callconv(.c) void {
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
fn rocDeallocFn(roc_dealloc: *builtins.host_abi.RocDealloc, env: *anyopaque) callconv(.c) void {
    _ = roc_dealloc;
    _ = env;
    // NoOp as our arena frees all memory at once
}

/// Roc reallocation function
fn rocReallocFn(roc_realloc: *builtins.host_abi.RocRealloc, env: *anyopaque) callconv(.c) void {
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
fn rocDbgFn(roc_dbg: *const builtins.host_abi.RocDbg, env: *anyopaque) callconv(.c) void {
    _ = env;
    const message = roc_dbg.utf8_bytes[0..roc_dbg.len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

/// Roc expect failed function
fn rocExpectFailedFn(roc_expect: *const builtins.host_abi.RocExpectFailed, env: *anyopaque) callconv(.c) void {
    _ = env;
    const source_bytes = roc_expect.utf8_bytes[0..roc_expect.len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

/// Roc crashed function
fn rocCrashedFn(roc_crashed: *const builtins.host_abi.RocCrashed, env: *anyopaque) callconv(.c) noreturn {
    _ = env;
    const message = roc_crashed.utf8_bytes[0..roc_crashed.len];
    @panic(message);
}

// The app's entrypoint, named by `provides { main_for_host!: "main" }`.
// Follows the RocCall ABI: ops, ret_ptr, then a pointer to the argument tuple.
extern fn roc__main(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;

/// Host.double! (dispatch index 0): double a number in the host.
fn hostedHostDouble(roc_ops: *builtins.host_abi.RocOps, ret_ptr: *i64, arg_ptr: *i64) callconv(.c) void {
    _ = roc_ops;
    ret_ptr.* = arg_ptr.* * 2;
}

// Hosted functions sorted alphabetically by Module.fn_name (trailing ! stripped).
const hosted_function_ptrs = [_]builtins.host_abi.HostedFn{
    builtins.host_abi.hostedFn(&hostedHostDouble), // Host.double! (index 0)
};

/// The shared library's outward-facing C API: run the Roc app on `n` and
/// return its answer.
export fn roc_run_app(n: i64) callconv(.c) i64 {
    var host_env = HostEnv{
        .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
    };
    defer host_env.arena.deinit();

    var roc_ops = builtins.host_abi.RocOps{
        .env = @as(*anyopaque, @ptrCast(&host_env)),
        .roc_alloc = rocAllocFn,
        .roc_dealloc = rocDeallocFn,
        .roc_realloc = rocReallocFn,
        .roc_dbg = rocDbgFn,
        .roc_expect_failed = rocExpectFailedFn,
        .roc_crashed = rocCrashedFn,
        .hosted_fns = .{
            .count = hosted_function_ptrs.len,
            .fns = @constCast(&hosted_function_ptrs),
        },
    };

    var arg: i64 = n;
    var ret: i64 = undefined;
    roc__main(&roc_ops, @ptrCast(&ret), @ptrCast(&arg));
    return ret;
}
