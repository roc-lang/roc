//! Host for the static-archive test platform. This code is packaged INTO the
//! archive that `roc build` produces, and it exposes the archive's outward-facing
//! C API (`roc_run_app`). A separate consumer program (test/archive/consumer.zig)
//! links the archive and calls that API, which drives the Roc app through the
//! platform ABI.

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
fn rocAllocFn(ops: *builtins.host_abi.RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const allocator = host.arena.allocator();

    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    // Prepend size metadata so realloc can know the old size
    const size_storage_bytes = @max(alignment, @alignOf(usize));
    const total_size = length + size_storage_bytes;

    const base_ptr = allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse {
        @panic("Host allocation failed");
    };

    // Store total size right before the user data
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    return @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
}

/// Roc deallocation function
fn rocDeallocFn(ops: *builtins.host_abi.RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    _ = ops;
    _ = ptr;
    _ = alignment;
    // NoOp as our arena frees all memory at once
}

/// Roc reallocation function
fn rocReallocFn(ops: *builtins.host_abi.RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const allocator = host.arena.allocator();

    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    const size_storage_bytes = @max(alignment, @alignOf(usize));

    // Read old size from metadata
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
    const old_total_size = old_size_ptr.*;

    // Allocate new block
    const new_total_size = new_length + size_storage_bytes;
    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse {
        @panic("Host reallocation failed");
    };

    // Copy old data to new location
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);
    const copy_size = @min(old_total_size, new_total_size);
    @memcpy(new_ptr[0..copy_size], old_base_ptr[0..copy_size]);

    // Store new size in metadata
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    return @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes);
}

/// Roc debug function
fn rocDbgFn(ops: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    std.debug.print("ROC DBG: {s}\n", .{bytes[0..len]});
}

/// Roc expect failed function
fn rocExpectFailedFn(ops: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    const trimmed = std.mem.trim(u8, bytes[0..len], " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

/// Roc crashed function
fn rocCrashedFn(ops: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    @panic(bytes[0..len]);
}

// The app's entrypoint, named by `provides { main_for_host!: "main" }`.
// Follows the RocCall ABI: ops, ret_ptr, then a pointer to the argument tuple.
extern fn roc__main(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;

/// Host.double! (dispatch index 0): double a number in the host.
/// I64 -> I64 involves no refcounted values, so under the hosted C ABI it
/// receives no leading *RocOps.
fn hostedHostDouble(n: i64) callconv(.c) i64 {
    return n * 2;
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
