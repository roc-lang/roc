//! WASM platform host for testing static_lib builds.
//!
//! This host is designed to be compiled to WebAssembly and linked with a Roc
//! application that targets static_lib. It imports runtime functions from the
//! host environment (JavaScript in browsers, or bytebox for testing).
//!
//! ## Imported Functions
//!
//! The WASM module imports these functions from the "env" namespace:
//! - roc_panic: Called when the Roc program panics
//! - roc_dbg: Called when the Roc program uses dbg
//! - roc_expect_failed: Called when an expect fails
//!
//! ## Memory Management
//!
//! Uses Zig's standard WASM allocator for proper memory management with
//! deallocation and reallocation support.

const std = @import("std");
const builtins = @import("builtins");

const RocStr = builtins.str.RocStr;
const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;

// Import functions from the host environment
extern "env" fn roc_panic(ptr: [*]const u8, len: usize) noreturn;
extern "env" fn roc_dbg(ptr: [*]const u8, len: usize) void;
extern "env" fn roc_expect_failed(ptr: [*]const u8, len: usize) void;

// Use Zig's standard WASM allocator for proper memory management
const wasm_allocator = std.heap.wasm_allocator;

// Direct exports for the interpreter shim's memory allocation
// (The interpreter shim's wasmAlloc calls these directly)
//
// IMPORTANT: Since roc_dealloc doesn't receive the allocation size, but Zig's WasmAllocator
// needs it to determine the correct size class, we store the size at the beginning of each
// allocation and return a pointer past it. This adds @sizeOf(usize) overhead per allocation.

export fn roc_alloc(size: usize, alignment: u32) callconv(.c) ?*anyopaque {
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, alignment));

    // Header size must be at least alignment to ensure returned pointer is properly aligned
    // We store the size in the header, which only needs @sizeOf(usize) bytes
    const header_size = @max(alignment, @sizeOf(usize));
    const total_size = size + header_size;

    const result = wasm_allocator.rawAlloc(total_size, align_log2, @returnAddress()) orelse return null;

    // Store the original requested size at the beginning
    const size_ptr: *usize = @ptrCast(@alignCast(result));
    size_ptr.* = size;

    // Return pointer past the header (properly aligned)
    return @ptrCast(result + header_size);
}

export fn roc_dealloc(ptr: *anyopaque, alignment: u32) callconv(.c) void {
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, alignment));

    // Calculate header size (must match roc_alloc)
    const header_size = @max(alignment, @sizeOf(usize));

    // Get the base pointer (before the header we stored in roc_alloc)
    const byte_ptr: [*]u8 = @ptrCast(ptr);
    const base_ptr = byte_ptr - header_size;

    // Read the original size from the header
    const size_ptr: *const usize = @ptrCast(@alignCast(base_ptr));
    const original_size = size_ptr.*;

    // Free the full allocation including the header
    const total_size = original_size + header_size;
    wasm_allocator.rawFree(base_ptr[0..total_size], align_log2, @returnAddress());
}

export fn roc_realloc(ptr: *anyopaque, new_size: usize, old_size: usize, alignment: u32) callconv(.c) ?*anyopaque {
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, alignment));

    // Calculate header size (must match roc_alloc)
    const header_size = @max(alignment, @sizeOf(usize));

    // Get the base pointer (before the header)
    const byte_ptr: [*]u8 = @ptrCast(ptr);
    const base_ptr = byte_ptr - header_size;

    // Allocate new memory with header
    const new_total_size = new_size + header_size;
    const new_base_ptr = wasm_allocator.rawAlloc(new_total_size, align_log2, @returnAddress()) orelse return null;

    // Store new size at the beginning
    const new_size_ptr: *usize = @ptrCast(@alignCast(new_base_ptr));
    new_size_ptr.* = new_size;

    // Copy old data (from user pointer to new user pointer)
    const new_user_ptr = new_base_ptr + header_size;
    const copy_size = @min(old_size, new_size);
    @memcpy(new_user_ptr[0..copy_size], byte_ptr[0..copy_size]);

    // Free old memory including the header
    const old_total_size = old_size + header_size;
    wasm_allocator.rawFree(base_ptr[0..old_total_size], align_log2, @returnAddress());

    return @ptrCast(new_user_ptr);
}

// RocOps callback implementations
// These use the same size-header approach as the exported roc_alloc/roc_dealloc,
// because RocDealloc doesn't provide the length (by design for seamless slices).

fn rocAllocFn(alloc_req: *RocAlloc, _: *anyopaque) callconv(.c) void {
    const alignment: u32 = @intCast(alloc_req.alignment);
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, alignment));

    // Header size must be at least alignment to ensure returned pointer is properly aligned
    const header_size = @max(alignment, @sizeOf(usize));
    const total_size = alloc_req.length + header_size;

    const result = wasm_allocator.rawAlloc(total_size, align_log2, @returnAddress()) orelse @panic("WASM allocation failed");

    // Store the original requested size at the beginning
    const size_ptr: *usize = @ptrCast(@alignCast(result));
    size_ptr.* = alloc_req.length;

    // Return pointer past the header (properly aligned)
    alloc_req.answer = @ptrCast(result + header_size);
}

fn rocDeallocFn(dealloc_req: *RocDealloc, _: *anyopaque) callconv(.c) void {
    const alignment: u32 = @intCast(dealloc_req.alignment);
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, alignment));

    // Calculate header size (must match rocAllocFn)
    const header_size = @max(alignment, @sizeOf(usize));

    // Get the base pointer (before the header we stored in rocAllocFn)
    const byte_ptr: [*]u8 = @ptrCast(dealloc_req.ptr);
    const base_ptr = byte_ptr - header_size;

    // Read the original size from the header
    const size_ptr: *const usize = @ptrCast(@alignCast(base_ptr));
    const original_size = size_ptr.*;

    // Free the full allocation including the header
    const total_size = original_size + header_size;
    wasm_allocator.rawFree(base_ptr[0..total_size], align_log2, @returnAddress());
}

fn rocReallocFn(realloc_req: *RocRealloc, _: *anyopaque) callconv(.c) void {
    // RocRealloc provides new_length but we need to allocate with size header
    const alignment: u32 = @intCast(realloc_req.alignment);
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, alignment));

    // Header size must be at least alignment
    const header_size = @max(alignment, @sizeOf(usize));
    const total_size = realloc_req.new_length + header_size;

    const result = wasm_allocator.rawAlloc(total_size, align_log2, @returnAddress()) orelse @panic("WASM reallocation failed");

    // Store the size at the beginning
    const size_ptr: *usize = @ptrCast(@alignCast(result));
    size_ptr.* = realloc_req.new_length;

    // Return pointer past the header
    realloc_req.answer = @ptrCast(result + header_size);
}

fn rocDbgFn(roc_dbg_arg: *const RocDbg, _: *anyopaque) callconv(.c) void {
    roc_dbg(roc_dbg_arg.utf8_bytes, roc_dbg_arg.len);
}

fn rocExpectFailedFn(roc_expect: *const RocExpectFailed, _: *anyopaque) callconv(.c) void {
    roc_expect_failed(roc_expect.utf8_bytes, roc_expect.len);
}

fn rocCrashedFn(roc_crashed: *const RocCrashed, _: *anyopaque) callconv(.c) noreturn {
    roc_panic(roc_crashed.utf8_bytes, roc_crashed.len);
}

// External Roc entrypoint
extern fn roc__main(ops: *RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;

// Dummy env for RocOps (not used in WASM)
var dummy_env: u8 = 0;

// Empty hosted functions array (this platform has no hosted effects)
const empty_hosted_fns = [_]builtins.host_abi.HostedFn{};

// Store the last result for wasm_result_len()
var last_result: RocStr = undefined;

/// Main export for WASM module
/// Returns a pointer to the result string data and its length
export fn wasm_main() [*]const u8 {
    var roc_ops = RocOps{
        .env = @ptrCast(&dummy_env),
        .roc_alloc = rocAllocFn,
        .roc_dealloc = rocDeallocFn,
        .roc_realloc = rocReallocFn,
        .roc_dbg = rocDbgFn,
        .roc_expect_failed = rocExpectFailedFn,
        .roc_crashed = rocCrashedFn,
        .hosted_fns = .{
            .count = 0,
            .fns = @constCast(&empty_hosted_fns),
        },
    };

    // Pass a valid pointer for the unit argument () - the pointer itself doesn't
    // matter for zero-sized types, but it must be non-null to indicate "call this function"
    var unit_arg: u8 = 0;
    roc__main(&roc_ops, @ptrCast(&last_result), @ptrCast(&unit_arg));

    // Return pointer to the string bytes - use asU8ptr() for SSO support
    return last_result.asU8ptr();
}

/// Get the length of the result string from the last wasm_main() call
export fn wasm_result_len() usize {
    return last_result.len();
}
