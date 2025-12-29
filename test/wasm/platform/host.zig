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
export fn roc_alloc(size: usize, alignment: u32) callconv(.c) ?*anyopaque {
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, alignment));
    const result = wasm_allocator.rawAlloc(size, align_log2, @returnAddress());
    return @ptrCast(result);
}

export fn roc_dealloc(ptr: *anyopaque, alignment: u32) callconv(.c) void {
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, alignment));
    // WasmAllocator tracks size via size classes, but we need to pass something for rawFree
    // Use 0 as the size since WasmAllocator doesn't actually need it for deallocation
    const slice_ptr: [*]u8 = @ptrCast(ptr);
    wasm_allocator.rawFree(slice_ptr[0..0], align_log2, @returnAddress());
}

export fn roc_realloc(ptr: *anyopaque, new_size: usize, old_size: usize, alignment: u32) callconv(.c) ?*anyopaque {
    // For WASM, we need to alloc new, copy, and free old since rawRealloc may not be available
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, alignment));
    const slice_ptr: [*]u8 = @ptrCast(ptr);

    // Allocate new memory
    const new_ptr = wasm_allocator.rawAlloc(new_size, align_log2, @returnAddress()) orelse return null;

    // Copy old data
    const copy_size = @min(old_size, new_size);
    @memcpy(new_ptr[0..copy_size], slice_ptr[0..copy_size]);

    // Free old memory
    wasm_allocator.rawFree(slice_ptr[0..old_size], align_log2, @returnAddress());

    return @ptrCast(new_ptr);
}

// RocOps callback implementations

fn rocAllocFn(alloc_req: *RocAlloc, env: *anyopaque) callconv(.c) void {
    _ = env;
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, alloc_req.alignment));
    const ptr = wasm_allocator.rawAlloc(alloc_req.length, align_log2, @returnAddress());
    alloc_req.answer = @ptrCast(ptr orelse @panic("WASM allocation failed"));
}

fn rocDeallocFn(dealloc_req: *RocDealloc, env: *anyopaque) callconv(.c) void {
    _ = env;
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, dealloc_req.alignment));
    const slice_ptr: [*]u8 = @ptrCast(dealloc_req.ptr);
    wasm_allocator.rawFree(slice_ptr[0..0], align_log2, @returnAddress());
}

fn rocReallocFn(realloc_req: *RocRealloc, env: *anyopaque) callconv(.c) void {
    _ = env;
    // RocRealloc doesn't provide the old pointer or size - this is an allocation-only operation
    // Just allocate new memory
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, realloc_req.alignment));
    const ptr = wasm_allocator.rawAlloc(realloc_req.new_length, align_log2, @returnAddress());
    realloc_req.answer = @ptrCast(ptr orelse @panic("WASM reallocation failed"));
}

fn rocDbgFn(roc_dbg_arg: *const RocDbg, env: *anyopaque) callconv(.c) void {
    _ = env;
    roc_dbg(roc_dbg_arg.utf8_bytes, roc_dbg_arg.len);
}

fn rocExpectFailedFn(roc_expect: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    _ = env;
    roc_expect_failed(roc_expect.utf8_bytes, roc_expect.len);
}

fn rocCrashedFn(roc_crashed: *const RocCrashed, env: *anyopaque) callconv(.c) noreturn {
    _ = env;
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
