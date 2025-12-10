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
//! Uses a simple bump allocator for WASM linear memory. Memory is allocated
//! from the end of the data section and grows upward.

const std = @import("std");
const builtin = @import("builtin");
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

// Bump allocator state
var heap_ptr: usize = 0;
var heap_end: usize = 0;

// Initialize heap from __heap_base symbol (provided by wasm-ld)
extern var __heap_base: u8;

fn initHeap() void {
    if (heap_ptr == 0) {
        heap_ptr = @intFromPtr(&__heap_base);
        // Start with 64KB of heap, can grow via memory.grow
        heap_end = heap_ptr + 65536;
    }
}

fn bumpAlloc(size: usize, alignment: usize) ?[*]u8 {
    initHeap();

    // Align the heap pointer
    const aligned_ptr = (heap_ptr + alignment - 1) & ~(alignment - 1);
    const new_heap_ptr = aligned_ptr + size;

    // Check if we need to grow memory
    if (new_heap_ptr > heap_end) {
        const pages_needed = (new_heap_ptr - heap_end + 65535) / 65536;
        const result = @wasmMemoryGrow(0, pages_needed);
        if (result == -1) {
            return null; // Out of memory
        }
        heap_end += pages_needed * 65536;
    }

    heap_ptr = new_heap_ptr;
    return @ptrFromInt(aligned_ptr);
}

// Direct exports for the interpreter shim's memory allocation
// (The interpreter shim's wasmAlloc calls these directly)
export fn roc_alloc(size: usize, alignment: u32) callconv(.c) ?*anyopaque {
    return @ptrCast(bumpAlloc(size, alignment));
}

export fn roc_dealloc(_: *anyopaque, _: u32) callconv(.c) void {
    // Bump allocator doesn't free individual allocations
}

export fn roc_realloc(ptr: *anyopaque, new_size: usize, _: usize, alignment: u32) callconv(.c) ?*anyopaque {
    // Simple realloc: allocate new, don't copy (bump allocator limitation)
    _ = ptr;
    return @ptrCast(bumpAlloc(new_size, alignment));
}

// RocOps callback implementations

fn rocAllocFn(alloc_req: *RocAlloc, env: *anyopaque) callconv(.c) void {
    _ = env;
    const ptr = bumpAlloc(alloc_req.length, alloc_req.alignment);
    alloc_req.answer = @ptrCast(ptr);
}

fn rocDeallocFn(dealloc_req: *RocDealloc, env: *anyopaque) callconv(.c) void {
    // Bump allocator doesn't free individual allocations
    _ = dealloc_req;
    _ = env;
}

fn rocReallocFn(realloc_req: *RocRealloc, env: *anyopaque) callconv(.c) void {
    _ = env;
    // Simple realloc: just allocate new memory
    // For a bump allocator, we can't efficiently realloc - just allocate new
    // Note: old data is NOT copied since we don't know the old length
    // The caller is expected to handle this or use a more sophisticated allocator
    const new_ptr = bumpAlloc(realloc_req.new_length, realloc_req.alignment);
    if (new_ptr) |ptr| {
        realloc_req.answer = @ptrCast(ptr);
    }
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

/// Get heap usage for debugging
export fn wasm_heap_used() usize {
    initHeap();
    return heap_ptr - @intFromPtr(&__heap_base);
}
