//! Verifies that a wasm host can invoke a boxed Roc callable through the
//! canonical erased-callable ABI.
//! Regression for https://github.com/roc-lang/roc/issues/10665.

const std = @import("std");
const builtins = @import("builtins");
const host_alloc = @import("host_alloc");

const RocOps = builtins.host_abi.RocOps;

extern fn roc_make_boxed_callable(offset: u64) callconv(.c) ?[*]u8;
extern fn roc_drop_boxed_callable(callable: ?[*]u8) callconv(.c) void;

const U64Arg = extern struct {
    arg0: u64,
};

const wasm_allocator = std.heap.wasm_allocator;

export fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    allocation_counts[0] += 1;
    return host_alloc.alloc(wasm_allocator, length, alignment);
}

export fn roc_dealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    allocation_counts[1] += 1;
    host_alloc.dealloc(wasm_allocator, ptr, alignment);
}

export fn roc_realloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    allocation_counts[0] += 1;
    allocation_counts[1] += 1;
    return host_alloc.realloc(wasm_allocator, ptr, new_length, alignment);
}

export fn roc_dbg(_: [*]const u8, _: usize) callconv(.c) void {}
export fn roc_expect_failed(_: [*]const u8, _: usize) callconv(.c) void {}

export fn roc_crashed(_: [*]const u8, _: usize) callconv(.c) void {
    @trap();
}

fn rocOpsAlloc(_: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return roc_alloc(length, alignment);
}

fn rocOpsDealloc(_: *RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    roc_dealloc(ptr, alignment);
}

fn rocOpsRealloc(_: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return roc_realloc(ptr, new_length, alignment);
}

fn rocOpsDbg(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    roc_dbg(bytes, len);
}

fn rocOpsExpectFailed(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    roc_expect_failed(bytes, len);
}

fn rocOpsCrashed(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    roc_crashed(bytes, len);
}

var host_context: u8 = 0;
var result: []const u8 = "";
var allocation_counts: [2]usize = .{ 0, 0 };

export fn wasm_main() [*]const u8 {
    var roc_ops = RocOps{
        .env = @ptrCast(&host_context),
        .roc_alloc = rocOpsAlloc,
        .roc_dealloc = rocOpsDealloc,
        .roc_realloc = rocOpsRealloc,
        .roc_dbg = rocOpsDbg,
        .roc_expect_failed = rocOpsExpectFailed,
        .roc_crashed = rocOpsCrashed,
        .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
    };
    const callable = roc_make_boxed_callable(1) orelse @trap();

    const payload = builtins.erased_callable.payloadPtr(callable);
    var args = U64Arg{ .arg0 = 41 };
    var value: u64 = 0;
    payload.callable_fn_ptr(
        &roc_ops,
        @ptrCast(&value),
        @ptrCast(&args),
        builtins.erased_callable.capturePtr(callable),
        null,
    );
    roc_drop_boxed_callable(callable);

    result = if (value == 42) "42" else "wrong result";
    return result.ptr;
}

export fn wasm_result_len() usize {
    return result.len;
}

export fn wasm_reset_alloc_counts() void {
    allocation_counts = .{ 0, 0 };
}

export fn wasm_alloc_count() usize {
    return allocation_counts[0];
}

export fn wasm_dealloc_count() usize {
    return allocation_counts[1];
}
