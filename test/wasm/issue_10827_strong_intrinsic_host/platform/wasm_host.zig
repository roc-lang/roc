const host_alloc = @import("host_alloc");
var heap: [1024 * 1024]u8 align(16) = undefined;
var allocator: std.heap.FixedBufferAllocator = .init(&heap);

fn hostAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return host_alloc.alloc(allocator.allocator(), length, alignment) orelse @trap();
}

fn hostDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    host_alloc.dealloc(allocator.allocator(), ptr, alignment);
}

fn hostRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return host_alloc.realloc(allocator.allocator(), ptr, new_length, alignment) orelse @trap();
}

comptime {
    host_alloc.exportRuntimeFns(.{
        .alloc = &hostAlloc,
        .dealloc = &hostDealloc,
        .realloc = &hostRealloc,
        .dbg = &hostDbg,
        .expect_failed = &hostExpectFailed,
        .crashed = &hostCrashed,
    });
}

fn hostDbg(_: [*]const u8, _: usize) callconv(.c) void {}

fn hostExpectFailed(_: [*]const u8, _: usize) callconv(.c) void {}

fn hostCrashed(_: [*]const u8, _: usize) callconv(.c) void {
    @trap();
}

// Stable Rust has no stable weak-linkage attribute, so real platform hosts
// which implement compiler intrinsics by hand produce this same strong symbol.
// Its spelling must be irrelevant to the private Roc implementation.
export fn __multi3(a: i128, b: i128) callconv(.c) i128 {
    return a +% b;
}

extern fn roc_main() callconv(.c) u64;

export fn run() callconv(.c) u64 {
    return roc_main();
}

const std = @import("std");
