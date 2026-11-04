//! A wasm platform host that leaves the app's hosted `roc_conflicting_put`
//! to the embedder and calls it itself with a signature that disagrees
//! with the app's: `(u32) void` where the app's `U64 => {}` is
//! `(u64) void`. Linking the two must report the conflict.

const host_alloc = @import("host_alloc");
const std = @import("std");

var heap: [1024 * 1024]u8 align(16) = undefined;
var allocator: std.heap.FixedBufferAllocator = .init(&heap);

fn hostAlloc(length: usize, alignment: usize) callconv(.c) *anyopaque {
    return host_alloc.alloc(allocator.allocator(), length, alignment) orelse @trap();
}

fn hostDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    host_alloc.dealloc(allocator.allocator(), ptr, alignment);
}

fn hostRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) *anyopaque {
    return host_alloc.realloc(allocator.allocator(), ptr, new_length, alignment) orelse @trap();
}

fn hostDbg(_: [*]const u8, _: usize) callconv(.c) void {}

fn hostExpectFailed(_: [*]const u8, _: usize) callconv(.c) void {}

fn hostCrashed(_: [*]const u8, _: usize) callconv(.c) void {
    @trap();
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

extern fn roc_conflicting_put(value: u32) callconv(.c) void;
extern fn roc_main() callconv(.c) u64;

export fn run() callconv(.c) u64 {
    roc_conflicting_put(1);
    return roc_main();
}
