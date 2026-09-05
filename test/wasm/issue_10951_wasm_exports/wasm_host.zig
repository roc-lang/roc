//! Minimal wasm platform host for the `exports:` diagnostic fixtures.
//!
//! It defines the shim runtime symbols a linked Roc app needs and wraps the
//! app's `roc_main` entrypoint in a single host-visible `run` export, so the
//! only thing that decides what the final module exports is the platform
//! header's `exports:` field.

const std = @import("std");

var heap: [1024 * 1024]u8 align(16) = undefined;
var heap_top: usize = 0;

export fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const effective_alignment = @max(alignment, 1);
    const start = std.mem.alignForward(usize, heap_top, effective_alignment);
    const end = start + length;
    if (end > heap.len) @trap();
    heap_top = end;
    return &heap[start];
}

export fn roc_dealloc(_: *anyopaque, _: usize) callconv(.c) void {}

export fn roc_realloc(_: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return roc_alloc(new_length, alignment);
}

export fn roc_dbg(_: [*]const u8, _: usize) callconv(.c) void {}

export fn roc_expect_failed(_: [*]const u8, _: usize) callconv(.c) void {}

export fn roc_crashed(_: [*]const u8, _: usize) callconv(.c) void {
    @trap();
}

extern fn roc_main() callconv(.c) u64;

export fn run() callconv(.c) u64 {
    return roc_main();
}
