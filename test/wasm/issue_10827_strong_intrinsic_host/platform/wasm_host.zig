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
