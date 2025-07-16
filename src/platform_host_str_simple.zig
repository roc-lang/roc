//! Simple platform host that calls into a simplified Roc entrypoint and prints a string result.

const std = @import("std");
const RocStr = @import("builtins/str.zig").RocStr;

// Roc memory allocation function
fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    const log2_align = std.math.log2_int(u32, alignment);
    const align_enum: std.mem.Alignment = @enumFromInt(log2_align);
    const result = std.heap.c_allocator.vtable.alloc(std.heap.c_allocator.ptr, size, align_enum, @returnAddress());
    return result;
}

// Roc memory deallocation function
export fn roc_dealloc(ptr: *anyopaque, alignment: u32) callconv(.C) void {
    _ = ptr;
    _ = alignment;
    // For this simple host, we don't actually deallocate
    // The memory is managed by the shim and cleaned up when the process exits
}

// External symbol provided by the Roc runtime object file
extern fn roc_entrypoint(roc_alloc: *const fn (size: usize, alignment: u32) callconv(.C) ?*anyopaque) RocStr;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    // Call the Roc entrypoint to get our string
    var roc_str = roc_entrypoint(&roc_alloc);
    defer roc_str.decref();

    // Get the string as a slice and print it
    try stdout.print("{s}\n", .{roc_str.asSlice()});
}
