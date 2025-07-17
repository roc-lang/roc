//! Simple platform host that calls into a simplified Roc entrypoint and prints a string result.

const std = @import("std");
const RocStr = @import("builtins/str.zig").RocStr;

// Global arena allocator for Roc memory management
// Using arena allocator since we don't need individual deallocations for this simple example
var arena_allocator = std.heap.ArenaAllocator.init(std.heap.page_allocator);

// Allocator to give to Roc
fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    const log2_align = std.math.log2_int(u32, alignment);
    const align_enum: std.mem.Alignment = @enumFromInt(log2_align);
    const allocator = arena_allocator.allocator();
    const result = allocator.rawAlloc(size, align_enum, @returnAddress());
    return result;
}

// Roc memory deallocation function
export fn roc_dealloc(ptr: *anyopaque, alignment: u32) callconv(.C) void {
    _ = ptr;
    _ = alignment;
    // NoOp as our arena frees all memory at once
}

// External symbol provided by the Roc runtime object file
extern fn roc_entrypoint(roc_alloc: *const fn (size: usize, alignment: u32) callconv(.C) ?*anyopaque) RocStr;

/// Platform host entrypoint -- this is where the roc application starts and does platform things
/// before the platform calls into Roc to do application-specific things.
pub fn main() !void {
    defer arena_allocator.deinit(); // Clean up all allocations on exit

    const stdout = std.io.getStdOut().writer();

    // Call the Roc entrypoint to get our string
    var roc_str = roc_entrypoint(&roc_alloc);
    defer roc_str.decref();

    // Get the string as a slice and print it
    try stdout.print("{s}\n", .{roc_str.asSlice()});
}
