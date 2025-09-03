const std = @import("std");
const gt = @import("greenthreads.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var store = gt.ThreadStore.init(allocator);
    defer store.deinit();

    std.debug.print("Starting test\n", .{});

    var output: i32 = 0;
    const testFunc = struct {
        fn compute(in: *anyopaque, out: *anyopaque) callconv(.C) void {
            _ = in;
            const typed_out = @as(*i32, @ptrCast(@alignCast(out)));
            std.debug.print("In thread function, setting value to 42\n", .{});
            typed_out.* = 42;
        }
    }.compute;

    var dummy: i32 = 0;
    std.debug.print("Before spawn\n", .{});
    try store.spawn(testFunc, @ptrCast(&dummy), @ptrCast(&output));
    std.debug.print("After spawn, output = {}\n", .{output});
}