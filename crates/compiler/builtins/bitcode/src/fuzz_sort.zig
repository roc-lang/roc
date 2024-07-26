const std = @import("std");
const sort = @import("sort.zig");

extern fn malloc(size: usize) callconv(.C) ?*anyopaque;
extern fn free(c_ptr: *anyopaque) callconv(.C) void;

pub fn main() !void {
    const size = 1000000;
    var arr_ptr: [*]i64 = @alignCast(@ptrCast(testing_roc_alloc(size * @sizeOf(i64), @alignOf(i64))));
    defer testing_roc_dealloc(arr_ptr, @alignOf(i64));

    const seed = 42;
    var rng = std.rand.DefaultPrng.init(seed);
    for (0..size) |i| {
        arr_ptr[i] = rng.random().int(i64);
    }

    var timer = try std.time.Timer.start();
    sort.quadsort(@ptrCast(arr_ptr), size, &test_i64_compare, null, false, &test_i64_inc_n, @sizeOf(i64), @alignOf(i64), &test_i64_copy);
    const elapsed: f64 = @floatFromInt(timer.read());
    std.debug.print("Time elapsed is: {d:.3}ms\n", .{
        elapsed / std.time.ns_per_ms,
    });
}

const Opaque = ?[*]u8;
fn test_i64_compare(_: Opaque, a_ptr: Opaque, b_ptr: Opaque) callconv(.C) u8 {
    const a = @as(*i64, @alignCast(@ptrCast(a_ptr))).*;
    const b = @as(*i64, @alignCast(@ptrCast(b_ptr))).*;

    const gt = @as(u8, @intFromBool(a > b));
    const lt = @as(u8, @intFromBool(a < b));

    // Eq = 0
    // GT = 1
    // LT = 2
    return lt + lt + gt;
}

fn test_i64_copy(dst_ptr: Opaque, src_ptr: Opaque) callconv(.C) void {
    @as(*i64, @alignCast(@ptrCast(dst_ptr))).* = @as(*i64, @alignCast(@ptrCast(src_ptr))).*;
}

fn test_i64_inc_n(_: ?[*]u8, _: usize) callconv(.C) void {}

comptime {
    @export(testing_roc_alloc, .{ .name = "roc_alloc", .linkage = .Strong });
    @export(testing_roc_dealloc, .{ .name = "roc_dealloc", .linkage = .Strong });
    @export(testing_roc_panic, .{ .name = "roc_panic", .linkage = .Strong });
}

fn testing_roc_alloc(size: usize, _: u32) callconv(.C) ?*anyopaque {
    return malloc(size);
}

fn testing_roc_dealloc(c_ptr: *anyopaque, _: u32) callconv(.C) void {
    free(c_ptr);
}

fn testing_roc_panic(c_ptr: *anyopaque, tag_id: u32) callconv(.C) void {
    _ = c_ptr;
    _ = tag_id;

    @panic("Roc panicked");
}
