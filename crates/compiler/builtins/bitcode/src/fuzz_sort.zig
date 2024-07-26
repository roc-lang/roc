const std = @import("std");
const sort = @import("sort.zig");

extern fn malloc(size: usize) callconv(.C) ?*anyopaque;
extern fn free(c_ptr: *anyopaque) callconv(.C) void;

fn cMain() callconv(.C) void {
    fuzz_main() catch unreachable;
}

comptime {
    @export(cMain, .{ .name = "main", .linkage = .Strong });
}

var allocator: std.mem.Allocator = undefined;

pub fn fuzz_main() !void {
    // Setup an allocator that will detect leaks/use-after-free/etc
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // this will check for leaks and crash the program if it finds any
    defer std.debug.assert(gpa.deinit() == .ok);
    allocator = gpa.allocator();

    // Read the data from stdin
    const stdin = std.io.getStdIn();
    const data = try stdin.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(data);

    const size = data.len / @sizeOf(i64);
    const arr_ptr: [*]i64 = @alignCast(@ptrCast(data.ptr));

    sort.quadsort(@ptrCast(arr_ptr), size, &test_i64_compare, null, false, &test_i64_inc_n, @sizeOf(i64), @alignOf(i64), &test_i64_copy);

    std.debug.assert(std.sort.isSorted(i64, arr_ptr[0..size], {}, std.sort.asc(i64)));
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
    // We store an extra usize which is the size of the full allocation.
    const full_size = size + @sizeOf(usize);
    var raw_ptr = (allocator.alloc(u8, full_size) catch unreachable).ptr;
    @as([*]usize, @alignCast(@ptrCast(raw_ptr)))[0] = full_size;
    raw_ptr += @sizeOf(usize);
    return @as(?*anyopaque, @ptrCast(raw_ptr));
}

fn testing_roc_dealloc(c_ptr: *anyopaque, _: u32) callconv(.C) void {
    const raw_ptr = @as([*]u8, @ptrCast(c_ptr)) - @sizeOf(usize);
    const full_size = @as([*]usize, @alignCast(@ptrCast(raw_ptr)))[0];
    const slice = raw_ptr[0..full_size];
    allocator.free(slice);
}

fn testing_roc_panic(c_ptr: *anyopaque, tag_id: u32) callconv(.C) void {
    _ = c_ptr;
    _ = tag_id;

    @panic("Roc panicked");
}
