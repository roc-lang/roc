const std = @import("std");
const sort = @import("sort.zig");

extern fn malloc(size: usize) callconv(.C) ?*anyopaque;
extern fn free(c_ptr: *anyopaque) callconv(.C) void;

fn cMain() callconv(.C) i32 {
    fuzz_main() catch unreachable;
    return 0;
}

comptime {
    @export(&cMain, .{ .name = "main", .linkage = .Strong });
}

const DEBUG = false;

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

    const len = data.len / @sizeOf(i64);
    const arr_ptr: [*]i64 = @alignCast(@ptrCast(data.ptr));

    if (DEBUG) {
        std.debug.print("Input: [{d}]{d}\n", .{ len, arr_ptr[0..len] });
    }

    var test_count: i64 = 0;
    sort.fluxsort(@ptrCast(arr_ptr), len, &test_i64_compare_refcounted, @ptrCast(&test_count), true, &test_inc_n_data, @sizeOf(i64), @alignOf(i64), &test_i64_copy);

    const sorted = std.sort.isSorted(i64, arr_ptr[0..len], {}, std.sort.asc(i64));
    if (DEBUG) {
        std.debug.print("Output: [{d}]{d}\nSorted: {}\nFinal RC: {}\n", .{ len, arr_ptr[0..len], sorted, test_count });
    }
    std.debug.assert(sorted);
    std.debug.assert(test_count == 0);
}

const Opaque = ?[*]u8;
fn test_i64_compare_refcounted(count_ptr: Opaque, a_ptr: Opaque, b_ptr: Opaque) callconv(.C) u8 {
    const a = @as(*i64, @alignCast(@ptrCast(a_ptr))).*;
    const b = @as(*i64, @alignCast(@ptrCast(b_ptr))).*;

    const gt = @as(u8, @intFromBool(a > b));
    const lt = @as(u8, @intFromBool(a < b));

    std.debug.assert(@as(*isize, @ptrCast(@alignCast(count_ptr))).* > 0);
    @as(*isize, @ptrCast(@alignCast(count_ptr))).* -= 1;
    // Eq = 0
    // GT = 1
    // LT = 2
    return lt + lt + gt;
}

fn test_i64_copy(dst_ptr: Opaque, src_ptr: Opaque) callconv(.C) void {
    @as(*i64, @alignCast(@ptrCast(dst_ptr))).* = @as(*i64, @alignCast(@ptrCast(src_ptr))).*;
}

fn test_inc_n_data(count_ptr: Opaque, n: usize) callconv(.C) void {
    @as(*isize, @ptrCast(@alignCast(count_ptr))).* += @intCast(n);
}

comptime {
    @export(&testing_roc_alloc, .{ .name = "roc_alloc", .linkage = .Strong });
    @export(&testing_roc_dealloc, .{ .name = "roc_dealloc", .linkage = .Strong });
    @export(&testing_roc_panic, .{ .name = "roc_panic", .linkage = .Strong });
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
