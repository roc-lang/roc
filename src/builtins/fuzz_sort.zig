//! Fuzzing utility for sorting algorithms with memory leak detection and testing.
//!
//! This module provides a fuzz testing implementation for sorting functions,
//! featuring memory allocation tracking, sorting verification, and reference-counted
//! comparison mechanisms.

const std = @import("std");
const build_options = @import("build_options");
const Allocator = std.mem.Allocator;
const sort = @import("sort.zig");
const utils = @import("utils.zig");

fn cMain() callconv(.c) i32 {
    fuzz_main() catch unreachable;
    return 0;
}

comptime {
    @export(&cMain, .{ .name = "main", .linkage = .Strong });
}

const DEBUG = false;

/// TODO: Document fuzz_main.
pub fn fuzz_main() Allocator.Error!void {
    // Setup an allocator that will detect leaks/use-after-free/etc
    var gpa = std.heap.DebugAllocator(.{ .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }){};
    // this will check for leaks and crash the program if it finds any
    defer std.debug.assert(build_options.debugGpaOk(gpa.deinit()));
    const allocator = gpa.allocator();
    var test_env = utils.TestEnv.init(allocator);
    defer test_env.deinit();

    // Read the data from stdin.
    // Access Io types via @import("std") to avoid the banned std-dot-Io string
    // in core modules. This standalone fuzzer doesn't have the io module available.
    const stdin = @import("std").Io.File.stdin();
    const data = try stdin.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(data);

    const len = data.len / @sizeOf(i64);
    const arr_ptr: [*]i64 = @ptrCast(@alignCast(data.ptr));

    if (DEBUG) {
        std.debug.print("Input: [{d}]{d}\n", .{ len, arr_ptr[0..len] });
    }

    var test_count: i64 = 0;
    sort.fluxsort(
        @ptrCast(arr_ptr),
        len,
        &test_i64_compare_refcounted,
        @ptrCast(&test_count),
        true,
        null,
        &test_inc_n_data,
        @sizeOf(i64),
        @alignOf(i64),
        &test_i64_copy,
        test_env.getOps(),
    );

    const sorted = std.sort.isSorted(i64, arr_ptr[0..len], {}, std.sort.asc(i64));
    if (DEBUG) {
        std.debug.print("Output: [{d}]{d}\nSorted: {}\nFinal RC: {}\n", .{ len, arr_ptr[0..len], sorted, test_count });
    }
    std.debug.assert(sorted);
    std.debug.assert(test_count == 0);
}

const Opaque = ?[*]u8;
fn test_i64_compare_refcounted(count_ptr: Opaque, a_ptr: Opaque, b_ptr: Opaque) callconv(.c) u8 {
    const a = @as(*i64, @ptrCast(@alignCast(a_ptr))).*;
    const b = @as(*i64, @ptrCast(@alignCast(b_ptr))).*;

    std.debug.assert(@as(*isize, @ptrCast(@alignCast(count_ptr))).* > 0);
    @as(*isize, @ptrCast(@alignCast(count_ptr))).* -= 1;
    if (a < b) return @intFromEnum(utils.Ordering.Before);
    if (a > b) return @intFromEnum(utils.Ordering.After);
    return @intFromEnum(utils.Ordering.Same);
}

fn test_i64_copy(dst_ptr: Opaque, src_ptr: Opaque) callconv(.c) void {
    @as(*i64, @ptrCast(@alignCast(dst_ptr))).* = @as(*i64, @ptrCast(@alignCast(src_ptr))).*;
}

fn test_inc_n_data(_: ?*anyopaque, count_ptr: Opaque, n: usize) callconv(.c) void {
    @as(*isize, @ptrCast(@alignCast(count_ptr))).* += @intCast(n);
}
