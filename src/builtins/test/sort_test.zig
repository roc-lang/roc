const std = @import("std");
const builtins = @import("builtins");

const Opaque = builtins.sort.Opaque;
const partial_backwards_merge = builtins.sort.partial_backwards_merge;
const flux_default_partition = builtins.sort.flux_default_partition;
const flux_reverse_partition = builtins.sort.flux_reverse_partition;
const monobound_binary_first = builtins.sort.monobound_binary_first;
const head_branchless_merge = builtins.sort.head_branchless_merge;
const partial_forward_merge = builtins.sort.partial_forward_merge;
const tail_branchless_merge = builtins.sort.tail_branchless_merge;
const median_of_cube_root = builtins.sort.median_of_cube_root;
const parity_merge_four = builtins.sort.parity_merge_four;
const trinity_rotation = builtins.sort.trinity_rotation;
const quad_merge_block = builtins.sort.quad_merge_block;
const parity_merge_two = builtins.sort.parity_merge_two;
const swap_branchless = builtins.sort.swap_branchless;
const quad_swap_merge = builtins.sort.quad_swap_merge;
const median_of_nine = builtins.sort.median_of_nine;
const quad_reversal = builtins.sort.quad_reversal;
const binary_median = builtins.sort.binary_median;
const parity_merge = builtins.sort.parity_merge;
const rotate_merge = builtins.sort.rotate_merge;
const cross_merge = builtins.sort.cross_merge;
const quad_merge = builtins.sort.quad_merge;
const tail_merge = builtins.sort.tail_merge;
const trim_four = builtins.sort.trim_four;
const quad_swap = builtins.sort.quad_swap;
const tail_swap = builtins.sort.tail_swap;
const tiny_sort = builtins.sort.tiny_sort;

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

fn test_i64_compare_refcounted(count_ptr: Opaque, a_ptr: Opaque, b_ptr: Opaque) callconv(.C) u8 {
    const a = @as(*i64, @alignCast(@ptrCast(a_ptr))).*;
    const b = @as(*i64, @alignCast(@ptrCast(b_ptr))).*;

    const gt = @as(u8, @intFromBool(a > b));
    const lt = @as(u8, @intFromBool(a < b));

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

test "flux_default_partition" {
    var expected: [32]i64 = undefined;
    var test_count: i64 = 0;
    var pivot: i64 = 0;

    var arr: [32]i64 = undefined;
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap: [32]i64 = undefined;
    const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    arr = [32]i64{
        1, 3, 5, 7, 9,  11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31,
        2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32,
    };
    expected = [32]i64{
        // <= pivot first half
        1,  3,  5,  7,  9,  11, 13, 15,
        // <= pivot second half
        2,  4,  6,  8,  10, 12, 14, 16,
        // > pivot first half
        17, 19, 21, 23, 25, 27, 29, 31,
        // > pivot second half
        18, 20, 22, 24, 26, 28, 30, 32,
    };
    pivot = 16;
    var arr_len = flux_default_partition(arr_ptr, swap_ptr, arr_ptr, @ptrCast(&pivot), 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr_len, 16);
    try std.testing.expectEqualSlices(i64, arr[0..16], expected[0..16]);
    try std.testing.expectEqualSlices(i64, swap[0..16], expected[16..32]);

    arr = [32]i64{
        1, 3, 5, 7, 9,  11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31,
        2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32,
    };
    expected = [32]i64{
        // <= pivot first half
        1,  3,  5,  7,  9,  11, 13, 15, 17, 19, 21, 23,
        // <= pivot second half
        2,  4,  6,  8,  10, 12, 14, 16, 18, 20, 22, 24,
        // > pivot first half
        25, 27, 29, 31,
        // > pivot second half
        26, 28, 30, 32,
    };
    pivot = 24;
    arr_len = flux_default_partition(arr_ptr, swap_ptr, arr_ptr, @ptrCast(&pivot), 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr_len, 24);
    try std.testing.expectEqualSlices(i64, arr[0..24], expected[0..24]);
    try std.testing.expectEqualSlices(i64, swap[0..8], expected[24..32]);

    arr = [32]i64{
        1, 3, 5, 7, 9,  11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31,
        2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32,
    };
    expected = [32]i64{
        // <= pivot first half
        1, 3, 5, 7, 9,  11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31,
        // <= pivot second half
        2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32,
    };
    pivot = 32;
    arr_len = flux_default_partition(arr_ptr, swap_ptr, arr_ptr, @ptrCast(&pivot), 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr_len, 32);
    try std.testing.expectEqualSlices(i64, arr[0..32], expected[0..32]);

    arr = [32]i64{
        1,  3,  5,  7,  9,  11, 13, 15,
        2,  4,  6,  8,  10, 12, 14, 16,
        18, 20, 22, 24, 26, 28, 30, 32,
        17, 19, 21, 23, 25, 27, 29, 31,
    };
    for (0..31) |i| {
        expected[i] = @intCast(i + 1);
    }
    pivot = 16;
    arr_len = flux_default_partition(arr_ptr, swap_ptr, arr_ptr, @ptrCast(&pivot), 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr_len, 0);
    try std.testing.expectEqualSlices(i64, arr[0..32], expected[0..32]);
}

test "flux_reverse_partition" {
    const expected = [32]i64{
        1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15, 16,
        17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
    };
    var test_count: i64 = 0;
    var pivot: i64 = 0;

    var arr: [32]i64 = undefined;
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap: [32]i64 = undefined;
    const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    arr = [32]i64{
        1, 3, 5, 7, 9,  11, 13, 15, 17, 17, 17, 17, 17, 17, 17, 17,
        2, 4, 6, 8, 10, 12, 14, 16, 17, 17, 17, 17, 17, 17, 17, 17,
    };
    pivot = 17;
    flux_reverse_partition(arr_ptr, swap_ptr, arr_ptr, @ptrCast(&pivot), 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    arr = [32]i64{
        1,  17, 3,  17, 5,  17, 7,  17, 9,  17, 11, 17, 13, 17, 15, 17,
        17, 2,  17, 4,  17, 6,  17, 8,  17, 10, 17, 12, 17, 14, 17, 16,
    };
    pivot = 17;
    flux_reverse_partition(arr_ptr, swap_ptr, arr_ptr, @ptrCast(&pivot), 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    arr = [32]i64{
        15, 17, 13, 17, 11, 17, 9,  17, 7,  17, 5,  17, 3,  17, 1,  17,
        17, 16, 17, 14, 17, 12, 17, 10, 17, 8,  17, 6,  17, 4,  17, 2,
    };
    pivot = 17;
    flux_reverse_partition(arr_ptr, swap_ptr, arr_ptr, @ptrCast(&pivot), 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);
}

test "median_of_cube_root" {
    var test_count: i64 = 0;
    var out: i64 = 0;
    var generic = false;

    var swap: [32]i64 = undefined;
    const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));
    {
        var arr: [32]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [32]i64{
            1, 3, 5, 7, 9,  11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31,
            2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32,
        };
        median_of_cube_root(arr_ptr, swap_ptr, arr_ptr, 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, @ptrCast(&generic), @ptrCast(&out), false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(out, 17);
        try std.testing.expectEqual(generic, false);

        for (0..32) |i| {
            arr[i] = 7;
        }
        median_of_cube_root(arr_ptr, swap_ptr, arr_ptr, 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, @ptrCast(&generic), @ptrCast(&out), false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(out, 7);
        try std.testing.expectEqual(generic, true);

        for (0..32) |i| {
            arr[i] = 7 + @as(i64, @intCast(i % 2));
        }
        median_of_cube_root(arr_ptr, swap_ptr, arr_ptr, 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, @ptrCast(&generic), @ptrCast(&out), false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(out, 8);
        try std.testing.expectEqual(generic, false);
    }
}

test "median_of_nine" {
    var test_count: i64 = 0;
    var out: i64 = 0;

    {
        var arr: [9]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [9]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9 };
        median_of_nine(arr_ptr, 10, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, @ptrCast(&out), false);
        try std.testing.expectEqual(test_count, 0);
        // Note: median is not guaranteed to be exact. in this case:
        // [2, 3], [6, 7] -> [3, 6] -> [3, 6, 9] -> 6
        try std.testing.expectEqual(out, 6);

        arr = [9]i64{ 1, 3, 5, 7, 9, 2, 4, 6, 8 };
        median_of_nine(arr_ptr, 10, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, @ptrCast(&out), false);
        try std.testing.expectEqual(test_count, 0);
        // Note: median is not guaranteed to be exact. in this case:
        // [3, 5], [4, 6] -> [4, 5] -> [4, 5, 8] -> 5
        try std.testing.expectEqual(out, 5);

        arr = [9]i64{ 2, 3, 9, 4, 5, 7, 8, 6, 1 };
        median_of_nine(arr_ptr, 10, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, @ptrCast(&out), false);
        try std.testing.expectEqual(test_count, 0);
        // Note: median is not guaranteed to be exact. in this case:
        // [3, 4], [5, 6] -> [4, 5] -> [1, 4, 5] -> 4
        try std.testing.expectEqual(out, 4);
    }
}

test "trim_four" {
    var test_count: i64 = 0;

    var arr: [4]i64 = undefined;
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

    arr = [4]i64{ 1, 2, 3, 4 };
    trim_four(arr_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 2, 3, 1, 4 };
    trim_four(arr_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, [4]i64{ 2, 3, 2, 4 });

    arr = [4]i64{ 4, 3, 2, 1 };
    trim_four(arr_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, [4]i64{ 3, 2, 3, 2 });
}

test "binary_median" {
    var test_count: i64 = 0;
    var out: i64 = 0;

    {
        var arr: [10]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        binary_median(arr_ptr, arr_ptr + 5 * @sizeOf(i64), 5, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, @ptrCast(&out), false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(out, 6);

        arr = [10]i64{ 1, 3, 5, 7, 9, 2, 4, 6, 8, 10 };
        binary_median(arr_ptr, arr_ptr + 5 * @sizeOf(i64), 5, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, @ptrCast(&out), false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(out, 5);
    }
    {
        var arr: [16]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [16]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
        binary_median(arr_ptr, arr_ptr + 8 * @sizeOf(i64), 8, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, @ptrCast(&out), false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(out, 9);

        arr = [16]i64{ 1, 3, 5, 7, 9, 11, 13, 15, 2, 4, 6, 8, 10, 12, 14, 16 };
        binary_median(arr_ptr, arr_ptr + 8 * @sizeOf(i64), 8, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, @ptrCast(&out), false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(out, 9);

        arr = [16]i64{ 9, 10, 11, 12, 13, 14, 15, 16, 1, 2, 3, 4, 5, 6, 7, 8 };
        binary_median(arr_ptr, arr_ptr + 8 * @sizeOf(i64), 8, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, @ptrCast(&out), false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(out, 9);
    }
}

test "rotate_merge" {
    const expected = [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    var test_count: i64 = 0;

    var arr: [10]i64 = undefined;
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap: [10]i64 = undefined;
    const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 2, 9, 10 };
    rotate_merge(arr_ptr, 10, swap_ptr, 10, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 9, 2, 10 };
    rotate_merge(arr_ptr, 9, swap_ptr, 9, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    arr = [10]i64{ 3, 4, 6, 9, 1, 2, 5, 10, 7, 8 };
    rotate_merge(arr_ptr, 10, swap_ptr, 10, 4, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    // Limited swap, can't finish merge
    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 9, 2, 10 };
    rotate_merge(arr_ptr, 10, swap_ptr, 4, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);
}

test "monobound_binary_first" {
    var test_count: i64 = 0;

    var arr = [25]i64{ 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49 };
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var value: i64 = undefined;
    const value_ptr = @as([*]u8, @ptrCast(&value));

    value = 7;
    var res = monobound_binary_first(arr_ptr, 25, value_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(res, 3);

    value = 39;
    res = monobound_binary_first(arr_ptr, 25, value_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(res, 19);

    value = 40;
    res = monobound_binary_first(arr_ptr, 25, value_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(res, 20);

    value = -10;
    res = monobound_binary_first(arr_ptr, 25, value_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(res, 0);

    value = 10000;
    res = monobound_binary_first(arr_ptr, 25, value_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(res, 25);
}

test "trinity_rotation" {
    {
        var arr: [10]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        var swap: [10]i64 = undefined;
        const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

        // Even.
        arr = [10]i64{ 6, 7, 8, 9, 10, 1, 2, 3, 4, 5 };
        trinity_rotation(arr_ptr, 10, swap_ptr, 10, 5, @sizeOf(i64), &test_i64_copy);
        try std.testing.expectEqual(arr, [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });

        // left large, right fits in swap.
        arr = [10]i64{ 3, 4, 5, 6, 7, 8, 9, 10, 1, 2 };
        trinity_rotation(arr_ptr, 10, swap_ptr, 10, 8, @sizeOf(i64), &test_i64_copy);
        try std.testing.expectEqual(arr, [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });

        // right large, left fits in swap.
        arr = [10]i64{ 9, 10, 1, 2, 3, 4, 5, 6, 7, 8 };
        trinity_rotation(arr_ptr, 10, swap_ptr, 10, 2, @sizeOf(i64), &test_i64_copy);
        try std.testing.expectEqual(arr, [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });

        // left large, no swap.
        arr = [10]i64{ 3, 4, 5, 6, 7, 8, 9, 10, 1, 2 };
        trinity_rotation(arr_ptr, 10, swap_ptr, 0, 8, @sizeOf(i64), &test_i64_copy);
        try std.testing.expectEqual(arr, [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });

        // right large, no swap.
        arr = [10]i64{ 9, 10, 1, 2, 3, 4, 5, 6, 7, 8 };
        trinity_rotation(arr_ptr, 10, swap_ptr, 0, 2, @sizeOf(i64), &test_i64_copy);
        try std.testing.expectEqual(arr, [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });
    }
    {
        var arr: [16]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        var swap: [5]i64 = undefined;
        const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

        // left larger, bridge in swap.
        arr = [16]i64{ 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1, 2, 3, 4, 5, 6 };
        trinity_rotation(arr_ptr, 16, swap_ptr, 5, 10, @sizeOf(i64), &test_i64_copy);
        try std.testing.expectEqual(arr, [16]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 });

        // // right large, bridge in swap.
        arr = [16]i64{ 11, 12, 13, 14, 15, 16, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        trinity_rotation(arr_ptr, 16, swap_ptr, 5, 6, @sizeOf(i64), &test_i64_copy);
        try std.testing.expectEqual(arr, [16]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 });
    }
}

test "tail_merge" {
    var test_count: i64 = 0;
    const expected = [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

    var arr: [10]i64 = undefined;
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap: [10]i64 = undefined;
    const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 2, 9, 10 };
    tail_merge(arr_ptr, 10, swap_ptr, 10, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 2, 9, 10 };
    tail_merge(arr_ptr, 9, swap_ptr, 9, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    arr = [10]i64{ 3, 4, 6, 9, 1, 2, 5, 10, 7, 8 };
    tail_merge(arr_ptr, 10, swap_ptr, 10, 4, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);
}

test "partial_backwards_merge" {
    var test_count: i64 = 0;
    {
        const expected = [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

        var arr: [10]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        var swap: [10]i64 = undefined;
        const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

        arr = [10]i64{ 3, 4, 5, 6, 7, 8, 1, 2, 9, 10 };
        partial_backwards_merge(arr_ptr, 10, swap_ptr, 10, 6, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, expected);

        arr = [10]i64{ 2, 4, 6, 8, 9, 10, 1, 3, 5, 7 };
        partial_backwards_merge(arr_ptr, 10, swap_ptr, 10, 6, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, expected);

        arr = [10]i64{ 1, 2, 3, 4, 5, 6, 8, 9, 10, 7 };
        partial_backwards_merge(arr_ptr, 10, swap_ptr, 10, 9, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, expected);

        arr = [10]i64{ 1, 2, 4, 5, 6, 8, 9, 3, 7, 10 };
        partial_backwards_merge(arr_ptr, 10, swap_ptr, 9, 7, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, expected);
    }

    {
        var expected: [64]i64 = undefined;
        for (0..64) |i| {
            expected[i] = @intCast(i + 1);
        }

        var arr: [64]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        var swap: [64]i64 = undefined;
        const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

        // chunks
        for (0..16) |i| {
            arr[i] = @intCast(i + 17);
        }
        for (0..16) |i| {
            arr[i + 16] = @intCast(i + 49);
        }
        for (0..16) |i| {
            arr[i + 32] = @intCast(i + 1);
        }
        for (0..16) |i| {
            arr[i + 48] = @intCast(i + 33);
        }
        partial_backwards_merge(arr_ptr, 64, swap_ptr, 64, 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, expected);

        // chunks with break
        for (0..16) |i| {
            arr[i] = @intCast(i + 17);
        }
        for (0..16) |i| {
            arr[i + 32] = @intCast(i + 1);
        }
        for (0..16) |i| {
            arr[i + 16] = @intCast(i + 49);
        }
        for (0..16) |i| {
            arr[i + 48] = @intCast(i + 34);
        }
        arr[16] = 33;
        arr[63] = 49;

        partial_backwards_merge(arr_ptr, 64, swap_ptr, 64, 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, expected);
    }
}

test "partial_forward_merge" {
    var test_count: i64 = 0;
    const expected = [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

    var arr: [10]i64 = undefined;
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap: [10]i64 = undefined;
    const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    arr = [10]i64{ 3, 4, 5, 6, 7, 8, 1, 2, 9, 10 };
    partial_forward_merge(arr_ptr, 10, swap_ptr, 10, 6, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    arr = [10]i64{ 2, 4, 6, 8, 9, 10, 1, 3, 5, 7 };
    partial_forward_merge(arr_ptr, 10, swap_ptr, 10, 6, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    arr = [10]i64{ 1, 2, 3, 4, 5, 6, 8, 9, 10, 7 };
    partial_forward_merge(arr_ptr, 10, swap_ptr, 10, 9, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    arr = [10]i64{ 1, 2, 4, 5, 6, 8, 9, 3, 7, 10 };
    partial_forward_merge(arr_ptr, 10, swap_ptr, 9, 7, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);
}

test "quad_merge" {
    var test_count: i64 = 0;
    const expected = [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

    var arr: [10]i64 = undefined;
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap: [10]i64 = undefined;
    const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));
    var size: usize = undefined;

    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 2, 9, 10 };
    size = quad_merge(arr_ptr, 10, swap_ptr, 10, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);
    try std.testing.expectEqual(size, 16);

    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 9, 2, 10 };
    size = quad_merge(arr_ptr, 9, swap_ptr, 9, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);
    try std.testing.expectEqual(size, 16);

    arr = [10]i64{ 3, 4, 6, 9, 1, 2, 5, 10, 7, 8 };
    size = quad_merge(arr_ptr, 10, swap_ptr, 10, 4, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);
    try std.testing.expectEqual(size, 8);

    // Limited swap, can't finish merge
    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 9, 2, 10 };
    size = quad_merge(arr_ptr, 10, swap_ptr, 4, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, [10]i64{ 1, 3, 4, 5, 6, 7, 8, 9, 2, 10 });
    try std.testing.expectEqual(size, 4);

    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 9, 2, 10 };
    size = quad_merge(arr_ptr, 10, swap_ptr, 3, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, [10]i64{ 5, 6, 7, 8, 1, 3, 4, 9, 2, 10 });
    try std.testing.expectEqual(size, 4);
}

test "quad_merge_block" {
    var test_count: i64 = 0;
    const expected = [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 };

    var arr: [8]i64 = undefined;
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap: [8]i64 = undefined;
    const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    // case 0 - totally unsorted
    arr = [8]i64{ 7, 8, 5, 6, 3, 4, 1, 2 };
    quad_merge_block(arr_ptr, swap_ptr, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    // case 1 - first half sorted
    arr = [8]i64{ 5, 6, 7, 8, 3, 4, 1, 2 };
    quad_merge_block(arr_ptr, swap_ptr, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    // case 2 - second half sorted
    arr = [8]i64{ 7, 8, 5, 6, 1, 2, 3, 4 };
    quad_merge_block(arr_ptr, swap_ptr, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    // case 3 both haves sorted
    arr = [8]i64{ 1, 3, 5, 7, 2, 4, 6, 8 };
    quad_merge_block(arr_ptr, swap_ptr, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);

    // case 3 - lucky, sorted
    arr = [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 };
    quad_merge_block(arr_ptr, swap_ptr, 2, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    // try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(arr, expected);
}

test "cross_merge" {
    var test_count: i64 = 0;
    var expected: [64]i64 = undefined;
    for (0..64) |i| {
        expected[i] = @intCast(i + 1);
    }

    var src: [64]i64 = undefined;
    var dest: [64]i64 = undefined;
    const src_ptr = @as([*]u8, @ptrCast(&src[0]));
    const dest_ptr = @as([*]u8, @ptrCast(&dest[0]));

    // Opitimal case, ordered but swapped
    for (0..32) |i| {
        src[i] = @intCast(i + 33);
    }
    for (0..32) |i| {
        src[i + 32] = @intCast(i + 1);
    }
    cross_merge(dest_ptr, src_ptr, 32, 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(dest, expected);

    // will fallback, every other
    for (0..32) |i| {
        src[i * 2] = @intCast(i * 2 + 1);
        src[i * 2 + 1] = @intCast(i * 2 + 2);
    }
    cross_merge(dest_ptr, src_ptr, 32, 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(dest, expected);

    // super uneven
    for (0..20) |i| {
        src[i] = @intCast(i + 45);
    }
    for (0..44) |i| {
        src[i + 20] = @intCast(i + 1);
    }
    cross_merge(dest_ptr, src_ptr, 20, 44, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(dest, expected);

    // chunks
    for (0..16) |i| {
        src[i] = @intCast(i + 17);
    }
    for (0..16) |i| {
        src[i + 16] = @intCast(i + 49);
    }
    for (0..16) |i| {
        src[i + 32] = @intCast(i + 1);
    }
    for (0..16) |i| {
        src[i + 48] = @intCast(i + 33);
    }
    cross_merge(dest_ptr, src_ptr, 32, 32, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(dest, expected);
}

test "quad_swap" {
    var test_count: i64 = 0;
    var arr: [75]i64 = undefined;
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

    arr = [75]i64{
        // multiple ordered chunks
        1,  3,  5,  7,  9,  11, 13, 15,
        33, 34, 35, 36, 37, 38, 39, 40,
        // partially ordered
        41, 42, 45, 46, 43, 44, 47, 48,
        // multiple reverse chunks
        70, 69, 68, 67, 66, 65, 64, 63,
        16, 14, 12, 10, 8,  6,  4,  2,
        // another ordered
        49, 50, 51, 52, 53, 54, 55, 56,
        // unordered
        23, 21, 19, 20, 24, 22, 18, 17,
        // partially reversed
        32, 31, 28, 27, 30, 29, 26, 25,
        // awkward tail
        62, 59, 61, 60, 71, 73, 75, 74,
        72, 58, 57,
    };

    var result = quad_swap(arr_ptr, 75, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(result, .unfinished);
    try std.testing.expectEqual(arr, [75]i64{
        // first 32 elements sorted (with 8 reversed that get flipped here)
        1,  2,  3,  4,  5,  6,  7,  8,
        9,  10, 11, 12, 13, 14, 15, 16,
        33, 34, 35, 36, 37, 38, 39, 40,
        41, 42, 43, 44, 45, 46, 47, 48,
        // second 32 elements sorted (with 8 reversed that get flipped here)
        17, 18, 19, 20, 21, 22, 23, 24,
        25, 26, 27, 28, 29, 30, 31, 32,
        49, 50, 51, 52, 53, 54, 55, 56,
        63, 64, 65, 66, 67, 68, 69, 70,
        // awkward tail
        57, 58, 59, 60, 61, 62, 71, 72,
        73, 74, 75,
    });

    // Just reversed.
    var expected: [75]i64 = undefined;
    for (0..75) |i| {
        expected[i] = @intCast(i + 1);
        arr[i] = @intCast(75 - i);
    }
    result = quad_swap(arr_ptr, 75, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
    try std.testing.expectEqual(test_count, 0);
    try std.testing.expectEqual(result, .sorted);
    try std.testing.expectEqual(arr, expected);
}

test "quad_swap_merge" {
    var arr: [8]i64 = undefined;
    var swap: [8]i64 = undefined;
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    arr = [8]i64{ 5, 6, 7, 8, 1, 2, 3, 4 };
    swap = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    quad_swap_merge(arr_ptr, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(arr, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 5, 7, 1, 3, 6, 8, 2, 4 };
    swap = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    quad_swap_merge(arr_ptr, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(arr, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 1, 8, 3, 4, 5, 6, 2, 7 };
    swap = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    quad_swap_merge(arr_ptr, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(arr, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });
}

test "quad_reversal" {
    {
        var arr = [8]i64{ 8, 7, 6, 5, 4, 3, 2, 1 };
        const start_ptr = @as([*]u8, @ptrCast(&arr[0]));
        const end_ptr = @as([*]u8, @ptrCast(&arr[7]));
        quad_reversal(start_ptr, end_ptr, @sizeOf(i64), &test_i64_copy);
        try std.testing.expectEqual(arr, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });
    }
    {
        var arr = [9]i64{ 9, 8, 7, 6, 5, 4, 3, 2, 1 };
        const start_ptr = @as([*]u8, @ptrCast(&arr[0]));
        const end_ptr = @as([*]u8, @ptrCast(&arr[8]));
        quad_reversal(start_ptr, end_ptr, @sizeOf(i64), &test_i64_copy);
        try std.testing.expectEqual(arr, [9]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9 });
    }
}

test "tail_swap" {
    var test_count: i64 = 0;
    var swap: [31]i64 = undefined;
    const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    var arr: [31]i64 = undefined;
    var expected: [31]i64 = undefined;
    for (0..31) |i| {
        arr[i] = @intCast(i + 1);
        expected[i] = @intCast(i + 1);
    }
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

    for (0..10) |seed| {
        var rng = std.Random.DefaultPrng.init(seed);
        rng.random().shuffle(i64, arr[0..]);

        tail_swap(arr_ptr, 31, swap_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, expected);
    }
}

test "parity_merge" {
    var test_count: i64 = 0;
    {
        var dest: [8]i64 = undefined;
        const dest_ptr = @as([*]u8, @ptrCast(&dest[0]));

        var arr: [8]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [8]i64{ 1, 3, 5, 7, 2, 4, 6, 8 };
        dest = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
        parity_merge(dest_ptr, arr_ptr, 4, 4, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

        arr = [8]i64{ 5, 6, 7, 8, 1, 2, 3, 4 };
        dest = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
        parity_merge(dest_ptr, arr_ptr, 4, 4, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });
    }
    {
        var dest: [9]i64 = undefined;
        const dest_ptr = @as([*]u8, @ptrCast(&dest[0]));

        var arr: [9]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [9]i64{ 1, 3, 5, 8, 2, 4, 6, 7, 9 };
        dest = [9]i64{ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
        parity_merge(dest_ptr, arr_ptr, 4, 5, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(dest, [9]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9 });

        arr = [9]i64{ 6, 7, 8, 9, 1, 2, 3, 4, 5 };
        dest = [9]i64{ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
        parity_merge(dest_ptr, arr_ptr, 4, 5, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(dest, [9]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9 });

        arr = [9]i64{ 1, 3, 5, 7, 8, 2, 4, 6, 9 };
        dest = [9]i64{ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
        parity_merge(dest_ptr, arr_ptr, 5, 4, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(dest, [9]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9 });

        arr = [9]i64{ 5, 6, 7, 8, 9, 1, 2, 3, 4 };
        dest = [9]i64{ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
        parity_merge(dest_ptr, arr_ptr, 5, 4, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(dest, [9]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9 });
    }
}

test "tiny_sort" {
    var test_count: i64 = 0;
    var swap: [7]i64 = undefined;
    const swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    {
        var arr: [7]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [7]i64{ 3, 1, 2, 5, 4, 7, 6 };
        tiny_sort(arr_ptr, 7, swap_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, [7]i64{ 1, 2, 3, 4, 5, 6, 7 });

        arr = [7]i64{ 7, 6, 5, 4, 3, 2, 1 };
        tiny_sort(arr_ptr, 7, swap_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, [7]i64{ 1, 2, 3, 4, 5, 6, 7 });
    }
    {
        var arr: [6]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [6]i64{ 3, 1, 2, 6, 4, 5 };
        tiny_sort(arr_ptr, 6, swap_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, [6]i64{ 1, 2, 3, 4, 5, 6 });

        arr = [6]i64{ 6, 5, 4, 3, 2, 1 };
        tiny_sort(arr_ptr, 6, swap_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, [6]i64{ 1, 2, 3, 4, 5, 6 });
    }
    {
        var arr: [5]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [5]i64{ 2, 1, 4, 3, 5 };
        tiny_sort(arr_ptr, 5, swap_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, [5]i64{ 1, 2, 3, 4, 5 });

        arr = [5]i64{ 5, 4, 3, 2, 1 };
        tiny_sort(arr_ptr, 5, swap_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, [5]i64{ 1, 2, 3, 4, 5 });
    }
    {
        var arr: [4]i64 = undefined;
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [4]i64{ 4, 2, 1, 3 };
        tiny_sort(arr_ptr, 4, swap_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, [4]i64{ 1, 2, 3, 4 });

        arr = [4]i64{ 2, 1, 4, 3 };
        tiny_sort(arr_ptr, 4, swap_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, [4]i64{ 1, 2, 3, 4 });
    }
    {
        var arr = [3]i64{ 2, 3, 1 };
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        tiny_sort(arr_ptr, 3, swap_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, [3]i64{ 1, 2, 3 });
    }
    {
        var arr = [2]i64{ 2, 1 };
        const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        tiny_sort(arr_ptr, 2, swap_ptr, &test_i64_compare_refcounted, @ptrCast(&test_count), @sizeOf(i64), &test_i64_copy, true, &test_inc_n_data, false);
        try std.testing.expectEqual(test_count, 0);
        try std.testing.expectEqual(arr, [2]i64{ 1, 2 });
    }
}

test "parity_merge_four" {
    var arr: [8]i64 = undefined;
    var dest: [8]i64 = undefined;
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    const dest_ptr = @as([*]u8, @ptrCast(&dest[0]));

    arr = [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 };
    dest = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    parity_merge_four(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 5, 6, 7, 8, 1, 2, 3, 4 };
    dest = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    parity_merge_four(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 1, 3, 5, 7, 2, 4, 6, 8 };
    dest = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    parity_merge_four(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });
}

test "parity_merge_two" {
    var arr: [4]i64 = undefined;
    var dest: [4]i64 = undefined;
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    const dest_ptr = @as([*]u8, @ptrCast(&dest[0]));

    arr = [4]i64{ 1, 2, 3, 4 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 1, 3, 2, 4 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 3, 4, 1, 2 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 2, 4, 1, 3 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 1, 4, 2, 3 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });
}

test "head_branchless_merge" {
    var dest = [6]i64{ 0, 0, 0, 0, 0, 0 };
    var left = [4]i64{ 1, 7, 10, 22 };
    var right = [4]i64{ 2, 2, 8, 22 };
    var dest_ptr = @as([*]u8, @ptrCast(&dest[0]));
    var left_ptr = @as([*]u8, @ptrCast(&left[0]));
    var right_ptr = @as([*]u8, @ptrCast(&right[0]));

    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);

    try std.testing.expectEqual(dest, [6]i64{ 1, 2, 2, 7, 8, 10 });
}

test "tail_branchless_merge" {
    var dest = [6]i64{ 0, 0, 0, 0, 0, 0 };
    var left = [4]i64{ -22, 1, 7, 10 };
    var right = [4]i64{ -22, 2, 2, 8 };
    var dest_ptr = @as([*]u8, @ptrCast(&dest[dest.len - 1]));
    var left_ptr = @as([*]u8, @ptrCast(&left[left.len - 1]));
    var right_ptr = @as([*]u8, @ptrCast(&right[right.len - 1]));

    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);

    try std.testing.expectEqual(dest, [6]i64{ 1, 2, 2, 7, 8, 10 });
}

test "swap" {
    var arr: [2]i64 = undefined;
    var tmp: i64 = undefined;
    const arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    const tmp_ptr = @as([*]u8, @ptrCast(&tmp));

    arr = [2]i64{ 10, 20 };
    swap_branchless(arr_ptr, tmp_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(arr, [2]i64{ 10, 20 });

    arr = [2]i64{ 77, -12 };
    swap_branchless(arr_ptr, tmp_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(arr, [2]i64{ -12, 77 });

    arr = [2]i64{ -22, -22 };
    swap_branchless(arr_ptr, tmp_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy, false);
    try std.testing.expectEqual(arr, [2]i64{ -22, -22 });
}
