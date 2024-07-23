const std = @import("std");
const testing = std.testing;

const utils = @import("utils.zig");
const roc_panic = @import("panic.zig").panic_help;

const Opaque = ?[*]u8;
const CompareFn = *const fn (Opaque, Opaque, Opaque) callconv(.C) u8;
const CopyFn = *const fn (Opaque, Opaque) callconv(.C) void;
const IncN = *const fn (?[*]u8, usize) callconv(.C) void;

/// Any size larger than the max element buffer will be sorted indirectly via pointers.
const MAX_ELEMENT_BUFFER_SIZE: usize = 64;

pub fn quadsort(
    source_ptr: [*]u8,
    len: usize,
    cmp: CompareFn,
    cmp_data: Opaque,
    data_is_owned: bool,
    inc_n_data: IncN,
    element_width: usize,
    copy: CopyFn,
) void {
    // Note, knowing constant versions of element_width and copy could have huge perf gains.
    // Hopefully llvm will essentially always do it via constant argument propagation and inlining.
    // If not, we may want to generate `n` different version of this function with comptime.
    // Then have our builtin dispatch to the correct version.
    // llvm garbage collection would remove all other variants.
    // Also, for numeric types, inlining the compare function can be a 2x perf gain.
    if (element_width <= MAX_ELEMENT_BUFFER_SIZE) {
        quadsort_direct(source_ptr, len, cmp, cmp_data, data_is_owned, inc_n_data, element_width, copy);
    } else {
        roc_panic("todo: fallback to an indirect pointer sort", 0);
    }
}

fn quadsort_direct(
    source_ptr: [*]u8,
    len: usize,
    cmp: CompareFn,
    cmp_data: Opaque,
    data_is_owned: bool,
    inc_n_data: IncN,
    element_width: usize,
    copy: CopyFn,
) void {
    _ = inc_n_data;
    _ = data_is_owned;
    _ = cmp_data;
    _ = len;
    _ = copy;
    _ = element_width;
    _ = cmp;
    _ = source_ptr;
    roc_panic("todo: quadsort", 0);
}

/// Merge two neighboring sorted 4 element arrays into swap.
inline fn parity_merge_four(ptr: [*]u8, swap: [*]u8, cmp_data: Opaque, cmp: CompareFn, element_width: usize, copy: CopyFn) void {
    var left = ptr;
    var right = ptr + (4 * element_width);
    var swap_ptr = swap;
    head_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    head_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    head_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const lte = @as(utils.Ordering, @enumFromInt(cmp(cmp_data, left, right))) != utils.Ordering.GT;
    var to_copy = if (lte) left else right;
    copy(swap_ptr, to_copy);

    left = ptr + (3 * element_width);
    right = ptr + (7 * element_width);
    swap_ptr = swap + (7 * element_width);
    tail_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    tail_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    tail_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const gt = @as(utils.Ordering, @enumFromInt(cmp(cmp_data, left, right))) == utils.Ordering.GT;
    to_copy = if (gt) left else right;
    copy(swap_ptr, to_copy);
}

/// Merge two neighboring sorted 2 element arrays into swap.
inline fn parity_merge_two(ptr: [*]u8, swap: [*]u8, cmp_data: Opaque, cmp: CompareFn, element_width: usize, copy: CopyFn) void {
    var left = ptr;
    var right = ptr + (2 * element_width);
    var swap_ptr = swap;
    head_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const lte = @as(utils.Ordering, @enumFromInt(cmp(cmp_data, left, right))) != utils.Ordering.GT;
    var to_copy = if (lte) left else right;
    copy(swap_ptr, to_copy);

    left = ptr + element_width;
    right = ptr + (3 * element_width);
    swap_ptr = swap + (3 * element_width);
    tail_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const gt = @as(utils.Ordering, @enumFromInt(cmp(cmp_data, left, right))) == utils.Ordering.GT;
    to_copy = if (gt) left else right;
    copy(swap_ptr, to_copy);
}

/// Moves the smaller element from left and rigth to dest.
/// Will increment both dest and the smaller element ptr to their next index.
/// Inlining will remove the extra level of pointer indirection here.
/// It is just used to allow mutating the input pointers.
inline fn head_branchless_merge(dest: *[*]u8, left: *[*]u8, right: *[*]u8, cmp_data: Opaque, cmp: CompareFn, element_width: usize, copy: CopyFn) void {
    // Note there is a much simpler version here:
    //    *ptd++ = cmp(ptl, ptr) <= 0 ? *ptl++ : *ptr++;
    // That said, not sure how to write that in zig and guarantee it is branchless.
    // Thus using the longer form.
    const lte = @as(utils.Ordering, @enumFromInt(cmp(cmp_data, left.*, right.*))) != utils.Ordering.GT;
    // TODO: double check this is branchless.
    const x = if (lte) element_width else 0;
    const not_x = if (lte) 0 else element_width;
    copy(dest.*, left.*);
    left.* += x;
    copy((dest.* + x), right.*);
    right.* += not_x;
    dest.* += element_width;
}

/// Moves the smaller element from left and rigth to dest.
/// Will decrement both dest and the smaller element ptr to their previous index.
/// Inlining will remove the extra level of pointer indirection here.
/// It is just used to allow mutating the input pointers.
inline fn tail_branchless_merge(dest: *[*]u8, left: *[*]u8, right: *[*]u8, cmp_data: Opaque, cmp: CompareFn, element_width: usize, copy: CopyFn) void {
    // Note there is a much simpler version here:
    //    *tpd-- = cmp(tpl, tpr) > 0 ? *tpl-- : *tpr--;
    // That said, not sure how to write that in zig and guarantee it is branchless.
    const lte = @as(utils.Ordering, @enumFromInt(cmp(cmp_data, left.*, right.*))) != utils.Ordering.GT;
    // TODO: double check this is branchless.
    const y = if (lte) element_width else 0;
    const not_y = if (lte) 0 else element_width;
    copy(dest.*, left.*);
    left.* -= not_y;
    dest.* -= element_width;
    copy((dest.* + y), right.*);
    right.* -= y;
}

/// Swaps the element at ptr with the element after it if the element is greater than the next.
inline fn swap_branchless(ptr: [*]u8, swap: [*]u8, cmp_data: Opaque, cmp: CompareFn, element_width: usize, copy: CopyFn) void {
    const gt = @as(utils.Ordering, @enumFromInt(cmp(cmp_data, ptr, ptr + element_width))) == utils.Ordering.GT;
    // TODO: double check this is branchless. I would expect llvm to optimize this to be branchless.
    // But based on reading some comments in quadsort, llvm seems to prefer branches very often.
    const x = if (gt) element_width else 0;
    const y = if (gt) 0 else element_width;

    copy(swap, ptr + y);
    copy(ptr, ptr + x);
    copy(ptr + element_width, swap);
}

test "parity_merge_four" {
    var arr = [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 };
    var swap = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };

    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    parity_merge_four(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(swap, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 5, 6, 7, 8, 1, 2, 3, 4 };
    swap = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };

    parity_merge_four(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(swap, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 1, 3, 5, 7, 2, 4, 6, 8 };
    swap = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };

    parity_merge_four(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(swap, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });
}

test "parity_merge_two" {
    var arr = [4]i64{ 1, 2, 3, 4 };
    var swap = [4]i64{ 0, 0, 0, 0 };

    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    parity_merge_two(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(swap, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 1, 3, 2, 4 };
    swap = [4]i64{ 0, 0, 0, 0 };

    parity_merge_two(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(swap, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 3, 4, 1, 2 };
    swap = [4]i64{ 0, 0, 0, 0 };

    parity_merge_two(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(swap, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 2, 4, 1, 3 };
    swap = [4]i64{ 0, 0, 0, 0 };

    parity_merge_two(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(swap, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 1, 4, 2, 3 };
    swap = [4]i64{ 0, 0, 0, 0 };

    parity_merge_two(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(swap, [4]i64{ 1, 2, 3, 4 });
}

test "head_merge" {
    var dest = [6]i64{ 0, 0, 0, 0, 0, 0 };
    var left = [4]i64{ 1, 7, 10, 22 };
    var right = [4]i64{ 2, 2, 8, 22 };
    var dest_ptr = @as([*]u8, @ptrCast(&dest[0]));
    var left_ptr = @as([*]u8, @ptrCast(&left[0]));
    var right_ptr = @as([*]u8, @ptrCast(&right[0]));

    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(dest, [6]i64{ 1, 2, 2, 7, 8, 10 });
}

test "tail_merge" {
    var dest = [6]i64{ 0, 0, 0, 0, 0, 0 };
    var left = [4]i64{ -22, 1, 7, 10 };
    var right = [4]i64{ -22, 2, 2, 8 };
    var dest_ptr = @as([*]u8, @ptrCast(&dest[dest.len - 1]));
    var left_ptr = @as([*]u8, @ptrCast(&left[left.len - 1]));
    var right_ptr = @as([*]u8, @ptrCast(&right[right.len - 1]));

    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(dest, [6]i64{ 1, 2, 2, 7, 8, 10 });
}

test "swap" {
    var arr = [2]i64{ 10, 20 };
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap: i64 = 0;
    var swap_ptr = @as([*]u8, @ptrCast(&swap));

    swap_branchless(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(arr[0], 10);
    try testing.expectEqual(arr[1], 20);

    arr[0] = 77;
    arr[1] = -12;

    swap_branchless(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(arr[0], -12);
    try testing.expectEqual(arr[1], 77);

    arr[0] = -22;
    arr[1] = -22;

    swap_branchless(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(arr[0], -22);
    try testing.expectEqual(arr[1], -22);
}

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
