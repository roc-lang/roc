const std = @import("std");
const testing = std.testing;

const utils = @import("utils.zig");
const roc_panic = @import("panic.zig").panic_help;

const Ordering = utils.Ordering;
const GT = Ordering.GT;
const LT = Ordering.LT;
const EQ = Ordering.EQ;
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

// ================ Small Arrays ==============================================
// Below are functions for sorting 0 to 31 element arrays.

// Inserts elements from offset to len into the sorted begining chunk of the array before offest.
// offset must be at least 2.
fn twice_unguarded_insert(array: [*]u8, offset: usize, len: usize, cmp_data: Opaque, cmp: CompareFn, element_width: usize, copy: CopyFn) void {
    var buffer: [MAX_ELEMENT_BUFFER_SIZE]u8 = undefined;
    const key_ptr = @as([*]u8, @ptrCast(&buffer[0]));

    for (offset..len) |i| {
        var end_ptr = array + i * element_width;
        var arr_ptr = end_ptr - element_width;

        const lte = compare(cmp, cmp_data, arr_ptr, end_ptr) != GT;
        if (lte) {
            continue;
        }

        copy(key_ptr, end_ptr);
        var gt = compare(cmp, cmp_data, array + element_width, key_ptr) == GT;
        if (gt) {
            var top = i - 1;
            while (true) {
                copy(end_ptr, arr_ptr);
                end_ptr -= element_width;
                arr_ptr -= element_width;

                top -= 1;
                if (top == 0) {
                    break;
                }
            }
            copy(end_ptr, key_ptr);
            end_ptr -= element_width;
        } else {
            while (true) {
                inline for (0..1) |_| {
                    copy(end_ptr, arr_ptr);
                    end_ptr -= element_width;
                    arr_ptr -= element_width;
                }
                gt = compare(cmp, cmp_data, arr_ptr, key_ptr) == GT;
                if (!gt) {
                    break;
                }
            }
            copy(end_ptr, end_ptr + element_width);
            copy(end_ptr + element_width, key_ptr);
        }
        swap_branchless(end_ptr, key_ptr, cmp_data, cmp, element_width, copy);
    }
}

/// Sort arrays of 0 to 4 elements.
fn tiny_sort(array: [*]u8, len: usize, cmp_data: Opaque, cmp: CompareFn, element_width: usize, copy: CopyFn) void {
    var buffer: [MAX_ELEMENT_BUFFER_SIZE]u8 = undefined;
    const swap_ptr = @as([*]u8, @ptrCast(&buffer[0]));

    switch (len) {
        4 => {
            var arr_ptr = array;
            swap_branchless(arr_ptr, swap_ptr, cmp_data, cmp, element_width, copy);
            arr_ptr += 2 * element_width;
            swap_branchless(arr_ptr, swap_ptr, cmp_data, cmp, element_width, copy);
            arr_ptr -= element_width;

            const gt = @as(Ordering, @enumFromInt(cmp(cmp_data, arr_ptr, arr_ptr + element_width))) == GT;
            if (gt) {
                copy(swap_ptr, arr_ptr);
                copy(arr_ptr, arr_ptr + element_width);
                copy(arr_ptr + element_width, swap_ptr);
                arr_ptr -= element_width;

                swap_branchless(arr_ptr, swap_ptr, cmp_data, cmp, element_width, copy);
                arr_ptr += 2 * element_width;
                swap_branchless(arr_ptr, swap_ptr, cmp_data, cmp, element_width, copy);
                arr_ptr -= element_width;
                swap_branchless(arr_ptr, swap_ptr, cmp_data, cmp, element_width, copy);
            }
        },
        3 => {
            var arr_ptr = array;
            swap_branchless(arr_ptr, swap_ptr, cmp_data, cmp, element_width, copy);
            arr_ptr += element_width;
            swap_branchless(arr_ptr, swap_ptr, cmp_data, cmp, element_width, copy);
            arr_ptr = array;
            swap_branchless(arr_ptr, swap_ptr, cmp_data, cmp, element_width, copy);
        },
        2 => {
            swap_branchless(array, swap_ptr, cmp_data, cmp, element_width, copy);
        },
        1, 0 => {
            return;
        },
        else => {
            unreachable;
        },
    }
}

test "twice_unguarded_insert" {
    {
        var arr = [7]i64{ 2, 3, 5, 6, 4, 1, 7 };
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        twice_unguarded_insert(arr_ptr, 4, 7, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [7]i64{ 1, 2, 3, 4, 5, 6, 7 });
    }
    {
        var arr = [11]i64{ 1, 2, 5, 6, 7, 8, 9, 11, 10, 4, 3 };
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        twice_unguarded_insert(arr_ptr, 8, 11, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [11]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 });
    }
    {
        var arr = [23]i64{ 5, 6, 7, 8, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 10, 4, 3, 2, 16, 1, 9 };
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        twice_unguarded_insert(arr_ptr, 16, 23, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [23]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23 });
    }
}
test "tiny_sort" {
    var arr: [4]i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

    arr = [4]i64{ 4, 2, 1, 3 };
    tiny_sort(arr_ptr, 4, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 2, 1, 4, 3 };
    tiny_sort(arr_ptr, 4, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 2, 3, 1, -1 };
    tiny_sort(arr_ptr, 3, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [4]i64{ 1, 2, 3, -1 });

    arr = [4]i64{ 2, 1, -1, -1 };
    tiny_sort(arr_ptr, 2, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [4]i64{ 1, 2, -1, -1 });
}

// ================ Primitives ================================================
// Below are sorting primitives that attempt to be branchless.
// They all also are always inline for performance.
// The are the smallest fundamental unit.

/// Merge two neighboring sorted 4 element arrays into swap.
inline fn parity_merge_four(ptr: [*]u8, swap: [*]u8, cmp_data: Opaque, cmp: CompareFn, element_width: usize, copy: CopyFn) void {
    var left = ptr;
    var right = ptr + (4 * element_width);
    var swap_ptr = swap;
    head_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    head_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    head_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const lte = compare(cmp, cmp_data, left, right) != GT;
    var to_copy = if (lte) left else right;
    copy(swap_ptr, to_copy);

    left = ptr + (3 * element_width);
    right = ptr + (7 * element_width);
    swap_ptr = swap + (7 * element_width);
    tail_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    tail_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    tail_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const gt = compare(cmp, cmp_data, left, right) == GT;
    to_copy = if (gt) left else right;
    copy(swap_ptr, to_copy);
}

/// Merge two neighboring sorted 2 element arrays into swap.
inline fn parity_merge_two(ptr: [*]u8, swap: [*]u8, cmp_data: Opaque, cmp: CompareFn, element_width: usize, copy: CopyFn) void {
    var left = ptr;
    var right = ptr + (2 * element_width);
    var swap_ptr = swap;
    head_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const lte = compare(cmp, cmp_data, left, right) != GT;
    var to_copy = if (lte) left else right;
    copy(swap_ptr, to_copy);

    left = ptr + element_width;
    right = ptr + (3 * element_width);
    swap_ptr = swap + (3 * element_width);
    tail_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const gt = compare(cmp, cmp_data, left, right) == GT;
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
    const lte = compare(cmp, cmp_data, left.*, right.*) != GT;
    // While not guaranteed branchless, tested in godbolt for x86_64, aarch32, aarch64, riscv64, and wasm32.
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
    const lte = compare(cmp, cmp_data, left.*, right.*) != GT;
    // While not guaranteed branchless, tested in godbolt for x86_64, aarch32, aarch64, riscv64, and wasm32.
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
    const gt = compare(cmp, cmp_data, ptr, ptr + element_width) == GT;
    // While not guaranteed branchless, tested in godbolt for x86_64, aarch32, aarch64, riscv64, and wasm32.
    const x = if (gt) element_width else 0;
    const y = if (gt) 0 else element_width;

    copy(swap, ptr + y);
    copy(ptr, ptr + x);
    copy(ptr + element_width, swap);
}

inline fn compare(cmp: CompareFn, cmp_data: Opaque, lhs: [*]u8, rhs: [*]u8) Ordering {
    return @as(Ordering, @enumFromInt(cmp(cmp_data, lhs, rhs)));
}

test "parity_merge_four" {
    var arr: [8]i64 = undefined;
    var swap: [8]i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    arr = [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 };
    swap = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
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
    var arr: [4]i64 = undefined;
    var swap: [4]i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    arr = [4]i64{ 1, 2, 3, 4 };
    swap = [4]i64{ 0, 0, 0, 0 };
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
    var arr: [2]i64 = undefined;
    var swap: i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap_ptr = @as([*]u8, @ptrCast(&swap));

    arr = [2]i64{ 10, 20 };
    swap_branchless(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [2]i64{ 10, 20 });

    arr = [2]i64{ 77, -12 };
    swap_branchless(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [2]i64{ -12, 77 });

    arr = [2]i64{ -22, -22 };
    swap_branchless(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [2]i64{ -22, -22 });
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
