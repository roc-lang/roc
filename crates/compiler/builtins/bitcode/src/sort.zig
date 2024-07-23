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
/// TODO: tune this.
/// I did some basic basic testing on my M1 and x86 machines with the c version of fluxsort.
/// The best tradeoff point is not the clearest and heavily depends on machine specifics.
/// Generally speaking, the faster memcpy is and the larger the cache line, the larger this should be.
/// Also, to my surprise, sorting by pointer is more performant on short arrays than long arrays (probably reduces time of final gather to order main array).
/// Anyway, there seems to be a hard cut off were the direct sort cost suddenly gets way larger.
/// In my testing for long arrays, the cutoff seems to be around 96-128 bytes.
/// For sort arrays, the custoff seems to be around 64-96 bytes.
const MAX_ELEMENT_BUFFER_SIZE: usize = 96;

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
// Below are functions for sorting under 31 element arrays.

/// Merges two neighboring sorted arrays into dest.
/// Left must be equal to or 1 smaller than right.
fn parity_merge(
    dest: [*]u8,
    src: [*]u8,
    left_len: usize,
    right_len: usize,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    std.debug.assert(left_len == right_len or left_len == right_len - 1);

    var left_head = src;
    var right_head = src + left_len * element_width;
    var dest_head = dest;

    var left_tail = right_head - element_width;
    var right_tail = left_tail + right_len * element_width;
    var dest_tail = dest + (left_len + right_len - 1) * element_width;

    if (left_len < right_len) {
        head_branchless_merge(&dest_head, &left_head, &right_head, cmp_data, cmp, element_width, copy);
    }
    head_branchless_merge(&dest_head, &left_head, &right_head, cmp_data, cmp, element_width, copy);

    var ll = left_len - 1;
    while (ll != 0) : (ll -= 1) {
        head_branchless_merge(&dest_head, &left_head, &right_head, cmp_data, cmp, element_width, copy);
        tail_branchless_merge(&dest_tail, &left_tail, &right_tail, cmp_data, cmp, element_width, copy);
    }
    tail_branchless_merge(&dest_tail, &left_tail, &right_tail, cmp_data, cmp, element_width, copy);
}

test "parity_merge" {
    {
        var dest: [8]i64 = undefined;
        var dest_ptr = @as([*]u8, @ptrCast(&dest[0]));

        var arr: [8]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [8]i64{ 1, 3, 5, 7, 2, 4, 6, 8 };
        dest = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
        parity_merge(dest_ptr, arr_ptr, 4, 4, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

        arr = [8]i64{ 5, 6, 7, 8, 1, 2, 3, 4 };
        dest = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
        parity_merge(dest_ptr, arr_ptr, 4, 4, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });
    }
    {
        var dest: [9]i64 = undefined;
        var dest_ptr = @as([*]u8, @ptrCast(&dest[0]));

        var arr: [9]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [9]i64{ 1, 3, 5, 8, 2, 4, 6, 7, 9 };
        dest = [9]i64{ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
        parity_merge(dest_ptr, arr_ptr, 4, 5, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(dest, [9]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9 });

        arr = [9]i64{ 6, 7, 8, 9, 1, 2, 3, 4, 5 };
        dest = [9]i64{ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
        parity_merge(dest_ptr, arr_ptr, 4, 5, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(dest, [9]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9 });
    }
}
// ================ Tiny Arrays ==============================================
// Below are functions for sorting 0 to 7 element arrays.

/// Sort arrays of 0 to 7 elements.
fn tiny_sort(
    array: [*]u8,
    len: usize,
    swap: [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    std.debug.assert(len < 8);

    var buffer: [MAX_ELEMENT_BUFFER_SIZE]u8 = undefined;
    const tmp_ptr = @as([*]u8, @ptrCast(&buffer[0]));

    switch (len) {
        1, 0 => {
            return;
        },
        2 => {
            swap_branchless(array, tmp_ptr, cmp_data, cmp, element_width, copy);
        },
        3 => {
            var arr_ptr = array;
            swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
            arr_ptr += element_width;
            swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
            arr_ptr -= element_width;
            swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
        },
        4 => {
            parity_swap_four(array, tmp_ptr, cmp_data, cmp, element_width, copy);
        },
        5 => {
            parity_swap_five(array, tmp_ptr, cmp_data, cmp, element_width, copy);
        },
        6 => {
            parity_swap_six(array, tmp_ptr, swap, cmp_data, cmp, element_width, copy);
        },
        7 => {
            parity_swap_seven(array, tmp_ptr, swap, cmp_data, cmp, element_width, copy);
        },
        else => {
            unreachable;
        },
    }
}

fn parity_swap_four(
    array: [*]u8,
    tmp_ptr: [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    var arr_ptr = array;
    swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr += 2 * element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr -= element_width;

    const gt = compare(cmp, cmp_data, arr_ptr, arr_ptr + element_width) == GT;
    if (gt) {
        copy(tmp_ptr, arr_ptr);
        copy(arr_ptr, arr_ptr + element_width);
        copy(arr_ptr + element_width, tmp_ptr);
        arr_ptr -= element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
        arr_ptr += 2 * element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
        arr_ptr -= element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    }
}

fn parity_swap_five(
    array: [*]u8,
    tmp_ptr: [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    var arr_ptr = array;
    swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr += 2 * element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr -= element_width;
    var more_work = swap_branchless_return_gt(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr += 2 * element_width;
    more_work += swap_branchless_return_gt(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr = array;

    if (more_work != 0) {
        swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
        arr_ptr += 2 * element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
        arr_ptr -= element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
        arr_ptr += 2 * element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
        arr_ptr = array;
        swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
        arr_ptr += 2 * element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    }
}

fn parity_swap_six(
    array: [*]u8,
    tmp_ptr: [*]u8,
    swap: [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    var arr_ptr = array;
    swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr += element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr += 3 * element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr -= element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr = array;

    {
        const lte = compare(cmp, cmp_data, arr_ptr + 2 * element_width, arr_ptr + 3 * element_width) != GT;
        if (lte) {
            swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
            arr_ptr += 4 * element_width;
            swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
            return;
        }
    }

    {
        const gt = compare(cmp, cmp_data, arr_ptr, arr_ptr + element_width) == GT;
        var x = if (gt) element_width else 0;
        var not_x = if (!gt) element_width else 0;
        copy(swap, arr_ptr + x);
        copy(swap + element_width, arr_ptr + not_x);
        copy(swap + 2 * element_width, arr_ptr + 2 * element_width);
        arr_ptr += 4 * element_width;
    }
    {
        const gt = compare(cmp, cmp_data, arr_ptr, arr_ptr + element_width) == GT;
        var x = if (gt) element_width else 0;
        var not_x = if (!gt) element_width else 0;
        copy(swap + 4 * element_width, arr_ptr + x);
        copy(swap + 5 * element_width, arr_ptr + not_x);
        copy(swap + 3 * element_width, arr_ptr - element_width);
    }

    arr_ptr = array;
    var left = swap;
    var right = swap + 3 * element_width;

    head_branchless_merge(&arr_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    head_branchless_merge(&arr_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    head_branchless_merge(&arr_ptr, &left, &right, cmp_data, cmp, element_width, copy);

    arr_ptr = array + 5 * element_width;
    left = swap + 2 * element_width;
    right = swap + 5 * element_width;

    tail_branchless_merge(&arr_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    tail_branchless_merge(&arr_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const gt = compare(cmp, cmp_data, left, right) == GT;
    const from = if (gt) left else right;
    copy(arr_ptr, from);
}

fn parity_swap_seven(
    array: [*]u8,
    tmp_ptr: [*]u8,
    swap: [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    var arr_ptr = array;
    swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr += 2 * element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr += 2 * element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr -= 3 * element_width;
    var more_work = swap_branchless_return_gt(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr += 2 * element_width;
    more_work += swap_branchless_return_gt(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr += 2 * element_width;
    more_work += swap_branchless_return_gt(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr -= element_width;

    if (more_work == 0)
        return;

    swap_branchless(arr_ptr, tmp_ptr, cmp_data, cmp, element_width, copy);
    arr_ptr = array;

    {
        const gt = compare(cmp, cmp_data, arr_ptr, arr_ptr + element_width) == GT;
        var x = if (gt) element_width else 0;
        var not_x = if (!gt) element_width else 0;
        copy(swap, arr_ptr + x);
        copy(swap + element_width, arr_ptr + not_x);
        copy(swap + 2 * element_width, arr_ptr + 2 * element_width);
        arr_ptr += 3 * element_width;
    }
    {
        const gt = compare(cmp, cmp_data, arr_ptr, arr_ptr + element_width) == GT;
        var x = if (gt) element_width else 0;
        var not_x = if (!gt) element_width else 0;
        copy(swap + 3 * element_width, arr_ptr + x);
        copy(swap + 4 * element_width, arr_ptr + not_x);
        arr_ptr += 2 * element_width;
    }
    {
        const gt = compare(cmp, cmp_data, arr_ptr, arr_ptr + element_width) == GT;
        var x = if (gt) element_width else 0;
        var not_x = if (!gt) element_width else 0;
        copy(swap + 5 * element_width, arr_ptr + x);
        copy(swap + 6 * element_width, arr_ptr + not_x);
    }

    arr_ptr = array;
    var left = swap;
    var right = swap + 3 * element_width;

    head_branchless_merge(&arr_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    head_branchless_merge(&arr_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    head_branchless_merge(&arr_ptr, &left, &right, cmp_data, cmp, element_width, copy);

    arr_ptr = array + 6 * element_width;
    left = swap + 2 * element_width;
    right = swap + 6 * element_width;

    tail_branchless_merge(&arr_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    tail_branchless_merge(&arr_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    tail_branchless_merge(&arr_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const gt = compare(cmp, cmp_data, left, right) == GT;
    const from = if (gt) left else right;
    copy(arr_ptr, from);
}

test "tiny_sort" {
    var swap: [7]i64 = undefined;
    var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    {
        var arr: [7]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [7]i64{ 3, 1, 2, 5, 4, 7, 6 };
        tiny_sort(arr_ptr, 7, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [7]i64{ 1, 2, 3, 4, 5, 6, 7 });

        arr = [7]i64{ 7, 6, 5, 4, 3, 2, 1 };
        tiny_sort(arr_ptr, 7, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [7]i64{ 1, 2, 3, 4, 5, 6, 7 });
    }
    {
        var arr: [6]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [6]i64{ 3, 1, 2, 6, 4, 5 };
        tiny_sort(arr_ptr, 6, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [6]i64{ 1, 2, 3, 4, 5, 6 });

        arr = [6]i64{ 6, 5, 4, 3, 2, 1 };
        tiny_sort(arr_ptr, 6, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [6]i64{ 1, 2, 3, 4, 5, 6 });
    }
    {
        var arr: [5]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [5]i64{ 2, 1, 4, 3, 5 };
        tiny_sort(arr_ptr, 5, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [5]i64{ 1, 2, 3, 4, 5 });

        arr = [5]i64{ 5, 4, 3, 2, 1 };
        tiny_sort(arr_ptr, 5, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [5]i64{ 1, 2, 3, 4, 5 });
    }
    {
        var arr: [4]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [4]i64{ 4, 2, 1, 3 };
        tiny_sort(arr_ptr, 4, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [4]i64{ 1, 2, 3, 4 });

        arr = [4]i64{ 2, 1, 4, 3 };
        tiny_sort(arr_ptr, 4, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [4]i64{ 1, 2, 3, 4 });
    }
    {
        var arr = [3]i64{ 2, 3, 1 };
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        tiny_sort(arr_ptr, 3, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [3]i64{ 1, 2, 3 });
    }
    {
        var arr = [2]i64{ 2, 1 };
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        tiny_sort(arr_ptr, 2, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [2]i64{ 1, 2 });
    }
}

// ================ Primitives ================================================
// Below are sorting primitives that attempt to be branchless.
// They all also are always inline for performance.
// The are the smallest fundamental unit.

/// Merge two neighboring sorted 4 element arrays into swap.
inline fn parity_merge_four(
    array: [*]u8,
    swap: [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    var left = array;
    var right = array + (4 * element_width);
    var swap_ptr = swap;
    head_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    head_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    head_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const lte = compare(cmp, cmp_data, left, right) != GT;
    var to_copy = if (lte) left else right;
    copy(swap_ptr, to_copy);

    left = array + (3 * element_width);
    right = array + (7 * element_width);
    swap_ptr = swap + (7 * element_width);
    tail_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    tail_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    tail_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const gt = compare(cmp, cmp_data, left, right) == GT;
    to_copy = if (gt) left else right;
    copy(swap_ptr, to_copy);
}

/// Merge two neighboring sorted 2 element arrays into swap.
inline fn parity_merge_two(
    array: [*]u8,
    swap: [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    var left = array;
    var right = array + (2 * element_width);
    var swap_ptr = swap;
    head_branchless_merge(&swap_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const lte = compare(cmp, cmp_data, left, right) != GT;
    var to_copy = if (lte) left else right;
    copy(swap_ptr, to_copy);

    left = array + element_width;
    right = array + (3 * element_width);
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
inline fn head_branchless_merge(
    dest: *[*]u8,
    left: *[*]u8,
    right: *[*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    // Note equivalent c code:
    //    *ptd++ = cmp(ptl, ptr) <= 0 ? *ptl++ : *ptr++;
    // While not guaranteed branchless, tested in godbolt for x86_64, aarch32, aarch64, riscv64, and wasm32.
    const lte = compare(cmp, cmp_data, left.*, right.*) != GT;
    const from = if (lte) left else right;
    copy(dest.*, from.*);
    from.* += element_width;
    dest.* += element_width;
}

/// Moves the smaller element from left and rigth to dest.
/// Will decrement both dest and the smaller element ptr to their previous index.
/// Inlining will remove the extra level of pointer indirection here.
/// It is just used to allow mutating the input pointers.
inline fn tail_branchless_merge(
    dest: *[*]u8,
    left: *[*]u8,
    right: *[*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    // Note equivalent c code:
    //    *tpd-- = cmp(tpl, tpr) > 0 ? *tpl-- : *tpr--;
    // While not guaranteed branchless, tested in godbolt for x86_64, aarch32, aarch64, riscv64, and wasm32.
    const gt = compare(cmp, cmp_data, left.*, right.*) == GT;
    const from = if (gt) left else right;
    copy(dest.*, from.*);
    from.* -= element_width;
    dest.* -= element_width;
}

/// Swaps the element at ptr with the element after it if the element is greater than the next.
inline fn swap_branchless(
    ptr: [*]u8,
    tmp: [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    // While not guaranteed branchless, tested in godbolt for x86_64, aarch32, aarch64, riscv64, and wasm32.
    _ = swap_branchless_return_gt(ptr, tmp, cmp_data, cmp, element_width, copy);
}

inline fn swap_branchless_return_gt(
    ptr: [*]u8,
    tmp: [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) u8 {
    // While not guaranteed branchless, tested in godbolt for x86_64, aarch32, aarch64, riscv64, and wasm32.
    const gt = compare(cmp, cmp_data, ptr, ptr + element_width) == GT;
    var x = if (gt) element_width else 0;
    const from = if (gt) ptr else ptr + element_width;
    copy(tmp, from);
    copy(ptr, ptr + x);
    copy(ptr + element_width, tmp);
    return @intFromBool(gt);
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
    var tmp: i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var tmp_ptr = @as([*]u8, @ptrCast(&tmp));

    arr = [2]i64{ 10, 20 };
    swap_branchless(arr_ptr, tmp_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [2]i64{ 10, 20 });

    arr = [2]i64{ 77, -12 };
    swap_branchless(arr_ptr, tmp_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [2]i64{ -12, 77 });

    arr = [2]i64{ -22, -22 };
    swap_branchless(arr_ptr, tmp_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
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
