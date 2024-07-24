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
const BufferType = [MAX_ELEMENT_BUFFER_SIZE]u8;
const BufferAlign = @alignOf(u128);
comptime {
    std.debug.assert(MAX_ELEMENT_BUFFER_SIZE % BufferAlign == 0);
}

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

// ================ Unbalanced Merges =========================================

/// Merges a full left block with a smaller than block size right chunk.
/// The merge goes from tail to head.
fn partial_backwards_merge(
    array: [*]u8,
    len: usize,
    swap: [*]u8,
    swap_len: usize,
    block_len: usize,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    std.debug.assert(swap_len >= block_len);

    if (len == block_len) {
        // Just a single block, already done.
        return;
    }

    var left_tail = array + (block_len - 1) * element_width;
    var dest_tail = array + (len - 1) * element_width;

    if (compare(cmp, cmp_data, left_tail, left_tail + element_width) != GT) {
        // Lucky case, blocks happen to be sorted.
        return;
    }

    const right_len = len - block_len;
    if (len <= swap_len and right_len >= 64) {
        // Large remaining merge and we have enough space to just do it in swap.

        cross_merge(swap, array, block_len, right_len, cmp_data, cmp, element_width, copy);

        @memcpy(array[0..(element_width * len)], swap[0..(element_width * len)]);

        return;
    }

    @memcpy(swap[0..(element_width * right_len)], (array + block_len * element_width)[0..(element_width * right_len)]);

    var right_tail = swap + (right_len - 1) * element_width;

    // For backards, we first try to do really large chunks, of 16 elements.
    outer: while (@intFromPtr(left_tail) > @intFromPtr(array + 16 * element_width) and @intFromPtr(right_tail) > @intFromPtr(swap + 16 * element_width)) {
        while (compare(cmp, cmp_data, left_tail, right_tail - 15 * element_width) != GT) {
            inline for (0..16) |_| {
                copy(dest_tail, right_tail);
                dest_tail -= element_width;
                right_tail -= element_width;
            }
            if (@intFromPtr(right_tail) <= @intFromPtr(swap + 16 * element_width))
                break :outer;
        }
        while (compare(cmp, cmp_data, left_tail - 15 * element_width, right_tail) == GT) {
            inline for (0..16) |_| {
                copy(dest_tail, left_tail);
                dest_tail -= element_width;
                left_tail -= element_width;
            }
            if (@intFromPtr(left_tail) <= @intFromPtr(array + 16 * element_width))
                break :outer;
        }
        // Attempt to deal with the rest of the chunk in groups of 2.
        var loops: usize = 8;
        while (true) {
            if (compare(cmp, cmp_data, left_tail, right_tail - element_width) != GT) {
                inline for (0..2) |_| {
                    copy(dest_tail, right_tail);
                    dest_tail -= element_width;
                    right_tail -= element_width;
                }
            } else if (compare(cmp, cmp_data, left_tail - element_width, right_tail) == GT) {
                inline for (0..2) |_| {
                    copy(dest_tail, left_tail);
                    dest_tail -= element_width;
                    left_tail -= element_width;
                }
            } else {
                // Couldn't move two elements, do a cross swap and continue.
                const lte = compare(cmp, cmp_data, left_tail, right_tail) != GT;
                var x = if (lte) element_width else 0;
                var not_x = if (!lte) element_width else 0;
                dest_tail -= element_width;
                copy(dest_tail + x, right_tail);
                right_tail -= element_width;
                copy(dest_tail + not_x, left_tail);
                left_tail -= element_width;
                dest_tail -= element_width;

                tail_branchless_merge(&dest_tail, &left_tail, &right_tail, cmp_data, cmp, element_width, copy);
            }

            loops -= 1;
            if (loops == 0)
                break;
        }
    }

    // For rest of tail, attempt to merge 2 elements a time from tail to head.
    while (@intFromPtr(right_tail) > @intFromPtr(swap) + element_width and @intFromPtr(left_tail) > @intFromPtr(array) + element_width) {
        // Note: I am not sure how to get the same generation as the original C.
        // This implementation has an extra function call here.
        // The C use `goto` to implement the two tail recursive functions below inline.
        const break_loop = partial_forward_merge_right_tail_2(&dest_tail, &array, &left_tail, &swap, &right_tail, cmp_data, cmp, element_width, copy);
        if (break_loop)
            break;

        // Couldn't move two elements, do a cross swap and continue.
        const lte = compare(cmp, cmp_data, left_tail, right_tail) != GT;
        var x = if (lte) element_width else 0;
        var not_x = if (!lte) element_width else 0;
        dest_tail -= element_width;
        copy(dest_tail + x, right_tail);
        right_tail -= element_width;
        copy(dest_tail + not_x, left_tail);
        left_tail -= element_width;
        dest_tail -= element_width;

        tail_branchless_merge(&dest_tail, &left_tail, &right_tail, cmp_data, cmp, element_width, copy);
    }

    // Deal with tail.
    while (@intFromPtr(right_tail) >= @intFromPtr(swap) and @intFromPtr(left_tail) >= @intFromPtr(array)) {
        tail_branchless_merge(&dest_tail, &left_tail, &right_tail, cmp_data, cmp, element_width, copy);
    }
    while (@intFromPtr(right_tail) >= @intFromPtr(swap)) {
        copy(dest_tail, right_tail);
        dest_tail -= element_width;
        right_tail -= element_width;
    }
}

// The following two functions are exactly the same but with the if blocks swapped.
// They hot loop on one side until it fails, then switch to the other list.

fn partial_forward_merge_right_tail_2(
    dest: *[*]u8,
    left_head: *const [*]u8,
    left_tail: *[*]u8,
    right_head: *const [*]u8,
    right_tail: *[*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) bool {
    if (compare(cmp, cmp_data, left_tail.*, right_tail.* - element_width) != GT) {
        inline for (0..2) |_| {
            copy(dest.*, right_tail.*);
            dest.* -= element_width;
            right_tail.* -= element_width;
        }
        if (@intFromPtr(right_tail.*) > @intFromPtr(right_head.*) + element_width) {
            return @call(.always_tail, partial_forward_merge_right_tail_2, .{ dest, left_head, left_tail, right_head, right_tail, cmp_data, cmp, element_width, copy });
        }
        return true;
    }
    if (compare(cmp, cmp_data, left_tail.* - element_width, right_tail.*) == GT) {
        inline for (0..2) |_| {
            copy(dest.*, left_tail.*);
            dest.* -= element_width;
            left_tail.* -= element_width;
        }
        if (@intFromPtr(left_tail.*) > @intFromPtr(left_head.*) + element_width) {
            return @call(.always_tail, partial_forward_merge_left_tail_2, .{ dest, left_head, left_tail, right_head, right_tail, cmp_data, cmp, element_width, copy });
        }
        return true;
    }
    return false;
}

fn partial_forward_merge_left_tail_2(
    dest: *[*]u8,
    left_head: *const [*]u8,
    left_tail: *[*]u8,
    right_head: *const [*]u8,
    right_tail: *[*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) bool {
    if (compare(cmp, cmp_data, left_tail.* - element_width, right_tail.*) == GT) {
        inline for (0..2) |_| {
            copy(dest.*, left_tail.*);
            dest.* -= element_width;
            left_tail.* -= element_width;
        }
        if (@intFromPtr(left_tail.*) > @intFromPtr(left_head.*) + element_width) {
            return @call(.always_tail, partial_forward_merge_left_tail_2, .{ dest, left_head, left_tail, right_head, right_tail, cmp_data, cmp, element_width, copy });
        }
        return true;
    }
    if (compare(cmp, cmp_data, left_tail.*, right_tail.* - element_width) != GT) {
        inline for (0..2) |_| {
            copy(dest.*, right_tail.*);
            dest.* -= element_width;
            right_tail.* -= element_width;
        }
        if (@intFromPtr(right_tail.*) > @intFromPtr(right_head.*) + element_width) {
            return @call(.always_tail, partial_forward_merge_right_tail_2, .{ dest, left_head, left_tail, right_head, right_tail, cmp_data, cmp, element_width, copy });
        }
        return true;
    }
    return false;
}

/// Merges a full left block with a smaller than block size right chunk.
/// The merge goes from head to tail.
fn partial_forward_merge(
    array: [*]u8,
    len: usize,
    swap: [*]u8,
    swap_len: usize,
    block_len: usize,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    std.debug.assert(swap_len >= block_len);

    if (len == block_len) {
        // Just a single block, already done.
        return;
    }

    var right_head = array + block_len * element_width;
    var right_tail = array + (len - 1) * element_width;

    if (compare(cmp, cmp_data, right_head - element_width, right_head) != GT) {
        // Lucky case, blocks happen to be sorted.
        return;
    }

    @memcpy(swap[0..(element_width * block_len)], array[0..(element_width * block_len)]);

    var left_head = swap;
    var left_tail = swap + (block_len - 1) * element_width;

    var dest_head = array;
    // Attempt to merge 2 elements a time from head then tail.
    while (@intFromPtr(left_head) < @intFromPtr(left_tail) - element_width and @intFromPtr(right_head) < @intFromPtr(right_tail) - element_width) {
        // Note: I am not sure how to get the same generation as the original C.
        // This implementation has an extra function call here.
        // The C use `goto` to implement the two tail recursive functions below inline.
        const break_loop = partial_forward_merge_right_head_2(&dest_head, &left_head, &left_tail, &right_head, &right_tail, cmp_data, cmp, element_width, copy);
        if (break_loop)
            break;

        // Couldn't move two elements, do a cross swap and continue.
        const lte = compare(cmp, cmp_data, left_head, right_head) != GT;
        var x = if (lte) element_width else 0;
        var not_x = if (!lte) element_width else 0;
        copy(dest_head + x, right_head);
        right_head += element_width;
        copy(dest_head + not_x, left_head);
        left_head += element_width;
        dest_head += 2 * element_width;

        head_branchless_merge(&dest_head, &left_head, &right_head, cmp_data, cmp, element_width, copy);
    }

    // Deal with tail.
    while (@intFromPtr(left_head) <= @intFromPtr(left_tail) and @intFromPtr(right_head) <= @intFromPtr(right_tail)) {
        head_branchless_merge(&dest_head, &left_head, &right_head, cmp_data, cmp, element_width, copy);
    }
    while (@intFromPtr(left_head) <= @intFromPtr(left_tail)) {
        copy(dest_head, left_head);
        dest_head += element_width;
        left_head += element_width;
    }
}

// The following two functions are exactly the same but with the if blocks swapped.
// They hot loop on one side until it fails, then switch to the other list.

fn partial_forward_merge_right_head_2(
    dest: *[*]u8,
    left_head: *[*]u8,
    left_tail: *const [*]u8,
    right_head: *[*]u8,
    right_tail: *const [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) bool {
    if (compare(cmp, cmp_data, left_head.*, right_head.* + element_width) == GT) {
        inline for (0..2) |_| {
            copy(dest.*, right_head.*);
            dest.* += element_width;
            right_head.* += element_width;
        }
        if (@intFromPtr(right_head.*) < @intFromPtr(right_tail.*) - element_width) {
            return @call(.always_tail, partial_forward_merge_right_head_2, .{ dest, left_head, left_tail, right_head, right_tail, cmp_data, cmp, element_width, copy });
        }
        return true;
    }
    if (compare(cmp, cmp_data, left_head.* + element_width, right_head.*) != GT) {
        inline for (0..2) |_| {
            copy(dest.*, left_head.*);
            dest.* += element_width;
            left_head.* += element_width;
        }
        if (@intFromPtr(left_head.*) < @intFromPtr(left_tail.*) - element_width) {
            return @call(.always_tail, partial_forward_merge_left_head_2, .{ dest, left_head, left_tail, right_head, right_tail, cmp_data, cmp, element_width, copy });
        }
        return true;
    }
    return false;
}

fn partial_forward_merge_left_head_2(
    dest: *[*]u8,
    left_head: *[*]u8,
    left_tail: *const [*]u8,
    right_head: *[*]u8,
    right_tail: *const [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) bool {
    if (compare(cmp, cmp_data, left_head.* + element_width, right_head.*) != GT) {
        inline for (0..2) |_| {
            copy(dest.*, left_head.*);
            dest.* += element_width;
            left_head.* += element_width;
        }
        if (@intFromPtr(left_head.*) < @intFromPtr(left_tail.*) - element_width) {
            return @call(.always_tail, partial_forward_merge_left_head_2, .{ dest, left_head, left_tail, right_head, right_tail, cmp_data, cmp, element_width, copy });
        }
        return true;
    }
    if (compare(cmp, cmp_data, left_head.*, right_head.* + element_width) == GT) {
        inline for (0..2) |_| {
            copy(dest.*, right_head.*);
            dest.* += element_width;
            right_head.* += element_width;
        }
        if (@intFromPtr(right_head.*) < @intFromPtr(right_tail.*) - element_width) {
            return @call(.always_tail, partial_forward_merge_right_head_2, .{ dest, left_head, left_tail, right_head, right_tail, cmp_data, cmp, element_width, copy });
        }
        return true;
    }
    return false;
}

test "partial_backwards_merge" {
    {
        const expected = [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

        var arr: [10]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        var swap: [10]i64 = undefined;
        var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

        arr = [10]i64{ 3, 4, 5, 6, 7, 8, 1, 2, 9, 10 };
        partial_backwards_merge(arr_ptr, 10, swap_ptr, 10, 6, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, expected);

        arr = [10]i64{ 2, 4, 6, 8, 9, 10, 1, 3, 5, 7 };
        partial_backwards_merge(arr_ptr, 10, swap_ptr, 10, 6, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, expected);

        arr = [10]i64{ 1, 2, 3, 4, 5, 6, 8, 9, 10, 7 };
        partial_backwards_merge(arr_ptr, 10, swap_ptr, 10, 9, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, expected);

        arr = [10]i64{ 1, 2, 4, 5, 6, 8, 9, 3, 7, 10 };
        partial_backwards_merge(arr_ptr, 10, swap_ptr, 9, 7, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, expected);
    }

    {
        var expected: [64]i64 = undefined;
        for (0..64) |i| {
            expected[i] = @intCast(i + 1);
        }

        var arr: [64]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        var swap: [64]i64 = undefined;
        var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

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
        partial_backwards_merge(arr_ptr, 64, swap_ptr, 64, 32, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, expected);

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

        partial_backwards_merge(arr_ptr, 64, swap_ptr, 64, 32, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, expected);
    }
}

test "partial_forward_merge" {
    const expected = [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

    var arr: [10]i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap: [10]i64 = undefined;
    var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    arr = [10]i64{ 3, 4, 5, 6, 7, 8, 1, 2, 9, 10 };
    partial_forward_merge(arr_ptr, 10, swap_ptr, 10, 6, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    arr = [10]i64{ 2, 4, 6, 8, 9, 10, 1, 3, 5, 7 };
    partial_forward_merge(arr_ptr, 10, swap_ptr, 10, 6, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    arr = [10]i64{ 1, 2, 3, 4, 5, 6, 8, 9, 10, 7 };
    partial_forward_merge(arr_ptr, 10, swap_ptr, 10, 9, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    arr = [10]i64{ 1, 2, 4, 5, 6, 8, 9, 3, 7, 10 };
    partial_forward_merge(arr_ptr, 10, swap_ptr, 9, 7, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);
}

// ================ Quad Merge Support ========================================

// TODO: quad_merge, requires tail merge first.

/// Merges 4 even sized blocks of sorted elements.
fn quad_merge_block(
    array: [*]u8,
    swap: [*]u8,
    block_len: usize,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    const block_x_2 = 2 * block_len;

    const block1 = array;
    const block2 = block1 + block_len * element_width;
    const block3 = block2 + block_len * element_width;
    const block4 = block3 + block_len * element_width;

    const in_order_1_2: u2 = @intFromBool(compare(cmp, cmp_data, block2 - element_width, block2) != GT);
    const in_order_3_4: u2 = @intFromBool(compare(cmp, cmp_data, block4 - element_width, block4) != GT);

    switch (in_order_1_2 | (in_order_3_4 << 1)) {
        0 => {
            // Nothing sorted. Just run merges on both.
            cross_merge(swap, array, block_len, block_len, cmp_data, cmp, element_width, copy);
            cross_merge(swap + block_x_2 * element_width, block3, block_len, block_len, cmp_data, cmp, element_width, copy);
        },
        1 => {
            // First half sorted already.
            @memcpy(swap[0..(element_width * block_x_2)], array[0..(element_width * block_x_2)]);
            cross_merge(swap + block_x_2 * element_width, block3, block_len, block_len, cmp_data, cmp, element_width, copy);
        },
        2 => {
            // Second half sorted already.
            cross_merge(swap, array, block_len, block_len, cmp_data, cmp, element_width, copy);
            @memcpy((swap + element_width * block_x_2)[0..(element_width * block_x_2)], block3[0..(element_width * block_x_2)]);
        },
        3 => {
            const in_order_2_3 = compare(cmp, cmp_data, block3 - element_width, block3) != GT;
            if (in_order_2_3)
                // Lucky, all sorted.
                return;

            // Copy everything into swap to merge back into this array.
            @memcpy(swap[0..(element_width * block_x_2 * 2)], array[0..(element_width * block_x_2 * 2)]);
        },
    }

    // Merge 2 larger blocks.
    cross_merge(array, swap, block_x_2, block_x_2, cmp_data, cmp, element_width, copy);
}

/// Cross merge attempts to merge two arrays in chunks of multiple elements.
fn cross_merge(
    dest: [*]u8,
    src: [*]u8,
    left_len: usize,
    right_len: usize,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    var left_head = src;
    var right_head = src + left_len * element_width;
    var left_tail = right_head - element_width;
    var right_tail = left_tail + right_len * element_width;

    // If the data looks too random and the sizes are similar,
    // fallback to the branchless parity merge.
    if (left_len + 1 >= right_len and right_len + 1 >= left_len and left_len >= 32) {
        const offset = 15 * element_width;
        if (compare(cmp, cmp_data, left_head + offset, right_head) == GT and compare(cmp, cmp_data, left_head, right_head + offset) != GT and compare(cmp, cmp_data, left_tail, right_tail - offset) == GT and compare(cmp, cmp_data, left_tail - offset, right_tail) != GT) {
            parity_merge(dest, src, left_len, right_len, cmp_data, cmp, element_width, copy);
            return;
        }
    }

    var dest_head = dest;
    var dest_tail = dest + (left_len + right_len - 1) * element_width;

    outer: while (true) {
        if (@intFromPtr(left_tail) - @intFromPtr(left_head) > 8 * element_width) {
            // 8 elements all less than or equal to and can be moved together.
            while (compare(cmp, cmp_data, left_head + 7 * element_width, right_head) != GT) {
                inline for (0..8) |_| {
                    copy(dest_head, left_head);
                    dest_head += element_width;
                    left_head += element_width;
                }
                if (@intFromPtr(left_tail) - @intFromPtr(left_head) <= 8 * element_width)
                    continue :outer;
            }

            // Attempt to do the same from the tail.
            // 8 elements all greater than and can be moved together.
            while (compare(cmp, cmp_data, left_tail - 7 * element_width, right_tail) == GT) {
                inline for (0..8) |_| {
                    copy(dest_tail, left_tail);
                    dest_tail -= element_width;
                    left_tail -= element_width;
                }
                if (@intFromPtr(left_tail) - @intFromPtr(left_head) <= 8 * element_width)
                    continue :outer;
            }
        }

        // Attempt to do the same for the right list.
        if (@intFromPtr(right_tail) - @intFromPtr(right_head) > 8 * element_width) {
            // left greater than 8 elements right and can be moved together.
            while (compare(cmp, cmp_data, left_head, right_head + 7 * element_width) == GT) {
                inline for (0..8) |_| {
                    copy(dest_head, right_head);
                    dest_head += element_width;
                    right_head += element_width;
                }
                if (@intFromPtr(right_tail) - @intFromPtr(right_head) <= 8 * element_width)
                    continue :outer;
            }

            // Attempt to do the same from the tail.
            // left less than or equalt to 8 elements right and can be moved together.
            while (compare(cmp, cmp_data, left_tail, right_tail - 7 * element_width) != GT) {
                inline for (0..8) |_| {
                    copy(dest_tail, right_tail);
                    dest_tail -= element_width;
                    right_tail -= element_width;
                }
                if (@intFromPtr(right_tail) - @intFromPtr(right_head) <= 8 * element_width)
                    continue :outer;
            }
        }

        if (@intFromPtr(dest_tail) - @intFromPtr(dest_head) < 16 * element_width)
            break;

        // Large enough to warrent a two way merge.
        var loops: usize = 8;
        while (true) {
            head_branchless_merge(&dest_head, &left_head, &right_head, cmp_data, cmp, element_width, copy);
            tail_branchless_merge(&dest_tail, &left_tail, &right_tail, cmp_data, cmp, element_width, copy);

            loops -= 1;
            if (loops == 0)
                break;
        }
    }

    // Clean up tail.
    while (@intFromPtr(left_head) <= @intFromPtr(left_tail) and @intFromPtr(right_head) <= @intFromPtr(right_tail)) {
        head_branchless_merge(&dest_head, &left_head, &right_head, cmp_data, cmp, element_width, copy);
    }
    while (@intFromPtr(left_head) <= @intFromPtr(left_tail)) {
        copy(dest_head, left_head);
        dest_head += element_width;
        left_head += element_width;
    }
    while (@intFromPtr(right_head) <= @intFromPtr(right_tail)) {
        copy(dest_head, right_head);
        dest_head += element_width;
        right_head += element_width;
    }
}

test "quad_merge_block" {
    const expected = [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 };

    var arr: [8]i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap: [8]i64 = undefined;
    var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    // case 0 - totally unsorted
    arr = [8]i64{ 7, 8, 5, 6, 3, 4, 1, 2 };
    quad_merge_block(arr_ptr, swap_ptr, 2, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    // case 1 - first half sorted
    arr = [8]i64{ 5, 6, 7, 8, 3, 4, 1, 2 };
    quad_merge_block(arr_ptr, swap_ptr, 2, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    // case 2 - second half sorted
    arr = [8]i64{ 7, 8, 5, 6, 1, 2, 3, 4 };
    quad_merge_block(arr_ptr, swap_ptr, 2, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    // case 3 both haves sorted
    arr = [8]i64{ 1, 3, 5, 7, 2, 4, 6, 8 };
    quad_merge_block(arr_ptr, swap_ptr, 2, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    // case 3 - lucky, sorted
    arr = [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 };
    quad_merge_block(arr_ptr, swap_ptr, 2, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);
}

test "cross_merge" {
    var expected: [64]i64 = undefined;
    for (0..64) |i| {
        expected[i] = @intCast(i + 1);
    }

    var src: [64]i64 = undefined;
    var dest: [64]i64 = undefined;
    var src_ptr = @as([*]u8, @ptrCast(&src[0]));
    var dest_ptr = @as([*]u8, @ptrCast(&dest[0]));

    // Opitimal case, ordered but swapped
    for (0..32) |i| {
        src[i] = @intCast(i + 33);
    }
    for (0..32) |i| {
        src[i + 32] = @intCast(i + 1);
    }
    cross_merge(dest_ptr, src_ptr, 32, 32, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, expected);

    // will fallback, every other
    for (0..32) |i| {
        src[i * 2] = @intCast(i * 2 + 1);
        src[i * 2 + 1] = @intCast(i * 2 + 2);
    }
    cross_merge(dest_ptr, src_ptr, 32, 32, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, expected);

    // super uneven
    for (0..20) |i| {
        src[i] = @intCast(i + 45);
    }
    for (0..44) |i| {
        src[i + 20] = @intCast(i + 1);
    }
    cross_merge(dest_ptr, src_ptr, 20, 44, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, expected);

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
    cross_merge(dest_ptr, src_ptr, 32, 32, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, expected);
}

// ================ 32 Element Blocks =========================================

/// This is basically a fast path to avoid `roc_alloc` for very sort arrays.
// TODO: quad_swap, requires tail merge first.
// It deals with 32 elements without a large allocation.

/// Merge 4 sorted arrays of length 2 into a sorted array of length 8 using swap space.
fn quad_swap_merge(
    array: [*]u8,
    swap: [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    parity_merge_two(swap, array, cmp_data, cmp, element_width, copy);
    parity_merge_two(swap + 4 * element_width, array + 4 * element_width, cmp_data, cmp, element_width, copy);

    parity_merge_four(array, swap, cmp_data, cmp, element_width, copy);
}

/// Reverse values from start to end.
fn quad_reversal(
    start: [*]u8,
    end: [*]u8,
    element_width: usize,
    copy: CopyFn,
) void {
    var buffer1: BufferType align(BufferAlign) = undefined;
    var buffer2: BufferType align(BufferAlign) = undefined;

    const tmp1_ptr = @as([*]u8, @ptrCast(&buffer1[0]));
    const tmp2_ptr = @as([*]u8, @ptrCast(&buffer2[0]));

    var loops = (@intFromPtr(end) - @intFromPtr(start)) / (element_width * 2);

    var h1_start = start;
    var h1_end = start + loops * element_width;
    var h2_start = end - loops * element_width;
    var h2_end = end;

    if (loops % 2 == 0) {
        copy(tmp2_ptr, h1_end);
        copy(h1_end, h2_start);
        h1_end -= element_width;
        copy(h2_start, tmp2_ptr);
        h2_start += element_width;
        loops -= 1;
    }

    loops /= 2;

    while (true) {
        copy(tmp1_ptr, h1_start);
        copy(h1_start, h2_end);
        h1_start += element_width;
        copy(h2_end, tmp1_ptr);
        h2_end -= element_width;

        copy(tmp2_ptr, h1_end);
        copy(h1_end, h2_start);
        h1_end -= element_width;
        copy(h2_start, tmp2_ptr);
        h2_start += element_width;

        if (loops == 0)
            break;
        loops -= 1;
    }
}

test "quad_swap_merge" {
    var arr: [8]i64 = undefined;
    var swap: [8]i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    arr = [8]i64{ 5, 6, 7, 8, 1, 2, 3, 4 };
    swap = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    quad_swap_merge(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 5, 7, 1, 3, 6, 8, 2, 4 };
    swap = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    quad_swap_merge(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 1, 8, 3, 4, 5, 6, 2, 7 };
    swap = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    quad_swap_merge(arr_ptr, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });
}

test "quad_reversal" {
    {
        var arr = [8]i64{ 8, 7, 6, 5, 4, 3, 2, 1 };
        var start_ptr = @as([*]u8, @ptrCast(&arr[0]));
        var end_ptr = @as([*]u8, @ptrCast(&arr[7]));
        quad_reversal(start_ptr, end_ptr, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });
    }
    {
        var arr = [9]i64{ 9, 8, 7, 6, 5, 4, 3, 2, 1 };
        var start_ptr = @as([*]u8, @ptrCast(&arr[0]));
        var end_ptr = @as([*]u8, @ptrCast(&arr[8]));
        quad_reversal(start_ptr, end_ptr, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [9]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9 });
    }
}

// ================ Small Arrays ==============================================
// Below are functions for sorting under 32 element arrays.

/// Uses swap space to sort the tail of an array.
/// The array should be under 32 elements in length.
fn tail_swap(
    array: [*]u8,
    len: usize,
    swap: [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    std.debug.assert(len < 32);

    if (len < 8) {
        tiny_sort(array, len, swap, cmp_data, cmp, element_width, copy);
        return;
    }

    const half1 = len / 2;
    const quad1 = half1 / 2;
    const quad2 = half1 - quad1;
    const half2 = len - half1;
    const quad3 = half2 / 2;
    const quad4 = half2 - quad3;

    var arr_ptr = array;
    tail_swap(arr_ptr, quad1, swap, cmp_data, cmp, element_width, copy);
    arr_ptr += quad1 * element_width;
    tail_swap(arr_ptr, quad2, swap, cmp_data, cmp, element_width, copy);
    arr_ptr += quad2 * element_width;
    tail_swap(arr_ptr, quad3, swap, cmp_data, cmp, element_width, copy);
    arr_ptr += quad3 * element_width;
    tail_swap(arr_ptr, quad4, swap, cmp_data, cmp, element_width, copy);

    if (compare(cmp, cmp_data, array + (quad1 - 1) * element_width, array + quad1 * element_width) != GT and compare(cmp, cmp_data, array + (half1 - 1) * element_width, array + half1 * element_width) != GT and compare(cmp, cmp_data, arr_ptr - 1 * element_width, arr_ptr) != GT) {
        return;
    }

    parity_merge(swap, array, quad1, quad2, cmp_data, cmp, element_width, copy);
    parity_merge(swap + half1 * element_width, array + half1 * element_width, quad3, quad4, cmp_data, cmp, element_width, copy);
    parity_merge(array, swap, half1, half2, cmp_data, cmp, element_width, copy);
}

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

test "tail_swap" {
    var swap: [31]i64 = undefined;
    var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    var arr: [31]i64 = undefined;
    var expected: [31]i64 = undefined;
    for (0..31) |i| {
        arr[i] = @intCast(i + 1);
        expected[i] = @intCast(i + 1);
    }
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

    for (0..10) |seed| {
        var rng = std.rand.DefaultPrng.init(seed);
        rng.random().shuffle(i64, arr[0..]);

        tail_swap(arr_ptr, 31, swap_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, expected);
    }
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

// ================ Tiny Arrays ===============================================
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

    var buffer: BufferType align(BufferAlign) = undefined;
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

/// Merge two neighboring sorted 4 element arrays into dest.
inline fn parity_merge_four(
    dest: [*]u8,
    array: [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    var left = array;
    var right = array + (4 * element_width);
    var dest_ptr = dest;
    head_branchless_merge(&dest_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    head_branchless_merge(&dest_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    head_branchless_merge(&dest_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const lte = compare(cmp, cmp_data, left, right) != GT;
    var to_copy = if (lte) left else right;
    copy(dest_ptr, to_copy);

    left = array + (3 * element_width);
    right = array + (7 * element_width);
    dest_ptr = dest + (7 * element_width);
    tail_branchless_merge(&dest_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    tail_branchless_merge(&dest_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    tail_branchless_merge(&dest_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const gt = compare(cmp, cmp_data, left, right) == GT;
    to_copy = if (gt) left else right;
    copy(dest_ptr, to_copy);
}

/// Merge two neighboring sorted 2 element arrays into dest.
inline fn parity_merge_two(
    dest: [*]u8,
    array: [*]u8,
    cmp_data: Opaque,
    cmp: CompareFn,
    element_width: usize,
    copy: CopyFn,
) void {
    var left = array;
    var right = array + (2 * element_width);
    var dest_ptr = dest;
    head_branchless_merge(&dest_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const lte = compare(cmp, cmp_data, left, right) != GT;
    var to_copy = if (lte) left else right;
    copy(dest_ptr, to_copy);

    left = array + element_width;
    right = array + (3 * element_width);
    dest_ptr = dest + (3 * element_width);
    tail_branchless_merge(&dest_ptr, &left, &right, cmp_data, cmp, element_width, copy);
    const gt = compare(cmp, cmp_data, left, right) == GT;
    to_copy = if (gt) left else right;
    copy(dest_ptr, to_copy);
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
    var dest: [8]i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var dest_ptr = @as([*]u8, @ptrCast(&dest[0]));

    arr = [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 };
    dest = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    parity_merge_four(dest_ptr, arr_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 5, 6, 7, 8, 1, 2, 3, 4 };
    dest = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    parity_merge_four(dest_ptr, arr_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 1, 3, 5, 7, 2, 4, 6, 8 };
    dest = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    parity_merge_four(dest_ptr, arr_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });
}

test "parity_merge_two" {
    var arr: [4]i64 = undefined;
    var dest: [4]i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var dest_ptr = @as([*]u8, @ptrCast(&dest[0]));

    arr = [4]i64{ 1, 2, 3, 4 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 1, 3, 2, 4 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 3, 4, 1, 2 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 2, 4, 1, 3 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 1, 4, 2, 3 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, null, &test_i64_compare, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });
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
