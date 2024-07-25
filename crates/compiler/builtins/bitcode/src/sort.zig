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
    alignment: u32,
    copy: CopyFn,
) void {
    // Note, knowing constant versions of element_width and copy could have huge perf gains.
    // Hopefully llvm will essentially always do it via constant argument propagation and inlining.
    // If not, we may want to generate `n` different version of this function with comptime.
    // Then have our builtin dispatch to the correct version.
    // llvm garbage collection would remove all other variants.
    // Also, for numeric types, inlining the compare function can be a 2x perf gain.
    if (element_width <= MAX_ELEMENT_BUFFER_SIZE) {
        quadsort_direct(source_ptr, len, cmp, cmp_data, data_is_owned, inc_n_data, element_width, alignment, copy);
    } else {
        roc_panic("todo: fallback to an indirect pointer sort", 0);
    }
}

fn quadsort_direct(
    array: [*]u8,
    len: usize,
    cmp: CompareFn,
    cmp_data: Opaque,
    data_is_owned: bool,
    inc_n_data: IncN,
    element_width: usize,
    alignment: u32,
    copy: CopyFn,
) void {
    var arr_ptr = array;
    if (len < 32) {
        // TODO: This is a solid amount of stack space. Is that ok?
        // That said, it only ever allocates once (not recursive).
        // Aside from embedded is probably ok. Just a 3 KB with 96 byte MAX_ELEMENT_BUFFER_SIZE.
        // Also, zig doesn't hav alloca, so we always do max size here.
        var swap_buffer: [MAX_ELEMENT_BUFFER_SIZE * 32]u8 align(BufferAlign) = undefined;
        const swap = @as([*]u8, @ptrCast(&swap_buffer[0]));
        tail_swap(arr_ptr, len, swap, cmp, cmp_data, element_width, copy);
    } else if (quad_swap(arr_ptr, len, cmp, cmp_data, element_width, copy) != .sorted) {
        var swap_size = len;

        // This is optional, for about 5% perf hit, lower memory usage on large arrays.
        // if (len > 4194304) {
        //     swap_size = 4194304;
        //     while (swap_size * 8 <= len) : (swap_size *= 4) {}
        // }

        if (utils.alloc(swap_size * element_width, alignment)) |swap| {
            const block_len = quad_merge(arr_ptr, len, swap, swap_size, 32, cmp, cmp_data, element_width, copy);
            _ = block_len;

            // TODO: final rotate merge.

            utils.dealloc(swap, alignment);
        } else {
            // Fallback to still sort even when out of memory.
            @call(.never_inline, quadsort_stack_swap, .{ arr_ptr, len, cmp, cmp_data, data_is_owned, inc_n_data, element_width, copy });
        }
    }
}

fn quadsort_stack_swap(
    array: [*]u8,
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
    // Use a 512 element on stack swap buffer.
    var swap_buffer: [MAX_ELEMENT_BUFFER_SIZE * 512]u8 align(BufferAlign) = undefined;
    const swap = @as([*]u8, @ptrCast(&swap_buffer[0]));

    const block_len = quad_merge(array, len, swap, 512, 32, cmp, cmp_data, element_width, copy);
    _ = block_len;

    // TODO: final rotate merge.
}

// ================ Inplace Rotate Merge ======================================
// These are used as backup if the swap size is not large enough.
// Also can be used for the final merge to reduce memory footprint.

/// Binary search, but more cache friendly!
fn monobound_binary_first(
    array: [*]u8,
    initial_top: usize,
    value_ptr: [*]u8,
    cmp: CompareFn,
    cmp_data: Opaque,
    element_width: usize,
) usize {
    var top = initial_top;
    var end_ptr = array + top * element_width;

    while (top > 1) {
        const mid = top / 2;

        if (compare(cmp, cmp_data, value_ptr, end_ptr - mid * element_width) != GT) {
            end_ptr -= mid * element_width;
        }
        top -= mid;
    }

    if (compare(cmp, cmp_data, value_ptr, end_ptr - element_width) != GT) {
        end_ptr -= element_width;
    }
    return (@intFromPtr(end_ptr) - @intFromPtr(array)) / element_width;
}

/// Swap two neighboring chunks of an array quickly with limited memory.
fn trinity_rotation(
    array: [*]u8,
    len: usize,
    swap: [*]u8,
    full_swap_len: usize,
    left_len: usize,
    element_width: usize,
    copy: CopyFn,
) void {
    var buffer: BufferType align(BufferAlign) = undefined;
    const tmp_ptr = @as([*]u8, @ptrCast(&buffer[0]));

    const right_len = len - left_len;

    var swap_len = full_swap_len;
    if (full_swap_len > 65536) {
        swap_len = 65536;
    }

    if (left_len < right_len) {
        if (left_len <= swap_len) {
            @memcpy(swap[0..(element_width * left_len)], array[0..(element_width * left_len)]);
            std.mem.copyForwards(u8, array[0..(element_width * right_len)], (array + left_len * element_width)[0..(element_width * right_len)]);
            @memcpy((array + right_len * element_width)[0..(element_width * left_len)], swap[0..(element_width * left_len)]);
        } else {
            var a_ptr = array;
            var b_ptr = a_ptr + left_len * element_width;

            var bridge = right_len - left_len;
            if (bridge <= swap_len and bridge > 3) {
                var c_ptr = a_ptr + right_len * element_width;
                var d_ptr = c_ptr + left_len * element_width;

                @memcpy(swap[0..(bridge * element_width)], b_ptr[0..(bridge * element_width)]);

                for (0..left_len) |_| {
                    c_ptr -= element_width;
                    d_ptr -= element_width;
                    copy(c_ptr, d_ptr);
                    b_ptr -= element_width;
                    copy(d_ptr, b_ptr);
                }
                @memcpy(a_ptr[0..(bridge * element_width)], swap[0..(bridge * element_width)]);
            } else {
                var c_ptr = b_ptr;
                var d_ptr = c_ptr + right_len * element_width;

                bridge = left_len / 2;

                for (0..bridge) |_| {
                    b_ptr -= element_width;
                    copy(tmp_ptr, b_ptr);
                    copy(b_ptr, a_ptr);
                    copy(a_ptr, c_ptr);
                    a_ptr += element_width;
                    d_ptr -= element_width;
                    copy(c_ptr, d_ptr);
                    c_ptr += element_width;
                    copy(d_ptr, tmp_ptr);
                }

                bridge = (@intFromPtr(d_ptr) - @intFromPtr(c_ptr)) / (element_width * 2);
                for (0..bridge) |_| {
                    copy(tmp_ptr, c_ptr);
                    d_ptr -= element_width;
                    copy(c_ptr, d_ptr);
                    c_ptr += element_width;
                    copy(d_ptr, a_ptr);
                    copy(a_ptr, tmp_ptr);
                    a_ptr += element_width;
                }

                bridge = (@intFromPtr(d_ptr) - @intFromPtr(a_ptr)) / (element_width * 2);
                for (0..bridge) |_| {
                    copy(tmp_ptr, a_ptr);
                    d_ptr -= element_width;
                    copy(a_ptr, d_ptr);
                    a_ptr += element_width;
                    copy(d_ptr, tmp_ptr);
                }
            }
        }
    } else if (right_len < left_len) {
        if (right_len <= swap_len) {
            @memcpy(swap[0..(element_width * right_len)], (array + left_len * element_width)[0..(element_width * right_len)]);
            std.mem.copyBackwards(u8, (array + right_len * element_width)[0..(element_width * left_len)], array[0..(element_width * left_len)]);
            @memcpy(array[0..(element_width * right_len)], swap[0..(element_width * right_len)]);
        } else {
            var a_ptr = array;
            var b_ptr = a_ptr + left_len * element_width;

            var bridge = left_len - right_len;
            if (bridge <= swap_len and bridge > 3) {
                var c_ptr = a_ptr + right_len * element_width;
                var d_ptr = c_ptr + left_len * element_width;

                @memcpy(swap[0..(bridge * element_width)], c_ptr[0..(bridge * element_width)]);

                for (0..right_len) |_| {
                    copy(c_ptr, a_ptr);
                    c_ptr += element_width;
                    copy(a_ptr, b_ptr);
                    a_ptr += element_width;
                    b_ptr += element_width;
                }
                @memcpy((d_ptr - bridge * element_width)[0..(bridge * element_width)], swap[0..(bridge * element_width)]);
            } else {
                var c_ptr = b_ptr;
                var d_ptr = c_ptr + right_len * element_width;

                bridge = right_len / 2;

                for (0..bridge) |_| {
                    b_ptr -= element_width;
                    copy(tmp_ptr, b_ptr);
                    copy(b_ptr, a_ptr);
                    copy(a_ptr, c_ptr);
                    a_ptr += element_width;
                    d_ptr -= element_width;
                    copy(c_ptr, d_ptr);
                    c_ptr += element_width;
                    copy(d_ptr, tmp_ptr);
                }

                bridge = (@intFromPtr(b_ptr) - @intFromPtr(a_ptr)) / (element_width * 2);
                for (0..bridge) |_| {
                    b_ptr -= element_width;
                    copy(tmp_ptr, b_ptr);
                    copy(b_ptr, a_ptr);
                    d_ptr -= element_width;
                    copy(a_ptr, d_ptr);
                    a_ptr += element_width;
                    copy(d_ptr, tmp_ptr);
                }

                bridge = (@intFromPtr(d_ptr) - @intFromPtr(a_ptr)) / (element_width * 2);
                for (0..bridge) |_| {
                    copy(tmp_ptr, a_ptr);
                    d_ptr -= element_width;
                    copy(a_ptr, d_ptr);
                    a_ptr += element_width;
                    copy(d_ptr, tmp_ptr);
                }
            }
        }
    } else {
        var left_ptr = array;
        var right_ptr = left_ptr + left_len * element_width;

        for (0..left_len) |_| {
            copy(tmp_ptr, left_ptr);
            copy(left_ptr, right_ptr);
            left_ptr += element_width;
            copy(right_ptr, tmp_ptr);
            right_ptr += element_width;
        }
    }
}

test "monobound_binary_first" {
    var arr = [25]i64{ 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49 };
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var value: i64 = undefined;
    var value_ptr = @as([*]u8, @ptrCast(&value));

    value = 7;
    var res = monobound_binary_first(arr_ptr, 25, value_ptr, &test_i64_compare, null, @sizeOf(i64));
    try testing.expectEqual(res, 3);

    value = 39;
    res = monobound_binary_first(arr_ptr, 25, value_ptr, &test_i64_compare, null, @sizeOf(i64));
    try testing.expectEqual(res, 19);

    value = 40;
    res = monobound_binary_first(arr_ptr, 25, value_ptr, &test_i64_compare, null, @sizeOf(i64));
    try testing.expectEqual(res, 20);

    value = -10;
    res = monobound_binary_first(arr_ptr, 25, value_ptr, &test_i64_compare, null, @sizeOf(i64));
    try testing.expectEqual(res, 0);

    value = 10000;
    res = monobound_binary_first(arr_ptr, 25, value_ptr, &test_i64_compare, null, @sizeOf(i64));
    try testing.expectEqual(res, 25);
}

test "trinity_rotation" {
    {
        var arr: [10]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        var swap: [10]i64 = undefined;
        var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

        // Even.
        arr = [10]i64{ 6, 7, 8, 9, 10, 1, 2, 3, 4, 5 };
        trinity_rotation(arr_ptr, 10, swap_ptr, 10, 5, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });

        // left large, right fits in swap.
        arr = [10]i64{ 3, 4, 5, 6, 7, 8, 9, 10, 1, 2 };
        trinity_rotation(arr_ptr, 10, swap_ptr, 10, 8, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });

        // right large, left fits in swap.
        arr = [10]i64{ 9, 10, 1, 2, 3, 4, 5, 6, 7, 8 };
        trinity_rotation(arr_ptr, 10, swap_ptr, 10, 2, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });

        // left large, no swap.
        arr = [10]i64{ 3, 4, 5, 6, 7, 8, 9, 10, 1, 2 };
        trinity_rotation(arr_ptr, 10, swap_ptr, 0, 8, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });

        // right large, no swap.
        arr = [10]i64{ 9, 10, 1, 2, 3, 4, 5, 6, 7, 8 };
        trinity_rotation(arr_ptr, 10, swap_ptr, 0, 2, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });
    }
    {
        var arr: [16]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        var swap: [5]i64 = undefined;
        var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

        // left larger, bridge in swap.
        arr = [16]i64{ 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1, 2, 3, 4, 5, 6 };
        trinity_rotation(arr_ptr, 16, swap_ptr, 5, 10, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [16]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 });

        // // right large, bridge in swap.
        arr = [16]i64{ 11, 12, 13, 14, 15, 16, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        trinity_rotation(arr_ptr, 16, swap_ptr, 5, 6, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [16]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 });
    }
}

// ================ Unbalanced Merges =========================================

/// Merges the remaining blocks at the tail of the array.
fn tail_merge(
    array: [*]u8,
    len: usize,
    swap: [*]u8,
    swap_len: usize,
    block_len: usize,
    cmp: CompareFn,
    cmp_data: Opaque,
    element_width: usize,
    copy: CopyFn,
) void {
    const end_ptr = array + len * element_width;
    var current_block_len = block_len;
    while (current_block_len < len and current_block_len <= swap_len) : (current_block_len *= 2) {
        var arr_ptr = array;
        while (@intFromPtr(arr_ptr) + current_block_len * element_width < @intFromPtr(end_ptr)) : (arr_ptr += 2 * current_block_len * element_width) {
            if (@intFromPtr(arr_ptr) + 2 * current_block_len * element_width < @intFromPtr(end_ptr)) {
                partial_backwards_merge(arr_ptr, 2 * current_block_len, swap, swap_len, current_block_len, cmp, cmp_data, element_width, copy);
                continue;
            }
            const rem_len = (@intFromPtr(end_ptr) - @intFromPtr(arr_ptr)) / element_width;
            partial_backwards_merge(arr_ptr, rem_len, swap, swap_len, current_block_len, cmp, cmp_data, element_width, copy);
            break;
        }
    }
}

/// Merges a full left block with a smaller than block size right chunk.
/// The merge goes from tail to head.
fn partial_backwards_merge(
    array: [*]u8,
    len: usize,
    swap: [*]u8,
    swap_len: usize,
    block_len: usize,
    cmp: CompareFn,
    cmp_data: Opaque,
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

        cross_merge(swap, array, block_len, right_len, cmp, cmp_data, element_width, copy);

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

                tail_branchless_merge(&dest_tail, &left_tail, &right_tail, cmp, cmp_data, element_width, copy);
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
        // I think the closest equivalent in zig would be to use an enum and a switch.
        // That would potentially optimize to computed gotos.
        const break_loop = partial_forward_merge_right_tail_2(&dest_tail, &array, &left_tail, &swap, &right_tail, cmp, cmp_data, element_width, copy);
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

        tail_branchless_merge(&dest_tail, &left_tail, &right_tail, cmp, cmp_data, element_width, copy);
    }

    // Deal with tail.
    while (@intFromPtr(right_tail) >= @intFromPtr(swap) and @intFromPtr(left_tail) >= @intFromPtr(array)) {
        tail_branchless_merge(&dest_tail, &left_tail, &right_tail, cmp, cmp_data, element_width, copy);
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
    cmp: CompareFn,
    cmp_data: Opaque,
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
            return partial_forward_merge_right_tail_2(dest, left_head, left_tail, right_head, right_tail, cmp, cmp_data, element_width, copy);
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
            return partial_forward_merge_left_tail_2(dest, left_head, left_tail, right_head, right_tail, cmp, cmp_data, element_width, copy);
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
    cmp: CompareFn,
    cmp_data: Opaque,
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
            return partial_forward_merge_left_tail_2(dest, left_head, left_tail, right_head, right_tail, cmp, cmp_data, element_width, copy);
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
            return partial_forward_merge_right_tail_2(dest, left_head, left_tail, right_head, right_tail, cmp, cmp_data, element_width, copy);
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
    cmp: CompareFn,
    cmp_data: Opaque,
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
        // I think the closest equivalent in zig would be to use an enum and a switch.
        // That would potentially optimize to computed gotos.
        const break_loop = partial_forward_merge_right_head_2(&dest_head, &left_head, &left_tail, &right_head, &right_tail, cmp, cmp_data, element_width, copy);
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

        head_branchless_merge(&dest_head, &left_head, &right_head, cmp, cmp_data, element_width, copy);
    }

    // Deal with tail.
    while (@intFromPtr(left_head) <= @intFromPtr(left_tail) and @intFromPtr(right_head) <= @intFromPtr(right_tail)) {
        head_branchless_merge(&dest_head, &left_head, &right_head, cmp, cmp_data, element_width, copy);
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
    cmp: CompareFn,
    cmp_data: Opaque,
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
            return @call(.always_tail, partial_forward_merge_right_head_2, .{ dest, left_head, left_tail, right_head, right_tail, cmp, cmp_data, element_width, copy });
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
            return @call(.always_tail, partial_forward_merge_left_head_2, .{ dest, left_head, left_tail, right_head, right_tail, cmp, cmp_data, element_width, copy });
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
    cmp: CompareFn,
    cmp_data: Opaque,
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
            return @call(.always_tail, partial_forward_merge_left_head_2, .{ dest, left_head, left_tail, right_head, right_tail, cmp, cmp_data, element_width, copy });
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
            return @call(.always_tail, partial_forward_merge_right_head_2, .{ dest, left_head, left_tail, right_head, right_tail, cmp, cmp_data, element_width, copy });
        }
        return true;
    }
    return false;
}

test "tail_merge" {
    const expected = [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

    var arr: [10]i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap: [10]i64 = undefined;
    var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 2, 9, 10 };
    tail_merge(arr_ptr, 10, swap_ptr, 10, 2, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 2, 9, 10 };
    tail_merge(arr_ptr, 9, swap_ptr, 9, 2, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    arr = [10]i64{ 3, 4, 6, 9, 1, 2, 5, 10, 7, 8 };
    tail_merge(arr_ptr, 10, swap_ptr, 10, 4, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);
}

test "partial_backwards_merge" {
    {
        const expected = [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

        var arr: [10]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        var swap: [10]i64 = undefined;
        var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

        arr = [10]i64{ 3, 4, 5, 6, 7, 8, 1, 2, 9, 10 };
        partial_backwards_merge(arr_ptr, 10, swap_ptr, 10, 6, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, expected);

        arr = [10]i64{ 2, 4, 6, 8, 9, 10, 1, 3, 5, 7 };
        partial_backwards_merge(arr_ptr, 10, swap_ptr, 10, 6, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, expected);

        arr = [10]i64{ 1, 2, 3, 4, 5, 6, 8, 9, 10, 7 };
        partial_backwards_merge(arr_ptr, 10, swap_ptr, 10, 9, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, expected);

        arr = [10]i64{ 1, 2, 4, 5, 6, 8, 9, 3, 7, 10 };
        partial_backwards_merge(arr_ptr, 10, swap_ptr, 9, 7, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
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
        partial_backwards_merge(arr_ptr, 64, swap_ptr, 64, 32, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
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

        partial_backwards_merge(arr_ptr, 64, swap_ptr, 64, 32, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
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
    partial_forward_merge(arr_ptr, 10, swap_ptr, 10, 6, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    arr = [10]i64{ 2, 4, 6, 8, 9, 10, 1, 3, 5, 7 };
    partial_forward_merge(arr_ptr, 10, swap_ptr, 10, 6, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    arr = [10]i64{ 1, 2, 3, 4, 5, 6, 8, 9, 10, 7 };
    partial_forward_merge(arr_ptr, 10, swap_ptr, 10, 9, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    arr = [10]i64{ 1, 2, 4, 5, 6, 8, 9, 3, 7, 10 };
    partial_forward_merge(arr_ptr, 10, swap_ptr, 9, 7, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);
}

// ================ Quad Merge Support ========================================

/// Merges an array of of sized blocks of sorted elements with a tail.
/// Returns the block length of sorted runs after the call.
/// This is needed if the merge ran out of swap space.
fn quad_merge(
    array: [*]u8,
    len: usize,
    swap: [*]u8,
    swap_len: usize,
    block_len: usize,
    cmp: CompareFn,
    cmp_data: Opaque,
    element_width: usize,
    copy: CopyFn,
) usize {
    const end_ptr = array + len * element_width;
    var current_block_len = block_len * 4;

    while (current_block_len <= len and current_block_len <= swap_len) : (current_block_len *= 4) {
        var arr_ptr = array;
        while (true) {
            quad_merge_block(arr_ptr, swap, current_block_len / 4, cmp, cmp_data, element_width, copy);

            arr_ptr += current_block_len * element_width;
            if (@intFromPtr(arr_ptr) + current_block_len * element_width > @intFromPtr(end_ptr))
                break;
        }

        const rem_len = (@intFromPtr(end_ptr) - @intFromPtr(arr_ptr)) / element_width;
        tail_merge(arr_ptr, rem_len, swap, swap_len, current_block_len / 4, cmp, cmp_data, element_width, copy);
    }

    tail_merge(array, len, swap, swap_len, current_block_len / 4, cmp, cmp_data, element_width, copy);

    return current_block_len / 2;
}

/// Merges 4 even sized blocks of sorted elements.
fn quad_merge_block(
    array: [*]u8,
    swap: [*]u8,
    block_len: usize,
    cmp: CompareFn,
    cmp_data: Opaque,
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
            cross_merge(swap, array, block_len, block_len, cmp, cmp_data, element_width, copy);
            cross_merge(swap + block_x_2 * element_width, block3, block_len, block_len, cmp, cmp_data, element_width, copy);
        },
        1 => {
            // First half sorted already.
            @memcpy(swap[0..(element_width * block_x_2)], array[0..(element_width * block_x_2)]);
            cross_merge(swap + block_x_2 * element_width, block3, block_len, block_len, cmp, cmp_data, element_width, copy);
        },
        2 => {
            // Second half sorted already.
            cross_merge(swap, array, block_len, block_len, cmp, cmp_data, element_width, copy);
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
    cross_merge(array, swap, block_x_2, block_x_2, cmp, cmp_data, element_width, copy);
}

/// Cross merge attempts to merge two arrays in chunks of multiple elements.
fn cross_merge(
    dest: [*]u8,
    src: [*]u8,
    left_len: usize,
    right_len: usize,
    cmp: CompareFn,
    cmp_data: Opaque,
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
            parity_merge(dest, src, left_len, right_len, cmp, cmp_data, element_width, copy);
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
            head_branchless_merge(&dest_head, &left_head, &right_head, cmp, cmp_data, element_width, copy);
            tail_branchless_merge(&dest_tail, &left_tail, &right_tail, cmp, cmp_data, element_width, copy);

            loops -= 1;
            if (loops == 0)
                break;
        }
    }

    // Clean up tail.
    while (@intFromPtr(left_head) <= @intFromPtr(left_tail) and @intFromPtr(right_head) <= @intFromPtr(right_tail)) {
        head_branchless_merge(&dest_head, &left_head, &right_head, cmp, cmp_data, element_width, copy);
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

test "quad_merge" {
    const expected = [10]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

    var arr: [10]i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap: [10]i64 = undefined;
    var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));
    var size: usize = undefined;

    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 2, 9, 10 };
    size = quad_merge(arr_ptr, 10, swap_ptr, 10, 2, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);
    try testing.expectEqual(size, 16);

    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 9, 2, 10 };
    size = quad_merge(arr_ptr, 9, swap_ptr, 9, 2, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);
    try testing.expectEqual(size, 16);

    arr = [10]i64{ 3, 4, 6, 9, 1, 2, 5, 10, 7, 8 };
    size = quad_merge(arr_ptr, 10, swap_ptr, 10, 4, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);
    try testing.expectEqual(size, 8);

    // Limited swap, can't finish merge
    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 9, 2, 10 };
    size = quad_merge(arr_ptr, 10, swap_ptr, 4, 2, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [10]i64{ 1, 3, 4, 5, 6, 7, 8, 9, 2, 10 });
    try testing.expectEqual(size, 4);

    arr = [10]i64{ 7, 8, 5, 6, 3, 4, 1, 9, 2, 10 };
    size = quad_merge(arr_ptr, 10, swap_ptr, 3, 2, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [10]i64{ 5, 6, 7, 8, 1, 3, 4, 9, 2, 10 });
    try testing.expectEqual(size, 4);
}

test "quad_merge_block" {
    const expected = [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 };

    var arr: [8]i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap: [8]i64 = undefined;
    var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    // case 0 - totally unsorted
    arr = [8]i64{ 7, 8, 5, 6, 3, 4, 1, 2 };
    quad_merge_block(arr_ptr, swap_ptr, 2, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    // case 1 - first half sorted
    arr = [8]i64{ 5, 6, 7, 8, 3, 4, 1, 2 };
    quad_merge_block(arr_ptr, swap_ptr, 2, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    // case 2 - second half sorted
    arr = [8]i64{ 7, 8, 5, 6, 1, 2, 3, 4 };
    quad_merge_block(arr_ptr, swap_ptr, 2, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    // case 3 both haves sorted
    arr = [8]i64{ 1, 3, 5, 7, 2, 4, 6, 8 };
    quad_merge_block(arr_ptr, swap_ptr, 2, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, expected);

    // case 3 - lucky, sorted
    arr = [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 };
    quad_merge_block(arr_ptr, swap_ptr, 2, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
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
    cross_merge(dest_ptr, src_ptr, 32, 32, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, expected);

    // will fallback, every other
    for (0..32) |i| {
        src[i * 2] = @intCast(i * 2 + 1);
        src[i * 2 + 1] = @intCast(i * 2 + 2);
    }
    cross_merge(dest_ptr, src_ptr, 32, 32, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, expected);

    // super uneven
    for (0..20) |i| {
        src[i] = @intCast(i + 45);
    }
    for (0..44) |i| {
        src[i + 20] = @intCast(i + 1);
    }
    cross_merge(dest_ptr, src_ptr, 20, 44, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
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
    cross_merge(dest_ptr, src_ptr, 32, 32, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, expected);
}

// ================ 32 Element Blocks =========================================

const QuadSwapResult = enum {
    sorted,
    unfinished,
};

/// Starts with an unsorted array and turns it into sorted blocks of length 32.
fn quad_swap(
    array: [*]u8,
    len: usize,
    cmp: CompareFn,
    cmp_data: Opaque,
    element_width: usize,
    copy: CopyFn,
) QuadSwapResult {
    // TODO: This is a solid amount of stack space. Is that ok?
    // That said, it only ever allocates once (not recursive).
    // Aside from embedded is probably ok. Just a 3 KB with 96 byte MAX_ELEMENT_BUFFER_SIZE.
    var swap_buffer: [MAX_ELEMENT_BUFFER_SIZE * 32]u8 align(BufferAlign) = undefined;
    const swap = @as([*]u8, @ptrCast(&swap_buffer[0]));
    var tmp_buffer: BufferType align(BufferAlign) = undefined;
    const tmp_ptr = @as([*]u8, @ptrCast(&tmp_buffer[0]));

    var arr_ptr = array;
    var reverse_head = arr_ptr;

    // First sort groups of 8 elements.
    var count = len / 8;
    var skip_tail_swap = false;
    outer: while (count != 0) {
        count -= 1;

        var v1: u4 = @intFromBool(compare(cmp, cmp_data, arr_ptr + 0 * element_width, arr_ptr + 1 * element_width) == GT);
        var v2: u4 = @intFromBool(compare(cmp, cmp_data, arr_ptr + 2 * element_width, arr_ptr + 3 * element_width) == GT);
        var v3: u4 = @intFromBool(compare(cmp, cmp_data, arr_ptr + 4 * element_width, arr_ptr + 5 * element_width) == GT);
        var v4: u4 = @intFromBool(compare(cmp, cmp_data, arr_ptr + 6 * element_width, arr_ptr + 7 * element_width) == GT);

        // This is an attempt at computed gotos in zig.
        // Not yet sure if it will optimize as well as the raw gotos in C.
        const Cases = enum { ordered, reversed, not_ordered };
        var state: Cases = switch_state: {
            switch (v1 | (v2 << 1) | (v3 << 2) | (v4 << 3)) {
                0 => {
                    // potentially already ordered, check rest!
                    if (compare(cmp, cmp_data, arr_ptr + 1 * element_width, arr_ptr + 2 * element_width) != GT and compare(cmp, cmp_data, arr_ptr + 3 * element_width, arr_ptr + 4 * element_width) != GT and compare(cmp, cmp_data, arr_ptr + 5 * element_width, arr_ptr + 6 * element_width) != GT) {
                        break :switch_state .ordered;
                    }
                    quad_swap_merge(arr_ptr, swap, cmp, cmp_data, element_width, copy);

                    arr_ptr += 8 * element_width;
                    continue :outer;
                },
                // TODO: Figure out why the reversed case below is broken in some cases.
                // Note, it seems like the reverse itself is not broken, but the count and pointer become off somehow.
                // 15 => {
                //     // potentially already reverse ordered, check rest!
                //     if (compare(cmp, cmp_data, arr_ptr + 1 * element_width, arr_ptr + 2 * element_width) == GT and compare(cmp, cmp_data, arr_ptr + 3 * element_width, arr_ptr + 4 * element_width) == GT and compare(cmp, cmp_data, arr_ptr + 5 * element_width, arr_ptr + 6 * element_width) == GT) {
                //         reverse_head = arr_ptr;
                //         break :switch_state .reversed;
                //     }
                //     break :switch_state .not_ordered;
                // },
                else => {
                    break :switch_state .not_ordered;
                },
            }
        };
        while (true) {
            switch (state) {
                .not_ordered => {
                    inline for ([4]u4{ v1, v2, v3, v4 }) |v| {
                        const x = if (v == 0) element_width else 0;
                        const not_x = if (v != 0) element_width else 0;
                        copy(tmp_ptr, arr_ptr + x);
                        copy(arr_ptr, arr_ptr + not_x);
                        copy(arr_ptr + element_width, tmp_ptr);
                        arr_ptr += 2 * element_width;
                    }
                    arr_ptr -= 8 * element_width;

                    quad_swap_merge(arr_ptr, swap, cmp, cmp_data, element_width, copy);

                    arr_ptr += 8 * element_width;
                    continue :outer;
                },
                .ordered => {
                    arr_ptr += 8 * element_width;

                    // 1 group was order, lets see if that continues!
                    if (count != 0) {
                        count -= 1;
                        v1 = @intFromBool(compare(cmp, cmp_data, arr_ptr + 0 * element_width, arr_ptr + 1 * element_width) == GT);
                        v2 = @intFromBool(compare(cmp, cmp_data, arr_ptr + 2 * element_width, arr_ptr + 3 * element_width) == GT);
                        v3 = @intFromBool(compare(cmp, cmp_data, arr_ptr + 4 * element_width, arr_ptr + 5 * element_width) == GT);
                        v4 = @intFromBool(compare(cmp, cmp_data, arr_ptr + 6 * element_width, arr_ptr + 7 * element_width) == GT);
                        if (v1 | v2 | v3 | v4 != 0) {
                            // Sadly not ordered still, maybe reversed though?
                            if (v1 + v2 + v3 + v4 == 4 and compare(cmp, cmp_data, arr_ptr + 1 * element_width, arr_ptr + 2 * element_width) == GT and compare(cmp, cmp_data, arr_ptr + 3 * element_width, arr_ptr + 4 * element_width) == GT and compare(cmp, cmp_data, arr_ptr + 5 * element_width, arr_ptr + 6 * element_width) == GT) {
                                reverse_head = arr_ptr;
                                state = .reversed;
                                continue;
                            }
                            state = .not_ordered;
                            continue;
                        }
                        if (compare(cmp, cmp_data, arr_ptr + 1 * element_width, arr_ptr + 2 * element_width) != GT and compare(cmp, cmp_data, arr_ptr + 3 * element_width, arr_ptr + 4 * element_width) != GT and compare(cmp, cmp_data, arr_ptr + 5 * element_width, arr_ptr + 6 * element_width) != GT) {
                            state = .ordered;
                            continue;
                        }

                        quad_swap_merge(arr_ptr, swap, cmp, cmp_data, element_width, copy);
                        arr_ptr += 8 * element_width;
                        continue :outer;
                    }
                    break :outer;
                },
                .reversed => {
                    arr_ptr += 8 * element_width;

                    // 1 group was reversed, lets see if that continues!
                    if (count != 0) {
                        count -= 1;
                        v1 = @intFromBool(compare(cmp, cmp_data, arr_ptr + 0 * element_width, arr_ptr + 1 * element_width) != GT);
                        v2 = @intFromBool(compare(cmp, cmp_data, arr_ptr + 2 * element_width, arr_ptr + 3 * element_width) != GT);
                        v3 = @intFromBool(compare(cmp, cmp_data, arr_ptr + 4 * element_width, arr_ptr + 5 * element_width) != GT);
                        v4 = @intFromBool(compare(cmp, cmp_data, arr_ptr + 6 * element_width, arr_ptr + 7 * element_width) != GT);
                        if (v1 | v2 | v3 | v4 != 0) {
                            // Sadly not still reversed.
                            // So we just need to reverse upto this point, but not the current 8 element block.
                        } else {
                            // This also checks the boundary between this and the last block.
                            if (compare(cmp, cmp_data, arr_ptr - 1 * element_width, arr_ptr + 0 * element_width) == GT and compare(cmp, cmp_data, arr_ptr + 1 * element_width, arr_ptr + 2 * element_width) == GT and compare(cmp, cmp_data, arr_ptr + 3 * element_width, arr_ptr + 4 * element_width) == GT and compare(cmp, cmp_data, arr_ptr + 5 * element_width, arr_ptr + 6 * element_width) == GT) {
                                // Row multiple reversed blocks in a row!
                                state = .reversed;
                                continue;
                            }
                        }
                        // Actually fix up the reversed blocks.
                        quad_reversal(reverse_head, arr_ptr - element_width, element_width, copy);

                        // Since we already have v1 to v4, check the next block state.
                        if (v1 + v2 + v3 + v4 == 4 and compare(cmp, cmp_data, arr_ptr + 1 * element_width, arr_ptr + 2 * element_width) != GT and compare(cmp, cmp_data, arr_ptr + 3 * element_width, arr_ptr + 4 * element_width) != GT and compare(cmp, cmp_data, arr_ptr + 5 * element_width, arr_ptr + 6 * element_width) != GT) {
                            state = .ordered;
                            continue;
                        }
                        if (v1 + v2 + v3 + v4 == 0 and compare(cmp, cmp_data, arr_ptr + 1 * element_width, arr_ptr + 2 * element_width) == GT and compare(cmp, cmp_data, arr_ptr + 3 * element_width, arr_ptr + 4 * element_width) == GT and compare(cmp, cmp_data, arr_ptr + 5 * element_width, arr_ptr + 6 * element_width) == GT) {
                            reverse_head = arr_ptr;
                            state = .reversed;
                            continue;
                        }

                        // Just an unorderd block, do it inplace.

                        inline for ([4]u4{ v1, v2, v3, v4 }) |v| {
                            const x = if (v == 0) element_width else 0;
                            const not_x = if (v != 0) element_width else 0;
                            copy(tmp_ptr, arr_ptr + not_x);
                            copy(arr_ptr, arr_ptr + x);
                            copy(arr_ptr + element_width, tmp_ptr);
                            arr_ptr += 2 * element_width;
                        }
                        arr_ptr -= 8 * element_width;

                        if (compare(cmp, cmp_data, arr_ptr + 1 * element_width, arr_ptr + 2 * element_width) == GT or compare(cmp, cmp_data, arr_ptr + 3 * element_width, arr_ptr + 4 * element_width) == GT or compare(cmp, cmp_data, arr_ptr + 5 * element_width, arr_ptr + 6 * element_width) == GT) {
                            quad_swap_merge(arr_ptr, swap, cmp, cmp_data, element_width, copy);
                        }
                        arr_ptr += 8;
                        continue :outer;
                    }

                    // Handle tail block when reversing.
                    const rem = len % 8;
                    reverse_block: {
                        if (rem == 7 and compare(cmp, cmp_data, arr_ptr + 5 * element_width, arr_ptr + 6 * element_width) != GT)
                            break :reverse_block;
                        if (rem >= 6 and compare(cmp, cmp_data, arr_ptr + 4 * element_width, arr_ptr + 5 * element_width) != GT)
                            break :reverse_block;
                        if (rem >= 5 and compare(cmp, cmp_data, arr_ptr + 3 * element_width, arr_ptr + 4 * element_width) != GT)
                            break :reverse_block;
                        if (rem >= 4 and compare(cmp, cmp_data, arr_ptr + 2 * element_width, arr_ptr + 3 * element_width) != GT)
                            break :reverse_block;
                        if (rem >= 3 and compare(cmp, cmp_data, arr_ptr + 1 * element_width, arr_ptr + 2 * element_width) != GT)
                            break :reverse_block;
                        if (rem >= 2 and compare(cmp, cmp_data, arr_ptr + 0 * element_width, arr_ptr + 1 * element_width) != GT)
                            break :reverse_block;
                        if (rem >= 1 and compare(cmp, cmp_data, arr_ptr - 1 * element_width, arr_ptr + 0 * element_width) != GT)
                            break :reverse_block;
                        quad_reversal(reverse_head, arr_ptr + (rem - 1) * element_width, element_width, copy);

                        // If we just reversed the entire array, it is sorted.
                        if (reverse_head == array)
                            return .sorted;

                        skip_tail_swap = true;
                        break :outer;
                    }
                    quad_reversal(reverse_head, arr_ptr - element_width, element_width, copy);

                    break :outer;
                },
            }
        }
    }
    if (!skip_tail_swap) {
        tail_swap(arr_ptr, len % 8, swap, cmp, cmp_data, element_width, copy);
    }

    // Group into 32 element blocks.
    arr_ptr = array;

    count = len / 32;
    while (count != 0) : ({
        count -= 1;
        arr_ptr += 32 * element_width;
    }) {
        if (compare(cmp, cmp_data, arr_ptr + 7 * element_width, arr_ptr + 8 * element_width) != GT and compare(cmp, cmp_data, arr_ptr + 15 * element_width, arr_ptr + 16 * element_width) != GT and compare(cmp, cmp_data, arr_ptr + 23 * element_width, arr_ptr + 24 * element_width) != GT) {
            // Already in order.
            continue;
        }
        parity_merge(swap, arr_ptr, 8, 8, cmp, cmp_data, element_width, copy);
        parity_merge(swap + 16 * element_width, arr_ptr + 16 * element_width, 8, 8, cmp, cmp_data, element_width, copy);
        parity_merge(arr_ptr, swap, 16, 16, cmp, cmp_data, element_width, copy);
    }

    // Deal with final tail for 32 element blocks.
    // Anything over 8 elements is multiple blocks worth merging together.
    if (len % 32 > 8) {
        tail_merge(arr_ptr, len % 32, swap, 32, 8, cmp, cmp_data, element_width, copy);
    }

    return .unfinished;
}

/// Merge 4 sorted arrays of length 2 into a sorted array of length 8 using swap space.
fn quad_swap_merge(
    array: [*]u8,
    swap: [*]u8,
    cmp: CompareFn,
    cmp_data: Opaque,
    element_width: usize,
    copy: CopyFn,
) void {
    parity_merge_two(swap, array, cmp, cmp_data, element_width, copy);
    parity_merge_two(swap + 4 * element_width, array + 4 * element_width, cmp, cmp_data, element_width, copy);

    parity_merge_four(array, swap, cmp, cmp_data, element_width, copy);
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

// test "quad_swap" {
//     var arr: [75]i64 = undefined;
//     var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

//     arr = [75]i64{
//         // multiple ordered chunks
//         1,  3,  5,  7,  9,  11, 13, 15,
//         //
//         33, 34, 35, 36, 37, 38, 39, 40,
//         // partially ordered
//         41, 42, 45, 46, 43, 44, 47, 48,
//         // multiple reverse chunks
//         70, 69, 68, 67, 66, 65, 64, 63,
//         //
//         16, 14, 12, 10, 8,  6,  4,  2,
//         // another ordered
//         49, 50, 51, 52, 53, 54, 55, 56,
//         // unordered
//         23, 21, 19, 20, 24, 22, 18, 17,
//         // partially reversed
//         32, 31, 28, 27, 30, 29, 26, 25,
//         // awkward tail
//         62, 59, 61, 60, 71, 73, 75, 74,
//         //
//         72, 58, 57,
//     };

//     var result = quad_swap(arr_ptr, 75, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
//     try testing.expectEqual(result, .unfinished);
//     try testing.expectEqual(arr, [75]i64{
//         // first 32 elements sorted (with 8 reversed that get flipped here)
//         1,  2,  3,  4,  5,  6,  7,  8,
//         //
//         9,  10, 11, 12, 13, 14, 15, 16,
//         //
//         33, 34, 35, 36, 37, 38, 39, 40,
//         //
//         41, 42, 43, 44, 45, 46, 47, 48,
//         // second 32 elements sorted (with 8 reversed that get flipped here)
//         17, 18, 19, 20, 21, 22, 23, 24,
//         //
//         25, 26, 27, 28, 29, 30, 31, 32,
//         //
//         49, 50, 51, 52, 53, 54, 55, 56,
//         //
//         63, 64, 65, 66, 67, 68, 69, 70,
//         // awkward tail
//         57, 58, 59, 60, 61, 62, 71, 72,
//         //
//         73, 74, 75,
//     });

//     // Just reversed.
//     var expected: [75]i64 = undefined;
//     for (0..75) |i| {
//         expected[i] = @intCast(i + 1);
//         arr[i] = @intCast(75 - i);
//     }
//     result = quad_swap(arr_ptr, 75, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
//     try testing.expectEqual(result, .sorted);
//     try testing.expectEqual(arr, expected);
// }

test "quad_swap_merge" {
    var arr: [8]i64 = undefined;
    var swap: [8]i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var swap_ptr = @as([*]u8, @ptrCast(&swap[0]));

    arr = [8]i64{ 5, 6, 7, 8, 1, 2, 3, 4 };
    swap = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    quad_swap_merge(arr_ptr, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 5, 7, 1, 3, 6, 8, 2, 4 };
    swap = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    quad_swap_merge(arr_ptr, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 1, 8, 3, 4, 5, 6, 2, 7 };
    swap = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    quad_swap_merge(arr_ptr, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
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
    cmp: CompareFn,
    cmp_data: Opaque,
    element_width: usize,
    copy: CopyFn,
) void {
    std.debug.assert(len < 32);

    if (len < 8) {
        tiny_sort(array, len, swap, cmp, cmp_data, element_width, copy);
        return;
    }

    const half1 = len / 2;
    const quad1 = half1 / 2;
    const quad2 = half1 - quad1;
    const half2 = len - half1;
    const quad3 = half2 / 2;
    const quad4 = half2 - quad3;

    var arr_ptr = array;
    tail_swap(arr_ptr, quad1, swap, cmp, cmp_data, element_width, copy);
    arr_ptr += quad1 * element_width;
    tail_swap(arr_ptr, quad2, swap, cmp, cmp_data, element_width, copy);
    arr_ptr += quad2 * element_width;
    tail_swap(arr_ptr, quad3, swap, cmp, cmp_data, element_width, copy);
    arr_ptr += quad3 * element_width;
    tail_swap(arr_ptr, quad4, swap, cmp, cmp_data, element_width, copy);

    if (compare(cmp, cmp_data, array + (quad1 - 1) * element_width, array + quad1 * element_width) != GT and compare(cmp, cmp_data, array + (half1 - 1) * element_width, array + half1 * element_width) != GT and compare(cmp, cmp_data, arr_ptr - 1 * element_width, arr_ptr) != GT) {
        return;
    }

    parity_merge(swap, array, quad1, quad2, cmp, cmp_data, element_width, copy);
    parity_merge(swap + half1 * element_width, array + half1 * element_width, quad3, quad4, cmp, cmp_data, element_width, copy);
    parity_merge(array, swap, half1, half2, cmp, cmp_data, element_width, copy);
}

/// Merges two neighboring sorted arrays into dest.
/// Left must be equal to or 1 smaller than right.
fn parity_merge(
    dest: [*]u8,
    src: [*]u8,
    left_len: usize,
    right_len: usize,
    cmp: CompareFn,
    cmp_data: Opaque,
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
        head_branchless_merge(&dest_head, &left_head, &right_head, cmp, cmp_data, element_width, copy);
    }
    head_branchless_merge(&dest_head, &left_head, &right_head, cmp, cmp_data, element_width, copy);

    var ll = left_len - 1;
    while (ll != 0) : (ll -= 1) {
        head_branchless_merge(&dest_head, &left_head, &right_head, cmp, cmp_data, element_width, copy);
        tail_branchless_merge(&dest_tail, &left_tail, &right_tail, cmp, cmp_data, element_width, copy);
    }
    tail_branchless_merge(&dest_tail, &left_tail, &right_tail, cmp, cmp_data, element_width, copy);
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

        tail_swap(arr_ptr, 31, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
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
        parity_merge(dest_ptr, arr_ptr, 4, 4, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

        arr = [8]i64{ 5, 6, 7, 8, 1, 2, 3, 4 };
        dest = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
        parity_merge(dest_ptr, arr_ptr, 4, 4, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });
    }
    {
        var dest: [9]i64 = undefined;
        var dest_ptr = @as([*]u8, @ptrCast(&dest[0]));

        var arr: [9]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [9]i64{ 1, 3, 5, 8, 2, 4, 6, 7, 9 };
        dest = [9]i64{ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
        parity_merge(dest_ptr, arr_ptr, 4, 5, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(dest, [9]i64{ 1, 2, 3, 4, 5, 6, 7, 8, 9 });

        arr = [9]i64{ 6, 7, 8, 9, 1, 2, 3, 4, 5 };
        dest = [9]i64{ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
        parity_merge(dest_ptr, arr_ptr, 4, 5, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
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
    cmp: CompareFn,
    cmp_data: Opaque,
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
            swap_branchless(array, tmp_ptr, cmp, cmp_data, element_width, copy);
        },
        3 => {
            var arr_ptr = array;
            swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
            arr_ptr += element_width;
            swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
            arr_ptr -= element_width;
            swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
        },
        4 => {
            parity_swap_four(array, tmp_ptr, cmp, cmp_data, element_width, copy);
        },
        5 => {
            parity_swap_five(array, tmp_ptr, cmp, cmp_data, element_width, copy);
        },
        6 => {
            parity_swap_six(array, tmp_ptr, swap, cmp, cmp_data, element_width, copy);
        },
        7 => {
            parity_swap_seven(array, tmp_ptr, swap, cmp, cmp_data, element_width, copy);
        },
        else => {
            unreachable;
        },
    }
}

fn parity_swap_four(
    array: [*]u8,
    tmp_ptr: [*]u8,
    cmp: CompareFn,
    cmp_data: Opaque,
    element_width: usize,
    copy: CopyFn,
) void {
    var arr_ptr = array;
    swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr += 2 * element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr -= element_width;

    const gt = compare(cmp, cmp_data, arr_ptr, arr_ptr + element_width) == GT;
    if (gt) {
        copy(tmp_ptr, arr_ptr);
        copy(arr_ptr, arr_ptr + element_width);
        copy(arr_ptr + element_width, tmp_ptr);
        arr_ptr -= element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
        arr_ptr += 2 * element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
        arr_ptr -= element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    }
}

fn parity_swap_five(
    array: [*]u8,
    tmp_ptr: [*]u8,
    cmp: CompareFn,
    cmp_data: Opaque,
    element_width: usize,
    copy: CopyFn,
) void {
    var arr_ptr = array;
    swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr += 2 * element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr -= element_width;
    var more_work = swap_branchless_return_gt(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr += 2 * element_width;
    more_work += swap_branchless_return_gt(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr = array;

    if (more_work != 0) {
        swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
        arr_ptr += 2 * element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
        arr_ptr -= element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
        arr_ptr += 2 * element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
        arr_ptr = array;
        swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
        arr_ptr += 2 * element_width;
        swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    }
}

fn parity_swap_six(
    array: [*]u8,
    tmp_ptr: [*]u8,
    swap: [*]u8,
    cmp: CompareFn,
    cmp_data: Opaque,
    element_width: usize,
    copy: CopyFn,
) void {
    var arr_ptr = array;
    swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr += element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr += 3 * element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr -= element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr = array;

    {
        const lte = compare(cmp, cmp_data, arr_ptr + 2 * element_width, arr_ptr + 3 * element_width) != GT;
        if (lte) {
            swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
            arr_ptr += 4 * element_width;
            swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
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

    head_branchless_merge(&arr_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    head_branchless_merge(&arr_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    head_branchless_merge(&arr_ptr, &left, &right, cmp, cmp_data, element_width, copy);

    arr_ptr = array + 5 * element_width;
    left = swap + 2 * element_width;
    right = swap + 5 * element_width;

    tail_branchless_merge(&arr_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    tail_branchless_merge(&arr_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    const gt = compare(cmp, cmp_data, left, right) == GT;
    const from = if (gt) left else right;
    copy(arr_ptr, from);
}

fn parity_swap_seven(
    array: [*]u8,
    tmp_ptr: [*]u8,
    swap: [*]u8,
    cmp: CompareFn,
    cmp_data: Opaque,
    element_width: usize,
    copy: CopyFn,
) void {
    var arr_ptr = array;
    swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr += 2 * element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr += 2 * element_width;
    swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr -= 3 * element_width;
    var more_work = swap_branchless_return_gt(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr += 2 * element_width;
    more_work += swap_branchless_return_gt(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr += 2 * element_width;
    more_work += swap_branchless_return_gt(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
    arr_ptr -= element_width;

    if (more_work == 0)
        return;

    swap_branchless(arr_ptr, tmp_ptr, cmp, cmp_data, element_width, copy);
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

    head_branchless_merge(&arr_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    head_branchless_merge(&arr_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    head_branchless_merge(&arr_ptr, &left, &right, cmp, cmp_data, element_width, copy);

    arr_ptr = array + 6 * element_width;
    left = swap + 2 * element_width;
    right = swap + 6 * element_width;

    tail_branchless_merge(&arr_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    tail_branchless_merge(&arr_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    tail_branchless_merge(&arr_ptr, &left, &right, cmp, cmp_data, element_width, copy);
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
        tiny_sort(arr_ptr, 7, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [7]i64{ 1, 2, 3, 4, 5, 6, 7 });

        arr = [7]i64{ 7, 6, 5, 4, 3, 2, 1 };
        tiny_sort(arr_ptr, 7, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [7]i64{ 1, 2, 3, 4, 5, 6, 7 });
    }
    {
        var arr: [6]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [6]i64{ 3, 1, 2, 6, 4, 5 };
        tiny_sort(arr_ptr, 6, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [6]i64{ 1, 2, 3, 4, 5, 6 });

        arr = [6]i64{ 6, 5, 4, 3, 2, 1 };
        tiny_sort(arr_ptr, 6, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [6]i64{ 1, 2, 3, 4, 5, 6 });
    }
    {
        var arr: [5]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [5]i64{ 2, 1, 4, 3, 5 };
        tiny_sort(arr_ptr, 5, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [5]i64{ 1, 2, 3, 4, 5 });

        arr = [5]i64{ 5, 4, 3, 2, 1 };
        tiny_sort(arr_ptr, 5, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [5]i64{ 1, 2, 3, 4, 5 });
    }
    {
        var arr: [4]i64 = undefined;
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));

        arr = [4]i64{ 4, 2, 1, 3 };
        tiny_sort(arr_ptr, 4, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [4]i64{ 1, 2, 3, 4 });

        arr = [4]i64{ 2, 1, 4, 3 };
        tiny_sort(arr_ptr, 4, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [4]i64{ 1, 2, 3, 4 });
    }
    {
        var arr = [3]i64{ 2, 3, 1 };
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        tiny_sort(arr_ptr, 3, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
        try testing.expectEqual(arr, [3]i64{ 1, 2, 3 });
    }
    {
        var arr = [2]i64{ 2, 1 };
        var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
        tiny_sort(arr_ptr, 2, swap_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
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
    cmp: CompareFn,
    cmp_data: Opaque,
    element_width: usize,
    copy: CopyFn,
) void {
    var left = array;
    var right = array + (4 * element_width);
    var dest_ptr = dest;
    head_branchless_merge(&dest_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    head_branchless_merge(&dest_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    head_branchless_merge(&dest_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    const lte = compare(cmp, cmp_data, left, right) != GT;
    var to_copy = if (lte) left else right;
    copy(dest_ptr, to_copy);

    left = array + (3 * element_width);
    right = array + (7 * element_width);
    dest_ptr = dest + (7 * element_width);
    tail_branchless_merge(&dest_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    tail_branchless_merge(&dest_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    tail_branchless_merge(&dest_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    const gt = compare(cmp, cmp_data, left, right) == GT;
    to_copy = if (gt) left else right;
    copy(dest_ptr, to_copy);
}

/// Merge two neighboring sorted 2 element arrays into dest.
inline fn parity_merge_two(
    dest: [*]u8,
    array: [*]u8,
    cmp: CompareFn,
    cmp_data: Opaque,
    element_width: usize,
    copy: CopyFn,
) void {
    var left = array;
    var right = array + (2 * element_width);
    var dest_ptr = dest;
    head_branchless_merge(&dest_ptr, &left, &right, cmp, cmp_data, element_width, copy);
    const lte = compare(cmp, cmp_data, left, right) != GT;
    var to_copy = if (lte) left else right;
    copy(dest_ptr, to_copy);

    left = array + element_width;
    right = array + (3 * element_width);
    dest_ptr = dest + (3 * element_width);
    tail_branchless_merge(&dest_ptr, &left, &right, cmp, cmp_data, element_width, copy);
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
    cmp: CompareFn,
    cmp_data: Opaque,
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
    cmp: CompareFn,
    cmp_data: Opaque,
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
    cmp: CompareFn,
    cmp_data: Opaque,
    element_width: usize,
    copy: CopyFn,
) void {
    // While not guaranteed branchless, tested in godbolt for x86_64, aarch32, aarch64, riscv64, and wasm32.
    _ = swap_branchless_return_gt(ptr, tmp, cmp, cmp_data, element_width, copy);
}

inline fn swap_branchless_return_gt(
    ptr: [*]u8,
    tmp: [*]u8,
    cmp: CompareFn,
    cmp_data: Opaque,
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
    parity_merge_four(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 5, 6, 7, 8, 1, 2, 3, 4 };
    dest = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    parity_merge_four(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });

    arr = [8]i64{ 1, 3, 5, 7, 2, 4, 6, 8 };
    dest = [8]i64{ 0, 0, 0, 0, 0, 0, 0, 0 };
    parity_merge_four(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [8]i64{ 1, 2, 3, 4, 5, 6, 7, 8 });
}

test "parity_merge_two" {
    var arr: [4]i64 = undefined;
    var dest: [4]i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var dest_ptr = @as([*]u8, @ptrCast(&dest[0]));

    arr = [4]i64{ 1, 2, 3, 4 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 1, 3, 2, 4 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 3, 4, 1, 2 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 2, 4, 1, 3 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });

    arr = [4]i64{ 1, 4, 2, 3 };
    dest = [4]i64{ 0, 0, 0, 0 };
    parity_merge_two(dest_ptr, arr_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(dest, [4]i64{ 1, 2, 3, 4 });
}

test "head_branchless_merge" {
    var dest = [6]i64{ 0, 0, 0, 0, 0, 0 };
    var left = [4]i64{ 1, 7, 10, 22 };
    var right = [4]i64{ 2, 2, 8, 22 };
    var dest_ptr = @as([*]u8, @ptrCast(&dest[0]));
    var left_ptr = @as([*]u8, @ptrCast(&left[0]));
    var right_ptr = @as([*]u8, @ptrCast(&right[0]));

    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    head_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(dest, [6]i64{ 1, 2, 2, 7, 8, 10 });
}

test "tail_branchless_merge" {
    var dest = [6]i64{ 0, 0, 0, 0, 0, 0 };
    var left = [4]i64{ -22, 1, 7, 10 };
    var right = [4]i64{ -22, 2, 2, 8 };
    var dest_ptr = @as([*]u8, @ptrCast(&dest[dest.len - 1]));
    var left_ptr = @as([*]u8, @ptrCast(&left[left.len - 1]));
    var right_ptr = @as([*]u8, @ptrCast(&right[right.len - 1]));

    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    tail_branchless_merge(&dest_ptr, &left_ptr, &right_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);

    try testing.expectEqual(dest, [6]i64{ 1, 2, 2, 7, 8, 10 });
}

test "swap" {
    var arr: [2]i64 = undefined;
    var tmp: i64 = undefined;
    var arr_ptr = @as([*]u8, @ptrCast(&arr[0]));
    var tmp_ptr = @as([*]u8, @ptrCast(&tmp));

    arr = [2]i64{ 10, 20 };
    swap_branchless(arr_ptr, tmp_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [2]i64{ 10, 20 });

    arr = [2]i64{ 77, -12 };
    swap_branchless(arr_ptr, tmp_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
    try testing.expectEqual(arr, [2]i64{ -12, 77 });

    arr = [2]i64{ -22, -22 };
    swap_branchless(arr_ptr, tmp_ptr, &test_i64_compare, null, @sizeOf(i64), &test_i64_copy);
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
