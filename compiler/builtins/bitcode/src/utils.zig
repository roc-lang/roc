const std = @import("std");
const Allocator = std.mem.Allocator;

const REFCOUNT_ONE_ISIZE: comptime isize = std.math.minInt(isize);
pub const REFCOUNT_ONE: usize = @bitCast(usize, REFCOUNT_ONE_ISIZE);

pub fn decref(
    allocator: *Allocator,
    alignment: usize,
    bytes_or_null: ?[*]u8,
    data_bytes: usize,
) void {
    if (data_bytes == 0) {
        return;
    }

    var bytes = bytes_or_null orelse return;

    const usizes: [*]usize = @ptrCast([*]usize, @alignCast(8, bytes));

    const refcount = (usizes - 1)[0];
    const refcount_isize = @bitCast(isize, refcount);

    switch (alignment) {
        8 => {
            if (refcount == REFCOUNT_ONE) {
                allocator.free((bytes - 8)[0 .. 8 + data_bytes]);
            } else if (refcount_isize < 0) {
                (usizes - 1)[0] = refcount + 1;
            }
        },
        16 => {
            if (refcount == REFCOUNT_ONE) {
                allocator.free((bytes - 16)[0 .. 16 + data_bytes]);
            } else if (refcount_isize < 0) {
                (usizes - 1)[0] = refcount + 1;
            }
        },
        else => unreachable,
    }
}

pub fn allocateWithRefcount(
    allocator: *Allocator,
    alignment: usize,
    data_bytes: usize,
) [*]u8 {
    comptime const result_in_place = false;

    switch (alignment) {
        8 => {
            const length = @sizeOf(usize) + data_bytes;

            var new_bytes: []align(8) u8 = allocator.alignedAlloc(u8, 8, length) catch unreachable;

            var as_usize_array = @ptrCast([*]usize, new_bytes);
            if (result_in_place) {
                as_usize_array[0] = @intCast(usize, number_of_slots);
            } else {
                as_usize_array[0] = REFCOUNT_ONE;
            }

            var as_u8_array = @ptrCast([*]u8, new_bytes);
            const first_slot = as_u8_array + @sizeOf(usize);

            return first_slot;
        },
        16 => {
            const length = 2 * @sizeOf(usize) + data_bytes;

            var new_bytes: []align(16) u8 = allocator.alignedAlloc(u8, 16, length) catch unreachable;

            var as_usize_array = @ptrCast([*]usize, new_bytes);
            if (result_in_place) {
                as_usize_array[0] = 0;
                as_usize_array[1] = @intCast(usize, number_of_slots);
            } else {
                as_usize_array[0] = 0;
                as_usize_array[1] = REFCOUNT_ONE;
            }

            var as_u8_array = @ptrCast([*]u8, new_bytes);
            const first_slot = as_u8_array + 2 * @sizeOf(usize);

            return first_slot;
        },
        else => unreachable,
    }
}
