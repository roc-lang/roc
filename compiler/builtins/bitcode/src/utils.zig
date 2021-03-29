const std = @import("std");
const Allocator = std.mem.Allocator;

const REFCOUNT_ONE_ISIZE: comptime isize = std.math.minInt(isize);
pub const REFCOUNT_ONE: usize = @bitCast(usize, REFCOUNT_ONE_ISIZE);

pub const IntWidth = enum(u8) {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    Usize,
};

pub fn intWidth(width: IntWidth) anytype {
    switch (width) {
        IntWidth.U8 => {
            return u8;
        },
        IntWidth.U16 => {
            return u16;
        },
        IntWidth.U32 => {
            return u32;
        },
        IntWidth.U64 => {
            return u64;
        },
        IntWidth.U128 => {
            return u128;
        },
        IntWidth.I8 => {
            return i8;
        },
        IntWidth.I16 => {
            return i16;
        },
        IntWidth.I32 => {
            return i32;
        },
        IntWidth.I64 => {
            return i64;
        },
        IntWidth.I128 => {
            return i128;
        },
        IntWidth.Usize => {
            return usize;
        },
    }
}

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

    const isizes: [*]isize = @ptrCast([*]isize, @alignCast(8, bytes));

    const refcount = (isizes - 1)[0];
    const refcount_isize = @bitCast(isize, refcount);

    switch (alignment) {
        16 => {
            if (refcount == REFCOUNT_ONE_ISIZE) {
                allocator.free((bytes - 16)[0 .. 16 + data_bytes]);
            } else if (refcount_isize < 0) {
                (isizes - 1)[0] = refcount - 1;
            }
        },
        else => {
            // NOTE enums can currently have an alignment of < 8
            if (refcount == REFCOUNT_ONE_ISIZE) {
                allocator.free((bytes - 8)[0 .. 8 + data_bytes]);
            } else if (refcount_isize < 0) {
                (isizes - 1)[0] = refcount - 1;
            }
        },
    }
}

pub fn allocateWithRefcount(
    allocator: *Allocator,
    alignment: usize,
    data_bytes: usize,
) [*]u8 {
    comptime const result_in_place = false;

    switch (alignment) {
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
        else => {
            const length = @sizeOf(usize) + data_bytes;

            var new_bytes: []align(8) u8 = allocator.alignedAlloc(u8, 8, length) catch unreachable;

            var as_usize_array = @ptrCast([*]isize, new_bytes);
            if (result_in_place) {
                as_usize_array[0] = @intCast(isize, number_of_slots);
            } else {
                as_usize_array[0] = REFCOUNT_ONE_ISIZE;
            }

            var as_u8_array = @ptrCast([*]u8, new_bytes);
            const first_slot = as_u8_array + @sizeOf(usize);

            return first_slot;
        },
    }
}

pub const RocResult = extern struct {
    bytes: ?[*]u8,

    pub fn isOk(self: RocResult) bool {
        // assumptions
        //
        // - the tag is the first field
        // - the tag is usize bytes wide
        // - Ok has tag_id 1, because Err < Ok
        const usizes: [*]usize = @ptrCast([*]usize, @alignCast(8, self.bytes));

        return usizes[0] == 1;
    }

    pub fn isErr(self: RocResult) bool {
        return !self.isOk();
    }
};
