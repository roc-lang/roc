const std = @import("std");
const always_inline = std.builtin.CallOptions.Modifier.always_inline;

// If allocation fails, this must cxa_throw - it must not return a null pointer!
extern fn roc_alloc(alignment: usize, size: usize) callconv(.C) *c_void;

// This should never be passed a null pointer.
// If allocation fails, this must cxa_throw - it must not return a null pointer!
extern fn roc_realloc(alignment: usize, c_ptr: *c_void, old_size: usize, new_size: usize) callconv(.C) *c_void;

// This should never be passed a null pointer.
extern fn roc_dealloc(alignment: usize, c_ptr: *c_void) callconv(.C) void;

comptime {
    if (std.builtin.is_test) {
        @export(testing_roc_alloc, .{ .name = "roc_alloc", .linkage = .Strong });
        @export(testing_roc_realloc, .{ .name = "roc_realloc", .linkage = .Strong });
        @export(testing_roc_dealloc, .{ .name = "roc_dealloc", .linkage = .Strong });
    }
}

fn testing_roc_alloc(alignment: usize, size: usize) callconv(.C) *c_void {
    return @ptrCast(*c_void, std.testing.allocator.alloc(u8, size) catch unreachable);
}

fn testing_roc_realloc(alignment: usize, c_ptr: *c_void, old_size: usize, new_size: usize) callconv(.C) *c_void {
    const ptr = @ptrCast([*]u8, @alignCast(16, c_ptr));
    const slice = ptr[0..old_size];

    return @ptrCast(*c_void, std.testing.allocator.realloc(slice, new_size) catch unreachable);
}

fn testing_roc_dealloc(alignment: usize, c_ptr: *c_void) callconv(.C) void {
    const ptr = @ptrCast([*]u8, @alignCast(16, c_ptr));

    std.testing.allocator.destroy(ptr);
}

pub fn alloc(alignment: usize, size: usize) [*]u8 {
    return @ptrCast([*]u8, @call(.{ .modifier = always_inline }, roc_alloc, .{ alignment, size }));
}

pub fn realloc(alignment: usize, c_ptr: [*]u8, old_size: usize, new_size: usize) [*]u8 {
    return @ptrCast([*]u8, @call(.{ .modifier = always_inline }, roc_realloc, .{ alignment, c_ptr, old_size, new_size }));
}

pub fn dealloc(alignment: usize, c_ptr: [*]u8) void {
    return @call(.{ .modifier = always_inline }, roc_dealloc, .{ alignment, c_ptr });
}

pub const Inc = fn (?[*]u8) callconv(.C) void;
pub const IncN = fn (?[*]u8, u64) callconv(.C) void;
pub const Dec = fn (?[*]u8) callconv(.C) void;

const REFCOUNT_MAX_ISIZE: comptime isize = 0;
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
                dealloc(alignment, bytes - 16);
            } else if (refcount_isize < 0) {
                (isizes - 1)[0] = refcount - 1;
            }
        },
        else => {
            // NOTE enums can currently have an alignment of < 8
            if (refcount == REFCOUNT_ONE_ISIZE) {
                dealloc(alignment, bytes - 8);
            } else if (refcount_isize < 0) {
                (isizes - 1)[0] = refcount - 1;
            }
        },
    }
}

pub fn allocateWithRefcount(
    alignment: usize,
    data_bytes: usize,
) [*]u8 {
    comptime const result_in_place = false;

    switch (alignment) {
        16 => {
            const length = 2 * @sizeOf(usize) + data_bytes;

            var new_bytes: [*]align(16) u8 = @alignCast(16, alloc(alignment, length));

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

            var new_bytes: [*]align(8) u8 = @alignCast(8, alloc(alignment, length));

            var as_isize_array = @ptrCast([*]isize, new_bytes);
            if (result_in_place) {
                as_isize_array[0] = @intCast(isize, number_of_slots);
            } else {
                as_isize_array[0] = REFCOUNT_ONE_ISIZE;
            }

            var as_u8_array = @ptrCast([*]u8, new_bytes);
            const first_slot = as_u8_array + @sizeOf(usize);

            return first_slot;
        },
    }
}

pub fn unsafeReallocate(
    source_ptr: [*]u8,
    alignment: usize,
    old_length: usize,
    new_length: usize,
    element_width: usize,
) [*]u8 {
    const align_width: usize = blk: {
        if (alignment > 8) {
            break :blk (2 * @sizeOf(usize));
        } else {
            break :blk @sizeOf(usize);
        }
    };

    const old_width = align_width + old_length * element_width;
    const new_width = align_width + new_length * element_width;

    // TODO handle out of memory
    // NOTE realloc will dealloc the original allocation
    const old_allocation = source_ptr - align_width;
    const new_allocation = realloc(alignment, old_allocation, old_width, new_width);

    const new_source = @ptrCast([*]u8, new_allocation) + align_width;
    return new_source;
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

pub const Ordering = packed enum(u8) {
    EQ = 0,
    GT = 1,
    LT = 2,
};
