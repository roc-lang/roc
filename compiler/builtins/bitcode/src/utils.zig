const std = @import("std");
const always_inline = std.builtin.CallOptions.Modifier.always_inline;

pub fn WithOverflow(comptime T: type) type {
    return extern struct { value: T, has_overflowed: bool };
}

// If allocation fails, this must cxa_throw - it must not return a null pointer!
extern fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*c_void;

// This should never be passed a null pointer.
// If allocation fails, this must cxa_throw - it must not return a null pointer!
extern fn roc_realloc(c_ptr: *c_void, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*c_void;

// This should never be passed a null pointer.
extern fn roc_dealloc(c_ptr: *c_void, alignment: u32) callconv(.C) void;

// Signals to the host that the program has panicked
extern fn roc_panic(c_ptr: *c_void, tag_id: u32) callconv(.C) void;

comptime {
    const builtin = @import("builtin");
    // During tetsts, use the testing allocators to satisfy these functions.
    if (builtin.is_test) {
        @export(testing_roc_alloc, .{ .name = "roc_alloc", .linkage = .Strong });
        @export(testing_roc_realloc, .{ .name = "roc_realloc", .linkage = .Strong });
        @export(testing_roc_dealloc, .{ .name = "roc_dealloc", .linkage = .Strong });
        @export(testing_roc_panic, .{ .name = "roc_panic", .linkage = .Strong });
    }
}

fn testing_roc_alloc(size: usize, _: u32) callconv(.C) ?*c_void {
    return @ptrCast(?*c_void, std.testing.allocator.alloc(u8, size) catch unreachable);
}

fn testing_roc_realloc(c_ptr: *c_void, new_size: usize, old_size: usize, _: u32) callconv(.C) ?*c_void {
    const ptr = @ptrCast([*]u8, @alignCast(16, c_ptr));
    const slice = ptr[0..old_size];

    return @ptrCast(?*c_void, std.testing.allocator.realloc(slice, new_size) catch unreachable);
}

fn testing_roc_dealloc(c_ptr: *c_void, _: u32) callconv(.C) void {
    const ptr = @ptrCast([*]u8, @alignCast(16, c_ptr));

    std.testing.allocator.destroy(ptr);
}

fn testing_roc_panic(c_ptr: *c_void, tag_id: u32) callconv(.C) void {
    _ = c_ptr;
    _ = tag_id;

    @panic("Roc panicked");
}

pub fn alloc(size: usize, alignment: u32) [*]u8 {
    return @ptrCast([*]u8, @call(.{ .modifier = always_inline }, roc_alloc, .{ size, alignment }));
}

pub fn realloc(c_ptr: [*]u8, new_size: usize, old_size: usize, alignment: u32) [*]u8 {
    return @ptrCast([*]u8, @call(.{ .modifier = always_inline }, roc_realloc, .{ c_ptr, new_size, old_size, alignment }));
}

pub fn dealloc(c_ptr: [*]u8, alignment: u32) void {
    return @call(.{ .modifier = always_inline }, roc_dealloc, .{ c_ptr, alignment });
}

// must export this explicitly because right now it is not used from zig code
pub fn panic(c_ptr: *c_void, alignment: u32) callconv(.C) void {
    return @call(.{ .modifier = always_inline }, roc_panic, .{ c_ptr, alignment });
}

// indirection because otherwise zig creates an alias to the panic function which our LLVM code
// does not know how to deal with
pub fn test_panic(c_ptr: *c_void, alignment: u32) callconv(.C) void {
    _ = c_ptr;
    _ = alignment;
    // const cstr = @ptrCast([*:0]u8, c_ptr);

    // const stderr = std.io.getStdErr().writer();
    // stderr.print("Roc panicked: {s}!\n", .{cstr}) catch unreachable;

    // std.c.exit(1);
}

pub const Inc = fn (?[*]u8) callconv(.C) void;
pub const IncN = fn (?[*]u8, u64) callconv(.C) void;
pub const Dec = fn (?[*]u8) callconv(.C) void;

const REFCOUNT_MAX_ISIZE: isize = 0;
pub const REFCOUNT_ONE_ISIZE: isize = std.math.minInt(isize);
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

pub fn decrefC(
    bytes_or_null: ?[*]isize,
    alignment: u32,
) callconv(.C) void {
    // IMPORTANT: bytes_or_null is this case is expected to be a pointer to the refcount
    // (NOT the start of the data, or the start of the allocation)

    // this is of course unsafe, but we trust what we get from the llvm side
    var bytes = @ptrCast([*]isize, bytes_or_null);

    return @call(.{ .modifier = always_inline }, decref_ptr_to_refcount, .{ bytes, alignment });
}

pub fn decrefCheckNullC(
    bytes_or_null: ?[*]u8,
    alignment: u32,
) callconv(.C) void {
    if (bytes_or_null) |bytes| {
        const isizes: [*]isize = @ptrCast([*]isize, @alignCast(@sizeOf(isize), bytes));
        return @call(.{ .modifier = always_inline }, decref_ptr_to_refcount, .{ isizes - 1, alignment });
    }
}

pub fn decref(
    bytes_or_null: ?[*]u8,
    data_bytes: usize,
    alignment: u32,
) void {
    if (data_bytes == 0) {
        return;
    }

    var bytes = bytes_or_null orelse return;

    const isizes: [*]isize = @ptrCast([*]isize, @alignCast(@sizeOf(isize), bytes));

    decref_ptr_to_refcount(isizes - 1, alignment);
}

inline fn decref_ptr_to_refcount(
    refcount_ptr: [*]isize,
    alignment: u32,
) void {
    const refcount: isize = refcount_ptr[0];
    const extra_bytes = std.math.max(alignment, @sizeOf(usize));

    if (refcount == REFCOUNT_ONE_ISIZE) {
        dealloc(@ptrCast([*]u8, refcount_ptr) - (extra_bytes - @sizeOf(usize)), alignment);
    } else if (refcount < 0) {
        refcount_ptr[0] = refcount - 1;
    }
}

pub fn allocateWithRefcount(
    data_bytes: usize,
    element_alignment: u32,
) [*]u8 {
    const alignment = std.math.max(@sizeOf(usize), element_alignment);
    const first_slot_offset = std.math.max(@sizeOf(usize), element_alignment);
    const length = alignment + data_bytes;

    switch (alignment) {
        16 => {
            var new_bytes: [*]align(16) u8 = @alignCast(16, alloc(length, alignment));

            var as_usize_array = @ptrCast([*]usize, new_bytes);
            as_usize_array[0] = 0;
            as_usize_array[1] = REFCOUNT_ONE;

            var as_u8_array = @ptrCast([*]u8, new_bytes);
            const first_slot = as_u8_array + first_slot_offset;

            return first_slot;
        },
        8 => {
            var raw = alloc(length, alignment);
            var new_bytes: [*]align(8) u8 = @alignCast(8, raw);

            var as_isize_array = @ptrCast([*]isize, new_bytes);
            as_isize_array[0] = REFCOUNT_ONE_ISIZE;

            var as_u8_array = @ptrCast([*]u8, new_bytes);
            const first_slot = as_u8_array + first_slot_offset;

            return first_slot;
        },
        4 => {
            var raw = alloc(length, alignment);
            var new_bytes: [*]align(@alignOf(isize)) u8 = @alignCast(@alignOf(isize), raw);

            var as_isize_array = @ptrCast([*]isize, new_bytes);
            as_isize_array[0] = REFCOUNT_ONE_ISIZE;

            var as_u8_array = @ptrCast([*]u8, new_bytes);
            const first_slot = as_u8_array + first_slot_offset;

            return first_slot;
        },
        else => {
            // const stdout = std.io.getStdOut().writer();
            // stdout.print("alignment: {d}", .{alignment}) catch unreachable;
            // @panic("allocateWithRefcount with invalid alignment");
            unreachable;
        },
    }
}

pub fn unsafeReallocate(
    source_ptr: [*]u8,
    alignment: u32,
    old_length: usize,
    new_length: usize,
    element_width: usize,
) [*]u8 {
    const align_width: usize = std.math.max(alignment, @sizeOf(usize));

    const old_width = align_width + old_length * element_width;
    const new_width = align_width + new_length * element_width;

    // TODO handle out of memory
    // NOTE realloc will dealloc the original allocation
    const old_allocation = source_ptr - align_width;
    const new_allocation = realloc(old_allocation, new_width, old_width, alignment);

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

pub const Ordering = enum(u8) {
    EQ = 0,
    GT = 1,
    LT = 2,
};

pub const UpdateMode = enum(u8) {
    Immutable = 0,
    InPlace = 1,
};
