const std = @import("std");
const always_inline = std.builtin.CallOptions.Modifier.always_inline;
const Monotonic = std.builtin.AtomicOrder.Monotonic;

pub fn WithOverflow(comptime T: type) type {
    return extern struct { value: T, has_overflowed: bool };
}

// If allocation fails, this must cxa_throw - it must not return a null pointer!
extern fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*anyopaque;

// This should never be passed a null pointer.
// If allocation fails, this must cxa_throw - it must not return a null pointer!
extern fn roc_realloc(c_ptr: *anyopaque, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*anyopaque;

// This should never be passed a null pointer.
extern fn roc_dealloc(c_ptr: *anyopaque, alignment: u32) callconv(.C) void;

// Signals to the host that the program has panicked
extern fn roc_panic(c_ptr: *const anyopaque, tag_id: u32) callconv(.C) void;

// should work just like libc memcpy (we can't assume libc is present)
extern fn roc_memcpy(dst: [*]u8, src: [*]u8, size: usize) callconv(.C) void;

comptime {
    const builtin = @import("builtin");
    // During tests, use the testing allocators to satisfy these functions.
    if (builtin.is_test) {
        @export(testing_roc_alloc, .{ .name = "roc_alloc", .linkage = .Strong });
        @export(testing_roc_realloc, .{ .name = "roc_realloc", .linkage = .Strong });
        @export(testing_roc_dealloc, .{ .name = "roc_dealloc", .linkage = .Strong });
        @export(testing_roc_panic, .{ .name = "roc_panic", .linkage = .Strong });
        @export(testing_roc_memcpy, .{ .name = "roc_memcpy", .linkage = .Strong });
    }
}

fn testing_roc_alloc(size: usize, _: u32) callconv(.C) ?*anyopaque {
    return @ptrCast(?*anyopaque, std.testing.allocator.alloc(u8, size) catch unreachable);
}

fn testing_roc_realloc(c_ptr: *anyopaque, new_size: usize, old_size: usize, _: u32) callconv(.C) ?*anyopaque {
    const ptr = @ptrCast([*]u8, @alignCast(2 * @alignOf(usize), c_ptr));
    const slice = ptr[0..old_size];

    return @ptrCast(?*anyopaque, std.testing.allocator.realloc(slice, new_size) catch unreachable);
}

fn testing_roc_dealloc(c_ptr: *anyopaque, _: u32) callconv(.C) void {
    const ptr = @ptrCast([*]u8, @alignCast(2 * @alignOf(usize), c_ptr));

    std.testing.allocator.destroy(ptr);
}

fn testing_roc_panic(c_ptr: *anyopaque, tag_id: u32) callconv(.C) void {
    _ = c_ptr;
    _ = tag_id;

    @panic("Roc panicked");
}

fn testing_roc_memcpy(dest: *anyopaque, src: *anyopaque, bytes: usize) callconv(.C) ?*anyopaque {
    const zig_dest = @ptrCast([*]u8, dest);
    const zig_src = @ptrCast([*]u8, src);

    @memcpy(zig_dest, zig_src, bytes);
    return dest;
}

pub fn alloc(size: usize, alignment: u32) ?[*]u8 {
    return @ptrCast(?[*]u8, @call(.{ .modifier = always_inline }, roc_alloc, .{ size, alignment }));
}

pub fn realloc(c_ptr: [*]u8, new_size: usize, old_size: usize, alignment: u32) [*]u8 {
    return @ptrCast([*]u8, @call(.{ .modifier = always_inline }, roc_realloc, .{ c_ptr, new_size, old_size, alignment }));
}

pub fn dealloc(c_ptr: [*]u8, alignment: u32) void {
    return @call(.{ .modifier = always_inline }, roc_dealloc, .{ c_ptr, alignment });
}

// must export this explicitly because right now it is not used from zig code
pub fn panic(c_ptr: *const anyopaque, alignment: u32) callconv(.C) void {
    return @call(.{ .modifier = always_inline }, roc_panic, .{ c_ptr, alignment });
}

pub fn memcpy(dst: [*]u8, src: [*]u8, size: usize) void {
    @call(.{ .modifier = always_inline }, roc_memcpy, .{ dst, src, size });
}

// indirection because otherwise zig creates an alias to the panic function which our LLVM code
// does not know how to deal with
pub fn test_panic(c_ptr: *anyopaque, alignment: u32) callconv(.C) void {
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
    U8 = 0,
    U16 = 1,
    U32 = 2,
    U64 = 3,
    U128 = 4,
    I8 = 5,
    I16 = 6,
    I32 = 7,
    I64 = 8,
    I128 = 9,
};

const Refcount = enum {
    none,
    normal,
    atomic,
};

const RC_TYPE = Refcount.normal;

pub fn increfC(ptr_to_refcount: *isize, amount: isize) callconv(.C) void {
    if (RC_TYPE == Refcount.none) return;
    // Ensure that the refcount is not whole program lifetime.
    if (ptr_to_refcount.* != REFCOUNT_MAX_ISIZE) {
        // Note: we assume that a refcount will never overflow.
        // As such, we do not need to cap incrementing.
        switch (RC_TYPE) {
            Refcount.normal => {
                ptr_to_refcount.* += amount;
            },
            Refcount.atomic => {
                _ = @atomicRmw(isize, ptr_to_refcount, std.builtin.AtomicRmwOp.Add, amount, Monotonic);
            },
            Refcount.none => unreachable,
        }
    }
}

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

    const isizes: [*]isize = @ptrCast([*]isize, @alignCast(@alignOf(isize), bytes));

    decref_ptr_to_refcount(isizes - 1, alignment);
}

inline fn decref_ptr_to_refcount(
    refcount_ptr: [*]isize,
    alignment: u32,
) void {
    if (RC_TYPE == Refcount.none) return;
    const extra_bytes = std.math.max(alignment, @sizeOf(usize));
    // Ensure that the refcount is not whole program lifetime.
    const refcount: isize = refcount_ptr[0];
    if (refcount != REFCOUNT_MAX_ISIZE) {
        switch (RC_TYPE) {
            Refcount.normal => {
                refcount_ptr[0] = refcount -% 1;
                if (refcount == REFCOUNT_ONE_ISIZE) {
                    dealloc(@ptrCast([*]u8, refcount_ptr) - (extra_bytes - @sizeOf(usize)), alignment);
                }
            },
            Refcount.atomic => {
                var last = @atomicRmw(isize, &refcount_ptr[0], std.builtin.AtomicRmwOp.Sub, 1, Monotonic);
                if (last == REFCOUNT_ONE_ISIZE) {
                    dealloc(@ptrCast([*]u8, refcount_ptr) - (extra_bytes - @sizeOf(usize)), alignment);
                }
            },
            Refcount.none => unreachable,
        }
    }
}

pub fn allocateWithRefcountC(
    data_bytes: usize,
    element_alignment: u32,
) callconv(.C) [*]u8 {
    return allocateWithRefcount(data_bytes, element_alignment);
}

pub fn allocateWithRefcount(
    data_bytes: usize,
    element_alignment: u32,
) [*]u8 {
    const ptr_width = @sizeOf(usize);
    const alignment = std.math.max(ptr_width, element_alignment);
    const length = alignment + data_bytes;

    var new_bytes: [*]u8 = alloc(length, alignment) orelse unreachable;

    const data_ptr = new_bytes + alignment;
    const refcount_ptr = @ptrCast([*]usize, @alignCast(ptr_width, data_ptr) - ptr_width);
    refcount_ptr[0] = if (RC_TYPE == Refcount.none) REFCOUNT_MAX_ISIZE else REFCOUNT_ONE;

    return data_ptr;
}

pub const CSlice = extern struct {
    pointer: *anyopaque,
    len: usize,
};

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

    if (old_width >= new_width) {
        return source_ptr;
    }

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
        const usizes: [*]usize = @ptrCast([*]usize, @alignCast(@alignOf(usize), self.bytes));

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

test "increfC, refcounted data" {
    var mock_rc: isize = REFCOUNT_ONE_ISIZE + 17;
    var ptr_to_refcount: *isize = &mock_rc;
    increfC(ptr_to_refcount, 2);
    try std.testing.expectEqual(mock_rc, REFCOUNT_ONE_ISIZE + 19);
}

test "increfC, static data" {
    var mock_rc: isize = REFCOUNT_MAX_ISIZE;
    var ptr_to_refcount: *isize = &mock_rc;
    increfC(ptr_to_refcount, 2);
    try std.testing.expectEqual(mock_rc, REFCOUNT_MAX_ISIZE);
}
