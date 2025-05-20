const std = @import("std");
const builtin = @import("builtin");

const DEBUG_INCDEC = false;
const DEBUG_TESTING_ALLOC = false;
const DEBUG_ALLOC = false;

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

extern fn roc_dbg(loc: *anyopaque, message: *anyopaque, src: *anyopaque) callconv(.C) void;

// Since roc_dbg is never used by the builtins, we need at export a function that uses it to stop DCE.
pub fn test_dbg(loc: *anyopaque, src: *anyopaque, message: *anyopaque) callconv(.C) void {
    roc_dbg(loc, message, src);
}

extern fn kill(pid: c_int, sig: c_int) c_int;
extern fn shm_open(name: *const i8, oflag: c_int, mode: c_uint) c_int;
extern fn mmap(addr: ?*anyopaque, length: c_uint, prot: c_int, flags: c_int, fd: c_int, offset: c_uint) *anyopaque;
extern fn getppid() c_int;

fn testing_roc_getppid() callconv(.C) c_int {
    return getppid();
}

fn roc_getppid_windows_stub() callconv(.C) c_int {
    return 0;
}

fn testing_roc_shm_open(name: *const i8, oflag: c_int, mode: c_uint) callconv(.C) c_int {
    return shm_open(name, oflag, mode);
}
fn testing_roc_mmap(addr: ?*anyopaque, length: c_uint, prot: c_int, flags: c_int, fd: c_int, offset: c_uint) callconv(.C) *anyopaque {
    return mmap(addr, length, prot, flags, fd, offset);
}

fn testing_roc_dbg(loc: *anyopaque, message: *anyopaque, src: *anyopaque) callconv(.C) void {
    _ = message;
    _ = src;
    _ = loc;
}

comptime {
    // During tests, use the testing allocators to satisfy these functions.
    if (builtin.is_test) {
        @export(&testing_roc_alloc, .{ .name = "roc_alloc", .linkage = .strong });
        @export(&testing_roc_realloc, .{ .name = "roc_realloc", .linkage = .strong });
        @export(&testing_roc_dealloc, .{ .name = "roc_dealloc", .linkage = .strong });
        @export(&testing_roc_panic, .{ .name = "roc_panic", .linkage = .strong });
        @export(&testing_roc_dbg, .{ .name = "roc_dbg", .linkage = .strong });

        if (builtin.os.tag == .macos or builtin.os.tag == .linux) {
            @export(&testing_roc_getppid, .{ .name = "roc_getppid", .linkage = .strong });
            @export(&testing_roc_mmap, .{ .name = "roc_mmap", .linkage = .strong });
            @export(&testing_roc_shm_open, .{ .name = "roc_shm_open", .linkage = .strong });
        }

        if (builtin.os.tag == .windows) {
            @export(&roc_getppid_windows_stub, .{ .name = "roc_getppid", .linkage = .strong });
        }
    }
}

fn testing_roc_alloc(size: usize, nominal_alignment: u32) callconv(.C) ?*anyopaque {
    const real_alignment = 16;
    if (nominal_alignment > real_alignment) {
        @panic("alignments larger than that of 2 usize are not currently supported");
    }
    // We store an extra usize which is the size of the data plus the size of the size, directly before the data.
    // We need enough clocks of the alignment size to fit this (usually this will be one)
    const size_of_size = @sizeOf(usize);
    const alignments_needed = size_of_size / real_alignment + comptime if (size_of_size % real_alignment == 0) 0 else 1;
    const extra_bytes = alignments_needed * size_of_size;

    const full_size = size + extra_bytes;
    const whole_ptr = (std.testing.allocator.alignedAlloc(u8, real_alignment, full_size) catch unreachable).ptr;
    const written_to_size = size + size_of_size;
    @as([*]align(real_alignment) usize, @ptrCast(whole_ptr))[extra_bytes - size_of_size] = written_to_size;

    const data_ptr = @as(?*anyopaque, @ptrCast(whole_ptr + extra_bytes));

    if (DEBUG_TESTING_ALLOC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("+ alloc {*}: {} bytes\n", .{ data_ptr, size });
    }

    return data_ptr;
}

fn testing_roc_realloc(c_ptr: *anyopaque, new_size: usize, old_size: usize, nominal_alignment: u32) callconv(.C) ?*anyopaque {
    const real_alignment = 16;
    if (nominal_alignment > real_alignment) {
        @panic("alignments larger than that of 2 usize are not currently supported");
    }
    const raw_ptr = @as([*]align(real_alignment) u8, @alignCast(@as([*]u8, @ptrCast(c_ptr)) - @sizeOf(usize)));
    const slice = raw_ptr[0..(old_size + @sizeOf(usize))];

    const new_full_size = new_size + @sizeOf(usize);
    var new_raw_ptr = @as([*]u8, @alignCast((std.testing.allocator.realloc(slice, new_full_size) catch unreachable).ptr));
    @as([*]usize, @alignCast(@ptrCast(new_raw_ptr)))[0] = new_full_size;
    new_raw_ptr += @sizeOf(usize);
    const new_ptr = @as(?*anyopaque, @ptrCast(new_raw_ptr));

    if (DEBUG_TESTING_ALLOC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("- realloc {*}\n", .{new_ptr});
    }

    return new_ptr;
}

fn testing_roc_dealloc(c_ptr: *anyopaque, _: u32) callconv(.C) void {
    const alignment = 16;
    const size_of_size = @sizeOf(usize);
    const alignments_needed = size_of_size / alignment + comptime if (size_of_size % alignment == 0) 0 else 1;
    const extra_bytes = alignments_needed * size_of_size;
    const byte_array = @as([*]u8, @ptrCast(c_ptr)) - extra_bytes;
    const allocation_ptr = @as([*]align(alignment) u8, @alignCast(byte_array));
    const offset_from_allocation_to_size = extra_bytes - size_of_size;
    const size_of_data_and_size = @as([*]usize, @alignCast(@ptrCast(allocation_ptr)))[offset_from_allocation_to_size];
    const full_size = size_of_data_and_size + offset_from_allocation_to_size;
    const slice = allocation_ptr[0..full_size];

    if (DEBUG_TESTING_ALLOC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("💀 dealloc {*}\n", .{slice.ptr});
    }

    std.testing.allocator.free(slice);
}

fn testing_roc_panic(c_ptr: *anyopaque, tag_id: u32) callconv(.C) void {
    _ = c_ptr;
    _ = tag_id;

    @panic("Roc panicked");
}

pub fn alloc(size: usize, alignment: u32) ?[*]u8 {
    return @as(?[*]u8, @ptrCast(roc_alloc(size, alignment)));
}

pub fn realloc(c_ptr: [*]u8, new_size: usize, old_size: usize, alignment: u32) [*]u8 {
    if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("- realloc {*}\n", .{c_ptr});
    }
    return @as([*]u8, @ptrCast(roc_realloc(c_ptr, new_size, old_size, alignment)));
}

pub fn dealloc(c_ptr: [*]u8, alignment: u32) void {
    return roc_dealloc(c_ptr, alignment);
}

// indirection because otherwise zig creates an alias to the panic function which our LLVM code
// does not know how to deal with
pub fn test_panic(c_ptr: *anyopaque, crash_tag: u32) callconv(.C) void {
    _ = c_ptr;
    _ = crash_tag;

    //    const cstr = @ptrCast([*:0]u8, c_ptr);
    //
    //    const stderr = std.io.getStdErr().writer();
    //    stderr.print("Roc panicked: {s}!\n", .{cstr}) catch unreachable;
    //
    //    std.c.exit(1);
}

pub const Inc = fn (?[*]u8) callconv(.C) void;
pub const IncN = fn (?[*]u8, u64) callconv(.C) void;
pub const Dec = fn (?[*]u8) callconv(.C) void;

const REFCOUNT_MAX_ISIZE: isize = 0;

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

const RC_TYPE: Refcount = .atomic;

pub fn increfRcPtrC(ptr_to_refcount: *isize, amount: isize) callconv(.C) void {
    if (RC_TYPE == .none) return;

    if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("| increment {*}: ", .{ptr_to_refcount});
    }

    // Ensure that the refcount is not whole program lifetime.
    const refcount: isize = ptr_to_refcount.*;
    if (!rcConstant(refcount)) {
        // Note: we assume that a refcount will never overflow.
        // As such, we do not need to cap incrementing.
        switch (RC_TYPE) {
            .normal => {
                if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
                    const old = @as(usize, @bitCast(refcount));
                    const new = old + @as(usize, @intCast(amount));

                    std.debug.print("{} + {} = {}!\n", .{ old, amount, new });
                }

                ptr_to_refcount.* = refcount +% amount;
            },
            .atomic => {
                _ = @atomicRmw(isize, ptr_to_refcount, .Add, amount, .monotonic);
            },
            .none => unreachable,
        }
    }
}

pub fn decrefRcPtrC(
    bytes_or_null: ?[*]isize,
    alignment: u32,
    elements_refcounted: bool,
) callconv(.C) void {
    // IMPORTANT: bytes_or_null is this case is expected to be a pointer to the refcount
    // (NOT the start of the data, or the start of the allocation)

    // this is of course unsafe, but we trust what we get from the llvm side
    const bytes = @as([*]isize, @ptrCast(bytes_or_null));

    return @call(.always_inline, decref_ptr_to_refcount, .{ bytes, alignment, elements_refcounted });
}

pub fn decrefCheckNullC(
    bytes_or_null: ?[*]u8,
    alignment: u32,
    elements_refcounted: bool,
) callconv(.C) void {
    if (bytes_or_null) |bytes| {
        const isizes: [*]isize = @as([*]isize, @ptrCast(@alignCast(bytes)));
        return @call(.always_inline, decref_ptr_to_refcount, .{ isizes - 1, alignment, elements_refcounted });
    }
}

pub fn decrefDataPtrC(
    bytes_or_null: ?[*]u8,
    alignment: u32,
    elements_refcounted: bool,
) callconv(.C) void {
    const bytes = bytes_or_null orelse return;

    const data_ptr = @intFromPtr(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const unmasked_ptr = data_ptr & ~tag_mask;

    const isizes: [*]isize = @as([*]isize, @ptrFromInt(unmasked_ptr));
    const rc_ptr = isizes - 1;

    return decrefRcPtrC(rc_ptr, alignment, elements_refcounted);
}

pub fn increfDataPtrC(
    bytes_or_null: ?[*]u8,
    inc_amount: isize,
) callconv(.C) void {
    const bytes = bytes_or_null orelse return;

    const ptr = @intFromPtr(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const masked_ptr = ptr & ~tag_mask;

    const isizes: *isize = @as(*isize, @ptrFromInt(masked_ptr - @sizeOf(usize)));

    return increfRcPtrC(isizes, inc_amount);
}

pub fn freeDataPtrC(
    bytes_or_null: ?[*]u8,
    alignment: u32,
    elements_refcounted: bool,
) callconv(.C) void {
    const bytes = bytes_or_null orelse return;

    const ptr = @intFromPtr(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const masked_ptr = ptr & ~tag_mask;

    const isizes: [*]isize = @as([*]isize, @ptrFromInt(masked_ptr));

    // we always store the refcount right before the data
    return freeRcPtrC(isizes - 1, alignment, elements_refcounted);
}

pub fn freeRcPtrC(
    bytes_or_null: ?[*]isize,
    alignment: u32,
    elements_refcounted: bool,
) callconv(.C) void {
    const bytes = bytes_or_null orelse return;
    return free_ptr_to_refcount(bytes, alignment, elements_refcounted);
}

pub fn decref(
    bytes_or_null: ?[*]u8,
    data_bytes: usize,
    alignment: u32,
    elements_refcounted: bool,
) void {
    if (data_bytes == 0) {
        return;
    }

    const bytes = bytes_or_null orelse return;

    const isizes: [*]isize = @as([*]isize, @ptrCast(@alignCast(bytes)));

    decref_ptr_to_refcount(isizes - 1, alignment, elements_refcounted);
}

inline fn free_ptr_to_refcount(
    refcount_ptr: [*]isize,
    alignment: u32,
    elements_refcounted: bool,
) void {
    if (RC_TYPE == .none) return;
    const ptr_width = @sizeOf(usize);
    const required_space: usize = if (elements_refcounted) (2 * ptr_width) else ptr_width;
    const extra_bytes = @max(required_space, alignment);
    const allocation_ptr = @as([*]u8, @ptrCast(refcount_ptr)) - (extra_bytes - @sizeOf(usize));

    // NOTE: we don't even check whether the refcount is "infinity" here!
    dealloc(allocation_ptr, alignment);

    if (DEBUG_ALLOC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("💀 freed {*}\n", .{allocation_ptr});
    }
}

inline fn decref_ptr_to_refcount(
    refcount_ptr: [*]isize,
    element_alignment: u32,
    elements_refcounted: bool,
) void {
    if (RC_TYPE == .none) return;

    if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("| decrement {*}: ", .{refcount_ptr});
    }

    // Due to RC alignmen tmust take into acount pointer size.
    const ptr_width = @sizeOf(usize);
    const alignment = @max(ptr_width, element_alignment);

    // Ensure that the refcount is not whole program lifetime.
    const refcount: isize = refcount_ptr[0];
    if (!rcConstant(refcount)) {
        switch (RC_TYPE) {
            .normal => {
                if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
                    const old = @as(usize, @bitCast(refcount));
                    const new = @as(usize, @bitCast(refcount_ptr[0] -% 1));

                    std.debug.print("{} - 1 = {}!\n", .{ old, new });
                }

                refcount_ptr[0] = refcount -% 1;
                if (refcount == 1) {
                    free_ptr_to_refcount(refcount_ptr, alignment, elements_refcounted);
                }
            },
            .atomic => {
                const last = @atomicRmw(isize, &refcount_ptr[0], .Sub, 1, .monotonic);
                if (last == 1) {
                    free_ptr_to_refcount(refcount_ptr, alignment, elements_refcounted);
                }
            },
            .none => unreachable,
        }
    }
}

pub fn isUnique(
    bytes_or_null: ?[*]u8,
) callconv(.C) bool {
    const bytes = bytes_or_null orelse return true;

    const ptr = @intFromPtr(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const masked_ptr = ptr & ~tag_mask;

    const isizes: [*]isize = @as([*]isize, @ptrFromInt(masked_ptr));

    const refcount = (isizes - 1)[0];

    if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("| is unique {*}\n", .{isizes - 1});
    }

    return rcUnique(refcount);
}

pub inline fn rcUnique(refcount: isize) bool {
    switch (RC_TYPE) {
        .normal => {
            return refcount == 1;
        },
        .atomic => {
            return refcount == 1;
        },
        .none => {
            return false;
        },
    }
}

pub inline fn rcConstant(refcount: isize) bool {
    switch (RC_TYPE) {
        .normal => {
            return refcount == REFCOUNT_MAX_ISIZE;
        },
        .atomic => {
            return refcount == REFCOUNT_MAX_ISIZE;
        },
        .none => {
            return true;
        },
    }
}

// We follow roughly the [fbvector](https://github.com/facebook/folly/blob/main/folly/docs/FBVector.md) when it comes to growing a RocList.
// Here is [their growth strategy](https://github.com/facebook/folly/blob/3e0525988fd444201b19b76b390a5927c15cb697/folly/FBVector.h#L1128) for push_back:
//
// (1) initial size
//     Instead of growing to size 1 from empty, fbvector allocates at least
//     64 bytes. You may still use reserve to reserve a lesser amount of
//     memory.
// (2) 1.5x
//     For medium-sized vectors, the growth strategy is 1.5x. See the docs
//     for details.
//     This does not apply to very small or very large fbvectors. This is a
//     heuristic.
//
// In our case, we exposed allocate and reallocate, which will use a smart growth stategy.
// We also expose allocateExact and reallocateExact for case where a specific number of elements is requested.

// calculateCapacity should only be called in cases the list will be growing.
// requested_length should always be greater than old_capacity.
pub inline fn calculateCapacity(
    old_capacity: usize,
    requested_length: usize,
    element_width: usize,
) usize {
    // TODO: Deal with the fact we allocate an extra u64 for refcount.
    // This may lead to allocating page size + 8 bytes.
    // That could mean allocating an entire page for 8 bytes of data which isn't great.

    if (requested_length != old_capacity + 1) {
        // The user is explicitly requesting n elements.
        // Trust the user and just reserve that amount.
        return requested_length;
    }

    var new_capacity: usize = 0;
    if (element_width == 0) {
        return requested_length;
    } else if (old_capacity == 0) {
        new_capacity = 64 / element_width;
    } else if (old_capacity < 4096 / element_width) {
        new_capacity = old_capacity * 2;
    } else if (old_capacity > 4096 * 32 / element_width) {
        new_capacity = old_capacity * 2;
    } else {
        new_capacity = (old_capacity * 3 + 1) / 2;
    }

    return @max(new_capacity, requested_length);
}

pub fn allocateWithRefcountC(
    data_bytes: usize,
    element_alignment: u32,
    elements_refcounted: bool,
) callconv(.C) [*]u8 {
    return allocateWithRefcount(data_bytes, element_alignment, elements_refcounted);
}

pub fn allocateWithRefcount(
    data_bytes: usize,
    element_alignment: u32,
    elements_refcounted: bool,
) [*]u8 {
    // If the element type is refcounted, we need to also allocate space to store the element count on the heap.
    // This is used so that a seamless slice can de-allocate the underlying list type.
    const ptr_width = @sizeOf(usize);
    const alignment = @max(ptr_width, element_alignment);
    const required_space: usize = if (elements_refcounted) (2 * ptr_width) else ptr_width;
    const extra_bytes = @max(required_space, element_alignment);
    const length = extra_bytes + data_bytes;

    const new_bytes: [*]u8 = alloc(length, alignment) orelse unreachable;

    if (DEBUG_ALLOC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("+ allocated {*} ({} bytes with alignment {})\n", .{ new_bytes, data_bytes, alignment });
    }

    const data_ptr = new_bytes + extra_bytes;
    const refcount_ptr = @as([*]usize, @ptrCast(@as([*]align(ptr_width) u8, @alignCast(data_ptr)) - ptr_width));
    refcount_ptr[0] = if (RC_TYPE == .none) REFCOUNT_MAX_ISIZE else 1;

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
    elements_refcounted: bool,
) [*]u8 {
    const ptr_width: usize = @sizeOf(usize);
    const required_space: usize = if (elements_refcounted) (2 * ptr_width) else ptr_width;
    const extra_bytes = @max(required_space, alignment);

    const old_width = extra_bytes + old_length * element_width;
    const new_width = extra_bytes + new_length * element_width;

    if (old_width >= new_width) {
        return source_ptr;
    }

    // TODO handle out of memory
    // NOTE realloc will dealloc the original allocation
    const old_allocation = source_ptr - extra_bytes;
    const new_allocation = realloc(old_allocation, new_width, old_width, alignment);

    const new_source = @as([*]u8, @ptrCast(new_allocation)) + extra_bytes;
    return new_source;
}

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
    var mock_rc: isize = 17;
    const ptr_to_refcount: *isize = &mock_rc;
    increfRcPtrC(ptr_to_refcount, 2);
    try std.testing.expectEqual(mock_rc, 19);
}

test "increfC, static data" {
    var mock_rc: isize = REFCOUNT_MAX_ISIZE;
    const ptr_to_refcount: *isize = &mock_rc;
    increfRcPtrC(ptr_to_refcount, 2);
    try std.testing.expectEqual(mock_rc, REFCOUNT_MAX_ISIZE);
}

// This returns a compilation dependent pseudo random seed for dictionaries.
// The seed is the address of this function.
// This avoids all roc Dicts using a known seed and being trivial to DOS.
// Still not as secure as true random, but a lot better.
// This value must not change between calls unless Dict is changed to store the seed on creation.
// Note: On esstentially all OSes, this will be affected by ASLR and different each run.
// In wasm, the value will be constant to the build as a whole.
// Either way, it can not be know by an attacker unless they get access to the executable.
pub fn dictPseudoSeed() callconv(.C) u64 {
    return @as(u64, @intCast(@intFromPtr(&dictPseudoSeed)));
}
