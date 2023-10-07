const std = @import("std");
const builtin = @import("builtin");
const always_inline = std.builtin.CallOptions.Modifier.always_inline;
const Monotonic = std.builtin.AtomicOrder.Monotonic;

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

comptime {
    // During tests, use the testing allocators to satisfy these functions.
    if (builtin.is_test) {
        @export(testing_roc_alloc, .{ .name = "roc_alloc", .linkage = .Strong });
        @export(testing_roc_realloc, .{ .name = "roc_realloc", .linkage = .Strong });
        @export(testing_roc_dealloc, .{ .name = "roc_dealloc", .linkage = .Strong });
        @export(testing_roc_panic, .{ .name = "roc_panic", .linkage = .Strong });

        if (builtin.os.tag == .macos or builtin.os.tag == .linux) {
            @export(testing_roc_getppid, .{ .name = "roc_getppid", .linkage = .Strong });
            @export(testing_roc_mmap, .{ .name = "roc_mmap", .linkage = .Strong });
            @export(testing_roc_shm_open, .{ .name = "roc_shm_open", .linkage = .Strong });
        }

        if (builtin.os.tag == .windows) {
            @export(roc_getppid_windows_stub, .{ .name = "roc_getppid", .linkage = .Strong });
        }
    }
}

fn testing_roc_alloc(size: usize, _: u32) callconv(.C) ?*anyopaque {
    const ptr = @ptrCast(?*anyopaque, std.testing.allocator.alloc(u8, size) catch unreachable);

    if (DEBUG_TESTING_ALLOC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("+ alloc {*}: {} bytes\n", .{ ptr, size });
    }

    return ptr;
}

fn testing_roc_realloc(c_ptr: *anyopaque, new_size: usize, old_size: usize, _: u32) callconv(.C) ?*anyopaque {
    const ptr = @ptrCast([*]u8, @alignCast(2 * @alignOf(usize), c_ptr));
    const slice = ptr[0..old_size];

    const new = @ptrCast(?*anyopaque, std.testing.allocator.realloc(slice, new_size) catch unreachable);

    if (DEBUG_TESTING_ALLOC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("- realloc {*}\n", .{new});
    }

    return new;
}

fn testing_roc_dealloc(c_ptr: *anyopaque, _: u32) callconv(.C) void {
    const ptr = @ptrCast([*]u8, @alignCast(2 * @alignOf(usize), c_ptr));

    if (DEBUG_TESTING_ALLOC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("💀 dealloc {*}\n", .{ptr});
    }

    std.testing.allocator.destroy(ptr);
}

fn testing_roc_panic(c_ptr: *anyopaque, tag_id: u32) callconv(.C) void {
    _ = c_ptr;
    _ = tag_id;

    @panic("Roc panicked");
}

pub fn alloc(size: usize, alignment: u32) ?[*]u8 {
    return @ptrCast(?[*]u8, roc_alloc(size, alignment));
}

pub fn realloc(c_ptr: [*]u8, new_size: usize, old_size: usize, alignment: u32) [*]u8 {
    if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("- realloc {*}\n", .{c_ptr});
    }
    return @ptrCast([*]u8, roc_realloc(c_ptr, new_size, old_size, alignment));
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

pub fn increfRcPtrC(ptr_to_refcount: *isize, amount: isize) callconv(.C) void {
    if (RC_TYPE == Refcount.none) return;

    if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("| increment {*}: ", .{ptr_to_refcount});
    }

    // Ensure that the refcount is not whole program lifetime.
    if (ptr_to_refcount.* != REFCOUNT_MAX_ISIZE) {
        // Note: we assume that a refcount will never overflow.
        // As such, we do not need to cap incrementing.
        switch (RC_TYPE) {
            Refcount.normal => {
                if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
                    const old = @bitCast(usize, ptr_to_refcount.*);
                    const new = old + @intCast(usize, amount);

                    const oldH = old - REFCOUNT_ONE + 1;
                    const newH = new - REFCOUNT_ONE + 1;

                    std.debug.print("{} + {} = {}!\n", .{ oldH, amount, newH });
                }

                ptr_to_refcount.* += amount;
            },
            Refcount.atomic => {
                _ = @atomicRmw(isize, ptr_to_refcount, std.builtin.AtomicRmwOp.Add, amount, Monotonic);
            },
            Refcount.none => unreachable,
        }
    }
}

pub fn decrefRcPtrC(
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

pub fn decrefDataPtrC(
    bytes_or_null: ?[*]isize,
    alignment: u32,
) callconv(.C) void {
    var bytes = bytes_or_null orelse return;

    const data_ptr = @ptrToInt(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const unmasked_ptr = data_ptr & ~tag_mask;

    const isizes: [*]isize = @intToPtr([*]isize, unmasked_ptr);
    const rc_ptr = isizes - 1;

    return decrefRcPtrC(rc_ptr, alignment);
}

pub fn increfDataPtrC(
    bytes_or_null: ?[*]isize,
    inc_amount: isize,
) callconv(.C) void {
    var bytes = bytes_or_null orelse return;

    const ptr = @ptrToInt(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const masked_ptr = ptr & ~tag_mask;

    const isizes: *isize = @intToPtr(*isize, masked_ptr - @sizeOf(usize));

    return increfRcPtrC(isizes, inc_amount);
}

pub fn freeDataPtrC(
    bytes_or_null: ?[*]isize,
    alignment: u32,
) callconv(.C) void {
    var bytes = bytes_or_null orelse return;

    const ptr = @ptrToInt(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const masked_ptr = ptr & ~tag_mask;

    const isizes: [*]isize = @intToPtr([*]isize, masked_ptr);

    return freeRcPtrC(isizes - 1, alignment);
}

pub fn freeRcPtrC(
    bytes_or_null: ?[*]isize,
    alignment: u32,
) callconv(.C) void {
    var bytes = bytes_or_null orelse return;
    return free_ptr_to_refcount(bytes, alignment);
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

inline fn free_ptr_to_refcount(
    refcount_ptr: [*]isize,
    alignment: u32,
) void {
    if (RC_TYPE == Refcount.none) return;
    const extra_bytes = std.math.max(alignment, @sizeOf(usize));
    const allocation_ptr = @ptrCast([*]u8, refcount_ptr) - (extra_bytes - @sizeOf(usize));

    // NOTE: we don't even check whether the refcount is "infinity" here!
    dealloc(allocation_ptr, alignment);

    if (DEBUG_ALLOC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("💀 freed {*}\n", .{allocation_ptr});
    }
}

inline fn decref_ptr_to_refcount(
    refcount_ptr: [*]isize,
    alignment: u32,
) void {
    if (RC_TYPE == Refcount.none) return;

    if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("| decrement {*}: ", .{refcount_ptr});
    }

    // Ensure that the refcount is not whole program lifetime.
    const refcount: isize = refcount_ptr[0];
    if (refcount != REFCOUNT_MAX_ISIZE) {
        switch (RC_TYPE) {
            Refcount.normal => {
                const old = @bitCast(usize, refcount);
                refcount_ptr[0] = refcount -% 1;
                const new = @bitCast(usize, refcount -% 1);

                if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
                    const oldH = old - REFCOUNT_ONE + 1;
                    const newH = new - REFCOUNT_ONE + 1;

                    std.debug.print("{} - 1 = {}!\n", .{ oldH, newH });
                }

                if (refcount == REFCOUNT_ONE_ISIZE) {
                    free_ptr_to_refcount(refcount_ptr, alignment);
                }
            },
            Refcount.atomic => {
                var last = @atomicRmw(isize, &refcount_ptr[0], std.builtin.AtomicRmwOp.Sub, 1, Monotonic);
                if (last == REFCOUNT_ONE_ISIZE) {
                    free_ptr_to_refcount(refcount_ptr, alignment);
                }
            },
            Refcount.none => unreachable,
        }
    }
}

pub fn isUnique(
    bytes_or_null: ?[*]u8,
) callconv(.C) bool {
    var bytes = bytes_or_null orelse return true;

    const ptr = @ptrToInt(bytes);
    const tag_mask: usize = if (@sizeOf(usize) == 8) 0b111 else 0b11;
    const masked_ptr = ptr & ~tag_mask;

    const isizes: [*]isize = @intToPtr([*]isize, masked_ptr);

    const refcount = (isizes - 1)[0];

    if (DEBUG_INCDEC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("| is unique {*}\n", .{isizes - 1});
    }

    return refcount == REFCOUNT_ONE_ISIZE;
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
    // TODO: there are two adjustments that would likely lead to better results for Roc.
    // 1. Deal with the fact we allocate an extra u64 for refcount.
    //    This may lead to allocating page size + 8 bytes.
    //    That could mean allocating an entire page for 8 bytes of data which isn't great.
    // 2. Deal with the fact that we can request more than 1 element at a time.
    //    fbvector assumes just appending 1 element at a time when using this algorithm.
    //    As such, they will generally grow in a way that should better match certain memory multiple.
    //    This is also the normal case for roc, but we could also grow by a much larger amount.
    //    We may want to round to multiples of 2 or something similar.
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

    return std.math.max(new_capacity, requested_length);
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

    if (DEBUG_ALLOC and builtin.target.cpu.arch != .wasm32) {
        std.debug.print("+ allocated {*} ({} bytes with alignment {})\n", .{ new_bytes, data_bytes, alignment });
    }

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
    increfRcPtrC(ptr_to_refcount, 2);
    try std.testing.expectEqual(mock_rc, REFCOUNT_ONE_ISIZE + 19);
}

test "increfC, static data" {
    var mock_rc: isize = REFCOUNT_MAX_ISIZE;
    var ptr_to_refcount: *isize = &mock_rc;
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
    return @intCast(u64, @ptrToInt(dictPseudoSeed));
}
