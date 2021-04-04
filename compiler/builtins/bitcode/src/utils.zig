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

pub const Ordering = packed enum(u8) {
    EQ = 0,
    GT = 1,
    LT = 2,
};

// including this code because of fixes introduced in https://github.com/ziglang/zig/commit/d73f46b57c1c407bacd0daed8c69c4f14d14a06a
// Are we using a version of zig bigger than 0.7.1? Then shoud be removed and we can use the version from std.heap!

const mem = std.mem;
const assert = debug.assert;
const debug = std.debug;

fn sliceContainsPtr(container: []u8, ptr: [*]u8) bool {
    return @ptrToInt(ptr) >= @ptrToInt(container.ptr) and
        @ptrToInt(ptr) < (@ptrToInt(container.ptr) + container.len);
}

fn sliceContainsSlice(container: []u8, slice: []u8) bool {
    return @ptrToInt(slice.ptr) >= @ptrToInt(container.ptr) and
        (@ptrToInt(slice.ptr) + slice.len) <= (@ptrToInt(container.ptr) + container.len);
}

pub fn stackFallback(comptime size: usize, fallback_allocator: *Allocator) StackFallbackAllocator(size) {
    return StackFallbackAllocator(size){
        .buffer = undefined,
        .fallback_allocator = fallback_allocator,
        .fixed_buffer_allocator = undefined,
        .allocator = Allocator{
            .allocFn = StackFallbackAllocator(size).alloc,
            .resizeFn = StackFallbackAllocator(size).resize,
        },
    };
}

pub fn StackFallbackAllocator(comptime size: usize) type {
    return struct {
        const Self = @This();

        buffer: [size]u8,
        allocator: Allocator,
        fallback_allocator: *Allocator,
        fixed_buffer_allocator: FixedBufferAllocator,

        pub fn get(self: *Self) *Allocator {
            self.fixed_buffer_allocator = FixedBufferAllocator.init(self.buffer[0..]);
            return &self.allocator;
        }

        fn alloc(
            allocator: *Allocator,
            len: usize,
            ptr_align: u29,
            len_align: u29,
            return_address: usize,
        ) error{OutOfMemory}![]u8 {
            const self = @fieldParentPtr(Self, "allocator", allocator);
            return FixedBufferAllocator.alloc(&self.fixed_buffer_allocator.allocator, len, ptr_align, len_align, return_address) catch
                return self.fallback_allocator.allocFn(self.fallback_allocator, len, ptr_align, len_align, return_address);
        }

        fn resize(
            allocator: *Allocator,
            buf: []u8,
            buf_align: u29,
            new_len: usize,
            len_align: u29,
            return_address: usize,
        ) error{OutOfMemory}!usize {
            const self = @fieldParentPtr(Self, "allocator", allocator);
            if (self.fixed_buffer_allocator.ownsPtr(buf.ptr)) {
                return FixedBufferAllocator.resize(&self.fixed_buffer_allocator.allocator, buf, buf_align, new_len, len_align, return_address);
            } else {
                return self.fallback_allocator.resizeFn(self.fallback_allocator, buf, buf_align, new_len, len_align, return_address);
            }
        }
    };
}

pub const FixedBufferAllocator = struct {
    allocator: Allocator,
    end_index: usize,
    buffer: []u8,

    pub fn init(buffer: []u8) FixedBufferAllocator {
        return FixedBufferAllocator{
            .allocator = Allocator{
                .allocFn = alloc,
                .resizeFn = resize,
            },
            .buffer = buffer,
            .end_index = 0,
        };
    }

    pub fn ownsPtr(self: *FixedBufferAllocator, ptr: [*]u8) bool {
        return sliceContainsPtr(self.buffer, ptr);
    }

    pub fn ownsSlice(self: *FixedBufferAllocator, slice: []u8) bool {
        return sliceContainsSlice(self.buffer, slice);
    }

    /// NOTE: this will not work in all cases, if the last allocation had an adjusted_index
    ///       then we won't be able to determine what the last allocation was.  This is because
    ///       the alignForward operation done in alloc is not reverisible.
    pub fn isLastAllocation(self: *FixedBufferAllocator, buf: []u8) bool {
        return buf.ptr + buf.len == self.buffer.ptr + self.end_index;
    }

    fn alloc(allocator: *Allocator, n: usize, ptr_align: u29, len_align: u29, ra: usize) ![]u8 {
        const self = @fieldParentPtr(FixedBufferAllocator, "allocator", allocator);

        const stdout = std.io.getStdOut().writer();
        stdout.print("Hello, {d} {d}!\n", .{ @ptrToInt(self.buffer.ptr), self.end_index }) catch unreachable;

        const aligned_addr = mem.alignForward(@ptrToInt(self.buffer.ptr) + self.end_index, ptr_align);
        const adjusted_index = aligned_addr - @ptrToInt(self.buffer.ptr);
        const new_end_index = adjusted_index + n;
        if (new_end_index > self.buffer.len) {
            return error.OutOfMemory;
        }
        const result = self.buffer[adjusted_index..new_end_index];
        self.end_index = new_end_index;

        return result;
    }

    fn resize(
        allocator: *Allocator,
        buf: []u8,
        buf_align: u29,
        new_size: usize,
        len_align: u29,
        return_address: usize,
    ) Allocator.Error!usize {
        const self = @fieldParentPtr(FixedBufferAllocator, "allocator", allocator);
        assert(self.ownsSlice(buf)); // sanity check

        if (!self.isLastAllocation(buf)) {
            if (new_size > buf.len)
                return error.OutOfMemory;
            return if (new_size == 0) 0 else mem.alignAllocLen(buf.len, new_size, len_align);
        }

        if (new_size <= buf.len) {
            const sub = buf.len - new_size;
            self.end_index -= sub;
            return if (new_size == 0) 0 else mem.alignAllocLen(buf.len - sub, new_size, len_align);
        }

        const add = new_size - buf.len;
        if (add + self.end_index > self.buffer.len) {
            return error.OutOfMemory;
        }
        self.end_index += add;
        return new_size;
    }

    pub fn reset(self: *FixedBufferAllocator) void {
        self.end_index = 0;
    }
};
