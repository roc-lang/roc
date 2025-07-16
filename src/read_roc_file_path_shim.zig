//! A shim to read the filepath for our app.roc from shared memory for the interpreter

const std = @import("std");
const c = std.c;
// Minimal RocStr implementation matching the main str.zig
const RocStr = extern struct {
    bytes: ?[*]u8,
    length: usize,
    capacity_or_alloc_ptr: usize,

    const SMALL_STRING_SIZE = @sizeOf(RocStr);
    const MASK_ISIZE: isize = std.math.minInt(isize);
    const MASK: usize = @as(usize, @bitCast(MASK_ISIZE));

    pub inline fn empty() RocStr {
        return RocStr{
            .length = 0,
            .bytes = null,
            .capacity_or_alloc_ptr = MASK,
        };
    }

    pub fn isSmallStr(self: RocStr) bool {
        return @as(isize, @bitCast(self.capacity_or_alloc_ptr)) < 0;
    }

    fn asArray(self: RocStr) [@sizeOf(RocStr)]u8 {
        const as_ptr = @as([*]const u8, @ptrCast(&self));
        const slice = as_ptr[0..@sizeOf(RocStr)];
        return slice.*;
    }

    pub fn asU8ptrMut(self: *RocStr) [*]u8 {
        if (self.isSmallStr()) {
            return @as([*]u8, @ptrCast(self));
        } else {
            return @as([*]u8, @ptrCast(self.bytes));
        }
    }

    pub fn setLen(self: *RocStr, length: usize) void {
        if (self.isSmallStr()) {
            self.asU8ptrMut()[@sizeOf(RocStr) - 1] = @as(u8, @intCast(length)) | 0b1000_0000;
        } else {
            self.length = length;
        }
    }
};

// External C functions for POSIX shared memory
extern "c" fn shm_open(name: [*:0]const u8, oflag: c_int, mode: c.mode_t) c_int;
extern "c" fn shm_unlink(name: [*:0]const u8) c_int;
extern "c" fn mmap(addr: ?*anyopaque, len: usize, prot: c_int, flags: c_int, fd: c_int, offset: c.off_t) ?*anyopaque;
extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;
extern "c" fn __error() *c_int; // macOS way to get errno

/// Exported symbol that reads a string from shared memory ROC_FILE_TO_INTERPRET
/// Returns a RocStr to the caller
/// Expected format in shared memory: [usize length][u8... data]
export fn roc_entrypoint(roc_alloc: *const fn (size: usize, alignment: u32) callconv(.C) ?*anyopaque) RocStr {
    const shm_name = "/ROC_FILE_TO_INTERPRET";

    // Open the shared memory object
    const shm_fd = shm_open(shm_name, 0, 0); // O_RDONLY = 0
    if (shm_fd < 0) {
        return RocStr.empty();
    }
    defer _ = c.close(shm_fd);

    // Get shared memory size
    var stat_buf: c.Stat = undefined;
    if (c.fstat(shm_fd, &stat_buf) != 0) {
        return RocStr.empty();
    }

    if (stat_buf.size < @sizeOf(usize)) {
        std.debug.print("Shared memory too small for length header\n", .{});
        return RocStr.empty();
    }

    // Map the shared memory
    const mapped_ptr = mmap(
        null,
        @intCast(stat_buf.size),
        0x01, // PROT_READ
        0x0001, // MAP_SHARED
        shm_fd,
        0,
    ) orelse {
        return RocStr.empty();
    };
    const mapped_memory = @as([*]u8, @ptrCast(mapped_ptr))[0..@intCast(stat_buf.size)];
    defer _ = munmap(mapped_ptr, @intCast(stat_buf.size));

    // Read the length from the beginning of shared memory
    const length_ptr: *align(1) const usize = @ptrCast(mapped_memory.ptr);
    const string_length = length_ptr.*;

    // Check if we have enough data
    if (stat_buf.size < @sizeOf(usize) + string_length) {
        return RocStr.empty();
    }

    // Create a RocStr from the data
    const string_data = mapped_memory.ptr + @sizeOf(usize);

    // For small strings, we can create them directly
    if (string_length <= @sizeOf(RocStr) - 1) {
        var result = RocStr.empty();
        @memcpy(result.asU8ptrMut()[0..string_length], string_data[0..string_length]);
        result.setLen(string_length);
        return result;
    }

    // For larger strings, allocate memory using roc_alloc
    const alignment = @alignOf(usize);
    const alloc_ptr = roc_alloc(string_length, alignment) orelse {
        return RocStr.empty();
    };

    // Create a RocStr with the allocated memory
    const result = RocStr{
        .bytes = @ptrCast(alloc_ptr),
        .length = string_length,
        .capacity_or_alloc_ptr = string_length,
    };

    // Copy the data into the allocated memory
    @memcpy(@as([*]u8, @ptrCast(alloc_ptr)), string_data[0..string_length]);

    return result;
}
