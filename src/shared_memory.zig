//! Shared memory reading functionality for the Roc runtime.
//! This module provides the _roc_entrypoint symbol that host applications can call
//! to get string data from POSIX shared memory.

const std = @import("std");
const c = std.c;

// External C functions for POSIX shared memory
extern "c" fn shm_open(name: [*:0]const u8, oflag: c_int, mode: c.mode_t) c_int;
extern "c" fn shm_unlink(name: [*:0]const u8) c_int;
extern "c" fn mmap(addr: ?*anyopaque, len: usize, prot: c_int, flags: c_int, fd: c_int, offset: c.off_t) ?*anyopaque;
extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;
extern "c" fn __error() *c_int; // macOS way to get errno

/// Exported symbol that reads a string from shared memory ROC_FILE_TO_INTERPRET
/// Returns a null-terminated C string to the caller
/// Expected format in shared memory: [usize length][u8... data]
export fn _roc_entrypoint() ?[*:0]u8 {
    const shm_name = "/ROC_FILE_TO_INTERPRET";

    // Open the shared memory object
    const shm_fd = shm_open(shm_name, 0, 0); // O_RDONLY = 0
    if (shm_fd < 0) {
        const errno = __error().*;
        std.debug.print("Failed to open shared memory: {s}, fd = {}, errno = {}\n", .{ shm_name, shm_fd, errno });
        return null;
    }
    defer _ = c.close(shm_fd);

    // Get the size of the shared memory object
    var stat_buf: c.Stat = undefined;
    if (c.fstat(shm_fd, &stat_buf) != 0) {
        std.debug.print("Failed to stat shared memory\n", .{});
        return null;
    }

    if (stat_buf.size < @sizeOf(usize)) {
        std.debug.print("Shared memory too small for length header\n", .{});
        return null;
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
        std.debug.print("Failed to map shared memory\n", .{});
        return null;
    };
    const mapped_memory = @as([*]u8, @ptrCast(mapped_ptr))[0..@intCast(stat_buf.size)];
    defer _ = munmap(mapped_ptr, @intCast(stat_buf.size));

    // Read the length from the beginning of shared memory
    const length_ptr: *align(1) const usize = @ptrCast(mapped_memory.ptr);
    const string_length = length_ptr.*;

    // Check if we have enough data
    if (stat_buf.size < @sizeOf(usize) + string_length) {
        std.debug.print("Shared memory too small for string data\n", .{});
        return null;
    }

    // Create a null-terminated string from the data
    const allocator = std.heap.c_allocator;
    const string_data = mapped_memory.ptr + @sizeOf(usize);
    const result = allocator.allocSentinel(u8, string_length, 0) catch |err| {
        std.debug.print("Failed to allocate result string: {}\n", .{err});
        return null;
    };

    @memcpy(result, string_data[0..string_length]);

    return result.ptr;
}
