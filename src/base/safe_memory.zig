//! Memory safety utilities for bounds checking and safe memory operations
//! Provides helpers to prevent buffer overflows and memory corruption

const std = @import("std");

/// Memory safety error types
pub const MemoryError = error{
    BufferOverflow,
    InvalidOffset,
    NullPointer,
};

/// Safely copy memory with bounds checking
pub fn safeCopy(dst: []u8, src: []const u8, total_size: usize) MemoryError!void {
    if (src.len > total_size) {
        return error.BufferOverflow;
    }
    if (dst.len < src.len) {
        return error.BufferOverflow;
    }
    @memcpy(dst[0..src.len], src);
}

/// Safely get a slice from a pointer with bounds checking
pub fn safeSlice(ptr: ?*anyopaque, offset: usize, length: usize, total_size: usize) MemoryError![]u8 {
    if (ptr == null) {
        return error.NullPointer;
    }
    if (offset >= total_size) {
        return error.InvalidOffset;
    }
    if (offset + length > total_size) {
        return error.BufferOverflow;
    }
    const base_ptr = @as([*]u8, @ptrCast(ptr.?));
    return base_ptr[offset .. offset + length];
}

/// Safely copy argument data with bounds checking
/// For RocStr (24-byte) arguments, performs a simple copy since strings should be handled
/// by the caller using appropriate RocStr methods when needed
pub fn safeCopyArgument(
    arg_ptr: ?*anyopaque,
    dest_ptr: ?*anyopaque,
    elem_offset: usize,
    elem_size: usize,
    max_arg_size: usize,
) MemoryError!void {
    if (arg_ptr == null or dest_ptr == null) {
        return error.NullPointer;
    }

    if (elem_offset + elem_size > max_arg_size) {
        std.log.err("Argument copy would overflow: offset={}, size={}, max={}", .{ elem_offset, elem_size, max_arg_size });
        return error.BufferOverflow;
    }

    if (elem_size > 0) {
        const src_slice = try safeSlice(arg_ptr, elem_offset, elem_size, max_arg_size);
        const dst_slice = (@as([*]u8, @ptrCast(dest_ptr.?)))[0..elem_size];
        @memcpy(dst_slice, src_slice);
    }
}

/// Validate that an offset and size combination is within bounds
pub fn validateBounds(offset: usize, size: usize, total_size: usize) MemoryError!void {
    if (offset >= total_size) {
        return error.InvalidOffset;
    }
    if (offset + size > total_size) {
        return error.BufferOverflow;
    }
}

/// Safely cast and bounds-check a pointer to a specific type
pub fn safeCast(comptime T: type, ptr: ?*anyopaque, offset: usize, total_size: usize) MemoryError!*T {
    if (ptr == null) {
        return error.NullPointer;
    }

    try validateBounds(offset, @sizeOf(T), total_size);

    const base_ptr = @as([*]u8, @ptrCast(ptr.?));
    const typed_ptr = @as(*T, @ptrCast(@alignCast(base_ptr + offset)));
    return typed_ptr;
}

/// Safely read a value from memory with bounds checking
pub fn safeRead(comptime T: type, ptr: ?*anyopaque, offset: usize, total_size: usize) MemoryError!T {
    const typed_ptr = try safeCast(T, ptr, offset, total_size);
    return typed_ptr.*;
}

/// Safely write a value to memory with bounds checking
pub fn safeWrite(comptime T: type, ptr: ?*anyopaque, offset: usize, total_size: usize, value: T) MemoryError!void {
    const typed_ptr = try safeCast(T, ptr, offset, total_size);
    typed_ptr.* = value;
}
