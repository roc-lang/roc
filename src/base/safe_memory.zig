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

test "safeCopy basic functionality" {
    var dst: [10]u8 = undefined;
    const src = "hello";

    try safeCopy(&dst, src, 10);
    try std.testing.expectEqualSlices(u8, "hello", dst[0..5]);
}

test "safeCopy buffer overflow detection" {
    var dst: [3]u8 = undefined;
    const src = "hello";

    try std.testing.expectError(error.BufferOverflow, safeCopy(&dst, src, 10));
}

test "safeSlice basic functionality" {
    var buffer = [_]u8{ 1, 2, 3, 4, 5 };
    const ptr = @as(*anyopaque, @ptrCast(&buffer));

    const slice = try safeSlice(ptr, 1, 3, 5);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 2, 3, 4 }, slice);
}

test "safeSlice bounds checking" {
    var buffer = [_]u8{ 1, 2, 3, 4, 5 };
    const ptr = @as(*anyopaque, @ptrCast(&buffer));

    try std.testing.expectError(error.BufferOverflow, safeSlice(ptr, 3, 4, 5));
    try std.testing.expectError(error.InvalidOffset, safeSlice(ptr, 6, 1, 5));
}

test "safeCast and safeRead" {
    var buffer = [_]u8{ 0x12, 0x34, 0x56, 0x78 };
    const ptr = @as(*anyopaque, @ptrCast(&buffer));

    const value = try safeRead(u16, ptr, 0, 4);
    // Endianness dependent, but should not crash
    _ = value;

    try std.testing.expectError(error.BufferOverflow, safeRead(u32, ptr, 1, 4));
}
