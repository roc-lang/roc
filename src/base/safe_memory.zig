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

test "safeCopyArgument basic functionality" {
    // Source data: two I64 values (like our closure arguments)
    var src_data = [_]u8{ 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, // first I64
                          0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18 }; // second I64
    const src_ptr = @as(*anyopaque, @ptrCast(&src_data));
    
    // Destination buffer
    var dst_data: [8]u8 = undefined;
    const dst_ptr = @as(*anyopaque, @ptrCast(&dst_data));
    
    // Copy first I64 (offset=0, size=8)
    try safeCopyArgument(src_ptr, dst_ptr, 0, 8, 16);
    try std.testing.expectEqualSlices(u8, src_data[0..8], &dst_data);
    
    // Copy second I64 (offset=8, size=8)
    try safeCopyArgument(src_ptr, dst_ptr, 8, 8, 16);
    try std.testing.expectEqualSlices(u8, src_data[8..16], &dst_data);
}

test "safeCopyArgument bounds checking" {
    var src_data = [_]u8{ 0x01, 0x02, 0x03, 0x04 };
    const src_ptr = @as(*anyopaque, @ptrCast(&src_data));
    
    var dst_data: [8]u8 = undefined;
    const dst_ptr = @as(*anyopaque, @ptrCast(&dst_data));
    
    // Should fail: offset + size > max_arg_size
    try std.testing.expectError(error.BufferOverflow, safeCopyArgument(src_ptr, dst_ptr, 2, 4, 4));
    
    // Should fail: reading beyond source buffer
    try std.testing.expectError(error.BufferOverflow, safeCopyArgument(src_ptr, dst_ptr, 0, 8, 8));
}

test "safeCopyArgument null pointer handling" {
    var dst_data: [8]u8 = undefined;
    const dst_ptr = @as(*anyopaque, @ptrCast(&dst_data));
    
    // Should fail with null source pointer
    try std.testing.expectError(error.NullPointer, safeCopyArgument(null, dst_ptr, 0, 4, 8));
    
    var src_data = [_]u8{ 0x01, 0x02, 0x03, 0x04 };
    const src_ptr = @as(*anyopaque, @ptrCast(&src_data));
    
    // Should fail with null destination pointer
    try std.testing.expectError(error.NullPointer, safeCopyArgument(src_ptr, null, 0, 4, 8));
}

test "safeCopyArgument zero size" {
    var src_data = [_]u8{ 0x01, 0x02, 0x03, 0x04 };
    const src_ptr = @as(*anyopaque, @ptrCast(&src_data));
    
    var dst_data: [8]u8 = undefined;
    const dst_ptr = @as(*anyopaque, @ptrCast(&dst_data));
    
    // Zero size copy should succeed without copying anything
    try safeCopyArgument(src_ptr, dst_ptr, 0, 0, 8);
}

test "safeCopyArgument real-world closure scenario" {
    // Simulate the exact scenario from the closure: two I64 parameters
    var args = [_]i64{ 42, 1337 };  // Two I64 values
    const args_bytes = std.mem.asBytes(&args);
    const arg_ptr = @as(*anyopaque, @ptrCast(args_bytes.ptr));
    
    // First parameter
    var first_param: i64 = undefined;
    const first_dst = @as(*anyopaque, @ptrCast(&first_param));
    try safeCopyArgument(arg_ptr, first_dst, 0, 8, 16);
    try std.testing.expectEqual(@as(i64, 42), first_param);
    
    // Second parameter  
    var second_param: i64 = undefined;
    const second_dst = @as(*anyopaque, @ptrCast(&second_param));
    try safeCopyArgument(arg_ptr, second_dst, 8, 8, 16);
    try std.testing.expectEqual(@as(i64, 1337), second_param);
}