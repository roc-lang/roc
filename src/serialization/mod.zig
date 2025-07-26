//! Serialization utilities for the Roc compiler
//!
//! This module provides:
//! - Common traits for serializable types
//! - Memory safety utilities
//! - Testing framework for serialization
//! - Error types and utilities
//! - IoVec-based scatter-gather I/O serialization
//! - Memory relocation utilities
//! - Aligned data writing utilities

const std = @import("std");

// Abstract serialization interfaces and utilities
pub const testing = @import("testing.zig");
pub const safety = @import("safety.zig");

// Concrete implementation modules
pub const iovec_serialize = @import("iovec_serialize.zig");
// Note: relocate functionality is now implemented as methods on individual types
pub const write_aligned = @import("write_aligned.zig");

// Re-export commonly used types and functions
pub const IovecWriter = iovec_serialize.IovecWriter;
pub const writeAlignedData = write_aligned.writeAlignedData;

const Allocator = std.mem.Allocator;

/// Standard alignment for all serialization operations
/// This ensures consistent alignment across all cached data and serialization
pub const SERIALIZATION_ALIGNMENT = 16;

/// Common errors that can occur during serialization
pub const SerializationError = error{
    BufferTooSmall,
    InvalidFormat,
    CorruptedData,
    UnsupportedVersion,
};

/// Helper function to write integers in little-endian format
pub fn writeInt(comptime T: type, buffer: []u8, value: T) void {
    if (buffer.len < @sizeOf(T)) {
        @panic("Buffer too small for integer");
    }
    std.mem.writeInt(T, buffer[0..@sizeOf(T)], value, .little);
}

/// Helper function to read integers in little-endian format
pub fn readInt(comptime T: type, buffer: []const u8) T {
    if (buffer.len < @sizeOf(T)) {
        @panic("Buffer too small for integer");
    }
    return std.mem.readInt(T, buffer[0..@sizeOf(T)], .little);
}

/// Helper function to ensure proper alignment for a buffer
pub fn alignBuffer(comptime T: type, buffer: []u8) []align(@alignOf(T)) u8 {
    return @as([]align(@alignOf(T)) u8, @alignCast(buffer));
}

/// Helper function to ensure proper alignment for a const buffer
pub fn alignBufferConst(comptime T: type, buffer: []const u8) []align(@alignOf(T)) const u8 {
    return @as([]align(@alignOf(T)) const u8, @alignCast(buffer));
}

/// Common validation function for serialization buffers
pub fn validateBuffer(required_size: usize, buffer: []const u8) SerializationError!void {
    if (buffer.len < required_size) {
        return SerializationError.BufferTooSmall;
    }
}

test {
    _ = @import("test_iovec_serialize.zig");
    // Note: relocate functionality now implemented as methods on individual types
    // _ = @import("test_relocate.zig");
}
