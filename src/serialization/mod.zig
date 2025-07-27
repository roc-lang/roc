//! Common serialization utilities and traits for the Roc compiler
//!
//! This module provides:
//! - Common traits for serializable types
//! - Memory safety utilities
//! - Testing framework for serialization
//! - Error types and utilities

const std = @import("std");

pub const testing = @import("testing.zig");
pub const safety = @import("safety.zig");
pub const CompactWriter = @import("CompactWriter.zig").CompactWriter;

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

/// Common errors that can occur during deserialization
pub const DeserializationError = error{
    BufferTooSmall,
    InvalidFormat,
    CorruptedData,
    UnsupportedVersion,
    OutOfMemory,
};

/// Trait interface for types that can be serialized
/// This is more of a documentation/convention than enforced by the compiler
pub fn Serializable(comptime T: type) type {
    return struct {
        /// Calculate the size needed to serialize this value
        pub fn serializedSize(self: *const T) usize {
            _ = self;
            @compileError("serializedSize must be implemented for " ++ @typeName(T));
        }

        /// Serialize this value into the provided buffer
        /// Returns the slice of buffer that was written to
        pub fn serializeInto(self: *const T, buffer: []u8) SerializationError![]const u8 {
            _ = self;
            _ = buffer;
            @compileError("serializeInto must be implemented for " ++ @typeName(T));
        }

        /// Deserialize a value from the provided buffer
        pub fn deserializeFrom(buffer: []const u8, allocator: Allocator) DeserializationError!T {
            _ = buffer;
            _ = allocator;
            @compileError("deserializeFrom must be implemented for " ++ @typeName(T));
        }
    };
}

/// Trait interface for types that can be serialized with an allocator
pub fn SerializableWithAllocator(comptime T: type) type {
    return struct {
        /// Calculate the size needed to serialize this value
        pub fn serializedSize(self: *const T) usize {
            _ = self;
            @compileError("serializedSize must be implemented for " ++ @typeName(T));
        }

        /// Serialize this value into the provided buffer
        /// Returns the slice of buffer that was written to
        pub fn serializeInto(self: *const T, buffer: []u8, allocator: Allocator) SerializationError![]const u8 {
            _ = self;
            _ = buffer;
            _ = allocator;
            @compileError("serializeInto must be implemented for " ++ @typeName(T));
        }

        /// Deserialize a value from the provided buffer
        pub fn deserializeFrom(buffer: []const u8, allocator: Allocator) DeserializationError!T {
            _ = buffer;
            _ = allocator;
            @compileError("deserializeFrom must be implemented for " ++ @typeName(T));
        }
    };
}

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

/// Common validation function for deserialization buffers
pub fn validateDeserializationBuffer(required_size: usize, buffer: []const u8) DeserializationError!void {
    if (buffer.len < required_size) {
        return DeserializationError.BufferTooSmall;
    }
}

/// Helper to check if a type implements the basic serialization interface
pub fn hasSerializationInterface(comptime T: type) bool {
    return @hasDecl(T, "serializedSize") and
        @hasDecl(T, "serializeInto") and
        @hasDecl(T, "deserializeFrom");
}

/// Helper to check if a type implements serialization with allocator
pub fn hasSerializationWithAllocatorInterface(comptime T: type) bool {
    if (!hasSerializationInterface(T)) return false;

    // Check if serializeInto takes an allocator parameter
    const serialize_info = @typeInfo(@TypeOf(T.serializeInto));
    if (serialize_info != .@"fn") return false;

    return serialize_info.@"fn".params.len >= 3; // self, buffer, allocator
}

test "serialization interface detection" {
    const TestType = struct {
        value: u32,

        pub fn serializedSize(self: *const @This()) usize {
            _ = self;
            return @sizeOf(u32);
        }

        pub fn serializeInto(self: *const @This(), buffer: []u8) SerializationError![]const u8 {
            try validateBuffer(@sizeOf(u32), buffer);
            writeInt(u32, buffer, self.value);
            return buffer[0..@sizeOf(u32)];
        }

        pub fn deserializeFrom(buffer: []const u8, allocator: Allocator) DeserializationError!@This() {
            _ = allocator;
            try validateDeserializationBuffer(@sizeOf(u32), buffer);
            return @This(){ .value = readInt(u32, buffer) };
        }
    };

    const std_testing = std.testing;
    try std_testing.expect(hasSerializationInterface(TestType));
    try std_testing.expect(!hasSerializationWithAllocatorInterface(TestType));
}

test {
    _ = @import("test/compact_writer_test.zig");
}
