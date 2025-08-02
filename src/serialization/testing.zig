//! Simple testing framework for serialization

const std = @import("std");
const safety = @import("safety.zig");
const mod = @import("mod.zig");

const SERIALIZATION_ALIGNMENT = safety.SERIALIZATION_ALIGNMENT;

const testing = std.testing;
const Allocator = std.mem.Allocator;

/// Simple comprehensive test for serializable types
pub fn testSerialization(comptime T: type, value: *const T, allocator: Allocator) !void {
    const size = value.serializedSize();

    // Test with buffer that has extra space to detect overruns
    const buffer_size = size + 32;
    var buffer = try safety.createStandardAlignedCanaryBuffer(allocator, buffer_size);
    defer allocator.free(buffer);

    // Serialize
    const serialized = try serializeValue(T, value, buffer[0..size], allocator);
    try testing.expectEqual(size, serialized.len);

    // Check for buffer overruns
    try safety.checkBufferIntegrity(buffer, size);

    // Check for uninitialized memory
    try safety.checkForUninitializedMemory(serialized);

    // Deserialize
    const deserialized = try deserializeValue(T, serialized, allocator);
    defer deinitValue(T, &deserialized, allocator);

    // Test deterministic output - create aligned buffer
    const buffer2 = try allocator.alignedAlloc(u8, SERIALIZATION_ALIGNMENT, size);
    defer allocator.free(buffer2);
    const serialized2 = try serializeValue(T, value, buffer2, allocator);
    try testing.expectEqualSlices(u8, serialized, serialized2);

    // Test boundary conditions
    if (size > 0) {
        const small_buffer = try allocator.alignedAlloc(u8, SERIALIZATION_ALIGNMENT, size - 1);
        defer allocator.free(small_buffer);
        try testing.expectError(error.BufferTooSmall, serializeValue(T, value, small_buffer, allocator));
    }
}

/// Helper function to create aligned buffer with standard alignment
fn createStandardAlignedBuffer(allocator: Allocator, size: usize) ![]align(SERIALIZATION_ALIGNMENT) u8 {
    return try allocator.alignedAlloc(u8, SERIALIZATION_ALIGNMENT, size);
}

/// Helper function to create aligned canary buffer with standard alignment
fn createStandardAlignedCanaryBuffer(allocator: Allocator, size: usize) ![]align(SERIALIZATION_ALIGNMENT) u8 {
    return try safety.createStandardAlignedCanaryBuffer(allocator, size);
}

/// Helper function to serialize a value, handling different function signatures
fn serializeValue(comptime T: type, value: *const T, buffer: []align(SERIALIZATION_ALIGNMENT) u8, allocator: Allocator) ![]const u8 {
    if (!@hasDecl(T, "serializeInto")) {
        @compileError("Type " ++ @typeName(T) ++ " does not implement serializeInto");
    }

    // Check function signature at compile time
    const serialize_info = @typeInfo(@TypeOf(T.serializeInto));
    if (serialize_info != .@"fn") {
        @compileError("serializeInto is not a function");
    }

    // Check parameter count to determine which signature to use
    const param_count = serialize_info.@"fn".params.len;
    if (param_count >= 3) {
        // Signature: serializeInto(self, buffer, allocator)
        return try value.serializeInto(buffer, allocator);
    } else {
        // Signature: serializeInto(self, buffer)
        return try value.serializeInto(buffer);
    }
}

/// Helper function to deserialize a value, handling different function signatures
fn deserializeValue(comptime T: type, buffer: []const u8, allocator: Allocator) !T {
    // Create properly aligned mutable buffer copy
    const new_buffer = try allocator.alignedAlloc(u8, SERIALIZATION_ALIGNMENT, buffer.len);
    defer allocator.free(new_buffer);
    @memcpy(new_buffer, buffer);

    if (!@hasDecl(T, "deserializeFrom")) {
        @compileError("Type " ++ @typeName(T) ++ " does not implement deserializeFrom");
    }

    // Check function signature at compile time
    const deserialize_info = @typeInfo(@TypeOf(T.deserializeFrom));
    if (deserialize_info != .@"fn") {
        @compileError("deserializeFrom is not a function");
    }

    // Check parameter count to determine which signature to use
    const param_count = deserialize_info.@"fn".params.len;
    if (param_count >= 2) {
        // Signature: deserializeFrom(buffer, allocator)
        return try T.deserializeFrom(new_buffer, allocator);
    } else {
        // Signature: deserializeFrom(buffer)
        return try T.deserializeFrom(new_buffer);
    }
}

/// Helper function to deinitialize a value if it has a deinit method
fn deinitValue(comptime T: type, value: *const T, allocator: Allocator) void {
    if (@hasDecl(T, "deinit")) {
        var mutable_value = value.*;
        const deinit_info = @typeInfo(@TypeOf(T.deinit));
        if (deinit_info == .@"fn" and deinit_info.@"fn".params.len >= 2) {
            mutable_value.deinit(allocator);
        } else {
            mutable_value.deinit();
        }
    }
}

