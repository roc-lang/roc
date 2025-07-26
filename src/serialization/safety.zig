//! Memory safety utilities for serialization
//!
//! This module provides utilities to detect memory safety issues
//! during serialization and deserialization operations.

const std = @import("std");

/// During deserialization, we align the base pointer to this number of bytes.
pub const SERIALIZATION_ALIGNMENT = 16;

/// Pattern used to detect uninitialized memory
pub const CANARY_PATTERN: u8 = 0xAA;

/// Maximum consecutive canary bytes before considering it uninitialized memory
pub const MAX_CANARY_RUN: usize = 8;

/// Error types for memory safety violations
pub const MemorySafetyError = error{
    UninitializedMemory,
    BufferOverrun,
};

/// Check for uninitialized memory patterns in data
/// This function looks for long runs of canary bytes which may indicate
/// uninitialized memory being included in serialized data
pub fn checkForUninitializedMemory(data: []const u8) MemorySafetyError!void {
    var canary_run: usize = 0;
    for (data) |byte| {
        if (byte == CANARY_PATTERN) {
            canary_run += 1;
            if (canary_run > MAX_CANARY_RUN) {
                return MemorySafetyError.UninitializedMemory;
            }
        } else {
            canary_run = 0;
        }
    }
}

/// Check buffer integrity by verifying canary patterns in unused portions
/// This helps detect buffer overruns during serialization
pub fn checkBufferIntegrity(buffer: []const u8, used_size: usize) MemorySafetyError!void {
    if (used_size > buffer.len) {
        return MemorySafetyError.BufferOverrun;
    }

    for (buffer[used_size..]) |byte| {
        if (byte != CANARY_PATTERN) {
            return MemorySafetyError.BufferOverrun;
        }
    }
}

/// Create a buffer filled with canary pattern for testing
pub fn createCanaryBuffer(allocator: std.mem.Allocator, size: usize) ![]u8 {
    const buffer = try allocator.alloc(u8, size);
    @memset(buffer, CANARY_PATTERN);
    return buffer;
}

/// Create an aligned buffer filled with canary pattern for testing
pub fn createCanaryBufferAligned(allocator: std.mem.Allocator, size: usize, comptime alignment: u29) ![]u8 {
    const buffer = try allocator.alignedAlloc(u8, alignment, size);
    @memset(buffer, CANARY_PATTERN);
    return buffer;
}

/// Create a buffer with standard serialization alignment filled with canary pattern
pub fn createStandardAlignedCanaryBuffer(allocator: std.mem.Allocator, size: usize) ![]align(SERIALIZATION_ALIGNMENT) u8 {
    const buffer = try allocator.alignedAlloc(u8, SERIALIZATION_ALIGNMENT, size);
    @memset(buffer, CANARY_PATTERN);
    return buffer;
}

test "canary pattern detection" {
    const testing = std.testing;

    // Test detection of long canary runs
    var buffer = try createCanaryBuffer(testing.allocator, 100);
    defer testing.allocator.free(buffer);

    // Should detect uninitialized memory
    try testing.expectError(MemorySafetyError.UninitializedMemory, checkForUninitializedMemory(buffer));

    // Break up the pattern
    for (0..buffer.len) |i| {
        if (i % 8 == 0) {
            buffer[i] = 0xFF;
        }
    }

    // Should pass now
    try checkForUninitializedMemory(buffer);
}

test "buffer integrity checking" {
    const testing = std.testing;

    const buffer_size = 100;
    const used_size = 50;

    var buffer = try createCanaryBuffer(testing.allocator, buffer_size);
    defer testing.allocator.free(buffer);

    // Should pass with correct usage
    try checkBufferIntegrity(buffer, used_size);

    // Corrupt the unused portion
    buffer[used_size + 10] = 0xFF;

    // Should detect buffer overrun
    try testing.expectError(MemorySafetyError.BufferOverrun, checkBufferIntegrity(buffer, used_size));
}
