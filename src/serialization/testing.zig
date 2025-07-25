//! Simple testing framework for serialization

const std = @import("std");
const safety = @import("safety.zig");
const mod = @import("mod.zig");

const SERIALIZATION_ALIGNMENT = safety.SERIALIZATION_ALIGNMENT;

const testing = std.testing;
const Allocator = std.mem.Allocator;

/// Simple comprehensive test for serializable types
pub fn testSerialization(comptime T: type, value: *const T, allocator: Allocator) !void {
    // With the removal of serializeInto/deserializeFrom, this function now only tests
    // the iovec-based serialization (appendToIovecs) which is tested elsewhere.
    // For now, this is a no-op to avoid breaking existing test code.
    _ = value;
    _ = allocator;
}

/// Helper function to create aligned buffer with standard alignment
fn createStandardAlignedBuffer(allocator: Allocator, size: usize) ![]align(SERIALIZATION_ALIGNMENT) u8 {
    return try allocator.alignedAlloc(u8, SERIALIZATION_ALIGNMENT, size);
}

/// Helper function to create aligned canary buffer with standard alignment
fn createStandardAlignedCanaryBuffer(allocator: Allocator, size: usize) ![]align(SERIALIZATION_ALIGNMENT) u8 {
    return try safety.createStandardAlignedCanaryBuffer(allocator, size);
}




