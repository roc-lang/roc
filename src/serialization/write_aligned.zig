//! Utilities for writing data with proper alignment to buffers
//!
//! This module provides functions for writing data to buffers while maintaining
//! alignment requirements and ensuring deterministic output by zeroing padding bytes.

const std = @import("std");

/// Zero padding buffer for alignment - sized to handle typical alignment requirements
/// 2 * @sizeOf(usize) should be enough for most alignment needs (typically 16 bytes on 64-bit)
pub const ZERO_PADDING: [2 * @sizeOf(usize)]u8 = [_]u8{0} ** (2 * @sizeOf(usize));

/// Write data to buffer with proper alignment, zeroing padding bytes for deterministic output
/// Returns the offset where the data was written
pub fn writeAlignedData(buffer: []u8, write_offset: *usize, data: []const u8, alignment: usize) usize {
    // Align the write offset
    const aligned_offset = std.mem.alignForward(usize, write_offset.*, alignment);

    // Zero out padding bytes for deterministic output
    if (aligned_offset > write_offset.*) {
        @memset(buffer[write_offset.*..aligned_offset], 0);
    }

    write_offset.* = aligned_offset;

    const data_offset = write_offset.*;
    @memcpy(buffer[data_offset .. data_offset + data.len], data);
    write_offset.* += data.len;
    return data_offset;
}

/// Append data to an iovec list with proper alignment
/// Returns the number of iovec entries added (1 or 2 depending on if padding was needed)
pub fn appendAlignedToIovecs(
    iovecs: *std.ArrayList(std.posix.iovec_const),
    current_offset: *usize,
    data: []const u8,
    alignment: usize,
) !usize {
    var entries_added: usize = 0;

    // Calculate aligned offset
    const aligned_offset = std.mem.alignForward(usize, current_offset.*, alignment);

    // Add padding if needed
    if (aligned_offset > current_offset.*) {
        const padding_size = aligned_offset - current_offset.*;
        std.debug.assert(padding_size <= ZERO_PADDING.len);

        try iovecs.append(.{
            .base = &ZERO_PADDING,
            .len = padding_size,
        });
        entries_added += 1;
        current_offset.* = aligned_offset;
    }

    // Add the actual data
    if (data.len > 0) {
        try iovecs.append(.{
            .base = data.ptr,
            .len = data.len,
        });
        entries_added += 1;
        current_offset.* += data.len;
    }

    return entries_added;
}
