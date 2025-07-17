//! Utilities for writing data with proper alignment to buffers
//!
//! This module provides functions for writing data to buffers while maintaining
//! alignment requirements and ensuring deterministic output by zeroing padding bytes.

const std = @import("std");

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
