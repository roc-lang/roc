//! ByteSlices provides a way to efficiently store multiple byte slices (like strings or number literals)
//! in a single contiguous buffer, with each slice prefixed by its length.
//!
//! This is useful for storing things like string literals, number literals, and other variable-length
//! data in a compact format that can be easily indexed and retrieved.

const std = @import("std");
const collections = @import("collections");
const Allocator = std.mem.Allocator;

const ByteSlices = @This();

entries: collections.SafeList(u8),

pub const Idx = enum(u32) {
    _,

    fn asUsize(self: Idx) usize {
        return @intCast(@intFromEnum(self));
    }
};

pub fn slice(self: *const ByteSlices, idx: ByteSlices.Idx) []const u8 {
    const idx_usize = idx.asUsize();

    // Read the length as a native-endian u32
    const len_bytes = self.entries.items.items[idx_usize .. idx_usize + 4];
    const slice_len = std.mem.readInt(u32, len_bytes[0..4], .little);
    const slice_start = idx_usize + @sizeOf(u32);

    return self.entries.items.items[slice_start .. slice_start + @as(usize, @intCast(slice_len))];
}

/// Appends the given slice inline to the bytes, with the u32 length written first
/// (after up to three zeros for alignment padding as necessary), then returns
/// the index of the length.
pub fn append(self: *ByteSlices, allocator: Allocator, bytes: []const u8) Allocator.Error!ByteSlices.Idx {
    // We may need some alignment padding bytes to store a u32 in our bytes array.
    const current_len = self.entries.items.items.len;
    const len_type = u32;
    const len_size = @sizeOf(len_type);
    const len_alignment = @alignOf(len_type);
    const padding = (len_alignment - (current_len % len_alignment)) % len_alignment;

    // Store the length right after the alignment padding.
    const len_idx = current_len + padding;

    // Reserve enough space for alignment padding, u32 length, and the actual bytes.
    try self.entries.items.ensureUnusedCapacity(allocator, padding + len_size + bytes.len);

    // Branchlessly zero out the padding by appending three zeros.
    // There will definitely be enough space, because we just reserved
    // space for at *least* the 4-byte length, and if it turned out
    // we didn't need any padding, the length will override these anyway.
    // This approach guarantees we don't pay for a branch misprediction.
    inline for (0..len_size - 1) |_| {
        self.entries.items.appendAssumeCapacity(0);
    }

    // Set the length to include the length field first, so we can write to it
    self.entries.items.items.len = len_idx + len_size;

    // Now write the length at the aligned position
    std.debug.assert(@intFromPtr(self.entries.items.items.ptr + len_idx) % len_alignment == 0);
    const len_bytes = self.entries.items.items[len_idx .. len_idx + len_size];
    std.mem.writeInt(len_type, len_bytes[0..len_size], @as(len_type, @intCast(bytes.len)), .little);

    // Append the bytes after the length.
    self.entries.items.appendSliceAssumeCapacity(bytes);

    // Return the index where the length was written
    return @as(ByteSlices.Idx, @enumFromInt(@as(u32, @intCast(len_idx))));
}
