//! IoVec-based serialization support for writing data structures to disk
//!
//! This module provides utilities for serializing data structures using scatter-gather I/O
//! (vectored I/O) instead of memory mapping. This approach allows us to build up a list
//! of memory regions to write without copying data into a single buffer first.

const std = @import("std");
const builtin = @import("builtin");
const write_aligned = @import("write_aligned.zig");

/// Writer that accumulates iovec entries instead of writing to a buffer
pub const IovecWriter = struct {
    iovecs: std.ArrayList(std.posix.iovec_const),
    current_offset: usize = 0,
    /// Deferred writes - these will be written to a buffer and added as a single iovec at the end
    deferred_writes: std.ArrayList(DeferredWrite) = undefined,
    allocator: std.mem.Allocator,

    const Self = @This();

    const DeferredWrite = struct {
        offset: usize,
        data: []const u8,
    };

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .iovecs = std.ArrayList(std.posix.iovec_const).init(allocator),
            .deferred_writes = std.ArrayList(DeferredWrite).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.iovecs.deinit();
        self.deferred_writes.deinit();
    }

    /// Get the current offset (total bytes that would be written)
    pub fn getOffset(self: *const Self) usize {
        return self.current_offset;
    }

    /// Append raw bytes without alignment
    pub fn appendBytes(self: *Self, data: []const u8) !void {
        if (data.len > 0) {
            try self.iovecs.append(.{
                .base = data.ptr,
                .len = data.len,
            });
            self.current_offset += data.len;
        }
    }

    /// Append bytes with alignment
    pub fn appendAligned(self: *Self, data: []const u8, alignment: usize) !usize {
        const offset_before = self.current_offset;
        _ = try write_aligned.appendAlignedToIovecs(&self.iovecs, &self.current_offset, data, alignment);
        return offset_before;
    }

    /// Append a struct as bytes
    pub fn appendStruct(self: *Self, value: anytype) !void {
        const bytes = std.mem.asBytes(&value);
        try self.appendBytes(bytes);
    }

    /// Append a struct with alignment
    pub fn appendStructAligned(self: *Self, value: anytype) !usize {
        const T = @TypeOf(value);
        const bytes = std.mem.asBytes(&value);
        return self.appendAligned(bytes, @alignOf(T));
    }

    /// Reserve space for a struct to be filled in later
    /// Returns the offset where the struct would be written
    pub fn reserveStruct(self: *Self, comptime T: type) !usize {
        const offset = self.current_offset;
        // We don't actually add an iovec here - the caller will need to
        // handle writing the actual data later
        self.current_offset += @sizeOf(T);
        return offset;
    }

    /// Add a deferred write - this will be written at the specified offset
    /// when finalize() is called
    pub fn writeDeferredStruct(self: *Self, offset: usize, value: anytype) !void {
        const bytes = std.mem.asBytes(&value);
        try self.deferred_writes.append(.{
            .offset = offset,
            .data = bytes,
        });
    }

    /// Finalize the writer by applying all deferred writes
    pub fn finalize(self: *Self) !void {
        if (self.deferred_writes.items.len == 0) return;

        // Create a buffer for all deferred writes
        var max_end_offset: usize = 0;
        for (self.deferred_writes.items) |write| {
            const end = write.offset + write.data.len;
            if (end > max_end_offset) max_end_offset = end;
        }

        const deferred_buffer = try self.allocator.alloc(u8, max_end_offset);
        @memset(deferred_buffer, 0);

        // Apply all deferred writes to the buffer
        for (self.deferred_writes.items) |write| {
            @memcpy(deferred_buffer[write.offset..][0..write.data.len], write.data);
        }

        // Add the buffer as the first iovec
        try self.iovecs.insert(0, .{
            .base = deferred_buffer.ptr,
            .len = deferred_buffer.len,
        });
    }

    /// Get total size that would be written
    pub fn totalSize(self: *const Self) usize {
        return self.current_offset;
    }

    /// Write all iovecs to a file using pwritev (POSIX) or fallback (Windows)
    /// Note: You should call finalize() before this if you have any deferred writes
    pub fn writevToFile(self: *const Self, file: std.fs.File, offset: u64) !void {
        if (builtin.os.tag == .windows) {
            // Windows doesn't have pwritev, so we need to allocate and copy
            const total_size = self.totalSize();
            const buffer = try self.iovecs.allocator.alloc(u8, total_size);
            defer self.iovecs.allocator.free(buffer);

            // Copy all iovec data into the buffer
            var write_offset: usize = 0;
            for (self.iovecs.items) |iov| {
                @memcpy(buffer[write_offset..][0..iov.len], iov.base[0..iov.len]);
                write_offset += iov.len;
            }

            // Write the entire buffer at once
            try file.pwriteAll(buffer, offset);
        } else {
            // Use pwritev on POSIX systems
            var bytes_written: usize = 0;
            const total_size = self.totalSize();

            while (bytes_written < total_size) {
                const n = try std.posix.pwritev(file.handle, self.iovecs.items, offset + bytes_written);
                if (n == 0) return error.UnexpectedEof;
                bytes_written += n;
            }
        }
    }
};

/// Interface for types that can serialize themselves to iovecs
pub fn SerializableToIovecs(comptime Self: type) type {
    return struct {
        /// Append this structure to an iovec writer
        /// Should return the offset where the main structure was written
        pub fn appendToIovecs(_: *const Self, _: *IovecWriter) !usize {
            @compileError("Type " ++ @typeName(Self) ++ " must implement appendToIovecs");
        }
    };
}

/// Helper to create a buffer from iovecs for testing/debugging
pub fn iovecsToBuf(allocator: std.mem.Allocator, iovecs: []const std.posix.iovec_const) ![]u8 {
    var total_size: usize = 0;
    for (iovecs) |iov| {
        total_size += iov.len;
    }

    const buffer = try allocator.alloc(u8, total_size);
    var offset: usize = 0;
    for (iovecs) |iov| {
        @memcpy(buffer[offset..][0..iov.len], iov.base[0..iov.len]);
        offset += iov.len;
    }

    return buffer;
}

test "IovecWriter basic operations" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Test appending bytes
    try writer.appendBytes("hello");
    try testing.expectEqual(@as(usize, 5), writer.getOffset());

    // Test appending with alignment
    const offset = try writer.appendAligned("world", 8);
    try testing.expectEqual(@as(usize, 5), offset); // Offset where aligned write starts

    // Test total size
    try testing.expectEqual(@as(usize, 13), writer.totalSize()); // 5 + 3 padding + 5

    // Verify the data
    const buffer = try iovecsToBuf(allocator, writer.iovecs.items);
    defer allocator.free(buffer);

    try testing.expectEqualStrings("hello\x00\x00\x00world", buffer);
}

test "IovecWriter struct serialization" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Test appending raw data
    const data = [_]u8{ 1, 2, 3, 4, 5, 6, 7, 8 };
    try writer.appendBytes(&data);

    const buffer = try iovecsToBuf(allocator, writer.iovecs.items);
    defer allocator.free(buffer);

    // Verify the buffer contains the expected data
    try testing.expectEqualSlices(u8, &data, buffer);
}
