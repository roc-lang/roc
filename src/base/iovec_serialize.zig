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
    /// Track buffers we own and need to free
    owned_buffers: std.ArrayList([]u8) = undefined,

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
            .owned_buffers = std.ArrayList([]u8).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.iovecs.deinit();
        self.deferred_writes.deinit();
        // Free all owned buffers
        for (self.owned_buffers.items) |buffer| {
            self.allocator.free(buffer);
        }
        self.owned_buffers.deinit();
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
        // We need to copy the bytes since the value might be a temporary
        const data_copy = try self.allocator.alloc(u8, bytes.len);
        @memcpy(data_copy, bytes);
        try self.deferred_writes.append(.{
            .offset = offset,
            .data = data_copy,
        });
        // Track this allocation so we can free it later
        try self.owned_buffers.append(data_copy);
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

        // Track this buffer so we can free it later
        try self.owned_buffers.append(deferred_buffer);
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
            // Use pwritev on POSIX systems with proper partial write handling
            var bytes_written: usize = 0;
            var current_iovec: usize = 0;
            var iovec_offset: usize = 0;
            const total_size = self.totalSize();

            while (bytes_written < total_size) {
                // Create adjusted iovec array for partial writes
                const remaining_iovecs = self.iovecs.items.len - current_iovec;
                var adjusted_iovecs = try self.allocator.alloc(std.posix.iovec_const, remaining_iovecs);
                defer self.allocator.free(adjusted_iovecs);

                // Copy remaining iovecs, adjusting first one for partial write
                for (self.iovecs.items[current_iovec..], 0..) |iovec, j| {
                    if (j == 0 and iovec_offset > 0) {
                        // Adjust first iovec for partial write
                        adjusted_iovecs[j] = .{
                            .base = @ptrFromInt(@intFromPtr(iovec.base) + iovec_offset),
                            .len = iovec.len - iovec_offset,
                        };
                    } else {
                        adjusted_iovecs[j] = iovec;
                    }
                }

                const n = try std.posix.pwritev(file.handle, adjusted_iovecs, offset + bytes_written);
                if (n == 0) return error.UnexpectedEof;

                // Update position tracking
                bytes_written += n;
                var remaining = n;

                // Figure out where we are now
                while (remaining > 0 and current_iovec < self.iovecs.items.len) {
                    const iovec_remaining = self.iovecs.items[current_iovec].len - iovec_offset;
                    if (remaining >= iovec_remaining) {
                        remaining -= iovec_remaining;
                        current_iovec += 1;
                        iovec_offset = 0;
                    } else {
                        iovec_offset += remaining;
                        remaining = 0;
                    }
                }
            }
        }
    }

    /// Write all iovecs to a file atomically using a temporary file
    /// This ensures we don't leave a partially written file on error
    pub fn writevToFileAtomic(self: *const Self, dir: std.fs.Dir, file_path: []const u8, mode: std.fs.File.Mode) !void {
        // Generate temporary file name
        var tmp_name_buf: [256]u8 = undefined;
        const tmp_name = try std.fmt.bufPrint(&tmp_name_buf, "{s}.tmp.{d}", .{ file_path, std.time.milliTimestamp() });

        // Write to temporary file
        const tmp_file = try dir.createFile(tmp_name, .{ .mode = mode });
        defer tmp_file.close();

        // If write fails, ensure temp file is deleted
        errdefer dir.deleteFile(tmp_name) catch {};

        try self.writevToFile(tmp_file, 0);

        // Sync to ensure data is on disk
        try tmp_file.sync();

        // Atomically rename temp file to final name
        try dir.rename(tmp_name, file_path);
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

test "IovecWriter atomic file write" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Add some test data
    try writer.appendBytes("hello");
    try writer.appendBytes("world");

    // Create a temporary directory for testing
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Write atomically
    try writer.writevToFileAtomic(tmp_dir.dir, "test_file.bin", 0o644);

    // Verify the file exists and has correct content
    const file = try tmp_dir.dir.openFile("test_file.bin", .{});
    defer file.close();

    var buffer: [10]u8 = undefined;
    const bytes_read = try file.read(&buffer);
    try testing.expectEqual(@as(usize, 10), bytes_read);
    try testing.expectEqualStrings("helloworld", buffer[0..bytes_read]);

    // Ensure no temp files remain
    var iter = tmp_dir.dir.iterate();
    var count: usize = 0;
    while (try iter.next()) |entry| {
        count += 1;
        try testing.expectEqualStrings("test_file.bin", entry.name);
    }
    try testing.expectEqual(@as(usize, 1), count);
}

test "IovecWriter deferred writes memory management" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Create some deferred writes
    const TestStruct = struct { a: u32, b: u64 };
    const offset1 = try writer.reserveStruct(TestStruct);
    const offset2 = try writer.reserveStruct(TestStruct);

    // Verify offsets are correct
    try testing.expectEqual(@as(usize, 0), offset1);
    try testing.expectEqual(@as(usize, @sizeOf(TestStruct)), offset2);

    // Write deferred structs
    try writer.writeDeferredStruct(offset1, TestStruct{ .a = 42, .b = 100 });
    try writer.writeDeferredStruct(offset2, TestStruct{ .a = 84, .b = 200 });

    // Finalize - this should allocate a buffer that gets tracked
    try writer.finalize();

    // Verify owned_buffers has the deferred buffer plus the two struct copies
    try testing.expectEqual(@as(usize, 3), writer.owned_buffers.items.len);

    // Verify the data is correct
    // The finalized buffer only contains the deferred writes
    const deferred_buffer = writer.iovecs.items[0]; // First iovec is the deferred buffer
    try testing.expectEqual(@as(usize, @sizeOf(TestStruct) * 2), deferred_buffer.len);

    // Read structs from the deferred buffer
    const struct1 = @as(*const TestStruct, @ptrCast(@alignCast(deferred_buffer.base))).*;
    const struct2 = @as(*const TestStruct, @ptrCast(@alignCast(deferred_buffer.base + @sizeOf(TestStruct)))).*;

    try testing.expectEqual(@as(u32, 42), struct1.a);
    try testing.expectEqual(@as(u64, 100), struct1.b);
    try testing.expectEqual(@as(u32, 84), struct2.a);
    try testing.expectEqual(@as(u64, 200), struct2.b);

    // deinit should free the owned buffer (tested by running with leak detection)
}

test "IovecWriter partial write simulation" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Test that our partial write logic correctly handles iovec boundaries
    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Add multiple iovecs of different sizes
    try writer.appendBytes("hello"); // 5 bytes
    try writer.appendBytes("world"); // 5 bytes
    try writer.appendBytes("foo"); // 3 bytes
    try writer.appendBytes("bar"); // 3 bytes

    // Total: 16 bytes across 4 iovecs

    // Simulate partial write scenarios
    {
        // Scenario 1: Write stops in middle of first iovec
        var current_iovec: usize = 0;
        var iovec_offset: usize = 0;
        var remaining: usize = 3; // Only 3 bytes written

        while (remaining > 0 and current_iovec < writer.iovecs.items.len) {
            const iovec_remaining = writer.iovecs.items[current_iovec].len - iovec_offset;
            if (remaining >= iovec_remaining) {
                remaining -= iovec_remaining;
                current_iovec += 1;
                iovec_offset = 0;
            } else {
                iovec_offset += remaining;
                remaining = 0;
            }
        }

        try testing.expectEqual(@as(usize, 0), current_iovec);
        try testing.expectEqual(@as(usize, 3), iovec_offset);
    }

    {
        // Scenario 2: Write completes first iovec and stops in second
        var current_iovec: usize = 0;
        var iovec_offset: usize = 0;
        var remaining: usize = 7; // 7 bytes written

        while (remaining > 0 and current_iovec < writer.iovecs.items.len) {
            const iovec_remaining = writer.iovecs.items[current_iovec].len - iovec_offset;
            if (remaining >= iovec_remaining) {
                remaining -= iovec_remaining;
                current_iovec += 1;
                iovec_offset = 0;
            } else {
                iovec_offset += remaining;
                remaining = 0;
            }
        }

        try testing.expectEqual(@as(usize, 1), current_iovec);
        try testing.expectEqual(@as(usize, 2), iovec_offset);
    }
}

test "IovecWriter Windows fallback" {
    if (builtin.os.tag != .windows) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Add some data
    try writer.appendBytes("hello");
    try writer.appendBytes("world");

    // Create temporary file for testing
    const tmp_dir = testing.tmp_dir;
    const tmp_file = try tmp_dir.dir.createFile("test_iovec.bin", .{});
    defer tmp_file.close();
    defer tmp_dir.dir.deleteFile("test_iovec.bin") catch {};

    // Write using Windows fallback
    try writer.writevToFile(tmp_file, 0);

    // Verify file contents
    try tmp_file.seekTo(0);
    var read_buffer: [10]u8 = undefined;
    const bytes_read = try tmp_file.read(&read_buffer);

    try testing.expectEqual(@as(usize, 10), bytes_read);
    try testing.expectEqualStrings("helloworld", read_buffer[0..bytes_read]);
}
