//! IoVec-based serialization support for writing data structures to disk
//!
//! This module provides utilities for serializing data structures using scatter-gather I/O
//! (vectored I/O) instead of memory mapping. This approach allows us to build up a list
//! of memory regions to write without copying data into a single buffer first.
//!
//! ## Platform Support
//!
//! - **POSIX systems**: Uses `pwritev()` for efficient scatter-gather I/O
//! - **Windows**: Automatically falls back to buffering all iovecs into a single
//!   memory buffer and writing with `pwriteAll()`, since Windows doesn't have pwritev

const std = @import("std");
const builtin = @import("builtin");
const write_aligned = @import("write_aligned.zig");

/// Standard alignment for all serialization operations
const SERIALIZATION_ALIGNMENT = 16;

/// Sentinel value for empty arrays during serialization
/// This value is used instead of 0 to avoid null pointers
/// We use a large value that's unlikely to be a real offset and is properly aligned
/// This value is aligned to the maximum alignment we might need (16 bytes)
pub const EMPTY_ARRAY_SENTINEL: usize = 0x1000; // 4096 - page aligned

/// Minimum offset for any data in the serialization buffer
/// This ensures that offset 0 is never used for actual data
/// which would create null pointers during deserialization
pub const MIN_OFFSET: usize = 16;

/// Writer that accumulates iovec entries instead of writing to a buffer
pub const IovecWriter = struct {
    iovecs: std.ArrayList(std.posix.iovec_const),
    current_offset: usize = 0,
    allocator: std.mem.Allocator,
    /// Track buffers we own and need to free
    owned_buffers: std.ArrayList([]u8) = undefined,

    const Self = @This();


    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .iovecs = std.ArrayList(std.posix.iovec_const).init(allocator),
            .current_offset = MIN_OFFSET, // Start at MIN_OFFSET to avoid offset 0
            .allocator = allocator,
            .owned_buffers = std.ArrayList([]u8).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.iovecs.deinit();
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

    /// Append bytes with alignment for the data's natural type alignment
    /// This is the primary method for appending data
    pub fn appendBytes(self: *Self, comptime T: type, data: []const u8) !usize {
        // Ensure alignment for the target type
        const current_offset = self.current_offset;
        const type_alignment = @alignOf(T);
        const aligned_offset = std.mem.alignForward(usize, current_offset, type_alignment);
        if (aligned_offset > current_offset) {
            const padding_size = aligned_offset - current_offset;
            const padding = try self.allocator.alloc(u8, padding_size);
            @memset(padding, 0);
            try self.appendBytesRaw(padding);
            try self.owned_buffers.append(padding);
        }
        
        const start_offset = self.current_offset;
        try self.appendBytesRaw(data);
        return start_offset;
    }

    /// Legacy method for compatibility - defaults to u8 alignment
    pub fn appendBytesLegacy(self: *Self, data: []const u8) !usize {
        return self.appendBytes(u8, data);
    }

    /// Append a struct with proper alignment for the struct type
    pub fn appendStruct(self: *Self, value: anytype) !usize {
        const T = @TypeOf(value);
        const info = @typeInfo(T);
        
        // Handle different types appropriately
        const bytes = switch (info) {
            .pointer => |ptr_info| blk: {
                // If it's a single-item pointer, dereference it
                if (ptr_info.size == .One) {
                    break :blk std.mem.asBytes(value);
                } else {
                    @compileError("appendStruct should not be used with slices or multi-item pointers. Use appendBytes instead.");
                }
            },
            .array => std.mem.asBytes(&value),
            .@"struct", .@"union", .@"enum", .int, .float, .bool => blk: {
                // For value types that are passed by value, we need to be careful
                // Create a copy to ensure we have a stable address
                const copy = try self.allocator.alloc(u8, @sizeOf(T));
                @memcpy(copy, std.mem.asBytes(&value));
                try self.owned_buffers.append(copy);
                break :blk copy;
            },
            else => @compileError("appendStruct doesn't support type " ++ @typeName(T)),
        };
        
        // The type alignment we need
        const AlignType = if (info == .pointer and info.pointer.size == .One) info.pointer.child else T;
        return self.appendBytes(AlignType, bytes);
    }

    /// Temporary helper for tests - append bytes with u8 alignment
    pub fn appendBytesU8(self: *Self, data: []const u8) !usize {
        return self.appendBytes(u8, data);
    }

    /// Internal method to append raw bytes without alignment (only used internally)
    fn appendBytesRaw(self: *Self, data: []const u8) !void {
        if (data.len > 0) {
            try self.iovecs.append(.{
                .base = data.ptr,
                .len = data.len,
            });
            self.current_offset += data.len;
        }
    }


    /// Get total size that would be written
    pub fn totalSize(self: *const Self) usize {
        return self.current_offset;
    }

    /// Serialize all iovecs into a single buffer
    /// This is essentially the same logic as the Windows fallback in writevToFile
    pub fn serialize(self: *const Self, allocator: std.mem.Allocator) ![]u8 {
        // Use current_offset which already tracks the total size correctly
        const buffer = try allocator.alloc(u8, self.current_offset);
        
        // Fill initial padding with zeros
        @memset(buffer[0..MIN_OFFSET], 0);
        
        // Copy all iovec data into the buffer
        var write_offset: usize = MIN_OFFSET;
        for (self.iovecs.items) |iov| {
            @memcpy(buffer[write_offset..][0..iov.len], iov.base[0..iov.len]);
            write_offset += iov.len;
        }
        
        return buffer;
    }

    /// Write all iovecs to a file using pwritev (POSIX) or fallback (Windows)
    pub fn writevToFile(self: *const Self, file: std.fs.File, offset: u64) !void {
        if (builtin.os.tag == .windows) {
            // Windows doesn't have pwritev, so serialize to a buffer and write it
            const buffer = try self.serialize(self.allocator);
            defer self.allocator.free(buffer);
            
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
    _ = try writer.appendBytesU8("hello");
    try testing.expectEqual(@as(usize, 5), writer.getOffset());

    // Test appending with alignment - use a type that requires 4-byte alignment
    const u32_value: u32 = 42;
    const offset = try writer.appendStruct(u32_value);
    try testing.expect(offset % 4 == 0); // Should be 4-byte aligned

    // Test total size (should include padding to align the u32)
    const expected_size = 5 + 3 + 4; // 5 bytes + 3 padding + 4 bytes for u32
    try testing.expectEqual(expected_size, writer.totalSize());

    // Verify the data
    const buffer = try iovecsToBuf(allocator, writer.iovecs.items);
    defer allocator.free(buffer);

    // Check basic structure - should have "hello" + padding + u32
    try testing.expectEqual(@as(usize, 12), buffer.len);
    try testing.expectEqualStrings("hello", buffer[0..5]);
}

test "IovecWriter struct serialization" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Test appending raw data
    const data = [_]u8{ 1, 2, 3, 4, 5, 6, 7, 8 };
    _ = try writer.appendBytes(u8, std.mem.asBytes(&data));

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
    _ = try writer.appendBytesU8("hello");
    _ = try writer.appendBytesU8("world");

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


test "IovecWriter partial write simulation" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Test that our partial write logic correctly handles iovec boundaries
    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Add multiple iovecs of different sizes
    _ = try writer.appendBytes(u8, "hello"); // 5 bytes
    _ = try writer.appendBytes(u8, "world"); // 5 bytes
    _ = try writer.appendBytes(u8, "foo"); // 3 bytes
    _ = try writer.appendBytes(u8, "bar"); // 3 bytes

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

test "IovecWriter serialize method" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Add some data
    _ = try writer.appendBytes(u8, "hello");
    _ = try writer.appendBytes(u8, "world");
    
    // Test the serialize method
    const buffer = try writer.serialize(allocator);
    defer allocator.free(buffer);
    
    try testing.expectEqual(@as(usize, 10), buffer.len);
    try testing.expectEqualStrings("helloworld", buffer);
}

test "IovecWriter Windows fallback" {
    if (builtin.os.tag != .windows) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Add some data
    _ = try writer.appendBytes(u8, "hello");
    _ = try writer.appendBytes(u8, "world");

    // Create temporary file for testing
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    
    const tmp_file = try tmp_dir.dir.createFile("test_iovec.bin", .{});
    defer tmp_file.close();

    // Write using Windows fallback (which now uses serialize() internally)
    try writer.writevToFile(tmp_file, 0);

    // Verify file contents
    try tmp_file.seekTo(0);
    var read_buffer: [10]u8 = undefined;
    const bytes_read = try tmp_file.read(&read_buffer);

    try testing.expectEqual(@as(usize, 10), bytes_read);
    try testing.expectEqualStrings("helloworld", read_buffer[0..bytes_read]);
}

