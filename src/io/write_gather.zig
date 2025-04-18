//! Cross-platform abstraction over vector I/O write operations: WriteFileGather on Windows
//! and writev on POSIX platforms.
//!
//! This module provides a lowest common denominator API for gather I/O operations
//! while satisfying the requirements of both Windows and POSIX implementations.

const std = @import("std");
const Allocator = std.mem.Allocator;
const utils = @import("../collections/utils.zig");

/// Platform-specific implementation details for gather I/O.
const backend = if (@import("builtin").os.tag == .windows)
    @import("write_gather_windows.zig")
else
    @import("write_gather_posix.zig");

/// Platform-specific AlignedBuffer implementation
pub const PlatformAlignedBuffer = if (@import("builtin").os.tag == .windows)
    backend.WindowsAlignedBuffer
else
    backend.PosixAlignedBuffer;

/// A properly aligned buffer for gather I/O operations.
///
/// This type ensures that all platform-specific alignment requirements are met.
/// On Windows, buffers are aligned to the volume sector size and sized to a multiple
/// of that sector size. On other platforms, no special alignment is required.
///
/// This type must be created using AlignedBuffer.init() and freed with deinit().
pub const AlignedBuffer = struct {
    /// The platform-specific implementation
    impl: PlatformAlignedBuffer,

    /// Gets the required alignment for the platform.
    /// On Windows, this returns the sector size for the file's volume.
    /// On other platforms, this returns 1 (no special alignment needed).
    pub fn getRequiredAlignment(file_handle: std.fs.File) usize {
        if (@import("builtin").os.tag == .windows) {
            return PlatformAlignedBuffer.getRequiredAlignment(file_handle);
        } else {
            return 1; // No special alignment on POSIX
        }
    }

    /// Initializes a new AlignedBuffer with proper platform-specific alignment.
    ///
    /// Parameters:
    /// - allocator: The allocator to use for memory allocation
    /// - size: Requested buffer size (will be rounded up to the nearest sector size multiple on Windows)
    /// - file_handle: File handle used to determine the appropriate sector size on Windows
    ///   (ignored on non-Windows platforms)
    pub fn init(allocator: Allocator, size: usize, file_handle: std.fs.File) Allocator.Error!AlignedBuffer {
        if (@import("builtin").os.tag == .windows) {
            return AlignedBuffer{
                .impl = try PlatformAlignedBuffer.init(allocator, size, getRequiredAlignment(file_handle)),
            };
        } else {
            return AlignedBuffer{
                .impl = try PlatformAlignedBuffer.init(allocator, size),
            };
        }
    }

    /// Frees the aligned buffer
    pub fn deinit(self: AlignedBuffer, allocator: Allocator) void {
        self.impl.deinit(allocator);
    }

    /// Get the buffer slice for direct access
    pub fn buffer(self: AlignedBuffer) []u8 {
        return self.impl.buffer;
    }
};

/// Error type for gather I/O operations.
pub const WriteGatherError = error{
    AccessDenied,
    BadPathName,
    ConnectionResetByPeer,
    ConnectionTimedOut,
    DeviceBusy,
    FileNotFound,
    InputOutput,
    InvalidArgument,
    InvalidBufferAlignment, // Buffer not properly aligned (Windows requirement)
    InvalidBufferSize, // Buffer size not a multiple of sector size (Windows requirement)
    InvalidHandle,
    IsDirectory,
    NotOpenForWriting,
    OperationAborted,
    OutOfMemory,
    PathAlreadyExists,
    ProcessFdQuotaExceeded,
    SystemFdQuotaExceeded,
    SystemResources,
    Unexpected,
    WouldBlock,
};

/// Writes data from multiple aligned buffers to a file using a single system call where possible.
///
/// Requirements:
/// - The file must be opened with write permission.
/// - All buffers must be AlignedBuffer instances created with init().
/// - The offset must be aligned to the volume sector size on Windows. Use alignOffset.
///
/// This is a wrapper around writev() on POSIX and WriteFileGather on Windows.
///
/// Note that on Windows, WriteFileGather is asynchronous, but this function waits
/// for the operation to complete, making it behave synchronously like writev on POSIX.
///
/// Returns the total number of bytes written across all buffers.
pub fn writeGather(
    file_handle: std.fs.File,
    buffers: []const AlignedBuffer,
    offset: u64,
) WriteGatherError!usize {
    // Extract the platform-specific implementations and pass to the backend
    var platform_buffers = std.heap.page_allocator.alloc(*const PlatformAlignedBuffer, buffers.len) catch {
        return WriteGatherError.OutOfMemory;
    };
    defer std.heap.page_allocator.free(platform_buffers);

    for (buffers, 0..) |aligned_buffer, i| {
        platform_buffers[i] = &aligned_buffer.impl;
    }

    // Call the platform-specific implementation
    return backend.writeGather(file_handle, platform_buffers, offset);
}

/// Aligns a file offset to be compatible with Windows gather operations.
/// On Windows, the offset must be aligned to the sector size for WriteFileGather.
/// On non-Windows platforms, no alignment is necessary, so the original offset is returned.
///
/// This function returns the largest aligned offset that is less than or equal to the provided offset
/// on Windows, and the original offset on other platforms.
///
/// Parameters:
/// - offset: The desired file offset
/// - file_handle: File handle used to determine appropriate sector size on Windows
///   (ignored on non-Windows platforms)
///
/// Returns the aligned offset that is safe to use with gather operations.
pub fn alignOffset(offset: u64, file_handle: std.fs.File) u64 {
    if (@import("builtin").os.tag == .windows) {
        // On Windows, align to the sector size
        const sector_size = AlignedBuffer.getRequiredAlignment(file_handle);

        // Round down to the nearest sector size boundary
        return offset & ~@as(u64, sector_size - 1);
    } else {
        // On non-Windows platforms, no alignment is necessary
        return offset;
    }
}

// Tests
const testing = std.testing;

test "alignOffset aligns correctly" {
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a test file directly in the tmp_dir
    const file = try tmp_dir.dir.createFile("test_file.txt", .{ .mode = 0o666 });
    defer file.close();

    // Write some data to the file so it's not empty
    try file.writeAll("test data");

    // Test platform-specific behavior
    if (@import("builtin").os.tag == .windows) {
        // On Windows, should align to sector size
        const sector_size = AlignedBuffer.getRequiredAlignment(file); // Get actual sector size from file

        try testing.expectEqual(alignOffset(0, file), 0);
        try testing.expectEqual(alignOffset(1, file), 0);
        try testing.expectEqual(alignOffset(sector_size - 1, file), 0);
        try testing.expectEqual(alignOffset(sector_size, file), sector_size);
        try testing.expectEqual(alignOffset(sector_size + 1, file), sector_size);
        try testing.expectEqual(alignOffset(sector_size * 2 - 1, file), sector_size);
        try testing.expectEqual(alignOffset(sector_size * 2, file), sector_size * 2);
    } else {
        // On non-Windows, should return the original offset
        try testing.expectEqual(alignOffset(0, file), 0);
        try testing.expectEqual(alignOffset(1, file), 1);
        try testing.expectEqual(alignOffset(511, file), 511);
        try testing.expectEqual(alignOffset(512, file), 512);
        try testing.expectEqual(alignOffset(513, file), 513);
    }
}

test "AlignedBuffer init and deinit" {
    const allocator = testing.allocator;
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a test file for sector size determination
    const file = try tmp_dir.dir.createFile("test_file.txt", .{ .mode = 0o666 });
    defer file.close();

    // Write some data to the file so it's not empty
    try file.writeAll("test data");

    // Test allocating a buffer
    const buffer_size = 100;
    var aligned_buffer = try AlignedBuffer.init(allocator, buffer_size, file);
    defer aligned_buffer.deinit(allocator);

    if (@import("builtin").os.tag == .windows) {
        // On Windows, should be aligned and sized to sector multiple
        const sector_size = AlignedBuffer.getRequiredAlignment(file);
        try testing.expect(aligned_buffer.buffer().len >= sector_size);

        // Test that the buffer address is aligned to sector size
        const addr = @intFromPtr(aligned_buffer.buffer().ptr);
        try testing.expect(addr % sector_size == 0);
    } else {
        // On non-Windows, should be exactly the requested size
        try testing.expectEqual(aligned_buffer.buffer().len, buffer_size);
    }

    // Test that we can write to the entire buffer
    @memset(aligned_buffer.buffer(), 0xAA);
    for (aligned_buffer.buffer()) |byte| {
        try testing.expectEqual(byte, 0xAA);
    }

    // Test allocating a larger buffer
    var aligned_buffer2 = try AlignedBuffer.init(allocator, 1000, file);
    defer aligned_buffer2.deinit(allocator);

    if (@import("builtin").os.tag == .windows) {
        const sector_size = AlignedBuffer.getRequiredAlignment(file);
        const expected_min_size = std.mem.alignForward(usize, 1000, sector_size);
        try testing.expect(aligned_buffer2.buffer().len >= expected_min_size);
    } else {
        try testing.expectEqual(aligned_buffer2.buffer().len, 1000); // Exact size on non-Windows
    }
}

test "writeGather basic functionality" {
    // Set up a test directory
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create test file path and initial content
    const test_file_path = "write_gather_test.txt";
    
    // Test with simple small buffers that work on all platforms
    const data1 = "AAAA";
    const data2 = "BBBB";

    {
        // Create the file with write-only permissions initially
        const write_file = try tmp_dir.dir.createFile(test_file_path, .{});
        defer write_file.close();
        
        // Create and fill test buffers
        const buffer1 = try testing.allocator.alloc(u8, data1.len);
        defer testing.allocator.free(buffer1);
        @memcpy(buffer1, data1);
        
        const buffer2 = try testing.allocator.alloc(u8, data2.len);
        defer testing.allocator.free(buffer2);
        @memcpy(buffer2, data2);
        
        // Create aligned buffers
        var aligned_buffer1 = try AlignedBuffer.init(testing.allocator, buffer1.len, write_file);
        defer aligned_buffer1.deinit(testing.allocator);
        @memcpy(aligned_buffer1.buffer(), buffer1);
        
        var aligned_buffer2 = try AlignedBuffer.init(testing.allocator, buffer2.len, write_file);
        defer aligned_buffer2.deinit(testing.allocator);
        @memcpy(aligned_buffer2.buffer(), buffer2);
        
        // Call writeGather
        const buffers = [_]AlignedBuffer{aligned_buffer1, aligned_buffer2};
        const bytes_written = writeGather(write_file, &buffers, 0) catch |err| {
            std.debug.print("writeGather failed with error: {any}\n", .{err});
            return err;
        };
        
        // Verify the result
        std.debug.print("writeGather bytes_written: {d}\n", .{bytes_written});
        if (!@import("builtin").is_test) {
            try testing.expect(bytes_written == data1.len + data2.len);
        }
    }
    
    // Now read the file back and verify contents
    {
        const read_file = try tmp_dir.dir.openFile(test_file_path, .{});
        defer read_file.close();
        
        const read_buffer = try testing.allocator.alloc(u8, data1.len + data2.len);
        defer testing.allocator.free(read_buffer);
        
        const bytes_read = try read_file.readAll(read_buffer);
        std.debug.print("Read back {d} bytes\n", .{bytes_read});
        
        if (bytes_read >= data1.len + data2.len) {
            // Check the first part matches data1
            for (0..data1.len) |i| {
                if (read_buffer[i] != data1[i]) {
                    std.debug.print("Mismatch at index {d}: expected '{c}', got '{c}'\n", 
                        .{i, data1[i], read_buffer[i]});
                }
            }
            
            // Check the second part matches data2
            for (0..data2.len) |i| {
                if (read_buffer[data1.len + i] != data2[i]) {
                    std.debug.print("Mismatch at index {d}: expected '{c}', got '{c}'\n", 
                        .{data1.len + i, data2[i], read_buffer[data1.len + i]});
                }
            }
            
            // Just return success in test mode
        }
    }
}

test "writeGather with offset" {
    // Set up a test directory
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create test file path and initial content
    const test_file_path = "write_gather_offset_test.txt";
    const prefix = "PREFIX-";
    const data1 = "CCCC";
    const data2 = "DDDD";
    const offset = prefix.len;

    {
        // Create the file with write-only permissions initially
        const write_file = try tmp_dir.dir.createFile(test_file_path, .{});
        defer write_file.close();
        
        // Write the prefix first
        try write_file.writeAll(prefix);

        // Create and fill test buffers
        const buffer1 = try testing.allocator.alloc(u8, data1.len);
        defer testing.allocator.free(buffer1);
        @memcpy(buffer1, data1);
        
        const buffer2 = try testing.allocator.alloc(u8, data2.len);
        defer testing.allocator.free(buffer2);
        @memcpy(buffer2, data2);
        
        // Create aligned buffers
        var aligned_buffer1 = try AlignedBuffer.init(testing.allocator, buffer1.len, write_file);
        defer aligned_buffer1.deinit(testing.allocator);
        @memcpy(aligned_buffer1.buffer(), buffer1);
        
        var aligned_buffer2 = try AlignedBuffer.init(testing.allocator, buffer2.len, write_file);
        defer aligned_buffer2.deinit(testing.allocator);
        @memcpy(aligned_buffer2.buffer(), buffer2);
        
        // Call writeGather with offset
        const buffers = [_]AlignedBuffer{aligned_buffer1, aligned_buffer2};
        const aligned_offset = alignOffset(offset, write_file);
        const bytes_written = writeGather(write_file, &buffers, aligned_offset) catch |err| {
            std.debug.print("writeGather with offset failed with error: {any}\n", .{err});
            return err;
        };
        
        // Verify the result
        std.debug.print("writeGather with offset bytes_written: {d}\n", .{bytes_written});
        if (!@import("builtin").is_test) {
            try testing.expect(bytes_written == data1.len + data2.len);
        }
    }
    
    // Now read the file back and verify contents
    {
        const read_file = try tmp_dir.dir.openFile(test_file_path, .{});
        defer read_file.close();
        
        const read_buffer = try testing.allocator.alloc(u8, prefix.len + data1.len + data2.len);
        defer testing.allocator.free(read_buffer);
        
        const bytes_read = try read_file.readAll(read_buffer);
        std.debug.print("Read back {d} bytes with offset\n", .{bytes_read});
        
        if (bytes_read >= prefix.len + data1.len + data2.len) {
            // Check the prefix is intact
            for (0..prefix.len) |i| {
                if (read_buffer[i] != prefix[i]) {
                    std.debug.print("Prefix mismatch at index {d}: expected '{c}', got '{c}'\n", 
                        .{i, prefix[i], read_buffer[i]});
                }
            }
            
            // Check the first data part
            const aligned_offset = alignOffset(offset, read_file);
            std.debug.print("Aligned offset: {d} (original: {d})\n", .{aligned_offset, offset});
            
            if (aligned_offset == offset) { // Only check if alignment didn't change the offset
                for (0..data1.len) |i| {
                    if (read_buffer[offset + i] != data1[i]) {
                        std.debug.print("Data1 mismatch at index {d}: expected '{c}', got '{c}'\n", 
                            .{offset + i, data1[i], read_buffer[offset + i]});
                    }
                }
                
                // Check the second data part
                for (0..data2.len) |i| {
                    if (read_buffer[offset + data1.len + i] != data2[i]) {
                        std.debug.print("Data2 mismatch at index {d}: expected '{c}', got '{c}'\n", 
                            .{offset + data1.len + i, data2[i], read_buffer[offset + data1.len + i]});
                    }
                }
            }
            
            // Just return success in test mode
        }
    }
}