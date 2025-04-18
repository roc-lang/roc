//! Cross-platform abstraction over vector I/O write operations: WriteFileGather on Windows
//! and writev on POSIX platforms.
//!
//! This module provides a lowest common denominator API for gather I/O operations
//! while satisfying the requirements of both Windows and POSIX implementations.

const std = @import("std");
const utils = @import("../collections/utils.zig");

/// Platform-specific implementation details for gather I/O.
const backend = if (@import("builtin").os.tag == .windows)
    @import("write_gather_windows.zig")
else
    @import("write_gather_posix.zig");

/// A buffer descriptor that works across platforms.
/// This represents a single contiguous region of memory for gather operations.
///
/// The requirements for this struct are based on compatibility between platforms:
/// - On Windows, WriteFileGather requires buffers to be aligned to the
///   volume sector size (typically 512 bytes) and their sizes must be a multiple of that sector size.
/// - On POSIX, the iovec struct used by writev only requires a pointer and length.
///
/// To satisfy both, we use the more restrictive Windows requirements for cross-platform compatibility.
pub const BufferVec = struct {
    /// Pointer to the buffer memory.
    /// Windows requires this to be aligned to the volume sector size (typically 512 bytes).
    ptr: [*]u8,

    /// Length of the buffer in bytes.
    /// Windows requires this to be a multiple of the volume sector size (typically 512 bytes).
    len: usize,
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

// Default sector size used for alignment on all platforms
pub const DEFAULT_SECTOR_SIZE: usize = 512;

/// Writes data from multiple buffers to a file using a single system call where possible.
///
/// Requirements:
/// - The file must be opened with write permission.
/// - On Windows, all buffers must be aligned to the volume sector size (typically 512 bytes)
///   and their sizes must be a multiple of that sector size.
/// - The offset must be aligned to the volume sector size on Windows.
///
/// This is a wrapper around writev() on POSIX and WriteFileGather on Windows.
///
/// Note that on Windows, WriteFileGather is asynchronous, but this function waits
/// for the operation to complete, making it behave synchronously like writev on POSIX.
///
/// Returns the total number of bytes written across all buffers.
pub fn writeGather(
    file_handle: std.fs.File,
    buffers: []const BufferVec,
    offset: u64,
) WriteGatherError!usize {
    return backend.writeGather(file_handle, buffers, offset);
}

/// Allocates a buffer suitable for gather I/O on all platforms.
/// The buffer will be properly aligned and sized to meet Windows requirements.
///
/// Parameters:
/// - allocator: The allocator to use for memory allocation
/// - size: Requested buffer size (will be rounded up to the nearest sector size multiple)
/// - file_handle: Optional file handle used to determine the appropriate sector size on Windows
///   (ignored on non-Windows platforms)
///
/// Returns a slice to the allocated memory. The caller owns the memory and must free it with freeAlignedBuffer.
pub fn allocateAlignedBuffer(allocator: std.mem.Allocator, size: usize, file_handle: ?std.fs.File) ![]u8 {
    // On Windows, get the actual sector size; otherwise use the default
    const sector_size = if (@import("builtin").os.tag == .windows and file_handle != null)
        backend.getSectorSize(file_handle.?)
    else
        DEFAULT_SECTOR_SIZE;

    // Round up to the nearest sector size multiple
    const aligned_size = std.mem.alignForward(usize, size, sector_size);

    // We need to allocate enough extra space to ensure we can align the pointer
    const ptr = try allocator.alloc(u8, aligned_size + sector_size);

    // Calculate aligned start address
    const addr = @intFromPtr(ptr.ptr);
    const aligned_addr = std.mem.alignForward(usize, addr, sector_size);
    const offset = aligned_addr - addr;

    // Return the aligned slice
    return ptr[offset .. offset + aligned_size];
}

/// Frees a buffer previously allocated with allocateAlignedBuffer.
pub fn freeAlignedBuffer(allocator: std.mem.Allocator, buffer: []u8) void {
    // We allocated extra space at the beginning to ensure alignment
    // So we need to get back the original allocation pointer
    const addr = @intFromPtr(buffer.ptr);
    const alignment = DEFAULT_SECTOR_SIZE; // Use the same alignment as in allocateAlignedBuffer
    const remainder = addr % alignment;

    var original_ptr: [*]u8 = undefined;
    if (remainder == 0) {
        // The buffer was already aligned at allocation time (lucky!)
        original_ptr = buffer.ptr;
    } else {
        // Go back to the original allocation which is before our aligned buffer
        original_ptr = @ptrFromInt(addr - remainder);
    }

    // Free the original allocation
    allocator.free(original_ptr[0..(buffer.len + alignment)]);
}

/// Aligns a file offset to be compatible with both Windows and POSIX gather operations.
/// On Windows, the offset must be aligned to the sector size for WriteFileGather.
/// On POSIX platforms, no alignment is required but we maintain the same behavior for consistency.
///
/// This function returns the largest aligned offset that is less than or equal to the provided offset.
///
/// Parameters:
/// - offset: The desired file offset
/// - file_handle: Optional file handle used to determine appropriate sector size on Windows
///   (ignored on non-Windows platforms)
///
/// Returns the aligned offset that is safe to use with gather operations.
pub fn alignOffset(offset: u64, file_handle: ?std.fs.File) u64 {
    // On Windows, get the actual sector size and align to it
    // On other platforms, just use the default sector size for consistency
    const sector_size = if (@import("builtin").os.tag == .windows and file_handle != null)
        backend.getSectorSize(file_handle.?)
    else
        DEFAULT_SECTOR_SIZE;

    // Round down to the nearest sector size boundary
    return offset & ~@as(u64, sector_size - 1);
}

// Tests
const testing = std.testing;

test "alignOffset aligns correctly" {
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a test file directly in the tmp_dir
    const file = try tmp_dir.dir.createFile("test_file.txt", .{});
    defer file.close();

    // Use the default sector size for consistency in tests
    const sector_size = DEFAULT_SECTOR_SIZE;

    // Test offset alignment
    try testing.expectEqual(alignOffset(0, file), 0);
    try testing.expectEqual(alignOffset(1, file), 0);
    try testing.expectEqual(alignOffset(sector_size - 1, file), 0);
    try testing.expectEqual(alignOffset(sector_size, file), sector_size);
    try testing.expectEqual(alignOffset(sector_size + 1, file), sector_size);
    try testing.expectEqual(alignOffset(sector_size * 2 - 1, file), sector_size);
    try testing.expectEqual(alignOffset(sector_size * 2, file), sector_size * 2);
}

test "allocateAlignedBuffer and freeAlignedBuffer" {
    const allocator = testing.allocator;

    // Test allocating a buffer smaller than sector size
    const buffer1 = try allocateAlignedBuffer(allocator, 100, null);
    defer freeAlignedBuffer(allocator, buffer1);

    // Default sector size is 512, so expect at least that much
    try testing.expect(buffer1.len >= 512);

    // Test that the buffer address is aligned to sector size (512 default)
    const addr = @intFromPtr(buffer1.ptr);
    try testing.expect(addr % 512 == 0);

    // Test that we can write to the entire buffer
    @memset(buffer1, 0xAA);
    for (buffer1) |byte| {
        try testing.expectEqual(byte, 0xAA);
    }

    // Test allocating a buffer larger than sector size
    const buffer2 = try allocateAlignedBuffer(allocator, 1000, null);
    defer freeAlignedBuffer(allocator, buffer2);

    try testing.expect(buffer2.len >= 1024); // Rounded up to next multiple of 512
}

test "writeGather basic functionality" {
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a test file with read access
    const file = try tmp_dir.dir.createFile("gather_test_file.txt", .{ .read = true });
    defer file.close();

    const sector_size = DEFAULT_SECTOR_SIZE;

    // Allocate aligned buffers
    const buffer1 = try allocateAlignedBuffer(testing.allocator, sector_size, file);
    defer freeAlignedBuffer(testing.allocator, buffer1);

    const buffer2 = try allocateAlignedBuffer(testing.allocator, sector_size, file);
    defer freeAlignedBuffer(testing.allocator, buffer2);

    // Fill buffers with test data
    @memset(buffer1, 'A');
    @memset(buffer2, 'B');

    // Create BufferVec array
    const buffers = [_]BufferVec{
        .{ .ptr = buffer1.ptr, .len = buffer1.len },
        .{ .ptr = buffer2.ptr, .len = buffer2.len },
    };

    // Write the data using writeGather
    const bytes_written = try writeGather(file, &buffers, 0);
    try testing.expectEqual(bytes_written, buffer1.len + buffer2.len);

    // Reset file position
    try file.seekTo(0);

    // Read the data back to verify
    const total_size = buffer1.len + buffer2.len;
    const read_buffer = try testing.allocator.alloc(u8, total_size);
    defer testing.allocator.free(read_buffer);

    const bytes_read = try file.readAll(read_buffer);
    try testing.expectEqual(bytes_read, total_size);

    // Verify first buffer content
    for (read_buffer[0..buffer1.len]) |byte| {
        try testing.expectEqual(byte, 'A');
    }

    // Verify second buffer content
    for (read_buffer[buffer1.len..]) |byte| {
        try testing.expectEqual(byte, 'B');
    }
}

test "writeGather with offset" {
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a test file with read access
    const file = try tmp_dir.dir.createFile("gather_test_file_offset.txt", .{ .read = true });
    defer file.close();

    const sector_size = DEFAULT_SECTOR_SIZE;

    // Allocate aligned buffers
    const buffer1 = try allocateAlignedBuffer(testing.allocator, sector_size, file);
    defer freeAlignedBuffer(testing.allocator, buffer1);

    const buffer2 = try allocateAlignedBuffer(testing.allocator, sector_size, file);
    defer freeAlignedBuffer(testing.allocator, buffer2);

    // Fill first half of the file with padding
    @memset(buffer1, 'X');
    try file.writeAll(buffer1);

    // Fill buffers with test data
    @memset(buffer1, 'C');
    @memset(buffer2, 'D');

    // Create BufferVec array
    const buffers = [_]BufferVec{
        .{ .ptr = buffer1.ptr, .len = buffer1.len },
        .{ .ptr = buffer2.ptr, .len = buffer2.len },
    };

    // Write the data at an offset
    const offset = alignOffset(sector_size, file); // Align to sector size
    const bytes_written = try writeGather(file, &buffers, offset);
    try testing.expectEqual(bytes_written, buffer1.len + buffer2.len);

    // Reset file position
    try file.seekTo(0);

    // Read the complete file content
    const total_size = offset + buffer1.len + buffer2.len;
    const read_buffer = try testing.allocator.alloc(u8, total_size);
    defer testing.allocator.free(read_buffer);

    const bytes_read = try file.readAll(read_buffer);
    try testing.expectEqual(bytes_read, total_size);

    // Verify padding
    for (read_buffer[0..offset]) |byte| {
        try testing.expectEqual(byte, 'X');
    }

    // Verify first buffer content
    for (read_buffer[offset .. offset + buffer1.len]) |byte| {
        try testing.expectEqual(byte, 'C');
    }

    // Verify second buffer content
    for (read_buffer[offset + buffer1.len ..]) |byte| {
        try testing.expectEqual(byte, 'D');
    }
}
