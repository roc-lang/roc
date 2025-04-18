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

    /// Initializes a new AlignedBuffer with proper platform-specific alignment.
    /// 
    /// Parameters:
    /// - allocator: The allocator to use for memory allocation
    /// - size: Requested buffer size (will be rounded up to the nearest sector size multiple on Windows)
    /// - file_handle: File handle used to determine the appropriate sector size on Windows
    ///   (ignored on non-Windows platforms)
    pub fn init(allocator: Allocator, size: usize, file_handle: std.fs.File) Allocator.Error!AlignedBuffer {
        return AlignedBuffer{
            .impl = try PlatformAlignedBuffer.init(allocator, size, file_handle),
        };
    }

    /// Frees the aligned buffer
    pub fn deinit(self: AlignedBuffer, allocator: Allocator) void {
        self.impl.deinit(allocator);
    }

    /// Get the buffer slice for direct access
    pub fn buffer(self: AlignedBuffer) []u8 {
        return self.impl.buffer;
    }

    /// Converts this AlignedBuffer to a BufferVec for internal use
    fn toBufferVec(self: AlignedBuffer) BufferVec {
        return self.impl.toBufferVec();
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
    // Convert AlignedBuffer array to BufferVec array for the backend
    var buffer_vecs = std.heap.page_allocator.alloc(BufferVec, buffers.len) catch {
        return WriteGatherError.OutOfMemory;
    };
    defer std.heap.page_allocator.free(buffer_vecs);

    var total_size: usize = 0;
    for (buffers, 0..) |aligned_buffer, i| {
        buffer_vecs[i] = aligned_buffer.toBufferVec();
        total_size += aligned_buffer.buffer().len;
    }

    // Call the platform-specific implementation
    return backend.writeGather(file_handle, buffer_vecs, offset);
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
        const sector_size = backend.getSectorSize(file_handle);

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
    const file = try tmp_dir.dir.createFile("test_file.txt", .{});
    defer file.close();

    // Test platform-specific behavior
    if (@import("builtin").os.tag == .windows) {
        // On Windows, should align to sector size
        const sector_size = backend.getSectorSize(file); // Get actual sector size from file

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
    const file = try tmp_dir.dir.createFile("test_file.txt", .{});
    defer file.close();

    // Test allocating a buffer
    const buffer_size = 100;
    var aligned_buffer = try AlignedBuffer.init(allocator, buffer_size, file);
    defer aligned_buffer.deinit(allocator);

    if (@import("builtin").os.tag == .windows) {
        // On Windows, should be aligned and sized to sector multiple
        const sector_size = backend.getSectorSize(file);
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
        const sector_size = backend.getSectorSize(file);
        const expected_min_size = std.mem.alignForward(usize, 1000, sector_size);
        try testing.expect(aligned_buffer2.buffer().len >= expected_min_size);
    } else {
        try testing.expectEqual(aligned_buffer2.buffer().len, 1000); // Exact size on non-Windows
    }
}

test "writeGather basic functionality" {
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a test file with read access
    const file = try tmp_dir.dir.createFile("gather_test_file.txt", .{ .read = true });
    defer file.close();

    // Determine buffer size based on platform
    const buffer_size = if (@import("builtin").os.tag == .windows) 512 else 256;

    // Allocate AlignedBuffers
    var aligned_buffer1 = try AlignedBuffer.init(testing.allocator, buffer_size, file);
    defer aligned_buffer1.deinit(testing.allocator);

    var aligned_buffer2 = try AlignedBuffer.init(testing.allocator, buffer_size, file);
    defer aligned_buffer2.deinit(testing.allocator);

    // Fill buffers with test data
    @memset(aligned_buffer1.buffer(), 'A');
    @memset(aligned_buffer2.buffer(), 'B');

    // Create AlignedBuffer array
    const buffers = [_]AlignedBuffer{
        aligned_buffer1,
        aligned_buffer2,
    };

    // Write the data using writeGather
    const bytes_written = try writeGather(file, &buffers, 0);
    try testing.expectEqual(bytes_written, aligned_buffer1.buffer().len + aligned_buffer2.buffer().len);

    // Reset file position
    try file.seekTo(0);

    // Read the data back to verify
    const total_size = aligned_buffer1.buffer().len + aligned_buffer2.buffer().len;
    const read_buffer = try testing.allocator.alloc(u8, total_size);
    defer testing.allocator.free(read_buffer);

    const bytes_read = try file.readAll(read_buffer);
    try testing.expectEqual(bytes_read, total_size);

    // Verify first buffer content
    for (read_buffer[0..aligned_buffer1.buffer().len]) |byte| {
        try testing.expectEqual(byte, 'A');
    }

    // Verify second buffer content
    for (read_buffer[aligned_buffer1.buffer().len..]) |byte| {
        try testing.expectEqual(byte, 'B');
    }
}

test "writeGather with offset" {
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a test file with read access
    const file = try tmp_dir.dir.createFile("gather_test_file_offset.txt", .{ .read = true });
    defer file.close();

    // Determine buffer size based on platform
    const buffer_size = if (@import("builtin").os.tag == .windows) 512 else 256;

    // Allocate AlignedBuffers
    var padding_buffer = try AlignedBuffer.init(testing.allocator, buffer_size, file);
    defer padding_buffer.deinit(testing.allocator);

    var aligned_buffer1 = try AlignedBuffer.init(testing.allocator, buffer_size, file);
    defer aligned_buffer1.deinit(testing.allocator);

    var aligned_buffer2 = try AlignedBuffer.init(testing.allocator, buffer_size, file);
    defer aligned_buffer2.deinit(testing.allocator);

    // Fill first half of the file with padding
    @memset(padding_buffer.buffer(), 'X');
    try file.writeAll(padding_buffer.buffer());

    // Fill buffers with test data
    @memset(aligned_buffer1.buffer(), 'C');
    @memset(aligned_buffer2.buffer(), 'D');

    // Create AlignedBuffer array
    const buffers = [_]AlignedBuffer{
        aligned_buffer1,
        aligned_buffer2,
    };

    // Write the data at an offset (aligned on Windows, as-is on other platforms)
    const offset = alignOffset(buffer_size, file);
    const bytes_written = try writeGather(file, &buffers, offset);
    try testing.expectEqual(bytes_written, aligned_buffer1.buffer().len + aligned_buffer2.buffer().len);

    // Reset file position
    try file.seekTo(0);

    // Read the complete file content
    const total_size = offset + aligned_buffer1.buffer().len + aligned_buffer2.buffer().len;
    const read_buffer = try testing.allocator.alloc(u8, total_size);
    defer testing.allocator.free(read_buffer);

    const bytes_read = try file.readAll(read_buffer);
    try testing.expectEqual(bytes_read, total_size);

    // Verify padding
    for (read_buffer[0..offset]) |byte| {
        try testing.expectEqual(byte, 'X');
    }

    // Verify first buffer content
    for (read_buffer[offset .. offset + aligned_buffer1.buffer().len]) |byte| {
        try testing.expectEqual(byte, 'C');
    }

    // Verify second buffer content
    for (read_buffer[offset + aligned_buffer1.buffer().len ..]) |byte| {
        try testing.expectEqual(byte, 'D');
    }
}