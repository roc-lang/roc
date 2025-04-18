//! Cross-platform abstraction over vector I/O operations: ReadFileScatter/WriteFileGather on Windows
//! and readv/writev on POSIX platforms.
//!
//! This module provides a lowest common denominator API for scatter/gather I/O operations
//! while satisfying the requirements of both Windows and POSIX implementations.

const std = @import("std");
const utils = @import("../collections/utils.zig");

/// Platform-specific implementation details for scatter/gather I/O.
const backend = if (std.Target.current.os.tag == .windows) 
    @import("io/scatter_gather_windows.zig") 
    else 
    @import("io/scatter_gather_posix.zig");

/// A buffer descriptor that works across platforms.
/// This represents a single contiguous region of memory for scatter/gather operations.
///
/// The requirements for this struct are based on compatibility between platforms:
/// - On Windows, ReadFileScatter/WriteFileGather requires buffers to be aligned to the 
///   volume sector size (typically 512 bytes) and their sizes must be a multiple of that sector size.
/// - On POSIX, the iovec struct used by readv/writev only requires a pointer and length.
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

/// Error type for scatter/gather I/O operations.
pub const ScatterGatherError = error{
    AccessDenied,
    BadPathName,
    ConnectionResetByPeer,
    ConnectionTimedOut,
    DeviceBusy,
    FileNotFound,
    InputOutput,
    InvalidArgument,
    InvalidBufferAlignment,   // Buffer not properly aligned (Windows requirement)
    InvalidBufferSize,        // Buffer size not a multiple of sector size (Windows requirement)
    InvalidHandle,
    IsDirectory,
    NotOpenForReading,
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

/// Returns the file system's sector size, which is the required alignment
/// and size multiple for scatter/gather I/O on Windows.
/// 
/// POSIX systems don't have this requirement, but we need to satisfy Windows
/// for cross-platform compatibility.
///
/// This should be used to properly allocate and align buffers for scatter/gather I/O.
/// 
/// Note: This function never fails - it will return a default sector size (512 bytes)
/// if the actual sector size cannot be determined.
pub fn getSectorSize(file_handle: std.fs.File) usize {
    return backend.getSectorSize(file_handle);
}

/// Reads data from a file into multiple buffers using a single system call where possible.
/// 
/// Requirements:
/// - The file must be opened with read permission.
/// - On Windows, all buffers must be aligned to the volume sector size (typically 512 bytes)
///   and their sizes must be a multiple of that sector size.
/// - The offset must be aligned to the volume sector size on Windows.
///
/// This is a wrapper around readv() on POSIX and ReadFileScatter on Windows.
///
/// Note that on Windows, ReadFileScatter is asynchronous, but this function waits
/// for the operation to complete, making it behave synchronously like readv on POSIX.
///
/// Returns the total number of bytes read across all buffers.
pub fn readScatter(
    file_handle: std.fs.File,
    buffers: []const BufferVec,
    offset: u64,
) ScatterGatherError!usize {
    return backend.readScatter(file_handle, buffers, offset);
}

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
) ScatterGatherError!usize {
    return backend.writeGather(file_handle, buffers, offset);
}

/// Allocates a buffer suitable for scatter/gather I/O on all platforms.
/// The buffer will be properly aligned and sized to meet Windows requirements.
///
/// Parameters:
/// - allocator: The allocator to use for memory allocation
/// - size: Requested buffer size (will be rounded up to the nearest sector size multiple)
/// - file_handle: Optional file handle used to determine the appropriate sector size
///
/// Returns a slice to the allocated memory. The caller owns the memory and must free it.
pub fn allocateAlignedBuffer(allocator: std.mem.Allocator, size: usize, file_handle: ?std.fs.File) ![]u8 {
    const sector_size = if (file_handle) |handle| getSectorSize(handle) else 512;
    
    // Round up to the nearest sector size multiple
    const aligned_size = std.mem.alignForward(size, sector_size);
    
    // Allocate aligned memory
    return allocator.alignedAlloc(u8, sector_size, aligned_size) catch |err| switch (err) {
        error.OutOfMemory => return ScatterGatherError.OutOfMemory,
        else => return ScatterGatherError.Unexpected,
    };
}

/// Frees a buffer previously allocated with allocateAlignedBuffer.
pub fn freeAlignedBuffer(allocator: std.mem.Allocator, buffer: []u8) void {
    allocator.free(buffer);
}

/// Aligns a file offset to be compatible with both Windows and POSIX scatter/gather operations.
/// On Windows, the offset must be aligned to the sector size for ReadFileScatter/WriteFileGather.
/// This function returns the largest aligned offset that is less than or equal to the provided offset.
///
/// Parameters:
/// - offset: The desired file offset
/// - file_handle: File handle used to determine the appropriate sector size
///
/// Returns the aligned offset that is safe to use with scatter/gather operations.
pub fn alignOffset(offset: u64, file_handle: std.fs.File) u64 {
    const sector_size = getSectorSize(file_handle);
    // Round down to the nearest sector size boundary
    return offset & ~@as(u64, sector_size - 1);
}