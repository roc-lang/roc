//! POSIX-specific implementation of gather I/O using writev.

const std = @import("std");
const os = std.os;
const posix = std.posix;

const write_gather = @import("write_gather.zig");
const WriteGatherError = write_gather.WriteGatherError;
const BufferVec = write_gather.BufferVec;

/// Gets the sector size for a given file handle.
///
/// On POSIX systems, there's no alignment requirement for writev,
/// but we return a value to maintain the cross-platform interface.
/// By default, we'll return 512 bytes, which is a common disk sector size.
///
/// Note: This function never fails - it returns a default sector size (512 bytes) if
/// the actual sector size cannot be determined.
pub fn getSectorSize(file_handle: std.fs.File) usize {
    if (@import("builtin").os.tag == .macos) {
        // Just return 512 on macOS since fcntl access is complex
        return 512;
    } else {
        var stat: posix.Statfs = undefined;

        if (posix.statfs(file_handle.handle, &stat)) |_| {
            return stat.bsize;
        } else |_| {
            // Fallback to common disk sector size
            return 512;
        }
    }
}

/// Convert BufferVec array to iovec array for POSIX writev
fn toIovecs(buffers: []const BufferVec, iovecs: []posix.iovec_const) void {
    for (buffers, 0..) |buffer, i| {
        iovecs[i] = .{
            .base = buffer.ptr,
            .len = buffer.len,
        };
    }
}

/// Check if the platform supports pwritev function
fn hasPwritev() bool {
    const builtin = @import("builtin");
    // Check if Linux - which supports pwritev
    if (builtin.os.tag == .linux) return true;
    
    // Check if FreeBSD - which also supports pwritev
    if (builtin.os.tag == .freebsd) return true;
    
    // On other platforms, we'll use lseek + writev instead
    return false;
}

/// Implementation of writeGather for POSIX using pwritev when available, falling back to writev
pub fn writeGather(
    file_handle: std.fs.File,
    buffers: []const BufferVec,
    offset: u64,
) WriteGatherError!usize {
    // For POSIX, we need to convert BufferVec to iovec
    const iovecs = std.heap.page_allocator.alloc(posix.iovec_const, buffers.len) catch {
        return WriteGatherError.OutOfMemory;
    };
    defer std.heap.page_allocator.free(iovecs);

    // Convert BufferVec array to iovec array
    toIovecs(buffers, iovecs);

    // Platform has pwritev (Linux, FreeBSD) - use it to avoid changing file position
    if (hasPwritev()) {
        // Implement platform-specific pwritev call
        const result = switch (@import("builtin").os.tag) {
            .linux => std.os.linux.pwritev(file_handle.handle, iovecs, offset),
            .freebsd => std.os.freebsd.pwritev(file_handle.handle, iovecs, offset),
            else => unreachable, // Should be caught by hasPwritev
        } catch |err| return translateError(err);
    
        return @intCast(result);
    } else {
        // Fall back to lseek + writev for platforms without pwritev
        // Find the current position directly using C functions
        const SEEK_CUR = 1; // C constant for current position
        const original_position = std.c.lseek(file_handle.handle, 0, SEEK_CUR);
        if (original_position < 0) {
            return WriteGatherError.InputOutput;
        }
        
        // Seek to the requested position
        const SEEK_SET = 0; // C constant for beginning of file
        const seekResult = std.c.lseek(file_handle.handle, @intCast(offset), SEEK_SET);
        if (seekResult < 0) {
            return WriteGatherError.InputOutput;
        }
    
        // Perform the gather write operation
        const bytes_written = posix.writev(file_handle.handle, iovecs) catch |err| {
            // Try to restore the original position (best effort)
            _ = std.c.lseek(file_handle.handle, original_position, SEEK_SET);
        
            return translateError(err);
        };
    
        // Restore the original position
        _ = std.c.lseek(file_handle.handle, original_position, SEEK_SET);
        // If we can't restore the position but the write succeeded, we'll still return success
        // but future operations on this file handle might be affected
    
        return @intCast(bytes_written);
    }
}

/// Translates POSIX-specific errors to WriteGatherError
fn translateError(err: anyerror) WriteGatherError {
    return switch (err) {
        error.AccessDenied => WriteGatherError.AccessDenied,
        error.BadPathName => WriteGatherError.BadPathName,
        error.ConnectionResetByPeer => WriteGatherError.ConnectionResetByPeer,
        error.ConnectionTimedOut => WriteGatherError.ConnectionTimedOut,
        error.DeviceBusy => WriteGatherError.DeviceBusy,
        error.FileNotFound => WriteGatherError.FileNotFound,
        error.InputOutput => WriteGatherError.InputOutput,
        error.InvalidArgument => WriteGatherError.InvalidArgument,
        error.IsDir => WriteGatherError.IsDirectory,
        error.NotOpenForWriting => WriteGatherError.NotOpenForWriting,
        error.OperationAborted => WriteGatherError.OperationAborted,
        error.OutOfMemory => WriteGatherError.OutOfMemory,
        error.PathAlreadyExists => WriteGatherError.PathAlreadyExists,
        error.ProcessFdQuotaExceeded => WriteGatherError.ProcessFdQuotaExceeded,
        error.SystemFdQuotaExceeded => WriteGatherError.SystemFdQuotaExceeded,
        error.SystemResources => WriteGatherError.SystemResources,
        error.WouldBlock => WriteGatherError.WouldBlock,
        else => WriteGatherError.Unexpected,
    };
}