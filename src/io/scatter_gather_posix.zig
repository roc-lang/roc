//! POSIX-specific implementation of scatter/gather I/O using readv and writev.

const std = @import("std");
const os = std.os;
const posix = std.os.posix;

const io = @import("../io.zig");
const ScatterGatherError = io.ScatterGatherError;
const BufferVec = io.BufferVec;

/// Gets the sector size for a given file handle.
///
/// On POSIX systems, there's no alignment requirement for readv/writev,
/// but we return a value to maintain the cross-platform interface.
/// By default, we'll return 512 bytes, which is a common disk sector size.
///
/// Note: This function never fails - it returns a default sector size (512 bytes) if
/// the actual sector size cannot be determined.
pub fn getSectorSize(file_handle: std.fs.File) usize {
    if (std.Target.current.isDarwin()) {
        const F_GETBLKSIZE = 106;
        const rc = os.fcntl(file_handle.handle, F_GETBLKSIZE, 0) catch return 512;

        return @intCast(rc);
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

/// Convert BufferVec array to iovec array for POSIX readv/writev
fn toIovecs(buffers: []const BufferVec, iovecs: []posix.iovec) void {
    for (buffers, 0..) |buffer, i| {
        iovecs[i] = .{
            .iov_base = @constCast(buffer.ptr),
            .iov_len = buffer.len,
        };
    }
}

/// Check if the platform supports preadv/pwritev functions
fn hasPreadv() bool {
    // Check if Linux - which supports preadv/pwritev
    if (std.Target.current.os.tag == .linux) return true;
    
    // Check if FreeBSD - which also supports preadv/pwritev
    if (std.Target.current.os.tag == .freebsd) return true;
    
    // On other platforms, we'll use lseek + readv/writev instead
    return false;
}

/// Implementation of readScatter for POSIX using preadv when available, falling back to readv
pub fn readScatter(
    file_handle: std.fs.File,
    buffers: []const BufferVec,
    offset: u64,
) ScatterGatherError!usize {
    const iovecs = std.heap.page_allocator.alloc(posix.iovec, buffers.len) catch {
        return ScatterGatherError.OutOfMemory;
    };
    defer std.heap.page_allocator.free(iovecs);

    toIovecs(buffers, iovecs);

    // Platform has preadv (Linux, FreeBSD) - use it to avoid changing file position
    if (hasPreadv()) {
        // Implement platform-specific preadv call
        const result = switch (std.Target.current.os.tag) {
            .linux => std.os.linux.preadv(file_handle.handle, iovecs, offset),
            .freebsd => std.os.freebsd.preadv(file_handle.handle, iovecs, offset),
            else => unreachable, // Should be caught by hasPreadv
        } catch |err| return translateError(err);
        
        return @intCast(result);
    } else {
        // Fall back to lseek + readv for platforms without preadv
        const original_position = posix.lseek(file_handle.handle, 0, posix.SEEK.CUR) catch {
            return ScatterGatherError.InputOutput;
        };

        // Seek to the requested position
        _ = posix.lseek(file_handle.handle, @intCast(offset), posix.SEEK.SET) catch {
            return ScatterGatherError.InputOutput;
        };

        // Perform the scatter read operation
        const bytes_read = posix.readv(file_handle.handle, iovecs) catch |err| {
            // Try to restore the original position (best effort)
            _ = posix.lseek(file_handle.handle, original_position, posix.SEEK.SET) catch {};

            return translateError(err);
        };

        // Restore the original position
        _ = posix.lseek(file_handle.handle, original_position, posix.SEEK.SET) catch {
            // If we can't restore the position but the read succeeded, we'll still return success
            // but future operations on this file handle might be affected
        };
        
        return @intCast(bytes_read);
    }
}

/// Implementation of writeGather for POSIX using pwritev when available, falling back to writev
pub fn writeGather(
    file_handle: std.fs.File,
    buffers: []const BufferVec,
    offset: u64,
) ScatterGatherError!usize {
    // For POSIX, we need to convert BufferVec to iovec
    const iovecs = std.heap.page_allocator.alloc(posix.iovec, buffers.len) catch {
        return ScatterGatherError.OutOfMemory;
    };
    defer std.heap.page_allocator.free(iovecs);

    // Convert BufferVec array to iovec array
    toIovecs(buffers, iovecs);

    // Platform has pwritev (Linux, FreeBSD) - use it to avoid changing file position
    if (hasPreadv()) {
        // Implement platform-specific pwritev call
        const result = switch (std.Target.current.os.tag) {
            .linux => std.os.linux.pwritev(file_handle.handle, iovecs, offset),
            .freebsd => std.os.freebsd.pwritev(file_handle.handle, iovecs, offset),
            else => unreachable, // Should be caught by hasPreadv
        } catch |err| return translateError(err);
    
        return @intCast(result);
    } else {
        // Fall back to lseek + writev for platforms without pwritev
        const original_position = posix.lseek(file_handle.handle, 0, posix.SEEK.CUR) catch {
            return ScatterGatherError.InputOutput;
        };
    
        // Seek to the requested position
        _ = posix.lseek(file_handle.handle, @intCast(offset), posix.SEEK.SET) catch {
            return ScatterGatherError.InputOutput;
        };
    
        // Perform the gather write operation
        const bytes_written = posix.writev(file_handle.handle, iovecs) catch |err| {
            // Try to restore the original position (best effort)
            _ = posix.lseek(file_handle.handle, original_position, posix.SEEK.SET) catch {};
        
            return translateError(err);
        };
    
        // Restore the original position
        _ = posix.lseek(file_handle.handle, original_position, posix.SEEK.SET) catch {
            // If we can't restore the position but the write succeeded, we'll still return success
            // but future operations on this file handle might be affected
        };
    
        return @intCast(bytes_written);
    }
}

/// Translates POSIX-specific errors to ScatterGatherError
fn translateError(err: anyerror) ScatterGatherError {
    return switch (err) {
        error.AccessDenied => ScatterGatherError.AccessDenied,
        error.BadPathName => ScatterGatherError.BadPathName,
        error.ConnectionResetByPeer => ScatterGatherError.ConnectionResetByPeer,
        error.ConnectionTimedOut => ScatterGatherError.ConnectionTimedOut,
        error.DeviceBusy => ScatterGatherError.DeviceBusy,
        error.FileNotFound => ScatterGatherError.FileNotFound,
        error.InputOutput => ScatterGatherError.InputOutput,
        error.InvalidArgument => ScatterGatherError.InvalidArgument,
        error.IsDir => ScatterGatherError.IsDirectory,
        error.NotOpenForReading => ScatterGatherError.NotOpenForReading,
        error.NotOpenForWriting => ScatterGatherError.NotOpenForWriting,
        error.OperationAborted => ScatterGatherError.OperationAborted,
        error.OutOfMemory => ScatterGatherError.OutOfMemory,
        error.PathAlreadyExists => ScatterGatherError.PathAlreadyExists,
        error.ProcessFdQuotaExceeded => ScatterGatherError.ProcessFdQuotaExceeded,
        error.SystemFdQuotaExceeded => ScatterGatherError.SystemFdQuotaExceeded,
        error.SystemResources => ScatterGatherError.SystemResources,
        error.WouldBlock => ScatterGatherError.WouldBlock,
        else => ScatterGatherError.Unexpected,
    };
}
