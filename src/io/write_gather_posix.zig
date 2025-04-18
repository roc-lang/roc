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

    const result = switch (@import("builtin").os.tag) {
        .linux => std.os.linux.pwritev(file_handle.handle, iovecs, offset),
        .freebsd => std.os.freebsd.pwritev(file_handle.handle, iovecs, offset),
        else => blk: {
            // For platforms that are not explicitly listed, use pwritev
            // from std.posix if available, or make a specific OS implementation
            //
            // Note: This assumes all platforms support pwritev in some form
            if (@hasDecl(std.posix, "pwritev")) {
                break :blk std.posix.pwritev(file_handle.handle, iovecs, offset);
            } else {
                @compileError("pwritev not available for this platform. Update code to add support.");
            }
        },
    } catch |err| return translateError(err);

    return @intCast(result);
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
