//! POSIX-specific implementation of gather I/O using writev.

const std = @import("std");
const os = std.os;
const posix = std.posix;
const Allocator = std.mem.Allocator;

const write_gather = @import("write_gather.zig");
const WriteGatherError = write_gather.WriteGatherError;

pub const PosixAlignedBuffer = struct {
    base: [*]const u8,
    len: usize,
    /// The original buffer allocation for deallocation
    buffer: []u8,

    /// Initializes a new PosixAlignedBuffer
    pub fn init(allocator: Allocator, size: usize, _: std.fs.File) Allocator.Error!PosixAlignedBuffer {
        // On POSIX, no special alignment is necessary
        const buffer = try allocator.alloc(u8, size);
        return PosixAlignedBuffer{
            .base = buffer.ptr,
            .len = buffer.len,
            .buffer = buffer,
        };
    }

    /// Frees the buffer
    pub fn deinit(self: PosixAlignedBuffer, allocator: Allocator) void {
        allocator.free(self.buffer);
    }
};

/// Implementation of writeGather for POSIX using pwritev directly with PosixAlignedBuffer
/// which is already in the correct format for iovec
pub fn writeGather(
    file_handle: std.fs.File,
    buffers: []*const PosixAlignedBuffer,
    offset: u64,
) WriteGatherError!usize {
    // PosixAlignedBuffer is already in the right format to be used with iovec
    // so just reinterpret the buffers array directly
    const iovecs = @as([*]const posix.iovec_const, @ptrCast(buffers.ptr))[0..buffers.len];

    // Use posix.pwritev directly with our buffer array
    const result = posix.pwritev(file_handle.handle, iovecs, offset) catch |err|
        return translateError(err);

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
