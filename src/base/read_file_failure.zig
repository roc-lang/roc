//! Classification of `std.Io.Dir.readFileAlloc` failures.

const std = @import("std");

/// The distinctions callers draw between `readFileAlloc` failures.
pub const Kind = enum { file_not_found, out_of_memory, other };

/// Classifies `err`. The switch names every error so each one is placed explicitly.
pub fn kind(err: std.Io.Dir.ReadFileAllocError) Kind {
    return switch (err) {
        error.FileNotFound => .file_not_found,
        error.OutOfMemory => .out_of_memory,
        error.AccessDenied,
        error.AntivirusInterference,
        error.BadPathName,
        error.Canceled,
        error.ConnectionResetByPeer,
        error.DeviceBusy,
        error.FileBusy,
        error.FileLocksUnsupported,
        error.FileTooBig,
        error.InputOutput,
        error.IsDir,
        error.LockViolation,
        error.NameTooLong,
        error.NetworkNotFound,
        error.NoDevice,
        error.NoSpaceLeft,
        error.NotDir,
        error.NotOpenForReading,
        error.PathAlreadyExists,
        error.PermissionDenied,
        error.PipeBusy,
        error.ProcessFdQuotaExceeded,
        error.ReadOnlyFileSystem,
        error.SocketUnconnected,
        error.StreamTooLong,
        error.SymLinkLoop,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.Unexpected,
        error.WouldBlock,
        => .other,
    };
}
