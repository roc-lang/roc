//! Inter-process communication utilities, used for fast dev using the interpreter shim
//!
//! This module provides platform-agnostic abstractions for shared memory
//! and process coordination, supporting both Windows and POSIX systems.

pub const std = @import("std");
pub const platform = @import("platform.zig");
pub const coordination = @import("coordination.zig");

pub const SharedMemoryAllocator = @import("SharedMemoryAllocator.zig");
pub const Handle = platform.Handle;
pub const FdInfo = coordination.FdInfo;
pub const CoordinationError = coordination.CoordinationError;

/// Shared-memory header for a viewable LIR runtime image.
///
/// Semantic compilation happens in the parent process before this header is
/// published. The child process maps the same shared-memory object and views a
/// target-specific LIR runtime image in place.
pub const LirRuntimeImageHeader = extern struct {
    magic: u32,
    format_version: u32,
    payload_offset: u64,
    payload_len: u64,
};

test "ipc tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("coordination.zig"));
    std.testing.refAllDecls(@import("platform.zig"));
    std.testing.refAllDecls(@import("SharedMemoryAllocator.zig"));
}
