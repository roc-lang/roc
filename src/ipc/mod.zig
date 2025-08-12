//! Inter-process communication utilities, used for fast dev using the interpreter shim
//!
//! This module provides platform-agnostic abstractions for shared memory
//! and process coordination, supporting both Windows and POSIX systems.

pub const std = @import("std");
pub const platform = @import("platform.zig");
pub const coordination = @import("coordination.zig");

pub const Handle = platform.Handle;
pub const FdInfo = coordination.FdInfo;
pub const CoordinationError = coordination.CoordinationError;

test "ipc tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("coordination.zig"));
    std.testing.refAllDecls(@import("platform.zig"));
}
