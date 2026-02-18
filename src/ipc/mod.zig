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

/// A properly aligned header structure for sending a serialized ModuleEnv over IPC.
pub const ModuleEnvHeader = extern struct {
    parent_base_addr: u64,
    entry_count: u32,
    _padding: u32, // Ensure 8-byte alignment
    def_indices_offset: u64,
    module_env_offset: u64,
};

test "ipc tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("coordination.zig"));
    std.testing.refAllDecls(@import("platform.zig"));
    std.testing.refAllDecls(@import("SharedMemoryAllocator.zig"));
}
