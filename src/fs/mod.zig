//! Filesystem abstraction for the Roc compiler.
//!
//! This module provides a filesystem interface that allows for easy testing
//! and mocking of file operations throughout the compiler.

const std = @import("std");
const builtin = @import("builtin");

// Maximum file size constant - available without importing Filesystem
pub const max_file_size = std.math.maxInt(u32);

// Filesystem module uses std.posix which is not available on wasm32-freestanding
pub const Filesystem = if (builtin.cpu.arch == .wasm32) struct {
    pub const max_file_size = @This().max_file_size;
} else @import("Filesystem.zig");
