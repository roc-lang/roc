//! Filesystem and I/O abstraction for the Roc compiler.
//!
//! This module provides a unified I/O interface covering both filesystem
//! operations and stdio, allowing easy testing and alternative implementations
//! (e.g. WASM playground, in-memory test mocks).

pub const Filesystem = @import("Filesystem.zig");
/// Alias: the new unified I/O type is `Filesystem` (named for historical reasons).
pub const Io = Filesystem;
