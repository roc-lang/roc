//! Shared debug flags for LSP server and syntax components.

/// Flags allowing granular LSP debugging.
pub const DebugFlags = struct {
    build: bool = false,
    syntax: bool = false,
    server: bool = false,
    completion: bool = false,
};
