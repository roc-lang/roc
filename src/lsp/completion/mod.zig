//! Completion module entry point.
//!
//! This module provides completion context detection and builtin type utilities
//! for the LSP completion system.

const context = @import("context.zig");
pub const builtins = @import("builtins.zig");

pub const CompletionContext = context.CompletionContext;
pub const detectCompletionContext = context.detectCompletionContext;
pub const computeOffset = context.computeOffset;

test {
    _ = context;
    _ = builtins;
}
