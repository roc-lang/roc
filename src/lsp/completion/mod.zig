//! Completion module entry point.
//!
//! This module provides completion context detection, builtin type utilities,
//! and completion item building for the LSP completion system.

const context = @import("context.zig");
pub const builtins = @import("builtins.zig");
pub const builder = @import("builder.zig");

pub const CompletionContext = context.CompletionContext;
pub const detectCompletionContext = context.detectCompletionContext;
pub const computeOffset = context.computeOffset;

pub const CompletionBuilder = builder.CompletionBuilder;

// Force transitive test compilation of imported modules.
test {
    std.testing.refAllDecls(@import("context.zig"));
    std.testing.refAllDecls(@import("builtins.zig"));
}

const std = @import("std");
