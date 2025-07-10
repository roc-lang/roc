//! Module loading and building system for the Roc compiler
//!
//! This module provides abstractions for loading, parsing, and canonicalizing
//! Roc modules with support for single-threaded, multi-threaded, and test modes.

pub const Builder = @import("Builder.zig");
pub const Task = @import("Task.zig");
pub const Worker = @import("Worker.zig");
pub const TestRunner = @import("TestRunner.zig");

test {
    _ = @import("test.zig");
}
