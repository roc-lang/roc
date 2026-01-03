//! Code generation backends for the Roc compiler.
//!
//! This module provides various code generation backends:
//! - LLVM: Full-featured backend that generates native code via LLVM

pub const llvm = @import("llvm/mod.zig");

test "backend tests" {
    const std = @import("std");
    std.testing.refAllDecls(llvm);
}
