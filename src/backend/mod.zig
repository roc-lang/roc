//! Code generation backends for the Roc compiler.
//!
//! This module provides various code generation backends:
//! - LLVM: Full-featured backend that generates native code via LLVM
//! - Dev: Fast development backends that generate native code directly (x86_64, aarch64)

pub const llvm = @import("llvm/mod.zig");
pub const dev = @import("dev/mod.zig");

test "backend tests" {
    const std = @import("std");
    std.testing.refAllDecls(llvm);
    std.testing.refAllDecls(dev);
}
