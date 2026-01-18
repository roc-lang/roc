//! Reference Counting IR Transformation Module
//!
//! This module implements compile-time reference counting insertion for the Roc compiler.
//! RC operations are inserted into the CIR (Canonical IR) as explicit expressions,
//! which are then consumed by all backends (interpreter, dev backend, LLVM).
//!
//! The key principle is that RC logic is a compile-time transformation, NOT runtime logic.
//! All backends emit code for the same RC-annotated IR.

const std = @import("std");

pub const insert = @import("insert.zig");

// Re-export the main entry point
pub const InsertPass = insert.InsertPass;

test "rc tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(insert);
}
