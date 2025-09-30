//! Read-Eval-Print-Loop (REPL) functionality
//!
//! This module provides the infrastructure for evaluating Roc expressions
//! interactively, including expression evaluation and test environment setup.

const std = @import("std");

pub const Repl = @import("eval.zig").Repl;

test "repl tests" {
    std.testing.refAllDecls(@This());

    std.testing.refAllDecls(@import("Repl.zig"));
    std.testing.refAllDecls(@import("repl_test.zig"));
    std.testing.refAllDecls(@import("repl_test_env.zig"));
}
