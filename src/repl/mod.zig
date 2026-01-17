//! Read-Eval-Print-Loop (REPL) functionality
//!
//! This module provides the infrastructure for evaluating Roc expressions
//! interactively, including expression evaluation and test environment setup.

const std = @import("std");

const eval_zig = @import("eval.zig");
pub const Repl = eval_zig.Repl;
pub const Backend = eval_zig.Backend;

test "repl tests" {
    std.testing.refAllDecls(@This());

    std.testing.refAllDecls(@import("repl_test.zig"));
    std.testing.refAllDecls(@import("repl_test_env.zig"));
}
