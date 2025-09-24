//! Stack-based interpreter for evaluating Roc expressions.

const std = @import("std");

pub const Interpreter = @import("interpreter.zig").Interpreter;
pub const Interpreter2 = @import("interpreter2.zig").Interpreter2;
pub const Stack = @import("stack.zig").Stack;
pub const StackOverflow = @import("stack.zig").StackOverflow;
pub const StackValue = @import("StackValue.zig");
pub const EvalError = @import("interpreter.zig").EvalError;
pub const TestRunner = @import("test_runner.zig").TestRunner;

test "eval tests" {
    std.testing.refAllDecls(@This());

    std.testing.refAllDecls(@import("interpreter.zig"));
    std.testing.refAllDecls(@import("interpreter2.zig"));
    std.testing.refAllDecls(@import("stack.zig"));
    std.testing.refAllDecls(@import("StackValue.zig"));

    std.testing.refAllDecls(@import("test/TestEnv.zig"));
    std.testing.refAllDecls(@import("test/eval_test.zig"));
    std.testing.refAllDecls(@import("test/helpers.zig"));
}
