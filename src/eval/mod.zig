//! Stack-based interpreter for evaluating Roc expressions.

const std = @import("std");

/// Runtime interpreter used by the REPL, tests, and snapshot tooling.
pub const Interpreter = @import("interpreter.zig").Interpreter;
/// Stack implementation backing interpreter evaluations.
pub const Stack = @import("stack.zig").Stack;
/// Error raised when the interpreter stack exhausts its capacity.
pub const StackOverflow = @import("stack.zig").StackOverflow;
/// Value wrapper that pairs raw memory with layout metadata.
pub const StackValue = @import("StackValue.zig");
/// Convenience alias for the interpreter’s error type.
pub const EvalError = Interpreter.Error;
/// Runs `expect` expressions inside evaluation tests.
pub const TestRunner = @import("test_runner.zig").TestRunner;
/// Evaluates top-level declarations at compile time
pub const ComptimeEvaluator = @import("comptime_evaluator.zig").ComptimeEvaluator;
/// Contains all builtin types required by the Interpreter
pub const BuiltinTypes = @import("builtins.zig").BuiltinTypes;
/// Utilities for loading compiled builtin modules
pub const builtin_loading = @import("builtin_loading.zig");
/// Centralized management of builtin modules (Bool, Result, Str)
pub const BuiltinModules = @import("BuiltinModules.zig").BuiltinModules;
const crash_context = @import("crash_context.zig");
pub const CrashContext = crash_context.CrashContext;
pub const CrashState = crash_context.CrashState;

test "eval tests" {
    std.testing.refAllDecls(@This());

    std.testing.refAllDecls(@import("interpreter.zig"));
    std.testing.refAllDecls(@import("stack.zig"));
    std.testing.refAllDecls(@import("StackValue.zig"));
    std.testing.refAllDecls(@import("crash_context.zig"));

    std.testing.refAllDecls(@import("test/TestEnv.zig"));
    std.testing.refAllDecls(@import("test/eval_test.zig"));
    std.testing.refAllDecls(@import("test/comptime_eval_test.zig"));
    std.testing.refAllDecls(@import("test/helpers.zig"));
    std.testing.refAllDecls(@import("test/interpreter_style_test.zig"));
    std.testing.refAllDecls(@import("test/interpreter_polymorphism_test.zig"));
    std.testing.refAllDecls(@import("test/anno_only_interp_test.zig"));
}
