//! Evaluation module for the Roc compiler.
//!
//! Provides interpreter-based evaluation support.

const std = @import("std");

/// Compile-time value representation for the dev backend
pub const comptime_value = @import("comptime_value.zig");
/// Executable memory for running generated code (re-exported from backend module)
const backend = @import("backend");
pub const ExecutableMemory = backend.ExecutableMemory;
/// Layout module (re-exported for result type information)
pub const layout = @import("layout");
/// Utilities for loading compiled builtin modules
pub const builtin_loading = @import("builtin_loading.zig");
/// Centralized loading and management of builtin modules
pub const BuiltinModules = @import("BuiltinModules.zig").BuiltinModules;
/// Builtin types for type checking
pub const BuiltinTypes = @import("builtins.zig").BuiltinTypes;
/// Crash context for host crash handling
const crash_context = @import("crash_context.zig");
pub const CrashContext = crash_context.CrashContext;
pub const CrashState = crash_context.CrashState;

/// Concrete runtime value for the interpreter
pub const value = @import("value.zig");
pub const Value = value.Value;
/// LIR expression interpreter
pub const interpreter = @import("interpreter.zig");
pub const Interpreter = interpreter.Interpreter;
/// Test environment providing RocOps with allocation tracking.
pub const TestEnv = @import("test/TestEnv.zig");

test "eval tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("comptime_value.zig"));
    std.testing.refAllDecls(@import("BuiltinModules.zig"));
    std.testing.refAllDecls(@import("builtins.zig"));
    std.testing.refAllDecls(@import("crash_context.zig"));
    std.testing.refAllDecls(@import("value.zig"));
    std.testing.refAllDecls(@import("interpreter.zig"));
    std.testing.refAllDecls(@import("fold_type.zig"));
    std.testing.refAllDecls(@import("value_to_cir.zig"));
    std.testing.refAllDecls(@import("stack.zig"));
    std.testing.refAllDecls(@import("test/TestEnv.zig"));
    std.testing.refAllDecls(@import("test/anno_only_interp_test.zig"));
    std.testing.refAllDecls(@import("test/comptime_eval_test.zig"));
    std.testing.refAllDecls(@import("test/low_level_interp_test.zig"));
    std.testing.refAllDecls(@import("test/stack_test.zig"));
}
