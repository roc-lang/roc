//! Evaluation module for the Roc compiler.
//!
//! Provides native code generation and execution for Roc expressions.

const std = @import("std");

/// Dev backend-based evaluator for native code generation using Mono IR
pub const DevEvaluator = @import("dev_evaluator.zig").DevEvaluator;
/// Compile-time value representation for the dev backend
pub const comptime_value = @import("comptime_value.zig");
/// Executable memory for running generated code (re-exported from backend module)
const backend = @import("backend");
pub const ExecutableMemory = backend.ExecutableMemory;
/// Backwards compatibility alias
pub const JitCode = ExecutableMemory;
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
/// Compile-time expression evaluator for constant folding
pub const ComptimeEvaluator = @import("comptime_evaluator.zig").ComptimeEvaluator;

test "eval tests" {
    std.testing.refAllDecls(@This());

    std.testing.refAllDecls(@import("dev_evaluator.zig"));
    std.testing.refAllDecls(@import("comptime_value.zig"));
    std.testing.refAllDecls(@import("BuiltinModules.zig"));
    std.testing.refAllDecls(@import("builtins.zig"));
    std.testing.refAllDecls(@import("crash_context.zig"));
    std.testing.refAllDecls(@import("comptime_evaluator.zig"));
}
