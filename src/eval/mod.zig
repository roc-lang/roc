//! Evaluation module for the Roc compiler.
//!
//! Provides interpreter-based evaluation support.

const std = @import("std");

/// Backends available for evaluating Roc code.
pub const EvalBackend = enum {
    interpreter,
    dev,
    wasm,
    llvm,
};

/// Whether a backend is currently implemented in this compiler build.
pub fn backendAvailable(backend_kind: EvalBackend) bool {
    return switch (backend_kind) {
        .interpreter => true,
        .dev => true,
        .wasm => true,
        // TODO: implement statement-only LIR LLVM codegen.
        .llvm => false,
    };
}

/// Compile-time value representation for the dev backend
pub const comptime_value = @import("comptime_value.zig");
/// Compile-time evaluator for top-level declarations
pub const ComptimeEvaluator = @import("comptime_evaluator.zig").ComptimeEvaluator;
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
/// Stack value representation used by the interpreter
pub const StackValue = @import("StackValue.zig");
/// LIR expression interpreter
pub const interpreter = @import("interpreter.zig");
pub const Interpreter = interpreter.Interpreter;
/// Production-faithful RocOps recorder used by eval tests.
pub const RuntimeHostEnv = @import("test/RuntimeHostEnv.zig");
/// Backward-compatible export for existing eval test helpers and tests.
pub const TestEnv = RuntimeHostEnv;
/// Snapshot/eval runner for expect statements.
pub const TestRunner = @import("test_runner.zig").TestRunner;
/// Shared cor-style eval test helpers.
pub const test_helpers = @import("test/helpers.zig");
/// Shared parsing/checking/lowering pipeline.
pub const pipeline = @import("pipeline.zig");
/// LIR-backed wasm evaluator.
pub const wasm_evaluator = @import("wasm_evaluator.zig");
/// Bytebox runner for wasm modules.
pub const wasm_runner = @import("wasm_runner.zig");

test "eval tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("comptime_value.zig"));
    std.testing.refAllDecls(@import("BuiltinModules.zig"));
    std.testing.refAllDecls(@import("builtins.zig"));
    std.testing.refAllDecls(@import("crash_context.zig"));
    std.testing.refAllDecls(@import("value.zig"));
    std.testing.refAllDecls(@import("interpreter.zig"));
    std.testing.refAllDecls(@import("stack.zig"));
    std.testing.refAllDecls(@import("comptime_evaluator.zig"));
    std.testing.refAllDecls(@import("test/RuntimeHostEnv.zig"));
    std.testing.refAllDecls(@import("test/helpers.zig"));
    std.testing.refAllDecls(@import("pipeline.zig"));
    std.testing.refAllDecls(@import("wasm_evaluator.zig"));
    std.testing.refAllDecls(@import("wasm_runner.zig"));
    std.testing.refAllDecls(@import("test/cor_pipeline_test.zig"));
    std.testing.refAllDecls(@import("test/stack_test.zig"));
}
