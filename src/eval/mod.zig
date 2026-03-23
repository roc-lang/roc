//! Evaluation module for the Roc compiler.
//!
//! Provides expression evaluation via interpreter, native code generation,
//! and WebAssembly execution for Roc expressions.

const std = @import("std");

/// Dev backend-based evaluator for native code generation using Mono IR
const dev_evaluator_mod = @import("dev_evaluator.zig");
pub const DevEvaluator = dev_evaluator_mod.DevEvaluator;
/// Compile-time value representation for the dev backend
pub const comptime_value = @import("comptime_value.zig");
/// Executable memory for running generated code (re-exported from backend module)
const backend = @import("backend");
pub const ExecutableMemory = backend.ExecutableMemory;
/// Layout module (re-exported for result type information)
pub const layout = @import("layout");
/// Interpreter-specific layout module (still needed by comptime_evaluator)
pub const interpreter_layout = @import("interpreter_layout");
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

// --- LIR interpreter (primary) ---
/// Shared CIR → MIR → LIR → RC lowering pipeline
pub const cir_to_lir = @import("cir_to_lir.zig");
pub const LirProgram = cir_to_lir.LirProgram;
/// Concrete runtime value for the LIR interpreter
pub const value = @import("value.zig");
pub const Value = value.Value;
/// LIR expression interpreter
pub const interpreter = @import("interpreter.zig");
pub const LirInterpreter = interpreter.LirInterpreter;
/// Layout-based value formatter for the LIR interpreter
pub const value_format = @import("value_format.zig");

/// Backend selection for expression evaluation
pub const EvalBackend = enum {
    interpreter,
    dev,
    llvm,
    wasm,

    pub fn fromString(s: []const u8) ?EvalBackend {
        return std.meta.stringToEnum(EvalBackend, s);
    }
};

/// Unified evaluation runner for all backends
pub const runner = @import("runner.zig");

/// Test runner for expect expressions (uses LIR interpreter)
pub const TestRunner = @import("test_runner.zig").TestRunner;
/// LLVM-based evaluator for optimized code generation
pub const LlvmEvaluator = @import("llvm_evaluator.zig").LlvmEvaluator;
/// WebAssembly-based evaluator for wasm code generation
const wasm_evaluator_mod = @import("wasm_evaluator.zig");
pub const WasmEvaluator = wasm_evaluator_mod.WasmEvaluator;

test "eval tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("runner.zig"));

    std.testing.refAllDecls(@import("dev_evaluator.zig"));
    std.testing.refAllDecls(@import("comptime_value.zig"));
    std.testing.refAllDecls(@import("BuiltinModules.zig"));
    std.testing.refAllDecls(@import("builtins.zig"));
    std.testing.refAllDecls(@import("crash_context.zig"));
    std.testing.refAllDecls(@import("comptime_evaluator.zig"));
    std.testing.refAllDecls(@import("StackValue.zig"));
    std.testing.refAllDecls(@import("render_helpers.zig"));
    std.testing.refAllDecls(@import("llvm_evaluator.zig"));
    std.testing.refAllDecls(@import("cir_to_lir.zig"));
    std.testing.refAllDecls(@import("value.zig"));
    std.testing.refAllDecls(@import("interpreter.zig"));
    std.testing.refAllDecls(@import("fold_type.zig"));
    std.testing.refAllDecls(@import("value_to_cir.zig"));
    std.testing.refAllDecls(@import("value_format.zig"));
    std.testing.refAllDecls(@import("wasm_evaluator.zig"));
    std.testing.refAllDecls(@import("stack.zig"));
    std.testing.refAllDecls(@import("test/TestEnv.zig"));

    // Test files that compare interpreter output with dev backend
    std.testing.refAllDecls(@import("test/helpers.zig"));
    std.testing.refAllDecls(@import("test/eval_test.zig"));
    std.testing.refAllDecls(@import("test/list_refcount_basic.zig"));
    std.testing.refAllDecls(@import("test/list_refcount_simple.zig"));
    std.testing.refAllDecls(@import("test/list_refcount_nested.zig"));
    std.testing.refAllDecls(@import("test/list_refcount_pattern.zig"));
    std.testing.refAllDecls(@import("test/list_refcount_alias.zig"));
    std.testing.refAllDecls(@import("test/list_refcount_complex.zig"));
    std.testing.refAllDecls(@import("test/list_refcount_conditional.zig"));
    std.testing.refAllDecls(@import("test/list_refcount_containers.zig"));
    std.testing.refAllDecls(@import("test/list_refcount_function.zig"));
    std.testing.refAllDecls(@import("test/list_refcount_builtins.zig"));
    std.testing.refAllDecls(@import("test/list_refcount_strings.zig"));
    std.testing.refAllDecls(@import("test/arithmetic_comprehensive_test.zig"));
    std.testing.refAllDecls(@import("test/anno_only_interp_test.zig"));
    std.testing.refAllDecls(@import("test/comptime_eval_test.zig"));
    std.testing.refAllDecls(@import("test/low_level_interp_test.zig"));
    std.testing.refAllDecls(@import("test/mono_emit_test.zig"));
    std.testing.refAllDecls(@import("test/closure_test.zig"));
    std.testing.refAllDecls(@import("test/stack_test.zig"));
}
