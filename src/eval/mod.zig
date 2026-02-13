//! Evaluation module for the Roc compiler.
//!
//! Provides native code generation and execution for Roc expressions.

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
/// Interpreter for running CIR expressions
pub const Interpreter = @import("interpreter.zig").Interpreter;
/// Stack value representation for interpreter
pub const StackValue = @import("StackValue.zig");
/// Render helpers for outputting values
pub const render_helpers = @import("render_helpers.zig");
/// Stack memory allocator for evaluating Roc IR
const stack_mod = @import("stack.zig");
pub const Stack = stack_mod.Stack;
pub const StackOverflow = stack_mod.StackOverflow;
/// Eval error type alias
pub const EvalError = Interpreter.Error;
/// Test runner for expect expressions
pub const TestRunner = @import("test_runner.zig").TestRunner;
/// LLVM-based evaluator for optimized code generation
pub const LlvmEvaluator = @import("llvm_evaluator.zig").LlvmEvaluator;
/// WebAssembly-based evaluator for wasm code generation
const wasm_evaluator_mod = @import("wasm_evaluator.zig");
pub const WasmEvaluator = wasm_evaluator_mod.WasmEvaluator;

test "eval tests" {
    std.testing.refAllDecls(@This());

    std.testing.refAllDecls(@import("dev_evaluator.zig"));
    std.testing.refAllDecls(@import("comptime_value.zig"));
    std.testing.refAllDecls(@import("BuiltinModules.zig"));
    std.testing.refAllDecls(@import("builtins.zig"));
    std.testing.refAllDecls(@import("crash_context.zig"));
    std.testing.refAllDecls(@import("comptime_evaluator.zig"));
    std.testing.refAllDecls(@import("interpreter.zig"));
    std.testing.refAllDecls(@import("StackValue.zig"));
    std.testing.refAllDecls(@import("render_helpers.zig"));
    std.testing.refAllDecls(@import("llvm_evaluator.zig"));
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
    std.testing.refAllDecls(@import("test/interpreter_polymorphism_test.zig"));
    std.testing.refAllDecls(@import("test/interpreter_style_test.zig"));
    std.testing.refAllDecls(@import("test/low_level_interp_test.zig"));
    std.testing.refAllDecls(@import("test/mono_emit_test.zig"));
    std.testing.refAllDecls(@import("test/mono_lower_test.zig"));
    std.testing.refAllDecls(@import("test/closure_test.zig"));
    std.testing.refAllDecls(@import("test/rc_insert_test.zig"));
    std.testing.refAllDecls(@import("test/stack_test.zig"));
}
