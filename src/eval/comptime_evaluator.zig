//! Evaluates top-level declarations at compile time
//!
//! NOTE: This is a stubbed version. The actual interpreter-based evaluation
//! has been removed. This stub maintains the interface but does not actually
//! evaluate expressions at compile time.

const std = @import("std");
const can = @import("can");
const check_mod = @import("check");
const types_mod = @import("types");
const import_mapping_mod = types_mod.import_mapping;
const eval_mod = @import("mod.zig");

const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const ProblemStore = check_mod.problem.Store;
const BuiltinTypes = eval_mod.BuiltinTypes;

/// Compile-time expression evaluator (stubbed - no actual evaluation)
///
/// This type maintains the interface for compile-time evaluation but does not
/// actually evaluate expressions. The interpreter has been removed in favor
/// of the dev backend with Mono IR.
pub const ComptimeEvaluator = struct {
    allocator: Allocator,

    pub fn init(
        allocator: Allocator,
        _: *ModuleEnv,
        _: []const *const ModuleEnv,
        _: *ProblemStore,
        _: BuiltinTypes,
        _: *const ModuleEnv,
        _: *const import_mapping_mod.ImportMapping,
    ) !ComptimeEvaluator {
        return ComptimeEvaluator{
            .allocator = allocator,
        };
    }

    pub fn deinit(_: *ComptimeEvaluator) void {
        // No-op: nothing to clean up in stubbed version
    }

    /// Evaluate all top-level declarations
    ///
    /// NOTE: This is a no-op stub. Returns an empty results slice.
    pub fn evalAll(_: *ComptimeEvaluator) ![]const EvalResult {
        // No-op: stubbed version doesn't actually evaluate
        return &[_]EvalResult{};
    }

    /// Evaluate and fold a single expression
    ///
    /// NOTE: This is a no-op stub. Returns null (no folding performed).
    pub fn evalAndFoldExpr(_: *ComptimeEvaluator, _: can.CIR.Expr.Idx) !?can.CIR.Expr.Idx {
        // No-op: stubbed version doesn't actually evaluate
        return null;
    }

    /// Result of evaluating a single top-level declaration
    pub const EvalResult = struct {
        decl_name: []const u8,
        success: bool,
        crash_message: ?[]const u8,
    };
};
