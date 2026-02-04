//! Performs Hindley-Milner type inference with constraint solving and unification on the Canonical Intermediate Representation (CIR).
//!
//! This module implements constraint-based type inference.

const std = @import("std");
const base = @import("base");
const can = @import("can");

const Allocators = base.Allocators;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;

/// **Hindley-Milner+ Unification**
pub const unifier = @import("unify.zig");
/// **Type Snapshot**
pub const snapshot = @import("snapshot.zig");
/// **Recursion Checking**
pub const occurs = @import("occurs.zig");
/// **Problems**
pub const problem = @import("problem.zig");
/// **Reporting**
pub const report = @import("report.zig");
/// **Exhaustiveness Checking**
pub const exhaustive = @import("exhaustive.zig");

pub const Check = @import("Check.zig");

pub const ReportBuilder = report.ReportBuilder;

/// Type check a full module file.
///
/// This is the unified entry point for module type checking. It:
/// 1. Initializes the type checker
/// 2. Type checks the entire file
/// 3. Cleans up type checker resources
///
/// Results are stored in module_env.types and checker problems.
///
/// Memory ownership:
/// - allocators: Caller provides and manages
/// - module_env: Caller provides; type information stored here
/// - imported_modules: Caller provides slice of imported module environments
/// - auto_imported_types: Optional map of auto-imported type environments
/// - builtin_ctx: Builtin type context for resolution
pub fn checkModule(
    allocators: *Allocators,
    module_env: *ModuleEnv,
    imported_modules: []const *const ModuleEnv,
    auto_imported_types: ?*const std.AutoHashMap(Ident.Idx, can.Can.AutoImportedType),
    builtin_ctx: Check.BuiltinContext,
) std.mem.Allocator.Error!void {
    var checker = try Check.init(
        allocators,
        &module_env.types,
        module_env,
        imported_modules,
        auto_imported_types,
        &module_env.store.regions,
        builtin_ctx,
    );
    defer checker.deinit();
    checker.fixupTypeWriter();
    try checker.checkFile();
}

/// Type check a single expression (for REPL).
///
/// Returns the type check result for the expression.
/// Check module_env for any type errors.
///
/// Memory ownership:
/// - allocators: Caller provides and manages
/// - module_env: Caller provides; type information stored here
/// - expr_idx: The canonical expression to type check
/// - imported_modules: Caller provides slice of imported module environments
/// - auto_imported_types: Optional map of auto-imported type environments
/// - builtin_ctx: Builtin type context for resolution
pub fn checkExpr(
    allocators: *Allocators,
    module_env: *ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    imported_modules: []const *const ModuleEnv,
    auto_imported_types: ?*const std.AutoHashMap(Ident.Idx, can.Can.AutoImportedType),
    builtin_ctx: Check.BuiltinContext,
) std.mem.Allocator.Error!Check.TypeCheckResult {
    var checker = try Check.init(
        allocators,
        &module_env.types,
        module_env,
        imported_modules,
        auto_imported_types,
        &module_env.store.regions,
        builtin_ctx,
    );
    defer checker.deinit();
    checker.fixupTypeWriter();
    return checker.checkExprRepl(expr_idx);
}

test "check tests" {
    std.testing.refAllDecls(@import("Check.zig"));
    std.testing.refAllDecls(@import("copy_import.zig"));
    std.testing.refAllDecls(@import("exhaustive.zig"));
    std.testing.refAllDecls(@import("occurs.zig"));
    std.testing.refAllDecls(@import("problem.zig"));
    std.testing.refAllDecls(@import("problem/context.zig"));
    std.testing.refAllDecls(@import("problem/store.zig"));
    std.testing.refAllDecls(@import("problem/types.zig"));
    std.testing.refAllDecls(@import("report.zig"));
    std.testing.refAllDecls(@import("snapshot.zig"));
    std.testing.refAllDecls(@import("unify.zig"));
    std.testing.refAllDecls(@import("snapshot/diff.zig"));

    std.testing.refAllDecls(@import("test/cross_module_test.zig"));
    std.testing.refAllDecls(@import("test/type_checking_integration.zig"));
    std.testing.refAllDecls(@import("test/let_polymorphism_integration_test.zig"));
    std.testing.refAllDecls(@import("test/num_type_requirements_test.zig"));
    std.testing.refAllDecls(@import("test/custom_num_type_test.zig"));
    std.testing.refAllDecls(@import("test/builtin_scope_test.zig"));
    std.testing.refAllDecls(@import("test/num_type_inference_test.zig"));
    std.testing.refAllDecls(@import("test/unify_test.zig"));
    std.testing.refAllDecls(@import("test/instantiate_tag_union_test.zig"));
    std.testing.refAllDecls(@import("test/where_clause_test.zig"));
    std.testing.refAllDecls(@import("test/recursive_alias_test.zig"));
    std.testing.refAllDecls(@import("test/generalize_redirect_test.zig"));
    std.testing.refAllDecls(@import("test/exhaustiveness_test.zig"));
    std.testing.refAllDecls(@import("test/repros_test.zig"));

    // Cross-module monomorphization tests
    std.testing.refAllDecls(@import("test/cross_module_mono_test.zig"));
}
