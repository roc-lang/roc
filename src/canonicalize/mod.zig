//! This module contains the canonicalizer and the Canonical Intermediate Representation (CIR).

const std = @import("std");
const parse = @import("parse");

const AST = parse.AST;

/// The canonicalizer (the thing that canonicalizes the AST).
pub const Can = @import("Can.zig");
/// The Canonical Intermediate Representation (CIR)
pub const CIR = @import("CIR.zig");
/// The Module Environment after canonicalization (used also for type checking and serialization)
pub const ModuleEnv = @import("ModuleEnv.zig");
/// Scope management for canonicalization
pub const Scope = @import("Scope.zig");
/// Dependency graph and SCC (Strongly Connected Components) analysis
pub const DependencyGraph = @import("DependencyGraph.zig");
/// Hosted function compiler - replaces annotation-only with hosted lambdas
pub const HostedCompiler = @import("HostedCompiler.zig");
/// Roc code emitter - converts CIR to valid Roc source code
pub const RocEmitter = @import("RocEmitter.zig");
/// Node storage for CIR nodes (used internally by ModuleEnv)
pub const NodeStore = @import("NodeStore.zig");

/// Re-export CoreCtx for callers that need to create a canonicalizer
pub const CoreCtx = @import("ctx").CoreCtx;
/// Re-export AutoImportedType for callers
pub const AutoImportedType = Can.AutoImportedType;

/// Canonicalize a full module file.
///
/// This is the unified entry point for module canonicalization. It:
/// 1. Initializes the canonicalizer
/// 2. Canonicalizes the entire file
/// 3. Validates the result for type checking
/// 4. Cleans up canonicalizer resources
///
/// Results are stored in module_env (all_defs, all_statements, diagnostics, etc).
///
/// Memory ownership:
/// - roc_ctx: Caller provides and manages
/// - module_env: Caller provides; results stored here
/// - parse_ast: Caller provides and manages
/// - context: Builtin type context plus optional explicit imported module environments
pub fn canonicalizeModule(
    roc_ctx: CoreCtx,
    module_env: *ModuleEnv,
    parse_ast: *AST,
    context: Can.ModuleInitContext,
) std.mem.Allocator.Error!void {
    var czer = try Can.initModule(roc_ctx, module_env, parse_ast, context);
    defer czer.deinit();
    try czer.canonicalizeFile();
    try czer.validateForChecking();
}

/// Canonicalize a single expression (for REPL).
///
/// Returns the canonical expression result, or null if canonicalization failed.
/// Check module_env.getDiagnostics() for any errors.
///
/// Memory ownership:
/// - roc_ctx: Caller provides and manages
/// - module_env: Caller provides; results stored here
/// - parse_ast: Caller provides (root_node_idx should point to expression)
/// - context: Builtin type context plus optional explicit imported module environments
pub fn canonicalizeExpr(
    roc_ctx: CoreCtx,
    module_env: *ModuleEnv,
    parse_ast: *AST,
    context: Can.ModuleInitContext,
) std.mem.Allocator.Error!?Can.CanonicalizedExpr {
    var czer = try Can.initModule(roc_ctx, module_env, parse_ast, context);
    defer czer.deinit();
    const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    return try czer.canonicalizeExpr(expr_idx);
}

test "compile tests" {
    std.testing.refAllDecls(@This());

    std.testing.refAllDecls(@import("Can.zig"));
    std.testing.refAllDecls(@import("CIR.zig"));
    std.testing.refAllDecls(@import("Diagnostic.zig"));
    std.testing.refAllDecls(@import("Expression.zig"));
    std.testing.refAllDecls(@import("ExternalDecl.zig"));
    std.testing.refAllDecls(@import("ModuleEnv.zig"));
    std.testing.refAllDecls(@import("Node.zig"));
    std.testing.refAllDecls(@import("NodeStore.zig"));
    std.testing.refAllDecls(@import("Pattern.zig"));
    std.testing.refAllDecls(@import("Scope.zig"));
    std.testing.refAllDecls(@import("Statement.zig"));
    std.testing.refAllDecls(@import("TypeAnnotation.zig"));

    std.testing.refAllDecls(@import("test/anno_only_test.zig"));
    std.testing.refAllDecls(@import("test/bool_test.zig"));
    std.testing.refAllDecls(@import("test/exposed_shadowing_test.zig"));
    std.testing.refAllDecls(@import("test/frac_test.zig"));
    std.testing.refAllDecls(@import("test/if_statement_test.zig"));
    std.testing.refAllDecls(@import("test/import_validation_test.zig"));
    std.testing.refAllDecls(@import("test/int_test.zig"));
    std.testing.refAllDecls(@import("test/node_store_test.zig"));
    std.testing.refAllDecls(@import("test/import_store_test.zig"));
    std.testing.refAllDecls(@import("test/scope_test.zig"));
    std.testing.refAllDecls(@import("test/record_test.zig"));
    std.testing.refAllDecls(@import("test/type_decl_stmt_test.zig"));

    // Backend tests (Roc emitter)
    std.testing.refAllDecls(@import("RocEmitter.zig"));
    std.testing.refAllDecls(@import("test/roc_emitter_test.zig"));
}
