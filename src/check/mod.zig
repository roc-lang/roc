//! Performs Hindley-Milner type inference with constraint solving and unification on the Canonical Intermediate Representation (CIR).
//!
//! This module implements constraint-based type inference.

const std = @import("std");

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
    std.testing.refAllDecls(@import("test/problem_coverage_test.zig"));

    // Cross-module monomorphization tests
    std.testing.refAllDecls(@import("test/cross_module_mono_test.zig"));
}
