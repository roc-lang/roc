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
pub const TypedCIR = @import("typed_cir.zig");
pub const CheckedIds = @import("checked_ids.zig");
pub const CanonicalNames = @import("canonical_names.zig");
/// Checked boundary names exported under the post-check-facing name.
pub const CheckedNames = CanonicalNames;
pub const CanonicalTypeKeys = @import("canonical_type_keys.zig");
pub const StaticDispatchRegistry = @import("static_dispatch_registry.zig");
pub const HoistRoots = @import("hoist_roots.zig");
pub const ConstStore = @import("const_store.zig");
pub const CheckedArtifact = @import("checked_artifact.zig");
/// Generic comptime layout fingerprint for a `Serialized` type (recurses into nested
/// aggregates and `SerializedElement`s). Shared so both the checked-artifact cache and
/// the ModuleEnv cache invalidate on any layout change, not just a top-level one.
pub const layoutVersionHash = @import("artifact_serialize.zig").layoutVersionHash;
/// Checked module data exported under the post-check-facing name.
pub const CheckedModule = CheckedArtifact;

pub const Check = @import("Check.zig");
pub const TestEnv = @import("test/TestEnv.zig");

pub const ReportBuilder = report.ReportBuilder;

test "check tests" {
    std.testing.refAllDecls(@import("Check.zig"));
    std.testing.refAllDecls(@import("exhaustive.zig"));
    std.testing.refAllDecls(@import("occurs.zig"));
    std.testing.refAllDecls(@import("problem.zig"));
    std.testing.refAllDecls(@import("problem/context.zig"));
    std.testing.refAllDecls(@import("problem/store.zig"));
    std.testing.refAllDecls(@import("problem/types.zig"));
    std.testing.refAllDecls(@import("report.zig"));
    std.testing.refAllDecls(@import("static_dispatch_registry.zig"));
    std.testing.refAllDecls(@import("hoist_roots.zig"));
    std.testing.refAllDecls(@import("canonical_names.zig"));
    std.testing.refAllDecls(@import("canonical_type_keys.zig"));
    std.testing.refAllDecls(@import("const_store.zig"));
    std.testing.refAllDecls(@import("artifact_serialize.zig"));
    std.testing.refAllDecls(@import("checked_artifact.zig"));
    std.testing.refAllDecls(@import("checked_ids.zig"));
    std.testing.refAllDecls(@import("typed_cir.zig"));
    std.testing.refAllDecls(@import("snapshot.zig"));
    std.testing.refAllDecls(@import("unify.zig"));
    std.testing.refAllDecls(@import("snapshot/diff.zig"));

    std.testing.refAllDecls(@import("test/cross_module_test.zig"));
    std.testing.refAllDecls(@import("test/type_checking_integration.zig"));
    std.testing.refAllDecls(@import("test/let_polymorphism_integration_test.zig"));
    std.testing.refAllDecls(@import("test/hoist_roots_test.zig"));
    std.testing.refAllDecls(@import("test/num_type_requirements_test.zig"));
    std.testing.refAllDecls(@import("test/custom_num_type_test.zig"));
    std.testing.refAllDecls(@import("test/builtin_scope_test.zig"));
    std.testing.refAllDecls(@import("test/num_type_inference_test.zig"));
    std.testing.refAllDecls(@import("test/unify_test.zig"));
    std.testing.refAllDecls(@import("test/instantiate_tag_union_test.zig"));
    std.testing.refAllDecls(@import("test/where_clause_test.zig"));
    std.testing.refAllDecls(@import("test/range_test.zig"));
    std.testing.refAllDecls(@import("test/recursive_alias_test.zig"));
    std.testing.refAllDecls(@import("test/generalize_redirect_test.zig"));
    std.testing.refAllDecls(@import("test/exhaustiveness_test.zig"));
    std.testing.refAllDecls(@import("test/issue_9705_test.zig"));
    std.testing.refAllDecls(@import("test/issue_9709_test.zig"));
    std.testing.refAllDecls(@import("test/issue_9710_test.zig"));
    std.testing.refAllDecls(@import("test/issue_9711_test.zig"));
    std.testing.refAllDecls(@import("test/repros_test.zig"));
    std.testing.refAllDecls(@import("test/typed_cir_test.zig"));

    // Cross-module monomorphization tests
    std.testing.refAllDecls(@import("test/cross_module_mono_test.zig"));
}
