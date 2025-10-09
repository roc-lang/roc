//! Performs Hindley-Milner type inference with constraint solving and unification on the Canonical Intermediate Representation (CIR).
//!
//! This module implements constraint-based type inference.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const tracy = @import("tracy");
const collections = @import("collections");
const types_mod = @import("types");
const Can = @import("can");
const compile = @import("compile");

/// **Hindley-Milner+ Unification**
pub const unifier = @import("unify.zig");
/// **Type Snapshot**
pub const snapshot = @import("snapshot.zig");
/// **Recursion Checking**
pub const occurs = @import("occurs.zig");
/// **Problem Reporting**
pub const problem = @import("problem.zig");

pub const Check = @import("Check.zig");

pub const ReportBuilder = problem.ReportBuilder;

test "check tests" {
    std.testing.refAllDecls(@import("Check.zig"));
    std.testing.refAllDecls(@import("copy_import.zig"));
    std.testing.refAllDecls(@import("occurs.zig"));
    std.testing.refAllDecls(@import("problem.zig"));
    std.testing.refAllDecls(@import("snapshot.zig"));
    std.testing.refAllDecls(@import("unify.zig"));

    std.testing.refAllDecls(@import("test/compiled_builtins_test.zig"));
    std.testing.refAllDecls(@import("test/unify_test.zig"));
    std.testing.refAllDecls(@import("test/cross_module_test.zig"));
    std.testing.refAllDecls(@import("test/type_checking_integration.zig"));
    std.testing.refAllDecls(@import("test/nominal_type_origin_test.zig"));
    std.testing.refAllDecls(@import("test/static_dispatch_test.zig"));
    std.testing.refAllDecls(@import("test/let_polymorphism_integration_test.zig"));
    std.testing.refAllDecls(@import("test/num_type_inference_test.zig"));
    std.testing.refAllDecls(@import("test/num_type_requirements_test.zig"));
}
