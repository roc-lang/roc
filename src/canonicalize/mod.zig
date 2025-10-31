//! This module contains the canonicalizer and the Canonical Intermediate Representation (CIR).

const std = @import("std");

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
    std.testing.refAllDecls(@import("test/import_validation_test.zig"));
    std.testing.refAllDecls(@import("test/int_test.zig"));
    std.testing.refAllDecls(@import("test/node_store_test.zig"));
    std.testing.refAllDecls(@import("test/import_store_test.zig"));
    std.testing.refAllDecls(@import("test/scope_test.zig"));
    std.testing.refAllDecls(@import("test/record_test.zig"));
}
