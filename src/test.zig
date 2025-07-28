//! Test runner for the Roc compiler.
//!
//! This module provides a centralized test runner that recursively references
//! all declarations from the main compiler modules to ensure all tests are
//! discovered and executed during the test phase.

const std = @import("std");
const testing = std.testing;

test {
    testing.refAllDeclsRecursive(@import("serialization"));
    testing.refAllDeclsRecursive(@import("collections"));
    testing.refAllDeclsRecursive(@import("types"));

    // Using refAllDeclsRecursive here gives "error: @fence is deprecated, use other atomics to establish ordering" for
    // some reason in an internal zig file. I suspect we need to update our zig version to resolve this
    testing.refAllDecls(@import("base"));

    testing.refAllDeclsRecursive(@import("main.zig"));
    testing.refAllDeclsRecursive(@import("cache/mod.zig"));
    testing.refAllDeclsRecursive(@import("cache/CacheModule.zig"));

    testing.refAllDeclsRecursive(@import("check/let_polymorphism_integration_test.zig"));

    // TODO: Remove after hooking up
    testing.refAllDeclsRecursive(@import("reporting"));
    testing.refAllDeclsRecursive(@import("reporting/test.zig"));
    testing.refAllDeclsRecursive(@import("eval/interpreter.zig"));
    testing.refAllDeclsRecursive(@import("eval/test/eval_test.zig"));
    testing.refAllDeclsRecursive(@import("check/canonicalize_bool_test.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types/unify.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types/let_polymorphism_test.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types/test/static_dispatch_test.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types/test/nominal_type_origin_test.zig"));
    // testing.refAllDeclsRecursive(@import("check/canonicalize/test/node_store_test.zig"));
    testing.refAllDeclsRecursive(@import("check/canonicalize/test/import_validation_test.zig"));
    // testing.refAllDeclsRecursive(@import("check/canonicalize/test/exposed_shadowing_test.zig"));
    testing.refAllDeclsRecursive(@import("snapshot.zig"));
    testing.refAllDeclsRecursive(@import("layout/layout.zig"));
    testing.refAllDeclsRecursive(@import("layout/store.zig"));
    testing.refAllDeclsRecursive(@import("layout/store_test.zig"));
    testing.refAllDeclsRecursive(@import("check/check_types/test_rigid_instantiation.zig"));
    testing.refAllDeclsRecursive(@import("repl/eval.zig"));
}
