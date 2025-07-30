//! Test runner for the Roc compiler.
//!
//! This module provides a centralized test runner that recursively references
//! all declarations from the main compiler modules to ensure all tests are
//! discovered and executed during the test phase.

const std = @import("std");
const testing = std.testing;

test {
    testing.refAllDecls(@import("base"));
    testing.refAllDecls(@import("base/test/base_test.zig"));

    testing.refAllDeclsRecursive(@import("builtins"));

    testing.refAllDeclsRecursive(@import("collections"));

    testing.refAllDecls(@import("types"));
    testing.refAllDecls(@import("types/test/rigid_instantiation.zig"));

    testing.refAllDeclsRecursive(@import("serialization"));
    testing.refAllDecls(@import("serialization/test/compact_writer_test.zig"));

    testing.refAllDecls(@import("can"));
    testing.refAllDecls(@import("canonicalize/test/bool_test.zig"));
    testing.refAllDecls(@import("canonicalize/test/exposed_shadowing_test.zig"));
    testing.refAllDecls(@import("canonicalize/test/frac_test.zig"));
    testing.refAllDecls(@import("canonicalize/test/import_validation_test.zig"));
    testing.refAllDecls(@import("canonicalize/test/int_test.zig"));
    testing.refAllDecls(@import("canonicalize/test/node_store_test.zig"));

    testing.refAllDecls(@import("check"));
    testing.refAllDecls(@import("check/test/cross_module_test.zig"));
    testing.refAllDecls(@import("check/test/let_polymorphism_integration_test.zig"));
    testing.refAllDecls(@import("check/test/let_polymorphism_test.zig"));
    testing.refAllDecls(@import("check/test/literal_size_test.zig"));
    testing.refAllDecls(@import("check/test/nominal_type_origin_test.zig"));
    testing.refAllDecls(@import("check/test/static_dispatch_test.zig"));
    testing.refAllDecls(@import("check/test/test_rigid_instantiation.zig"));

    testing.refAllDecls(@import("parse"));
    testing.refAllDecls(@import("parse/test/ast_node_store_test.zig"));

    testing.refAllDeclsRecursive(@import("main.zig"));
    testing.refAllDeclsRecursive(@import("cache/mod.zig"));
    testing.refAllDeclsRecursive(@import("cache/CacheModule.zig"));

    // TODO: Remove after hooking up
    testing.refAllDeclsRecursive(@import("reporting"));
    testing.refAllDeclsRecursive(@import("reporting/test.zig"));
    testing.refAllDeclsRecursive(@import("eval/interpreter.zig"));
    testing.refAllDeclsRecursive(@import("eval/test/eval_test.zig"));
    testing.refAllDeclsRecursive(@import("snapshot.zig"));
    testing.refAllDeclsRecursive(@import("layout/layout.zig"));
    testing.refAllDeclsRecursive(@import("layout/store.zig"));
    testing.refAllDeclsRecursive(@import("layout/store_test.zig"));
    testing.refAllDeclsRecursive(@import("repl/eval.zig"));
}
