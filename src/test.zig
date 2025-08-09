//! Test runner for the Roc compiler.
//!
//! This module provides a centralized test runner that recursively references
//! all declarations from the main compiler modules to ensure all tests are
//! discovered and executed during the test phase.

const std = @import("std");

test {
    std.testing.refAllDecls(@import("base"));
    std.testing.refAllDecls(@import("base/test/base_test.zig"));

    std.testing.refAllDecls(@import("builtins"));
    std.testing.refAllDecls(@import("builtins/test/dec_test.zig"));
    std.testing.refAllDecls(@import("builtins/test/hash_test.zig"));
    std.testing.refAllDecls(@import("builtins/test/list_test.zig"));
    std.testing.refAllDecls(@import("builtins/test/sort_test.zig"));
    std.testing.refAllDecls(@import("builtins/test/str_test.zig"));
    std.testing.refAllDecls(@import("builtins/test/utils.test.zig"));
    std.testing.refAllDecls(@import("builtins/test/num_test.zig"));

    std.testing.refAllDeclsRecursive(@import("collections"));

    std.testing.refAllDecls(@import("types"));
    std.testing.refAllDecls(@import("types/test/rigid_instantiation.zig"));
    std.testing.refAllDecls(@import("test_shared_memory_system.zig"));

    std.testing.refAllDeclsRecursive(@import("serialization"));
    std.testing.refAllDecls(@import("serialization/test/compact_writer_test.zig"));

    std.testing.refAllDecls(@import("can"));
    std.testing.refAllDecls(@import("canonicalize/test/bool_test.zig"));
    std.testing.refAllDecls(@import("canonicalize/test/exposed_shadowing_test.zig"));
    std.testing.refAllDecls(@import("canonicalize/test/frac_test.zig"));
    std.testing.refAllDecls(@import("canonicalize/test/import_validation_test.zig"));
    std.testing.refAllDecls(@import("canonicalize/test/int_test.zig"));
    std.testing.refAllDecls(@import("canonicalize/test/node_store_test.zig"));

    std.testing.refAllDecls(@import("check"));
    std.testing.refAllDecls(@import("check/test/cross_module_test.zig"));
    std.testing.refAllDecls(@import("check/test/let_polymorphism_integration_test.zig"));
    std.testing.refAllDecls(@import("check/test/let_polymorphism_test.zig"));
    std.testing.refAllDecls(@import("check/test/literal_size_test.zig"));
    std.testing.refAllDecls(@import("check/test/nominal_type_origin_test.zig"));
    std.testing.refAllDecls(@import("check/test/static_dispatch_test.zig"));

    std.testing.refAllDecls(@import("parse"));
    std.testing.refAllDecls(@import("parse/test/ast_node_store_test.zig"));

    std.testing.refAllDeclsRecursive(@import("compile"));
    std.testing.refAllDecls(@import("compile/test/module_env_test.zig"));

    std.testing.refAllDeclsRecursive(@import("eval/interpreter.zig"));
    std.testing.refAllDeclsRecursive(@import("eval/test/eval_test.zig"));

    std.testing.refAllDeclsRecursive(@import("main.zig"));
    std.testing.refAllDeclsRecursive(@import("cache"));

    // TODO: Remove after hooking up
    std.testing.refAllDeclsRecursive(@import("reporting"));
    std.testing.refAllDeclsRecursive(@import("reporting/test.zig"));
    std.testing.refAllDeclsRecursive(@import("snapshot.zig"));
    std.testing.refAllDeclsRecursive(@import("layout/layout.zig"));
    std.testing.refAllDeclsRecursive(@import("layout/store.zig"));
    std.testing.refAllDeclsRecursive(@import("layout/store_test.zig"));
    std.testing.refAllDeclsRecursive(@import("repl/eval.zig"));
}
