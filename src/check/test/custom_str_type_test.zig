//! Tests for custom string types that implement try_from_str
//!
//! This module contains tests for string literals that unify with custom types
//! via the try_from_str static dispatch constraint.
//!
//! String literals are polymorphic with the constraint:
//!   a where [a.try_from_str : Str -> Try(a, [InvalidStr(Str)])]
//! This allows custom types that implement try_from_str to accept string literals.
//! If unresolved at codegen time, they default to Str.

const std = @import("std");
const testing = std.testing;
const TestEnv = @import("./TestEnv.zig");

test "String literal produces Str type" {
    // String literals are polymorphic with try_from_str constraint
    const source = "\"hello world\"";

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();

    // String literals have try_from_str constraint
    try test_env.assertLastDefType("a where [a.try_from_str : Str -> Try(a, [InvalidStr(Str)])]");
}

test "String literal unifies with Str type annotation" {
    const source =
        \\  x : Str
        \\  x = "hello"
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Should type-check successfully - Str is the default/builtin string type
    try test_env.assertNoErrors();
    try test_env.assertLastDefType("Str");
}
