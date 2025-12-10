//! Tests for recursive alias detection.
//!
//! Type aliases (`:`) cannot be recursive because they are transparent type synonyms.
//! Recursive types must use nominal types (`:=`) instead.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

const testing = std.testing;

// ============================================================================
// Direct self-reference tests
// ============================================================================

test "recursive alias - direct self-reference without args" {
    // Simple recursive alias: A : List(A)
    const source =
        \\A : List(A)
    ;
    var test_env = try TestEnv.init("A", source);
    defer test_env.deinit();
    try test_env.assertFirstTypeError("RECURSIVE ALIAS");
}

test "recursive alias - direct self-reference with args (apply case)" {
    // Parameterized recursive alias: Node(a) : { value: a, children: List(Node(a)) }
    const source =
        \\Node(a) : { value: a, children: List(Node(a)) }
    ;
    var test_env = try TestEnv.init("Node", source);
    defer test_env.deinit();
    try test_env.assertFirstTypeError("RECURSIVE ALIAS");
}

// ============================================================================
// Nominal type recursion is allowed
// ============================================================================

test "nominal type - direct self-reference is allowed" {
    // Nominal types can be recursive
    const source =
        \\Node := [Node({ value: Str, children: List(Node) })]
    ;
    var test_env = try TestEnv.init("Node", source);
    defer test_env.deinit();
    // No error - nominal types can be recursive
    try test_env.assertNoErrors();
}

test "nominal type with args - self-reference is allowed" {
    // Parameterized nominal types can be recursive
    const source =
        \\Tree := [Empty, Node({ value: Str, left: Tree, right: Tree })]
    ;
    var test_env = try TestEnv.init("Tree", source);
    defer test_env.deinit();
    // No error - nominal types can be recursive
    try test_env.assertNoErrors();
}

// ============================================================================
// Non-recursive aliases should work
// ============================================================================

test "non-recursive alias - simple alias works" {
    const source =
        \\Point : { x: I64, y: I64 }
    ;
    var test_env = try TestEnv.init("Point", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "non-recursive alias - parameterized alias works" {
    const source =
        \\Pair(a, b) : (a, b)
    ;
    var test_env = try TestEnv.init("Pair", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}

test "non-recursive alias - alias to List works" {
    const source =
        \\IntList : List(I64)
    ;
    var test_env = try TestEnv.init("IntList", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}
