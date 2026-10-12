//! Regression tests for https://github.com/roc-lang/roc/issues/12016.
//!
//! A lambda that supplies a block's, conditional's, or match's value is
//! consumed by that enclosing expression, so it is not generalized on its own:
//! the enclosing expression never acquires a scheme. A binding of such an
//! expression is a weak value binding, so it is monomorphic, and an annotation
//! cannot make it polymorphic (design.md "Value Bindings Generalize By
//! Expression").
const TestEnv = @import("TestEnv.zig");

fn expectOneTypeMismatch(comptime source: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Type Mismatch");
}

test "issue 12016: an unannotated binding of a conditional between lambdas is monomorphic" {
    try expectOneTypeMismatch(
        \\pick = |c| {
        \\    id = if c |s| s else |s| s
        \\    (id("x"), id(1.U8))
        \\}
    );
}

test "issue 12016: an unannotated binding of a match between lambdas is monomorphic" {
    try expectOneTypeMismatch(
        \\pick = |c| {
        \\    id = match c {
        \\        True => |s| s
        \\        False => |s| s
        \\    }
        \\    (id("x"), id(1.U8))
        \\}
    );
}

test "issue 12016: an unannotated binding of a block ending in a lambda is monomorphic" {
    try expectOneTypeMismatch(
        \\pick = |_| {
        \\    id = { |s| s }
        \\    (id("x"), id(1.U8))
        \\}
    );
}

test "issue 12016: an annotation cannot make a conditional between lambdas polymorphic" {
    var test_env = try TestEnv.init("Test",
        \\pick : Bool -> Str
        \\pick = |c| {
        \\    id : a -> a
        \\    id = if c |s| s else |s| s
        \\    id("x")
        \\}
    );
    defer test_env.deinit();
    try test_env.assertOneTypeError("Value Is Not Polymorphic");
}

test "issue 12016: a record stores lambdas chosen by a conditional" {
    var test_env = try TestEnv.init("Test",
        \\format : Bool -> Str
        \\format = |c| {
        \\    config = { format: if c |s| s.trim() else |s| s }
        \\    (config.format)("  hi  ")
        \\}
    );
    defer test_env.deinit();
    try test_env.assertNoErrors();
}
