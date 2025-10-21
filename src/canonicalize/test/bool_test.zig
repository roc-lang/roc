//! Tests for canonicalizing boolean expressions
const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const CIR = @import("../CIR.zig");
const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");

const testing = std.testing;

const TestEnv = @import("TestEnv.zig").TestEnv;

test "canonicalize True as Bool" {
    const source = "True";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;

    // Get the expression
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    try testing.expectEqual(.e_tag, std.meta.activeTag(expr));

    // The tag should be "True"
    const tag_name = test_env.getIdent(expr.e_tag.name);
    try testing.expectEqualStrings("True", tag_name);
}

test "canonicalize False as Bool" {
    const source = "False";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;

    // Get the expression
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    try testing.expectEqual(.e_tag, std.meta.activeTag(expr));

    // The tag should be "False"
    const tag_name = test_env.getIdent(expr.e_tag.name);
    try testing.expectEqualStrings("False", tag_name);
}

test "canonicalize random tag not as Bool" {
    const source = "SomeTag";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;

    // Get the expression
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    // Check that it's NOT a nominal expression - just a plain tag
    try testing.expectEqual(.e_tag, std.meta.activeTag(expr));

    // The tag should be "SomeTag"
    const tag_name = test_env.getIdent(expr.e_tag.name);
    try testing.expectEqualStrings("SomeTag", tag_name);
}
