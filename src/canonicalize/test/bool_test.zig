//! Tests for canonicalizing boolean expressions
const std = @import("std");

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

test "canonicalize local nominal name as plain tag in expression position" {
    const source =
        \\{
        \\    Fmt := {}
        \\    Fmt
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    try testing.expectEqual(.e_block, std.meta.activeTag(expr));

    const final_expr = test_env.getCanonicalExpr(expr.e_block.final_expr);
    try testing.expectEqual(.e_tag, std.meta.activeTag(final_expr));
    try testing.expectEqualStrings("Fmt", test_env.getIdent(final_expr.e_tag.name));
}
