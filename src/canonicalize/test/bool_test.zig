//! Tests for canonicalizing boolean expressions
const std = @import("std");

const testing = std.testing;

const TestEnv = @import("TestEnv.zig").TestEnv;
const Emitter = @import("../RocEmitter.zig");

test "canonicalize unary not retains its surface syntax when emitted" {
    for ([_][]const u8{ "!True", "!(!True)", "(!True).value", "(!True)(False)" }) |source| {
        var test_env = try TestEnv.init(source);
        defer test_env.deinit();

        const canonical_expr = (try test_env.canonicalizeExpr()).?;
        var emitter = Emitter.init(testing.allocator, test_env.module_env);
        defer emitter.deinit();
        try emitter.emitExpr(canonical_expr.get_idx());
        try testing.expectEqualStrings(source, emitter.getOutput());
    }
}

test "canonicalize unary not as a unary not node" {
    var test_env = try TestEnv.init("!True");
    defer test_env.deinit();

    const canonical_expr = (try test_env.canonicalizeExpr()).?;
    const unary = test_env.getCanonicalExpr(canonical_expr.get_idx()).e_unary_not;
    try testing.expectEqualStrings("True", test_env.getIdent(test_env.getCanonicalExpr(unary.expr).e_tag.name));
}

test "canonicalize True as Bool" {
    const source = "True";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse return error.TestUnexpectedResult;

    // Get the expression
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    try testing.expectEqual(.e_tag, std.meta.activeTag(expr));

    // The tag should be "True"
    const tag_name = test_env.getIdent(expr.e_tag.name);
    try testing.expectEqualStrings("True", tag_name);
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

    const canonical_expr = try test_env.canonicalizeExpr() orelse return error.TestUnexpectedResult;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    try testing.expectEqual(.e_block, std.meta.activeTag(expr));

    const final_expr = test_env.getCanonicalExpr(expr.e_block.final_expr);
    try testing.expectEqual(.e_tag, std.meta.activeTag(final_expr));
    try testing.expectEqualStrings("Fmt", test_env.getIdent(final_expr.e_tag.name));
}
