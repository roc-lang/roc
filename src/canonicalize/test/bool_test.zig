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

test "canonicalize unary not as a call to builtin Bool.not" {
    var test_env = try TestEnv.init("!True");
    defer test_env.deinit();

    const canonical_expr = (try test_env.canonicalizeExpr()).?;
    const call = test_env.getCanonicalExpr(canonical_expr.get_idx()).e_call;
    try testing.expectEqual(.unary_op, call.called_via);
    const callee = test_env.getCanonicalExpr(call.func).e_lookup_associated;
    try testing.expectEqualStrings("Bool", test_env.getIdent(callee.type_ident));
    try testing.expectEqualStrings("not", test_env.getIdent(callee.item_ident));
    const args = test_env.module_env.store.sliceExpr(call.args);
    try testing.expectEqual(@as(usize, 1), args.len);
    try testing.expectEqualStrings("True", test_env.getIdent(test_env.getCanonicalExpr(args[0]).e_tag.name));
}

test "canonicalize unary not ignores a shadowing Bool declaration" {
    const source =
        \\{
        \\    Bool := [Other].{
        \\        not = |value| value
        \\    }
        \\    !True
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = (try test_env.canonicalizeExpr()).?;
    const block = test_env.getCanonicalExpr(canonical_expr.get_idx()).e_block;
    const call = test_env.getCanonicalExpr(block.final_expr).e_call;
    const callee = test_env.getCanonicalExpr(call.func).e_lookup_associated;
    try testing.expectEqualStrings("Bool", test_env.getIdent(callee.type_ident));
    try testing.expectEqualStrings("not", test_env.getIdent(callee.item_ident));
    const builtin_bool = test_env.can.builtin_auto_imported_types.get(test_env.module_env.idents.bool).?;
    try testing.expectEqual(builtin_bool.env.getExposedNodeIndexByStatementIdx(builtin_bool.statement_idx.?).?, callee.type_node_idx);
}

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
