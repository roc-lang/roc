//! Tests for canonicalizing vars declared without an initial value.

const std = @import("std");
const testing = std.testing;

const TestEnv = @import("TestEnv.zig").TestEnv;

test "uninitialized var read becomes runtime error expression" {
    const source =
        \\{
        \\    var $value
        \\    $value
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const block = test_env.module_env.store.getExpr(canonical_expr.get_idx());
    try testing.expectEqual(.e_block, std.meta.activeTag(block));

    const statements = test_env.module_env.store.sliceStatements(block.e_block.stmts);
    try testing.expectEqual(@as(usize, 1), statements.len);

    const var_stmt = test_env.module_env.store.getStatement(statements[0]);
    try testing.expectEqual(.s_var_uninitialized, std.meta.activeTag(var_stmt));

    const final_expr = test_env.module_env.store.getExpr(block.e_block.final_expr);
    try testing.expectEqual(.e_runtime_error, std.meta.activeTag(final_expr));

    const diag = test_env.module_env.store.getDiagnostic(final_expr.e_runtime_error.diagnostic);
    try testing.expectEqual(.read_uninitialized_var, std.meta.activeTag(diag));
    try testing.expectEqualStrings("$value", test_env.getIdent(diag.read_uninitialized_var.ident));
}

test "write occurrences retain each destructured target and exclude fresh binders" {
    const source =
        \\{
        \\    var $x = 0
        \\    var $y
        \\    $y = 1
        \\    (a, var $x, var $z) = (1, $x + 1, 2)
        \\    { value: var $x, extra: b } = { value: $x + a, extra: $z }
        \\    ($x, c) = ($x + b, 3)
        \\    $x + $y + c
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();
    const result = try test_env.canonicalizeExpr() orelse unreachable;
    try testing.expect(!test_env.hasParseErrors());
    const store = &test_env.module_env.store;
    const block = store.getExpr(result.get_idx()).e_block;
    const statements = store.sliceStatements(block.stmts);
    const x = store.getStatement(statements[0]).s_var.pattern_idx;
    const y = store.getStatement(statements[1]).s_var_uninitialized.pattern_idx;
    const writes = store.write_occurrences.items.items;
    try testing.expectEqual(@as(usize, 4), writes.len);
    try testing.expectEqual(y, writes[0].pattern_idx);
    try testing.expectEqualStrings("$y", source[writes[0].start..writes[0].end]);
    for (writes[1..]) |write| {
        try testing.expectEqual(x, write.pattern_idx);
        try testing.expectEqualStrings("$x", source[write.start..write.end]);
        try testing.expect(write.start > store.getPatternRegion(x).end.offset);
    }
    try testing.expect(writes[1].start < writes[2].start);
    try testing.expect(writes[2].start < writes[3].start);
}
