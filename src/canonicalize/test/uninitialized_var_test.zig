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
