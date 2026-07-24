//! Source-level tests for canonical where-clause receiver ownership.

const std = @import("std");
const testing = std.testing;

const CIR = @import("../CIR.zig");
const TestEnv = @import("TestEnv.zig").TestEnv;

const TestError = std.mem.Allocator.Error || error{
    ExpectedLambda,
    TestExpectedEqual,
    TestUnexpectedResult,
};

fn firstStatement(test_env: *TestEnv, block_idx: CIR.Expr.Idx) TestError!CIR.Statement {
    const block = test_env.getCanonicalExpr(block_idx);
    try testing.expectEqual(.e_block, std.meta.activeTag(block));
    const statements = test_env.module_env.store.sliceStatements(block.e_block.stmts);
    try testing.expect(statements.len > 0);
    return test_env.module_env.store.getStatement(statements[0]);
}

fn lambdaBody(test_env: *TestEnv, expr_idx: CIR.Expr.Idx) TestError!CIR.Expr.Idx {
    const expr = test_env.getCanonicalExpr(expr_idx);
    const lambda_idx = switch (expr) {
        .e_lambda => expr_idx,
        .e_closure => |closure| closure.lambda_idx,
        else => return error.ExpectedLambda,
    };
    const lambda = test_env.getCanonicalExpr(lambda_idx);
    try testing.expectEqual(.e_lambda, std.meta.activeTag(lambda));
    return lambda.e_lambda.body;
}

test "canonical where ownership follows rigid declarations through signatures" {
    const source =
        \\{
        \\    chain : c -> a where [c.get : c -> item, item.get : item -> a]
        \\    chain = |value| value
        \\    chain
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const root = try test_env.canonicalizeExpr() orelse unreachable;
    const statement = try firstStatement(&test_env, root.get_idx());
    try testing.expectEqual(.s_decl, std.meta.activeTag(statement));
    const annotation = test_env.module_env.store.getAnnotation(statement.s_decl.anno.?);
    const where = annotation.where.?;
    const owners = test_env.module_env.store.sliceWhereClauseOwners(where);
    try testing.expectEqual(@as(usize, 2), owners.len);

    for (owners) |owner| {
        try testing.expect(owner.introduced_in_scope);
        const owner_idx: CIR.TypeAnno.Idx = @enumFromInt(owner.rigid_var);
        try testing.expectEqual(.rigid_var, std.meta.activeTag(test_env.module_env.store.getTypeAnno(owner_idx)));
        for (test_env.module_env.store.sliceWhereClausesForOwner(owner)) |where_idx| {
            const method = test_env.module_env.store.getWhereClause(where_idx).w_method;
            switch (test_env.module_env.store.getTypeAnno(method.var_)) {
                .rigid_var => try testing.expectEqual(owner_idx, method.var_),
                .rigid_var_lookup => |lookup| try testing.expectEqual(owner_idx, lookup.ref),
                else => return error.ExpectedRigidReceiver,
            }
        }
    }
}

test "inner where lookup does not take ownership of enclosing rigid" {
    const source =
        \\{
        \\    outer : a -> Str
        \\    outer = |value| {
        \\        inner : a -> Str where [a.show : a -> Str]
        \\        inner = |_ignored| "ok"
        \\        inner(value)
        \\    }
        \\    outer
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const root = try test_env.canonicalizeExpr() orelse unreachable;
    const outer = try firstStatement(&test_env, root.get_idx());
    try testing.expectEqual(.s_decl, std.meta.activeTag(outer));
    const inner = try firstStatement(&test_env, try lambdaBody(&test_env, outer.s_decl.expr));
    try testing.expectEqual(.s_decl, std.meta.activeTag(inner));

    const annotation = test_env.module_env.store.getAnnotation(inner.s_decl.anno.?);
    const owners = test_env.module_env.store.sliceWhereClauseOwners(annotation.where.?);
    try testing.expectEqual(@as(usize, 1), owners.len);
    try testing.expect(!owners[0].introduced_in_scope);

    const method_idx = test_env.module_env.store.sliceWhereClausesForOwner(owners[0])[0];
    const method = test_env.module_env.store.getWhereClause(method_idx).w_method;
    const lookup = test_env.module_env.store.getTypeAnno(method.var_).rigid_var_lookup;
    try testing.expectEqual(owners[0].rigid_var, @intFromEnum(lookup.ref));
}
