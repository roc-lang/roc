//! Coverage for the generalization nesting limit: a rank is 14 bits, so a
//! generalization scope nested inside one at `Rank.deepest` gets no rank of
//! its own, and the module reports that once. Reaching `Rank.deepest` takes
//! 16,382 nested scopes, so these tests lower the deepest rank the checker's
//! envs give a scope; the counting is the same at any limit.

const std = @import("std");
const types = @import("types");
const TestEnv = @import("./TestEnv.zig");

const Rank = types.Rank;
const testing = std.testing;

const nested_functions =
    \\f0 = |_| {
    \\    f1 = |_| {
    \\        f2 = |_| {
    \\            f3 = |_| {
    \\                f4 = |_| 1
    \\                f4(0)
    \\            }
    \\            f3(0)
    \\        }
    \\        f2(0)
    \\    }
    \\    f1(0)
    \\}
;

fn rankAt(depth: u14) Rank {
    return @fromBackingInt(depth);
}

test "generalization nesting limit: scopes past the deepest rank report once" {
    var test_env = try TestEnv.initWithDeepestRank("Test", nested_functions, rankAt(3));
    defer test_env.deinit();

    try test_env.assertOneTypeError("Definitions Nested Too Deeply");
    try test_env.assertOneTypeProblemMsgContains(
        "Definitions Nested Too Deeply",
        "This definition is nested inside too many other definitions for the compiler to generalize its type.",
    );
}

test "generalization nesting limit: two scopes past the limit still report once" {
    const source =
        \\f0 = |_| {
        \\    f1 = |_| {
        \\        a = |_| { b = |_| 1
        \\        b(0) }
        \\        c = |_| { d = |_| 2
        \\        d(0) }
        \\        a(0) + c(0)
        \\    }
        \\    f1(0)
        \\}
    ;
    var test_env = try TestEnv.initWithDeepestRank("Test", source, rankAt(3));
    defer test_env.deinit();

    try test_env.assertOneTypeError("Definitions Nested Too Deeply");
}

test "generalization nesting limit: nesting within the deepest rank reports nothing" {
    var test_env = try TestEnv.initWithDeepestRank("Test", nested_functions, .deepest);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "generalization nesting limit: a definition past the limit is retired with everything nested in it" {
    var test_env = try TestEnv.initWithDeepestRank("Test", nested_functions, rankAt(3));
    defer test_env.deinit();

    // Walk down the chain of nested definitions from `f0`. The first one past
    // the limit is an erroneous value, so no later stage reads a definition
    // that was checked without being generalized; the ones around it are
    // still functions.
    const store = &test_env.module_env.store;
    var function = try test_env.defExpr("f0");
    var functions_kept: usize = 0;
    const retired = while (functions_kept < 5) {
        const expr = store.getExpr(function);
        if (expr == .e_runtime_error) break true;
        const lambda = if (expr == .e_closure)
            store.getExpr(expr.e_closure.lambda_idx).e_lambda
        else if (expr == .e_lambda)
            expr.e_lambda
        else
            break false;
        functions_kept += 1;
        const body = store.getExpr(lambda.body);
        if (body != .e_block) break false;
        const statements = store.sliceStatements(body.e_block.stmts);
        if (statements.len == 0) break false;
        const nested = store.getStatement(statements[0]);
        if (nested != .s_decl) break false;
        function = nested.s_decl.expr;
    } else false;
    try testing.expect(retired);
    try testing.expect(functions_kept >= 1 and functions_kept < 5);
}
