//! Tests for canonicalizing while loops.

const std = @import("std");
const testing = std.testing;

const CIR = @import("../CIR.zig");
const TestEnv = @import("TestEnv.zig").TestEnv;

const WhileLoopTestError = std.mem.Allocator.Error || error{
    ExpectedDiagnosticNotFound,
    ExpectedLambda,
    TestExpectedEqual,
    TestUnexpectedResult,
};

fn firstBlockStatement(test_env: *TestEnv, expr_idx: CIR.Expr.Idx) WhileLoopTestError!CIR.Statement {
    return try blockStatementAt(test_env, expr_idx, 0);
}

fn blockStatementAt(test_env: *TestEnv, expr_idx: CIR.Expr.Idx, index: usize) WhileLoopTestError!CIR.Statement {
    const expr = test_env.getCanonicalExpr(expr_idx);
    try testing.expectEqual(.e_block, std.meta.activeTag(expr));

    const statements = test_env.module_env.store.sliceStatements(expr.e_block.stmts);
    try testing.expect(index < statements.len);

    return test_env.module_env.store.getStatement(statements[index]);
}

fn firstLambdaBodyStatement(test_env: *TestEnv, expr_idx: CIR.Expr.Idx) WhileLoopTestError!CIR.Statement {
    const expr = test_env.getCanonicalExpr(expr_idx);
    const lambda_idx = switch (expr) {
        .e_lambda => expr_idx,
        .e_closure => |closure| closure.lambda_idx,
        else => return error.ExpectedLambda,
    };

    const lambda = test_env.getCanonicalExpr(lambda_idx);
    try testing.expectEqual(.e_lambda, std.meta.activeTag(lambda));

    return try firstBlockStatement(test_env, lambda.e_lambda.body);
}

fn expectDiagnosticTag(test_env: *TestEnv, expected: std.meta.Tag(CIR.Diagnostic)) WhileLoopTestError!void {
    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    for (diagnostics) |diagnostic| {
        if (std.meta.activeTag(diagnostic) == expected) return;
    }

    return error.ExpectedDiagnosticNotFound;
}

fn expectNoDiagnosticTag(test_env: *TestEnv, unexpected: std.meta.Tag(CIR.Diagnostic)) WhileLoopTestError!void {
    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    for (diagnostics) |diagnostic| {
        try testing.expect(std.meta.activeTag(diagnostic) != unexpected);
    }
}

test "while True with an exit canonicalizes as infinite_loop" {
    const source =
        \\{
        \\    while True {
        \\        crash "done"
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
}

test "while parenthesized True canonicalizes as infinite_loop" {
    const source =
        \\{
        \\    while ((True)) {
        \\        crash "done"
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
}

test "while Bool.True canonicalizes as infinite_loop" {
    const source =
        \\{
        \\    while Bool.True {
        \\        crash "done"
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
}

test "while False does not canonicalize as infinite_loop" {
    const source =
        \\{
        \\    while False {
        \\        crash "done"
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_while, std.meta.activeTag(stmt));
}

test "while Bool.False does not canonicalize as infinite_loop" {
    const source =
        \\{
        \\    while Bool.False {
        \\        crash "done"
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_while, std.meta.activeTag(stmt));
}

test "while block True condition with no statements canonicalizes as infinite_loop" {
    const source =
        \\{
        \\    while { True } {
        \\        crash "done"
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
}

test "while block True condition with statements does not canonicalize as infinite_loop" {
    const source =
        \\{
        \\    while {
        \\        dbg "condition"
        \\        True
        \\    } {
        \\        crash "done"
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_while, std.meta.activeTag(stmt));
}

test "non-builtin nominal True does not canonicalize as infinite_loop" {
    const source =
        \\{
        \\    MyBool := [False, True]
        \\    while MyBool.True {
        \\        crash "done"
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try blockStatementAt(&test_env, canonical_expr.get_idx(), 1);

    try testing.expectEqual(.s_while, std.meta.activeTag(stmt));
}

test "while True with loop-owned break canonicalizes as breakable_loop" {
    const source =
        \\{
        \\    while True {
        \\        break
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_breakable_loop, std.meta.activeTag(stmt));
}

test "break inside if makes while True breakable_loop" {
    const source =
        \\{
        \\    while True {
        \\        if Bool.True {
        \\            break
        \\        }
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_breakable_loop, std.meta.activeTag(stmt));
}

test "break inside nested loop does not make outer while True breakable" {
    const source =
        \\{
        \\    while True {
        \\        for item in [] {
        \\            break
        \\        }
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
    try expectDiagnosticTag(&test_env, .infinite_loop_never_exits);
}

test "break inside nested while does not make outer while True breakable" {
    const source =
        \\{
        \\    while True {
        \\        while Bool.False {
        \\            break
        \\        }
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
    try expectDiagnosticTag(&test_env, .infinite_loop_never_exits);
}

test "while True with no exits warns" {
    const source =
        \\{
        \\    while True {
        \\        dbg "still running"
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
    try expectDiagnosticTag(&test_env, .infinite_loop_never_exits);
}

test "return inside while True suppresses infinite loop warning" {
    const source =
        \\|_| {
        \\    while True {
        \\        return 1
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstLambdaBodyStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
    try expectNoDiagnosticTag(&test_env, .infinite_loop_never_exits);
}

test "try suffix inside while True suppresses infinite loop warning" {
    const source =
        \\|_| {
        \\    while True {
        \\        Try.Err("bad")?
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstLambdaBodyStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
    try expectNoDiagnosticTag(&test_env, .infinite_loop_never_exits);
}

test "crash inside while True suppresses infinite loop warning" {
    const source =
        \\{
        \\    while True {
        \\        crash "done"
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
    try expectNoDiagnosticTag(&test_env, .infinite_loop_never_exits);
}

test "expect inside while True does not suppress infinite loop warning" {
    const source =
        \\{
        \\    while True {
        \\        expect Bool.True
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
    try expectDiagnosticTag(&test_env, .infinite_loop_never_exits);
}

test "try suffix inside expect does not suppress infinite loop warning" {
    const source =
        \\{
        \\    while True {
        \\        expect Try.Err("bad")? == "ok"
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
    try expectDiagnosticTag(&test_env, .infinite_loop_never_exits);
}

test "return inside nested lambda does not count as infinite loop exit" {
    const source =
        \\{
        \\    while True {
        \\        f = |_| {
        \\            return 1
        \\        }
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
    try expectDiagnosticTag(&test_env, .infinite_loop_never_exits);
}

test "crash inside nested lambda does not count as infinite loop exit" {
    const source =
        \\{
        \\    while True {
        \\        f = |_| {
        \\            crash "done"
        \\        }
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const stmt = try firstBlockStatement(&test_env, canonical_expr.get_idx());

    try testing.expectEqual(.s_infinite_loop, std.meta.activeTag(stmt));
    try expectDiagnosticTag(&test_env, .infinite_loop_never_exits);
}
