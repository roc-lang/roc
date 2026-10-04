//! Regression tests for rejected relations whose checker diagnostic must also
//! retire the exact owning node, so post-check lowering never instantiates the
//! rejected relation (https://github.com/roc-lang/roc/issues/12012,
//! https://github.com/roc-lang/roc/issues/12015,
//! https://github.com/roc-lang/roc/issues/12045), and whose rejection leaves
//! every shared solved class intact.

const std = @import("std");
const CIR = @import("can").CIR;
const TestEnv = @import("./TestEnv.zig");

fn liveRecordDestructureStatements(test_env: *TestEnv) usize {
    const store = &test_env.module_env.store;
    var count: usize = 0;
    var raw_node_idx: u32 = 0;
    while (raw_node_idx < store.nodes.len()) : (raw_node_idx += 1) {
        const node_idx: CIR.Node.Idx = @enumFromInt(raw_node_idx);
        if (store.nodes.get(node_idx).tag != .statement_decl) continue;
        const stmt = store.getStatement(@enumFromInt(raw_node_idx)).s_decl;
        if (store.getPattern(stmt.pattern) == .record_destructure) count += 1;
    }
    return count;
}

fn lambdaBody(test_env: *TestEnv, name: []const u8) !CIR.Expr {
    const store = &test_env.module_env.store;
    const def_expr = store.getExpr(try test_env.defExpr(name));
    const lambda_expr = if (def_expr == .e_closure) store.getExpr(def_expr.e_closure.lambda_idx) else def_expr;
    try std.testing.expect(lambda_expr == .e_lambda);
    return store.getExpr(lambda_expr.e_lambda.body);
}

test "a nested record destructure missing a field rejects its binding statement" {
    const source =
        \\run = || {
        \\    { a: { b } } = { a: {} }
        \\    b
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Type Mismatch");
    try std.testing.expectEqual(@as(usize, 0), liveRecordDestructureStatements(&test_env));
}

test "a destructured binder whose use demands another type rejects its binding statement" {
    const source =
        \\first_tag : { name : Str, tags : List(Str) } -> Str
        \\first_tag = |item| {
        \\    { tags, .. } = item
        \\    tags
        \\}
        \\
        \\name_of : { name : Str, tags : List(Str) } -> Str
        \\name_of = |item| {
        \\    { name, .. } = item
        \\    name
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Type Mismatch");
    // Only `first_tag`'s destructure is rejected; `name_of`'s stays live.
    try std.testing.expectEqual(@as(usize, 1), liveRecordDestructureStatements(&test_env));
    try std.testing.expect(try lambdaBody(&test_env, "name_of") == .e_block);
    // The rejected relation poisoned neither the destructured field nor the
    // binder's uses, so the annotation keeps its type.
    try test_env.assertDefTypeOptions("first_tag", "{ name: Str, tags: List(Str) } -> Str", .{ .allow_type_errors = true });
}

test "a top-level destructure whose binder relation is rejected retires the definition" {
    const source =
        \\{ a: { b } } = { a: { b: 1.U64, c: 2.U64 } }
        \\
        \\ok : U64
        \\ok = 5
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Type Mismatch");
    try test_env.assertDefTypeOptions("ok", "U64", .{ .allow_type_errors = true });
    const env = test_env.module_env;
    for (env.store.sliceDefs(env.all_defs)) |def_idx| {
        const def = env.store.getDef(def_idx);
        if (env.store.getPattern(def.pattern) != .record_destructure) continue;
        try std.testing.expect(env.store.getExpr(def.expr) == .e_runtime_error);
    }
}

test "an instantiated method constraint fails at its instantiating use, not inside the scheme" {
    const source =
        \\g = |r| r.is_ok()
        \\f = |a| g(a).x
        \\
        \\expect f([1].first())
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Type Mismatch");
    // Both helpers are valid for other receivers, so their bodies stay intact.
    try std.testing.expect(try lambdaBody(&test_env, "g") == .e_dispatch_call);
    try std.testing.expect(try lambdaBody(&test_env, "f") == .e_field_access);

    const env = test_env.module_env;
    var found_expect = false;
    for (env.store.sliceStatements(env.all_statements)) |stmt_idx| {
        const stmt = env.store.getStatement(stmt_idx);
        if (stmt != .s_expect) continue;
        found_expect = true;
        const body = env.store.getExpr(stmt.s_expect.body);
        const retired = body == .e_runtime_error or
            (body == .e_call and env.store.getExpr(body.e_call.func) == .e_runtime_error);
        try std.testing.expect(retired);
    }
    try std.testing.expect(found_expect);
}

test "a non-Bool right operand of and retires the operator and leaves its callee's type intact" {
    const source =
        \\describe : U64 -> [Verbose(U64)]
        \\describe = |n| Verbose(n)
        \\
        \\f = |x| Bool.True and describe(x)
        \\
        \\g = |n| describe(n)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(
        \\**Type Mismatch**
        \\I'm having trouble with this bool operation.
        \\```roc
        \\f = |x| Bool.True and describe(x)
        \\```
        \\                      ^^^^^^^^^^^
        \\
        \\Both sides of `and` must be `Bool` values, but the right side is:
        \\
        \\    [Verbose(U64)]
        \\
        \\__Note:__ Roc does not have "truthiness". You must convert values to bools yourself.
        \\
        \\
    );
    try test_env.assertDefTypeOptions("describe", "U64 -> [Verbose(U64)]", .{ .allow_type_errors = true });
    try test_env.assertDefTypeOptions("g", "U64 -> [Verbose(U64)]", .{ .allow_type_errors = true });
    try test_env.assertDefTypeOptions("f", "U64 -> Bool", .{ .allow_type_errors = true });
    try std.testing.expect(try lambdaBody(&test_env, "f") == .e_runtime_error);
}

test "a non-Bool left operand of or retires the operator" {
    const source =
        \\f = |_| Verbose or Bool.False
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Type Mismatch");
    try std.testing.expect(try lambdaBody(&test_env, "f") == .e_runtime_error);
}

test "Bool operands of and and or check" {
    const source =
        \\f : Bool, Bool -> Bool
        \\f = |a, b| (a and b) or !a
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertDefType("f", "Bool, Bool -> Bool");
}

test "binders of a rejected lambda parameter pattern add no reports of their own" {
    const source =
        \\Cents := [Cents(U64, U64)]
        \\
        \\show : Cents -> Str
        \\show = |Cents.Cents(c)| c.to_str()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Invalid Nominal Tag");
    try std.testing.expect(test_env.module_env.store.getExpr(try test_env.defExpr("show")) == .e_runtime_error);
}

test "binders of a rejected match branch pattern add no reports of their own" {
    const source =
        \\Cents := [Cents(U64, U64)]
        \\
        \\show : Cents -> Str
        \\show = |x| match x {
        \\    Cents.Cents(c) => c.to_str()
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Invalid Nominal Tag");
    try std.testing.expect(try lambdaBody(&test_env, "show") == .e_runtime_error);
}

test "a literal pattern whose conversion fails rejects its binding statement" {
    const source =
        \\run : (U64, U64) -> U64
        \\run = |pair| {
        \\    ("bad", rest) = pair
        \\    rest
        \\}
        \\
        \\ok : U64
        \\ok = 5
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try std.testing.expect(try test_env.typeProblemCount() != 0);
    try test_env.assertDefTypeOptions("ok", "U64", .{ .allow_type_errors = true });
    try std.testing.expect(try test_env.lambdaBodyStatement("run", 0) == .s_runtime_error);
}

test "a top-level literal pattern whose conversion fails retires the definition" {
    const source =
        \\pair : (U64, U64)
        \\pair = (1, 2)
        \\
        \\("bad", rest) = pair
        \\
        \\ok : U64
        \\ok = 5
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try std.testing.expect(try test_env.typeProblemCount() != 0);
    try test_env.assertDefTypeOptions("ok", "U64", .{ .allow_type_errors = true });
    const env = test_env.module_env;
    var retired_defs: usize = 0;
    for (env.store.sliceDefs(env.all_defs)) |def_idx| {
        const def = env.store.getDef(def_idx);
        if (env.store.getPattern(def.pattern) != .tuple) continue;
        try std.testing.expect(env.store.getExpr(def.expr) == .e_runtime_error);
        retired_defs += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), retired_defs);
}
