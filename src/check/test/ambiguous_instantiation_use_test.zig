//! An ambiguity verdict about one instantiation of a constrained scheme
//! retires that instantiation's use. The scheme's body is shared by every
//! use, so it stays intact and other uses keep calling it.

const std = @import("std");
const TestEnv = @import("./TestEnv.zig");

const apply_with_where_clause =
    \\apply : p -> Str where [p.to_str : p -> Str]
    \\apply = |x| x.to_str()
    \\
    \\
;

test "an undetermined where-clause receiver retires its use and keeps the shared body" {
    const source = apply_with_where_clause ++
        \\shown : Str
        \\shown = if (Bool.False) (apply(crash "boom")) else ("ok")
        \\
        \\five = apply(5.U8)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("Missing Method");

    const store = &test_env.module_env.store;
    const lambda = store.getExpr(try test_env.defExpr("apply"));
    try std.testing.expect(lambda == .e_lambda);
    try std.testing.expect(store.getExpr(lambda.e_lambda.body) != .e_runtime_error);
    try std.testing.expect(store.getExpr(try test_env.defExpr("five")) != .e_runtime_error);

    // The report points at the use whose instantiation is undetermined.
    const region = test_env.checker.problems.problems.items[0].static_dispatch.unresolved_dispatcher.region;
    const shown_expr = store.getExpr(try test_env.defExpr("shown"));
    try std.testing.expect(shown_expr == .e_if);
    const then_body = store.getIfBranch(store.sliceIfBranches(shown_expr.e_if.branches)[0]).body;
    const then_region = store.getExprRegion(then_body);
    try std.testing.expect(region.start.offset >= then_region.start.offset);
    try std.testing.expect(region.end.offset <= then_region.end.offset);
}

test "a where-clause receiver its use determines checks cleanly" {
    const source = apply_with_where_clause ++
        \\five = apply(5.U8)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertNoErrors();
}
