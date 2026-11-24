//! Regression tests for https://github.com/roc-lang/roc/issues/13342.
//!
//! A field or tuple access evaluates its receiver before it produces a value.
//! A receiver that is an erroneous value crashes where it is evaluated, so the
//! access produces nothing even when the component it reads has an error-free
//! type. The access is retired like every other consumer of an erroneous
//! operand: it becomes a runtime error that still evaluates its receiver, and
//! no later stage reads the receiver's erroneous type.

const std = @import("std");
const CIR = @import("can").CIR;
const TestEnv = @import("./TestEnv.zig");

fn expectAccessRetired(source: []const u8, def_name: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneCanError("Name Not In Scope");
    try std.testing.expectEqual(@as(usize, 0), try test_env.typeProblemCount());

    const store = &test_env.module_env.store;
    const access = store.getExpr(try test_env.defExpr(def_name));
    try std.testing.expectEqual(std.meta.Tag(CIR.Expr).e_runtime_error, std.meta.activeTag(access));
    // The receiver is the access's only operand and the one that crashes.
    const evaluated = store.sliceExpr(access.e_runtime_error.evaluated);
    try std.testing.expectEqual(@as(usize, 1), evaluated.len);
    try std.testing.expect(store.getExpr(evaluated[0]) == .e_runtime_error);
}

test "issue 13342: a field access reading an error-free field of an erroneous record is retired" {
    try expectAccessRetired("n = { bad: nope, n: 5.U64 }.n", "n");
}

test "issue 13342: an annotated field access reading an error-free field of an erroneous record is retired" {
    try expectAccessRetired(
        \\n : U64
        \\n = { bad: nope, n: 5.U64 }.n
    , "n");
}

test "issue 13342: a tuple access reading an error-free item of an erroneous tuple is retired" {
    try expectAccessRetired("n = (nope, 5.U64).1", "n");
}

test "issue 13342: a field access path through an erroneous record is retired" {
    try expectAccessRetired("n = { a: { bad: nope, n: 5.U64 }, b: 1.U64 }.a.n", "n");
}
