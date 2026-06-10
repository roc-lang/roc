//! Tests for range operator (`..<` / `..=`) canonicalization into CIR binops.

const std = @import("std");
const testing = std.testing;
const CIR = @import("../CIR.zig");
const TestEnv = @import("TestEnv.zig").TestEnv;
const ModuleEnv = @import("../ModuleEnv.zig");

fn getBinop(module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) error{NotABinop}!CIR.Expr.Binop {
    const expr = module_env.store.getExpr(expr_idx);
    return switch (expr) {
        .e_binop => |binop| binop,
        else => error.NotABinop,
    };
}

test "canonicalize exclusive range to range_to_excluding binop" {
    var test_env = try TestEnv.init("1..<5");
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const binop = try getBinop(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(CIR.Expr.Binop.Op.range_to_excluding, binop.op);
}

test "canonicalize inclusive range to range_to_including binop" {
    var test_env = try TestEnv.init("1..=5");
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const binop = try getBinop(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(CIR.Expr.Binop.Op.range_to_including, binop.op);
}

test "chained ranges are rejected with a diagnostic" {
    var test_env = try TestEnv.init("1..<5..<10");
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.module_env.store.getExpr(canonical_expr.get_idx());
    try testing.expect(expr == .e_runtime_error);

    // Verify it's specifically the chained-range diagnostic, not some other error.
    const diag = test_env.module_env.store.getDiagnostic(expr.e_runtime_error.diagnostic);
    try testing.expect(diag == .range_op_chained);
}
