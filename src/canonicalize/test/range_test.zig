//! Tests for range operator (`..<` / `..=`) canonicalization into
//! `Iter.exclusive_range` / `Iter.inclusive_range` constructor calls.

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const CIR = @import("../CIR.zig");
const TestEnv = @import("TestEnv.zig").TestEnv;
const ModuleEnv = @import("../ModuleEnv.zig");

fn getCall(module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) error{NotACall}!@TypeOf(@as(CIR.Expr, undefined).e_call) {
    const expr = module_env.store.getExpr(expr_idx);
    return switch (expr) {
        .e_call => |call| call,
        else => error.NotACall,
    };
}

test "canonicalize exclusive range to Iter.exclusive_range call" {
    var test_env = try TestEnv.init("1..<5");
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const call = try getCall(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(base.CalledVia.range, call.called_via);

    const func = test_env.module_env.store.getExpr(call.func);
    try testing.expect(func == .e_lookup_external);
}

test "canonicalize inclusive range to Iter.inclusive_range call" {
    var test_env = try TestEnv.init("1..=5");
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const call = try getCall(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(base.CalledVia.range, call.called_via);

    const func = test_env.module_env.store.getExpr(call.func);
    try testing.expect(func == .e_lookup_external);
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
