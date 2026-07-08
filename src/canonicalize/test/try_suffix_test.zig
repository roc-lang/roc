//! Tests for Try suffix desugaring shape.

const std = @import("std");
const testing = std.testing;

const parse = @import("parse");
const CIR = @import("../CIR.zig");
const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const BuiltinTestContext = @import("BuiltinTestContext.zig").BuiltinTestContext;
const TestEnv = @import("TestEnv.zig").TestEnv;
const CoreCtx = @import("ctx").CoreCtx;

const TrySuffixTestError = std.mem.Allocator.Error || error{
    TestExpectedEqual,
    TestUnexpectedResult,
    ExpectedLambda,
    ExpectedNominalTryPattern,
    ExpectedNominalTryExpr,
};

fn expectMatch(
    test_env: *TestEnv,
    expr_idx: CIR.Expr.Idx,
    is_try_suffix: bool,
) TrySuffixTestError!CIR.Expr.Match {
    const expr = test_env.getCanonicalExpr(expr_idx);
    try testing.expectEqual(.e_match, std.meta.activeTag(expr));
    const match = expr.e_match;
    try testing.expectEqual(is_try_suffix, match.is_try_suffix);
    try testing.expectEqual(@as(usize, 2), test_env.module_env.store.sliceMatchBranches(match.branches).len);
    return match;
}

fn finalExpr(test_env: *TestEnv, expr_idx: CIR.Expr.Idx) CIR.Expr.Idx {
    return switch (test_env.getCanonicalExpr(expr_idx)) {
        .e_block => |block| block.final_expr,
        else => expr_idx,
    };
}

fn lambdaBodyFinalExpr(test_env: *TestEnv, expr_idx: CIR.Expr.Idx) TrySuffixTestError!CIR.Expr.Idx {
    const expr = test_env.getCanonicalExpr(expr_idx);
    const lambda_idx = switch (expr) {
        .e_lambda => expr_idx,
        .e_closure => |closure| closure.lambda_idx,
        else => return error.ExpectedLambda,
    };

    const lambda = test_env.getCanonicalExpr(lambda_idx);
    try testing.expectEqual(.e_lambda, std.meta.activeTag(lambda));
    return finalExpr(test_env, lambda.e_lambda.body);
}

fn branchAt(
    test_env: *TestEnv,
    match: CIR.Expr.Match,
    index: usize,
) TrySuffixTestError!CIR.Expr.Match.Branch {
    const branch_idxs = test_env.module_env.store.sliceMatchBranches(match.branches);
    try testing.expect(index < branch_idxs.len);
    return test_env.module_env.store.getMatchBranch(branch_idxs[index]);
}

fn branchPatternAt(
    test_env: *TestEnv,
    branch: CIR.Expr.Match.Branch,
    index: usize,
) TrySuffixTestError!CIR.Pattern.Idx {
    const branch_pattern_idxs = test_env.module_env.store.sliceMatchBranchPatterns(branch.patterns);
    try testing.expect(index < branch_pattern_idxs.len);
    const branch_pattern = test_env.module_env.store.getMatchBranchPattern(branch_pattern_idxs[index]);
    try testing.expect(!branch_pattern.degenerate);
    return branch_pattern.pattern;
}

fn expectTryTagPattern(
    test_env: *TestEnv,
    pattern_idx: CIR.Pattern.Idx,
    expected_tag: []const u8,
) TrySuffixTestError!void {
    const pattern = test_env.module_env.store.getPattern(pattern_idx);
    const backing_pattern_idx = switch (pattern) {
        .nominal => |nominal| nominal.backing_pattern,
        .nominal_external => |nominal| nominal.backing_pattern,
        else => return error.ExpectedNominalTryPattern,
    };

    const backing_pattern = test_env.module_env.store.getPattern(backing_pattern_idx);
    try testing.expectEqual(.applied_tag, std.meta.activeTag(backing_pattern));
    try testing.expectEqualStrings(expected_tag, test_env.getIdent(backing_pattern.applied_tag.name));
}

fn expectTryTagExpr(
    test_env: *TestEnv,
    expr_idx: CIR.Expr.Idx,
    expected_tag: []const u8,
) TrySuffixTestError!void {
    const expr = test_env.getCanonicalExpr(expr_idx);
    const backing_expr_idx = switch (expr) {
        .e_nominal => |nominal| nominal.backing_expr,
        .e_nominal_external => |nominal| nominal.backing_expr,
        else => return error.ExpectedNominalTryExpr,
    };

    const backing_expr = test_env.getCanonicalExpr(backing_expr_idx);
    try testing.expectEqual(.e_tag, std.meta.activeTag(backing_expr));
    try testing.expectEqualStrings(expected_tag, test_env.getIdent(backing_expr.e_tag.name));
}

fn expectBranchPatternTag(
    test_env: *TestEnv,
    branch: CIR.Expr.Match.Branch,
    expected_tag: []const u8,
) TrySuffixTestError!void {
    const pattern_idx = try branchPatternAt(test_env, branch, 0);
    try expectTryTagPattern(test_env, pattern_idx, expected_tag);
}

fn moduleContainsExpectErr(source: []const u8) TrySuffixTestError!bool {
    const allocator = testing.allocator;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");

    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();

    const roc_ctx = CoreCtx.testing(allocator, allocator);
    var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer can.deinit();

    try can.canonicalizeFile();

    var raw_node_idx: u32 = 0;
    while (raw_node_idx < env.store.nodes.len()) : (raw_node_idx += 1) {
        const node_idx: CIR.Node.Idx = @enumFromInt(raw_node_idx);
        if (env.store.nodes.get(node_idx).tag == .expr_expect_err) return true;
    }

    return false;
}

test "try suffix creates nominal wrapped try match" {
    var test_env = try TestEnv.init("Try.Ok(1)?");
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const match = try expectMatch(&test_env, canonical_expr.get_idx(), true);
    const ok_branch = try branchAt(&test_env, match, 0);
    const err_branch = try branchAt(&test_env, match, 1);

    try expectBranchPatternTag(&test_env, ok_branch, "Ok");
    try expectBranchPatternTag(&test_env, err_branch, "Err");
}

test "try suffix err branch returns nominal Err inside lambda" {
    var test_env = try TestEnv.init("|_| Try.Err(\"x\")?");
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const body_idx = try lambdaBodyFinalExpr(&test_env, canonical_expr.get_idx());
    const match = try expectMatch(&test_env, body_idx, true);
    const err_branch = try branchAt(&test_env, match, 1);

    const branch_value = test_env.getCanonicalExpr(err_branch.value);
    try testing.expectEqual(.e_return, std.meta.activeTag(branch_value));
    try expectTryTagExpr(&test_env, branch_value.e_return.expr, "Err");
}

test "single question binop shares try suffix ok branch shape" {
    var test_env = try TestEnv.init("|_| Try.Err(\"x\") ? Bad");
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const body_idx = try lambdaBodyFinalExpr(&test_env, canonical_expr.get_idx());
    const match = try expectMatch(&test_env, body_idx, true);
    const ok_branch = try branchAt(&test_env, match, 0);

    try expectBranchPatternTag(&test_env, ok_branch, "Ok");
}

test "double question shares try branch patterns without try suffix flag" {
    var test_env = try TestEnv.init("Try.Err(\"x\") ?? 0");
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const match = try expectMatch(&test_env, canonical_expr.get_idx(), false);
    const ok_branch = try branchAt(&test_env, match, 0);
    const err_branch = try branchAt(&test_env, match, 1);

    try expectBranchPatternTag(&test_env, ok_branch, "Ok");
    try expectBranchPatternTag(&test_env, err_branch, "Err");
    try testing.expectEqual(.e_num, std.meta.activeTag(test_env.getCanonicalExpr(err_branch.value)));
}

test "try suffix inside expect emits expect err" {
    const source =
        \\module [main]
        \\
        \\expect Try.Err("x")? == "ok"
        \\
        \\main = {}
    ;
    try testing.expect(try moduleContainsExpectErr(source));
}
