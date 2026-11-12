//! Regression test for https://github.com/roc-lang/roc/issues/12962.
//!
//! Inside a type's `.{ }` block, a block member shadows a top-level definition
//! of the same name. Which definition an unqualified use of that name refers to
//! must not depend on whether the top-level definition is written before or
//! after the type, and the shadowing must be reported in both orders.

const std = @import("std");
const parse = @import("parse");
const CIR = @import("../CIR.zig");
const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const BuiltinTestContext = @import("BuiltinTestContext.zig").BuiltinTestContext;
const CoreCtx = @import("ctx").CoreCtx;

const top_level_after_type =
    \\T := [].{
    \\    double : U64 -> U64
    \\    double = |n| double(n)
    \\}
    \\
    \\double : U64 -> U64
    \\double = |n| n * 2
;

const top_level_before_type =
    \\double : U64 -> U64
    \\double = |n| n * 2
    \\
    \\T := [].{
    \\    double : U64 -> U64
    \\    double = |n| double(n)
    \\}
;

const Target = enum { block_member, top_level, other };

const Resolution = struct {
    target: Target,
    shadowing_warnings: usize,
};

fn findDefNamed(env: *const ModuleEnv, name: []const u8) ?CIR.Def.Idx {
    for (env.store.sliceDefs(env.global_value_defs)) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern != .assign) continue;
        if (std.mem.eql(u8, env.getIdent(pattern.assign.ident), name)) return def_idx;
    }
    return null;
}

fn lambdaBody(env: *const ModuleEnv, expr_idx: CIR.Expr.Idx) ?CIR.Expr.Idx {
    return switch (env.store.getExpr(expr_idx)) {
        .e_lambda => |lambda| lambda.body,
        .e_closure => |closure| lambdaBody(env, closure.lambda_idx),
        else => null,
    };
}

/// Canonicalizes `source` and reports which definition the unqualified
/// `double` called inside the body of the block member `T.double` refers to.
fn resolveMemberSelfCall(source: []const u8) !Resolution {
    const allocator = std.testing.allocator;
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

    const member_def_idx = findDefNamed(&env, "Test.T.double") orelse return error.TestUnexpectedResult;
    const top_level_def_idx = findDefNamed(&env, "double") orelse return error.TestUnexpectedResult;
    const member_def = env.store.getDef(member_def_idx);
    const top_level_def = env.store.getDef(top_level_def_idx);

    const body = lambdaBody(&env, member_def.expr) orelse return error.TestUnexpectedResult;
    const call = switch (env.store.getExpr(body)) {
        .e_call => |call| call,
        else => return error.TestUnexpectedResult,
    };
    const target: Target = switch (env.store.getExpr(call.func)) {
        .e_lookup_local => |lookup| if (lookup.pattern_idx == member_def.pattern)
            .block_member
        else if (lookup.pattern_idx == top_level_def.pattern)
            .top_level
        else
            .other,
        else => .other,
    };

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    var shadowing_warnings: usize = 0;
    for (diagnostics) |diag| {
        if (diag != .shadowing_warning) continue;
        if (std.mem.eql(u8, env.getIdent(diag.shadowing_warning.ident), "double")) shadowing_warnings += 1;
    }

    return .{ .target = target, .shadowing_warnings = shadowing_warnings };
}

test "issue 12962: block member shadowing a top-level definition resolves independently of declaration order" {
    const after = try resolveMemberSelfCall(top_level_after_type);
    const before = try resolveMemberSelfCall(top_level_before_type);

    try std.testing.expect(after.target != .other);
    try std.testing.expect(before.target != .other);
    try std.testing.expectEqual(after.target, before.target);
}

test "issue 12962: block member shadowing a top-level definition is reported in either declaration order" {
    const after = try resolveMemberSelfCall(top_level_after_type);
    const before = try resolveMemberSelfCall(top_level_before_type);

    try std.testing.expectEqual(@as(usize, 1), after.shadowing_warnings);
    try std.testing.expectEqual(@as(usize, 1), before.shadowing_warnings);
}

/// Canonicalizes `source` and counts the shadowing warnings reported for `name`.
fn countShadowingWarnings(source: []const u8, name: []const u8) !usize {
    const allocator = std.testing.allocator;
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

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    var shadowing_warnings: usize = 0;
    for (diagnostics) |diag| {
        if (diag != .shadowing_warning) continue;
        if (std.mem.eql(u8, env.getIdent(diag.shadowing_warning.ident), name)) shadowing_warnings += 1;
    }
    return shadowing_warnings;
}

test "issue 12962: a lambda parameter shadowing a later block member is reported" {
    const source =
        \\T := [].{
        \\    f = |double| double
        \\    double = |n| n
        \\}
    ;
    try std.testing.expectEqual(@as(usize, 1), try countShadowingWarnings(source, "double"));
}

test "issue 12962: a lambda parameter shadowing an earlier block member is reported" {
    const source =
        \\T := [].{
        \\    double = |n| n
        \\    f = |double| double
        \\}
    ;
    try std.testing.expectEqual(@as(usize, 1), try countShadowingWarnings(source, "double"));
}

test "issue 12962: a block member's own parameter shadowing the member is reported" {
    const source =
        \\T := [].{
        \\    double = |double| double
        \\}
    ;
    try std.testing.expectEqual(@as(usize, 1), try countShadowingWarnings(source, "double"));
}

test "issue 12962: a local type's member shadowing a parameter of the enclosing function is reported" {
    const source =
        \\f = |double| {
        \\    T := [].{
        \\        double = |n| double(n)
        \\    }
        \\    T.double(double)
        \\}
    ;
    try std.testing.expectEqual(@as(usize, 1), try countShadowingWarnings(source, "double"));
}

test "issue 12962: a nested type's member does not shadow the enclosing type's member" {
    const source =
        \\T := [].{
        \\    U := [].{
        \\        to_num = |_| 1
        \\    }
        \\    to_num = |_| 2
        \\}
    ;
    try std.testing.expectEqual(@as(usize, 0), try countShadowingWarnings(source, "to_num"));
}

/// Canonicalizes `source` and reports which definition the body of the
/// non-lambda block member `T.x`, a bare `x`, refers to.
fn resolveNonLambdaMemberOwnName(source: []const u8) !Target {
    const allocator = std.testing.allocator;
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

    const member_def_idx = findDefNamed(&env, "Test.T.x") orelse return error.TestUnexpectedResult;
    const top_level_def_idx = findDefNamed(&env, "x") orelse return error.TestUnexpectedResult;
    const member_def = env.store.getDef(member_def_idx);
    const top_level_def = env.store.getDef(top_level_def_idx);

    return switch (env.store.getExpr(member_def.expr)) {
        .e_lookup_local => |lookup| if (lookup.pattern_idx == member_def.pattern)
            .block_member
        else if (lookup.pattern_idx == top_level_def.pattern)
            .top_level
        else
            .other,
        else => .other,
    };
}

test "issue 12962: a non-lambda block member's own name in its body means the shadowed top-level definition in either order" {
    const before =
        \\x : U64
        \\x = 5
        \\
        \\T := [].{
        \\    x : U64
        \\    x = x
        \\}
    ;
    const after =
        \\T := [].{
        \\    x : U64
        \\    x = x
        \\}
        \\
        \\x : U64
        \\x = 5
    ;
    try std.testing.expectEqual(Target.top_level, try resolveNonLambdaMemberOwnName(before));
    try std.testing.expectEqual(Target.top_level, try resolveNonLambdaMemberOwnName(after));
    try std.testing.expectEqual(@as(usize, 1), try countShadowingWarnings(before, "x"));
    try std.testing.expectEqual(@as(usize, 1), try countShadowingWarnings(after, "x"));
}
