//! Tests canonicalization's ownership of top-level name and concrete value-binding selection.

const std = @import("std");
const parse = @import("parse");
const CIR = @import("../CIR.zig");
const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const BuiltinTestContext = @import("BuiltinTestContext.zig").BuiltinTestContext;
const CoreCtx = @import("ctx").CoreCtx;

fn countDefs(
    env: *const ModuleEnv,
    span: CIR.Def.Span,
    name: []const u8,
    expr_tag: std.meta.Tag(CIR.Expr),
) usize {
    var count: usize = 0;
    for (env.store.sliceDefs(span)) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern != .assign) continue;
        if (!std.mem.eql(u8, env.getIdent(pattern.assign.ident), name)) continue;
        if (std.meta.activeTag(env.store.getExpr(def.expr)) != expr_tag) continue;
        count += 1;
    }
    return count;
}

fn countDefsWithExprTag(env: *const ModuleEnv, span: CIR.Def.Span, expr_tag: std.meta.Tag(CIR.Expr)) usize {
    var count: usize = 0;
    for (env.store.sliceDefs(span)) |def_idx| {
        const def = env.store.getDef(def_idx);
        if (std.meta.activeTag(env.store.getExpr(def.expr)) == expr_tag) count += 1;
    }
    return count;
}

test "canonicalization owns top-level name and value-binding selection" {
    const source =
        \\m = || {}
        \\m = {}
        \\a : {}
        \\other = {}
        \\a = {}
        \\orphan : {}
    ;

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

    try std.testing.expectEqual(@as(usize, 1), countDefs(&env, env.top_level_value_defs, "m", .e_lambda));
    try std.testing.expectEqual(@as(usize, 0), countDefs(&env, env.top_level_value_defs, "m", .e_empty_record));
    try std.testing.expectEqual(@as(usize, 1), countDefs(&env, env.top_level_value_defs, "a", .e_empty_record));
    try std.testing.expectEqual(@as(usize, 0), countDefs(&env, env.top_level_value_defs, "a", .e_anno_only));
    try std.testing.expectEqual(@as(usize, 1), countDefs(&env, env.top_level_value_defs, "orphan", .e_anno_only));

    try std.testing.expectEqual(@as(usize, 1), countDefs(&env, env.value_binding_defs, "m", .e_lambda));
    try std.testing.expectEqual(@as(usize, 1), countDefs(&env, env.value_binding_defs, "m", .e_empty_record));
    try std.testing.expectEqual(@as(usize, 1), countDefs(&env, env.value_binding_defs, "a", .e_empty_record));
    try std.testing.expectEqual(@as(usize, 0), countDefs(&env, env.value_binding_defs, "a", .e_anno_only));
    try std.testing.expectEqual(@as(usize, 1), countDefs(&env, env.value_binding_defs, "orphan", .e_anno_only));
}

test "generated associated method markers are not value bindings" {
    const source =
        \\Foo := { foo : Str }.{
        \\    parser_for : _
        \\}
    ;

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

    try std.testing.expectEqual(@as(usize, 1), countDefsWithExprTag(&env, env.global_value_defs, .e_derived_method));
    try std.testing.expectEqual(@as(usize, 0), countDefsWithExprTag(&env, env.value_binding_defs, .e_derived_method));
}

test "unfiltered value-definition spans reuse global definition storage" {
    const source =
        \\one = || {}
        \\two = {}
        \\three : {}
    ;

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

    try std.testing.expectEqual(env.global_value_defs.span, env.top_level_value_defs.span);
    try std.testing.expectEqual(env.global_value_defs.span, env.value_binding_defs.span);
}
