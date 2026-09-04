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

test "builtin Num annotations canonicalize lookup and application as distinct CIR forms" {
    const source =
        \\lookup_value : Num
        \\lookup_value = 1
        \\
        \\apply_value : Num(U8)
        \\apply_value = 1
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

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);

    var lookup_def_idx: ?CIR.Def.Idx = null;
    var apply_def_idx: ?CIR.Def.Idx = null;
    for (env.store.sliceDefs(env.top_level_value_defs)) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern != .assign) continue;
        const name = env.getIdent(pattern.assign.ident);
        if (std.mem.eql(u8, name, "lookup_value")) lookup_def_idx = def_idx;
        if (std.mem.eql(u8, name, "apply_value")) apply_def_idx = def_idx;
    }

    const lookup_def = env.store.getDef(lookup_def_idx orelse return error.TestUnexpectedResult);
    const apply_def = env.store.getDef(apply_def_idx orelse return error.TestUnexpectedResult);
    const lookup_idx = env.store.getAnnotation(lookup_def.annotation orelse return error.TestUnexpectedResult).anno;
    const apply_idx = env.store.getAnnotation(apply_def.annotation orelse return error.TestUnexpectedResult).anno;

    try std.testing.expectEqual(CIR.Node.Tag.ty_lookup, env.store.nodes.get(@enumFromInt(@intFromEnum(lookup_idx))).tag);
    const lookup = env.store.getTypeAnno(lookup_idx);
    try std.testing.expect(lookup == .lookup);
    try std.testing.expect(lookup.lookup.base == .builtin);
    try std.testing.expectEqual(CIR.TypeAnno.Builtin.num, lookup.lookup.base.builtin);
    try std.testing.expectEqualStrings("Num", env.getIdent(lookup.lookup.name));

    try std.testing.expectEqual(CIR.Node.Tag.ty_apply, env.store.nodes.get(@enumFromInt(@intFromEnum(apply_idx))).tag);
    const apply = env.store.getTypeAnno(apply_idx);
    try std.testing.expect(apply == .apply);
    try std.testing.expect(apply.apply.base == .builtin);
    try std.testing.expectEqual(CIR.TypeAnno.Builtin.num, apply.apply.base.builtin);
    try std.testing.expectEqualStrings("Num", env.getIdent(apply.apply.name));
    const args = env.store.sliceTypeAnnos(apply.apply.args);
    try std.testing.expectEqual(@as(usize, 1), args.len);
    const arg = env.store.getTypeAnno(args[0]);
    try std.testing.expect(arg == .lookup);
    try std.testing.expect(arg.lookup.base == .builtin);
    try std.testing.expectEqual(CIR.TypeAnno.Builtin.u8, arg.lookup.base.builtin);
    try std.testing.expectEqualStrings("U8", env.getIdent(arg.lookup.name));
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
