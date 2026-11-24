//! Tests canonicalization's ownership of top-level name and concrete value-binding selection.

const std = @import("std");
const parse = @import("parse");
const CIR = @import("../CIR.zig");
const Can = @import("../Can.zig");
const HostedCompiler = @import("../HostedCompiler.zig");
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

fn findAssignedDef(env: *const ModuleEnv, span: CIR.Def.Span, name: []const u8) ?CIR.Def.Idx {
    for (env.store.sliceDefs(span)) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern != .assign) continue;
        if (std.mem.eql(u8, env.getIdent(pattern.assign.ident), name)) return def_idx;
    }
    return null;
}

fn spanContainsDef(env: *const ModuleEnv, span: CIR.Def.Span, needle: CIR.Def.Idx) bool {
    for (env.store.sliceDefs(span)) |def_idx| {
        if (def_idx == needle) return true;
    }
    return false;
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

test "platform header targets and hosted rewrite share top-level selection" {
    const source =
        \\platform ""
        \\    requires {}
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main }
        \\    hosted { "roc_helper": helper! }
        \\
        \\main : {}
        \\
        \\main : {}
        \\
        \\helper! : () => {}
        \\
        \\helper! : () => {}
        \\
        \\implemented : {}
        \\
        \\implemented : {}
        \\implemented = {}
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

    const selected_main = findAssignedDef(&env, env.top_level_value_defs, "main") orelse return error.TestUnexpectedResult;
    const selected_helper = findAssignedDef(&env, env.top_level_value_defs, "helper!") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 1), env.provides_entries.len());
    try std.testing.expectEqual(selected_main, env.provides_entries.get(.first).local_def.?);
    try std.testing.expectEqual(@as(u64, 1), env.hosted_entries.len());
    try std.testing.expectEqual(selected_helper, env.hosted_entries.get(.first).target_def.?);

    try HostedCompiler.replaceAnnoOnlyWithHosted(&env);

    try std.testing.expectEqual(@as(usize, 1), countDefs(&env, env.global_value_defs, "main", .e_hosted_lambda));
    try std.testing.expectEqual(@as(usize, 1), countDefs(&env, env.global_value_defs, "main", .e_anno_only));
    try std.testing.expectEqual(@as(usize, 1), countDefs(&env, env.global_value_defs, "helper!", .e_hosted_lambda));
    try std.testing.expectEqual(@as(usize, 1), countDefs(&env, env.global_value_defs, "helper!", .e_anno_only));
    try std.testing.expectEqual(@as(usize, 0), countDefs(&env, env.global_value_defs, "implemented", .e_hosted_lambda));
    try std.testing.expectEqual(@as(usize, 1), countDefs(&env, env.global_value_defs, "implemented", .e_anno_only));
    try std.testing.expectEqual(@as(usize, 2), env.store.sliceDefs(env.hosted_defs).len);
    for (env.store.sliceDefs(env.hosted_defs)) |hosted_def| {
        try std.testing.expect(spanContainsDef(&env, env.value_binding_defs, hosted_def));
        // The hosted expression replaced the annotation's in the owner table.
        const hosted_expr = env.store.getDef(hosted_def).expr;
        try std.testing.expectEqual(hosted_def, env.topLevelDefOwner(ModuleEnv.nodeIdxFrom(hosted_expr)).?);
    }
}

test "a default app's echo! is its one hosted definition" {
    const source =
        \\main! = |_args| {
        \\    echo!("hi")
        \\    Ok({})
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

    try std.testing.expect(env.module_kind == .default_app);
    const echo = findAssignedDef(&env, env.all_defs, "echo!") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(.e_hosted_lambda, std.meta.activeTag(env.store.getExpr(env.store.getDef(echo).expr)));
    try std.testing.expectEqualSlices(CIR.Def.Idx, &.{echo}, env.store.sliceDefs(env.hosted_defs));
}

test "canonicalization records the top-level def owning each def, expression and pattern node" {
    const source =
        \\pair = (1, (2, 3))
        \\(a, (b, c)) = pair
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

    const pair_def = findAssignedDef(&env, env.global_value_defs, "pair") orelse return error.TestUnexpectedResult;
    const pair = env.store.getDef(pair_def);
    try std.testing.expectEqual(pair_def, env.topLevelDefOwner(ModuleEnv.nodeIdxFrom(pair_def)).?);
    try std.testing.expectEqual(pair_def, env.topLevelDefOwner(ModuleEnv.nodeIdxFrom(pair.expr)).?);
    try std.testing.expectEqual(pair_def, env.topLevelDefOwner(ModuleEnv.nodeIdxFrom(pair.pattern)).?);
    // Nodes inside a def's expression belong to no top-level def.
    const pair_elems = env.store.sliceExpr(env.store.getExpr(pair.expr).e_tuple.elems);
    try std.testing.expectEqual(@as(?CIR.Def.Idx, null), env.topLevelDefOwner(ModuleEnv.nodeIdxFrom(pair_elems[1])));

    var destructure_def: ?CIR.Def.Idx = null;
    for (env.store.sliceDefs(env.global_value_defs)) |def_idx| {
        if (env.store.getPattern(env.store.getDef(def_idx).pattern) == .tuple) destructure_def = def_idx;
    }
    const def_idx = destructure_def orelse return error.TestUnexpectedResult;
    const def = env.store.getDef(def_idx);
    try std.testing.expectEqual(def_idx, env.topLevelDefOwner(ModuleEnv.nodeIdxFrom(def_idx)).?);
    try std.testing.expectEqual(def_idx, env.topLevelDefOwner(ModuleEnv.nodeIdxFrom(def.expr)).?);
    const outer = env.store.slicePatterns(env.store.getPattern(def.pattern).tuple.patterns);
    const inner = env.store.slicePatterns(env.store.getPattern(outer[1]).tuple.patterns);
    for ([_]CIR.Pattern.Idx{ def.pattern, outer[0], outer[1], inner[0], inner[1] }) |pattern_idx| {
        try std.testing.expectEqual(def_idx, env.topLevelDefOwner(ModuleEnv.nodeIdxFrom(pattern_idx)).?);
    }

    const owners = env.top_level_def_owners.items();
    for (owners[1..], owners[0 .. owners.len - 1]) |owner, previous| {
        try std.testing.expect(previous.node < owner.node);
    }
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

test "issue 13077 nested derived declarations have distinct owners" {
    // https://github.com/roc-lang/roc/issues/13077
    // Deriving the same methods for distinct nominal types must not warn.
    const source =
        \\Outer := { node : Outer.Inner }.{
        \\    is_eq : _
        \\    to_hash : _
        \\    Inner := [Leaf(Str)].{
        \\        is_eq : _
        \\        to_hash : _
        \\    }
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
    try std.testing.expect(!ast.hasErrors());

    const roc_ctx = CoreCtx.testing(allocator, allocator);
    var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer can.deinit();
    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    for (diagnostics) |diagnostic| {
        std.debug.print("unexpected diagnostic: {s}\n", .{@tagName(diagnostic)});
    }
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);
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
