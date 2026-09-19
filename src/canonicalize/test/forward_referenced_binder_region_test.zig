//! A declaration that adopts the placeholder pattern created for references
//! ahead of it must give that pattern the declaration's own region, so every
//! consumer of the binder's location (go-to-definition, hover docs, docs
//! extraction, diagnostics) sees the declaration rather than a reference site.

const std = @import("std");
const parse = @import("parse");
const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const BuiltinTestContext = @import("BuiltinTestContext.zig").BuiltinTestContext;
const CoreCtx = @import("ctx").CoreCtx;

/// Expect the binder of `qualified_name` to span the first `binder_len` bytes
/// of `declaration`, which must occur exactly once in the source.
fn expectBinderRegion(env: *const ModuleEnv, qualified_name: []const u8, declaration: []const u8, binder_len: usize) !void {
    const source = env.getSourceAll();
    const start = std.mem.find(u8, source, declaration) orelse return error.TestDeclarationNotInSource;
    try std.testing.expectEqual(null, std.mem.findPos(u8, source, start + 1, declaration));
    for (env.store.sliceDefs(env.all_defs)) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern != .assign) continue;
        if (!std.mem.eql(u8, env.getIdentText(pattern.assign.ident), qualified_name)) continue;
        const region = env.store.getPatternRegion(def.pattern);
        try std.testing.expectEqual(start, region.start.offset);
        try std.testing.expectEqual(start + binder_len, region.end.offset);
        return;
    }
    return error.TestExpectedDefNotFound;
}

test "associated items referenced ahead of their declarations bind at the declaration" {
    const source =
        \\early = Test.late
        \\
        \\Test := [].{
        \\    first = second
        \\    second = Help.bar
        \\    late = 1
        \\}
        \\
        \\Help := [].{
        \\    bar = 6
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
    try can.validateForChecking();

    try expectBinderRegion(&env, "Test.second", "second = Help.bar", "second".len);
    try expectBinderRegion(&env, "Test.late", "late = 1", "late".len);
    try expectBinderRegion(&env, "Test.Help.bar", "bar = 6", "bar".len);
}

test "Builtin annotation-only items referenced ahead of their declarations bind at the declaration" {
    const source =
        \\Builtin :: [].{
        \\    Thing :: [ThingTag].{
        \\        by_type = |thing| Thing.size(thing)
        \\        by_name = |thing| length(thing)
        \\        size : Thing -> Thing
        \\        length : Thing -> Thing
        \\    }
        \\}
    ;

    const allocator = std.testing.allocator;
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Builtin");

    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();

    var can = try Can.initBuiltin(CoreCtx.testing(allocator, allocator), &env, ast);
    defer can.deinit();
    try can.canonicalizeFile();
    try can.validateForChecking();

    try expectBinderRegion(&env, "Builtin.Thing.size", "size : Thing -> Thing", "size : Thing -> Thing".len);
    try expectBinderRegion(&env, "Builtin.Thing.length", "length : Thing -> Thing", "length : Thing -> Thing".len);
}

test "block-local associated items referenced ahead of their declarations are declared at the declaration" {
    const source =
        \\main = {
        \\    Local := [].{
        \\        first = second
        \\        second = 1
        \\    }
        \\    Local.first
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

    const declaration = "second = 1";
    const start = std.mem.find(u8, source, declaration) orelse return error.TestDeclarationNotInSource;
    for (env.store.sliceDefs(env.all_defs)) |def_idx| {
        const expr = env.store.getExpr(env.store.getDef(def_idx).expr);
        if (expr != .e_block) continue;
        const block = expr.e_block;
        for (env.store.sliceStatements(block.stmts)) |stmt_idx| {
            const statement = env.store.getStatement(stmt_idx);
            if (statement != .s_decl) continue;
            const decl = statement.s_decl;
            const pattern = env.store.getPattern(decl.pattern);
            if (pattern != .assign) continue;
            if (!std.mem.endsWith(u8, env.getIdentText(pattern.assign.ident), ".second")) continue;
            const pattern_region = env.store.getPatternRegion(decl.pattern);
            try std.testing.expectEqual(start, pattern_region.start.offset);
            try std.testing.expectEqual(start + "second".len, pattern_region.end.offset);
            try std.testing.expectEqual(pattern_region, env.store.getStatementRegion(stmt_idx));
            return;
        }
    }
    return error.TestExpectedStatementNotFound;
}
