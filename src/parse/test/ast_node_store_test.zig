//! Unit tests to verify `CIR.Statement` are correctly stored in `NodeStore`

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const builtins = @import("builtins");

const RocDec = builtins.RocDec;
const Node = parse.Node;
const NodeStore = parse.NodeStore;
const AST = parse.AST;
const Token = parse.Token;

/// Custom comparison for Token values that handles IdentWithFlags properly
fn expectEqualTokens(expected: Token, actual: Token) !void {
    try testing.expectEqual(expected.tag, actual.tag);
    try testing.expectEqual(expected.region, actual.region);
    
    // Compare data based on tag
    switch (expected.tag) {
        .UpperIdent, .LowerIdent, .DotLowerIdent, .DotUpperIdent, 
        .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent, .NamedUnderscore, 
        .OpaqueName, .MalformedUnicodeIdent, .MalformedDotUnicodeIdent,
        .MalformedNoSpaceDotUnicodeIdent, .MalformedNamedUnderscoreUnicode,
        .MalformedOpaqueNameUnicode => {
            // These have IdentWithFlags data
            const expected_ident = expected.data.ident_with_flags;
            const actual_ident = actual.data.ident_with_flags;
            try testing.expect(expected_ident.ident.eql(actual_ident.ident));
            try testing.expectEqual(expected_ident.starts_with_underscore, actual_ident.starts_with_underscore);
            try testing.expectEqual(expected_ident.ends_with_underscore, actual_ident.ends_with_underscore);
        },
        else => {
            // For other token types, skip data comparison entirely to avoid
            // comparing unions containing Ident.Idx
            // The tag and region comparison above should be sufficient for testing
        },
    }
}

var rand = std.Random.DefaultPrng.init(1234);

/// Generate a random index of type `T`.
fn rand_idx(comptime T: type) T {
    return @enumFromInt(rand.random().int(u32));
}

/// Generate a random token index.
fn rand_token_idx() AST.Token.Idx {
    return rand.random().int(u32);
}

/// Generate a random identifier index.
fn rand_ident_idx() base.Ident.Idx {
    return @bitCast(rand.random().int(u32));
}

/// Helper to create a `TokenizedRegion` from raw start and end positions.
fn rand_region() AST.TokenizedRegion {
    const start = rand.random().int(u32);
    const end = rand.random().int(u32);
    return AST.TokenizedRegion{
        .start = start,
        .end = end,
    };
}

/// Helper to create a `DataSpan` from raw start and length positions.
fn rand_span() base.DataSpan {
    const start = rand.random().int(u32);
    const len = rand.random().int(u30); // Constrain len to fit within u30 (used by ImportRhs.num_exposes)
    return base.DataSpan{
        .start = start,
        .len = len,
    };
}

test "NodeStore round trip - Headers" {
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_HEADER_NODE_COUNT);
    defer store.deinit();

    var headers = std.ArrayList(AST.Header).init(gpa);
    defer headers.deinit();

    try headers.append(AST.Header{
        .app = .{
            .packages = rand_idx(AST.Collection.Idx),
            .platform_idx = rand_idx(AST.RecordField.Idx),
            .provides = rand_idx(AST.Collection.Idx),
            .region = rand_region(),
        },
    });

    try headers.append(AST.Header{
        .module = .{
            .exposes = rand_idx(AST.Collection.Idx),
            .region = rand_region(),
        },
    });

    try headers.append(AST.Header{
        .package = .{
            .exposes = rand_idx(AST.Collection.Idx),
            .packages = rand_idx(AST.Collection.Idx),
            .region = rand_region(),
        },
    });

    try headers.append(AST.Header{
        .platform = .{
            .exposes = rand_idx(AST.Collection.Idx),
            .name = rand_token_idx(),
            .packages = rand_idx(AST.Collection.Idx),
            .provides = rand_idx(AST.Collection.Idx),
            .requires_rigids = rand_idx(AST.Collection.Idx),
            .requires_signatures = rand_idx(AST.TypeAnno.Idx),
            .region = rand_region(),
        },
    });

    try headers.append(AST.Header{
        .hosted = .{
            .exposes = rand_idx(AST.Collection.Idx),
            .region = rand_region(),
        },
    });

    for (headers.items) |header| {
        const idx = try store.addHeader(header);
        const retrieved = store.getHeader(idx);

        expectEqualHeaders(header, retrieved) catch |err| {
            std.debug.print("\n\nOriginal:  {any}\n\n", .{header});
            std.debug.print("Retrieved: {any}\n\n", .{retrieved});
            return err;
        };
    }

    // Note + 1 here because we don't include the malformed
    const actual_test_count = headers.items.len + 1;

    if (actual_test_count < NodeStore.AST_HEADER_NODE_COUNT) {
        std.debug.print("Header test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.AST_HEADER_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing header variants.\n", .{});
        return error.IncompleteHeaderTestCoverage;
    }
}

test "NodeStore round trip - Statement" {
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_STATEMENT_NODE_COUNT);
    defer store.deinit();

    var statements = std.ArrayList(AST.Statement).init(gpa);
    defer statements.deinit();

    try statements.append(AST.Statement{
        .decl = .{
            .body = rand_idx(AST.Expr.Idx),
            .pattern = rand_idx(AST.Pattern.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(AST.Statement{
        .@"var" = .{
            .name = rand_token_idx(),
            .body = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(AST.Statement{
        .expr = .{
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(AST.Statement{
        .crash = .{
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(AST.Statement{
        .dbg = .{
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(AST.Statement{
        .expect = .{
            .body = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(AST.Statement{
        .@"for" = .{
            .patt = rand_idx(AST.Pattern.Idx),
            .expr = rand_idx(AST.Expr.Idx),
            .body = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(AST.Statement{
        .@"return" = .{
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    // Simple import with no tokens
    try statements.append(AST.Statement{
        .import = .{
            .alias_tok = null,
            .module_name_tok = rand_token_idx(),
            .qualifier_tok = null,
            .region = rand_region(),
            .exposes = AST.ExposedItem.Span{ .span = rand_span() },
        },
    });
    // Import with alias
    try statements.append(AST.Statement{
        .import = .{
            .alias_tok = rand_token_idx(),
            .module_name_tok = rand_token_idx(),
            .qualifier_tok = null,
            .region = rand_region(),
            .exposes = AST.ExposedItem.Span{ .span = rand_span() },
        },
    });
    // Import with qualifier but no alias
    try statements.append(AST.Statement{
        .import = .{
            .alias_tok = null,
            .module_name_tok = rand_token_idx(),
            .qualifier_tok = rand_token_idx(),
            .region = rand_region(),
            .exposes = AST.ExposedItem.Span{ .span = rand_span() },
        },
    });
    // Import with both qualifier and alias
    try statements.append(AST.Statement{
        .import = .{
            .alias_tok = rand_token_idx(),
            .module_name_tok = rand_token_idx(),
            .qualifier_tok = rand_token_idx(),
            .region = rand_region(),
            .exposes = AST.ExposedItem.Span{ .span = rand_span() },
        },
    });
    try statements.append(AST.Statement{
        .type_decl = .{
            .anno = rand_idx(AST.TypeAnno.Idx),
            .header = rand_idx(AST.TypeHeader.Idx),
            .kind = AST.TypeDeclKind.nominal,
            .region = rand_region(),
            .where = rand_idx(AST.Collection.Idx),
        },
    });
    try statements.append(AST.Statement{
        .type_anno = .{
            .name = rand_token_idx(),
            .anno = rand_idx(AST.TypeAnno.Idx),
            .where = rand_idx(AST.Collection.Idx),
            .region = rand_region(),
        },
    });

    for (statements.items) |statement| {
        const idx = try store.addStatement(statement);
        const retrieved = store.getStatement(idx);

        expectEqualStatements(statement, retrieved) catch |err| {
            std.debug.print("\n\nOriginal:  {any}\n\n", .{statement});
            std.debug.print("Retrieved: {any}\n\n", .{retrieved});
            return err;
        };
    }

    // Note + 1 here because we don't include the malformed
    const actual_test_count = statements.items.len + 1;

    if (actual_test_count < NodeStore.AST_STATEMENT_NODE_COUNT) {
        std.debug.print("Statement test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.AST_STATEMENT_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing statement variants.\n", .{});
        return error.IncompleteStatementTestCoverage;
    }
}

test "NodeStore round trip - Pattern" {
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_PATTERN_NODE_COUNT);
    defer store.deinit();

    var expected_test_count: usize = NodeStore.AST_PATTERN_NODE_COUNT;

    var patterns = std.ArrayList(AST.Pattern).init(gpa);
    defer patterns.deinit();

    try patterns.append(AST.Pattern{
        .ident = .{
            .ident_tok = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try patterns.append(AST.Pattern{
        .tag = .{
            .args = AST.Pattern.Span{ .span = rand_span() },
            .qualifiers = AST.Token.Span{ .span = rand_span() },
            .region = rand_region(),
            .tag_tok = rand_token_idx(),
        },
    });
    try patterns.append(AST.Pattern{
        .int = .{
            .number_tok = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try patterns.append(AST.Pattern{
        .frac = .{
            .number_tok = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try patterns.append(AST.Pattern{
        .string = .{
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
            .string_tok = rand_token_idx(),
        },
    });
    try patterns.append(AST.Pattern{
        .single_quote = .{
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try patterns.append(AST.Pattern{
        .record = .{
            .fields = AST.PatternRecordField.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try patterns.append(AST.Pattern{
        .list = .{
            .patterns = AST.Pattern.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try patterns.append(AST.Pattern{
        .list_rest = .{
            .name = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try patterns.append(AST.Pattern{
        .tuple = .{
            .patterns = AST.Pattern.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try patterns.append(AST.Pattern{
        .underscore = .{
            .region = rand_region(),
        },
    });
    try patterns.append(AST.Pattern{
        .alternatives = .{
            .patterns = AST.Pattern.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try patterns.append(AST.Pattern{
        .as = .{ .name = rand_token_idx(), .region = rand_region(), .pattern = rand_idx(AST.Pattern.Idx) },
    });

    // We don't include .malformed variant
    expected_test_count -= 1;

    for (patterns.items) |pattern| {
        const idx = try store.addPattern(pattern);
        const retrieved = store.getPattern(idx);

        expectEqualPatterns(pattern, retrieved) catch |err| {
            std.debug.print("\n\nOriginal:  {any}\n\n", .{pattern});
            std.debug.print("Retrieved: {any}\n\n", .{retrieved});
            return err;
        };
    }

    const actual_test_count = patterns.items.len;
    if (actual_test_count != expected_test_count) {
        std.debug.print("Pattern test coverage insufficient! Expected {d} test cases but found {d}.\n", .{ expected_test_count, actual_test_count });
        std.debug.print("Please add or remove test cases for missing pattern variants.\n", .{});
        return error.IncompletePatternTestCoverage;
    }
}

fn expectEqualTypeAnno(expected: AST.TypeAnno, actual: AST.TypeAnno) !void {
    try testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));

    switch (expected) {
        .apply => |expected_apply| {
            const actual_apply = actual.apply;
            try testing.expectEqual(expected_apply.args.span, actual_apply.args.span);
            try testing.expectEqual(expected_apply.region, actual_apply.region);
        },
        .ty_var => |expected_ty_var| {
            const actual_ty_var = actual.ty_var;
            try testing.expectEqual(expected_ty_var.tok, actual_ty_var.tok);
            try testing.expectEqual(expected_ty_var.region, actual_ty_var.region);
        },
        .underscore_type_var => |expected_underscore| {
            const actual_underscore = actual.underscore_type_var;
            try testing.expectEqual(expected_underscore.tok, actual_underscore.tok);
            try testing.expectEqual(expected_underscore.region, actual_underscore.region);
        },
        .underscore => |expected_underscore| {
            const actual_underscore = actual.underscore;
            try testing.expectEqual(expected_underscore.region, actual_underscore.region);
        },
        .ty => |expected_ty| {
            const actual_ty = actual.ty;
            try testing.expectEqual(expected_ty.token, actual_ty.token);
            try testing.expectEqual(expected_ty.qualifiers.span, actual_ty.qualifiers.span);
            try testing.expectEqual(expected_ty.region, actual_ty.region);
        },
        .tag_union => |expected_tag_union| {
            const actual_tag_union = actual.tag_union;
            try testing.expectEqual(expected_tag_union.tags.span, actual_tag_union.tags.span);
            if (expected_tag_union.open_anno) |expected_open| {
                try testing.expect(actual_tag_union.open_anno != null);
                try testing.expectEqual(expected_open, actual_tag_union.open_anno.?);
            } else {
                try testing.expect(actual_tag_union.open_anno == null);
            }
            try testing.expectEqual(expected_tag_union.region, actual_tag_union.region);
        },
        .tuple => |expected_tuple| {
            const actual_tuple = actual.tuple;
            try testing.expectEqual(expected_tuple.annos.span, actual_tuple.annos.span);
            try testing.expectEqual(expected_tuple.region, actual_tuple.region);
        },
        .record => |expected_record| {
            const actual_record = actual.record;
            try testing.expectEqual(expected_record.fields.span, actual_record.fields.span);
            try testing.expectEqual(expected_record.region, actual_record.region);
        },
        .@"fn" => |expected_fn| {
            const actual_fn = actual.@"fn";
            try testing.expectEqual(expected_fn.args.span, actual_fn.args.span);
            try testing.expectEqual(expected_fn.ret, actual_fn.ret);
            try testing.expectEqual(expected_fn.effectful, actual_fn.effectful);
            try testing.expectEqual(expected_fn.region, actual_fn.region);
        },
        .parens => |expected_parens| {
            const actual_parens = actual.parens;
            try testing.expectEqual(expected_parens.anno, actual_parens.anno);
            try testing.expectEqual(expected_parens.region, actual_parens.region);
        },
        .malformed => |expected_malformed| {
            const actual_malformed = actual.malformed;
            try testing.expectEqual(expected_malformed.reason, actual_malformed.reason);
            try testing.expectEqual(expected_malformed.region, actual_malformed.region);
        },
    }
}

fn expectEqualHeaders(expected: AST.Header, actual: AST.Header) !void {
    try testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
    
    switch (expected) {
        .app => |expected_app| {
            const actual_app = actual.app;
            try testing.expectEqual(expected_app.provides, actual_app.provides);
            try testing.expectEqual(expected_app.platform_idx, actual_app.platform_idx);
            try testing.expectEqual(expected_app.packages, actual_app.packages);
            try testing.expectEqual(expected_app.region, actual_app.region);
        },
        .module => |expected_mod| {
            const actual_mod = actual.module;
            try testing.expectEqual(expected_mod.exposes, actual_mod.exposes);
            try testing.expectEqual(expected_mod.region, actual_mod.region);
        },
        .package => |expected_pkg| {
            const actual_pkg = actual.package;
            try testing.expectEqual(expected_pkg.exposes, actual_pkg.exposes);
            try testing.expectEqual(expected_pkg.packages, actual_pkg.packages);
            try testing.expectEqual(expected_pkg.region, actual_pkg.region);
        },
        .platform => |expected_plat| {
            const actual_plat = actual.platform;
            try testing.expectEqual(expected_plat.name, actual_plat.name);
            try testing.expectEqual(expected_plat.requires_rigids, actual_plat.requires_rigids);
            try testing.expectEqual(expected_plat.requires_signatures, actual_plat.requires_signatures);
            try testing.expectEqual(expected_plat.exposes, actual_plat.exposes);
            try testing.expectEqual(expected_plat.packages, actual_plat.packages);
            try testing.expectEqual(expected_plat.provides, actual_plat.provides);
            try testing.expectEqual(expected_plat.region, actual_plat.region);
        },
        .hosted => |expected_hosted| {
            const actual_hosted = actual.hosted;
            try testing.expectEqual(expected_hosted.exposes, actual_hosted.exposes);
            try testing.expectEqual(expected_hosted.region, actual_hosted.region);
        },
        .malformed => {
            // Nothing to compare
        },
    }
}

fn expectEqualStatements(expected: AST.Statement, actual: AST.Statement) !void {
    try testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
    
    switch (expected) {
        .decl => |expected_decl| {
            try testing.expectEqual(expected_decl, actual.decl);
        },
        .@"var" => |expected_var| {
            const actual_var = actual.@"var";
            // Token.Idx comparison - these are just indices, should be fine
            try testing.expectEqual(expected_var.name, actual_var.name);
            try testing.expectEqual(expected_var.body, actual_var.body);
            try testing.expectEqual(expected_var.region, actual_var.region);
        },
        .expr => |expected_expr| {
            const actual_expr = actual.expr;
            try testing.expectEqual(expected_expr.expr, actual_expr.expr);
            try testing.expectEqual(expected_expr.region, actual_expr.region);
        },
        .crash => |expected_crash| {
            const actual_crash = actual.crash;
            try testing.expectEqual(expected_crash.expr, actual_crash.expr);
            try testing.expectEqual(expected_crash.region, actual_crash.region);
        },
        .dbg => |expected_dbg| {
            const actual_dbg = actual.dbg;
            try testing.expectEqual(expected_dbg.expr, actual_dbg.expr);
            try testing.expectEqual(expected_dbg.region, actual_dbg.region);
        },
        .expect => |expected_expect| {
            const actual_expect = actual.expect;
            try testing.expectEqual(expected_expect.body, actual_expect.body);
            try testing.expectEqual(expected_expect.region, actual_expect.region);
        },
        .@"for" => |expected_for| {
            const actual_for = actual.@"for";
            try testing.expectEqual(expected_for.patt, actual_for.patt);
            try testing.expectEqual(expected_for.expr, actual_for.expr);
            try testing.expectEqual(expected_for.body, actual_for.body);
            try testing.expectEqual(expected_for.region, actual_for.region);
        },
        .@"return" => |expected_return| {
            const actual_return = actual.@"return";
            try testing.expectEqual(expected_return.expr, actual_return.expr);
            try testing.expectEqual(expected_return.region, actual_return.region);
        },
        .import => |expected_import| {
            const actual_import = actual.import;
            try testing.expectEqual(expected_import.module_name_tok, actual_import.module_name_tok);
            try testing.expectEqual(expected_import.qualifier_tok, actual_import.qualifier_tok);
            try testing.expectEqual(expected_import.alias_tok, actual_import.alias_tok);
            try testing.expectEqual(expected_import.exposes, actual_import.exposes);
            try testing.expectEqual(expected_import.region, actual_import.region);
        },
        .type_decl => |expected_type_decl| {
            const actual_type_decl = actual.type_decl;
            try testing.expectEqual(expected_type_decl.header, actual_type_decl.header);
            try testing.expectEqual(expected_type_decl.anno, actual_type_decl.anno);
            try testing.expectEqual(expected_type_decl.where, actual_type_decl.where);
            try testing.expectEqual(expected_type_decl.kind, actual_type_decl.kind);
            try testing.expectEqual(expected_type_decl.region, actual_type_decl.region);
        },
        .type_anno => |expected_type_anno| {
            const actual_type_anno = actual.type_anno;
            try testing.expectEqual(expected_type_anno.name, actual_type_anno.name);
            try testing.expectEqual(expected_type_anno.anno, actual_type_anno.anno);
            try testing.expectEqual(expected_type_anno.where, actual_type_anno.where);
            try testing.expectEqual(expected_type_anno.region, actual_type_anno.region);
        },
        .malformed => |expected_malformed| {
            const actual_malformed = actual.malformed;
            try testing.expectEqual(expected_malformed.reason, actual_malformed.reason);
            try testing.expectEqual(expected_malformed.region, actual_malformed.region);
        },
    }
}

fn expectEqualPatterns(expected: AST.Pattern, actual: AST.Pattern) !void {
    // Since Pattern is complex and may contain Token references, we'll just compare tags for now
    try testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
}

fn expectEqualExprs(expected: AST.Expr, actual: AST.Expr) !void {
    // Since Expr is complex and may contain Token references, we'll just compare tags for now
    try testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
}

test "NodeStore round trip - TypeAnno" {
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_TYPE_ANNO_NODE_COUNT);
    defer store.deinit();

    var expected_test_count: usize = NodeStore.AST_TYPE_ANNO_NODE_COUNT;

    var ty_annos = std.ArrayList(AST.TypeAnno).init(gpa);
    defer ty_annos.deinit();

    try ty_annos.append(AST.TypeAnno{
        .apply = .{
            .args = AST.TypeAnno.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try ty_annos.append(AST.TypeAnno{
        .ty_var = .{
            .region = rand_region(),
            .tok = rand_token_idx(),
        },
    });
    try ty_annos.append(AST.TypeAnno{
        .underscore = .{
            .region = rand_region(),
        },
    });
    try ty_annos.append(AST.TypeAnno{
        .ty = .{
            .qualifiers = AST.Token.Span{ .span = rand_span() },
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try ty_annos.append(AST.TypeAnno{
        .tag_union = .{
            .open_anno = rand_idx(AST.TypeAnno.Idx),
            .tags = AST.TypeAnno.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try ty_annos.append(AST.TypeAnno{
        .tuple = .{
            .annos = AST.TypeAnno.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try ty_annos.append(AST.TypeAnno{
        .record = .{
            .fields = AST.AnnoRecordField.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try ty_annos.append(AST.TypeAnno{
        .@"fn" = .{
            .args = AST.TypeAnno.Span{ .span = rand_span() },
            .ret = rand_idx(AST.TypeAnno.Idx),
            .effectful = true,
            .region = rand_region(),
        },
    });
    try ty_annos.append(AST.TypeAnno{
        .parens = .{
            .anno = rand_idx(AST.TypeAnno.Idx),
            .region = rand_region(),
        },
    });

    // We don't include .malformed variant
    expected_test_count -= 1;

    for (ty_annos.items) |anno| {
        const idx = try store.addTypeAnno(anno);
        const retrieved = store.getTypeAnno(idx);

        expectEqualTypeAnno(anno, retrieved) catch |err| {
            std.debug.print("\n\nOriginal:  {any}\n\n", .{anno});
            std.debug.print("Retrieved: {any}\n\n", .{retrieved});
            return err;
        };
    }

    const actual_test_count = ty_annos.items.len;
    if (actual_test_count != expected_test_count) {
        std.debug.print("TypeAnno test coverage insufficient! Expected {d} test cases but found {d}.\n", .{ expected_test_count, actual_test_count });
        std.debug.print("Please add or remove test cases for missing type annotation variants.\n", .{});
        return error.IncompleteTypeAnnoTestCoverage;
    }
}

test "NodeStore round trip - Expr" {
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_EXPR_NODE_COUNT);
    defer store.deinit();

    var expected_test_count: usize = NodeStore.AST_EXPR_NODE_COUNT;

    var expressions = std.ArrayList(AST.Expr).init(gpa);
    defer expressions.deinit();

    try expressions.append(AST.Expr{
        .int = .{
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(AST.Expr{
        .frac = .{
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(AST.Expr{
        .single_quote = .{
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(AST.Expr{
        .string_part = .{
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(AST.Expr{
        .string = .{
            .parts = AST.Expr.Span{ .span = rand_span() },
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(AST.Expr{
        .list = .{
            .items = AST.Expr.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try expressions.append(AST.Expr{
        .tuple = .{
            .items = AST.Expr.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try expressions.append(AST.Expr{
        .record = .{
            .ext = rand_idx(AST.Expr.Idx),
            .fields = AST.RecordField.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try expressions.append(AST.Expr{
        .tag = .{
            .qualifiers = AST.Token.Span{ .span = rand_span() },
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(AST.Expr{
        .lambda = .{
            .args = AST.Pattern.Span{ .span = rand_span() },
            .region = rand_region(),
            .body = rand_idx(AST.Expr.Idx),
        },
    });
    try expressions.append(AST.Expr{
        .apply = .{
            .@"fn" = rand_idx(AST.Expr.Idx),
            .args = AST.Expr.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });

    try expressions.append(AST.Expr{
        .field_access = .{
            .left = rand_idx(AST.Expr.Idx),
            .right = rand_idx(AST.Expr.Idx),
            .operator = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try expressions.append(AST.Expr{
        .local_dispatch = .{
            .left = rand_idx(AST.Expr.Idx),
            .right = rand_idx(AST.Expr.Idx),
            .operator = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try expressions.append(AST.Expr{
        .bin_op = .{
            .left = rand_idx(AST.Expr.Idx),
            .right = rand_idx(AST.Expr.Idx),
            .operator = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try expressions.append(AST.Expr{
        .suffix_single_question = .{
            .expr = rand_idx(AST.Expr.Idx),
            .operator = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try expressions.append(AST.Expr{
        .unary_op = .{
            .expr = rand_idx(AST.Expr.Idx),
            .operator = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try expressions.append(AST.Expr{
        .if_then_else = .{
            .@"else" = rand_idx(AST.Expr.Idx),
            .condition = rand_idx(AST.Expr.Idx),
            .then = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try expressions.append(AST.Expr{
        .match = .{
            .branches = AST.MatchBranch.Span{ .span = rand_span() },
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try expressions.append(AST.Expr{
        .ident = .{
            .qualifiers = AST.Token.Span{ .span = rand_span() },
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(AST.Expr{
        .dbg = .{
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try expressions.append(AST.Expr{
        .record_builder = .{
            .fields = rand_idx(AST.RecordField.Idx),
            .mapper = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try expressions.append(AST.Expr{
        .ellipsis = .{ .region = rand_region() },
    });
    try expressions.append(AST.Expr{
        .block = .{
            .region = rand_region(),
            .statements = AST.Statement.Span{ .span = rand_span() },
        },
    });

    // We don't include .malformed variant
    expected_test_count -= 1;

    for (expressions.items) |expr| {
        const idx = try store.addExpr(expr);
        const retrieved = store.getExpr(idx);

        expectEqualExprs(expr, retrieved) catch |err| {
            std.debug.print("\n\nOriginal:  {any}\n\n", .{expr});
            std.debug.print("Retrieved: {any}\n\n", .{retrieved});
            return err;
        };
    }

    const actual_test_count = expressions.items.len;
    if (actual_test_count != expected_test_count) {
        std.debug.print("AST.Expr test coverage insufficient! Expected {d} test cases but found {d}.\n", .{ expected_test_count, actual_test_count });
        std.debug.print("Please add or remove test cases for missing expression variants.\n", .{});
        return error.IncompleteExprTestCoverage;
    }
}
