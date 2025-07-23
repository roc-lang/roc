//! Unit tests to verify `CIR.Statement` are correctly stored in `NodeStore`

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const types = @import("types");
const RocDec = @import("builtins").RocDec;
const Node = @import("../Node.zig");
const NodeStore = @import("../NodeStore.zig");
const AST = @import("../AST.zig");

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

        testing.expectEqualDeep(header, retrieved) catch |err| {
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

        testing.expectEqualDeep(statement, retrieved) catch |err| {
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

        testing.expectEqualDeep(pattern, retrieved) catch |err| {
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
        .mod_ty = .{
            .mod_ident = rand_ident_idx(),
            .ty_ident = rand_ident_idx(),
            .region = rand_region(),
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

        testing.expectEqualDeep(anno, retrieved) catch |err| {
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
    try expressions.append(AST.Expr{
        .record_updater = .{
            .token = rand_token_idx(),
            .region = rand_region(),
        },
    });

    // We don't include .malformed variant
    expected_test_count -= 1;

    for (expressions.items) |expr| {
        const idx = try store.addExpr(expr);
        const retrieved = store.getExpr(idx);

        testing.expectEqualDeep(expr, retrieved) catch |err| {
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
