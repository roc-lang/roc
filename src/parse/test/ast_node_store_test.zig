//! Unit tests to verify `CIR.Statement` are correctly stored in `NodeStore`

const std = @import("std");
const testing = std.testing;
const base = @import("base");

const NodeStore = @import("../NodeStore.zig");
const AST = @import("../AST.zig");
const NumericLiteral = @import("../NumericLiteral.zig");

/// Generate a random index of type `T`.
fn rand_idx(random: std.Random, comptime T: type) T {
    if (T == base.Ident.Idx) {
        return .{
            .attributes = .{
                .effectful = random.boolean(),
                .ignored = random.boolean(),
                .reassignable = random.boolean(),
            },
            .idx = random.int(u29),
        };
    }

    return switch (@typeInfo(T)) {
        .@"enum" => @enumFromInt(random.int(u32)),
        else => @compileError("rand_idx needs an explicit constructor for this index type"),
    };
}

/// Generate a random token index.
fn rand_token_idx(random: std.Random) AST.Token.Idx {
    return random.int(u32);
}

/// Helper to create a `TokenizedRegion` from raw start and end positions.
fn rand_region(random: std.Random) AST.TokenizedRegion {
    const start = random.int(u32);
    const end = random.int(u32);
    return AST.TokenizedRegion{
        .start = start,
        .end = end,
    };
}

/// Helper to create a `DataSpan` from raw start and length positions.
fn rand_span(random: std.Random) base.DataSpan {
    const start = random.int(u32);
    const len = random.int(u30); // Constrain len to fit within u30 (used by ImportRhs.num_exposes)
    return base.DataSpan{
        .start = start,
        .len = len,
    };
}

test "NodeStore round trip - Headers" {
    var prng = std.Random.DefaultPrng.init(0x48454144455253);
    const random = prng.random();
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_HEADER_NODE_COUNT);
    defer store.deinit();

    var headers = std.ArrayList(AST.Header).empty;
    defer headers.deinit(gpa);

    try headers.append(gpa, AST.Header{
        .app = .{
            .packages = rand_idx(random, AST.Collection.Idx),
            .platform_idx = rand_idx(random, AST.RecordField.Idx),
            .provides = rand_idx(random, AST.Collection.Idx),
            .region = rand_region(random),
        },
    });

    try headers.append(gpa, AST.Header{
        .module = .{
            .exposes = rand_idx(random, AST.Collection.Idx),
            .region = rand_region(random),
        },
    });

    try headers.append(gpa, AST.Header{
        .package = .{
            .exposes = rand_idx(random, AST.Collection.Idx),
            .packages = rand_idx(random, AST.Collection.Idx),
            .region = rand_region(random),
        },
    });

    try headers.append(gpa, AST.Header{
        .platform = .{
            .exposes = rand_idx(random, AST.Collection.Idx),
            .name = rand_token_idx(random),
            .packages = rand_idx(random, AST.Collection.Idx),
            .provides = .{ .span = rand_span(random) },
            .hosted = .{ .span = rand_span(random) },
            .requires_entries = .{ .span = .{ .start = 0, .len = 0 } },
            .targets = null,
            .region = rand_region(random),
        },
    });

    try headers.append(gpa, AST.Header{
        .hosted = .{
            .exposes = rand_idx(random, AST.Collection.Idx),
            .region = rand_region(random),
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
    var prng = std.Random.DefaultPrng.init(0x53544154454d454e);
    const random = prng.random();
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_STATEMENT_NODE_COUNT);
    defer store.deinit();

    var statements = std.ArrayList(AST.Statement).empty;
    defer statements.deinit(gpa);

    try statements.append(gpa, AST.Statement{
        .decl = .{
            .body = rand_idx(random, AST.Expr.Idx),
            .pattern = rand_idx(random, AST.Pattern.Idx),
            .region = rand_region(random),
        },
    });
    try statements.append(gpa, AST.Statement{
        .@"var" = .{
            .name = rand_token_idx(random),
            .body = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    try statements.append(gpa, AST.Statement{
        .expr = .{
            .expr = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    try statements.append(gpa, AST.Statement{
        .crash = .{
            .expr = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    try statements.append(gpa, AST.Statement{
        .dbg = .{
            .expr = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    try statements.append(gpa, AST.Statement{
        .expect = .{
            .body = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    try statements.append(gpa, AST.Statement{
        .@"for" = .{
            .patt = rand_idx(random, AST.Pattern.Idx),
            .expr = rand_idx(random, AST.Expr.Idx),
            .body = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    try statements.append(gpa, AST.Statement{
        .@"return" = .{
            .expr = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    // Simple import with no tokens
    try statements.append(gpa, AST.Statement{
        .import = .{
            .alias_tok = null,
            .module_name_tok = rand_token_idx(random),
            .qualifier_tok = null,
            .region = rand_region(random),
            .exposes = AST.ExposedItem.Span{ .span = rand_span(random) },
            .nested_import = false,
        },
    });
    // Import with alias
    try statements.append(gpa, AST.Statement{
        .import = .{
            .alias_tok = rand_token_idx(random),
            .module_name_tok = rand_token_idx(random),
            .qualifier_tok = null,
            .region = rand_region(random),
            .exposes = AST.ExposedItem.Span{ .span = rand_span(random) },
            .nested_import = false,
        },
    });
    // Import with qualifier but no alias
    try statements.append(gpa, AST.Statement{
        .import = .{
            .alias_tok = null,
            .module_name_tok = rand_token_idx(random),
            .qualifier_tok = rand_token_idx(random),
            .region = rand_region(random),
            .exposes = AST.ExposedItem.Span{ .span = rand_span(random) },
            .nested_import = false,
        },
    });
    // Import with both qualifier and alias
    try statements.append(gpa, AST.Statement{
        .import = .{
            .alias_tok = rand_token_idx(random),
            .module_name_tok = rand_token_idx(random),
            .qualifier_tok = rand_token_idx(random),
            .region = rand_region(random),
            .exposes = AST.ExposedItem.Span{ .span = rand_span(random) },
            .nested_import = false,
        },
    });
    try statements.append(gpa, AST.Statement{
        .type_decl = .{
            .anno = rand_idx(random, AST.TypeAnno.Idx),
            .header = rand_idx(random, AST.TypeHeader.Idx),
            .kind = AST.TypeDeclKind.nominal,
            .region = rand_region(random),
            .where = rand_idx(random, AST.Collection.Idx),
            .associated = null,
        },
    });
    try statements.append(gpa, AST.Statement{
        .type_anno = .{
            .name = rand_token_idx(random),
            .anno = rand_idx(random, AST.TypeAnno.Idx),
            .where = rand_idx(random, AST.Collection.Idx),
            .is_var = false,
            .region = rand_region(random),
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

test "NodeStore round trip - Statement type annotation full-width where index" {
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_STATEMENT_NODE_COUNT);
    defer store.deinit();

    const statements = [_]AST.Statement{
        .{ .type_anno = .{
            .name = 1,
            .anno = @enumFromInt(2),
            .where = @enumFromInt(0x80000000),
            .is_var = false,
            .region = .{ .start = 3, .end = 4 },
        } },
        .{ .type_anno = .{
            .name = 5,
            .anno = @enumFromInt(6),
            .where = @enumFromInt(0xffffffff),
            .is_var = true,
            .region = .{ .start = 7, .end = 8 },
        } },
    };

    for (statements) |statement| {
        const idx = try store.addStatement(statement);
        try testing.expectEqualDeep(statement, store.getStatement(idx));
    }
}

test "NodeStore round trip - Statement type declaration optional data starts at zero" {
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_STATEMENT_NODE_COUNT);
    defer store.deinit();

    const zero_collection_idx: u32 = 0; // Collection.Idx 0 is a valid value and must round-trip.
    const statements = [_]AST.Statement{
        .{ .type_decl = .{
            .anno = @enumFromInt(1),
            .header = @enumFromInt(2),
            .kind = .nominal,
            .region = .{ .start = 3, .end = 4 },
            .where = @enumFromInt(zero_collection_idx),
            .associated = null,
        } },
        .{ .type_decl = .{
            .anno = @enumFromInt(5),
            .header = @enumFromInt(6),
            .kind = .nominal,
            .region = .{ .start = 7, .end = 8 },
            .where = null,
            .associated = .{
                .statements = .{ .span = .{ .start = 9, .len = 10 } },
                .scope = @enumFromInt(11),
                .region = .{ .start = 12, .end = 13 },
            },
        } },
    };

    for (statements) |statement| {
        const idx = try store.addStatement(statement);
        try testing.expectEqualDeep(statement, store.getStatement(idx));
    }
}

test "NodeStore round trip - Pattern" {
    var prng = std.Random.DefaultPrng.init(0x5041545445524e);
    const random = prng.random();
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_PATTERN_NODE_COUNT);
    defer store.deinit();

    var expected_test_count: usize = NodeStore.AST_PATTERN_NODE_COUNT;

    var patterns = std.ArrayList(AST.Pattern).empty;
    defer patterns.deinit(gpa);

    try patterns.append(gpa, AST.Pattern{
        .ident = .{
            .ident_tok = rand_token_idx(random),
            .region = rand_region(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .var_ident = .{
            .ident_tok = rand_token_idx(random),
            .region = rand_region(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .tag = .{
            .args = AST.Pattern.Span{ .span = rand_span(random) },
            .qualifiers = AST.Token.Span{ .span = rand_span(random) },
            .region = rand_region(random),
            .tag_tok = rand_token_idx(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .int = .{
            .number_tok = rand_token_idx(random),
            .literal = rand_idx(random, NumericLiteral.Idx),
            .region = rand_region(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .frac = .{
            .number_tok = rand_token_idx(random),
            .literal = rand_idx(random, NumericLiteral.Idx),
            .region = rand_region(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .typed_int = .{
            .number_tok = rand_token_idx(random),
            .type_ident = rand_idx(random, base.Ident.Idx),
            .literal = rand_idx(random, NumericLiteral.Idx),
            .region = rand_region(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .typed_frac = .{
            .number_tok = rand_token_idx(random),
            .type_ident = rand_idx(random, base.Ident.Idx),
            .literal = rand_idx(random, NumericLiteral.Idx),
            .region = rand_region(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .string = .{
            .parts = AST.PatternStringPart.Span{ .span = rand_span(random) },
            .region = rand_region(random),
            .string_tok = rand_token_idx(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .single_quote = .{
            .region = rand_region(random),
            .token = rand_token_idx(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .record = .{
            .fields = AST.PatternRecordField.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .list = .{
            .patterns = AST.Pattern.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .list_rest = .{
            .name = rand_token_idx(random),
            .region = rand_region(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .tuple = .{
            .patterns = AST.Pattern.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .underscore = .{
            .region = rand_region(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .alternatives = .{
            .patterns = AST.Pattern.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .as = .{ .name = rand_token_idx(random), .region = rand_region(random), .pattern = rand_idx(random, AST.Pattern.Idx) },
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
    var prng = std.Random.DefaultPrng.init(0x54595045414e4e4f);
    const random = prng.random();
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_TYPE_ANNO_NODE_COUNT);
    defer store.deinit();

    var expected_test_count: usize = NodeStore.AST_TYPE_ANNO_NODE_COUNT;

    var ty_annos = std.ArrayList(AST.TypeAnno).empty;
    defer ty_annos.deinit(gpa);

    try ty_annos.append(gpa, AST.TypeAnno{
        .apply = .{
            .args = AST.TypeAnno.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .ty_var = .{
            .region = rand_region(random),
            .tok = rand_token_idx(random),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .underscore = .{
            .region = rand_region(random),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .underscore_type_var = .{
            .tok = rand_token_idx(random),
            .region = rand_region(random),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .ty = .{
            .qualifiers = AST.Token.Span{ .span = rand_span(random) },
            .region = rand_region(random),
            .token = rand_token_idx(random),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .tag_union = .{
            .ext = .closed,
            .tags = AST.TypeAnno.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .tag_union = .{
            .ext = .{ .named = .{ .anno = rand_idx(random, AST.TypeAnno.Idx), .region = rand_region(random) } },
            .tags = AST.TypeAnno.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .tag_union = .{
            .ext = .{ .open = rand_token_idx(random) },
            .tags = AST.TypeAnno.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .tuple = .{
            .annos = AST.TypeAnno.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .record = .{
            .fields = AST.AnnoRecordField.Span{ .span = rand_span(random) },
            .ext = .closed,
            .region = rand_region(random),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .record = .{
            .fields = AST.AnnoRecordField.Span{ .span = rand_span(random) },
            .ext = .{ .named = .{ .anno = rand_idx(random, AST.TypeAnno.Idx), .region = rand_region(random) } },
            .region = rand_region(random),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .record = .{
            .fields = AST.AnnoRecordField.Span{ .span = rand_span(random) },
            .ext = .{ .open = rand_token_idx(random) },
            .region = rand_region(random),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .@"fn" = .{
            .args = AST.TypeAnno.Span{ .span = rand_span(random) },
            .ret = rand_idx(random, AST.TypeAnno.Idx),
            .effectful = true,
            .region = rand_region(random),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .parens = .{
            .anno = rand_idx(random, AST.TypeAnno.Idx),
            .region = rand_region(random),
        },
    });

    // We don't include .malformed variant, but we do include extra test cases
    // for record and tag unions extensions (so they are tested with all variants)
    expected_test_count -= 1;
    expected_test_count += 4; // record & tag union ext variants

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
    var prng = std.Random.DefaultPrng.init(0x45585052);
    const random = prng.random();
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_EXPR_NODE_COUNT);
    defer store.deinit();

    var expected_test_count: usize = NodeStore.AST_EXPR_NODE_COUNT;

    var expressions = std.ArrayList(AST.Expr).empty;
    defer expressions.deinit(gpa);

    try expressions.append(gpa, AST.Expr{
        .int = .{
            .region = rand_region(random),
            .token = rand_token_idx(random),
            .literal = rand_idx(random, NumericLiteral.Idx),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .frac = .{
            .region = rand_region(random),
            .token = rand_token_idx(random),
            .literal = rand_idx(random, NumericLiteral.Idx),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .typed_int = .{
            .region = rand_region(random),
            .token = rand_token_idx(random),
            .type_ident = rand_idx(random, base.Ident.Idx),
            .literal = rand_idx(random, NumericLiteral.Idx),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .typed_frac = .{
            .region = rand_region(random),
            .token = rand_token_idx(random),
            .type_ident = rand_idx(random, base.Ident.Idx),
            .literal = rand_idx(random, NumericLiteral.Idx),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .single_quote = .{
            .region = rand_region(random),
            .token = rand_token_idx(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .string_part = .{
            .region = rand_region(random),
            .token = rand_token_idx(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .string = .{
            .parts = AST.Expr.Span{ .span = rand_span(random) },
            .region = rand_region(random),
            .token = rand_token_idx(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .multiline_string = .{
            .parts = AST.Expr.Span{ .span = rand_span(random) },
            .region = rand_region(random),
            .token = rand_token_idx(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .typed_string = .{
            .parts = AST.Expr.Span{ .span = rand_span(random) },
            .type_ident = rand_idx(random, base.Ident.Idx),
            .region = rand_region(random),
            .token = rand_token_idx(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .typed_multiline_string = .{
            .parts = AST.Expr.Span{ .span = rand_span(random) },
            .type_ident = rand_idx(random, base.Ident.Idx),
            .region = rand_region(random),
            .token = rand_token_idx(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .list = .{
            .items = AST.Expr.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .tuple = .{
            .items = AST.Expr.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .record = .{
            .ext = rand_idx(random, AST.Expr.Idx),
            .fields = AST.RecordField.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .tag = .{
            .qualifiers = AST.Token.Span{ .span = rand_span(random) },
            .region = rand_region(random),
            .token = rand_token_idx(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .lambda = .{
            .args = AST.Pattern.Span{ .span = rand_span(random) },
            .region = rand_region(random),
            .body = rand_idx(random, AST.Expr.Idx),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .apply = .{
            .@"fn" = rand_idx(random, AST.Expr.Idx),
            .args = AST.Expr.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .record_updater = .{
            .token = rand_token_idx(random),
            .region = rand_region(random),
        },
    });

    try expressions.append(gpa, AST.Expr{
        .field_access = .{
            .left = rand_idx(random, AST.Expr.Idx),
            .right = rand_idx(random, AST.Expr.Idx),
            .operator = rand_token_idx(random),
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .method_call = .{
            .receiver = rand_idx(random, AST.Expr.Idx),
            .method_token = rand_token_idx(random),
            .args = AST.Expr.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .tuple_access = .{
            .expr = rand_idx(random, AST.Expr.Idx),
            .elem_token = rand_token_idx(random),
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .arrow_call = .{
            .left = rand_idx(random, AST.Expr.Idx),
            .right = rand_idx(random, AST.Expr.Idx),
            .operator = rand_token_idx(random),
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .bin_op = .{
            .left = rand_idx(random, AST.Expr.Idx),
            .right = rand_idx(random, AST.Expr.Idx),
            .operator = rand_token_idx(random),
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .suffix_single_question = .{
            .expr = rand_idx(random, AST.Expr.Idx),
            .operator = rand_token_idx(random),
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .unary_op = .{
            .expr = rand_idx(random, AST.Expr.Idx),
            .operator = rand_token_idx(random),
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .if_then_else = .{
            .@"else" = rand_idx(random, AST.Expr.Idx),
            .condition = rand_idx(random, AST.Expr.Idx),
            .then = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .if_without_else = .{
            .condition = rand_idx(random, AST.Expr.Idx),
            .then = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .match = .{
            .branches = AST.MatchBranch.Span{ .span = rand_span(random) },
            .expr = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .ident = .{
            .qualifiers = AST.Token.Span{ .span = rand_span(random) },
            .region = rand_region(random),
            .token = rand_token_idx(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .dbg = .{
            .expr = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .record_builder = .{
            .fields = AST.RecordField.Span{ .span = rand_span(random) },
            .mapper = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .nominal_record = .{
            .mapper = rand_idx(random, AST.Expr.Idx),
            .backing = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .nominal_apply = .{
            .mapper = rand_idx(random, AST.Expr.Idx),
            .args = AST.Expr.Span{ .span = rand_span(random) },
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .ellipsis = .{ .region = rand_region(random) },
    });
    try expressions.append(gpa, AST.Expr{
        .block = .{
            .region = rand_region(random),
            .statements = AST.Statement.Span{ .span = rand_span(random) },
            .scope = rand_idx(random, AST.DeclIndex.ScopeIdx),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .for_expr = .{
            .patt = rand_idx(random, AST.Pattern.Idx),
            .expr = rand_idx(random, AST.Expr.Idx),
            .body = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .@"break" = .{ .region = rand_region(random) },
    });
    try expressions.append(gpa, AST.Expr{
        .@"return" = .{
            .expr = rand_idx(random, AST.Expr.Idx),
            .region = rand_region(random),
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

test "NodeStore round trip - Targets" {
    var prng = std.Random.DefaultPrng.init(0x54415247455453);
    const random = prng.random();
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_HEADER_NODE_COUNT);
    defer store.deinit();

    // Test TargetFile round trip
    const target_files = [_]AST.TargetFile{
        .{ .string_literal = rand_token_idx(random) },
        .{ .special_ident = rand_token_idx(random) },
        .{ .malformed = .{ .reason = .expected_targets_field_name, .region = rand_region(random) } },
    };

    for (target_files) |file| {
        const idx = try store.addTargetFile(file);
        const retrieved = store.getTargetFile(idx);

        testing.expectEqualDeep(file, retrieved) catch |err| {
            std.debug.print("\n\nOriginal TargetFile:  {any}\n\n", .{file});
            std.debug.print("Retrieved TargetFile: {any}\n\n", .{retrieved});
            return err;
        };
    }

    const config_value = AST.TargetConfigValue{ .int_literal = rand_token_idx(random) };
    const config_value_idx = try store.addTargetConfigValue(config_value);
    const retrieved_config_value = store.getTargetConfigValue(config_value_idx);

    testing.expectEqualDeep(config_value, retrieved_config_value) catch |err| {
        std.debug.print("\n\nOriginal TargetConfigValue:  {any}\n\n", .{config_value});
        std.debug.print("Retrieved TargetConfigValue: {any}\n\n", .{retrieved_config_value});
        return err;
    };

    const config_entry = AST.TargetConfigEntry{
        .name = rand_token_idx(random),
        .value = config_value_idx,
        .region = rand_region(random),
    };
    const config_entry_idx = try store.addTargetConfigEntry(config_entry);
    const retrieved_config_entry = store.getTargetConfigEntry(config_entry_idx);

    testing.expectEqualDeep(config_entry, retrieved_config_entry) catch |err| {
        std.debug.print("\n\nOriginal TargetConfigEntry:  {any}\n\n", .{config_entry});
        std.debug.print("Retrieved TargetConfigEntry: {any}\n\n", .{retrieved_config_entry});
        return err;
    };

    const config = AST.TargetConfig{
        .entries = .{ .span = rand_span(random) },
        .region = rand_region(random),
    };
    const config_idx = try store.addTargetConfig(config);
    const retrieved_config = store.getTargetConfig(config_idx);

    testing.expectEqualDeep(config, retrieved_config) catch |err| {
        std.debug.print("\n\nOriginal TargetConfig:  {any}\n\n", .{config});
        std.debug.print("Retrieved TargetConfig: {any}\n\n", .{retrieved_config});
        return err;
    };

    // Test TargetEntry round trip
    const entry = AST.TargetEntry{
        .target = rand_token_idx(random),
        .config = config_idx,
        .region = rand_region(random),
    };
    const entry_idx = try store.addTargetEntry(entry);
    const retrieved_entry = store.getTargetEntry(entry_idx);

    testing.expectEqualDeep(entry, retrieved_entry) catch |err| {
        std.debug.print("\n\nOriginal TargetEntry:  {any}\n\n", .{entry});
        std.debug.print("Retrieved TargetEntry: {any}\n\n", .{retrieved_entry});
        return err;
    };

    // Test TargetsSection round trip
    const section = AST.TargetsSection{
        .inputs_dir = rand_token_idx(random),
        .entries = .{ .span = rand_span(random) },
        .region = rand_region(random),
    };
    const section_idx = try store.addTargetsSection(section);
    const retrieved_section = store.getTargetsSection(section_idx);

    testing.expectEqualDeep(section, retrieved_section) catch |err| {
        std.debug.print("\n\nOriginal TargetsSection:  {any}\n\n", .{section});
        std.debug.print("Retrieved TargetsSection: {any}\n\n", .{retrieved_section});
        return err;
    };

    // Test TargetsSection with no inputs_dir directive
    const section_nulls = AST.TargetsSection{
        .inputs_dir = null,
        .entries = .{ .span = rand_span(random) },
        .region = rand_region(random),
    };
    const section_nulls_idx = try store.addTargetsSection(section_nulls);
    const retrieved_section_nulls = store.getTargetsSection(section_nulls_idx);

    testing.expectEqualDeep(section_nulls, retrieved_section_nulls) catch |err| {
        std.debug.print("\n\nOriginal TargetsSection (no inputs_dir):  {any}\n\n", .{section_nulls});
        std.debug.print("Retrieved TargetsSection (no inputs_dir): {any}\n\n", .{retrieved_section_nulls});
        return err;
    };
}

test "NodeStore debug function" {
    var prng = std.Random.DefaultPrng.init(0x4445425547);
    const random = prng.random();
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, 16);
    defer store.deinit();

    // Add some nodes to make debug output more interesting
    _ = try store.addHeader(.{
        .type_module = .{
            .region = rand_region(random),
        },
    });

    // Call debug function - it should not crash (use null writer to avoid polluting test output)
    var discard_buf: [4096]u8 = undefined;
    var discard = std.Io.Writer.Discarding.init(&discard_buf);
    try store.debugTo(&discard.writer);
}

test "NodeStore rejects optional index sentinel overflow in release builds" {
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, 16);
    defer store.deinit();

    const max_expr: AST.Expr.Idx = @enumFromInt(std.math.maxInt(u32));
    try testing.expectError(error.OutOfMemory, store.addMatchBranch(.{
        .pattern = @enumFromInt(1),
        .guard = max_expr,
        .body = @enumFromInt(1),
        .region = .{ .start = 0, .end = 0 },
    }));
}

test "NodeStore rejects unaddressable extra data reservations in release builds" {
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, 16);
    defer store.deinit();

    const original_len = store.extra_data.items.len;
    store.extra_data.items.len = std.math.maxInt(u32);
    defer store.extra_data.items.len = original_len;

    try testing.expectError(error.OutOfMemory, store.addHeader(.{
        .platform = .{
            .exposes = @enumFromInt(1),
            .name = 0,
            .packages = @enumFromInt(1),
            .provides = .{ .span = .{ .start = 0, .len = 0 } },
            .hosted = .{ .span = .{ .start = 0, .len = 0 } },
            .requires_entries = .{ .span = .{ .start = 0, .len = 0 } },
            .targets = null,
            .region = .{ .start = 0, .end = 0 },
        },
    }));
}
