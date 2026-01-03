//! Unit tests to verify `CIR.Statement` are correctly stored in `NodeStore`

const std = @import("std");
const testing = std.testing;
const base = @import("base");

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

    var headers = std.ArrayList(AST.Header).empty;
    defer headers.deinit(gpa);

    try headers.append(gpa, AST.Header{
        .app = .{
            .packages = rand_idx(AST.Collection.Idx),
            .platform_idx = rand_idx(AST.RecordField.Idx),
            .provides = rand_idx(AST.Collection.Idx),
            .region = rand_region(),
        },
    });

    try headers.append(gpa, AST.Header{
        .module = .{
            .exposes = rand_idx(AST.Collection.Idx),
            .region = rand_region(),
        },
    });

    try headers.append(gpa, AST.Header{
        .package = .{
            .exposes = rand_idx(AST.Collection.Idx),
            .packages = rand_idx(AST.Collection.Idx),
            .region = rand_region(),
        },
    });

    try headers.append(gpa, AST.Header{
        .platform = .{
            .exposes = rand_idx(AST.Collection.Idx),
            .name = rand_token_idx(),
            .packages = rand_idx(AST.Collection.Idx),
            .provides = rand_idx(AST.Collection.Idx),
            .requires_entries = .{ .span = .{ .start = 0, .len = 0 } },
            .targets = null,
            .region = rand_region(),
        },
    });

    try headers.append(gpa, AST.Header{
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

    var statements = std.ArrayList(AST.Statement).empty;
    defer statements.deinit(gpa);

    try statements.append(gpa, AST.Statement{
        .decl = .{
            .body = rand_idx(AST.Expr.Idx),
            .pattern = rand_idx(AST.Pattern.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(gpa, AST.Statement{
        .@"var" = .{
            .name = rand_token_idx(),
            .body = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(gpa, AST.Statement{
        .expr = .{
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(gpa, AST.Statement{
        .crash = .{
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(gpa, AST.Statement{
        .dbg = .{
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(gpa, AST.Statement{
        .expect = .{
            .body = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(gpa, AST.Statement{
        .@"for" = .{
            .patt = rand_idx(AST.Pattern.Idx),
            .expr = rand_idx(AST.Expr.Idx),
            .body = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try statements.append(gpa, AST.Statement{
        .@"return" = .{
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    // Simple import with no tokens
    try statements.append(gpa, AST.Statement{
        .import = .{
            .alias_tok = null,
            .module_name_tok = rand_token_idx(),
            .qualifier_tok = null,
            .region = rand_region(),
            .exposes = AST.ExposedItem.Span{ .span = rand_span() },
            .nested_import = false,
        },
    });
    // Import with alias
    try statements.append(gpa, AST.Statement{
        .import = .{
            .alias_tok = rand_token_idx(),
            .module_name_tok = rand_token_idx(),
            .qualifier_tok = null,
            .region = rand_region(),
            .exposes = AST.ExposedItem.Span{ .span = rand_span() },
            .nested_import = false,
        },
    });
    // Import with qualifier but no alias
    try statements.append(gpa, AST.Statement{
        .import = .{
            .alias_tok = null,
            .module_name_tok = rand_token_idx(),
            .qualifier_tok = rand_token_idx(),
            .region = rand_region(),
            .exposes = AST.ExposedItem.Span{ .span = rand_span() },
            .nested_import = false,
        },
    });
    // Import with both qualifier and alias
    try statements.append(gpa, AST.Statement{
        .import = .{
            .alias_tok = rand_token_idx(),
            .module_name_tok = rand_token_idx(),
            .qualifier_tok = rand_token_idx(),
            .region = rand_region(),
            .exposes = AST.ExposedItem.Span{ .span = rand_span() },
            .nested_import = false,
        },
    });
    try statements.append(gpa, AST.Statement{
        .type_decl = .{
            .anno = rand_idx(AST.TypeAnno.Idx),
            .header = rand_idx(AST.TypeHeader.Idx),
            .kind = AST.TypeDeclKind.nominal,
            .region = rand_region(),
            .where = rand_idx(AST.Collection.Idx),
            .associated = null,
        },
    });
    try statements.append(gpa, AST.Statement{
        .type_anno = .{
            .name = rand_token_idx(),
            .anno = rand_idx(AST.TypeAnno.Idx),
            .where = rand_idx(AST.Collection.Idx),
            .is_var = false,
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

    var patterns = std.ArrayList(AST.Pattern).empty;
    defer patterns.deinit(gpa);

    try patterns.append(gpa, AST.Pattern{
        .ident = .{
            .ident_tok = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .var_ident = .{
            .ident_tok = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .tag = .{
            .args = AST.Pattern.Span{ .span = rand_span() },
            .qualifiers = AST.Token.Span{ .span = rand_span() },
            .region = rand_region(),
            .tag_tok = rand_token_idx(),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .int = .{
            .number_tok = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .frac = .{
            .number_tok = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .string = .{
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
            .string_tok = rand_token_idx(),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .single_quote = .{
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .record = .{
            .fields = AST.PatternRecordField.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .list = .{
            .patterns = AST.Pattern.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .list_rest = .{
            .name = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .tuple = .{
            .patterns = AST.Pattern.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .underscore = .{
            .region = rand_region(),
        },
    });
    try patterns.append(gpa, AST.Pattern{
        .alternatives = .{
            .patterns = AST.Pattern.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try patterns.append(gpa, AST.Pattern{
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

    var ty_annos = std.ArrayList(AST.TypeAnno).empty;
    defer ty_annos.deinit(gpa);

    try ty_annos.append(gpa, AST.TypeAnno{
        .apply = .{
            .args = AST.TypeAnno.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .ty_var = .{
            .region = rand_region(),
            .tok = rand_token_idx(),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .underscore = .{
            .region = rand_region(),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .ty = .{
            .qualifiers = AST.Token.Span{ .span = rand_span() },
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .tag_union = .{
            .ext = .{ .named = rand_idx(AST.TypeAnno.Idx) },
            .tags = AST.TypeAnno.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .tuple = .{
            .annos = AST.TypeAnno.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .record = .{
            .fields = AST.AnnoRecordField.Span{ .span = rand_span() },
            .ext = null,
            .region = rand_region(),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .record = .{
            .fields = AST.AnnoRecordField.Span{ .span = rand_span() },
            .ext = rand_idx(AST.TypeAnno.Idx), // Test record with extension
            .region = rand_region(),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .@"fn" = .{
            .args = AST.TypeAnno.Span{ .span = rand_span() },
            .ret = rand_idx(AST.TypeAnno.Idx),
            .effectful = true,
            .region = rand_region(),
        },
    });
    try ty_annos.append(gpa, AST.TypeAnno{
        .parens = .{
            .anno = rand_idx(AST.TypeAnno.Idx),
            .region = rand_region(),
        },
    });

    // We don't include .malformed variant, but we do include an extra test case
    // for record with extension (so record is tested both with and without ext)
    expected_test_count -= 1;
    expected_test_count += 1; // record with ext

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

    var expressions = std.ArrayList(AST.Expr).empty;
    defer expressions.deinit(gpa);

    try expressions.append(gpa, AST.Expr{
        .int = .{
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .frac = .{
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .typed_int = .{
            .region = rand_region(),
            .token = rand_token_idx(),
            .type_token = rand_token_idx(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .typed_frac = .{
            .region = rand_region(),
            .token = rand_token_idx(),
            .type_token = rand_token_idx(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .single_quote = .{
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .string_part = .{
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .string = .{
            .parts = AST.Expr.Span{ .span = rand_span() },
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .list = .{
            .items = AST.Expr.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .tuple = .{
            .items = AST.Expr.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .record = .{
            .ext = rand_idx(AST.Expr.Idx),
            .fields = AST.RecordField.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .tag = .{
            .qualifiers = AST.Token.Span{ .span = rand_span() },
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .lambda = .{
            .args = AST.Pattern.Span{ .span = rand_span() },
            .region = rand_region(),
            .body = rand_idx(AST.Expr.Idx),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .apply = .{
            .@"fn" = rand_idx(AST.Expr.Idx),
            .args = AST.Expr.Span{ .span = rand_span() },
            .region = rand_region(),
        },
    });

    try expressions.append(gpa, AST.Expr{
        .field_access = .{
            .left = rand_idx(AST.Expr.Idx),
            .right = rand_idx(AST.Expr.Idx),
            .operator = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .local_dispatch = .{
            .left = rand_idx(AST.Expr.Idx),
            .right = rand_idx(AST.Expr.Idx),
            .operator = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .bin_op = .{
            .left = rand_idx(AST.Expr.Idx),
            .right = rand_idx(AST.Expr.Idx),
            .operator = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .suffix_single_question = .{
            .expr = rand_idx(AST.Expr.Idx),
            .operator = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .unary_op = .{
            .expr = rand_idx(AST.Expr.Idx),
            .operator = rand_token_idx(),
            .region = rand_region(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .if_then_else = .{
            .@"else" = rand_idx(AST.Expr.Idx),
            .condition = rand_idx(AST.Expr.Idx),
            .then = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .match = .{
            .branches = AST.MatchBranch.Span{ .span = rand_span() },
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .ident = .{
            .qualifiers = AST.Token.Span{ .span = rand_span() },
            .region = rand_region(),
            .token = rand_token_idx(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .dbg = .{
            .expr = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .record_builder = .{
            .fields = rand_idx(AST.RecordField.Idx),
            .mapper = rand_idx(AST.Expr.Idx),
            .region = rand_region(),
        },
    });
    try expressions.append(gpa, AST.Expr{
        .ellipsis = .{ .region = rand_region() },
    });
    try expressions.append(gpa, AST.Expr{
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
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, NodeStore.AST_HEADER_NODE_COUNT);
    defer store.deinit();

    // Test TargetFile round trip
    const target_files = [_]AST.TargetFile{
        .{ .string_literal = rand_token_idx() },
        .{ .special_ident = rand_token_idx() },
        .{ .malformed = .{ .reason = .expected_targets_field_name, .region = rand_region() } },
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

    // Test TargetEntry round trip
    const entry = AST.TargetEntry{
        .target = rand_token_idx(),
        .files = .{ .span = rand_span() },
        .region = rand_region(),
    };
    const entry_idx = try store.addTargetEntry(entry);
    const retrieved_entry = store.getTargetEntry(entry_idx);

    testing.expectEqualDeep(entry, retrieved_entry) catch |err| {
        std.debug.print("\n\nOriginal TargetEntry:  {any}\n\n", .{entry});
        std.debug.print("Retrieved TargetEntry: {any}\n\n", .{retrieved_entry});
        return err;
    };

    // Test TargetLinkType round trip
    const link_type = AST.TargetLinkType{
        .entries = .{ .span = rand_span() },
        .region = rand_region(),
    };
    const link_type_idx = try store.addTargetLinkType(link_type);
    const retrieved_link_type = store.getTargetLinkType(link_type_idx);

    testing.expectEqualDeep(link_type, retrieved_link_type) catch |err| {
        std.debug.print("\n\nOriginal TargetLinkType:  {any}\n\n", .{link_type});
        std.debug.print("Retrieved TargetLinkType: {any}\n\n", .{retrieved_link_type});
        return err;
    };

    // Test TargetsSection round trip
    const section = AST.TargetsSection{
        .files_path = rand_token_idx(),
        .exe = link_type_idx,
        .static_lib = null,
        .region = rand_region(),
    };
    const section_idx = try store.addTargetsSection(section);
    const retrieved_section = store.getTargetsSection(section_idx);

    testing.expectEqualDeep(section, retrieved_section) catch |err| {
        std.debug.print("\n\nOriginal TargetsSection:  {any}\n\n", .{section});
        std.debug.print("Retrieved TargetsSection: {any}\n\n", .{retrieved_section});
        return err;
    };

    // Test TargetsSection with null values
    const section_nulls = AST.TargetsSection{
        .files_path = null,
        .exe = null,
        .static_lib = null,
        .region = rand_region(),
    };
    const section_nulls_idx = try store.addTargetsSection(section_nulls);
    const retrieved_section_nulls = store.getTargetsSection(section_nulls_idx);

    testing.expectEqualDeep(section_nulls, retrieved_section_nulls) catch |err| {
        std.debug.print("\n\nOriginal TargetsSection (nulls):  {any}\n\n", .{section_nulls});
        std.debug.print("Retrieved TargetsSection (nulls): {any}\n\n", .{retrieved_section_nulls});
        return err;
    };
}

test "NodeStore debug function" {
    const gpa = testing.allocator;
    var store = try NodeStore.initCapacity(gpa, 16);
    defer store.deinit();

    // Add some nodes to make debug output more interesting
    _ = try store.addHeader(.{
        .type_module = .{
            .region = rand_region(),
        },
    });

    // Call debug function - it should not crash
    store.debug();
}
