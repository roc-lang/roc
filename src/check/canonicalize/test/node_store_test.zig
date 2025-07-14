//! Unit tests to verify `CIR.Statement` are correctly stored in `NodeStore`

const std = @import("std");
const testing = std.testing;
const base = @import("../../../base.zig");
const types = @import("../../../types.zig");
const RocDec = @import("../../../builtins/dec.zig").RocDec;
const Node = @import("../Node.zig");
const NodeStore = @import("../NodeStore.zig");
const CIR = @import("../CIR.zig");

const from_raw_offsets = base.Region.from_raw_offsets;

const Ident = base.Ident;
const CalledVia = base.CalledVia;

test "NodeStore round trip - Statements" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var statements = std.ArrayList(CIR.Statement).init(gpa);
    defer statements.deinit();

    try statements.append(CIR.Statement{
        .s_decl = .{
            .pattern = @enumFromInt(42),
            .expr = @enumFromInt(84),
        },
    });

    try statements.append(CIR.Statement{
        .s_var = .{
            .pattern_idx = @enumFromInt(100),
            .expr = @enumFromInt(200),
        },
    });

    try statements.append(CIR.Statement{
        .s_reassign = .{
            .pattern_idx = @enumFromInt(567),
            .expr = @enumFromInt(345),
        },
    });

    try statements.append(CIR.Statement{
        .s_expr = .{
            .expr = @enumFromInt(3456),
        },
    });

    try statements.append(CIR.Statement{
        .s_crash = .{
            .msg = @enumFromInt(5),
        },
    });

    try statements.append(CIR.Statement{
        .s_dbg = .{
            .expr = @enumFromInt(1234),
        },
    });

    try statements.append(CIR.Statement{
        .s_expect = .{
            .body = @enumFromInt(789),
        },
    });

    try statements.append(CIR.Statement{
        .s_for = .{
            .patt = @enumFromInt(4567),
            .expr = @enumFromInt(3456),
            .body = @enumFromInt(2345),
        },
    });

    try statements.append(CIR.Statement{
        .s_return = .{
            .expr = @enumFromInt(6789),
        },
    });

    const alias: Ident.Idx = @bitCast(@as(u32, 2342));
    const module: Ident.Idx = @bitCast(@as(u32, 4565));
    const qualifier: Ident.Idx = @bitCast(@as(u32, 56756));
    try statements.append(CIR.Statement{
        .s_import = .{
            .module_name_tok = module,
            .qualifier_tok = qualifier,
            .alias_tok = alias,
            .exposes = CIR.ExposedItem.Span{
                .span = base.DataSpan.init(234, 345),
            },
        },
    });

    try statements.append(CIR.Statement{
        .s_alias_decl = .{
            .header = @enumFromInt(456),
            .anno = @enumFromInt(234),
            .anno_var = @enumFromInt(123),
            .where = null,
        },
    });

    try statements.append(CIR.Statement{
        .s_nominal_decl = .{
            .header = @enumFromInt(567),
            .anno = @enumFromInt(345),
            .anno_var = @enumFromInt(234),
            .where = null,
        },
    });

    const name: Ident.Idx = @bitCast(@as(u32, 23423));
    try statements.append(CIR.Statement{ .s_type_anno = .{
        .name = name,
        .anno = @enumFromInt(8676),
        .where = null,
    } });

    for (statements.items, 0..) |stmt, i| {
        const region = from_raw_offsets(@intCast(i * 100), @intCast(i * 100 + 50));
        const idx = try store.addStatement(stmt, region);
        const retrieved = store.getStatement(idx);

        testing.expectEqualDeep(stmt, retrieved) catch |err| {
            std.debug.print("\n\nOriginal:  {any}\n\n", .{stmt});
            std.debug.print("Retrieved: {any}\n\n", .{retrieved});
            return err;
        };
    }

    const actual_test_count = statements.items.len;
    if (actual_test_count < NodeStore.CIR_STATEMENT_NODE_COUNT) {
        std.debug.print("Statement test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.CIR_STATEMENT_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing statement variants.\n", .{});
        return error.IncompleteStatementTestCoverage;
    }
}

test "NodeStore round trip - Expressions" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var expressions = std.ArrayList(CIR.Expr).init(gpa);
    defer expressions.deinit();

    try expressions.append(CIR.Expr{
        .e_int = .{
            .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
        },
    });
    try expressions.append(CIR.Expr{
        .e_frac_f64 = .{
            .value = 3.14,
        },
    });
    try expressions.append(CIR.Expr{
        .e_frac_dec = .{
            .value = CIR.RocDec{ .num = 314 },
        },
    });
    try expressions.append(CIR.Expr{
        .e_dec_small = .{
            .numerator = 314,
            .denominator_power_of_ten = 2,
        },
    });
    try expressions.append(CIR.Expr{
        .e_str_segment = .{
            .literal = @enumFromInt(42),
        },
    });
    try expressions.append(CIR.Expr{
        .e_str = .{
            .span = CIR.Expr.Span{ .span = base.DataSpan.init(6, 3) },
        },
    });
    try expressions.append(CIR.Expr{
        .e_lookup_local = .{
            .pattern_idx = @enumFromInt(200),
        },
    });
    try expressions.append(CIR.Expr{
        .e_lookup_external = .{
            .module_idx = @enumFromInt(0),
            .target_node_idx = 42,
            .region = from_raw_offsets(200, 210),
        },
    });
    try expressions.append(CIR.Expr{
        .e_list = .{
            .elem_var = @enumFromInt(345),
            .elems = CIR.Expr.Span{ .span = base.DataSpan.init(567, 890) },
        },
    });
    try expressions.append(CIR.Expr{
        .e_tuple = .{
            .elems = CIR.Expr.Span{ .span = base.DataSpan.init(456, 789) },
        },
    });
    try expressions.append(CIR.Expr{
        .e_match = CIR.Expr.Match{
            .cond = @enumFromInt(678),
            .branches = CIR.Expr.Match.Branch.Span{ .span = base.DataSpan.init(901, 1123) },
            .exhaustive = @enumFromInt(1134),
        },
    });
    try expressions.append(CIR.Expr{
        .e_if = .{
            .branches = CIR.Expr.IfBranch.Span{ .span = base.DataSpan.init(789, 1012) },
            .final_else = @enumFromInt(1234),
        },
    });
    try expressions.append(CIR.Expr{
        .e_call = .{
            .args = CIR.Expr.Span{ .span = base.DataSpan.init(678, 901) },
            .called_via = CalledVia.apply,
        },
    });
    try expressions.append(CIR.Expr{
        .e_record = .{
            .fields = CIR.RecordField.Span{ .span = base.DataSpan.init(15, 2) },
            .ext = null,
        },
    });
    try expressions.append(CIR.Expr{
        .e_empty_list = .{},
    });
    try expressions.append(CIR.Expr{
        .e_block = .{
            .stmts = CIR.Statement.Span{ .span = base.DataSpan.init(19, 3) },
            .final_expr = @enumFromInt(900),
        },
    });
    try expressions.append(CIR.Expr{
        .e_tag = .{
            .name = @bitCast(@as(u32, 2123)),
            .args = CIR.Expr.Span{ .span = base.DataSpan.init(901, 1234) },
        },
    });
    try expressions.append(CIR.Expr{
        .e_nominal = .{
            .nominal_type_decl = @enumFromInt(345),
            .backing_expr = @enumFromInt(456),
            .backing_type = .tag,
        },
    });
    try expressions.append(CIR.Expr{
        .e_zero_argument_tag = .{
            .closure_name = @bitCast(@as(u32, 2234)),
            .variant_var = @enumFromInt(2345),
            .ext_var = @enumFromInt(2456),
            .name = @bitCast(@as(u32, 2567)),
        },
    });
    try expressions.append(CIR.Expr{
        .e_lambda = .{
            .args = CIR.Pattern.Span{ .span = base.DataSpan.init(17, 2) },
            .body = @enumFromInt(600),
        },
    });
    try expressions.append(CIR.Expr{
        .e_binop = CIR.Expr.Binop.init(
            .add,
            @enumFromInt(700),
            @enumFromInt(800),
        ),
    });
    try expressions.append(CIR.Expr{
        .e_dot_access = .{
            .receiver = @enumFromInt(3012),
            .field_name = @bitCast(@as(u32, 3123)),
            .args = CIR.Expr.Span{ .span = base.DataSpan.init(1123, 1456) },
        },
    });
    try expressions.append(CIR.Expr{
        .e_dot_access = .{
            .receiver = @enumFromInt(3234),
            .field_name = @bitCast(@as(u32, 3345)),
            .args = null,
        },
    });
    try expressions.append(CIR.Expr{
        .e_runtime_error = .{
            .diagnostic = @enumFromInt(123),
        },
    });
    try expressions.append(CIR.Expr{
        .e_crash = .{
            .msg = @enumFromInt(234),
        },
    });
    try expressions.append(CIR.Expr{
        .e_dbg = .{
            .expr = @enumFromInt(345),
        },
    });
    try expressions.append(CIR.Expr{
        .e_empty_record = .{},
    });
    try expressions.append(CIR.Expr{
        .e_expect = .{
            .body = @enumFromInt(456),
        },
    });
    for (expressions.items, 0..) |expr, i| {
        const region = from_raw_offsets(@intCast(i * 100), @intCast(i * 100 + 50));
        const idx = try store.addExpr(expr, region);
        const retrieved = store.getExpr(idx);

        testing.expectEqualDeep(expr, retrieved) catch |err| {
            std.debug.print("\n\nOriginal:  {any}\n\n", .{expr});
            std.debug.print("Retrieved: {any}\n\n", .{retrieved});
            return err;
        };
    }

    const actual_test_count = expressions.items.len;
    if (actual_test_count < NodeStore.CIR_EXPR_NODE_COUNT) {
        std.debug.print("Expression test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.CIR_EXPR_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing expression variants.\n", .{});
        return error.IncompleteExpressionTestCoverage;
    }
}

test "NodeStore round trip - Diagnostics" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var diagnostics = std.ArrayList(CIR.Diagnostic).init(gpa);
    defer diagnostics.deinit();

    // Test all diagnostic types to ensure complete coverage
    try diagnostics.append(CIR.Diagnostic{
        .not_implemented = .{
            .feature = @enumFromInt(123),
            .region = from_raw_offsets(10, 20),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .invalid_num_literal = .{
            .region = from_raw_offsets(30, 40),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .ident_already_in_scope = .{
            .ident = @bitCast(@as(u32, 456)),
            .region = from_raw_offsets(50, 60),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .crash_expects_string = .{
            .region = from_raw_offsets(70, 80),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .ident_not_in_scope = .{
            .ident = @bitCast(@as(u32, 789)),
            .region = from_raw_offsets(70, 80),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .invalid_top_level_statement = .{
            .stmt = @enumFromInt(456),
            .region = from_raw_offsets(80, 90),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .expr_not_canonicalized = .{
            .region = from_raw_offsets(90, 100),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .invalid_string_interpolation = .{
            .region = from_raw_offsets(110, 120),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .pattern_arg_invalid = .{
            .region = from_raw_offsets(130, 140),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .pattern_not_canonicalized = .{
            .region = from_raw_offsets(150, 160),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .can_lambda_not_implemented = .{
            .region = from_raw_offsets(170, 180),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .lambda_body_not_canonicalized = .{
            .region = from_raw_offsets(190, 200),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .if_condition_not_canonicalized = .{
            .region = from_raw_offsets(210, 220),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .if_then_not_canonicalized = .{
            .region = from_raw_offsets(230, 240),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .if_else_not_canonicalized = .{
            .region = from_raw_offsets(250, 260),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .var_across_function_boundary = .{
            .region = from_raw_offsets(270, 280),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .shadowing_warning = .{
            .ident = @bitCast(@as(u32, 1011)),
            .region = from_raw_offsets(290, 300),
            .original_region = from_raw_offsets(310, 320),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .type_redeclared = .{
            .name = @bitCast(@as(u32, 1213)),
            .redeclared_region = from_raw_offsets(330, 340),
            .original_region = from_raw_offsets(350, 360),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .undeclared_type = .{
            .name = @bitCast(@as(u32, 1415)),
            .region = from_raw_offsets(370, 380),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .undeclared_type_var = .{
            .name = @bitCast(@as(u32, 1617)),
            .region = from_raw_offsets(390, 400),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .malformed_type_annotation = .{
            .region = from_raw_offsets(410, 420),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .malformed_where_clause = .{
            .region = from_raw_offsets(430, 440),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .unused_variable = .{
            .ident = @bitCast(@as(u32, 1819)),
            .region = from_raw_offsets(430, 440),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .used_underscore_variable = .{
            .ident = @bitCast(@as(u32, 2021)),
            .region = from_raw_offsets(450, 460),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .type_alias_redeclared = .{
            .name = @bitCast(@as(u32, 2223)),
            .original_region = from_raw_offsets(470, 480),
            .redeclared_region = from_raw_offsets(490, 500),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .nominal_type_redeclared = .{
            .name = @bitCast(@as(u32, 2425)),
            .original_region = from_raw_offsets(510, 520),
            .redeclared_region = from_raw_offsets(530, 540),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .type_shadowed_warning = .{
            .name = @bitCast(@as(u32, 2627)),
            .region = from_raw_offsets(550, 560),
            .original_region = from_raw_offsets(570, 580),
            .cross_scope = true,
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .type_parameter_conflict = .{
            .name = @bitCast(@as(u32, 2829)),
            .parameter_name = @bitCast(@as(u32, 3031)),
            .region = from_raw_offsets(590, 600),
            .original_region = from_raw_offsets(610, 610),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .duplicate_record_field = .{
            .field_name = @bitCast(@as(u32, 3233)),
            .duplicate_region = from_raw_offsets(630, 640),
            .original_region = from_raw_offsets(650, 660),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .invalid_single_quote = .{
            .region = from_raw_offsets(670, 680),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .too_long_single_quote = .{
            .region = from_raw_offsets(690, 700),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .empty_single_quote = .{
            .region = from_raw_offsets(710, 720),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .f64_pattern_literal = .{
            .region = from_raw_offsets(730, 740),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .tuple_elem_not_canonicalized = .{
            .region = from_raw_offsets(740, 750),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .empty_tuple = .{
            .region = from_raw_offsets(750, 760),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .exposed_but_not_implemented = .{
            .ident = @bitCast(@as(u32, 321)),
            .region = from_raw_offsets(760, 770),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .redundant_exposed = .{
            .ident = @bitCast(@as(u32, 432)),
            .region = from_raw_offsets(770, 780),
            .original_region = from_raw_offsets(780, 790),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .module_not_found = .{
            .module_name = @bitCast(@as(u32, 543)),
            .region = from_raw_offsets(790, 800),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .value_not_exposed = .{
            .module_name = @bitCast(@as(u32, 654)),
            .value_name = @bitCast(@as(u32, 655)),
            .region = from_raw_offsets(800, 810),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .type_not_exposed = .{
            .module_name = @bitCast(@as(u32, 765)),
            .type_name = @bitCast(@as(u32, 766)),
            .region = from_raw_offsets(810, 820),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .module_not_imported = .{
            .module_name = @bitCast(@as(u32, 876)),
            .region = from_raw_offsets(820, 830),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .too_many_exports = .{
            .count = 65536,
            .region = from_raw_offsets(830, 840),
        },
    });

    // Test the round-trip for all diagnostics
    for (diagnostics.items) |diagnostic| {
        const idx = try store.addDiagnostic(diagnostic);
        const retrieved = store.getDiagnostic(idx);

        testing.expectEqualDeep(diagnostic, retrieved) catch |err| {
            std.debug.print("\n\nOriginal:  {any}\n\n", .{diagnostic});
            std.debug.print("Retrieved: {any}\n\n", .{retrieved});
            return err;
        };
    }

    const actual_test_count = diagnostics.items.len;
    if (actual_test_count < NodeStore.CIR_DIAGNOSTIC_NODE_COUNT) {
        std.debug.print("Diagnostic test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.CIR_DIAGNOSTIC_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing diagnostic variants.\n", .{});
        return error.IncompleteDiagnosticTestCoverage;
    }
}

test "NodeStore round trip - TypeAnno" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var type_annos = std.ArrayList(CIR.TypeAnno).init(gpa);
    defer type_annos.deinit();

    // Test all TypeAnno variants to ensure complete coverage
    try type_annos.append(CIR.TypeAnno{
        .apply = .{
            .symbol = @bitCast(@as(u32, 123)),
            .args = CIR.TypeAnno.Span{ .span = base.DataSpan.init(456, 789) },
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .ty_var = .{
            .name = @bitCast(@as(u32, 234)),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .underscore = {},
    });

    try type_annos.append(CIR.TypeAnno{
        .ty = .{
            .symbol = @bitCast(@as(u32, 345)),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .ty = .{
            .symbol = @bitCast(@as(u32, 567)),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .tag_union = .{
            .tags = CIR.TypeAnno.Span{ .span = base.DataSpan.init(678, 890) },
            .ext = @enumFromInt(901),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .tuple = .{
            .elems = CIR.TypeAnno.Span{ .span = base.DataSpan.init(1012, 1234) },
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .record = .{
            .fields = CIR.TypeAnno.RecordField.Span{ .span = base.DataSpan.init(1345, 1567) },
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .@"fn" = .{
            .args = CIR.TypeAnno.Span{ .span = base.DataSpan.init(1678, 1890) },
            .ret = @enumFromInt(1901),
            .effectful = true,
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .parens = .{
            .anno = @enumFromInt(2012),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .ty = .{
            .symbol = @bitCast(@as(u32, 2034)),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .ty_lookup_external = .{
            .external_decl = @enumFromInt(3001),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .malformed = .{
            .diagnostic = @enumFromInt(2123),
        },
    });

    // Test the round-trip for all type annotations
    for (type_annos.items, 0..) |type_anno, i| {
        const region = from_raw_offsets(@intCast(i * 100), @intCast(i * 100 + 50));
        const idx = try store.addTypeAnno(type_anno, region);
        const retrieved = store.getTypeAnno(idx);

        testing.expectEqualDeep(type_anno, retrieved) catch |err| {
            std.debug.print("\n\nOriginal:  {any}\n\n", .{type_anno});
            std.debug.print("Retrieved: {any}\n\n", .{retrieved});
            return err;
        };
    }

    const actual_test_count = type_annos.items.len;
    if (actual_test_count < NodeStore.CIR_TYPE_ANNO_NODE_COUNT) {
        std.debug.print("CIR.TypeAnno test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.CIR_TYPE_ANNO_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing type annotation variants.\n", .{});
        return error.IncompleteTypeAnnoTestCoverage;
    }
}

test "NodeStore round trip - Pattern" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var patterns = std.ArrayList(CIR.Pattern).init(gpa);
    defer patterns.deinit();

    // Test all Pattern variants to ensure complete coverage
    try patterns.append(CIR.Pattern{
        .assign = .{
            .ident = @bitCast(@as(u32, 123)),
        },
    });
    try patterns.append(CIR.Pattern{
        .as = .{
            .pattern = @enumFromInt(234),
            .ident = @bitCast(@as(u32, 345)),
        },
    });
    try patterns.append(CIR.Pattern{
        .applied_tag = .{
            .name = @bitCast(@as(u32, 567)),
            .args = CIR.Pattern.Span{ .span = base.DataSpan.init(678, 789) },
        },
    });
    try patterns.append(CIR.Pattern{
        .record_destructure = .{
            .whole_var = @enumFromInt(890),
            .ext_var = @enumFromInt(901),
            .destructs = CIR.Pattern.RecordDestruct.Span{ .span = base.DataSpan.init(1012, 1123) },
        },
    });
    try patterns.append(CIR.Pattern{
        .list = .{
            .list_var = @enumFromInt(1234),
            .elem_var = @enumFromInt(1345),
            .patterns = CIR.Pattern.Span{ .span = base.DataSpan.init(1456, 1567) },
            .rest_info = .{ .index = 3, .pattern = @enumFromInt(5676) },
        },
    });
    try patterns.append(CIR.Pattern{
        .tuple = .{
            .patterns = CIR.Pattern.Span{ .span = base.DataSpan.init(1678, 1789) },
        },
    });
    try patterns.append(CIR.Pattern{
        .int_literal = .{
            .value = CIR.IntValue{
                .bytes = @bitCast(@as(i128, 42)),
                .kind = .i128,
            },
        },
    });
    try patterns.append(CIR.Pattern{
        .small_dec_literal = .{
            .numerator = 123,
            .denominator_power_of_ten = 2,
        },
    });
    try patterns.append(CIR.Pattern{
        .dec_literal = .{
            .value = RocDec.fromU64(1890),
        },
    });
    try patterns.append(CIR.Pattern{
        .str_literal = .{
            .literal = @enumFromInt(1901),
        },
    });

    try patterns.append(CIR.Pattern{ .underscore = {} });
    try patterns.append(CIR.Pattern{
        .runtime_error = .{
            .diagnostic = @enumFromInt(2123),
        },
    });

    // Test the round-trip for all patterns with their original regions
    const regions = [_]base.Region{
        from_raw_offsets(10, 20), // assign
        from_raw_offsets(30, 40), // as
        from_raw_offsets(50, 60), // applied_tag
        from_raw_offsets(70, 80), // record_destructure
        from_raw_offsets(90, 100), // list
        from_raw_offsets(110, 120), // tuple
        from_raw_offsets(130, 140), // int_literal
        from_raw_offsets(150, 160), // small_dec_literal
        from_raw_offsets(170, 180), // dec_literal
        from_raw_offsets(210, 220), // str_literal
        from_raw_offsets(230, 240), // underscore
        from_raw_offsets(250, 260), // runtime_error
    };

    for (patterns.items, regions) |pattern, region| {
        const idx = try store.addPattern(pattern, region);
        const retrieved = store.getPattern(idx);

        testing.expectEqualDeep(pattern, retrieved) catch |err| {
            std.debug.print("\n\nOriginal:  {any}\n\n", .{pattern});
            std.debug.print("Retrieved: {any}\n\n", .{retrieved});
            return err;
        };

        // Also verify the region was stored correctly
        const stored_region = store.getRegionAt(@enumFromInt(@intFromEnum(idx)));
        testing.expectEqualDeep(region, stored_region) catch |err| {
            std.debug.print("\n\nExpected region: {any}\n\n", .{region});
            std.debug.print("Stored region: {any}\n\n", .{stored_region});
            return err;
        };
    }

    const actual_test_count = patterns.items.len;
    if (actual_test_count < NodeStore.CIR_PATTERN_NODE_COUNT) {
        std.debug.print("CIR.Pattern test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.CIR_PATTERN_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing pattern variants.\n", .{});
        return error.IncompletePatternTestCoverage;
    }
}
