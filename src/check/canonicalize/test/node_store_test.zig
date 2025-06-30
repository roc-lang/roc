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

test "NodeStore round trip - Statements" {
    const gpa = testing.allocator;
    var store = NodeStore.init(gpa);
    defer store.deinit();

    var statements = std.ArrayList(CIR.Statement).init(gpa);
    defer statements.deinit();

    try statements.append(CIR.Statement{
        .s_decl = .{
            .pattern = @enumFromInt(42),
            .expr = @enumFromInt(84),
            .region = from_raw_offsets(23, 56),
        },
    });

    try statements.append(CIR.Statement{
        .s_var = .{
            .pattern_idx = @enumFromInt(100),
            .expr = @enumFromInt(200),
            .region = from_raw_offsets(6, 23),
        },
    });

    try statements.append(CIR.Statement{
        .s_reassign = .{
            .expr = @enumFromInt(345),
            .pattern_idx = @enumFromInt(567),
            .region = from_raw_offsets(34, 156),
        },
    });

    try statements.append(CIR.Statement{
        .s_expr = .{
            .expr = @enumFromInt(3456),
            .region = from_raw_offsets(12, 213),
        },
    });

    try statements.append(CIR.Statement{
        .s_crash = .{
            .msg = @enumFromInt(1234),
            .region = from_raw_offsets(12, 34),
        },
    });

    try statements.append(CIR.Statement{
        .s_expect = .{
            .body = @enumFromInt(565),
            .region = from_raw_offsets(67, 234),
        },
    });

    try statements.append(CIR.Statement{
        .s_for = .{
            .body = @enumFromInt(565),
            .expr = @enumFromInt(687),
            .patt = @enumFromInt(5234),
            .region = from_raw_offsets(23, 547),
        },
    });

    try statements.append(CIR.Statement{
        .s_return = .{
            .expr = @enumFromInt(7567),
            .region = from_raw_offsets(3453, 1232),
        },
    });

    const alias: Ident.Idx = @bitCast(@as(u32, 2342));
    const module: Ident.Idx = @bitCast(@as(u32, 4565));
    const qualifier: Ident.Idx = @bitCast(@as(u32, 56756));
    try statements.append(CIR.Statement{
        .s_import = .{
            .alias_tok = alias,
            .exposes = CIR.ExposedItem.Span{
                .span = base.DataSpan.init(234, 345),
            },
            .module_name_tok = module,
            .qualifier_tok = qualifier,
            .region = from_raw_offsets(75646, 123),
        },
    });

    try statements.append(CIR.Statement{
        .s_type_decl = .{
            .anno = @enumFromInt(8676),
            .header = @enumFromInt(723),
            .where = CIR.WhereClause.Span{ .span = base.DataSpan.init(234, 45645) },
            .region = from_raw_offsets(3453, 1232),
        },
    });

    const name: Ident.Idx = @bitCast(@as(u32, 23423));
    try statements.append(CIR.Statement{ .s_type_anno = .{
        .anno = @enumFromInt(8676),
        .name = name,
        .where = CIR.WhereClause.Span{ .span = base.DataSpan.init(234, 34534) },
        .region = from_raw_offsets(3453, 1232),
    } });

    for (statements.items) |stmt| {
        const idx = store.addStatement(stmt);
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
    var store = NodeStore.init(gpa);
    defer store.deinit();

    var expressions = std.ArrayList(CIR.Expr).init(gpa);
    defer expressions.deinit();

    try expressions.append(CIR.Expr{
        .e_num = .{
            .region = from_raw_offsets(123, 345),
            .value = CIR.IntValue.placeholder(),
        },
    });
    try expressions.append(CIR.Expr{
        .e_int = .{
            .region = from_raw_offsets(234, 345),
            .value = CIR.IntValue.placeholder(),
        },
    });
    try expressions.append(CIR.Expr{
        .e_frac_f64 = .{
            .value = 3.14159,
            .region = from_raw_offsets(456, 789),
        },
    });
    try expressions.append(CIR.Expr{
        .e_frac_dec = .{
            .value = RocDec.fromF64(2.718) orelse RocDec.one_point_zero,
            .region = from_raw_offsets(567, 890),
        },
    });
    try expressions.append(CIR.Expr{
        .e_dec_small = .{
            .numerator = 42,
            .denominator_power_of_ten = 3,
            .region = from_raw_offsets(678, 901),
        },
    });
    try expressions.append(CIR.Expr{
        .e_str_segment = .{
            .literal = @enumFromInt(789),
            .region = from_raw_offsets(789, 1012),
        },
    });
    try expressions.append(CIR.Expr{
        .e_str = .{
            .span = CIR.Expr.Span{ .span = base.DataSpan.init(123, 456) },
            .region = from_raw_offsets(890, 1123),
        },
    });
    try expressions.append(CIR.Expr{
        .e_lookup = .{
            .local = .{
                .pattern_idx = @enumFromInt(234),
                .region = from_raw_offsets(901, 1234),
            },
        },
    });
    try expressions.append(CIR.Expr{
        .e_lookup = .{
            .external = @enumFromInt(345),
        },
    });
    try expressions.append(CIR.Expr{
        .e_list = .{
            .elem_var = @enumFromInt(456),
            .elems = CIR.Expr.Span{ .span = base.DataSpan.init(234, 567) },
            .region = from_raw_offsets(1012, 1345),
        },
    });
    try expressions.append(CIR.Expr{
        .e_tuple = .{
            .elems = CIR.Expr.Span{ .span = base.DataSpan.init(345, 678) },
            .region = from_raw_offsets(1123, 1456),
        },
    });
    try expressions.append(CIR.Expr{
        .e_when = CIR.When{
            .loc_cond = @enumFromInt(567),
            .cond_var = @enumFromInt(678),
            .expr_var = @enumFromInt(789),
            .region = from_raw_offsets(1234, 1567),
            .branches = CIR.WhenBranch.Span{ .span = base.DataSpan.init(456, 789) },
            .branches_cond_var = @enumFromInt(890),
            .exhaustive = @enumFromInt(901),
        },
    });
    try expressions.append(CIR.Expr{
        .e_if = .{
            .branches = CIR.IfBranch.Span{ .span = base.DataSpan.init(567, 890) },
            .final_else = @enumFromInt(1234),
            .region = from_raw_offsets(1345, 1678),
        },
    });
    try expressions.append(CIR.Expr{
        .e_call = .{
            .args = CIR.Expr.Span{ .span = base.DataSpan.init(678, 901) },
            .called_via = base.CalledVia.apply,
            .effect_var = @enumFromInt(1345),
            .region = from_raw_offsets(1456, 1789),
        },
    });
    try expressions.append(CIR.Expr{
        .e_record = .{
            .ext_var = @enumFromInt(1456),
            .fields = CIR.RecordField.Span{ .span = base.DataSpan.init(789, 1012) },
            .region = from_raw_offsets(1567, 1890),
        },
    });
    try expressions.append(CIR.Expr{
        .e_empty_record = .{
            .region = from_raw_offsets(1678, 1901),
        },
    });
    try expressions.append(CIR.Expr{
        .e_block = .{
            .stmts = CIR.Statement.Span{ .span = base.DataSpan.init(890, 1123) },
            .final_expr = @enumFromInt(1567),
            .region = from_raw_offsets(1789, 2012),
        },
    });
    try expressions.append(CIR.Expr{
        .e_record_access = .{
            .record_var = @enumFromInt(1678),
            .ext_var = @enumFromInt(1789),
            .field_var = @enumFromInt(1890),
            .loc_expr = @enumFromInt(1901),
            .field = @bitCast(@as(u32, 2012)),
            .region = from_raw_offsets(1890, 2123),
        },
    });

    try expressions.append(CIR.Expr{
        .e_tag = .{
            .ext_var = @enumFromInt(2012),
            .name = @bitCast(@as(u32, 2123)),
            .args = CIR.Expr.Span{ .span = base.DataSpan.init(901, 1234) },
            .region = from_raw_offsets(1901, 2234),
        },
    });

    try expressions.append(CIR.Expr{
        .e_zero_argument_tag = .{
            .closure_name = @bitCast(@as(u32, 2234)),
            .variant_var = @enumFromInt(2345),
            .ext_var = @enumFromInt(2456),
            .name = @bitCast(@as(u32, 2567)),
            .region = from_raw_offsets(2012, 2345),
        },
    });

    try expressions.append(CIR.Expr{
        .e_lambda = .{
            .args = CIR.Pattern.Span{ .span = base.DataSpan.init(1012, 1345) },
            .body = @enumFromInt(2678),
            .effect_var = @enumFromInt(2789),
            .region = from_raw_offsets(2123, 2456),
        },
    });

    try expressions.append(CIR.Expr{
        .e_binop = CIR.Expr.Binop.init(
            .add,
            @enumFromInt(2890),
            @enumFromInt(2901),
            from_raw_offsets(2234, 2567),
        ),
    });
    try expressions.append(CIR.Expr{
        .e_dot_access = .{
            .receiver = @enumFromInt(3012),
            .field_name = @bitCast(@as(u32, 3123)),
            .args = CIR.Expr.Span{ .span = base.DataSpan.init(1123, 1456) },
            .region = from_raw_offsets(2345, 2678),
        },
    });
    try expressions.append(CIR.Expr{
        .e_dot_access = .{
            .receiver = @enumFromInt(3234),
            .field_name = @bitCast(@as(u32, 3345)),
            .args = null,
            .region = from_raw_offsets(2456, 2789),
        },
    });
    try expressions.append(CIR.Expr{
        .e_runtime_error = .{
            .diagnostic = @enumFromInt(3456),
            .region = from_raw_offsets(2567, 2890),
        },
    });

    for (expressions.items) |expr| {
        const idx = store.addExpr(expr);
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
    var store = NodeStore.init(gpa);
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
        .ident_not_in_scope = .{
            .ident = @bitCast(@as(u32, 789)),
            .region = from_raw_offsets(70, 80),
        },
    });

    try diagnostics.append(CIR.Diagnostic{
        .invalid_top_level_statement = .{
            .stmt = @enumFromInt(456),
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

    // Test the round-trip for all diagnostics
    for (diagnostics.items) |diagnostic| {
        const idx = store.addDiagnostic(diagnostic);
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
    var store = NodeStore.init(gpa);
    defer store.deinit();

    var type_annos = std.ArrayList(CIR.TypeAnno).init(gpa);
    defer type_annos.deinit();

    // Test all TypeAnno variants to ensure complete coverage
    try type_annos.append(CIR.TypeAnno{
        .apply = .{
            .symbol = @bitCast(@as(u32, 123)),
            .args = CIR.TypeAnno.Span{ .span = base.DataSpan.init(456, 789) },
            .region = from_raw_offsets(10, 20),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .ty_var = .{
            .name = @bitCast(@as(u32, 234)),
            .region = from_raw_offsets(30, 40),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .underscore = .{
            .region = from_raw_offsets(50, 60),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .ty = .{
            .symbol = @bitCast(@as(u32, 345)),
            .region = from_raw_offsets(70, 80),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .mod_ty = .{
            .mod_symbol = @bitCast(@as(u32, 456)),
            .ty_symbol = @bitCast(@as(u32, 567)),
            .region = from_raw_offsets(90, 100),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .tag_union = .{
            .tags = CIR.TypeAnno.Span{ .span = base.DataSpan.init(678, 890) },
            .open_anno = @enumFromInt(901),
            .region = from_raw_offsets(110, 120),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .tuple = .{
            .annos = CIR.TypeAnno.Span{ .span = base.DataSpan.init(1012, 1234) },
            .region = from_raw_offsets(130, 140),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .record = .{
            .fields = CIR.AnnoRecordField.Span{ .span = base.DataSpan.init(1345, 1567) },
            .region = from_raw_offsets(150, 160),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .@"fn" = .{
            .args = CIR.TypeAnno.Span{ .span = base.DataSpan.init(1678, 1890) },
            .ret = @enumFromInt(1901),
            .effectful = true,
            .region = from_raw_offsets(170, 180),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .parens = .{
            .anno = @enumFromInt(2012),
            .region = from_raw_offsets(190, 200),
        },
    });

    try type_annos.append(CIR.TypeAnno{
        .malformed = .{
            .diagnostic = @enumFromInt(2123),
            .region = from_raw_offsets(210, 220),
        },
    });

    // Test the round-trip for all type annotations
    for (type_annos.items) |type_anno| {
        const idx = store.addTypeAnno(type_anno);
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
