//! Unit tests to verify `ModuleEnv.Statement` are correctly stored in `NodeStore`

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const types = @import("types");
const compile = @import("compile");
const builtins = @import("builtins");

const StringLiteral = base.StringLiteral;
const ModuleEnv = compile.ModuleEnv;
const NodeStore = compile.NodeStore;
const RocDec = builtins.dec.RocDec;
const CalledVia = base.CalledVia;
const TypeVar = types.Var;
const Ident = base.Ident;
const from_raw_offsets = base.Region.from_raw_offsets;

var rand = std.Random.DefaultPrng.init(1234);

/// Generate a random index of type `T`.
fn rand_idx(comptime T: type) T {
    return @enumFromInt(rand.random().int(u32));
}

/// Generate a random index of type `T`.
fn rand_idx_u16(comptime T: type) T {
    return @enumFromInt(rand.random().int(u16));
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

/// Generate a random identifier index.
fn rand_ident_idx() Ident.Idx {
    return @bitCast(rand.random().int(u32));
}

/// Helper to create a random region.
fn rand_region() base.Region {
    const start = rand.random().int(u32);
    var end = rand.random().int(u32);
    while (start > end) {
        end = rand.random().int(u32);
    }
    return from_raw_offsets(start, end);
}

test "NodeStore round trip - Statements" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var statements = std.ArrayList(ModuleEnv.Statement).init(gpa);
    defer statements.deinit();

    try statements.append(ModuleEnv.Statement{
        .s_decl = .{
            .pattern = rand_idx(ModuleEnv.Pattern.Idx),
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_var = .{
            .pattern_idx = rand_idx(ModuleEnv.Pattern.Idx),
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_reassign = .{
            .pattern_idx = rand_idx(ModuleEnv.Pattern.Idx),
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_expr = .{
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_crash = .{
            .msg = rand_idx(StringLiteral.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_dbg = .{
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_expect = .{
            .body = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_for = .{
            .patt = rand_idx(ModuleEnv.Pattern.Idx),
            .expr = rand_idx(ModuleEnv.Expr.Idx),
            .body = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_return = .{
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_import = .{
            .module_name_tok = rand_ident_idx(),
            .qualifier_tok = rand_ident_idx(),
            .alias_tok = rand_ident_idx(),
            .exposes = ModuleEnv.ExposedItem.Span{ .span = rand_span() },
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_alias_decl = .{
            .header = rand_idx(ModuleEnv.TypeHeader.Idx),
            .anno = rand_idx(ModuleEnv.TypeAnno.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{
        .s_nominal_decl = .{
            .header = rand_idx(ModuleEnv.TypeHeader.Idx),
            .anno = rand_idx(ModuleEnv.TypeAnno.Idx),
        },
    });

    try statements.append(ModuleEnv.Statement{ .s_type_anno = .{
        .name = rand_ident_idx(),
        .anno = rand_idx(ModuleEnv.TypeAnno.Idx),
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
    if (actual_test_count < NodeStore.MODULEENV_STATEMENT_NODE_COUNT) {
        std.debug.print("Statement test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.MODULEENV_STATEMENT_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing statement variants.\n", .{});
        return error.IncompleteStatementTestCoverage;
    }
}

test "NodeStore round trip - Expressions" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var expressions = std.ArrayList(ModuleEnv.Expr).init(gpa);
    defer expressions.deinit();

    try expressions.append(ModuleEnv.Expr{
        .e_int = .{
            .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_frac_f32 = .{ .value = rand.random().float(f32) },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_frac_f64 = .{ .value = rand.random().float(f64) },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_frac_dec = .{
            .value = ModuleEnv.RocDec{ .num = 314 },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_dec_small = .{
            .numerator = rand.random().int(i16),
            .denominator_power_of_ten = rand.random().int(u8),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_str_segment = .{
            .literal = rand_idx(StringLiteral.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_str = .{
            .span = ModuleEnv.Expr.Span{ .span = rand_span() },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_lookup_local = .{
            .pattern_idx = rand_idx(ModuleEnv.Pattern.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_lookup_external = .{
            .module_idx = rand_idx_u16(ModuleEnv.Import.Idx),
            .target_node_idx = rand.random().int(u16),
            .region = rand_region(),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_list = .{
            .elem_var = rand_idx(TypeVar),
            .elems = ModuleEnv.Expr.Span{ .span = rand_span() },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_tuple = .{
            .elems = ModuleEnv.Expr.Span{ .span = rand_span() },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_match = ModuleEnv.Expr.Match{
            .cond = rand_idx(ModuleEnv.Expr.Idx),
            .branches = ModuleEnv.Expr.Match.Branch.Span{ .span = rand_span() },
            .exhaustive = rand_idx(TypeVar),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_if = .{
            .branches = ModuleEnv.Expr.IfBranch.Span{ .span = rand_span() },
            .final_else = rand_idx(ModuleEnv.Expr.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_call = .{
            .args = ModuleEnv.Expr.Span{ .span = rand_span() },
            .called_via = CalledVia.apply,
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_record = .{
            .fields = ModuleEnv.RecordField.Span{ .span = rand_span() },
            .ext = null,
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_empty_list = .{},
    });
    try expressions.append(ModuleEnv.Expr{
        .e_block = .{
            .stmts = ModuleEnv.Statement.Span{ .span = rand_span() },
            .final_expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_tag = .{
            .name = rand_ident_idx(),
            .args = ModuleEnv.Expr.Span{ .span = rand_span() },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_nominal = .{
            .nominal_type_decl = rand_idx(ModuleEnv.Statement.Idx),
            .backing_expr = rand_idx(ModuleEnv.Expr.Idx),
            .backing_type = .tag,
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_zero_argument_tag = .{
            .closure_name = rand_ident_idx(),
            .variant_var = rand_idx(TypeVar),
            .ext_var = rand_idx(TypeVar),
            .name = rand_ident_idx(),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_closure = .{
            .lambda_idx = rand_idx(ModuleEnv.Expr.Idx),
            .captures = ModuleEnv.Expr.Capture.Span{ .span = rand_span() },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_lambda = .{
            .args = ModuleEnv.Pattern.Span{ .span = rand_span() },
            .body = rand_idx(ModuleEnv.Expr.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_binop = ModuleEnv.Expr.Binop.init(
            .add,
            rand_idx(ModuleEnv.Expr.Idx),
            rand_idx(ModuleEnv.Expr.Idx),
        ),
    });
    try expressions.append(ModuleEnv.Expr{
        .e_unary_minus = ModuleEnv.Expr.UnaryMinus.init(rand_idx(ModuleEnv.Expr.Idx)),
    });
    try expressions.append(ModuleEnv.Expr{
        .e_unary_not = ModuleEnv.Expr.UnaryNot.init(rand_idx(ModuleEnv.Expr.Idx)),
    });
    try expressions.append(ModuleEnv.Expr{
        .e_dot_access = .{
            .receiver = rand_idx(ModuleEnv.Expr.Idx),
            .field_name = rand_ident_idx(),
            .args = null,
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_runtime_error = .{
            .diagnostic = rand_idx(ModuleEnv.Diagnostic.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_crash = .{
            .msg = rand_idx(StringLiteral.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_dbg = .{
            .expr = rand_idx(ModuleEnv.Expr.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_empty_record = .{},
    });
    try expressions.append(ModuleEnv.Expr{
        .e_expect = .{
            .body = rand_idx(ModuleEnv.Expr.Idx),
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_frac_dec = .{
            .value = ModuleEnv.RocDec{ .num = 123456789 },
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_nominal_external = .{
            .module_idx = rand_idx_u16(ModuleEnv.Import.Idx),
            .target_node_idx = rand.random().int(u16),
            .backing_expr = rand_idx(ModuleEnv.Expr.Idx),
            .backing_type = .tag,
        },
    });
    try expressions.append(ModuleEnv.Expr{
        .e_ellipsis = .{},
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
    if (actual_test_count < NodeStore.MODULEENV_EXPR_NODE_COUNT) {
        std.debug.print("Expression test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.MODULEENV_EXPR_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing expression variants.\n", .{});
        return error.IncompleteExpressionTestCoverage;
    }
}

test "NodeStore round trip - Diagnostics" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var diagnostics = std.ArrayList(ModuleEnv.Diagnostic).init(gpa);
    defer diagnostics.deinit();

    // Test all diagnostic types to ensure complete coverage
    try diagnostics.append(ModuleEnv.Diagnostic{
        .not_implemented = .{
            .feature = rand_idx(StringLiteral.Idx),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .invalid_num_literal = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .ident_already_in_scope = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .crash_expects_string = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .ident_not_in_scope = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .invalid_top_level_statement = .{
            .stmt = rand_idx(StringLiteral.Idx),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .expr_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .invalid_string_interpolation = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .pattern_arg_invalid = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .pattern_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .can_lambda_not_implemented = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .lambda_body_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .if_condition_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .if_then_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .if_else_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .var_across_function_boundary = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .shadowing_warning = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
            .original_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_redeclared = .{
            .name = rand_ident_idx(),
            .redeclared_region = rand_region(),
            .original_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .undeclared_type = .{
            .name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .undeclared_type_var = .{
            .name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .malformed_type_annotation = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .malformed_where_clause = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .where_clause_not_allowed_in_type_decl = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .unused_variable = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .used_underscore_variable = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_alias_redeclared = .{
            .name = rand_ident_idx(),
            .original_region = rand_region(),
            .redeclared_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .nominal_type_redeclared = .{
            .name = rand_ident_idx(),
            .original_region = rand_region(),
            .redeclared_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_shadowed_warning = .{
            .name = rand_ident_idx(),
            .region = rand_region(),
            .original_region = rand_region(),
            .cross_scope = rand.random().boolean(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_parameter_conflict = .{
            .name = rand_ident_idx(),
            .parameter_name = rand_ident_idx(),
            .region = rand_region(),
            .original_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .duplicate_record_field = .{
            .field_name = rand_ident_idx(),
            .duplicate_region = rand_region(),
            .original_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .invalid_single_quote = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .f64_pattern_literal = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .unused_type_var_name = .{
            .name = rand_ident_idx(),
            .suggested_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_var_marked_unused = .{
            .name = rand_ident_idx(),
            .suggested_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_var_ending_in_underscore = .{
            .name = rand_ident_idx(),
            .suggested_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .underscore_in_type_declaration = .{
            .is_alias = rand.random().boolean(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .tuple_elem_not_canonicalized = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .empty_tuple = .{
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .exposed_but_not_implemented = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .redundant_exposed = .{
            .ident = rand_ident_idx(),
            .region = rand_region(),
            .original_region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .module_not_found = .{
            .module_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .value_not_exposed = .{
            .module_name = rand_ident_idx(),
            .value_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .type_not_exposed = .{
            .module_name = rand_ident_idx(),
            .type_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .module_not_imported = .{
            .module_name = rand_ident_idx(),
            .region = rand_region(),
        },
    });

    try diagnostics.append(ModuleEnv.Diagnostic{
        .too_many_exports = .{
            .count = rand.random().int(u32),
            .region = rand_region(),
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
    if (actual_test_count < NodeStore.MODULEENV_DIAGNOSTIC_NODE_COUNT) {
        std.debug.print("Diagnostic test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.MODULEENV_DIAGNOSTIC_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing diagnostic variants.\n", .{});
        return error.IncompleteDiagnosticTestCoverage;
    }
}

test "NodeStore round trip - TypeAnno" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var type_annos = std.ArrayList(ModuleEnv.TypeAnno).init(gpa);
    defer type_annos.deinit();

    // Test all TypeAnno variants to ensure complete coverage
    try type_annos.append(ModuleEnv.TypeAnno{
        .apply = .{
            .symbol = rand_ident_idx(),
            .args = ModuleEnv.TypeAnno.Span{ .span = rand_span() },
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .ty_var = .{
            .name = rand_ident_idx(),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .underscore = {},
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .ty = .{
            .symbol = rand_ident_idx(),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .ty = .{
            .symbol = rand_ident_idx(),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .tag_union = .{
            .tags = ModuleEnv.TypeAnno.Span{ .span = rand_span() },
            .ext = rand_idx(ModuleEnv.TypeAnno.Idx),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .tuple = .{
            .elems = ModuleEnv.TypeAnno.Span{ .span = rand_span() },
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .record = .{
            .fields = ModuleEnv.TypeAnno.RecordField.Span{ .span = rand_span() },
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .@"fn" = .{
            .args = ModuleEnv.TypeAnno.Span{ .span = rand_span() },
            .ret = rand_idx(ModuleEnv.TypeAnno.Idx),
            .effectful = rand.random().boolean(),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .parens = .{
            .anno = rand_idx(ModuleEnv.TypeAnno.Idx),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .ty = .{
            .symbol = rand_ident_idx(),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .ty_lookup_external = .{
            .module_idx = rand_idx(ModuleEnv.Import.Idx),
            .target_node_idx = rand.random().int(u16),
        },
    });

    try type_annos.append(ModuleEnv.TypeAnno{
        .malformed = .{
            .diagnostic = rand_idx(ModuleEnv.Diagnostic.Idx),
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
    if (actual_test_count < NodeStore.MODULEENV_TYPE_ANNO_NODE_COUNT) {
        std.debug.print("ModuleEnv.TypeAnno test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.MODULEENV_TYPE_ANNO_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing type annotation variants.\n", .{});
        return error.IncompleteTypeAnnoTestCoverage;
    }
}

test "NodeStore round trip - Pattern" {
    const gpa = testing.allocator;
    var store = try NodeStore.init(gpa);
    defer store.deinit();

    var patterns = std.ArrayList(ModuleEnv.Pattern).init(gpa);
    defer patterns.deinit();

    // Test all Pattern variants to ensure complete coverage
    try patterns.append(ModuleEnv.Pattern{
        .assign = .{
            .ident = rand_ident_idx(),
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .as = .{
            .pattern = rand_idx(ModuleEnv.Pattern.Idx),
            .ident = rand_ident_idx(),
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .applied_tag = .{
            .name = rand_ident_idx(),
            .args = ModuleEnv.Pattern.Span{ .span = rand_span() },
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .nominal = .{
            .nominal_type_decl = rand_idx(ModuleEnv.Statement.Idx),
            .backing_pattern = rand_idx(ModuleEnv.Pattern.Idx),
            .backing_type = .tag,
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .nominal_external = .{
            .module_idx = rand_idx_u16(ModuleEnv.Import.Idx),
            .target_node_idx = rand.random().int(u16),
            .backing_pattern = rand_idx(ModuleEnv.Pattern.Idx),
            .backing_type = .tag,
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .record_destructure = .{
            .whole_var = rand_idx(TypeVar),
            .ext_var = rand_idx(TypeVar),
            .destructs = ModuleEnv.Pattern.RecordDestruct.Span{ .span = rand_span() },
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .list = .{
            .list_var = rand_idx(TypeVar),
            .elem_var = rand_idx(TypeVar),
            .patterns = ModuleEnv.Pattern.Span{ .span = rand_span() },
            .rest_info = .{ .index = rand.random().int(u32), .pattern = rand_idx(ModuleEnv.Pattern.Idx) },
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .tuple = .{
            .patterns = ModuleEnv.Pattern.Span{ .span = rand_span() },
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .int_literal = .{
            .value = ModuleEnv.IntValue{
                .bytes = @bitCast(rand.random().int(i128)),
                .kind = .i128,
            },
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .small_dec_literal = .{
            .numerator = rand.random().int(i16),
            .denominator_power_of_ten = rand.random().int(u8),
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .dec_literal = .{
            .value = RocDec.fromU64(rand.random().int(u64)),
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .str_literal = .{
            .literal = rand_idx(StringLiteral.Idx),
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .frac_f32_literal = .{
            .value = rand.random().float(f32),
        },
    });
    try patterns.append(ModuleEnv.Pattern{
        .frac_f64_literal = .{
            .value = rand.random().float(f64),
        },
    });
    try patterns.append(ModuleEnv.Pattern{ .underscore = {} });
    try patterns.append(ModuleEnv.Pattern{
        .runtime_error = .{
            .diagnostic = rand_idx(ModuleEnv.Diagnostic.Idx),
        },
    });

    // Test the round-trip for all patterns with their original regions
    var regions = [_]base.Region{undefined} ** NodeStore.MODULEENV_PATTERN_NODE_COUNT;
    for (&regions) |*region| {
        region.* = rand_region();
    }

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
    if (actual_test_count < NodeStore.MODULEENV_PATTERN_NODE_COUNT) {
        std.debug.print("ModuleEnv.Pattern test coverage insufficient! Need at least {d} test cases but found {d}.\n", .{ NodeStore.MODULEENV_PATTERN_NODE_COUNT, actual_test_count });
        std.debug.print("Please add test cases for missing pattern variants.\n", .{});
        return error.IncompletePatternTestCoverage;
    }
}
