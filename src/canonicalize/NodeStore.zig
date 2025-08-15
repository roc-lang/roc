//! Stores AST nodes and provides scratch arrays for working with nodes.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const builtins = @import("builtins");
const collections = @import("collections");

const Diagnostic = @import("Diagnostic.zig");
const ModuleEnv = @import("ModuleEnv.zig");
const Node = @import("Node.zig");
const CIR = @import("CIR.zig");

const SERIALIZATION_ALIGNMENT = collections.SERIALIZATION_ALIGNMENT;

const CompactWriter = collections.CompactWriter;
const SafeList = collections.SafeList;
const RocDec = builtins.dec.RocDec;
const DataSpan = base.DataSpan;
const Region = base.Region;
const StringLiteral = base.StringLiteral;
const Ident = base.Ident;
const PackedDataSpan = base.PackedDataSpan;
const FunctionArgs = base.FunctionArgs;

const NodeStore = @This();

gpa: std.mem.Allocator,
nodes: Node.List,
regions: Region.List,
extra_data: collections.SafeList(u32),
scratch_statements: base.Scratch(CIR.Statement.Idx),
scratch_exprs: base.Scratch(CIR.Expr.Idx),
scratch_record_fields: base.Scratch(CIR.RecordField.Idx),
scratch_match_branches: base.Scratch(CIR.Expr.Match.Branch.Idx),
scratch_match_branch_patterns: base.Scratch(CIR.Expr.Match.BranchPattern.Idx),
scratch_if_branches: base.Scratch(CIR.Expr.IfBranch.Idx),
scratch_where_clauses: base.Scratch(CIR.WhereClause.Idx),
scratch_patterns: base.Scratch(CIR.Pattern.Idx),
scratch_pattern_record_fields: base.Scratch(CIR.PatternRecordField.Idx),
scratch_record_destructs: base.Scratch(CIR.Pattern.RecordDestruct.Idx),
scratch_type_annos: base.Scratch(CIR.TypeAnno.Idx),
scratch_anno_record_fields: base.Scratch(CIR.TypeAnno.RecordField.Idx),
scratch_exposed_items: base.Scratch(CIR.ExposedItem.Idx),
scratch_defs: base.Scratch(CIR.Def.Idx),
scratch_diagnostics: base.Scratch(CIR.Diagnostic.Idx),
scratch_captures: base.Scratch(CIR.Expr.Capture.Idx),

/// Initializes the NodeStore
pub fn init(gpa: std.mem.Allocator) std.mem.Allocator.Error!NodeStore {
    // TODO determine what capacity to use
    // maybe these should be moved to build/compile flags?
    return try NodeStore.initCapacity(gpa, 128);
}

/// Initializes the NodeStore with a specified capacity.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!NodeStore {
    return .{
        .gpa = gpa,
        .nodes = try Node.List.initCapacity(gpa, capacity),
        .regions = try Region.List.initCapacity(gpa, capacity),
        .extra_data = try collections.SafeList(u32).initCapacity(gpa, capacity / 2),
        .scratch_statements = try base.Scratch(CIR.Statement.Idx).init(gpa),
        .scratch_exprs = try base.Scratch(CIR.Expr.Idx).init(gpa),
        .scratch_record_fields = try base.Scratch(CIR.RecordField.Idx).init(gpa),
        .scratch_match_branches = try base.Scratch(CIR.Expr.Match.Branch.Idx).init(gpa),
        .scratch_match_branch_patterns = try base.Scratch(CIR.Expr.Match.BranchPattern.Idx).init(gpa),
        .scratch_if_branches = try base.Scratch(CIR.Expr.IfBranch.Idx).init(gpa),
        .scratch_where_clauses = try base.Scratch(CIR.WhereClause.Idx).init(gpa),
        .scratch_patterns = try base.Scratch(CIR.Pattern.Idx).init(gpa),
        .scratch_pattern_record_fields = try base.Scratch(CIR.PatternRecordField.Idx).init(gpa),
        .scratch_record_destructs = try base.Scratch(CIR.Pattern.RecordDestruct.Idx).init(gpa),
        .scratch_type_annos = try base.Scratch(CIR.TypeAnno.Idx).init(gpa),
        .scratch_anno_record_fields = try base.Scratch(CIR.TypeAnno.RecordField.Idx).init(gpa),
        .scratch_exposed_items = try base.Scratch(CIR.ExposedItem.Idx).init(gpa),
        .scratch_defs = try base.Scratch(CIR.Def.Idx).init(gpa),
        .scratch_diagnostics = try base.Scratch(CIR.Diagnostic.Idx).init(gpa),
        .scratch_captures = try base.Scratch(CIR.Expr.Capture.Idx).init(gpa),
    };
}

/// Deinitializes the NodeStore, freeing any allocated resources.
pub fn deinit(store: *NodeStore) void {
    store.nodes.deinit(store.gpa);
    store.regions.deinit(store.gpa);
    store.extra_data.deinit(store.gpa);
    store.scratch_statements.deinit(store.gpa);
    store.scratch_exprs.deinit(store.gpa);
    store.scratch_record_fields.deinit(store.gpa);
    store.scratch_match_branches.deinit(store.gpa);
    store.scratch_match_branch_patterns.deinit(store.gpa);
    store.scratch_if_branches.deinit(store.gpa);
    store.scratch_where_clauses.deinit(store.gpa);
    store.scratch_patterns.deinit(store.gpa);
    store.scratch_pattern_record_fields.deinit(store.gpa);
    store.scratch_record_destructs.deinit(store.gpa);
    store.scratch_type_annos.deinit(store.gpa);
    store.scratch_anno_record_fields.deinit(store.gpa);
    store.scratch_exposed_items.deinit(store.gpa);
    store.scratch_defs.deinit(store.gpa);
    store.scratch_diagnostics.deinit(store.gpa);
    store.scratch_captures.deinit(store.gpa);
}

/// Compile-time constants for union variant counts to ensure we don't miss cases
/// when adding/removing variants from ModuleEnv unions. Update these when modifying the unions.
///
/// Count of the diagnostic nodes in the ModuleEnv
pub const MODULEENV_DIAGNOSTIC_NODE_COUNT = 46;
/// Count of the expression nodes in the ModuleEnv
pub const MODULEENV_EXPR_NODE_COUNT = 33;
/// Count of the statement nodes in the ModuleEnv
pub const MODULEENV_STATEMENT_NODE_COUNT = 13;
/// Count of the type annotation nodes in the ModuleEnv
pub const MODULEENV_TYPE_ANNO_NODE_COUNT = 12;
/// Count of the pattern nodes in the ModuleEnv
pub const MODULEENV_PATTERN_NODE_COUNT = 16;

comptime {
    // Check the number of CIR.Diagnostic nodes
    const diagnostic_fields = @typeInfo(CIR.Diagnostic).@"union".fields;
    std.debug.assert(diagnostic_fields.len == MODULEENV_DIAGNOSTIC_NODE_COUNT);
}

comptime {
    // Check the number of CIR.Expr nodes
    const expr_fields = @typeInfo(CIR.Expr).@"union".fields;
    std.debug.assert(expr_fields.len == MODULEENV_EXPR_NODE_COUNT);
}

comptime {
    // Check the number of CIR.Statement nodes
    const statement_fields = @typeInfo(CIR.Statement).@"union".fields;
    std.debug.assert(statement_fields.len == MODULEENV_STATEMENT_NODE_COUNT);
}

comptime {
    // Check the number of CIR.TypeAnno nodes
    const type_anno_fields = @typeInfo(CIR.TypeAnno).@"union".fields;
    std.debug.assert(type_anno_fields.len == MODULEENV_TYPE_ANNO_NODE_COUNT);
}

comptime {
    // Check the number of CIR.Pattern nodes
    const pattern_fields = @typeInfo(CIR.Pattern).@"union".fields;
    std.debug.assert(pattern_fields.len == MODULEENV_PATTERN_NODE_COUNT);
}

/// Helper function to get a region by node index, handling the type conversion
pub fn getRegionAt(store: *const NodeStore, node_idx: Node.Idx) Region {
    const idx: Region.Idx = @enumFromInt(@intFromEnum(node_idx));
    return store.regions.get(idx).*;
}

/// Helper function to get a region by pattern index
pub fn getPatternRegion(store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) Region {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));
    return store.getRegionAt(node_idx);
}

/// Helper function to get a region by expression index
pub fn getExprRegion(store: *const NodeStore, expr_idx: CIR.Expr.Idx) Region {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(expr_idx));
    return store.getRegionAt(node_idx);
}

/// Helper function to get a region by statement index
pub fn getStatementRegion(store: *const NodeStore, stmt_idx: CIR.Statement.Idx) Region {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(stmt_idx));
    return store.getRegionAt(node_idx);
}

/// Helper function to get a region by type annotation index
pub fn getTypeAnnoRegion(store: *const NodeStore, type_anno_idx: CIR.TypeAnno.Idx) Region {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(type_anno_idx));
    return store.getRegionAt(node_idx);
}

/// Retrieves a region from node from the store.
pub fn getNodeRegion(store: *const NodeStore, node_idx: Node.Idx) Region {
    return store.getRegionAt(node_idx);
}

/// Retrieves a statement node from the store.
pub fn getStatement(store: *const NodeStore, statement: CIR.Statement.Idx) CIR.Statement {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(statement));
    const node = store.nodes.get(node_idx);

    switch (node.tag) {
        .statement_decl => return CIR.Statement{ .s_decl = .{
            .pattern = @enumFromInt(node.data_1),
            .expr = @enumFromInt(node.data_2),
            .annotation = if (node.data_3 == 0) null else @enumFromInt(node.data_3),
        } },
        .statement_var => return CIR.Statement{ .s_var = .{
            .pattern_idx = @enumFromInt(node.data_1),
            .expr = @enumFromInt(node.data_2),
        } },
        .statement_reassign => return CIR.Statement{ .s_reassign = .{
            .pattern_idx = @enumFromInt(node.data_1),
            .expr = @enumFromInt(node.data_2),
        } },
        .statement_crash => return CIR.Statement{ .s_crash = .{
            .msg = @enumFromInt(node.data_1),
        } },
        .statement_dbg => return CIR.Statement{ .s_dbg = .{
            .expr = @enumFromInt(node.data_1),
        } },
        .statement_expr => return .{ .s_expr = .{
            .expr = @enumFromInt(node.data_1),
        } },
        .statement_expect => return CIR.Statement{ .s_expect = .{
            .body = @enumFromInt(node.data_1),
        } },
        .statement_for => return CIR.Statement{ .s_for = .{
            .patt = @enumFromInt(node.data_1),
            .expr = @enumFromInt(node.data_2),
            .body = @enumFromInt(node.data_3),
        } },
        .statement_return => return CIR.Statement{ .s_return = .{
            .expr = @enumFromInt(node.data_1),
        } },
        .statement_import => {
            const extra_start = node.data_2;
            const extra_data = store.extra_data.items.items[extra_start..];

            const alias_data = extra_data[0];
            const qualifier_data = extra_data[1];
            const flags = extra_data[2];
            const exposes_start = extra_data[3];
            const exposes_len = extra_data[4];

            const alias_tok = if (flags & 1 != 0) @as(?Ident.Idx, @bitCast(alias_data)) else null;
            const qualifier_tok = if (flags & 2 != 0) @as(?Ident.Idx, @bitCast(qualifier_data)) else null;

            return CIR.Statement{
                .s_import = .{
                    .module_name_tok = @bitCast(node.data_1),
                    .qualifier_tok = qualifier_tok,
                    .alias_tok = alias_tok,
                    .exposes = DataSpan.init(exposes_start, exposes_len).as(CIR.ExposedItem.Span),
                },
            };
        },
        .statement_alias_decl => {
            return CIR.Statement{
                .s_alias_decl = .{
                    .header = @as(CIR.TypeHeader.Idx, @enumFromInt(node.data_1)),
                    .anno = @as(CIR.TypeAnno.Idx, @enumFromInt(node.data_2)),
                },
            };
        },
        .statement_nominal_decl => {
            return CIR.Statement{
                .s_nominal_decl = .{
                    .header = @as(CIR.TypeHeader.Idx, @enumFromInt(node.data_1)),
                    .anno = @as(CIR.TypeAnno.Idx, @enumFromInt(node.data_2)),
                },
            };
        },
        .statement_type_anno => {
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items.items[extra_start..];

            const anno: CIR.TypeAnno.Idx = @enumFromInt(extra_data[0]);
            const name: Ident.Idx = @bitCast(extra_data[1]);
            const where_flag = extra_data[2];

            const where_clause = if (where_flag == 1) blk: {
                const where_start = extra_data[3];
                const where_len = extra_data[4];
                break :blk CIR.WhereClause.Span{ .span = DataSpan.init(where_start, where_len) };
            } else null;

            return CIR.Statement{
                .s_type_anno = .{
                    .name = name,
                    .anno = anno,
                    .where = where_clause,
                },
            };
        },
        else => {
            @panic("unreachable, node is not an expression tag");
        },
    }
}

/// Retrieves an expression node from the store.
pub fn getExpr(store: *const NodeStore, expr: CIR.Expr.Idx) CIR.Expr {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(expr));
    const node = store.nodes.get(node_idx);

    switch (node.tag) {
        .expr_var => {
            return CIR.Expr{
                .e_lookup_local = .{
                    .pattern_idx = @enumFromInt(node.data_1),
                },
            };
        },
        .expr_external_lookup => {
            // Handle external lookups
            return CIR.Expr{ .e_lookup_external = .{
                .module_idx = @enumFromInt(node.data_1),
                .target_node_idx = @intCast(node.data_2),
                .region = store.getRegionAt(node_idx),
            } };
        },
        .expr_int => {
            // Read i128 from extra_data (stored as 4 u32s in data_1)
            const value_as_u32s = store.extra_data.items.items[node.data_1..][0..4];

            // Retrieve type variable from data_2 and requirements from data_3
            return CIR.Expr{
                .e_int = .{
                    .value = .{ .bytes = @bitCast(value_as_u32s.*), .kind = .i128 },
                },
            };
        },
        .expr_list => {
            return CIR.Expr{
                .e_list = .{
                    .elem_var = @enumFromInt(node.data_3),
                    .elems = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
                },
            };
        },
        .expr_tuple => {
            return CIR.Expr{
                .e_tuple = .{
                    .elems = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
                },
            };
        },
        .expr_call => {
            // Retrieve args span from extra_data
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items.items[extra_start..];

            const args_start = extra_data[0];
            const args_len = extra_data[1];

            return CIR.Expr{
                .e_call = .{
                    .args = .{ .span = .{ .start = args_start, .len = args_len } },
                    .called_via = @enumFromInt(node.data_2),
                },
            };
        },
        .expr_frac_f32 => return CIR.Expr{ .e_frac_f32 = .{ .value = @bitCast(node.data_1) } },
        .expr_frac_f64 => {
            const raw: [2]u32 = .{ node.data_1, node.data_2 };

            return CIR.Expr{ .e_frac_f64 = .{ .value = @bitCast(raw) } };
        },
        .expr_frac_dec => {
            // Get value from extra_data
            const extra_data_idx = node.data_1;
            const value_as_u32s = store.extra_data.items.items[extra_data_idx..][0..4];
            const value_as_i128: i128 = @bitCast(value_as_u32s.*);

            return CIR.Expr{
                .e_frac_dec = .{
                    .value = RocDec{ .num = value_as_i128 },
                },
            };
        },
        .expr_dec_small => {
            // Unpack small dec data from data_1 and data_3
            // data_1: numerator (i16) stored as u32
            // data_3: denominator_power_of_ten (u8) in lower 8 bits
            const numerator = @as(i16, @intCast(@as(i32, @bitCast(node.data_1))));
            const denominator_power_of_ten = @as(u8, @truncate(node.data_3));

            return CIR.Expr{
                .e_dec_small = .{
                    .numerator = numerator,
                    .denominator_power_of_ten = denominator_power_of_ten,
                },
            };
        },
        .expr_string_segment => return CIR.Expr.initStrSegment(
            @enumFromInt(node.data_1),
        ),
        .expr_string => return CIR.Expr.initStr(
            DataSpan.init(node.data_1, node.data_2).as(CIR.Expr.Span),
        ),
        .expr_tag => {
            const name = @as(Ident.Idx, @bitCast(node.data_1));
            const args_start = node.data_2;
            const args_len = node.data_3;

            return CIR.Expr{
                .e_tag = .{
                    .name = name,
                    .args = .{ .span = .{ .start = args_start, .len = args_len } },
                },
            };
        },
        .expr_nominal => {
            const nominal_type_decl: CIR.Statement.Idx = @enumFromInt(node.data_1);
            const backing_expr: CIR.Expr.Idx = @enumFromInt(node.data_2);
            const backing_type: CIR.Expr.NominalBackingType = @enumFromInt(node.data_3);

            return CIR.Expr{
                .e_nominal = .{
                    .nominal_type_decl = nominal_type_decl,
                    .backing_expr = backing_expr,
                    .backing_type = backing_type,
                },
            };
        },
        .expr_nominal_external => {
            const module_idx: CIR.Import.Idx = @enumFromInt(node.data_1);
            const target_node_idx: u16 = @intCast(node.data_2);

            const extra_data_idx = node.data_3;
            const extra_data = store.extra_data.items.items[extra_data_idx..][0..2];
            const backing_expr: CIR.Expr.Idx = @enumFromInt(extra_data[0]);
            const backing_type: CIR.Expr.NominalBackingType = @enumFromInt(extra_data[1]);

            return CIR.Expr{
                .e_nominal_external = .{
                    .module_idx = module_idx,
                    .target_node_idx = target_node_idx,
                    .backing_expr = backing_expr,
                    .backing_type = backing_type,
                },
            };
        },
        .expr_bin_op => {
            return CIR.Expr{
                .e_binop = CIR.Expr.Binop.init(
                    @enumFromInt(node.data_1),
                    @enumFromInt(node.data_2),
                    @enumFromInt(node.data_3),
                ),
            };
        },
        .expr_closure => {
            // Retrieve closure data from extra_data
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items.items[extra_start..];

            const lambda_idx = extra_data[0];
            const capture_start = extra_data[1];
            const capture_len = extra_data[2];

            return CIR.Expr{
                .e_closure = .{
                    .lambda_idx = @enumFromInt(lambda_idx),
                    .captures = .{ .span = .{ .start = capture_start, .len = capture_len } },
                },
            };
        },
        .expr_lambda => {
            // Retrieve lambda data from extra_data
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items.items[extra_start..];

            const args_start = extra_data[0];
            const args_len = extra_data[1];
            const body_idx = extra_data[2];

            return CIR.Expr{
                .e_lambda = .{
                    .args = .{ .span = .{ .start = args_start, .len = args_len } },
                    .body = @enumFromInt(body_idx),
                },
            };
        },
        .expr_block => {
            return CIR.Expr{
                .e_block = .{
                    .stmts = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
                    .final_expr = @enumFromInt(node.data_3),
                },
            };
        },
        .expr_empty_record => {
            return CIR.Expr{ .e_empty_record = .{} };
        },
        .expr_empty_list => {
            return CIR.Expr{ .e_empty_list = .{} };
        },
        .expr_record => {
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items.items[extra_start..];

            const fields_start = extra_data[0];
            const fields_len = extra_data[1];
            const ext_value = extra_data[2];

            const ext = if (ext_value == 0) null else @as(CIR.Expr.Idx, @enumFromInt(ext_value));

            return CIR.Expr{
                .e_record = .{
                    .fields = .{ .span = .{ .start = fields_start, .len = fields_len } },
                    .ext = ext,
                },
            };
        },
        .expr_match => {
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items.items[extra_start..];

            const cond = @as(CIR.Expr.Idx, @enumFromInt(extra_data[0]));
            const branches_start = extra_data[1];
            const branches_len = extra_data[2];
            const exhaustive = @as(types.Var, @enumFromInt(extra_data[3]));

            return CIR.Expr{
                .e_match = CIR.Expr.Match{
                    .cond = cond,
                    .branches = .{ .span = .{ .start = branches_start, .len = branches_len } },
                    .exhaustive = exhaustive,
                },
            };
        },
        .expr_zero_argument_tag => {
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items.items[extra_start..];

            const closure_name = @as(Ident.Idx, @bitCast(extra_data[0]));
            const variant_var = @as(types.Var, @enumFromInt(extra_data[1]));
            const ext_var = @as(types.Var, @enumFromInt(extra_data[2]));
            const name = @as(Ident.Idx, @bitCast(extra_data[3]));

            return CIR.Expr{
                .e_zero_argument_tag = .{
                    .closure_name = closure_name,
                    .variant_var = variant_var,
                    .ext_var = ext_var,
                    .name = name,
                },
            };
        },
        .expr_crash => {
            return CIR.Expr{ .e_crash = .{
                .msg = @enumFromInt(node.data_1),
            } };
        },
        .expr_dbg => {
            return CIR.Expr{ .e_dbg = .{
                .expr = @enumFromInt(node.data_1),
            } };
        },
        .expr_unary_minus => {
            return CIR.Expr{ .e_unary_minus = .{
                .expr = @enumFromInt(node.data_1),
            } };
        },
        .expr_unary_not => {
            return CIR.Expr{ .e_unary_not = .{
                .expr = @enumFromInt(node.data_1),
            } };
        },
        .expr_static_dispatch,
        .expr_apply,
        .expr_record_update,
        .expr_suffix_single_question,
        .expr_record_builder,
        => {
            return CIR.Expr{ .e_runtime_error = .{
                .diagnostic = @enumFromInt(0),
            } };
        },
        .expr_ellipsis => {
            return CIR.Expr{ .e_ellipsis = .{} };
        },
        .expr_expect => {
            return CIR.Expr{ .e_expect = .{
                .body = @enumFromInt(node.data_1),
            } };
        },
        .expr_if_then_else => {
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items.items[extra_start..];

            const branches_span_start: u32 = extra_data[0];
            const branches_span_end: u32 = extra_data[1];
            const final_else: CIR.Expr.Idx = @enumFromInt(extra_data[2]);

            // Reconstruct the if expression from node data
            const branches_span = CIR.Expr.IfBranch.Span{ .span = .{
                .start = branches_span_start,
                .len = branches_span_end,
            } };

            return CIR.Expr{ .e_if = .{
                .branches = branches_span,
                .final_else = final_else,
            } };
        },
        .expr_dot_access => {
            const args_span = if (node.data_3 != 0) blk: {
                const packed_span = FunctionArgs.fromU32(node.data_3);
                const data_span = packed_span.toDataSpan();
                break :blk CIR.Expr.Span{ .span = data_span };
            } else null;

            return CIR.Expr{ .e_dot_access = .{
                .receiver = @enumFromInt(node.data_1),
                .field_name = @bitCast(node.data_2),
                .args = args_span,
            } };
        },
        .malformed => {
            return CIR.Expr{ .e_runtime_error = .{
                .diagnostic = @enumFromInt(node.data_1),
            } };
        },

        // NOTE: Diagnostic tags should NEVER appear in getExpr().
        // If compilation errors occur, use pushMalformed() to create .malformed nodes
        // that reference diagnostic indices. The .malformed case above handles
        // converting these to runtime_error nodes in the ModuleEnv.
        else => {
            @panic("unreachable, node is not an expression tag");
        },
    }
}

/// Get the more-specific expr index. Used to make error messages nicer.
///
/// For example, if the provided expr is a `block`, then this will return the
/// expr idx of the last expr in that block. This allows the error message to
/// reference the exact expr that has a problem, making the problem easier to
/// understand.
///
/// But for most exprs, this just returns the same expr idx provided.
pub fn getExprSpecific(store: *const NodeStore, expr_idx: CIR.Expr.Idx) CIR.Expr.Idx {
    const expr = store.getExpr(expr_idx);
    switch (expr) {
        .e_block => |block| return block.final_expr,
        else => return expr_idx,
    }
}

/// Retrieves a 'when' branch from the store.
pub fn getMatchBranch(store: *const NodeStore, branch: CIR.Expr.Match.Branch.Idx) CIR.Expr.Match.Branch {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(branch));
    const node = store.nodes.get(node_idx);

    std.debug.assert(node.tag == .match_branch);

    // Retrieve when branch data from extra_data
    const extra_start = node.data_1;
    const extra_data = store.extra_data.items.items[extra_start..];

    const patterns: CIR.Expr.Match.BranchPattern.Span = .{ .span = .{ .start = extra_data[0], .len = extra_data[1] } };
    const value_idx: CIR.Expr.Idx = @enumFromInt(extra_data[2]);
    const guard_idx: ?CIR.Expr.Idx = if (extra_data[3] == 0) null else @enumFromInt(extra_data[3]);
    const redundant: types.Var = @enumFromInt(extra_data[4]);

    return CIR.Expr.Match.Branch{
        .patterns = patterns,
        .value = value_idx,
        .guard = guard_idx,
        .redundant = redundant,
    };
}

/// Retrieves a pattern of a 'match' branch from the store.
pub fn getMatchBranchPattern(store: *const NodeStore, branch_pat: CIR.Expr.Match.BranchPattern.Idx) CIR.Expr.Match.BranchPattern {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(branch_pat));
    const node = store.nodes.get(node_idx);

    std.debug.assert(node.tag == .match_branch_pattern);

    return CIR.Expr.Match.BranchPattern{
        .pattern = @enumFromInt(node.data_1),
        .degenerate = node.data_2 != 0,
    };
}

/// Returns a slice of match branches from the given span.
pub fn matchBranchSlice(store: *const NodeStore, span: CIR.Expr.Match.Branch.Span) []CIR.Expr.Match.Branch.Idx {
    const slice = store.extra_data.items.items[span.span.start..(span.span.start + span.span.len)];
    const result: []CIR.Expr.Match.Branch.Idx = @ptrCast(@alignCast(slice));
    return result;
}

/// Retrieves a 'where' clause from the store.
pub fn getWhereClause(store: *const NodeStore, whereClause: CIR.WhereClause.Idx) CIR.WhereClause {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(whereClause));
    const node = store.nodes.get(node_idx);

    std.debug.assert(node.tag == .where_clause);

    // Retrieve where clause data from extra_data
    const extra_start = node.data_1;
    const extra_data = store.extra_data.items.items[extra_start..];

    const discriminant = extra_data[0];

    switch (discriminant) {
        0 => { // mod_method
            const var_name = @as(Ident.Idx, @bitCast(extra_data[1]));
            const method_name = @as(Ident.Idx, @bitCast(extra_data[2]));
            const args_start = extra_data[3];
            const args_len = extra_data[4];
            const ret_anno = @as(CIR.TypeAnno.Idx, @enumFromInt(extra_data[5]));
            const external_decl = @as(CIR.ExternalDecl.Idx, @enumFromInt(extra_data[6]));

            return CIR.WhereClause{
                .mod_method = .{
                    .var_name = var_name,
                    .method_name = method_name,
                    .args = .{ .span = .{ .start = args_start, .len = args_len } },
                    .ret_anno = ret_anno,
                    .external_decl = external_decl,
                },
            };
        },
        1 => { // mod_alias
            const var_name = @as(Ident.Idx, @bitCast(extra_data[1]));
            const alias_name = @as(Ident.Idx, @bitCast(extra_data[2]));
            const external_decl = @as(CIR.ExternalDecl.Idx, @enumFromInt(extra_data[3]));

            return CIR.WhereClause{
                .mod_alias = .{
                    .var_name = var_name,
                    .alias_name = alias_name,
                    .external_decl = external_decl,
                },
            };
        },
        2 => { // malformed
            const diagnostic = @as(CIR.Diagnostic.Idx, @enumFromInt(extra_data[1]));

            return CIR.WhereClause{
                .malformed = .{
                    .diagnostic = diagnostic,
                },
            };
        },
        else => @panic("Invalid where clause discriminant"),
    }
}

/// Retrieves a pattern from the store.
pub fn getPattern(store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) CIR.Pattern {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));
    const node = store.nodes.get(node_idx);

    switch (node.tag) {
        .pattern_identifier => return CIR.Pattern{
            .assign = .{
                .ident = @bitCast(node.data_1),
            },
        },
        .pattern_as => return CIR.Pattern{
            .as = .{
                .ident = @bitCast(node.data_1),
                .pattern = @enumFromInt(node.data_2),
            },
        },
        .pattern_applied_tag => {
            const arguments_start = node.data_1;
            const arguments_len = node.data_2;
            const tag_name = @as(Ident.Idx, @bitCast(node.data_3));
            return CIR.Pattern{
                .applied_tag = .{
                    .args = DataSpan.init(arguments_start, arguments_len).as(CIR.Pattern.Span),
                    .name = tag_name,
                },
            };
        },
        .pattern_nominal => {
            const nominal_type_decl: CIR.Statement.Idx = @enumFromInt(node.data_1);
            const backing_pattern: CIR.Pattern.Idx = @enumFromInt(node.data_2);
            const backing_type: CIR.Expr.NominalBackingType = @enumFromInt(node.data_3);
            return CIR.Pattern{
                .nominal = .{
                    .nominal_type_decl = nominal_type_decl,
                    .backing_pattern = backing_pattern,
                    .backing_type = backing_type,
                },
            };
        },
        .pattern_nominal_external => {
            const module_idx: CIR.Import.Idx = @enumFromInt(node.data_1);
            const target_node_idx: u16 = @intCast(node.data_2);

            const extra_data_idx = node.data_3;
            const extra_data = store.extra_data.items.items[extra_data_idx..][0..2];
            const backing_pattern: CIR.Pattern.Idx = @enumFromInt(extra_data[0]);
            const backing_type: CIR.Expr.NominalBackingType = @enumFromInt(extra_data[1]);

            return CIR.Pattern{
                .nominal_external = .{
                    .module_idx = module_idx,
                    .target_node_idx = target_node_idx,
                    .backing_pattern = backing_pattern,
                    .backing_type = backing_type,
                },
            };
        },
        .pattern_record_destructure => {
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items.items[extra_start..];

            const destructs_start = extra_data[0];
            const destructs_len = extra_data[1];
            const ext_var = @as(types.Var, @enumFromInt(extra_data[2]));
            const whole_var = @as(types.Var, @enumFromInt(extra_data[3]));

            return CIR.Pattern{
                .record_destructure = .{
                    .destructs = DataSpan.init(destructs_start, destructs_len).as(CIR.Pattern.RecordDestruct.Span),
                    .ext_var = ext_var,
                    .whole_var = whole_var,
                },
            };
        },
        .pattern_list => {
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items.items[extra_start..];

            const patterns_start = extra_data[0];
            const patterns_len = extra_data[1];
            const elem_var = @as(types.Var, @enumFromInt(extra_data[2]));
            const list_var = @as(types.Var, @enumFromInt(extra_data[3]));

            // Load rest_info
            const has_rest_info = extra_data[4] != 0;
            const rest_info = if (has_rest_info) blk: {
                const rest_index = extra_data[5];
                const has_pattern = extra_data[6] != 0;
                const rest_pattern = if (has_pattern)
                    @as(CIR.Pattern.Idx, @enumFromInt(extra_data[7]))
                else
                    null;
                break :blk @as(@TypeOf(@as(CIR.Pattern, undefined).list.rest_info), .{
                    .index = rest_index,
                    .pattern = rest_pattern,
                });
            } else null;

            return CIR.Pattern{
                .list = .{
                    .patterns = DataSpan.init(patterns_start, patterns_len).as(CIR.Pattern.Span),
                    .elem_var = elem_var,
                    .list_var = list_var,
                    .rest_info = rest_info,
                },
            };
        },
        .pattern_tuple => return CIR.Pattern{
            .tuple = .{
                .patterns = DataSpan.init(node.data_1, node.data_2).as(CIR.Pattern.Span),
            },
        },
        .pattern_num_literal => {
            const extra_data_idx = node.data_1;
            const value_as_u32s = store.extra_data.items.items[extra_data_idx..][0..4];
            const value_as_i128: i128 = @bitCast(value_as_u32s.*);

            return CIR.Pattern{
                .int_literal = .{
                    .value = .{ .bytes = @bitCast(value_as_i128), .kind = .i128 },
                },
            };
        },
        .pattern_int_literal => {
            const extra_data_idx = node.data_1;
            const value_as_u32s = store.extra_data.items.items[extra_data_idx..][0..4];
            const value_as_i128: i128 = @bitCast(value_as_u32s.*);

            return CIR.Pattern{
                .int_literal = .{
                    .value = .{ .bytes = @bitCast(value_as_i128), .kind = .i128 },
                },
            };
        },
        .pattern_f32_literal => return CIR.Pattern{
            .frac_f32_literal = .{ .value = @bitCast(node.data_1) },
        },
        .pattern_f64_literal => {
            const lower: u32 = node.data_1;
            const upper: u32 = node.data_2;
            const raw: u64 = (@as(u64, upper) << 32) | @as(u64, lower);

            return CIR.Pattern{
                .frac_f64_literal = .{ .value = @bitCast(raw) },
            };
        },
        .pattern_dec_literal => {
            const extra_data_idx = node.data_1;
            const value_as_u32s = store.extra_data.items.items[extra_data_idx..][0..4];
            const value_as_i128: i128 = @bitCast(value_as_u32s.*);

            return CIR.Pattern{
                .dec_literal = .{
                    .value = RocDec{ .num = value_as_i128 },
                },
            };
        },
        .pattern_small_dec_literal => {
            // Unpack small dec data from data_1 and data_3
            // data_1: numerator (i16) stored as u32
            // data_3: denominator_power_of_ten (u8) in lower 8 bits
            const numerator: i16 = @intCast(@as(i32, @bitCast(node.data_1)));
            const denominator_power_of_ten: u8 = @intCast(node.data_3 & 0xFF);

            return CIR.Pattern{
                .small_dec_literal = .{
                    .numerator = numerator,
                    .denominator_power_of_ten = denominator_power_of_ten,
                },
            };
        },
        .pattern_str_literal => return CIR.Pattern{ .str_literal = .{
            .literal = @enumFromInt(node.data_1),
        } },

        .pattern_underscore => return CIR.Pattern{ .underscore = {} },
        .malformed => {
            return CIR.Pattern{ .runtime_error = .{
                .diagnostic = @enumFromInt(node.data_1),
            } };
        },
        else => {
            @panic("unreachable, node is not an pattern tag");
        },
    }
}

/// Retrieves a pattern record field from the store.
pub fn getPatternRecordField(store: *NodeStore, patternRecordField: CIR.PatternRecordField.Idx) CIR.PatternRecordField {
    _ = store;
    _ = patternRecordField;
    // Return empty placeholder since PatternRecordField has no fields yet
    return CIR.PatternRecordField{};
}

/// Retrieves a type annotation from the store.
pub fn getTypeAnno(store: *const NodeStore, typeAnno: CIR.TypeAnno.Idx) CIR.TypeAnno {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(typeAnno));
    const node = store.nodes.get(node_idx);

    switch (node.tag) {
        .ty_apply => return CIR.TypeAnno{ .apply = .{
            .symbol = @bitCast(node.data_1),
            .args = .{ .span = .{ .start = node.data_2, .len = node.data_3 } },
        } },
        .ty_apply_external => {
            const extra_data_idx = node.data_3;
            const args_start = store.extra_data.items.items[extra_data_idx];
            const args_len = store.extra_data.items.items[extra_data_idx + 1];
            return CIR.TypeAnno{ .apply_external = .{
                .module_idx = @enumFromInt(node.data_1),
                .target_node_idx = @intCast(node.data_2),
                .args = .{ .span = .{ .start = args_start, .len = args_len } },
            } };
        },
        .ty_var => return CIR.TypeAnno{ .ty_var = .{
            .name = @bitCast(node.data_1),
        } },
        .ty_underscore => return CIR.TypeAnno{ .underscore = {} },
        .ty_ident => return CIR.TypeAnno{ .ty = .{
            .symbol = @bitCast(node.data_1),
        } },
        .ty_tag_union => return CIR.TypeAnno{ .tag_union = .{
            .tags = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
            .ext = if (node.data_3 != 0) @enumFromInt(node.data_3) else null,
        } },
        .ty_tuple => return CIR.TypeAnno{ .tuple = .{
            .elems = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
        } },
        .ty_record => return CIR.TypeAnno{ .record = .{
            .fields = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
        } },
        .ty_fn => {
            const extra_data_idx = node.data_3;
            const effectful = store.extra_data.items.items[extra_data_idx] != 0;
            const ret: CIR.TypeAnno.Idx = @enumFromInt(store.extra_data.items.items[extra_data_idx + 1]);
            return CIR.TypeAnno{ .@"fn" = .{
                .args = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
                .ret = ret,
                .effectful = effectful,
            } };
        },
        .ty_parens => return CIR.TypeAnno{ .parens = .{
            .anno = @enumFromInt(node.data_1),
        } },
        .ty_lookup_external => return CIR.TypeAnno{
            .ty_lookup_external = .{
                .module_idx = @enumFromInt(node.data_1),
                .target_node_idx = @intCast(node.data_2),
            },
        },
        .ty_malformed => return CIR.TypeAnno{ .malformed = .{
            .diagnostic = @enumFromInt(node.data_1),
        } },
        .malformed => return CIR.TypeAnno{ .malformed = .{
            .diagnostic = @enumFromInt(node.data_1),
        } },
        else => {
            std.debug.panic("Invalid node tag for TypeAnno: {}", .{node.tag});
        },
    }
}

/// Retrieves a type header from the store.
pub fn getTypeHeader(store: *const NodeStore, typeHeader: CIR.TypeHeader.Idx) CIR.TypeHeader {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(typeHeader));
    const node = store.nodes.get(node_idx);

    std.debug.assert(node.tag == .type_header);

    return CIR.TypeHeader{
        .name = @bitCast(node.data_1),
        .args = .{ .span = .{ .start = node.data_2, .len = node.data_3 } },
    };
}

/// Retrieves an annotation record field from the store.
pub fn getAnnoRecordField(store: *const NodeStore, annoRecordField: CIR.TypeAnno.RecordField.Idx) CIR.TypeAnno.RecordField {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(annoRecordField));
    const node = store.nodes.get(node_idx);
    return .{
        .name = @bitCast(node.data_1),
        .ty = @enumFromInt(node.data_2),
    };
}

/// Retrieves an annotation from the store.
pub fn getAnnotation(store: *const NodeStore, annotation: CIR.Annotation.Idx) CIR.Annotation {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(annotation));
    const node = store.nodes.get(node_idx);

    std.debug.assert(node.tag == .annotation);

    return CIR.Annotation{
        .type_anno = @enumFromInt(node.data_2),
        .signature = @enumFromInt(node.data_1),
    };
}

/// Retrieves an exposed item from the store.
pub fn getExposedItem(store: *const NodeStore, exposedItem: CIR.ExposedItem.Idx) CIR.ExposedItem {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(exposedItem));
    const node = store.nodes.get(node_idx);

    switch (node.tag) {
        .exposed_item => {
            return CIR.ExposedItem{
                .name = @bitCast(node.data_1),
                .alias = if (node.data_2 == 0) null else @bitCast(node.data_2),
                .is_wildcard = node.data_3 != 0,
            };
        },
        else => std.debug.panic("Expected exposed_item node, got {s}\n", .{@tagName(node.tag)}),
    }
}

/// Adds a statement to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addStatement(store: *NodeStore, statement: CIR.Statement, region: base.Region) std.mem.Allocator.Error!CIR.Statement.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .tag = undefined,
    };

    switch (statement) {
        .s_decl => |s| {
            node.tag = .statement_decl;
            node.data_1 = @intFromEnum(s.pattern);
            node.data_2 = @intFromEnum(s.expr);
            node.data_3 = if (s.annotation) |a| @intFromEnum(a) else 0;
        },
        .s_var => |s| {
            node.tag = .statement_var;
            node.data_1 = @intFromEnum(s.pattern_idx);
            node.data_2 = @intFromEnum(s.expr);
        },
        .s_reassign => |s| {
            node.tag = .statement_reassign;
            node.data_1 = @intFromEnum(s.pattern_idx);
            node.data_2 = @intFromEnum(s.expr);
        },
        .s_crash => |s| {
            node.tag = .statement_crash;
            node.data_1 = @intFromEnum(s.msg);
        },
        .s_dbg => |s| {
            node.tag = .statement_dbg;
            node.data_1 = @intFromEnum(s.expr);
        },
        .s_expr => |s| {
            node.tag = .statement_expr;
            node.data_1 = @intFromEnum(s.expr);
        },
        .s_expect => |s| {
            node.tag = .statement_expect;
            node.data_1 = @intFromEnum(s.body);
        },
        .s_for => |s| {
            node.tag = .statement_for;
            node.data_1 = @intFromEnum(s.patt);
            node.data_2 = @intFromEnum(s.expr);
            node.data_3 = @intFromEnum(s.body);
        },
        .s_return => |s| {
            node.tag = .statement_return;
            node.data_1 = @intFromEnum(s.expr);
        },
        .s_import => |s| {
            node.tag = .statement_import;
            node.data_1 = @bitCast(s.module_name_tok);

            // Store optional fields in extra_data
            const extra_start = store.extra_data.len();

            // Store alias_tok (nullable)
            const alias_data = if (s.alias_tok) |alias| @as(u32, @bitCast(alias)) else 0;
            _ = try store.extra_data.append(store.gpa, alias_data);

            // Store qualifier_tok (nullable)
            const qualifier_data = if (s.qualifier_tok) |qualifier| @as(u32, @bitCast(qualifier)) else 0;
            _ = try store.extra_data.append(store.gpa, qualifier_data);

            // Store flags indicating which fields are present
            var flags: u32 = 0;
            if (s.alias_tok != null) flags |= 1;
            if (s.qualifier_tok != null) flags |= 2;
            _ = try store.extra_data.append(store.gpa, flags);

            // Store extra_start in one of the remaining data fields
            // We need to reorganize data storage since all 3 data fields are used
            // Let's put extra_start where exposes span is, and move span to extra_data
            _ = try store.extra_data.append(store.gpa, s.exposes.span.start);
            _ = try store.extra_data.append(store.gpa, s.exposes.span.len);

            node.data_2 = @intCast(extra_start); // Point to extra_data
            node.data_3 = 0; // SPARE

        },
        .s_alias_decl => |s| {
            node.tag = .statement_alias_decl;
            node.data_1 = @intFromEnum(s.header);
            node.data_2 = @intFromEnum(s.anno);
        },
        .s_nominal_decl => |s| {
            node.tag = .statement_nominal_decl;
            node.data_1 = @intFromEnum(s.header);
            node.data_2 = @intFromEnum(s.anno);
        },
        .s_type_anno => |s| {
            node.tag = .statement_type_anno;

            // Store type_anno data in extra_data
            const extra_start = store.extra_data.len();

            // Store anno idx
            _ = try store.extra_data.append(store.gpa, @intFromEnum(s.anno));
            // Store name
            _ = try store.extra_data.append(store.gpa, @bitCast(s.name));
            // Store where clause information
            if (s.where) |where_clause| {
                // Store flag indicating where clause is present
                _ = try store.extra_data.append(store.gpa, 1);
                // Store where clause span start and len
                _ = try store.extra_data.append(store.gpa, where_clause.span.start);
                _ = try store.extra_data.append(store.gpa, where_clause.span.len);
            } else {
                // Store flag indicating where clause is not present
                _ = try store.extra_data.append(store.gpa, 0);
            }

            // Store the extra data start position in the node
            node.data_1 = @intCast(extra_start);
        },
    }

    const node_idx = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Adds an expression node to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addExpr(store: *NodeStore, expr: CIR.Expr, region: base.Region) std.mem.Allocator.Error!CIR.Expr.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .tag = @enumFromInt(0),
    };

    switch (expr) {
        .e_lookup_local => |local| {
            node.tag = .expr_var;
            node.data_1 = @intFromEnum(local.pattern_idx);
        },
        .e_lookup_external => |e| {
            // For external lookups, store the module index and target node index
            node.tag = .expr_external_lookup;
            node.data_1 = @intFromEnum(e.module_idx);
            node.data_2 = e.target_node_idx;
        },
        .e_int => |e| {
            node.tag = .expr_int;

            // Store i128 value in extra_data
            const extra_data_start = store.extra_data.len();

            // Store the IntLiteralValue as i128 (16 bytes = 4 u32s)
            // We always store as i128 internally
            const value_as_i128: i128 = @bitCast(e.value.bytes);
            const value_as_u32s: [4]u32 = @bitCast(value_as_i128);
            for (value_as_u32s) |word| {
                _ = try store.extra_data.append(store.gpa, word);
            }

            // Store the extra_data index in data_1
            node.data_1 = @intCast(extra_data_start);
        },
        .e_list => |e| {
            node.tag = .expr_list;
            node.data_1 = e.elems.span.start;
            node.data_2 = e.elems.span.len;
            node.data_3 = @intFromEnum(e.elem_var);
        },
        .e_empty_list => |_| {
            node.tag = .expr_empty_list;
        },
        .e_tuple => |e| {
            node.tag = .expr_tuple;
            node.data_1 = e.elems.span.start;
            node.data_2 = e.elems.span.len;
        },
        .e_frac_f32 => |e| {
            node.tag = Node.Tag.expr_frac_f32;
            node.data_1 = @bitCast(e.value);
        },
        .e_frac_f64 => |e| {
            node.tag = .expr_frac_f64;
            const raw: [2]u32 = @bitCast(e.value);
            node.data_1 = raw[0];
            node.data_2 = raw[1];
        },
        .e_frac_dec => |e| {
            node.tag = .expr_frac_dec;

            // Store the RocDec value in extra_data
            const extra_data_start = store.extra_data.len();
            const value_as_i128: i128 = e.value.num;
            const value_as_u32s: [4]u32 = @bitCast(value_as_i128);
            for (value_as_u32s) |word| {
                _ = try store.extra_data.append(store.gpa, word);
            }

            // Store the extra_data index in data_1
            node.data_1 = @intCast(extra_data_start);
        },
        .e_dec_small => |e| {
            node.tag = .expr_dec_small;

            // Pack small dec data into data_1 and data_3
            // data_1: numerator (i16) - fits in lower 16 bits
            // data_3: denominator_power_of_ten (u8) in lower 8 bits
            node.data_1 = @as(u32, @bitCast(@as(i32, e.numerator)));
            node.data_3 = @as(u32, e.denominator_power_of_ten);
        },
        .e_str_segment => |e| {
            node.tag = .expr_string_segment;
            node.data_1 = @intFromEnum(e.literal);
        },
        .e_str => |e| {
            node.tag = .expr_string;
            node.data_1 = e.span.span.start;
            node.data_2 = e.span.span.len;
        },
        .e_tag => |e| {
            node.tag = .expr_tag;
            node.data_1 = @bitCast(e.name);
            node.data_2 = e.args.span.start;
            node.data_3 = e.args.span.len;
        },
        .e_nominal => |e| {
            node.tag = .expr_nominal;
            node.data_1 = @intFromEnum(e.nominal_type_decl);
            node.data_2 = @intFromEnum(e.backing_expr);
            node.data_3 = @intFromEnum(e.backing_type);
        },
        .e_nominal_external => |e| {
            node.tag = .expr_nominal_external;
            node.data_1 = @intFromEnum(e.module_idx);
            node.data_2 = @intCast(e.target_node_idx);
            node.data_3 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.backing_expr));
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.backing_type));
        },
        .e_dot_access => |e| {
            node.tag = .expr_dot_access;
            node.data_1 = @intFromEnum(e.receiver);
            node.data_2 = @bitCast(e.field_name);
            if (e.args) |args| {
                // Use PackedDataSpan for efficient storage - FunctionArgs config is good for method call args
                std.debug.assert(FunctionArgs.canFit(args.span));
                const packed_span = FunctionArgs.fromDataSpanUnchecked(args.span);
                node.data_3 = packed_span.toU32();
            } else {
                node.data_3 = 0; // No args
            }
        },
        .e_runtime_error => |e| {
            node.data_1 = @intFromEnum(e.diagnostic);
            node.tag = .malformed;
        },
        .e_crash => |c| {
            node.tag = .expr_crash;
            node.data_1 = @intFromEnum(c.msg);
        },
        .e_dbg => |d| {
            node.tag = .expr_dbg;
            node.data_1 = @intFromEnum(d.expr);
        },
        .e_ellipsis => |_| {
            node.tag = .expr_ellipsis;
        },
        .e_match => |e| {
            node.tag = .expr_match;

            // Store when data in extra_data
            const extra_data_start = store.extra_data.len();
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.cond));
            _ = try store.extra_data.append(store.gpa, e.branches.span.start);
            _ = try store.extra_data.append(store.gpa, e.branches.span.len);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.exhaustive));
            node.data_1 = @intCast(extra_data_start);
        },
        .e_if => |e| {
            // Store def data in extra_data. We store the following fields:
            // 1. Branches span start
            // 2. Branches span end
            // 3. Final else expr idx
            const extra_start = store.extra_data.len();
            const num_extra_items = 3;
            _ = try store.extra_data.append(store.gpa, e.branches.span.start);
            _ = try store.extra_data.append(store.gpa, e.branches.span.len);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.final_else));

            node.tag = .expr_if_then_else;
            node.data_1 = @intCast(extra_start);
            node.data_2 = @intCast(extra_start + num_extra_items);

            std.debug.assert(node.data_2 == store.extra_data.len());
        },
        .e_call => |e| {
            node.tag = .expr_call;

            // Store call data in extra_data
            const extra_data_start = store.extra_data.len();

            // Store args span start
            _ = try store.extra_data.append(store.gpa, e.args.span.start);
            // Store args span length
            _ = try store.extra_data.append(store.gpa, e.args.span.len);

            node.data_1 = @intCast(extra_data_start);
            node.data_2 = @intFromEnum(e.called_via);
        },
        .e_record => |e| {
            node.tag = .expr_record;

            const extra_data_start = store.extra_data.len();

            // Store fields span start
            _ = try store.extra_data.append(store.gpa, e.fields.span.start);
            // Store fields span length
            _ = try store.extra_data.append(store.gpa, e.fields.span.len);
            // Store extension (0 if null)
            const ext_value = if (e.ext) |ext| @intFromEnum(ext) else 0;
            _ = try store.extra_data.append(store.gpa, ext_value);

            node.data_1 = @intCast(extra_data_start);
            node.data_2 = 0; // Unused
        },
        .e_empty_record => |_| {
            node.tag = .expr_empty_record;
        },
        .e_zero_argument_tag => |e| {
            node.tag = .expr_zero_argument_tag;

            // Store zero argument tag data in extra_data
            const extra_data_start = store.extra_data.len();
            _ = try store.extra_data.append(store.gpa, @bitCast(e.closure_name));
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.variant_var));
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.ext_var));
            _ = try store.extra_data.append(store.gpa, @bitCast(e.name));
            node.data_1 = @intCast(extra_data_start);
        },
        .e_closure => |e| {
            node.tag = .expr_closure;

            const extra_data_start = store.extra_data.len();

            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.lambda_idx));
            _ = try store.extra_data.append(store.gpa, e.captures.span.start);
            _ = try store.extra_data.append(store.gpa, e.captures.span.len);

            node.data_1 = @intCast(extra_data_start);
        },
        .e_lambda => |e| {
            node.tag = .expr_lambda;

            // Store lambda data in extra_data
            const extra_data_start = store.extra_data.len();

            // Store args span start
            _ = try store.extra_data.append(store.gpa, e.args.span.start);
            // Store args span length
            _ = try store.extra_data.append(store.gpa, e.args.span.len);
            // Store body index
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.body));

            node.data_1 = @intCast(extra_data_start);
        },
        .e_binop => |e| {
            node.tag = .expr_bin_op;
            node.data_1 = @intFromEnum(e.op);
            node.data_2 = @intFromEnum(e.lhs);
            node.data_3 = @intFromEnum(e.rhs);
        },
        .e_unary_minus => |e| {
            node.tag = .expr_unary_minus;
            node.data_1 = @intFromEnum(e.expr);
        },
        .e_unary_not => |e| {
            node.tag = .expr_unary_not;
            node.data_1 = @intFromEnum(e.expr);
        },
        .e_block => |e| {
            node.tag = .expr_block;
            node.data_1 = e.stmts.span.start;
            node.data_2 = e.stmts.span.len;
            node.data_3 = @intFromEnum(e.final_expr);
        },
        .e_expect => |e| {
            node.tag = .expr_expect;
            node.data_1 = @intFromEnum(e.body);
        },
    }

    const node_idx = try store.nodes.append(store.gpa, node);
    // For e_lookup_external, use the region from the expression itself
    const actual_region = switch (expr) {
        .e_lookup_external => |e| e.region,
        else => region,
    };
    _ = try store.regions.append(store.gpa, actual_region);
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Adds a record field to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addRecordField(store: *NodeStore, recordField: CIR.RecordField, region: base.Region) std.mem.Allocator.Error!CIR.RecordField.Idx {
    const node = Node{
        .data_1 = @bitCast(recordField.name),
        .data_2 = @intFromEnum(recordField.value),
        .data_3 = 0,
        .tag = .record_field,
    };

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a record destructuring to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addRecordDestruct(store: *NodeStore, record_destruct: CIR.Pattern.RecordDestruct, region: base.Region) std.mem.Allocator.Error!CIR.Pattern.RecordDestruct.Idx {
    const extra_data_start = @as(u32, @intCast(store.extra_data.len()));
    const node = Node{
        .data_1 = @bitCast(record_destruct.label),
        .data_2 = @bitCast(record_destruct.ident),
        .data_3 = extra_data_start,
        .tag = .record_destruct,
    };

    // Store kind in extra_data
    switch (record_destruct.kind) {
        .Required => |pattern_idx| {
            // Store kind tag (0 for Required)
            _ = try store.extra_data.append(store.gpa, 0);
            // Store pattern index
            _ = try store.extra_data.append(store.gpa, @intFromEnum(pattern_idx));
        },
        .SubPattern => |sub_pattern| {
            // Store kind tag (1 for SubPattern)
            _ = try store.extra_data.append(store.gpa, 1);
            // Store sub-pattern index
            _ = try store.extra_data.append(store.gpa, @intFromEnum(sub_pattern));
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a capture to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addCapture(store: *NodeStore, capture: CIR.Expr.Capture, region: base.Region) std.mem.Allocator.Error!CIR.Expr.Capture.Idx {
    const node = Node{
        .tag = .lambda_capture,
        .data_1 = @bitCast(capture.name),
        .data_2 = capture.scope_depth,
        .data_3 = @intFromEnum(capture.pattern_idx),
    };

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a 'match' branch to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addMatchBranch(store: *NodeStore, branch: CIR.Expr.Match.Branch, region: base.Region) std.mem.Allocator.Error!CIR.Expr.Match.Branch.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .tag = .match_branch,
    };

    // Store when branch data in extra_data
    const extra_data_start = store.extra_data.len();
    _ = try store.extra_data.append(store.gpa, branch.patterns.span.start);
    _ = try store.extra_data.append(store.gpa, branch.patterns.span.len);
    _ = try store.extra_data.append(store.gpa, @intFromEnum(branch.value));
    const guard_idx = if (branch.guard) |g| @intFromEnum(g) else 0;
    _ = try store.extra_data.append(store.gpa, guard_idx);
    _ = try store.extra_data.append(store.gpa, @intFromEnum(branch.redundant));
    node.data_1 = @intCast(extra_data_start);

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a 'match' branch to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addMatchBranchPattern(store: *NodeStore, branchPattern: CIR.Expr.Match.BranchPattern, region: base.Region) std.mem.Allocator.Error!CIR.Expr.Match.BranchPattern.Idx {
    const node = Node{
        .data_1 = @intFromEnum(branchPattern.pattern),
        .data_2 = @as(u32, @intFromBool(branchPattern.degenerate)),
        .data_3 = 0,
        .tag = .match_branch_pattern,
    };
    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a 'where' clause to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addWhereClause(store: *NodeStore, whereClause: CIR.WhereClause, region: base.Region) std.mem.Allocator.Error!CIR.WhereClause.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .tag = .where_clause,
    };

    // Store where clause data in extra_data
    const extra_data_start = store.extra_data.len();

    switch (whereClause) {
        .mod_method => |mod_method| {
            // Store discriminant (0 for mod_method)
            _ = try store.extra_data.append(store.gpa, 0);
            _ = try store.extra_data.append(store.gpa, @bitCast(mod_method.var_name));
            _ = try store.extra_data.append(store.gpa, @bitCast(mod_method.method_name));
            _ = try store.extra_data.append(store.gpa, mod_method.args.span.start);
            _ = try store.extra_data.append(store.gpa, mod_method.args.span.len);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(mod_method.ret_anno));
            _ = try store.extra_data.append(store.gpa, @intFromEnum(mod_method.external_decl));
        },
        .mod_alias => |mod_alias| {
            // Store discriminant (1 for mod_alias)
            _ = try store.extra_data.append(store.gpa, 1);
            _ = try store.extra_data.append(store.gpa, @bitCast(mod_alias.var_name));
            _ = try store.extra_data.append(store.gpa, @bitCast(mod_alias.alias_name));
            _ = try store.extra_data.append(store.gpa, @intFromEnum(mod_alias.external_decl));
        },
        .malformed => |malformed| {
            // Store discriminant (2 for malformed)
            _ = try store.extra_data.append(store.gpa, 2);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(malformed.diagnostic));
        },
    }

    node.data_1 = @intCast(extra_data_start);

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a pattern to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addPattern(store: *NodeStore, pattern: CIR.Pattern, region: base.Region) std.mem.Allocator.Error!CIR.Pattern.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .tag = @enumFromInt(0),
    };

    switch (pattern) {
        .assign => |p| {
            node.data_1 = @bitCast(p.ident);
            node.tag = .pattern_identifier;
        },
        .as => |p| {
            node.tag = .pattern_as;
            node.data_1 = @bitCast(p.ident);
            node.data_2 = @intFromEnum(p.pattern);
        },
        .applied_tag => |p| {
            node.tag = .pattern_applied_tag;
            node.data_1 = p.args.span.start;
            node.data_2 = p.args.span.len;
            node.data_3 = @bitCast(p.name);
        },
        .nominal => |n| {
            node.tag = .pattern_nominal;
            node.data_1 = @intFromEnum(n.nominal_type_decl);
            node.data_2 = @intFromEnum(n.backing_pattern);
            node.data_3 = @intFromEnum(n.backing_type);
        },
        .nominal_external => |n| {
            node.tag = .pattern_nominal_external;
            node.data_1 = @intFromEnum(n.module_idx);
            node.data_2 = @intCast(n.target_node_idx);
            node.data_3 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, @intFromEnum(n.backing_pattern));
            _ = try store.extra_data.append(store.gpa, @intFromEnum(n.backing_type));
        },
        .record_destructure => |p| {
            node.tag = .pattern_record_destructure;

            // Store record destructure data in extra_data
            const extra_data_start = store.extra_data.len();
            _ = try store.extra_data.append(store.gpa, p.destructs.span.start);
            _ = try store.extra_data.append(store.gpa, p.destructs.span.len);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(p.ext_var));
            _ = try store.extra_data.append(store.gpa, @intFromEnum(p.whole_var));
            node.data_1 = @intCast(extra_data_start);
        },
        .list => |p| {
            node.tag = .pattern_list;

            // Store list pattern data in extra_data
            const extra_data_start = store.extra_data.len();
            _ = try store.extra_data.append(store.gpa, p.patterns.span.start);
            _ = try store.extra_data.append(store.gpa, p.patterns.span.len);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(p.elem_var));
            _ = try store.extra_data.append(store.gpa, @intFromEnum(p.list_var));

            // Store rest_info
            if (p.rest_info) |rest| {
                _ = try store.extra_data.append(store.gpa, 1); // has rest_info
                _ = try store.extra_data.append(store.gpa, rest.index);
                if (rest.pattern) |pattern_idx| {
                    _ = try store.extra_data.append(store.gpa, 1); // has pattern
                    _ = try store.extra_data.append(store.gpa, @intFromEnum(pattern_idx));
                } else {
                    _ = try store.extra_data.append(store.gpa, 0); // no pattern
                }
            } else {
                _ = try store.extra_data.append(store.gpa, 0); // no rest_info
            }

            node.data_1 = @intCast(extra_data_start);
        },
        .tuple => |p| {
            node.tag = .pattern_tuple;
            node.data_1 = p.patterns.span.start;
            node.data_2 = p.patterns.span.len;
        },
        .int_literal => |p| {
            node.tag = .pattern_int_literal;
            // Store the value in extra_data
            const extra_data_start = store.extra_data.len();
            const value_as_u32s: [4]u32 = @bitCast(p.value.bytes);
            for (value_as_u32s) |word| {
                _ = try store.extra_data.append(store.gpa, word);
            }
            node.data_1 = @intCast(extra_data_start);
        },
        .small_dec_literal => |p| {
            node.tag = .pattern_small_dec_literal;
            // Pack small dec data into data_1 and data_3
            // data_1: numerator (i16) - fits in lower 16 bits
            // data_3: denominator_power_of_ten (u8) in lower 8 bits
            node.data_1 = @as(u32, @bitCast(@as(i32, p.numerator)));
            node.data_3 = @as(u32, p.denominator_power_of_ten);
        },
        .dec_literal => |p| {
            node.tag = .pattern_dec_literal;
            // Store the RocDec value in extra_data
            const extra_data_start = store.extra_data.len();
            const value_as_u32s: [4]u32 = @bitCast(p.value.num);
            for (value_as_u32s) |word| {
                _ = try store.extra_data.append(store.gpa, word);
            }
            node.data_1 = @intCast(extra_data_start);
        },
        .str_literal => |p| {
            node.tag = .pattern_str_literal;
            node.data_1 = @intFromEnum(p.literal);
        },
        .frac_f32_literal => |p| {
            node.tag = Node.Tag.pattern_f32_literal;
            node.data_1 = @bitCast(p.value);
        },
        .frac_f64_literal => |p| {
            node.tag = Node.Tag.pattern_f64_literal;
            const raw: u64 = @bitCast(p.value);
            const lower: u32 = @intCast(raw & 0xFFFFFFFF);
            const upper: u32 = @intCast(raw >> 32);
            node.data_1 = lower;
            node.data_2 = upper;
        },
        .underscore => {
            node.tag = .pattern_underscore;
        },
        .runtime_error => |e| {
            node.tag = .malformed;
            node.data_1 = @intFromEnum(e.diagnostic);
        },
    }

    const node_idx = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Adds a pattern record field to the store.
pub fn addPatternRecordField(store: *NodeStore, patternRecordField: CIR.PatternRecordField) std.mem.Allocator.Error!CIR.PatternRecordField.Idx {
    _ = store;
    _ = patternRecordField;

    return @enumFromInt(0);
}

/// Adds a type annotation to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addTypeAnno(store: *NodeStore, typeAnno: CIR.TypeAnno, region: base.Region) std.mem.Allocator.Error!CIR.TypeAnno.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .tag = @enumFromInt(0),
    };

    switch (typeAnno) {
        .apply => |a| {
            node.data_1 = @bitCast(a.symbol);
            node.data_2 = a.args.span.start;
            node.data_3 = a.args.span.len;
            node.tag = .ty_apply;
        },
        .apply_external => |a| {
            node.data_1 = @intFromEnum(a.module_idx);
            node.data_2 = a.target_node_idx;
            const ed_start = store.extra_data.len();
            _ = try store.extra_data.append(store.gpa, a.args.span.start);
            _ = try store.extra_data.append(store.gpa, a.args.span.len);
            node.data_3 = @intCast(ed_start);
            node.tag = .ty_apply_external;
        },
        .ty_var => |tv| {
            node.data_1 = @bitCast(tv.name);
            node.tag = .ty_var;
        },
        .underscore => |_| {
            node.tag = .ty_underscore;
        },
        .ty => |t| {
            node.data_1 = @bitCast(t.symbol);
            node.tag = .ty_ident;
        },
        .tag_union => |tu| {
            node.data_1 = tu.tags.span.start;
            node.data_2 = tu.tags.span.len;
            node.data_3 = if (tu.ext) |ext| @intFromEnum(ext) else 0;
            node.tag = .ty_tag_union;
        },
        .tuple => |t| {
            node.data_1 = t.elems.span.start;
            node.data_2 = t.elems.span.len;
            node.tag = .ty_tuple;
        },
        .record => |r| {
            node.data_1 = r.fields.span.start;
            node.data_2 = r.fields.span.len;
            node.tag = .ty_record;
        },
        .@"fn" => |f| {
            node.data_1 = f.args.span.start;
            node.data_2 = f.args.span.len;
            const ed_start = store.extra_data.len();
            _ = try store.extra_data.append(store.gpa, if (f.effectful) @as(u32, 1) else 0);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(f.ret));
            node.data_3 = @intCast(ed_start);
            node.tag = .ty_fn;
        },
        .parens => |p| {
            node.data_1 = @intFromEnum(p.anno);
            node.tag = .ty_parens;
        },
        .ty_lookup_external => |tle| {
            node.tag = .ty_lookup_external;
            node.data_1 = @intFromEnum(tle.module_idx);
            node.data_2 = tle.target_node_idx;
        },
        .malformed => |m| {
            node.data_1 = @intFromEnum(m.diagnostic);
            node.tag = .ty_malformed;
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a type header to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addTypeHeader(store: *NodeStore, typeHeader: CIR.TypeHeader, region: base.Region) std.mem.Allocator.Error!CIR.TypeHeader.Idx {
    const node = Node{
        .data_1 = @bitCast(typeHeader.name),
        .data_2 = typeHeader.args.span.start,
        .data_3 = typeHeader.args.span.len,
        .tag = .type_header,
    };

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an annotation record field to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addAnnoRecordField(store: *NodeStore, annoRecordField: CIR.TypeAnno.RecordField, region: base.Region) std.mem.Allocator.Error!CIR.TypeAnno.RecordField.Idx {
    const node = Node{
        .data_1 = @bitCast(annoRecordField.name),
        .data_2 = @intFromEnum(annoRecordField.ty),
        .data_3 = 0,
        .tag = .ty_record_field,
    };

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an annotation to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addAnnotation(store: *NodeStore, annotation: CIR.Annotation, region: base.Region) std.mem.Allocator.Error!CIR.Annotation.Idx {
    const node = Node{
        .data_1 = @intFromEnum(annotation.signature),
        .data_2 = @intFromEnum(annotation.type_anno),
        .data_3 = 0,
        .tag = .annotation,
    };

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an exposed item to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addExposedItem(store: *NodeStore, exposedItem: CIR.ExposedItem, region: base.Region) std.mem.Allocator.Error!CIR.ExposedItem.Idx {
    const node = Node{
        .data_1 = @bitCast(exposedItem.name),
        .data_2 = if (exposedItem.alias) |alias| @bitCast(alias) else 0,
        .data_3 = @intFromBool(exposedItem.is_wildcard),
        .tag = .exposed_item,
    };

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a definition to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addDef(store: *NodeStore, def: CIR.Def, region: base.Region) std.mem.Allocator.Error!CIR.Def.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .tag = .def,
    };

    // Store def data in extra_data
    const extra_start = store.extra_data.len();

    // Store pattern idx
    _ = try store.extra_data.append(store.gpa, @intFromEnum(def.pattern));
    // Store expr idx
    _ = try store.extra_data.append(store.gpa, @intFromEnum(def.expr));
    // Store kind tag as two u32's
    const kind_encoded = def.kind.encode();
    _ = try store.extra_data.append(store.gpa, kind_encoded[0]);
    _ = try store.extra_data.append(store.gpa, kind_encoded[1]);
    // Store annotation idx (0 if null)
    const anno_idx = if (def.annotation) |anno| @intFromEnum(anno) else 0;
    _ = try store.extra_data.append(store.gpa, anno_idx);

    // Store the extra data range in the node
    node.data_1 = @intCast(extra_start);
    node.data_2 = 5; // Number of extra data items

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Retrieves a definition from the store.
pub fn getDef(store: *const NodeStore, def_idx: CIR.Def.Idx) CIR.Def {
    const nid: Node.Idx = @enumFromInt(@intFromEnum(def_idx));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .def);

    const extra_start = node.data_1;
    const extra_data = store.extra_data.items.items[extra_start..];

    const pattern: CIR.Pattern.Idx = @enumFromInt(extra_data[0]);
    const expr: CIR.Expr.Idx = @enumFromInt(extra_data[1]);
    const kind_encoded = [_]u32{ extra_data[2], extra_data[3] };
    const kind = CIR.Def.Kind.decode(kind_encoded);
    const anno_idx = extra_data[4];
    const annotation = if (anno_idx == 0) null else @as(CIR.Annotation.Idx, @enumFromInt(anno_idx));

    return CIR.Def{
        .pattern = pattern,
        .expr = expr,
        .annotation = annotation,
        .kind = kind,
    };
}

/// Retrieves a capture from the store.
pub fn getCapture(store: *const NodeStore, capture_idx: CIR.Expr.Capture.Idx) CIR.Expr.Capture {
    const nid: Node.Idx = @enumFromInt(@intFromEnum(capture_idx));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .lambda_capture);

    return CIR.Expr.Capture{
        .name = @bitCast(node.data_1),
        .scope_depth = node.data_2,
        .pattern_idx = @enumFromInt(node.data_3),
    };
}

/// Retrieves a record field from the store.
pub fn getRecordField(store: *const NodeStore, idx: CIR.RecordField.Idx) CIR.RecordField {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    return CIR.RecordField{
        .name = @bitCast(node.data_1),
        .value = @enumFromInt(node.data_2),
    };
}

/// Retrieves a record destructure from the store.
pub fn getRecordDestruct(store: *const NodeStore, idx: CIR.Pattern.RecordDestruct.Idx) CIR.Pattern.RecordDestruct {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
    const node = store.nodes.get(node_idx);

    std.debug.assert(node.tag == .record_destruct);

    // Retrieve kind from extra_data if it exists
    const kind = blk: {
        const extra_start = node.data_3;
        const extra_data = store.extra_data.items.items[extra_start..][0..2];
        const kind_tag = extra_data[0];

        break :blk switch (kind_tag) {
            0 => CIR.Pattern.RecordDestruct.Kind{ .Required = @enumFromInt(extra_data[1]) },
            1 => CIR.Pattern.RecordDestruct.Kind{ .SubPattern = @enumFromInt(extra_data[1]) },
            else => unreachable,
        };
    };

    return CIR.Pattern.RecordDestruct{
        .label = @bitCast(node.data_1),
        .ident = @bitCast(node.data_2),
        .kind = kind,
    };
}

/// Retrieves an if branch from the store.
pub fn getIfBranch(store: *const NodeStore, if_branch_idx: CIR.Expr.IfBranch.Idx) CIR.Expr.IfBranch {
    const nid: Node.Idx = @enumFromInt(@intFromEnum(if_branch_idx));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .if_branch);

    return CIR.Expr.IfBranch{
        .cond = @enumFromInt(node.data_1),
        .body = @enumFromInt(node.data_2),
    };
}

/// Generic function to get the top of any scratch buffer
pub fn scratchTop(store: *NodeStore, comptime field_name: []const u8) u32 {
    return @field(store, field_name).top();
}

/// Generic function to add an item to any scratch buffer
pub fn addScratch(store: *NodeStore, comptime field_name: []const u8, idx: anytype) std.mem.Allocator.Error!void {
    try @field(store, field_name).append(store.gpa, idx);
}

/// Generic function to clear any scratch buffer from a given position
pub fn clearScratchFrom(store: *NodeStore, comptime field_name: []const u8, start: u32) void {
    @field(store, field_name).clearFrom(start);
}

/// Generic function to create a span from any scratch buffer
pub fn spanFrom(store: *NodeStore, comptime field_name: []const u8, comptime SpanType: type, start: u32) std.mem.Allocator.Error!SpanType {
    const scratch_field = &@field(store, field_name);
    const end = scratch_field.top();
    defer scratch_field.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = store.extra_data.len();
    std.debug.assert(end >= i);
    while (i < end) {
        _ = try store.extra_data.append(store.gpa, @intFromEnum(scratch_field.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = @intCast(ed_start), .len = @as(u32, @intCast(end)) - start } };
}

/// Returns the top of the scratch expressions buffer.
pub fn scratchExprTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_exprs");
}

/// Adds a scratch expression to temporary storage.
pub fn addScratchExpr(store: *NodeStore, idx: CIR.Expr.Idx) std.mem.Allocator.Error!void {
    try store.addScratch("scratch_exprs", idx);
}

/// Adds a capture index to the scratch captures list for building spans.
pub fn addScratchCapture(store: *NodeStore, idx: CIR.Expr.Capture.Idx) std.mem.Allocator.Error!void {
    try store.addScratch("scratch_captures", idx);
}

/// Adds a statement index to the scratch statements list for building spans.
pub fn addScratchStatement(store: *NodeStore, idx: CIR.Statement.Idx) std.mem.Allocator.Error!void {
    try store.addScratch("scratch_statements", idx);
}

/// Computes the span of an expression starting from a given index.
pub fn exprSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.Expr.Span {
    return try store.spanFrom("scratch_exprs", CIR.Expr.Span, start);
}

/// Computes the span of captures starting from a given index.
pub fn capturesSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.Expr.Capture.Span {
    return try store.spanFrom("scratch_captures", CIR.Expr.Capture.Span, start);
}

/// Creates a statement span from the given start position to the current top of scratch statements.
pub fn statementSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.Statement.Span {
    return try store.spanFrom("scratch_statements", CIR.Statement.Span, start);
}

/// Clears scratch expressions starting from a specified index.
pub fn clearScratchExprsFrom(store: *NodeStore, start: u32) void {
    store.clearScratchFrom("scratch_exprs", start);
}

/// Returns a slice of expressions from the scratch space.
pub fn exprSlice(store: *const NodeStore, span: CIR.Expr.Span) []CIR.Expr.Idx {
    return store.sliceFromSpan(CIR.Expr.Idx, span.span);
}

/// Returns the top index for scratch definitions.
pub fn scratchDefTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_defs");
}

/// Adds a scratch definition to temporary storage.
pub fn addScratchDef(store: *NodeStore, idx: CIR.Def.Idx) std.mem.Allocator.Error!void {
    try store.addScratch("scratch_defs", idx);
}

/// Adds a type annotation to the scratch buffer.
pub fn addScratchTypeAnno(store: *NodeStore, idx: CIR.TypeAnno.Idx) std.mem.Allocator.Error!void {
    try store.addScratch("scratch_type_annos", idx);
}

/// Adds a where clause to the scratch buffer.
pub fn addScratchWhereClause(store: *NodeStore, idx: CIR.WhereClause.Idx) std.mem.Allocator.Error!void {
    try store.addScratch("scratch_where_clauses", idx);
}

/// Returns the current top of the scratch type annotations buffer.
pub fn scratchTypeAnnoTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_type_annos");
}

/// Returns the current top of the scratch where clauses buffer.
pub fn scratchWhereClauseTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_where_clauses");
}

/// Clears scratch type annotations from the given index.
pub fn clearScratchTypeAnnosFrom(store: *NodeStore, from: u32) void {
    store.clearScratchFrom("scratch_type_annos", from);
}

/// Clears scratch where clauses from the given index.
pub fn clearScratchWhereClausesFrom(store: *NodeStore, from: u32) void {
    store.clearScratchFrom("scratch_where_clauses", from);
}

/// Creates a span from the scratch type annotations starting at the given index.
pub fn typeAnnoSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.TypeAnno.Span {
    return try store.spanFrom("scratch_type_annos", CIR.TypeAnno.Span, start);
}

/// Returns a span from the scratch anno record fields starting at the given index.
pub fn annoRecordFieldSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.TypeAnno.RecordField.Span {
    return try store.spanFrom("scratch_anno_record_fields", CIR.TypeAnno.RecordField.Span, start);
}

/// Returns a span from the scratch record fields starting at the given index.
pub fn recordFieldSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.RecordField.Span {
    return try store.spanFrom("scratch_record_fields", CIR.RecordField.Span, start);
}

/// Returns a span from the scratch where clauses starting at the given index.
pub fn whereClauseSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.WhereClause.Span {
    return try store.spanFrom("scratch_where_clauses", CIR.WhereClause.Span, start);
}

/// Returns the current top of the scratch exposed items buffer.
pub fn scratchExposedItemTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_exposed_items");
}

/// Adds an exposed item to the scratch buffer.
pub fn addScratchExposedItem(store: *NodeStore, idx: CIR.ExposedItem.Idx) std.mem.Allocator.Error!void {
    try store.addScratch("scratch_exposed_items", idx);
}

/// Creates a span from the scratch exposed items starting at the given index.
pub fn exposedItemSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.ExposedItem.Span {
    return try store.spanFrom("scratch_exposed_items", CIR.ExposedItem.Span, start);
}

/// Clears scratch exposed items from the given index.
pub fn clearScratchExposedItemsFrom(store: *NodeStore, start: u32) void {
    store.clearScratchFrom("scratch_exposed_items", start);
}

/// Returns the start position for a new Span of annoRecordFieldIdxs in scratch
pub fn scratchAnnoRecordFieldTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_anno_record_fields");
}

/// Places a new CIR.TypeAnno.RecordField.Idx in the scratch. Will panic on OOM.
pub fn addScratchAnnoRecordField(store: *NodeStore, idx: CIR.TypeAnno.RecordField.Idx) std.mem.Allocator.Error!void {
    try store.addScratch("scratch_anno_record_fields", idx);
}

/// Clears any AnnoRecordFieldIds added to scratch from start until the end.
pub fn clearScratchAnnoRecordFieldsFrom(store: *NodeStore, start: u32) void {
    store.clearScratchFrom("scratch_anno_record_fields", start);
}

/// Returns a new AnnoRecordField slice so that the caller can iterate through
/// all items in the span.
pub fn annoRecordFieldSlice(store: *NodeStore, span: CIR.TypeAnno.RecordField.Span) []CIR.TypeAnno.RecordField.Idx {
    return store.sliceFromSpan(CIR.TypeAnno.RecordField.Idx, span.span);
}

/// Computes the span of a definition starting from a given index.
pub fn defSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.Def.Span {
    return try store.spanFrom("scratch_defs", CIR.Def.Span, start);
}

/// Retrieves a slice of record destructures from the store.
pub fn recordDestructSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.Pattern.RecordDestruct.Span {
    return try store.spanFrom("scratch_record_destructs", CIR.Pattern.RecordDestruct.Span, start);
}

/// Returns the current top of the scratch patterns buffer.
pub fn scratchPatternTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_patterns");
}

/// Adds a pattern to the scratch patterns list for building spans.
pub fn addScratchPattern(store: *NodeStore, idx: CIR.Pattern.Idx) std.mem.Allocator.Error!void {
    try store.addScratch("scratch_patterns", idx);
}

/// Returns the current top of the scratch record destructures buffer.
pub fn scratchRecordDestructTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_record_destructs");
}

/// Adds a record destructure to the scratch record destructures list for building spans.
pub fn addScratchRecordDestruct(store: *NodeStore, idx: CIR.Pattern.RecordDestruct.Idx) std.mem.Allocator.Error!void {
    try store.addScratch("scratch_record_destructs", idx);
}

/// Creates a pattern span from the given start position to the current top of scratch patterns.
pub fn patternSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.Pattern.Span {
    return try store.spanFrom("scratch_patterns", CIR.Pattern.Span, start);
}

/// Clears scratch definitions starting from a specified index.
pub fn clearScratchDefsFrom(store: *NodeStore, start: u32) void {
    store.clearScratchFrom("scratch_defs", start);
}

/// Creates a slice corresponding to a span.
pub fn sliceFromSpan(store: *const NodeStore, comptime T: type, span: base.DataSpan) []T {
    return @ptrCast(store.extra_data.items.items[span.start..][0..span.len]);
}

/// Returns a slice of definitions from the store.
pub fn sliceDefs(store: *const NodeStore, span: CIR.Def.Span) []CIR.Def.Idx {
    return store.sliceFromSpan(CIR.Def.Idx, span.span);
}

/// Returns a slice of expressions from the store.
pub fn sliceExpr(store: *const NodeStore, span: CIR.Expr.Span) []CIR.Expr.Idx {
    return store.sliceFromSpan(CIR.Expr.Idx, span.span);
}

/// Returns a slice of `CanIR.Pattern.Idx`
pub fn slicePatterns(store: *const NodeStore, span: CIR.Pattern.Span) []CIR.Pattern.Idx {
    return store.sliceFromSpan(CIR.Pattern.Idx, span.span);
}

/// Returns a slice of `CIR.Expr.Capture.Idx`
pub fn sliceCaptures(store: *const NodeStore, span: CIR.Expr.Capture.Span) []CIR.Expr.Capture.Idx {
    return store.sliceFromSpan(CIR.Expr.Capture.Idx, span.span);
}

/// Returns a slice of statements from the store.
pub fn sliceStatements(store: *const NodeStore, span: CIR.Statement.Span) []CIR.Statement.Idx {
    return store.sliceFromSpan(CIR.Statement.Idx, span.span);
}

/// Returns a slice of record fields from the store.
pub fn sliceRecordFields(store: *const NodeStore, span: CIR.RecordField.Span) []CIR.RecordField.Idx {
    return store.sliceFromSpan(CIR.RecordField.Idx, span.span);
}

/// Retrieve a slice of IfBranch Idx's from a span
pub fn sliceIfBranches(store: *const NodeStore, span: CIR.Expr.IfBranch.Span) []CIR.Expr.IfBranch.Idx {
    return store.sliceFromSpan(CIR.Expr.IfBranch.Idx, span.span);
}

/// Retrieve a slice of Match.Branch Idx's from a span
pub fn sliceMatchBranches(store: *const NodeStore, span: CIR.Expr.Match.Branch.Span) []CIR.Expr.Match.Branch.Idx {
    return store.sliceFromSpan(CIR.Expr.Match.Branch.Idx, span.span);
}

/// Retrieve a slice of Match.BranchPattern Idx's from a span
pub fn sliceMatchBranchPatterns(store: *const NodeStore, span: CIR.Expr.Match.BranchPattern.Span) []CIR.Expr.Match.BranchPattern.Idx {
    return store.sliceFromSpan(CIR.Expr.Match.BranchPattern.Idx, span.span);
}

/// Creates a slice corresponding to a span.
pub fn firstFromSpan(store: *const NodeStore, comptime T: type, span: base.DataSpan) T {
    return @as(T, @enumFromInt(store.extra_data.items.items[span.start]));
}

/// Creates a slice corresponding to a span.
pub fn lastFromSpan(store: *const NodeStore, comptime T: type, span: base.DataSpan) T {
    return @as(T, @enumFromInt(store.extra_data.items.items[span.start + span.len - 1]));
}

/// Retrieve a slice of IfBranch Idx's from a span
pub fn firstFromIfBranches(store: *const NodeStore, span: CIR.Expr.IfBranch.Span) CIR.Expr.IfBranch.Idx {
    return store.firstFromSpan(CIR.Expr.IfBranch.Idx, span.span);
}

/// Retrieve a slice of IfBranch Idx's from a span
pub fn lastFromStatements(store: *const NodeStore, span: CIR.Statement.Span) CIR.Statement.Idx {
    return store.lastFromSpan(CIR.Statement.Idx, span.span);
}

/// Returns a slice of if branches from the store.
pub fn scratchIfBranchTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_if_branches");
}

/// Adds an if branch to the scratch if branches list for building spans.
pub fn addScratchIfBranch(store: *NodeStore, if_branch_idx: CIR.Expr.IfBranch.Idx) std.mem.Allocator.Error!void {
    try store.addScratch("scratch_if_branches", if_branch_idx);
}

/// Creates an if branch span from the given start position to the current top of scratch if branches.
pub fn ifBranchSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.Expr.IfBranch.Span {
    return try store.spanFrom("scratch_if_branches", CIR.Expr.IfBranch.Span, start);
}

/// Adds an if branch to the store and returns its index.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addIfBranch(store: *NodeStore, if_branch: CIR.Expr.IfBranch, region: base.Region) std.mem.Allocator.Error!CIR.Expr.IfBranch.Idx {
    const node = Node{
        .data_1 = @intFromEnum(if_branch.cond),
        .data_2 = @intFromEnum(if_branch.body),
        .data_3 = 0,
        .tag = .if_branch,
    };
    const node_idx = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Returns a slice of diagnostics from the store.
pub fn sliceDiagnostics(store: *const NodeStore, span: CIR.Diagnostic.Span) []CIR.Diagnostic.Idx {
    return store.sliceFromSpan(CIR.Diagnostic.Idx, span.span);
}

/// Returns a slice of type annotations from the store.
pub fn sliceTypeAnnos(store: *const NodeStore, span: CIR.TypeAnno.Span) []CIR.TypeAnno.Idx {
    return store.sliceFromSpan(CIR.TypeAnno.Idx, span.span);
}

/// Returns a slice of exposed items from the store.
pub fn sliceExposedItems(store: *const NodeStore, span: CIR.ExposedItem.Span) []CIR.ExposedItem.Idx {
    return store.sliceFromSpan(CIR.ExposedItem.Idx, span.span);
}

/// Returns a slice of where clauses from the store.
pub fn sliceWhereClauses(store: *const NodeStore, span: CIR.WhereClause.Span) []CIR.WhereClause.Idx {
    return store.sliceFromSpan(CIR.WhereClause.Idx, span.span);
}

/// Returns a slice of annotation record fields from the store.
pub fn sliceAnnoRecordFields(store: *const NodeStore, span: CIR.TypeAnno.RecordField.Span) []CIR.TypeAnno.RecordField.Idx {
    return store.sliceFromSpan(CIR.TypeAnno.RecordField.Idx, span.span);
}

/// Returns a slice of record destruct fields from the store.
pub fn sliceRecordDestructs(store: *const NodeStore, span: CIR.Pattern.RecordDestruct.Span) []CIR.Pattern.RecordDestruct.Idx {
    return store.sliceFromSpan(CIR.Pattern.RecordDestruct.Idx, span.span);
}

/// Creates a diagnostic node that stores error information.
///
/// Diagnostics are informational nodes that contain details about compilation errors.
/// They are stored separately from the main IR and are referenced by malformed nodes.
///
/// Note: This function creates diagnostic nodes for storage only.
/// To create a malformed node that represents a runtime error, use `addMalformed()` instead.
///
/// Returns: Index to the created diagnostic node
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addDiagnostic(store: *NodeStore, reason: CIR.Diagnostic) std.mem.Allocator.Error!CIR.Diagnostic.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .tag = @enumFromInt(0),
    };
    var region = base.Region.zero();

    switch (reason) {
        .not_implemented => |r| {
            node.tag = .diag_not_implemented;
            region = r.region;
            node.data_1 = @intFromEnum(r.feature);
        },
        .invalid_num_literal => |r| {
            node.tag = .diag_invalid_num_literal;
            region = r.region;
        },
        .invalid_single_quote => |r| {
            node.tag = .diag_invalid_single_quote;
            region = r.region;
        },
        .empty_tuple => |r| {
            node.tag = .diag_empty_tuple;
            region = r.region;
        },
        .ident_already_in_scope => |r| {
            node.tag = .diag_ident_already_in_scope;
            region = r.region;
            node.data_1 = @bitCast(r.ident);
        },
        .exposed_but_not_implemented => |r| {
            node.tag = .diagnostic_exposed_but_not_implemented;
            region = r.region;
            node.data_1 = @bitCast(r.ident);
        },
        .redundant_exposed => |r| {
            node.tag = .diag_redundant_exposed;
            region = r.region;
            node.data_1 = @bitCast(r.ident);

            // Store original region in extra_data
            const extra_start = store.extra_data.len();
            _ = try store.extra_data.append(store.gpa, r.original_region.start.offset);
            _ = try store.extra_data.append(store.gpa, r.original_region.end.offset);
            node.data_2 = @intCast(extra_start);
        },
        .ident_not_in_scope => |r| {
            node.tag = .diag_ident_not_in_scope;
            region = r.region;
            node.data_1 = @bitCast(r.ident);
        },
        .invalid_top_level_statement => |r| {
            node.tag = .diag_invalid_top_level_statement;
            node.data_1 = @intFromEnum(r.stmt);
            region = r.region;
        },
        .expr_not_canonicalized => |r| {
            node.tag = .diag_expr_not_canonicalized;
            region = r.region;
        },
        .invalid_string_interpolation => |r| {
            node.tag = .diag_invalid_string_interpolation;
            region = r.region;
        },
        .pattern_arg_invalid => |r| {
            node.tag = .diag_pattern_arg_invalid;
            region = r.region;
        },
        .pattern_not_canonicalized => |r| {
            node.tag = .diag_pattern_not_canonicalized;
            region = r.region;
        },
        .can_lambda_not_implemented => |r| {
            node.tag = .diag_can_lambda_not_implemented;
            region = r.region;
        },
        .lambda_body_not_canonicalized => |r| {
            node.tag = .diag_lambda_body_not_canonicalized;
            region = r.region;
        },
        .if_condition_not_canonicalized => |r| {
            node.tag = .diag_if_condition_not_canonicalized;
            region = r.region;
        },
        .if_then_not_canonicalized => |r| {
            node.tag = .diag_if_then_not_canonicalized;
            region = r.region;
        },
        .if_else_not_canonicalized => |r| {
            node.tag = .diag_if_else_not_canonicalized;
            region = r.region;
        },
        .malformed_type_annotation => |r| {
            node.tag = .diag_malformed_type_annotation;
            region = r.region;
        },
        .malformed_where_clause => |r| {
            node.tag = .diag_malformed_where_clause;
            region = r.region;
        },
        .where_clause_not_allowed_in_type_decl => |r| {
            node.tag = .diag_where_clause_not_allowed_in_type_decl;
            region = r.region;
        },
        .var_across_function_boundary => |r| {
            node.tag = .diag_var_across_function_boundary;
            region = r.region;
        },
        .shadowing_warning => |r| {
            node.tag = .diag_shadowing_warning;
            region = r.region;
            node.data_1 = @bitCast(r.ident);
            node.data_2 = r.original_region.start.offset;
            node.data_3 = r.original_region.end.offset;
        },
        .type_redeclared => |r| {
            node.tag = .diag_type_redeclared;
            region = r.redeclared_region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = r.original_region.start.offset;
            node.data_3 = r.original_region.end.offset;
        },
        .undeclared_type => |r| {
            node.tag = .diag_undeclared_type;
            region = r.region;
            node.data_1 = @bitCast(r.name);
        },
        .type_alias_but_needed_nominal => |r| {
            node.tag = .diag_type_alias_but_needed_nominal;
            region = r.region;
            node.data_1 = @bitCast(r.name);
        },
        .undeclared_type_var => |r| {
            node.tag = .diag_undeclared_type_var;
            region = r.region;
            node.data_1 = @bitCast(r.name);
        },
        .type_alias_redeclared => |r| {
            node.tag = .diag_type_alias_redeclared;
            region = r.redeclared_region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = r.original_region.start.offset;
            node.data_3 = r.original_region.end.offset;
        },
        .tuple_elem_not_canonicalized => |r| {
            node.tag = .diag_tuple_elem_not_canonicalized;
            region = r.region;
        },
        .module_not_found => |r| {
            node.tag = .diag_module_not_found;
            region = r.region;
            node.data_1 = @as(u32, @bitCast(r.module_name));
        },
        .value_not_exposed => |r| {
            node.tag = .diag_value_not_exposed;
            region = r.region;
            node.data_1 = @as(u32, @bitCast(r.module_name));
            node.data_2 = @as(u32, @bitCast(r.value_name));
        },
        .type_not_exposed => |r| {
            node.tag = .diag_type_not_exposed;
            region = r.region;
            node.data_1 = @as(u32, @bitCast(r.module_name));
            node.data_2 = @as(u32, @bitCast(r.type_name));
        },
        .module_not_imported => |r| {
            node.tag = .diag_module_not_imported;
            region = r.region;
            node.data_1 = @as(u32, @bitCast(r.module_name));
        },
        .too_many_exports => |r| {
            node.tag = .diag_too_many_exports;
            region = r.region;
            node.data_1 = r.count;
        },
        .nominal_type_redeclared => |r| {
            node.tag = .diag_nominal_type_redeclared;
            region = r.redeclared_region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = r.original_region.start.offset;
            node.data_3 = r.original_region.end.offset;
        },
        .type_shadowed_warning => |r| {
            node.tag = .diag_type_shadowed_warning;
            region = r.region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = @intFromBool(r.cross_scope);

            // Store original region in extra_data
            const extra_start = store.extra_data.len();
            _ = try store.extra_data.append(store.gpa, r.original_region.start.offset);
            _ = try store.extra_data.append(store.gpa, r.original_region.end.offset);
            node.data_3 = @intCast(extra_start);
        },
        .type_parameter_conflict => |r| {
            node.tag = .diag_type_parameter_conflict;
            region = r.region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = @bitCast(r.parameter_name);
            const extra_start = store.extra_data.len();
            _ = try store.extra_data.append(store.gpa, r.original_region.start.offset);
            _ = try store.extra_data.append(store.gpa, r.original_region.end.offset);
            node.data_3 = @intCast(extra_start);
        },
        .unused_variable => |r| {
            node.tag = .diag_unused_variable;
            region = r.region;
            node.data_1 = @bitCast(r.ident);
        },
        .used_underscore_variable => |r| {
            node.tag = .diag_used_underscore_variable;
            region = r.region;
            node.data_1 = @bitCast(r.ident);
        },
        .duplicate_record_field => |r| {
            node.tag = .diag_duplicate_record_field;
            region = r.duplicate_region;
            node.data_1 = @bitCast(r.field_name);
            node.data_2 = r.original_region.start.offset;
            node.data_3 = r.original_region.end.offset;
        },
        .crash_expects_string => |r| {
            node.tag = .diag_crash_expects_string;
            region = r.region;
        },
        .f64_pattern_literal => |r| {
            node.tag = .diag_f64_pattern_literal;
            region = r.region;
        },
        .unused_type_var_name => |r| {
            node.tag = .diag_unused_type_var_name;
            region = r.region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = @bitCast(r.suggested_name);
        },
        .type_var_marked_unused => |r| {
            node.tag = .diag_type_var_marked_unused;
            region = r.region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = @bitCast(r.suggested_name);
        },
        .type_var_ending_in_underscore => |r| {
            node.tag = .diag_type_var_ending_in_underscore;
            region = r.region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = @bitCast(r.suggested_name);
        },
        .underscore_in_type_declaration => |r| {
            node.tag = .diag_underscore_in_type_declaration;
            region = r.region;
            node.data_1 = if (r.is_alias) 1 else 0;
        },
    }

    const nid = @intFromEnum(try store.nodes.append(store.gpa, node));
    _ = try store.regions.append(store.gpa, region);

    // append to our scratch so we can get a span later of all our diagnostics
    try store.addScratch("scratch_diagnostics", @as(CIR.Diagnostic.Idx, @enumFromInt(nid)));

    return @enumFromInt(nid);
}

/// Creates a malformed node that represents a runtime error in the IR.
///
/// Malformed nodes follow the "Inform Don't Block" principle: they allow compilation
/// to continue while preserving error information. When encountered during execution,
/// they will crash with the associated diagnostic.
///
/// This function:
/// 1. Creates a diagnostic node to store the error details
/// 2. Creates a malformed node (.malformed tag) that references the diagnostic
/// 3. Returns an index of the requested type that points to the malformed node
///
/// The malformed node will generate a runtime_error in the ModuleEnv that properly
/// references the diagnostic index.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addMalformed(store: *NodeStore, diagnostic_idx: CIR.Diagnostic.Idx, region: Region) std.mem.Allocator.Error!Node.Idx {
    const malformed_node = Node{
        .data_1 = @intFromEnum(diagnostic_idx),
        .data_2 = 0,
        .data_3 = 0,
        .tag = .malformed,
    };
    const malformed_nid = try store.nodes.append(store.gpa, malformed_node);
    _ = try store.regions.append(store.gpa, region);
    return malformed_nid;
}

/// Retrieves diagnostic information from a diagnostic node.
///
/// This function extracts the stored diagnostic data from nodes with .diag_* tags.
/// It reconstructs the original CIR.Diagnostic from the node's stored data.
pub fn getDiagnostic(store: *const NodeStore, diagnostic: CIR.Diagnostic.Idx) CIR.Diagnostic {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(diagnostic));
    const node = store.nodes.get(node_idx);

    switch (node.tag) {
        .diag_not_implemented => return CIR.Diagnostic{ .not_implemented = .{
            .feature = @enumFromInt(node.data_1),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_invalid_num_literal => return CIR.Diagnostic{ .invalid_num_literal = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_invalid_single_quote => return CIR.Diagnostic{ .invalid_single_quote = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_empty_tuple => return CIR.Diagnostic{ .empty_tuple = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_ident_already_in_scope => return CIR.Diagnostic{ .ident_already_in_scope = .{
            .ident = @bitCast(node.data_1),
            .region = store.getRegionAt(node_idx),
        } },
        .diagnostic_exposed_but_not_implemented => return CIR.Diagnostic{ .exposed_but_not_implemented = .{
            .ident = @bitCast(node.data_1),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_redundant_exposed => {
            const extra_data = store.extra_data.items.items[node.data_2..];
            const original_start = extra_data[0];
            const original_end = extra_data[1];
            return CIR.Diagnostic{ .redundant_exposed = .{
                .ident = @bitCast(node.data_1),
                .region = store.getRegionAt(node_idx),
                .original_region = Region{
                    .start = .{ .offset = original_start },
                    .end = .{ .offset = original_end },
                },
            } };
        },
        .diag_ident_not_in_scope => return CIR.Diagnostic{ .ident_not_in_scope = .{
            .ident = @bitCast(node.data_1),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_invalid_top_level_statement => return CIR.Diagnostic{ .invalid_top_level_statement = .{
            .stmt = @enumFromInt(node.data_1),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_expr_not_canonicalized => return CIR.Diagnostic{ .expr_not_canonicalized = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_invalid_string_interpolation => return CIR.Diagnostic{ .invalid_string_interpolation = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_pattern_arg_invalid => return CIR.Diagnostic{ .pattern_arg_invalid = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_pattern_not_canonicalized => return CIR.Diagnostic{ .pattern_not_canonicalized = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_can_lambda_not_implemented => return CIR.Diagnostic{ .can_lambda_not_implemented = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_lambda_body_not_canonicalized => return CIR.Diagnostic{ .lambda_body_not_canonicalized = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_if_condition_not_canonicalized => return CIR.Diagnostic{ .if_condition_not_canonicalized = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_if_then_not_canonicalized => return CIR.Diagnostic{ .if_then_not_canonicalized = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_if_else_not_canonicalized => return CIR.Diagnostic{ .if_else_not_canonicalized = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_var_across_function_boundary => return CIR.Diagnostic{ .var_across_function_boundary = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_shadowing_warning => return CIR.Diagnostic{ .shadowing_warning = .{
            .ident = @bitCast(node.data_1),
            .region = store.getRegionAt(node_idx),
            .original_region = .{
                .start = .{ .offset = node.data_2 },
                .end = .{ .offset = node.data_3 },
            },
        } },
        .diag_type_redeclared => return CIR.Diagnostic{ .type_redeclared = .{
            .name = @bitCast(node.data_1),
            .redeclared_region = store.getRegionAt(node_idx),
            .original_region = .{
                .start = .{ .offset = node.data_2 },
                .end = .{ .offset = node.data_3 },
            },
        } },
        .diag_undeclared_type => return CIR.Diagnostic{ .undeclared_type = .{
            .name = @bitCast(node.data_1),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_type_alias_but_needed_nominal => return CIR.Diagnostic{ .type_alias_but_needed_nominal = .{
            .name = @bitCast(node.data_1),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_tuple_elem_not_canonicalized => return CIR.Diagnostic{ .tuple_elem_not_canonicalized = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_module_not_found => return CIR.Diagnostic{ .module_not_found = .{
            .module_name = @as(base.Ident.Idx, @bitCast(node.data_1)),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_value_not_exposed => return CIR.Diagnostic{ .value_not_exposed = .{
            .module_name = @as(base.Ident.Idx, @bitCast(node.data_1)),
            .value_name = @as(base.Ident.Idx, @bitCast(node.data_2)),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_type_not_exposed => return CIR.Diagnostic{ .type_not_exposed = .{
            .module_name = @as(base.Ident.Idx, @bitCast(node.data_1)),
            .type_name = @as(base.Ident.Idx, @bitCast(node.data_2)),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_module_not_imported => return CIR.Diagnostic{ .module_not_imported = .{
            .module_name = @as(base.Ident.Idx, @bitCast(node.data_1)),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_too_many_exports => return CIR.Diagnostic{ .too_many_exports = .{
            .count = node.data_1,
            .region = store.getRegionAt(node_idx),
        } },
        .diag_undeclared_type_var => return CIR.Diagnostic{ .undeclared_type_var = .{
            .name = @bitCast(node.data_1),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_malformed_type_annotation => return CIR.Diagnostic{ .malformed_type_annotation = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_malformed_where_clause => return CIR.Diagnostic{ .malformed_where_clause = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_where_clause_not_allowed_in_type_decl => return CIR.Diagnostic{ .where_clause_not_allowed_in_type_decl = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_type_alias_redeclared => return CIR.Diagnostic{ .type_alias_redeclared = .{
            .name = @bitCast(node.data_1),
            .redeclared_region = store.getRegionAt(node_idx),
            .original_region = .{
                .start = .{ .offset = @intCast(node.data_2) },
                .end = .{ .offset = @intCast(node.data_3) },
            },
        } },
        .diag_nominal_type_redeclared => return CIR.Diagnostic{ .nominal_type_redeclared = .{
            .name = @bitCast(node.data_1),
            .redeclared_region = store.getRegionAt(node_idx),
            .original_region = .{
                .start = .{ .offset = @intCast(node.data_2) },
                .end = .{ .offset = @intCast(node.data_3) },
            },
        } },
        .diag_type_shadowed_warning => {
            const extra_data = store.extra_data.items.items[node.data_3..];
            const original_start = extra_data[0];
            const original_end = extra_data[1];
            return CIR.Diagnostic{ .type_shadowed_warning = .{
                .name = @bitCast(node.data_1),
                .region = store.getRegionAt(node_idx),
                .cross_scope = node.data_2 != 0,
                .original_region = .{
                    .start = .{ .offset = original_start },
                    .end = .{ .offset = original_end },
                },
            } };
        },
        .diag_type_parameter_conflict => {
            const extra_data = store.extra_data.items.items[node.data_3..];
            const original_start = extra_data[0];
            const original_end = extra_data[1];
            return CIR.Diagnostic{ .type_parameter_conflict = .{
                .name = @bitCast(node.data_1),
                .parameter_name = @bitCast(node.data_2),
                .region = store.getRegionAt(node_idx),
                .original_region = .{
                    .start = .{ .offset = original_start },
                    .end = .{ .offset = original_end },
                },
            } };
        },
        .diag_unused_variable => return CIR.Diagnostic{ .unused_variable = .{
            .ident = @bitCast(node.data_1),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_used_underscore_variable => return CIR.Diagnostic{ .used_underscore_variable = .{
            .ident = @bitCast(node.data_1),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_duplicate_record_field => return CIR.Diagnostic{ .duplicate_record_field = .{
            .field_name = @bitCast(node.data_1),
            .duplicate_region = store.getRegionAt(node_idx),
            .original_region = .{
                .start = .{ .offset = @intCast(node.data_2) },
                .end = .{ .offset = @intCast(node.data_3) },
            },
        } },
        .diag_crash_expects_string => return CIR.Diagnostic{ .crash_expects_string = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_f64_pattern_literal => return CIR.Diagnostic{ .f64_pattern_literal = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_unused_type_var_name => return CIR.Diagnostic{ .unused_type_var_name = .{
            .name = @bitCast(node.data_1),
            .suggested_name = @bitCast(node.data_2),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_type_var_marked_unused => return CIR.Diagnostic{ .type_var_marked_unused = .{
            .name = @bitCast(node.data_1),
            .suggested_name = @bitCast(node.data_2),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_type_var_ending_in_underscore => return CIR.Diagnostic{ .type_var_ending_in_underscore = .{
            .name = @bitCast(node.data_1),
            .suggested_name = @bitCast(node.data_2),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_underscore_in_type_declaration => return CIR.Diagnostic{ .underscore_in_type_declaration = .{
            .is_alias = node.data_1 != 0,
            .region = store.getRegionAt(node_idx),
        } },
        else => {
            @panic("getDiagnostic called with non-diagnostic node - this indicates a compiler bug");
        },
    }
}

/// Computes the span of a diagnostic starting from a given index.
pub fn diagnosticSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.Diagnostic.Span {
    return try store.spanFrom("scratch_diagnostics", CIR.Diagnostic.Span, start);
}

/// Ensure the node store has capacity for at least the requested number of
/// slots. Then return the *final* index.
pub fn predictNodeIndex(store: *NodeStore, count: u32) std.mem.Allocator.Error!Node.Idx {
    const start_idx = store.nodes.len();
    try store.nodes.ensureTotalCapacity(store.gpa, start_idx + count);
    // Return where the LAST node will actually be placed
    return @enumFromInt(start_idx + count - 1);
}

/// Adds an type variable slot to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addTypeVarSlot(store: *NodeStore, parent_node_idx: Node.Idx, region: base.Region) std.mem.Allocator.Error!Node.Idx {
    const nid = try store.nodes.append(store.gpa, .{
        .tag = .type_var_slot,
        .data_1 = @intFromEnum(parent_node_idx),
        .data_2 = 0,
        .data_3 = 0,
    });
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Given a target node idx, check that the it is in bounds
/// If it is, do nothing
/// If it's not, then fill in the store with type_var_slots for all missing
/// intervening nodes, *up to and including* the provided node
pub fn fillInTypeVarSlotsThru(store: *NodeStore, target_idx: Node.Idx, parent_node_idx: Node.Idx, region: Region) std.mem.Allocator.Error!void {
    const idx = @intFromEnum(target_idx);
    try store.nodes.items.ensureTotalCapacity(store.gpa, idx);
    while (store.nodes.items.len <= idx) {
        store.nodes.items.appendAssumeCapacity(.{
            .tag = .type_var_slot,
            .data_1 = @intFromEnum(parent_node_idx),
            .data_2 = 0,
            .data_3 = 0,
        });
        _ = try store.regions.append(store.gpa, region);
    }
}

/// Return the current top index for scratch match branches.
pub fn scratchMatchBranchTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_match_branches");
}

/// Add a match branch index to the scratch buffer.
pub fn addScratchMatchBranch(store: *NodeStore, branch_idx: CIR.Expr.Match.Branch.Idx) std.mem.Allocator.Error!void {
    try store.addScratch("scratch_match_branches", branch_idx);
}

/// Create a span from the scratch match branches starting at the given index.
pub fn matchBranchSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.Expr.Match.Branch.Span {
    return try store.spanFrom("scratch_match_branches", CIR.Expr.Match.Branch.Span, start);
}

/// Return the current top index for scratch match branch patterns.
pub fn scratchMatchBranchPatternTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_match_branch_patterns");
}

/// Add a match branch pattern index to the scratch buffer.
pub fn addScratchMatchBranchPattern(store: *NodeStore, pattern_idx: CIR.Expr.Match.BranchPattern.Idx) std.mem.Allocator.Error!void {
    try store.addScratch("scratch_match_branch_patterns", pattern_idx);
}

/// Create a span from the scratch match branch patterns starting at the given index.
pub fn matchBranchPatternSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!CIR.Expr.Match.BranchPattern.Span {
    return try store.spanFrom("scratch_match_branch_patterns", CIR.Expr.Match.BranchPattern.Span, start);
}

/// Serialize this NodeStore to the given CompactWriter. The resulting NodeStore
/// in the writer's buffer will have offsets instead of pointers. Calling any
/// methods on it or dereferencing its internal "pointers" (which are now
/// offsets) is illegal behavior!
pub fn serialize(
    self: *const NodeStore,
    allocator: std.mem.Allocator,
    writer: *CompactWriter,
) std.mem.Allocator.Error!*const NodeStore {
    // First, write the NodeStore struct itself
    const offset_self = try writer.appendAlloc(allocator, NodeStore);

    // Then serialize the sub-structures and update the struct
    offset_self.* = .{
        .gpa = undefined, // Will be set when deserializing
        .nodes = (try self.nodes.serialize(allocator, writer)).*,
        .regions = (try self.regions.serialize(allocator, writer)).*,
        .extra_data = (try self.extra_data.serialize(allocator, writer)).*,
        // All scratch arrays are serialized as empty
        // TODO: maybe we can put these all at the end of ModuleEnv and not bother serializing them, and just re-init on deserialization?
        .scratch_statements = .{ .items = .{} },
        .scratch_exprs = .{ .items = .{} },
        .scratch_captures = .{ .items = .{} },
        .scratch_record_fields = .{ .items = .{} },
        .scratch_match_branches = .{ .items = .{} },
        .scratch_match_branch_patterns = .{ .items = .{} },
        .scratch_if_branches = .{ .items = .{} },
        .scratch_where_clauses = .{ .items = .{} },
        .scratch_patterns = .{ .items = .{} },
        .scratch_pattern_record_fields = .{ .items = .{} },
        .scratch_record_destructs = .{ .items = .{} },
        .scratch_type_annos = .{ .items = .{} },
        .scratch_anno_record_fields = .{ .items = .{} },
        .scratch_exposed_items = .{ .items = .{} },
        .scratch_defs = .{ .items = .{} },
        .scratch_diagnostics = .{ .items = .{} },
    };

    return @constCast(offset_self);
}

/// Add the given offset to the memory addresses of all pointers in `self`.
pub fn relocate(self: *NodeStore, offset: isize) void {
    self.nodes.relocate(offset);
    self.regions.relocate(offset);
    self.extra_data.relocate(offset);
    // Note: scratch arrays are empty after deserialization, so no need to relocate
}

/// Serialized representation of NodeStore
pub const Serialized = struct {
    nodes: Node.List.Serialized,
    regions: Region.List.Serialized,
    extra_data: collections.SafeList(u32).Serialized,
    // Scratch arrays - not serialized, just placeholders to match NodeStore size
    // TODO move these out of NodeStore so that we don't need to serialize and
    // deserialize a bunch of zeros for these; it's a waste of space.
    scratch_statements: std.ArrayListUnmanaged(CIR.Statement.Idx) = .{},
    scratch_exprs: std.ArrayListUnmanaged(CIR.Expr.Idx) = .{},
    scratch_record_fields: std.ArrayListUnmanaged(CIR.RecordField.Idx) = .{},
    scratch_match_branches: std.ArrayListUnmanaged(CIR.Expr.Match.Branch.Idx) = .{},
    scratch_match_branch_patterns: std.ArrayListUnmanaged(CIR.Expr.Match.BranchPattern.Idx) = .{},
    scratch_if_branches: std.ArrayListUnmanaged(CIR.Expr.IfBranch.Idx) = .{},
    scratch_where_clauses: std.ArrayListUnmanaged(CIR.WhereClause.Idx) = .{},
    scratch_patterns: std.ArrayListUnmanaged(CIR.Pattern.Idx) = .{},
    scratch_pattern_record_fields: std.ArrayListUnmanaged(CIR.PatternRecordField.Idx) = .{},
    scratch_record_destructs: std.ArrayListUnmanaged(CIR.Pattern.RecordDestruct.Idx) = .{},
    scratch_type_annos: std.ArrayListUnmanaged(CIR.TypeAnno.Idx) = .{},
    scratch_anno_record_fields: std.ArrayListUnmanaged(CIR.TypeAnno.RecordField.Idx) = .{},
    scratch_exposed_items: std.ArrayListUnmanaged(CIR.ExposedItem.Idx) = .{},
    scratch_defs: std.ArrayListUnmanaged(CIR.Def.Idx) = .{},
    scratch_diagnostics: std.ArrayListUnmanaged(CIR.Diagnostic.Idx) = .{},
    scratch_captures: std.ArrayListUnmanaged(CIR.Expr.Capture.Idx) = .{},
    gpa: std.mem.Allocator = undefined,

    /// Serialize a NodeStore into this Serialized struct, appending data to the writer
    pub fn serialize(
        self: *Serialized,
        store: *const NodeStore,
        allocator: std.mem.Allocator,
        writer: *CompactWriter,
    ) std.mem.Allocator.Error!void {
        // Serialize nodes
        try self.nodes.serialize(&store.nodes, allocator, writer);
        // Serialize regions
        try self.regions.serialize(&store.regions, allocator, writer);
        // Serialize extra_data
        try self.extra_data.serialize(&store.extra_data, allocator, writer);
    }

    /// Deserialize this Serialized struct into a NodeStore
    pub fn deserialize(self: *Serialized, offset: i64, gpa: std.mem.Allocator) *NodeStore {
        // NodeStore.Serialized should be at least as big as NodeStore
        std.debug.assert(@sizeOf(Serialized) >= @sizeOf(NodeStore));

        // Overwrite ourself with the deserialized version, and return our pointer after casting it to Self.
        const store = @as(*NodeStore, @ptrFromInt(@intFromPtr(self)));

        store.* = NodeStore{
            .gpa = gpa,
            .nodes = self.nodes.deserialize(offset).*,
            .regions = self.regions.deserialize(offset).*,
            .extra_data = self.extra_data.deserialize(offset).*,
            // Initialize scratch arrays as proper Scratch instances
            .scratch_statements = base.Scratch(CIR.Statement.Idx){ .items = .{} },
            .scratch_exprs = base.Scratch(CIR.Expr.Idx){ .items = .{} },
            .scratch_captures = base.Scratch(CIR.Expr.Capture.Idx){ .items = .{} },
            .scratch_patterns = base.Scratch(CIR.Pattern.Idx){ .items = .{} },
            .scratch_record_fields = base.Scratch(CIR.RecordField.Idx){ .items = .{} },
            .scratch_pattern_record_fields = base.Scratch(CIR.PatternRecordField.Idx){ .items = .{} },
            .scratch_record_destructs = base.Scratch(CIR.Pattern.RecordDestruct.Idx){ .items = .{} },
            .scratch_match_branches = base.Scratch(CIR.Expr.Match.Branch.Idx){ .items = .{} },
            .scratch_match_branch_patterns = base.Scratch(CIR.Expr.Match.BranchPattern.Idx){ .items = .{} },
            .scratch_if_branches = base.Scratch(CIR.Expr.IfBranch.Idx){ .items = .{} },
            .scratch_type_annos = base.Scratch(CIR.TypeAnno.Idx){ .items = .{} },
            .scratch_anno_record_fields = base.Scratch(CIR.TypeAnno.RecordField.Idx){ .items = .{} },
            .scratch_exposed_items = base.Scratch(CIR.ExposedItem.Idx){ .items = .{} },
            .scratch_defs = base.Scratch(CIR.Def.Idx){ .items = .{} },
            .scratch_where_clauses = base.Scratch(CIR.WhereClause.Idx){ .items = .{} },
            .scratch_diagnostics = base.Scratch(CIR.Diagnostic.Idx){ .items = .{} },
        };

        return store;
    }
};

test "NodeStore empty CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create an empty NodeStore
    var original = try NodeStore.init(gpa);
    defer original.deinit();

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_nodestore.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, NodeStore.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and deserialize
    const serialized_ptr: *NodeStore.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa);

    // Verify empty
    try testing.expectEqual(@as(usize, 0), deserialized.nodes.len());
    try testing.expectEqual(@as(usize, 0), deserialized.regions.len());
    try testing.expectEqual(@as(usize, 0), deserialized.extra_data.len());
}

test "NodeStore basic CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create NodeStore and add some nodes
    var original = try NodeStore.init(gpa);
    defer original.deinit();

    // Add a simple expression node
    const node1 = Node{
        .tag = .expr_int,
        .data_1 = 0, // extra_data index
        .data_2 = 0,
        .data_3 = 0,
    };
    _ = try original.nodes.append(gpa, node1);

    // Add integer value to extra_data (i128 as 4 u32s)
    const value: i128 = 42;
    const value_bytes: [16]u8 = @bitCast(value);
    const value_u32s: [4]u32 = @bitCast(value_bytes);
    for (value_u32s) |u32_val| {
        _ = try original.extra_data.append(gpa, u32_val);
    }

    // Add a region
    const region = Region{
        .start = .{ .offset = 0 },
        .end = .{ .offset = 5 },
    };
    _ = try original.regions.append(gpa, region);

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_nodestore.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, NodeStore.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and deserialize
    const serialized_ptr: *NodeStore.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa);

    // Verify nodes
    try testing.expectEqual(@as(usize, 1), deserialized.nodes.len());
    const retrieved_node = deserialized.nodes.get(@enumFromInt(0));
    try testing.expectEqual(Node.Tag.expr_int, retrieved_node.tag);
    try testing.expectEqual(@as(u32, 0), retrieved_node.data_1);

    // Verify extra_data
    try testing.expectEqual(@as(usize, 4), deserialized.extra_data.len());
    const retrieved_u32s = deserialized.extra_data.items.items[0..4];
    const retrieved_bytes: [16]u8 = @bitCast(retrieved_u32s.*);
    const retrieved_value: i128 = @bitCast(retrieved_bytes);
    try testing.expectEqual(@as(i128, 42), retrieved_value);

    // Verify regions
    try testing.expectEqual(@as(usize, 1), deserialized.regions.len());
    const retrieved_region = deserialized.regions.get(@enumFromInt(0));
    try testing.expectEqual(region.start.offset, retrieved_region.start.offset);
    try testing.expectEqual(region.end.offset, retrieved_region.end.offset);
}

test "NodeStore multiple nodes CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create NodeStore with various node types
    var original = try NodeStore.init(gpa);
    defer original.deinit();

    // Add expression variable node
    const var_node = Node{
        .tag = .expr_var,
        .data_1 = 5, // pattern_idx
        .data_2 = 0,
        .data_3 = 0,
    };
    _ = try original.nodes.append(gpa, var_node);

    // Add expression list node
    const list_node = Node{
        .tag = .expr_list,
        .data_1 = 10, // elems start
        .data_2 = 3, // elems len
        .data_3 = 2, // elem_var
    };
    _ = try original.nodes.append(gpa, list_node);

    // Add float node with extra data
    const float_node = Node{
        .tag = .expr_frac_f64,
        .data_1 = 0, // extra_data index
        .data_2 = 0,
        .data_3 = 0,
    };
    _ = try original.nodes.append(gpa, float_node);

    // Add float value to extra_data
    const float_value: f64 = 3.14159;
    const float_as_u64: u64 = @bitCast(float_value);
    const float_as_u32s: [2]u32 = @bitCast(float_as_u64);
    for (float_as_u32s) |u32_val| {
        _ = try original.extra_data.append(gpa, u32_val);
    }

    // Add regions for each node
    const regions = [_]Region{
        .{ .start = .{ .offset = 0 }, .end = .{ .offset = 5 } },
        .{ .start = .{ .offset = 10 }, .end = .{ .offset = 20 } },
        .{ .start = .{ .offset = 25 }, .end = .{ .offset = 32 } },
    };
    for (regions) |region| {
        _ = try original.regions.append(gpa, region);
    }

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_multiple_nodestore.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, NodeStore.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and deserialize
    const serialized_ptr: *NodeStore.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa);

    // Verify nodes
    try testing.expectEqual(@as(usize, 3), deserialized.nodes.len());

    // Verify var node
    const retrieved_var = deserialized.nodes.get(@enumFromInt(0));
    try testing.expectEqual(Node.Tag.expr_var, retrieved_var.tag);
    try testing.expectEqual(@as(u32, 5), retrieved_var.data_1);

    // Verify list node
    const retrieved_list = deserialized.nodes.get(@enumFromInt(1));
    try testing.expectEqual(Node.Tag.expr_list, retrieved_list.tag);
    try testing.expectEqual(@as(u32, 10), retrieved_list.data_1);
    try testing.expectEqual(@as(u32, 3), retrieved_list.data_2);
    try testing.expectEqual(@as(u32, 2), retrieved_list.data_3);

    // Verify float node and extra data
    const retrieved_float = deserialized.nodes.get(@enumFromInt(2));
    try testing.expectEqual(Node.Tag.expr_frac_f64, retrieved_float.tag);
    const retrieved_float_u32s = deserialized.extra_data.items.items[0..2];
    const retrieved_float_u64: u64 = @bitCast(retrieved_float_u32s.*);
    const retrieved_float_value: f64 = @bitCast(retrieved_float_u64);
    try testing.expectApproxEqAbs(float_value, retrieved_float_value, 0.0001);

    // Verify regions
    try testing.expectEqual(@as(usize, 3), deserialized.regions.len());
    for (regions, 0..) |expected_region, i| {
        const retrieved_region = deserialized.regions.get(@enumFromInt(i));
        try testing.expectEqual(expected_region.start.offset, retrieved_region.start.offset);
        try testing.expectEqual(expected_region.end.offset, retrieved_region.end.offset);
    }

    // Verify all scratch arrays are empty
    try testing.expectEqual(@as(usize, 0), deserialized.scratch_statements.items.items.len);
    try testing.expectEqual(@as(usize, 0), deserialized.scratch_exprs.items.items.len);
    try testing.expectEqual(@as(usize, 0), deserialized.scratch_patterns.items.items.len);
}
