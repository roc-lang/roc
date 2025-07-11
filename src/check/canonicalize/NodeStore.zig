//! Stores AST nodes and provides scratch arrays for working with nodes.

const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const collections = @import("../../collections.zig");
const Node = @import("Node.zig");
const CIR = @import("CIR.zig");
const RocDec = @import("../../builtins/dec.zig").RocDec;
const PackedDataSpan = @import("../../base/PackedDataSpan.zig");
const SERIALIZATION_ALIGNMENT = @import("../../serialization/mod.zig").SERIALIZATION_ALIGNMENT;

const DataSpan = base.DataSpan;
const Region = base.Region;
const StringLiteral = base.StringLiteral;
const Diagnostic = @import("Diagnostic.zig");
const Ident = base.Ident;

const exitOnOom = collections.exitOnOom;

const NodeStore = @This();

gpa: std.mem.Allocator,
nodes: Node.List,
regions: Region.List,
extra_data: std.ArrayListUnmanaged(u32),
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

/// Initializes the NodeStore
pub fn init(gpa: std.mem.Allocator) NodeStore {
    // TODO determine what capacity to use
    // maybe these should be moved to build/compile flags?
    return NodeStore.initCapacity(gpa, 128);
}

/// Initializes the NodeStore with a specified capacity.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) NodeStore {
    return .{
        .gpa = gpa,
        .nodes = Node.List.initCapacity(gpa, capacity),
        .regions = Region.List.initCapacity(gpa, capacity),
        .extra_data = std.ArrayListUnmanaged(u32).initCapacity(gpa, capacity / 2) catch |err| exitOnOom(err),
        .scratch_statements = base.Scratch(CIR.Statement.Idx).init(gpa),
        .scratch_exprs = base.Scratch(CIR.Expr.Idx).init(gpa),
        .scratch_patterns = base.Scratch(CIR.Pattern.Idx).init(gpa),
        .scratch_record_fields = base.Scratch(CIR.RecordField.Idx).init(gpa),
        .scratch_pattern_record_fields = base.Scratch(CIR.PatternRecordField.Idx).init(gpa),
        .scratch_record_destructs = base.Scratch(CIR.Pattern.RecordDestruct.Idx).init(gpa),
        .scratch_match_branches = base.Scratch(CIR.Expr.Match.Branch.Idx).init(gpa),
        .scratch_match_branch_patterns = base.Scratch(CIR.Expr.Match.BranchPattern.Idx).init(gpa),
        .scratch_if_branches = base.Scratch(CIR.Expr.IfBranch.Idx).init(gpa),
        .scratch_type_annos = base.Scratch(CIR.TypeAnno.Idx).init(gpa),
        .scratch_anno_record_fields = base.Scratch(CIR.TypeAnno.RecordField.Idx).init(gpa),
        .scratch_exposed_items = base.Scratch(CIR.ExposedItem.Idx).init(gpa),
        .scratch_defs = base.Scratch(CIR.Def.Idx).init(gpa),
        .scratch_where_clauses = base.Scratch(CIR.WhereClause.Idx).init(gpa),
        .scratch_diagnostics = base.Scratch(CIR.Diagnostic.Idx).init(gpa),
    };
}

/// Deinitializes the NodeStore, freeing any allocated resources.
pub fn deinit(store: *NodeStore) void {
    store.nodes.deinit(store.gpa);
    store.regions.deinit(store.gpa);
    store.extra_data.deinit(store.gpa);
    store.scratch_statements.items.deinit(store.gpa);
    store.scratch_exprs.items.deinit(store.gpa);
    store.scratch_patterns.items.deinit(store.gpa);
    store.scratch_record_fields.items.deinit(store.gpa);
    store.scratch_pattern_record_fields.items.deinit(store.gpa);
    store.scratch_record_destructs.items.deinit(store.gpa);
    store.scratch_match_branches.items.deinit(store.gpa);
    store.scratch_match_branch_patterns.items.deinit(store.gpa);
    store.scratch_if_branches.items.deinit(store.gpa);
    store.scratch_type_annos.items.deinit(store.gpa);
    store.scratch_anno_record_fields.items.deinit(store.gpa);
    store.scratch_exposed_items.items.deinit(store.gpa);
    store.scratch_defs.items.deinit(store.gpa);
    store.scratch_where_clauses.items.deinit(store.gpa);
    store.scratch_diagnostics.items.deinit(store.gpa);
}

/// Compile-time constants for union variant counts to ensure we don't miss cases
/// when adding/removing variants from CIR unions. Update these when modifying the unions.
///
/// Count of the diagnostic nodes in the CIR
pub const CIR_DIAGNOSTIC_NODE_COUNT = 42;
/// Count of the expression nodes in the CIR
pub const CIR_EXPR_NODE_COUNT = 28;
/// Count of the statement nodes in the CIR
pub const CIR_STATEMENT_NODE_COUNT = 13;
/// Count of the type annotation nodes in the CIR
pub const CIR_TYPE_ANNO_NODE_COUNT = 11;
/// Count of the pattern nodes in the CIR
pub const CIR_PATTERN_NODE_COUNT = 12;

comptime {
    // Check the number of CIR.Diagnostic nodes
    const diagnostic_fields = @typeInfo(CIR.Diagnostic).@"union".fields;
    std.debug.assert(diagnostic_fields.len == CIR_DIAGNOSTIC_NODE_COUNT);
}

comptime {
    // Check the number of CIR.Expr nodes
    const expr_fields = @typeInfo(CIR.Expr).@"union".fields;
    std.debug.assert(expr_fields.len == CIR_EXPR_NODE_COUNT);
}

comptime {
    // Check the number of CIR.Statement nodes
    const statement_fields = @typeInfo(CIR.Statement).@"union".fields;
    std.debug.assert(statement_fields.len == CIR_STATEMENT_NODE_COUNT);
}

comptime {
    // Check the number of CIR.TypeAnno nodes
    const type_anno_fields = @typeInfo(CIR.TypeAnno).@"union".fields;
    std.debug.assert(type_anno_fields.len == CIR_TYPE_ANNO_NODE_COUNT);
}

comptime {
    // Check the number of CIR.Pattern nodes
    const pattern_fields = @typeInfo(CIR.Pattern).@"union".fields;
    std.debug.assert(pattern_fields.len == CIR_PATTERN_NODE_COUNT);
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
            const extra_data = store.extra_data.items[extra_start..];

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
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items[extra_start..];

            const anno = @as(CIR.TypeAnno.Idx, @enumFromInt(extra_data[0]));
            const anno_var = @as(types.Var, @enumFromInt(extra_data[1]));
            const header = @as(CIR.TypeHeader.Idx, @enumFromInt(extra_data[2]));
            const has_where = extra_data[3] != 0;

            const where_clause = if (has_where) blk: {
                const where_start = extra_data[4];
                const where_len = extra_data[5];
                break :blk CIR.WhereClause.Span{ .span = DataSpan.init(where_start, where_len) };
            } else null;

            return CIR.Statement{
                .s_alias_decl = .{
                    .header = header,
                    .anno = anno,
                    .anno_var = anno_var,
                    .where = where_clause,
                },
            };
        },
        .statement_nominal_decl => {
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items[extra_start..];

            const anno = @as(CIR.TypeAnno.Idx, @enumFromInt(extra_data[0]));
            const anno_var = @as(types.Var, @enumFromInt(extra_data[1]));
            const header = @as(CIR.TypeHeader.Idx, @enumFromInt(extra_data[2]));
            const has_where = extra_data[3] != 0;

            const where_clause = if (has_where) blk: {
                const where_start = extra_data[4];
                const where_len = extra_data[5];
                break :blk CIR.WhereClause.Span{ .span = DataSpan.init(where_start, where_len) };
            } else null;

            return CIR.Statement{
                .s_nominal_decl = .{
                    .header = header,
                    .anno = anno,
                    .anno_var = anno_var,
                    .where = where_clause,
                },
            };
        },
        .statement_type_anno => {
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items[extra_start..];

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
            const value_as_u32s = store.extra_data.items[node.data_1..][0..4];

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
            const extra_data = store.extra_data.items[extra_start..];

            const args_start = extra_data[0];
            const args_len = extra_data[1];

            return CIR.Expr{
                .e_call = .{
                    .args = .{ .span = .{ .start = args_start, .len = args_len } },
                    .called_via = @enumFromInt(node.data_2),
                },
            };
        },
        .expr_frac_f64 => {
            // Get value from extra_data
            const extra_data_idx = node.data_1;
            const value_as_u32s = store.extra_data.items[extra_data_idx..][0..2];
            const value_as_u64: u64 = @bitCast(value_as_u32s.*);
            const value: f64 = @bitCast(value_as_u64);

            return CIR.Expr{
                .e_frac_f64 = .{
                    .value = value,
                },
            };
        },
        .expr_frac_dec => {
            // Get value from extra_data
            const extra_data_idx = node.data_1;
            const value_as_u32s = store.extra_data.items[extra_data_idx..][0..4];
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
        .expr_bin_op => {
            return CIR.Expr{
                .e_binop = CIR.Expr.Binop.init(
                    @enumFromInt(node.data_1),
                    @enumFromInt(node.data_2),
                    @enumFromInt(node.data_3),
                ),
            };
        },
        .expr_lambda => {
            // Retrieve lambda data from extra_data
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items[extra_start..];

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
            const extra_data = store.extra_data.items[extra_start..];

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
            const extra_data = store.extra_data.items[extra_start..];

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
            const extra_data = store.extra_data.items[extra_start..];

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
        .expr_static_dispatch,
        .expr_apply,
        .expr_record_update,
        .expr_unary,
        .expr_suffix_single_question,
        .expr_record_builder,
        => {
            std.log.debug("TODO: implement getExpr for node type {?}", .{node.tag});
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
            const extra_data = store.extra_data.items[extra_start..];

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
                const packed_span = PackedDataSpan.FunctionArgs.fromU32(node.data_3);
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
        // converting these to runtime_error nodes in the CIR.
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
    const extra_data = store.extra_data.items[extra_start..];

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
    const slice = store.extra_data.items[span.span.start..(span.span.start + span.span.len)];
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
    const extra_data = store.extra_data.items[extra_start..];

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
        .pattern_record_destructure => {
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items[extra_start..];

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
            const extra_data = store.extra_data.items[extra_start..];

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
            const value_as_u32s = store.extra_data.items[extra_data_idx..][0..4];
            const value_as_i128: i128 = @bitCast(value_as_u32s.*);

            return CIR.Pattern{
                .int_literal = .{
                    .value = .{ .bytes = @bitCast(value_as_i128), .kind = .i128 },
                },
            };
        },
        .pattern_int_literal => {
            const extra_data_idx = node.data_1;
            const value_as_u32s = store.extra_data.items[extra_data_idx..][0..4];
            const value_as_i128: i128 = @bitCast(value_as_u32s.*);

            return CIR.Pattern{
                .int_literal = .{
                    .value = .{ .bytes = @bitCast(value_as_i128), .kind = .i128 },
                },
            };
        },
        .pattern_dec_literal => {
            const extra_data_idx = node.data_1;
            const value_as_u32s = store.extra_data.items[extra_data_idx..][0..4];
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
            const ret_and_effectful = node.data_3;
            const ret: CIR.TypeAnno.Idx = @enumFromInt(ret_and_effectful & 0x7FFFFFFF);
            const effectful = (ret_and_effectful & (1 << 31)) != 0;
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
                .external_decl = @enumFromInt(node.data_1),
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
pub fn addStatement(store: *NodeStore, statement: CIR.Statement, region: base.Region) CIR.Statement.Idx {
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
            const extra_start = @as(u32, @intCast(store.extra_data.items.len));

            // Store alias_tok (nullable)
            const alias_data = if (s.alias_tok) |alias| @as(u32, @bitCast(alias)) else 0;
            store.extra_data.append(store.gpa, alias_data) catch |err| exitOnOom(err);

            // Store qualifier_tok (nullable)
            const qualifier_data = if (s.qualifier_tok) |qualifier| @as(u32, @bitCast(qualifier)) else 0;
            store.extra_data.append(store.gpa, qualifier_data) catch |err| exitOnOom(err);

            // Store flags indicating which fields are present
            var flags: u32 = 0;
            if (s.alias_tok != null) flags |= 1;
            if (s.qualifier_tok != null) flags |= 2;
            store.extra_data.append(store.gpa, flags) catch |err| exitOnOom(err);

            // Store extra_start in one of the remaining data fields
            // We need to reorganize data storage since all 3 data fields are used
            // Let's put extra_start where exposes span is, and move span to extra_data
            store.extra_data.append(store.gpa, s.exposes.span.start) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, s.exposes.span.len) catch |err| exitOnOom(err);

            node.data_2 = extra_start; // Point to extra_data
            node.data_3 = 0; // SPARE

        },
        .s_alias_decl => |s| {
            node.tag = .statement_alias_decl;

            // Store type_decl data in extra_data
            const extra_start = @as(u32, @intCast(store.extra_data.items.len));

            // Store anno idx
            store.extra_data.append(store.gpa, @intFromEnum(s.anno)) catch |err| exitOnOom(err);
            // Store anno var
            store.extra_data.append(store.gpa, @intFromEnum(s.anno_var)) catch |err| exitOnOom(err);
            // Store header idx
            store.extra_data.append(store.gpa, @intFromEnum(s.header)) catch |err| exitOnOom(err);
            // Store where clause information
            if (s.where) |where_clause| {
                // Store where clause span start and len
                store.extra_data.append(store.gpa, @intFromBool(true)) catch |err| exitOnOom(err);
                store.extra_data.append(store.gpa, where_clause.span.start) catch |err| exitOnOom(err);
                store.extra_data.append(store.gpa, where_clause.span.len) catch |err| exitOnOom(err);
            } else {
                store.extra_data.append(store.gpa, @intFromBool(false)) catch |err| exitOnOom(err);
            }

            // Store the extra data start position in the node
            node.data_1 = extra_start;
        },
        .s_nominal_decl => |s| {
            node.tag = .statement_nominal_decl;

            // Store type_decl data in extra_data
            const extra_start = @as(u32, @intCast(store.extra_data.items.len));

            // Store anno idx
            store.extra_data.append(store.gpa, @intFromEnum(s.anno)) catch |err| exitOnOom(err);
            // Store anno var
            store.extra_data.append(store.gpa, @intFromEnum(s.anno_var)) catch |err| exitOnOom(err);
            // Store header idx
            store.extra_data.append(store.gpa, @intFromEnum(s.header)) catch |err| exitOnOom(err);
            // Store where clause information
            if (s.where) |where_clause| {
                // Store where clause span start and len
                store.extra_data.append(store.gpa, @intFromBool(true)) catch |err| exitOnOom(err);
                store.extra_data.append(store.gpa, where_clause.span.start) catch |err| exitOnOom(err);
                store.extra_data.append(store.gpa, where_clause.span.len) catch |err| exitOnOom(err);
            } else {
                store.extra_data.append(store.gpa, @intFromBool(false)) catch |err| exitOnOom(err);
            }

            // Store the extra data start position in the node
            node.data_1 = extra_start;
        },
        .s_type_anno => |s| {
            node.tag = .statement_type_anno;

            // Store type_anno data in extra_data
            const extra_start = @as(u32, @intCast(store.extra_data.items.len));

            // Store anno idx
            store.extra_data.append(store.gpa, @intFromEnum(s.anno)) catch |err| exitOnOom(err);
            // Store name
            store.extra_data.append(store.gpa, @bitCast(s.name)) catch |err| exitOnOom(err);
            // Store where clause information
            if (s.where) |where_clause| {
                // Store flag indicating where clause is present
                store.extra_data.append(store.gpa, 1) catch |err| exitOnOom(err);
                // Store where clause span start and len
                store.extra_data.append(store.gpa, where_clause.span.start) catch |err| exitOnOom(err);
                store.extra_data.append(store.gpa, where_clause.span.len) catch |err| exitOnOom(err);
            } else {
                // Store flag indicating where clause is not present
                store.extra_data.append(store.gpa, 0) catch |err| exitOnOom(err);
            }

            // Store the extra data start position in the node
            node.data_1 = extra_start;
        },
    }

    const node_idx = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Adds an expression node to the store.
pub fn addExpr(store: *NodeStore, expr: CIR.Expr, region: base.Region) CIR.Expr.Idx {
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
            const extra_data_start = store.extra_data.items.len;

            // Store the IntLiteralValue as i128 (16 bytes = 4 u32s)
            // We always store as i128 internally
            const value_as_i128: i128 = @bitCast(e.value.bytes);
            const value_as_u32s: [4]u32 = @bitCast(value_as_i128);
            for (value_as_u32s) |word| {
                store.extra_data.append(store.gpa, word) catch |err| exitOnOom(err);
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
        .e_frac_f64 => |e| {
            node.tag = .expr_frac_f64;

            // Store the f64 value in extra_data
            const extra_data_start = store.extra_data.items.len;
            const value_as_u64: u64 = @bitCast(e.value);
            const value_as_u32s: [2]u32 = @bitCast(value_as_u64);
            for (value_as_u32s) |word| {
                store.extra_data.append(store.gpa, word) catch |err| exitOnOom(err);
            }

            // Store the extra_data index in data_1
            node.data_1 = @intCast(extra_data_start);
        },
        .e_frac_dec => |e| {
            node.tag = .expr_frac_dec;

            // Store the RocDec value in extra_data
            const extra_data_start = store.extra_data.items.len;
            const value_as_i128: i128 = e.value.num;
            const value_as_u32s: [4]u32 = @bitCast(value_as_i128);
            for (value_as_u32s) |word| {
                store.extra_data.append(store.gpa, word) catch |err| exitOnOom(err);
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
        .e_dot_access => |e| {
            node.tag = .expr_dot_access;
            node.data_1 = @intFromEnum(e.receiver);
            node.data_2 = @bitCast(e.field_name);
            if (e.args) |args| {
                // Use PackedDataSpan for efficient storage - FunctionArgs config is good for method call args
                std.debug.assert(PackedDataSpan.FunctionArgs.canFit(args.span));
                const packed_span = PackedDataSpan.FunctionArgs.fromDataSpanUnchecked(args.span);
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
            const extra_data_start = @as(u32, @intCast(store.extra_data.items.len));
            store.extra_data.append(store.gpa, @intFromEnum(e.cond)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, e.branches.span.start) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, e.branches.span.len) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @intFromEnum(e.exhaustive)) catch |err| exitOnOom(err);
            node.data_1 = extra_data_start;
        },
        .e_if => |e| {
            // Store def data in extra_data. We store the following fields:
            // 1. Branches span start
            // 2. Branches span end
            // 3. Final else expr idx
            const extra_start = @as(u32, @intCast(store.extra_data.items.len));
            const num_extra_items = 3;
            store.extra_data.append(store.gpa, e.branches.span.start) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, e.branches.span.len) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @intFromEnum(e.final_else)) catch |err| exitOnOom(err);

            node.tag = .expr_if_then_else;
            node.data_1 = extra_start;
            node.data_2 = extra_start + num_extra_items;

            std.debug.assert(node.data_2 == store.extra_data.items.len);
        },
        .e_call => |e| {
            node.tag = .expr_call;

            // Store call data in extra_data
            const extra_data_start = @as(u32, @intCast(store.extra_data.items.len));

            // Store args span start
            store.extra_data.append(store.gpa, e.args.span.start) catch |err| exitOnOom(err);
            // Store args span length
            store.extra_data.append(store.gpa, e.args.span.len) catch |err| exitOnOom(err);

            node.data_1 = extra_data_start;
            node.data_2 = @intFromEnum(e.called_via);
        },
        .e_record => |e| {
            node.tag = .expr_record;

            const extra_data_start = @as(u32, @intCast(store.extra_data.items.len));

            // Store fields span start
            store.extra_data.append(store.gpa, e.fields.span.start) catch |err| exitOnOom(err);
            // Store fields span length
            store.extra_data.append(store.gpa, e.fields.span.len) catch |err| exitOnOom(err);
            // Store extension (0 if null)
            const ext_value = if (e.ext) |ext| @intFromEnum(ext) else 0;
            store.extra_data.append(store.gpa, ext_value) catch |err| exitOnOom(err);

            node.data_1 = extra_data_start;
            node.data_2 = 0; // Unused
        },
        .e_empty_record => |_| {
            node.tag = .expr_empty_record;
        },
        .e_zero_argument_tag => |e| {
            node.tag = .expr_zero_argument_tag;

            // Store zero argument tag data in extra_data
            const extra_data_start = @as(u32, @intCast(store.extra_data.items.len));
            store.extra_data.append(store.gpa, @bitCast(e.closure_name)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @intFromEnum(e.variant_var)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @intFromEnum(e.ext_var)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @bitCast(e.name)) catch |err| exitOnOom(err);
            node.data_1 = extra_data_start;
        },
        .e_lambda => |e| {
            node.tag = .expr_lambda;

            // Store lambda data in extra_data
            const extra_data_start = @as(u32, @intCast(store.extra_data.items.len));

            // Store args span start
            store.extra_data.append(store.gpa, e.args.span.start) catch |err| exitOnOom(err);
            // Store args span length
            store.extra_data.append(store.gpa, e.args.span.len) catch |err| exitOnOom(err);
            // Store body index
            store.extra_data.append(store.gpa, @intFromEnum(e.body)) catch |err| exitOnOom(err);

            node.data_1 = extra_data_start;
        },
        .e_binop => |e| {
            node.tag = .expr_bin_op;
            node.data_1 = @intFromEnum(e.op);
            node.data_2 = @intFromEnum(e.lhs);
            node.data_3 = @intFromEnum(e.rhs);
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

    const node_idx = store.nodes.append(store.gpa, node);
    // For e_lookup_external, use the region from the expression itself
    const actual_region = switch (expr) {
        .e_lookup_external => |e| e.region,
        else => region,
    };
    _ = store.regions.append(store.gpa, actual_region);
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Adds a record field to the store.
pub fn addRecordField(store: *NodeStore, recordField: CIR.RecordField, region: base.Region) CIR.RecordField.Idx {
    const node = Node{
        .data_1 = @bitCast(recordField.name),
        .data_2 = @intFromEnum(recordField.value),
        .data_3 = 0,
        .tag = .record_field,
    };

    const nid = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a record destructuring to the store.
pub fn addRecordDestruct(store: *NodeStore, record_destruct: CIR.Pattern.RecordDestruct, region: base.Region) CIR.Pattern.RecordDestruct.Idx {
    var node = Node{
        .data_1 = @bitCast(record_destruct.label),
        .data_2 = @bitCast(record_destruct.ident),
        .data_3 = 0,
        .tag = .record_destruct,
    };

    // Store kind in extra_data if it's not Required
    switch (record_destruct.kind) {
        .Required => {
            // No extra data needed
        },

        .SubPattern => |sub_pattern| {
            const extra_data_start = @as(u32, @intCast(store.extra_data.items.len));

            // Store kind tag (1 for SubPattern)
            store.extra_data.append(store.gpa, 1) catch |err| exitOnOom(err);
            // Store sub-pattern index
            store.extra_data.append(store.gpa, @intFromEnum(sub_pattern)) catch |err| exitOnOom(err);

            node.data_3 = extra_data_start;
        },
    }

    const nid = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a 'match' branch to the store.
pub fn addMatchBranch(store: *NodeStore, branch: CIR.Expr.Match.Branch, region: base.Region) CIR.Expr.Match.Branch.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .tag = .match_branch,
    };

    // Store when branch data in extra_data
    const extra_data_start = @as(u32, @intCast(store.extra_data.items.len));
    store.extra_data.append(store.gpa, branch.patterns.span.start) catch |err| exitOnOom(err);
    store.extra_data.append(store.gpa, branch.patterns.span.len) catch |err| exitOnOom(err);
    store.extra_data.append(store.gpa, @intFromEnum(branch.value)) catch |err| exitOnOom(err);
    const guard_idx = if (branch.guard) |g| @intFromEnum(g) else 0;
    store.extra_data.append(store.gpa, guard_idx) catch |err| exitOnOom(err);
    store.extra_data.append(store.gpa, @intFromEnum(branch.redundant)) catch |err| exitOnOom(err);
    node.data_1 = extra_data_start;

    const nid = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a 'match' branch to the store.
pub fn addMatchBranchPattern(store: *NodeStore, branchPattern: CIR.Expr.Match.BranchPattern, region: base.Region) CIR.Expr.Match.BranchPattern.Idx {
    const node = Node{
        .data_1 = @intFromEnum(branchPattern.pattern),
        .data_2 = @as(u32, @intFromBool(branchPattern.degenerate)),
        .data_3 = 0,
        .tag = .match_branch_pattern,
    };
    const nid = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a 'where' clause to the store.
pub fn addWhereClause(store: *NodeStore, whereClause: CIR.WhereClause, region: base.Region) CIR.WhereClause.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .tag = .where_clause,
    };

    // Store where clause data in extra_data
    const extra_data_start = @as(u32, @intCast(store.extra_data.items.len));

    switch (whereClause) {
        .mod_method => |mod_method| {
            // Store discriminant (0 for mod_method)
            store.extra_data.append(store.gpa, 0) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @bitCast(mod_method.var_name)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @bitCast(mod_method.method_name)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, mod_method.args.span.start) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, mod_method.args.span.len) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @intFromEnum(mod_method.ret_anno)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @intFromEnum(mod_method.external_decl)) catch |err| exitOnOom(err);
        },
        .mod_alias => |mod_alias| {
            // Store discriminant (1 for mod_alias)
            store.extra_data.append(store.gpa, 1) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @bitCast(mod_alias.var_name)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @bitCast(mod_alias.alias_name)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @intFromEnum(mod_alias.external_decl)) catch |err| exitOnOom(err);
        },
        .malformed => |malformed| {
            // Store discriminant (2 for malformed)
            store.extra_data.append(store.gpa, 2) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @intFromEnum(malformed.diagnostic)) catch |err| exitOnOom(err);
        },
    }

    node.data_1 = extra_data_start;

    const nid = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a pattern to the store.
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
        .record_destructure => |p| {
            node.tag = .pattern_record_destructure;

            // Store record destructure data in extra_data
            const extra_data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, p.destructs.span.start);
            try store.extra_data.append(store.gpa, p.destructs.span.len);
            try store.extra_data.append(store.gpa, @intFromEnum(p.ext_var));
            try store.extra_data.append(store.gpa, @intFromEnum(p.whole_var));
            node.data_1 = extra_data_start;
        },
        .list => |p| {
            node.tag = .pattern_list;

            // Store list pattern data in extra_data
            const extra_data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, p.patterns.span.start);
            try store.extra_data.append(store.gpa, p.patterns.span.len);
            try store.extra_data.append(store.gpa, @intFromEnum(p.elem_var));
            try store.extra_data.append(store.gpa, @intFromEnum(p.list_var));

            // Store rest_info
            if (p.rest_info) |rest| {
                try store.extra_data.append(store.gpa, 1); // has rest_info
                try store.extra_data.append(store.gpa, rest.index);
                if (rest.pattern) |pattern_idx| {
                    try store.extra_data.append(store.gpa, 1); // has pattern
                    try store.extra_data.append(store.gpa, @intFromEnum(pattern_idx));
                } else {
                    try store.extra_data.append(store.gpa, 0); // no pattern
                }
            } else {
                try store.extra_data.append(store.gpa, 0); // no rest_info
            }

            node.data_1 = extra_data_start;
        },
        .tuple => |p| {
            node.tag = .pattern_tuple;
            node.data_1 = p.patterns.span.start;
            node.data_2 = p.patterns.span.len;
        },
        .int_literal => |p| {
            node.tag = .pattern_int_literal;
            // Store the value in extra_data
            const extra_data_start = store.extra_data.items.len;
            const value_as_u32s: [4]u32 = @bitCast(p.value.bytes);
            for (value_as_u32s) |word| {
                try store.extra_data.append(store.gpa, word);
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
            const extra_data_start = store.extra_data.items.len;
            const value_as_u32s: [4]u32 = @bitCast(p.value.num);
            for (value_as_u32s) |word| {
                try store.extra_data.append(store.gpa, word);
            }
            node.data_1 = @intCast(extra_data_start);
        },
        .str_literal => |p| {
            node.tag = .pattern_str_literal;
            node.data_1 = @intFromEnum(p.literal);
        },

        .underscore => {
            node.tag = .pattern_underscore;
        },
        .runtime_error => |e| {
            node.tag = .malformed;
            node.data_1 = @intFromEnum(e.diagnostic);
        },
    }

    const node_idx = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Adds a pattern record field to the store.
pub fn addPatternRecordField(store: *NodeStore, patternRecordField: CIR.PatternRecordField) CIR.PatternRecordField.Idx {
    _ = store;
    _ = patternRecordField;

    return .{ .id = @enumFromInt(0) };
}

/// Adds a type annotation to the store.
pub fn addTypeAnno(store: *NodeStore, typeAnno: CIR.TypeAnno, region: base.Region) CIR.TypeAnno.Idx {
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
            node.data_3 = @intFromEnum(f.ret) | (if (f.effectful) @as(u32, 1) << 31 else 0);
            node.tag = .ty_fn;
        },
        .parens => |p| {
            node.data_1 = @intFromEnum(p.anno);
            node.tag = .ty_parens;
        },
        .ty_lookup_external => |tle| {
            node.data_1 = @intFromEnum(tle.external_decl);
            node.tag = .ty_lookup_external;
        },
        .malformed => |m| {
            node.data_1 = @intFromEnum(m.diagnostic);
            node.tag = .ty_malformed;
        },
    }

    const nid = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a type header to the store.
pub fn addTypeHeader(store: *NodeStore, typeHeader: CIR.TypeHeader, region: base.Region) CIR.TypeHeader.Idx {
    const node = Node{
        .data_1 = @bitCast(typeHeader.name),
        .data_2 = typeHeader.args.span.start,
        .data_3 = typeHeader.args.span.len,
        .tag = .type_header,
    };

    const nid = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an annotation record field to the store.
pub fn addAnnoRecordField(store: *NodeStore, annoRecordField: CIR.TypeAnno.RecordField, region: base.Region) CIR.TypeAnno.RecordField.Idx {
    const node = Node{
        .data_1 = @bitCast(annoRecordField.name),
        .data_2 = @intFromEnum(annoRecordField.ty),
        .data_3 = 0,
        .tag = .ty_record_field,
    };

    const nid = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an annotation to the store.
pub fn addAnnotation(store: *NodeStore, annotation: CIR.Annotation, region: base.Region) CIR.Annotation.Idx {
    const node = Node{
        .data_1 = @intFromEnum(annotation.signature),
        .data_2 = @intFromEnum(annotation.type_anno),
        .data_3 = 0,
        .tag = .annotation,
    };

    const nid = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an exposed item to the store.
pub fn addExposedItem(store: *NodeStore, exposedItem: CIR.ExposedItem, region: base.Region) CIR.ExposedItem.Idx {
    const node = Node{
        .data_1 = @bitCast(exposedItem.name),
        .data_2 = if (exposedItem.alias) |alias| @bitCast(alias) else 0,
        .data_3 = @intFromBool(exposedItem.is_wildcard),
        .tag = .exposed_item,
    };

    const nid = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a definition to the store.
pub fn addDef(store: *NodeStore, def: CIR.Def, region: base.Region) CIR.Def.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .tag = .def,
    };

    // Store def data in extra_data
    const extra_start = @as(u32, @intCast(store.extra_data.items.len));

    // Store pattern idx
    store.extra_data.append(store.gpa, @intFromEnum(def.pattern)) catch |err| exitOnOom(err);
    // Store expr idx
    store.extra_data.append(store.gpa, @intFromEnum(def.expr)) catch |err| exitOnOom(err);
    // Store kind tag as two u32's
    const kind_encoded = def.kind.encode();
    store.extra_data.append(store.gpa, kind_encoded[0]) catch |err| exitOnOom(err);
    store.extra_data.append(store.gpa, kind_encoded[1]) catch |err| exitOnOom(err);
    // Store annotation idx (0 if null)
    const anno_idx = if (def.annotation) |anno| @intFromEnum(anno) else 0;
    store.extra_data.append(store.gpa, anno_idx) catch |err| exitOnOom(err);

    // Store the extra data range in the node
    node.data_1 = extra_start;
    node.data_2 = 5; // Number of extra data items

    const nid = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Retrieves a definition from the store.
pub fn getDef(store: *const NodeStore, def_idx: CIR.Def.Idx) CIR.Def {
    const nid: Node.Idx = @enumFromInt(@intFromEnum(def_idx));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .def);

    const extra_start = node.data_1;
    const extra_data = store.extra_data.items[extra_start..];

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

    // Retrieve kind from extra_data if it exists
    const kind = if (node.data_3 != 0) blk: {
        const extra_start = node.data_3;
        const extra_data = store.extra_data.items[extra_start..];
        const kind_tag = extra_data[0];

        break :blk switch (kind_tag) {
            0 => CIR.Pattern.RecordDestruct.Kind.Required,
            1 => CIR.Pattern.RecordDestruct.Kind{ .SubPattern = @enumFromInt(extra_data[1]) },
            else => unreachable,
        };
    } else CIR.Pattern.RecordDestruct.Kind.Required;

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
pub fn addScratch(store: *NodeStore, comptime field_name: []const u8, idx: anytype) void {
    @field(store, field_name).append(store.gpa, idx);
}

/// Generic function to clear any scratch buffer from a given position
pub fn clearScratchFrom(store: *NodeStore, comptime field_name: []const u8, start: u32) void {
    @field(store, field_name).clearFrom(start);
}

/// Generic function to create a span from any scratch buffer
pub fn spanFrom(store: *NodeStore, comptime field_name: []const u8, comptime SpanType: type, start: u32) SpanType {
    const scratch_field = &@field(store, field_name);
    const end = scratch_field.top();
    defer scratch_field.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(scratch_field.items.items[i])) catch |err| exitOnOom(err);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Returns the top of the scratch expressions buffer.
pub fn scratchExprTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_exprs");
}

/// Adds a scratch expression to temporary storage.
pub fn addScratchExpr(store: *NodeStore, idx: CIR.Expr.Idx) void {
    store.addScratch("scratch_exprs", idx);
}

/// Adds a statement index to the scratch statements list for building spans.
pub fn addScratchStatement(store: *NodeStore, idx: CIR.Statement.Idx) void {
    store.addScratch("scratch_statements", idx);
}

/// Computes the span of an expression starting from a given index.
pub fn exprSpanFrom(store: *NodeStore, start: u32) CIR.Expr.Span {
    return store.spanFrom("scratch_exprs", CIR.Expr.Span, start);
}

/// Creates a statement span from the given start position to the current top of scratch statements.
pub fn statementSpanFrom(store: *NodeStore, start: u32) CIR.Statement.Span {
    return store.spanFrom("scratch_statements", CIR.Statement.Span, start);
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
pub fn addScratchDef(store: *NodeStore, idx: CIR.Def.Idx) void {
    store.addScratch("scratch_defs", idx);
}

/// Adds a type annotation to the scratch buffer.
pub fn addScratchTypeAnno(store: *NodeStore, idx: CIR.TypeAnno.Idx) void {
    store.addScratch("scratch_type_annos", idx);
}

/// Adds a where clause to the scratch buffer.
pub fn addScratchWhereClause(store: *NodeStore, idx: CIR.WhereClause.Idx) void {
    store.addScratch("scratch_where_clauses", idx);
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
pub fn typeAnnoSpanFrom(store: *NodeStore, start: u32) CIR.TypeAnno.Span {
    return store.spanFrom("scratch_type_annos", CIR.TypeAnno.Span, start);
}

/// Returns a span from the scratch anno record fields starting at the given index.
pub fn annoRecordFieldSpanFrom(store: *NodeStore, start: u32) CIR.TypeAnno.RecordField.Span {
    return store.spanFrom("scratch_anno_record_fields", CIR.TypeAnno.RecordField.Span, start);
}

/// Returns a span from the scratch record fields starting at the given index.
pub fn recordFieldSpanFrom(store: *NodeStore, start: u32) CIR.RecordField.Span {
    return store.spanFrom("scratch_record_fields", CIR.RecordField.Span, start);
}

/// Returns a span from the scratch where clauses starting at the given index.
pub fn whereClauseSpanFrom(store: *NodeStore, start: u32) CIR.WhereClause.Span {
    return store.spanFrom("scratch_where_clauses", CIR.WhereClause.Span, start);
}

/// Returns the current top of the scratch exposed items buffer.
pub fn scratchExposedItemTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_exposed_items");
}

/// Adds an exposed item to the scratch buffer.
pub fn addScratchExposedItem(store: *NodeStore, idx: CIR.ExposedItem.Idx) void {
    store.addScratch("scratch_exposed_items", idx);
}

/// Creates a span from the scratch exposed items starting at the given index.
pub fn exposedItemSpanFrom(store: *NodeStore, start: u32) CIR.ExposedItem.Span {
    return store.spanFrom("scratch_exposed_items", CIR.ExposedItem.Span, start);
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
pub fn addScratchAnnoRecordField(store: *NodeStore, idx: CIR.TypeAnno.RecordField.Idx) void {
    store.addScratch("scratch_anno_record_fields", idx);
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
pub fn defSpanFrom(store: *NodeStore, start: u32) CIR.Def.Span {
    return store.spanFrom("scratch_defs", CIR.Def.Span, start);
}

/// Retrieves a slice of record destructures from the store.
pub fn recordDestructSpanFrom(store: *NodeStore, start: u32) CIR.Pattern.RecordDestruct.Span {
    return store.spanFrom("scratch_record_destructs", CIR.Pattern.RecordDestruct.Span, start);
}

/// Returns the current top of the scratch patterns buffer.
pub fn scratchPatternTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_patterns");
}

/// Adds a pattern to the scratch patterns list for building spans.
pub fn addScratchPattern(store: *NodeStore, idx: CIR.Pattern.Idx) void {
    store.addScratch("scratch_patterns", idx);
}

/// Returns the current top of the scratch record destructures buffer.
pub fn scratchRecordDestructTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_record_destructs");
}

/// Adds a record destructure to the scratch record destructures list for building spans.
pub fn addScratchRecordDestruct(store: *NodeStore, idx: CIR.Pattern.RecordDestruct.Idx) void {
    store.addScratch("scratch_record_destructs", idx);
}

/// Creates a pattern span from the given start position to the current top of scratch patterns.
pub fn patternSpanFrom(store: *NodeStore, start: u32) CIR.Pattern.Span {
    return store.spanFrom("scratch_patterns", CIR.Pattern.Span, start);
}

/// Clears scratch definitions starting from a specified index.
pub fn clearScratchDefsFrom(store: *NodeStore, start: u32) void {
    store.clearScratchFrom("scratch_defs", start);
}

/// Creates a slice corresponding to a span.
pub fn sliceFromSpan(store: *const NodeStore, comptime T: type, span: base.DataSpan) []T {
    return @ptrCast(store.extra_data.items[span.start..][0..span.len]);
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
    return @as(T, @enumFromInt(store.extra_data.items[span.start]));
}

/// Creates a slice corresponding to a span.
pub fn lastFromSpan(store: *const NodeStore, comptime T: type, span: base.DataSpan) T {
    return @as(T, @enumFromInt(store.extra_data.items[span.start + span.len - 1]));
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
pub fn addScratchIfBranch(store: *NodeStore, if_branch_idx: CIR.Expr.IfBranch.Idx) void {
    store.addScratch("scratch_if_branches", if_branch_idx);
}

/// Creates an if branch span from the given start position to the current top of scratch if branches.
pub fn ifBranchSpanFrom(store: *NodeStore, start: u32) CIR.Expr.IfBranch.Span {
    return store.spanFrom("scratch_if_branches", CIR.Expr.IfBranch.Span, start);
}

/// Adds an if branch to the store and returns its index.
pub fn addIfBranch(store: *NodeStore, if_branch: CIR.Expr.IfBranch, region: base.Region) CIR.Expr.IfBranch.Idx {
    const node = Node{
        .data_1 = @intFromEnum(if_branch.cond),
        .data_2 = @intFromEnum(if_branch.body),
        .data_3 = 0,
        .tag = .if_branch,
    };
    const node_idx = store.nodes.append(store.gpa, node);
    _ = store.regions.append(store.gpa, region);
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
pub fn addDiagnostic(store: *NodeStore, reason: CIR.Diagnostic) CIR.Diagnostic.Idx {
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
        .too_long_single_quote => |r| {
            node.tag = .diag_too_long_single_quote;
            region = r.region;
        },
        .empty_single_quote => |r| {
            node.tag = .diag_empty_single_quote;
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
            const extra_start = @as(u32, @intCast(store.extra_data.items.len));
            store.extra_data.append(store.gpa, r.original_region.start.offset) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, r.original_region.end.offset) catch |err| exitOnOom(err);
            node.data_2 = extra_start;
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
            node.data_2 = r.original_region.start.offset;
            node.data_3 = r.original_region.end.offset | (@as(u32, @intFromBool(r.cross_scope)) << 31);
        },
        .type_parameter_conflict => |r| {
            node.tag = .diag_type_parameter_conflict;
            region = r.region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = @bitCast(r.parameter_name);
            node.data_3 = r.original_region.start.offset;
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
    }

    const nid = @intFromEnum(store.nodes.append(store.gpa, node));
    _ = store.regions.append(store.gpa, region);

    // append to our scratch so we can get a span later of all our diagnostics
    store.addScratch("scratch_diagnostics", @as(CIR.Diagnostic.Idx, @enumFromInt(nid)));

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
/// The malformed node will generate a runtime_error in the CIR that properly
/// references the diagnostic index.
pub fn addMalformed(store: *NodeStore, comptime t: type, reason: CIR.Diagnostic) t {
    // First create the diagnostic node
    const diagnostic_idx = store.addDiagnostic(reason);

    // Then create a malformed node that references the diagnostic
    const malformed_node = Node{
        .data_1 = @intFromEnum(diagnostic_idx),
        .data_2 = 0,
        .data_3 = 0,
        .tag = .malformed,
    };

    const malformed_nid = @intFromEnum(store.nodes.append(store.gpa, malformed_node));
    _ = store.regions.append(store.gpa, reason.toRegion());
    return @enumFromInt(malformed_nid);
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
        .diag_too_long_single_quote => return CIR.Diagnostic{ .too_long_single_quote = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_empty_single_quote => return CIR.Diagnostic{ .empty_single_quote = .{
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
            const extra_data = store.extra_data.items[node.data_2..];
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
                .end = .{ .offset = @intCast(node.data_3 & 0x7FFFFFFF) },
            },
        } },
        .diag_type_shadowed_warning => return CIR.Diagnostic{ .type_shadowed_warning = .{
            .name = @bitCast(node.data_1),
            .region = store.getRegionAt(node_idx),
            .original_region = .{
                .start = .{ .offset = @intCast(node.data_2) },
                .end = .{ .offset = @intCast(node.data_3 & 0x7FFFFFFF) },
            },
            .cross_scope = (node.data_3 & 0x80000000) != 0,
        } },
        .diag_type_parameter_conflict => return CIR.Diagnostic{ .type_parameter_conflict = .{
            .name = @bitCast(node.data_1),
            .parameter_name = @bitCast(node.data_2),
            .region = store.getRegionAt(node_idx),
            .original_region = .{
                .start = .{ .offset = @intCast(node.data_3) },
                .end = .{ .offset = @intCast(node.data_3) },
            },
        } },
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
        else => {
            std.debug.print("Error: getDiagnostic called with non-diagnostic node!\n", .{});
            std.debug.print("  Node tag: {}\n", .{node.tag});
            std.debug.print("  Diagnostic index: {}\n", .{diagnostic});
            std.debug.print("  Node index: {}\n", .{node_idx});
            const region = store.getRegionAt(node_idx);
            std.debug.print("  Region: {}..{}\n", .{ region.start.offset, region.end.offset });
            std.debug.print("\nThis indicates that a non-diagnostic node was added to the diagnostics list.\n", .{});
            std.debug.print("Check that addDiagnostic is only called with CIR.Diagnostic values,\n", .{});
            std.debug.print("and that no other nodes are being added to scratch_diagnostics.\n", .{});
            @panic("unreachable, node is not a diagnostic tag");
        },
    }
}

/// Computes the span of a diagnostic starting from a given index.
pub fn diagnosticSpanFrom(store: *NodeStore, start: u32) CIR.Diagnostic.Span {
    return store.spanFrom("scratch_diagnostics", CIR.Diagnostic.Span, start);
}

/// Ensure the node store has capacity for at least the requested number of
/// slots. Then return the *final* index.
pub fn predictNodeIndex(store: *NodeStore, count: u32) Node.Idx {
    const start_idx = store.nodes.len();
    store.nodes.ensureTotalCapacity(store.gpa, start_idx + count);
    // Return where the LAST node will actually be placed
    return @enumFromInt(start_idx + count - 1);
}

/// Adds an type variable slot to the store.
pub fn addTypeVarSlot(store: *NodeStore, parent_node_idx: Node.Idx, region: base.Region) Node.Idx {
    const nid = store.nodes.append(store.gpa, .{
        .tag = .type_var_slot,
        .data_1 = @intFromEnum(parent_node_idx),
        .data_2 = 0,
        .data_3 = 0,
    });
    _ = store.regions.append(store.gpa, region);
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
        _ = store.regions.append(store.gpa, region);
    }
}

/// Return the current top index for scratch match branches.
pub fn scratchMatchBranchTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_match_branches");
}

/// Add a match branch index to the scratch buffer.
pub fn addScratchMatchBranch(store: *NodeStore, branch_idx: CIR.Expr.Match.Branch.Idx) void {
    store.addScratch("scratch_match_branches", branch_idx);
}

/// Create a span from the scratch match branches starting at the given index.
pub fn matchBranchSpanFrom(store: *NodeStore, start: u32) CIR.Expr.Match.Branch.Span {
    return store.spanFrom("scratch_match_branches", CIR.Expr.Match.Branch.Span, start);
}

/// Return the current top index for scratch match branch patterns.
pub fn scratchMatchBranchPatternTop(store: *NodeStore) u32 {
    return store.scratchTop("scratch_match_branch_patterns");
}

/// Add a match branch pattern index to the scratch buffer.
pub fn addScratchMatchBranchPattern(store: *NodeStore, pattern_idx: CIR.Expr.Match.BranchPattern.Idx) void {
    store.addScratch("scratch_match_branch_patterns", pattern_idx);
}

/// Create a span from the scratch match branch patterns starting at the given index.
pub fn matchBranchPatternSpanFrom(store: *NodeStore, start: u32) CIR.Expr.Match.BranchPattern.Span {
    return store.spanFrom("scratch_match_branch_patterns", CIR.Expr.Match.BranchPattern.Span, start);
}

/// Calculate the size needed to serialize this NodeStore
pub fn serializedSize(self: *const NodeStore) usize {
    // We only serialize nodes, regions, and extra_data (the scratch arrays are transient)
    const raw_size = self.nodes.serializedSize() +
        self.regions.serializedSize() +
        @sizeOf(u32) + // extra_data length
        (self.extra_data.items.len * @sizeOf(u32));
    // Align to SERIALIZATION_ALIGNMENT to maintain alignment for subsequent data
    return std.mem.alignForward(usize, raw_size, SERIALIZATION_ALIGNMENT);
}

/// Serialize this NodeStore into the provided buffer
/// Buffer must be at least serializedSize() bytes and properly aligned
pub fn serializeInto(self: *const NodeStore, buffer: []align(SERIALIZATION_ALIGNMENT) u8) ![]u8 {
    const size = self.serializedSize();
    if (buffer.len < size) return error.BufferTooSmall;

    var offset: usize = 0;

    // Serialize nodes - cast to proper alignment for Node type
    const nodes_slice = try self.nodes.serializeInto(@as([]align(SERIALIZATION_ALIGNMENT) u8, @alignCast(buffer[offset..])));
    offset += nodes_slice.len;

    // Serialize regions
    const regions_slice = try self.regions.serializeInto(@as([]align(SERIALIZATION_ALIGNMENT) u8, @alignCast(buffer[offset..])));
    offset += regions_slice.len;

    // Serialize extra_data length
    const extra_len_ptr = @as(*u32, @ptrCast(@alignCast(buffer.ptr + offset)));
    extra_len_ptr.* = @intCast(self.extra_data.items.len);
    offset += @sizeOf(u32);

    // Serialize extra_data items
    if (self.extra_data.items.len > 0) {
        const extra_ptr = @as([*]u32, @ptrCast(@alignCast(buffer.ptr + offset)));
        @memcpy(extra_ptr, self.extra_data.items);
        offset += self.extra_data.items.len * @sizeOf(u32);
    }

    // Zero out any padding bytes
    if (offset < size) {
        @memset(buffer[offset..size], 0);
    }

    return buffer[0..size];
}

/// Deserialize a NodeStore from the provided buffer
pub fn deserializeFrom(buffer: []align(@alignOf(Node)) const u8, allocator: std.mem.Allocator) !NodeStore {
    var offset: usize = 0;

    // Deserialize nodes - cast to proper alignment for Node type
    const nodes_buffer = @as([]align(@alignOf(Node)) const u8, @alignCast(buffer[offset..]));
    const nodes = try Node.List.deserializeFrom(nodes_buffer, allocator);
    offset += nodes.serializedSize();

    // Deserialize regions
    const regions_buffer = @as([]align(@alignOf(Region)) const u8, @alignCast(buffer[offset..]));
    const regions = try Region.List.deserializeFrom(regions_buffer, allocator);
    offset += regions.serializedSize();

    // Deserialize extra_data length
    if (buffer.len < offset + @sizeOf(u32)) return error.BufferTooSmall;
    const extra_len = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
    offset += @sizeOf(u32);

    // Deserialize extra_data items
    var extra_data = try std.ArrayListUnmanaged(u32).initCapacity(allocator, extra_len);
    if (extra_len > 0) {
        const remaining = buffer.len - offset;
        const expected = extra_len * @sizeOf(u32);
        if (remaining < expected) return error.BufferTooSmall;

        const extra_ptr = @as([*]const u32, @ptrCast(@alignCast(buffer.ptr + offset)));
        extra_data.appendSliceAssumeCapacity(extra_ptr[0..extra_len]);
    }

    // Create NodeStore with empty scratch arrays
    return NodeStore{
        .gpa = allocator,
        .nodes = nodes,
        .regions = regions,
        .extra_data = extra_data,
        // All scratch arrays start empty
        .scratch_statements = base.Scratch(CIR.Statement.Idx){ .items = .{} },
        .scratch_exprs = base.Scratch(CIR.Expr.Idx){ .items = .{} },
        .scratch_record_fields = base.Scratch(CIR.RecordField.Idx){ .items = .{} },
        .scratch_match_branches = base.Scratch(CIR.Expr.Match.Branch.Idx){ .items = .{} },
        .scratch_match_branch_patterns = base.Scratch(CIR.Expr.Match.BranchPattern.Idx){ .items = .{} },
        .scratch_if_branches = base.Scratch(CIR.Expr.IfBranch.Idx){ .items = .{} },
        .scratch_where_clauses = base.Scratch(CIR.WhereClause.Idx){ .items = .{} },
        .scratch_patterns = base.Scratch(CIR.Pattern.Idx){ .items = .{} },
        .scratch_pattern_record_fields = base.Scratch(CIR.PatternRecordField.Idx){ .items = .{} },
        .scratch_type_annos = base.Scratch(CIR.TypeAnno.Idx){ .items = .{} },
        .scratch_anno_record_fields = base.Scratch(CIR.TypeAnno.RecordField.Idx){ .items = .{} },
        .scratch_exposed_items = base.Scratch(CIR.ExposedItem.Idx){ .items = .{} },
        .scratch_defs = base.Scratch(CIR.Def.Idx){ .items = .{} },
        .scratch_diagnostics = base.Scratch(CIR.Diagnostic.Idx){ .items = .{} },
        .scratch_record_destructs = base.Scratch(CIR.Pattern.RecordDestruct.Idx){ .items = .{} },
    };
}
