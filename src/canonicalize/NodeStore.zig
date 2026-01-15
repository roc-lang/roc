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

const Allocator = std.mem.Allocator;
const CompactWriter = collections.CompactWriter;
const SafeList = collections.SafeList;
const RocDec = builtins.dec.RocDec;
const DataSpan = base.DataSpan;
const Region = base.Region;
const Ident = base.Ident;
const FunctionArgs = base.FunctionArgs;

/// When storing optional indices/values where 0 is a valid value, we add this offset
/// to distinguish "value is 0" from "value is null". This is a common pattern when
/// packing optional data into u32 fields where 0 would otherwise be ambiguous.
const OPTIONAL_VALUE_OFFSET: u32 = 1;

const NodeStore = @This();

gpa: Allocator,
nodes: Node.List,
regions: Region.List,
extra_data: collections.SafeList(u32),
scratch: ?*Scratch, // Nullable because when we deserialize a NodeStore, we don't bother to reinitialize scratch.

const Scratch = struct {
    statements: base.Scratch(CIR.Statement.Idx),
    exprs: base.Scratch(CIR.Expr.Idx),
    record_fields: base.Scratch(CIR.RecordField.Idx),
    match_branches: base.Scratch(CIR.Expr.Match.Branch.Idx),
    match_branch_patterns: base.Scratch(CIR.Expr.Match.BranchPattern.Idx),
    if_branches: base.Scratch(CIR.Expr.IfBranch.Idx),
    where_clauses: base.Scratch(CIR.WhereClause.Idx),
    patterns: base.Scratch(CIR.Pattern.Idx),
    record_destructs: base.Scratch(CIR.Pattern.RecordDestruct.Idx),
    type_annos: base.Scratch(CIR.TypeAnno.Idx),
    anno_record_fields: base.Scratch(CIR.TypeAnno.RecordField.Idx),
    exposed_items: base.Scratch(CIR.ExposedItem.Idx),
    defs: base.Scratch(CIR.Def.Idx),
    diagnostics: base.Scratch(CIR.Diagnostic.Idx),
    captures: base.Scratch(CIR.Expr.Capture.Idx),

    fn init(gpa: Allocator) Allocator.Error!*@This() {
        const ptr = try gpa.create(Scratch);

        ptr.* = .{
            .statements = try base.Scratch(CIR.Statement.Idx).init(gpa),
            .exprs = try base.Scratch(CIR.Expr.Idx).init(gpa),
            .record_fields = try base.Scratch(CIR.RecordField.Idx).init(gpa),
            .match_branches = try base.Scratch(CIR.Expr.Match.Branch.Idx).init(gpa),
            .match_branch_patterns = try base.Scratch(CIR.Expr.Match.BranchPattern.Idx).init(gpa),
            .if_branches = try base.Scratch(CIR.Expr.IfBranch.Idx).init(gpa),
            .where_clauses = try base.Scratch(CIR.WhereClause.Idx).init(gpa),
            .patterns = try base.Scratch(CIR.Pattern.Idx).init(gpa),
            .record_destructs = try base.Scratch(CIR.Pattern.RecordDestruct.Idx).init(gpa),
            .type_annos = try base.Scratch(CIR.TypeAnno.Idx).init(gpa),
            .anno_record_fields = try base.Scratch(CIR.TypeAnno.RecordField.Idx).init(gpa),
            .exposed_items = try base.Scratch(CIR.ExposedItem.Idx).init(gpa),
            .defs = try base.Scratch(CIR.Def.Idx).init(gpa),
            .diagnostics = try base.Scratch(CIR.Diagnostic.Idx).init(gpa),
            .captures = try base.Scratch(CIR.Expr.Capture.Idx).init(gpa),
        };

        return ptr;
    }

    fn deinit(self: *@This(), gpa: Allocator) void {
        self.statements.deinit();
        self.exprs.deinit();
        self.record_fields.deinit();
        self.match_branches.deinit();
        self.match_branch_patterns.deinit();
        self.if_branches.deinit();
        self.where_clauses.deinit();
        self.patterns.deinit();
        self.record_destructs.deinit();
        self.type_annos.deinit();
        self.anno_record_fields.deinit();
        self.exposed_items.deinit();
        self.defs.deinit();
        self.diagnostics.deinit();
        self.captures.deinit();
        gpa.destroy(self);
    }
};

/// Initializes the NodeStore
pub fn init(gpa: Allocator) Allocator.Error!NodeStore {
    // TODO determine what capacity to use
    // maybe these should be moved to build/compile flags?
    return try NodeStore.initCapacity(gpa, 128);
}

/// Initializes the NodeStore with a specified capacity.
pub fn initCapacity(gpa: Allocator, capacity: usize) Allocator.Error!NodeStore {
    return .{
        .gpa = gpa,
        .nodes = try Node.List.initCapacity(gpa, capacity),
        .regions = try Region.List.initCapacity(gpa, capacity),
        .extra_data = try collections.SafeList(u32).initCapacity(gpa, capacity / 2),
        .scratch = try Scratch.init(gpa),
    };
}

/// Deinitializes the NodeStore, freeing any allocated resources.
pub fn deinit(store: *NodeStore) void {
    store.nodes.deinit(store.gpa);
    store.regions.deinit(store.gpa);
    store.extra_data.deinit(store.gpa);
    if (store.scratch) |scratch| {
        scratch.deinit(store.gpa);
    }
}

/// Add the given offset to the memory addresses of all pointers in `self`.
/// This is used when loading a NodeStore from shared memory at a different address.
pub fn relocate(store: *NodeStore, offset: isize) void {
    store.nodes.relocate(offset);
    store.regions.relocate(offset);
    store.extra_data.relocate(offset);
    // scratch is null for deserialized NodeStores, no need to relocate
}

/// Compile-time constants for union variant counts to ensure we don't miss cases
/// when adding/removing variants from ModuleEnv unions. Update these when modifying the unions.
///
/// Count of the diagnostic nodes in the ModuleEnv
pub const MODULEENV_DIAGNOSTIC_NODE_COUNT = 63;
/// Count of the expression nodes in the ModuleEnv
pub const MODULEENV_EXPR_NODE_COUNT = 42;
/// Count of the statement nodes in the ModuleEnv
pub const MODULEENV_STATEMENT_NODE_COUNT = 18;
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

/// Helper function to get a region by  annotation index
pub fn getAnnotationRegion(store: *const NodeStore, anno_idx: CIR.Annotation.Idx) Region {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(anno_idx));
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
    const payload = node.getPayload();

    switch (node.tag) {
        .statement_decl => {
            const p = payload.statement_decl;
            return CIR.Statement{ .s_decl = .{
                .pattern = @enumFromInt(p.pattern),
                .expr = @enumFromInt(p.expr),
                .anno = blk: {
                    const extra_data = store.extra_data.items.items[p.extra_data_idx..];
                    const has_anno = extra_data[0] != 0;
                    if (has_anno) {
                        break :blk @as(CIR.Annotation.Idx, @enumFromInt(extra_data[1]));
                    } else {
                        break :blk null;
                    }
                },
            } };
        },
        .statement_decl_gen => {
            const p = payload.statement_decl;
            return CIR.Statement{ .s_decl_gen = .{
                .pattern = @enumFromInt(p.pattern),
                .expr = @enumFromInt(p.expr),
                .anno = blk: {
                    const extra_data = store.extra_data.items.items[p.extra_data_idx..];
                    const has_anno = extra_data[0] != 0;
                    if (has_anno) {
                        break :blk @as(CIR.Annotation.Idx, @enumFromInt(extra_data[1]));
                    } else {
                        break :blk null;
                    }
                },
            } };
        },
        .statement_var => {
            const p = payload.statement_var;
            return CIR.Statement{ .s_var = .{
                .pattern_idx = @enumFromInt(p.pattern_idx),
                .expr = @enumFromInt(p.expr),
                .anno = blk: {
                    const extra_data = store.extra_data.items.items[p.extra_data_idx..];
                    if (extra_data[0] != 0) {
                        break :blk @enumFromInt(extra_data[1]);
                    } else {
                        break :blk null;
                    }
                },
            } };
        },
        .statement_reassign => {
            const p = payload.statement_reassign;
            return CIR.Statement{ .s_reassign = .{
                .pattern_idx = @enumFromInt(p.pattern_idx),
                .expr = @enumFromInt(p.expr),
            } };
        },
        .statement_crash => {
            const p = payload.statement_crash;
            return CIR.Statement{ .s_crash = .{
                .msg = @enumFromInt(p.msg),
            } };
        },
        .statement_dbg => {
            const p = payload.statement_single_expr;
            return CIR.Statement{ .s_dbg = .{
                .expr = @enumFromInt(p.expr),
            } };
        },
        .statement_expr => {
            const p = payload.statement_single_expr;
            return .{ .s_expr = .{
                .expr = @enumFromInt(p.expr),
            } };
        },
        .statement_expect => {
            const p = payload.statement_single_expr;
            return CIR.Statement{ .s_expect = .{
                .body = @enumFromInt(p.expr),
            } };
        },
        .statement_for => {
            const p = payload.statement_for;
            return CIR.Statement{ .s_for = .{
                .patt = @enumFromInt(p.patt),
                .expr = @enumFromInt(p.expr),
                .body = @enumFromInt(p.body),
            } };
        },
        .statement_while => {
            const p = payload.statement_while;
            return CIR.Statement{ .s_while = .{
                .cond = @enumFromInt(p.cond),
                .body = @enumFromInt(p.body),
            } };
        },
        .statement_break => return CIR.Statement{ .s_break = .{} },
        .statement_return => {
            const p = payload.statement_return;
            return CIR.Statement{ .s_return = .{
                .expr = @enumFromInt(p.expr),
                .lambda = if (p.lambda_plus_one == 0) null else @as(?CIR.Expr.Idx, @enumFromInt(p.lambda_plus_one - 1)),
            } };
        },
        .statement_import => {
            const p = payload.statement_import;
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];

            const alias_data = extra_data[0];
            const qualifier_data = extra_data[1];
            const flags = extra_data[2];
            const exposes_start = extra_data[3];
            const exposes_len = extra_data[4];

            const alias_tok = if (flags & 1 != 0) @as(?Ident.Idx, @bitCast(alias_data)) else null;
            const qualifier_tok = if (flags & 2 != 0) @as(?Ident.Idx, @bitCast(qualifier_data)) else null;

            return CIR.Statement{
                .s_import = .{
                    .module_name_tok = @bitCast(p.module_name_tok),
                    .qualifier_tok = qualifier_tok,
                    .alias_tok = alias_tok,
                    .exposes = DataSpan.init(exposes_start, exposes_len).as(CIR.ExposedItem.Span),
                },
            };
        },
        .statement_alias_decl => {
            const p = payload.statement_alias_decl;
            return CIR.Statement{
                .s_alias_decl = .{
                    .header = @enumFromInt(p.header),
                    .anno = @enumFromInt(p.anno),
                },
            };
        },
        .statement_nominal_decl => {
            const p = payload.statement_nominal_decl;
            // Get is_opaque from extra_data
            const is_opaque = store.extra_data.items.items[p.extra_data_idx] != 0;
            return CIR.Statement{
                .s_nominal_decl = .{
                    .header = @enumFromInt(p.header),
                    .anno = @enumFromInt(p.anno),
                    .is_opaque = is_opaque,
                },
            };
        },
        .statement_type_anno => {
            const p = payload.statement_type_anno;
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];

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
        .statement_type_var_alias => {
            const p = payload.statement_type_var_alias;
            return CIR.Statement{
                .s_type_var_alias = .{
                    .alias_name = @bitCast(p.alias_name),
                    .type_var_name = @bitCast(p.type_var_name),
                    .type_var_anno = @enumFromInt(p.type_var_anno),
                },
            };
        },
        .malformed => {
            const p = payload.diagnostic;
            return CIR.Statement{ .s_runtime_error = .{
                .diagnostic = @enumFromInt(p.primary),
            } };
        },
        else => {
            std.debug.panic("unreachable, node is not a statement tag: {}", .{node.tag});
        },
    }
}

/// Retrieves an expression node from the store.
pub fn getExpr(store: *const NodeStore, expr: CIR.Expr.Idx) CIR.Expr {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(expr));
    const node = store.nodes.get(node_idx);
    const payload = node.getPayload();

    switch (node.tag) {
        .expr_var => {
            const p = payload.expr_var;
            return CIR.Expr{
                .e_lookup_local = .{
                    .pattern_idx = @enumFromInt(p.pattern_idx),
                },
            };
        },
        .expr_external_lookup => {
            const p = payload.expr_external_lookup;
            // Handle external lookups
            return CIR.Expr{ .e_lookup_external = .{
                .module_idx = @enumFromInt(p.module_idx),
                .target_node_idx = @intCast(p.target_node_idx),
                .ident_idx = @bitCast(p.ident_idx),
                .region = store.getRegionAt(node_idx),
            } };
        },
        .expr_required_lookup => {
            const p = payload.expr_required_lookup;
            // Handle required lookups (platform requires clause)
            return CIR.Expr{ .e_lookup_required = .{
                .requires_idx = ModuleEnv.RequiredType.SafeList.Idx.fromU32(p.requires_idx),
            } };
        },
        .expr_num => {
            const p = payload.expr_num;
            // Get requirements
            const kind: CIR.NumKind = @enumFromInt(p.kind);

            // Read i128 from extra_data (stored as 4 u32s)
            const val_kind: CIR.IntValue.IntKind = @enumFromInt(p.val_kind);
            const value_as_u32s = store.extra_data.items.items[p.extra_data_idx..][0..4];

            // Retrieve type variable from data_2 and requirements from data_3
            return CIR.Expr{
                .e_num = .{
                    .value = .{ .bytes = @bitCast(value_as_u32s.*), .kind = val_kind },
                    .kind = kind,
                },
            };
        },
        .expr_list => {
            const p = payload.expr_list;
            return CIR.Expr{
                .e_list = .{
                    .elems = .{ .span = .{ .start = p.elems_start, .len = p.elems_len } },
                },
            };
        },
        .expr_tuple => {
            const p = payload.expr_tuple;
            return CIR.Expr{
                .e_tuple = .{
                    .elems = .{ .span = .{ .start = p.elems_start, .len = p.elems_len } },
                },
            };
        },
        .expr_call => {
            const p = payload.expr_call;
            // Retrieve args span from extra_data
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];

            const args_start = extra_data[0];
            const args_len = extra_data[1];

            return CIR.Expr{
                .e_call = .{
                    .func = @enumFromInt(p.func),
                    .args = .{ .span = .{ .start = args_start, .len = args_len } },
                    .called_via = @enumFromInt(p.called_via),
                },
            };
        },
        .expr_frac_f32 => {
            const p = payload.expr_frac_f32;
            return CIR.Expr{ .e_frac_f32 = .{
                .value = @bitCast(p.value),
                .has_suffix = p.has_suffix != 0,
            } };
        },
        .expr_frac_f64 => {
            const p = payload.expr_frac_f64;
            const raw: [2]u32 = .{ p.value_lo, p.value_hi };

            return CIR.Expr{ .e_frac_f64 = .{
                .value = @bitCast(raw),
                .has_suffix = p.has_suffix != 0,
            } };
        },
        .expr_dec => {
            const p = payload.expr_dec;
            // Get value from extra_data
            const value_as_u32s = store.extra_data.items.items[p.extra_data_idx..][0..4];
            const value_as_i128: i128 = @bitCast(value_as_u32s.*);

            return CIR.Expr{
                .e_dec = .{
                    .value = RocDec{ .num = value_as_i128 },
                    .has_suffix = p.has_suffix != 0,
                },
            };
        },
        .expr_dec_small => {
            const p = payload.expr_dec_small;
            // Unpack small dec data
            const numerator = @as(i16, @intCast(@as(i32, @bitCast(p.numerator))));
            const denominator_power_of_ten = @as(u8, @truncate(p.denom_power));

            return CIR.Expr{
                .e_dec_small = .{
                    .value = .{
                        .numerator = numerator,
                        .denominator_power_of_ten = denominator_power_of_ten,
                    },
                    .has_suffix = p.has_suffix != 0,
                },
            };
        },
        .expr_typed_int => {
            const p = payload.raw;
            // Unpack typed int: value in extra_data, type_name in data_1
            const value_as_u32s = store.extra_data.items.items[p.data_3..][0..4];
            const value_as_i128: i128 = @bitCast(value_as_u32s.*);
            return CIR.Expr{
                .e_typed_int = .{
                    .value = .{
                        .bytes = @bitCast(value_as_i128),
                        .kind = @enumFromInt(p.data_2),
                    },
                    .type_name = @bitCast(p.data_1),
                },
            };
        },
        .expr_typed_frac => {
            const p = payload.raw;
            // Unpack typed frac: value in extra_data, type_name in data_1
            const value_as_u32s = store.extra_data.items.items[p.data_3..][0..4];
            const value_as_i128: i128 = @bitCast(value_as_u32s.*);
            return CIR.Expr{
                .e_typed_frac = .{
                    .value = .{
                        .bytes = @bitCast(value_as_i128),
                        .kind = @enumFromInt(p.data_2),
                    },
                    .type_name = @bitCast(p.data_1),
                },
            };
        },
        .expr_string_segment => {
            const p = payload.raw;
            return CIR.Expr.initStrSegment(@enumFromInt(p.data_1));
        },
        .expr_string => {
            const p = payload.expr_string;
            return CIR.Expr.initStr(DataSpan.init(p.segments_start, p.segments_len).as(CIR.Expr.Span));
        },
        .expr_tag => {
            const p = payload.expr_tag;
            return CIR.Expr{
                .e_tag = .{
                    .name = @bitCast(p.name),
                    .args = .{ .span = .{ .start = p.args_start, .len = p.args_len } },
                },
            };
        },
        .expr_nominal => {
            const p = payload.raw;
            return CIR.Expr{
                .e_nominal = .{
                    .nominal_type_decl = @enumFromInt(p.data_1),
                    .backing_expr = @enumFromInt(p.data_2),
                    .backing_type = @enumFromInt(p.data_3),
                },
            };
        },
        .expr_nominal_external => {
            const p = payload.raw;
            const extra_data = store.extra_data.items.items[p.data_3..][0..2];
            const backing_expr: CIR.Expr.Idx = @enumFromInt(extra_data[0]);
            const backing_type: CIR.Expr.NominalBackingType = @enumFromInt(extra_data[1]);

            return CIR.Expr{
                .e_nominal_external = .{
                    .module_idx = @enumFromInt(p.data_1),
                    .target_node_idx = @intCast(p.data_2),
                    .backing_expr = backing_expr,
                    .backing_type = backing_type,
                },
            };
        },
        .expr_bin_op => {
            const p = payload.expr_bin_op;
            return CIR.Expr{
                .e_binop = CIR.Expr.Binop.init(
                    @enumFromInt(p.op),
                    @enumFromInt(p.lhs),
                    @enumFromInt(p.rhs),
                ),
            };
        },
        .expr_closure => {
            const p = payload.expr_closure;
            // Retrieve closure data from extra_data
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];

            const lambda_idx = extra_data[0];
            const capture_start = extra_data[1];
            const capture_len = extra_data[2];
            const tag_name: base.Ident.Idx = @bitCast(extra_data[3]);

            return CIR.Expr{
                .e_closure = .{
                    .lambda_idx = @enumFromInt(lambda_idx),
                    .captures = .{ .span = .{ .start = capture_start, .len = capture_len } },
                    .tag_name = tag_name,
                },
            };
        },
        .expr_lambda => {
            const p = payload.expr_lambda;
            // Retrieve lambda data from extra_data
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];

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
            const p = payload.expr_block;
            return CIR.Expr{
                .e_block = .{
                    .stmts = .{ .span = .{ .start = p.stmts_start, .len = p.stmts_len } },
                    .final_expr = @enumFromInt(p.final_expr),
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
            const p = payload.expr_record;
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];

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
            const p = payload.expr_match;
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];

            const cond = @as(CIR.Expr.Idx, @enumFromInt(extra_data[0]));
            const branches_start = extra_data[1];
            const branches_len = extra_data[2];
            const exhaustive = @as(types.Var, @enumFromInt(extra_data[3]));
            const is_try_suffix = extra_data[4] != 0;

            return CIR.Expr{
                .e_match = CIR.Expr.Match{
                    .cond = cond,
                    .branches = .{ .span = .{ .start = branches_start, .len = branches_len } },
                    .exhaustive = exhaustive,
                    .is_try_suffix = is_try_suffix,
                },
            };
        },
        .expr_zero_argument_tag => {
            const p = payload.raw;
            const extra_data = store.extra_data.items.items[p.data_1..];

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
            const p = payload.raw;
            return CIR.Expr{ .e_crash = .{
                .msg = @enumFromInt(p.data_1),
            } };
        },
        .expr_dbg => {
            const p = payload.raw;
            return CIR.Expr{ .e_dbg = .{
                .expr = @enumFromInt(p.data_1),
            } };
        },
        .expr_unary_minus => {
            const p = payload.expr_unary;
            return CIR.Expr{ .e_unary_minus = .{
                .expr = @enumFromInt(p.expr),
            } };
        },
        .expr_unary_not => {
            const p = payload.expr_unary;
            return CIR.Expr{ .e_unary_not = .{
                .expr = @enumFromInt(p.expr),
            } };
        },
        .expr_static_dispatch,
        .expr_apply,
        .expr_record_update,
        .expr_suffix_single_question,
        .expr_record_builder,
        => {
            return CIR.Expr{
                .e_runtime_error = .{
                    .diagnostic = undefined, // deserialized runtime errors don't preserve diagnostics
                },
            };
        },
        .expr_ellipsis => {
            return CIR.Expr{ .e_ellipsis = .{} };
        },
        .expr_anno_only => {
            const p = payload.raw;
            return CIR.Expr{ .e_anno_only = .{
                .ident = @bitCast(p.data_1),
            } };
        },
        .expr_return => {
            const p = payload.raw;
            return CIR.Expr{ .e_return = .{
                .expr = @enumFromInt(p.data_1),
            } };
        },
        .expr_type_var_dispatch => {
            const p = payload.raw;
            // Retrieve type var dispatch data from node and extra_data
            const type_var_alias_stmt: CIR.Statement.Idx = @enumFromInt(p.data_1);
            const method_name: base.Ident.Idx = @bitCast(p.data_2);
            const extra_data = store.extra_data.items.items[p.data_3..];

            const args_start = extra_data[0];
            const args_len = extra_data[1];

            return CIR.Expr{ .e_type_var_dispatch = .{
                .type_var_alias_stmt = type_var_alias_stmt,
                .method_name = method_name,
                .args = .{ .span = .{ .start = args_start, .len = args_len } },
            } };
        },
        .expr_hosted_lambda => {
            const p = payload.expr_hosted_lambda;
            // Retrieve hosted lambda data from node and extra_data
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];

            const args_start = extra_data[0];
            const args_len = extra_data[1];
            const body_idx = extra_data[2];

            return CIR.Expr{ .e_hosted_lambda = .{
                .symbol_name = @bitCast(p.symbol_name),
                .index = p.index,
                .args = .{ .span = .{ .start = args_start, .len = args_len } },
                .body = @enumFromInt(body_idx),
            } };
        },
        .expr_low_level => {
            const p = payload.expr_low_level;
            // Retrieve low-level lambda data from extra_data
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];

            const args_start = extra_data[0];
            const args_len = extra_data[1];
            const body_idx = extra_data[2];

            return CIR.Expr{ .e_low_level_lambda = .{
                .op = @enumFromInt(p.op),
                .args = .{ .span = .{ .start = args_start, .len = args_len } },
                .body = @enumFromInt(body_idx),
            } };
        },
        .expr_expect => {
            const p = payload.raw;
            return CIR.Expr{ .e_expect = .{
                .body = @enumFromInt(p.data_1),
            } };
        },
        .expr_for => {
            const p = payload.statement_for;
            return CIR.Expr{ .e_for = .{
                .patt = @enumFromInt(p.patt),
                .expr = @enumFromInt(p.expr),
                .body = @enumFromInt(p.body),
            } };
        },
        .expr_if_then_else => {
            const p = payload.expr_if_then_else;
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];

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
            const p = payload.expr_dot_access;
            // Read extra data: field_name_region (2 u32s) + optional args (1 u32)
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];
            const field_name_region = base.Region{
                .start = .{ .offset = extra_data[0] },
                .end = .{ .offset = extra_data[1] },
            };
            const args_packed = extra_data[2];
            const args_span = if (args_packed != 0) blk: {
                const packed_span = FunctionArgs.fromU32(args_packed - OPTIONAL_VALUE_OFFSET);
                const data_span = packed_span.toDataSpan();
                break :blk CIR.Expr.Span{ .span = data_span };
            } else null;

            return CIR.Expr{ .e_dot_access = .{
                .receiver = @enumFromInt(p.receiver),
                .field_name = @bitCast(p.field_name),
                .field_name_region = field_name_region,
                .args = args_span,
            } };
        },
        .malformed => {
            const p = payload.diagnostic;
            return CIR.Expr{ .e_runtime_error = .{
                .diagnostic = @enumFromInt(p.primary),
            } };
        },

        // NOTE: Diagnostic tags should NEVER appear in getExpr().
        // If compilation errors occur, use pushMalformed() to create .malformed nodes
        // that reference diagnostic indices. The .malformed case above handles
        // converting these to runtime_error nodes in the ModuleEnv.
        else => {
            std.debug.panic("unreachable, node is not an expression tag: {}", .{node.tag});
        },
    }
}

/// Replaces an existing expression with an e_num expression in-place.
/// This is used for constant folding during compile-time evaluation.
/// Note: This modifies only the CIR node and should only be called after type-checking
/// is complete. Type information is stored separately and remains unchanged.
pub fn replaceExprWithNum(store: *NodeStore, expr_idx: CIR.Expr.Idx, value: CIR.IntValue, num_kind: CIR.NumKind) !void {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(expr_idx));

    const extra_data_start = store.extra_data.len();
    const value_as_i128: i128 = @bitCast(value.bytes);
    const value_as_u32s: [4]u32 = @bitCast(value_as_i128);
    _ = try store.extra_data.appendSlice(store.gpa, &value_as_u32s);

    store.nodes.set(node_idx, .{
        .tag = .expr_num,
        .data_1 = @intFromEnum(num_kind),
        .data_2 = @intFromEnum(value.kind),
        .data_3 = @intCast(extra_data_start),
    });
}

/// Replaces an existing expression with an e_zero_argument_tag expression in-place.
/// This is used for constant folding tag unions (like Bool) during compile-time evaluation.
/// Note: This modifies only the CIR node and should only be called after type-checking
/// is complete. Type information is stored separately and remains unchanged.
pub fn replaceExprWithZeroArgumentTag(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    closure_name: Ident.Idx,
    variant_var: types.Var,
    ext_var: types.Var,
    name: Ident.Idx,
) !void {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(expr_idx));

    const extra_data_start = store.extra_data.len();
    _ = try store.extra_data.append(store.gpa, @bitCast(closure_name));
    _ = try store.extra_data.append(store.gpa, @intFromEnum(variant_var));
    _ = try store.extra_data.append(store.gpa, @intFromEnum(ext_var));
    _ = try store.extra_data.append(store.gpa, @bitCast(name));

    store.nodes.set(node_idx, .{
        .tag = .expr_zero_argument_tag,
        .data_1 = @intCast(extra_data_start),
        .data_2 = 0,
        .data_3 = 0,
    });
}

/// Replaces an existing expression with an e_tuple expression in-place.
/// This is used for constant folding tuples during compile-time evaluation.
/// The elem_indices slice contains the indices of the tuple element expressions.
/// Note: This modifies only the CIR node and should only be called after type-checking
/// is complete. Type information is stored separately and remains unchanged.
pub fn replaceExprWithTuple(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    elem_indices: []const CIR.Expr.Idx,
) !void {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(expr_idx));

    // Store element indices in extra_data
    const extra_data_start = store.extra_data.len();
    for (elem_indices) |elem_idx| {
        _ = try store.extra_data.append(store.gpa, @intFromEnum(elem_idx));
    }

    store.nodes.set(node_idx, .{
        .tag = .expr_tuple,
        .data_1 = @intCast(extra_data_start),
        .data_2 = @intCast(elem_indices.len),
        .data_3 = 0,
    });
}

/// Replaces an existing expression with an e_tag expression in-place.
/// This is used for constant folding tag unions with payloads during compile-time evaluation.
/// The arg_indices slice contains the indices of the tag argument expressions.
/// Note: This modifies only the CIR node and should only be called after type-checking
/// is complete. Type information is stored separately and remains unchanged.
pub fn replaceExprWithTag(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    name: Ident.Idx,
    arg_indices: []const CIR.Expr.Idx,
) !void {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(expr_idx));

    // Store argument indices in extra_data
    const extra_data_start = store.extra_data.len();
    for (arg_indices) |arg_idx| {
        _ = try store.extra_data.append(store.gpa, @intFromEnum(arg_idx));
    }

    store.nodes.set(node_idx, .{
        .tag = .expr_tag,
        .data_1 = @bitCast(name),
        .data_2 = @intCast(extra_data_start),
        .data_3 = @intCast(arg_indices.len),
    });
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
    const payload = node.getPayload();

    std.debug.assert(node.tag == .match_branch);

    // Retrieve when branch data from extra_data
    const p = payload.match_branch;
    const extra_data = store.extra_data.items.items[p.extra_data_idx..];

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
    const payload = node.getPayload();

    std.debug.assert(node.tag == .match_branch_pattern);

    const p = payload.raw;
    return CIR.Expr.Match.BranchPattern{
        .pattern = @enumFromInt(p.data_1),
        .degenerate = p.data_2 != 0,
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
    const payload = node.getPayload();

    switch (node.tag) {
        .where_method => {
            const p = payload.where_clause;
            const var_ = @as(CIR.TypeAnno.Idx, @enumFromInt(p.var_idx));
            const method_name = @as(Ident.Idx, @bitCast(p.name));

            const extra_data = store.extra_data.items.items[p.extra_data_idx..];

            const args_start = extra_data[0];
            const args_len = extra_data[1];
            const ret = @as(CIR.TypeAnno.Idx, @enumFromInt(extra_data[2]));

            return CIR.WhereClause{ .w_method = .{
                .var_ = var_,
                .method_name = method_name,
                .args = .{ .span = .{ .start = args_start, .len = args_len } },
                .ret = ret,
            } };
        },
        .where_alias => {
            const p = payload.raw;
            const var_ = @as(CIR.TypeAnno.Idx, @enumFromInt(p.data_1));
            const alias_name = @as(Ident.Idx, @bitCast(p.data_2));

            return CIR.WhereClause{ .w_alias = .{
                .var_ = var_,
                .alias_name = alias_name,
            } };
        },
        .where_malformed => {
            const p = payload.diagnostic;
            const diagnostic = @as(CIR.Diagnostic.Idx, @enumFromInt(p.primary));

            return CIR.WhereClause{ .w_malformed = .{
                .diagnostic = diagnostic,
            } };
        },
        else => {
            std.debug.panic("unreachable, node is not a where tag: {}", .{node.tag});
        },
    }
}

/// Returns true if the given node tag represents a pattern node.
fn isPatternTag(tag: Node.Tag) bool {
    return switch (tag) {
        .pattern_identifier,
        .pattern_as,
        .pattern_applied_tag,
        .pattern_nominal,
        .pattern_nominal_external,
        .pattern_record_destructure,
        .pattern_list,
        .pattern_tuple,
        .pattern_num_literal,
        .pattern_dec_literal,
        .pattern_f32_literal,
        .pattern_f64_literal,
        .pattern_small_dec_literal,
        .pattern_str_literal,
        .pattern_underscore,
        .malformed, // Valid pattern tag for runtime_error patterns
        => true,
        else => false,
    };
}

/// Retrieves a pattern from the store.
pub fn getPattern(store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) CIR.Pattern {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));
    const node = store.nodes.get(node_idx);

    // Safety check: Handle cross-module node index issues where a pattern index
    // might point to a non-pattern node (e.g., type_header from another module)
    if (!isPatternTag(node.tag)) {
        // Return a placeholder pattern to avoid crash
        // This indicates a bug in cross-module canonicalization
        return CIR.Pattern{ .underscore = {} };
    }

    const payload = node.getPayload();

    switch (node.tag) {
        .pattern_identifier => {
            const p = payload.pattern_identifier;
            return CIR.Pattern{
                .assign = .{
                    .ident = @bitCast(p.ident),
                },
            };
        },
        .pattern_as => {
            const p = payload.pattern_as;
            return CIR.Pattern{
                .as = .{
                    .ident = @bitCast(p.ident),
                    .pattern = @enumFromInt(p.pattern),
                },
            };
        },
        .pattern_applied_tag => {
            const p = payload.pattern_applied_tag;
            return CIR.Pattern{
                .applied_tag = .{
                    .args = DataSpan.init(p.args_start, p.args_len).as(CIR.Pattern.Span),
                    .name = @bitCast(p.name),
                },
            };
        },
        .pattern_nominal => {
            const p = payload.raw;
            return CIR.Pattern{
                .nominal = .{
                    .nominal_type_decl = @enumFromInt(p.data_1),
                    .backing_pattern = @enumFromInt(p.data_2),
                    .backing_type = @enumFromInt(p.data_3),
                },
            };
        },
        .pattern_nominal_external => {
            const p = payload.raw;
            const extra_data = store.extra_data.items.items[p.data_3..][0..2];
            const backing_pattern: CIR.Pattern.Idx = @enumFromInt(extra_data[0]);
            const backing_type: CIR.Expr.NominalBackingType = @enumFromInt(extra_data[1]);

            return CIR.Pattern{
                .nominal_external = .{
                    .module_idx = @enumFromInt(p.data_1),
                    .target_node_idx = @intCast(p.data_2),
                    .backing_pattern = backing_pattern,
                    .backing_type = backing_type,
                },
            };
        },
        .pattern_record_destructure => {
            const p = payload.pattern_record_destructure;
            return CIR.Pattern{
                .record_destructure = .{
                    .destructs = DataSpan.init(p.destructs_start, p.destructs_len).as(CIR.Pattern.RecordDestruct.Span),
                },
            };
        },
        .pattern_list => {
            const p = payload.pattern_list;
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];

            const patterns_start = extra_data[0];
            const patterns_len = extra_data[1];

            // Load rest_info
            const has_rest_info = extra_data[2] != 0;
            const rest_info = if (has_rest_info) blk: {
                const rest_index = extra_data[3];
                const has_pattern = extra_data[4] != 0;
                const rest_pattern = if (has_pattern)
                    @as(CIR.Pattern.Idx, @enumFromInt(extra_data[5]))
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
                    .rest_info = rest_info,
                },
            };
        },
        .pattern_tuple => {
            const p = payload.pattern_tuple;
            return CIR.Pattern{
                .tuple = .{
                    .patterns = DataSpan.init(p.patterns_start, p.patterns_len).as(CIR.Pattern.Span),
                },
            };
        },
        .pattern_num_literal => {
            const p = payload.pattern_num_literal;
            const value_as_u32s = store.extra_data.items.items[p.extra_data_idx..][0..4];
            const value_as_i128: i128 = @bitCast(value_as_u32s.*);

            return CIR.Pattern{
                .num_literal = .{
                    .value = .{ .bytes = @bitCast(value_as_i128), .kind = @enumFromInt(p.value_kind) },
                    .kind = @enumFromInt(p.kind),
                },
            };
        },
        .pattern_f32_literal => {
            const p = payload.raw;
            return CIR.Pattern{
                .frac_f32_literal = .{ .value = @bitCast(p.data_1) },
            };
        },
        .pattern_f64_literal => {
            const p = payload.raw;
            const raw: u64 = (@as(u64, p.data_2) << 32) | @as(u64, p.data_1);

            return CIR.Pattern{
                .frac_f64_literal = .{ .value = @bitCast(raw) },
            };
        },
        .pattern_dec_literal => {
            const p = payload.raw;
            const value_as_u32s = store.extra_data.items.items[p.data_1..][0..4];
            const value_as_i128: i128 = @bitCast(value_as_u32s.*);

            return CIR.Pattern{
                .dec_literal = .{
                    .value = RocDec{ .num = value_as_i128 },
                    .has_suffix = p.data_2 != 0,
                },
            };
        },
        .pattern_small_dec_literal => {
            const p = payload.raw;
            // Unpack small dec data from data_1 and data_3
            // data_1: numerator (i16) stored as u32
            // data_3: denominator_power_of_ten (u8) in lower 8 bits
            const numerator: i16 = @intCast(@as(i32, @bitCast(p.data_1)));
            const denominator_power_of_ten: u8 = @intCast(p.data_2 & 0xFF);

            return CIR.Pattern{
                .small_dec_literal = .{
                    .value = .{
                        .numerator = numerator,
                        .denominator_power_of_ten = denominator_power_of_ten,
                    },
                    .has_suffix = p.data_3 != 0,
                },
            };
        },
        .pattern_str_literal => {
            const p = payload.raw;
            return CIR.Pattern{ .str_literal = .{
                .literal = @enumFromInt(p.data_1),
            } };
        },

        .pattern_underscore => return CIR.Pattern{ .underscore = {} },
        .malformed => {
            const p = payload.diagnostic;
            return CIR.Pattern{ .runtime_error = .{
                .diagnostic = @enumFromInt(p.primary),
            } };
        },
        else => {
            std.debug.panic("unreachable, node is not a pattern tag: {}", .{node.tag});
        },
    }
}

/// Retrieves a type annotation from the store.
pub fn getTypeAnno(store: *const NodeStore, typeAnno: CIR.TypeAnno.Idx) CIR.TypeAnno {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(typeAnno));
    const node = store.nodes.get(node_idx);
    const payload = node.getPayload();

    switch (node.tag) {
        .ty_apply => {
            const p = payload.ty_apply;
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];
            const args_len = extra_data[0];
            const base_enum: CIR.TypeAnno.LocalOrExternal.Tag = @enumFromInt(extra_data[1]);
            const type_base: CIR.TypeAnno.LocalOrExternal = blk: {
                switch (base_enum) {
                    .builtin => {
                        break :blk .{ .builtin = @enumFromInt(extra_data[2]) };
                    },
                    .local => {
                        break :blk .{ .local = .{ .decl_idx = @enumFromInt(extra_data[2]) } };
                    },
                    .external => {
                        break :blk .{ .external = .{
                            .module_idx = @enumFromInt(extra_data[2]),
                            .target_node_idx = @intCast(extra_data[3]),
                        } };
                    },
                }
            };

            return CIR.TypeAnno{ .apply = .{
                .name = @bitCast(p.name),
                .base = type_base,
                .args = .{ .span = .{ .start = p.args_start, .len = args_len } },
            } };
        },
        .ty_rigid_var => {
            const p = payload.raw;
            return CIR.TypeAnno{ .rigid_var = .{
                .name = @bitCast(p.data_1),
            } };
        },
        .ty_rigid_var_lookup => {
            const p = payload.raw;
            return CIR.TypeAnno{ .rigid_var_lookup = .{
                .ref = @enumFromInt(p.data_1),
            } };
        },
        .ty_underscore => return CIR.TypeAnno{ .underscore = {} },
        .ty_lookup => {
            const p = payload.ty_lookup;
            const extra_data = store.extra_data.items.items[p.extra_data_idx..];
            const base_enum: CIR.TypeAnno.LocalOrExternal.Tag = @enumFromInt(p.base);
            const type_base: CIR.TypeAnno.LocalOrExternal = blk: {
                switch (base_enum) {
                    .builtin => {
                        break :blk .{ .builtin = @enumFromInt(extra_data[0]) };
                    },
                    .local => {
                        break :blk .{ .local = .{ .decl_idx = @enumFromInt(extra_data[0]) } };
                    },
                    .external => {
                        break :blk .{ .external = .{
                            .module_idx = @enumFromInt(extra_data[0]),
                            .target_node_idx = @intCast(extra_data[1]),
                        } };
                    },
                }
            };

            return CIR.TypeAnno{ .lookup = .{
                .name = @bitCast(p.name),
                .base = type_base,
            } };
        },
        .ty_tag_union => {
            const p = payload.ty_tag_union;
            return CIR.TypeAnno{ .tag_union = .{
                .tags = .{ .span = .{ .start = p.tags_start, .len = p.tags_len } },
                .ext = if (p.ext_plus_one != 0) @enumFromInt(p.ext_plus_one - OPTIONAL_VALUE_OFFSET) else null,
            } };
        },
        .ty_tag => {
            const p = payload.ty_tag;
            return CIR.TypeAnno{ .tag = .{
                .name = @bitCast(p.name),
                .args = .{ .span = .{ .start = p.args_start, .len = p.args_len } },
            } };
        },
        .ty_tuple => {
            const p = payload.ty_tuple;
            return CIR.TypeAnno{ .tuple = .{
                .elems = .{ .span = .{ .start = p.elems_start, .len = p.elems_len } },
            } };
        },
        .ty_record => {
            const p = payload.ty_record;
            return CIR.TypeAnno{
                .record = .{
                    .fields = .{ .span = .{ .start = p.fields_start, .len = p.fields_len } },
                    .ext = if (p.ext_plus_one != 0) @enumFromInt(p.ext_plus_one - OPTIONAL_VALUE_OFFSET) else null,
                },
            };
        },
        .ty_fn => {
            const p = payload.ty_fn;
            const effectful = store.extra_data.items.items[p.extra_data_idx] != 0;
            const ret: CIR.TypeAnno.Idx = @enumFromInt(store.extra_data.items.items[p.extra_data_idx + 1]);
            return CIR.TypeAnno{ .@"fn" = .{
                .args = .{ .span = .{ .start = p.args_start, .len = p.args_len } },
                .ret = ret,
                .effectful = effectful,
            } };
        },
        .ty_parens => {
            const p = payload.raw;
            return CIR.TypeAnno{ .parens = .{
                .anno = @enumFromInt(p.data_1),
            } };
        },
        .ty_malformed => {
            const p = payload.diagnostic;
            return CIR.TypeAnno{ .malformed = .{
                .diagnostic = @enumFromInt(p.primary),
            } };
        },
        .malformed => {
            const p = payload.diagnostic;
            return CIR.TypeAnno{ .malformed = .{
                .diagnostic = @enumFromInt(p.primary),
            } };
        },
        else => {
            std.debug.panic("unreachable, node is not a type annotation tag: {}", .{node.tag});
        },
    }
}

/// Retrieves a type header from the store.
pub fn getTypeHeader(store: *const NodeStore, typeHeader: CIR.TypeHeader.Idx) CIR.TypeHeader {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(typeHeader));
    const node = store.nodes.get(node_idx);
    const payload = node.getPayload();

    std.debug.assert(node.tag == .type_header);

    const p = payload.raw;
    // Unpack args from packed format (start in upper 16 bits, len in lower 16 bits)
    const packed_args = p.data_3;
    const args_start: u32 = packed_args >> 16;
    const args_len: u32 = packed_args & 0xFFFF;

    return CIR.TypeHeader{
        .name = @bitCast(p.data_1),
        .relative_name = @bitCast(p.data_2),
        .args = .{ .span = .{ .start = args_start, .len = args_len } },
    };
}

/// Retrieves an annotation record field from the store.
pub fn getAnnoRecordField(store: *const NodeStore, annoRecordField: CIR.TypeAnno.RecordField.Idx) CIR.TypeAnno.RecordField {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(annoRecordField));
    const node = store.nodes.get(node_idx);
    const p = node.getPayload().raw;
    return .{
        .name = @bitCast(p.data_1),
        .ty = @enumFromInt(p.data_2),
    };
}

/// Retrieves an annotation from the store.
pub fn getAnnotation(store: *const NodeStore, annotation: CIR.Annotation.Idx) CIR.Annotation {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(annotation));
    const node = store.nodes.get(node_idx);
    const payload = node.getPayload();

    std.debug.assert(node.tag == .annotation);

    const p = payload.annotation;
    const anno: CIR.TypeAnno.Idx = @enumFromInt(p.anno);

    const where_clause = if (p.has_where == 1) blk: {
        const extra_data = store.extra_data.items.items[p.extra_data_idx..];

        const where_start = extra_data[0];
        const where_len = extra_data[1];
        break :blk CIR.WhereClause.Span{ .span = DataSpan.init(where_start, where_len) };
    } else null;

    return CIR.Annotation{
        .anno = anno,
        .where = where_clause,
    };
}

/// Retrieves an exposed item from the store.
pub fn getExposedItem(store: *const NodeStore, exposedItem: CIR.ExposedItem.Idx) CIR.ExposedItem {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(exposedItem));
    const node = store.nodes.get(node_idx);
    const payload = node.getPayload();

    switch (node.tag) {
        .exposed_item => {
            const p = payload.raw;
            return CIR.ExposedItem{
                .name = @bitCast(p.data_1),
                .alias = if (p.data_2 == 0) null else @bitCast(p.data_2),
                .is_wildcard = p.data_3 != 0,
            };
        },
        else => std.debug.panic("Expected exposed_item node, got {s}\n", .{@tagName(node.tag)}),
    }
}

/// Adds a statement to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addStatement(store: *NodeStore, statement: CIR.Statement, region: base.Region) Allocator.Error!CIR.Statement.Idx {
    const node = try store.makeStatementNode(statement);
    const node_idx = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Set a statement idx to the provided statement
///
/// This is used when defininig recursive type declarations:
/// 1. Make the placeholder node
/// 2. Introduce to scope
/// 3. Canonicalize the annotation
/// 4. Update the placeholder node with the actual annotation
pub fn setStatementNode(store: *NodeStore, stmt_idx: CIR.Statement.Idx, statement: CIR.Statement) Allocator.Error!void {
    const node = try store.makeStatementNode(statement);
    store.nodes.set(@enumFromInt(@intFromEnum(stmt_idx)), node);
}

/// Creates a statement node, but does not append to the store.
/// IMPORTANT: It *does* append to extra_data though
///
/// See `setStatementNode` to see why this exists
fn makeStatementNode(store: *NodeStore, statement: CIR.Statement) Allocator.Error!Node {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .tag = undefined,
    };

    switch (statement) {
        .s_decl => |s| {
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            if (s.anno) |anno| {
                _ = try store.extra_data.append(store.gpa, @intFromBool(true));
                _ = try store.extra_data.append(store.gpa, @intFromEnum(anno));
            } else {
                _ = try store.extra_data.append(store.gpa, @intFromBool(false));
            }

            node.tag = .statement_decl;
            node.setPayload(.{ .statement_decl = .{
                .pattern = @intFromEnum(s.pattern),
                .expr = @intFromEnum(s.expr),
                .extra_data_idx = extra_data_start,
            } });
        },
        .s_decl_gen => |s| {
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            if (s.anno) |anno| {
                _ = try store.extra_data.append(store.gpa, @intFromBool(true));
                _ = try store.extra_data.append(store.gpa, @intFromEnum(anno));
            } else {
                _ = try store.extra_data.append(store.gpa, @intFromBool(false));
            }

            node.tag = .statement_decl_gen;
            node.setPayload(.{ .statement_decl = .{
                .pattern = @intFromEnum(s.pattern),
                .expr = @intFromEnum(s.expr),
                .extra_data_idx = extra_data_start,
            } });
        },
        .s_var => |s| {
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            if (s.anno) |anno| {
                _ = try store.extra_data.append(store.gpa, @intFromBool(true));
                _ = try store.extra_data.append(store.gpa, @intFromEnum(anno));
            } else {
                _ = try store.extra_data.append(store.gpa, @intFromBool(false));
            }

            node.tag = .statement_var;
            node.setPayload(.{ .statement_var = .{
                .pattern_idx = @intFromEnum(s.pattern_idx),
                .expr = @intFromEnum(s.expr),
                .extra_data_idx = extra_data_start,
            } });
        },
        .s_reassign => |s| {
            node.tag = .statement_reassign;
            node.setPayload(.{ .statement_reassign = .{
                .pattern_idx = @intFromEnum(s.pattern_idx),
                .expr = @intFromEnum(s.expr),
                ._unused = 0,
            } });
        },
        .s_crash => |s| {
            node.tag = .statement_crash;
            node.setPayload(.{ .statement_crash = .{
                .msg = @intFromEnum(s.msg),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .s_dbg => |s| {
            node.tag = .statement_dbg;
            node.setPayload(.{ .statement_single_expr = .{
                .expr = @intFromEnum(s.expr),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .s_expr => |s| {
            node.tag = .statement_expr;
            node.setPayload(.{ .statement_single_expr = .{
                .expr = @intFromEnum(s.expr),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .s_expect => |s| {
            node.tag = .statement_expect;
            node.setPayload(.{ .statement_single_expr = .{
                .expr = @intFromEnum(s.body),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .s_for => |s| {
            node.tag = .statement_for;
            node.setPayload(.{ .statement_for = .{
                .patt = @intFromEnum(s.patt),
                .expr = @intFromEnum(s.expr),
                .body = @intFromEnum(s.body),
            } });
        },
        .s_while => |s| {
            node.tag = .statement_while;
            node.setPayload(.{ .statement_while = .{
                .cond = @intFromEnum(s.cond),
                .body = @intFromEnum(s.body),
                ._unused = 0,
            } });
        },
        .s_break => |_| {
            node.tag = .statement_break;
        },
        .s_return => |s| {
            node.tag = .statement_return;
            node.setPayload(.{ .statement_return = .{
                .expr = @intFromEnum(s.expr),
                // Store lambda using 0 for null and idx+1 for valid indices
                .lambda_plus_one = if (s.lambda) |lambda| @intFromEnum(lambda) + 1 else 0,
                ._unused = 0,
            } });
        },
        .s_import => |s| {
            node.tag = .statement_import;

            // Store optional fields in extra_data
            const extra_start: u32 = @intCast(store.extra_data.len());

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

            node.setPayload(.{ .statement_import = .{
                .module_name_tok = @bitCast(s.module_name_tok),
                .extra_data_idx = extra_start,
                ._unused = 0,
            } });
        },
        .s_alias_decl => |s| {
            node.tag = .statement_alias_decl;
            node.setPayload(.{ .statement_alias_decl = .{
                .header = @intFromEnum(s.header),
                .anno = @intFromEnum(s.anno),
                ._unused = 0,
            } });
        },
        .s_nominal_decl => |s| {
            node.tag = .statement_nominal_decl;
            // Store is_opaque in extra_data
            const extra_idx: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, if (s.is_opaque) 1 else 0);
            node.setPayload(.{ .statement_nominal_decl = .{
                .header = @intFromEnum(s.header),
                .anno = @intFromEnum(s.anno),
                .extra_data_idx = extra_idx,
            } });
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
            node.setPayload(.{ .statement_type_anno = .{
                .extra_data_idx = @intCast(extra_start),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .s_type_var_alias => |s| {
            node.tag = .statement_type_var_alias;
            node.setPayload(.{ .statement_type_var_alias = .{
                .alias_name = @bitCast(s.alias_name),
                .type_var_name = @bitCast(s.type_var_name),
                .type_var_anno = @intFromEnum(s.type_var_anno),
            } });
        },
        .s_runtime_error => |s| {
            node.tag = .malformed;
            node.setPayload(.{ .diagnostic = .{
                .primary = @intFromEnum(s.diagnostic),
                .secondary = 0,
                .tertiary = 0,
            } });
        },
    }

    return node;
}

/// Adds an expression node to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addExpr(store: *NodeStore, expr: CIR.Expr, region: base.Region) Allocator.Error!CIR.Expr.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .tag = undefined, // set below in switch
    };

    switch (expr) {
        .e_lookup_local => |local| {
            node.tag = .expr_var;
            node.setPayload(.{ .expr_var = .{
                .pattern_idx = @intFromEnum(local.pattern_idx),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .e_lookup_external => |e| {
            // For external lookups, store the module index, target node index, and ident
            node.tag = .expr_external_lookup;
            node.setPayload(.{ .expr_external_lookup = .{
                .module_idx = @intFromEnum(e.module_idx),
                .target_node_idx = e.target_node_idx,
                .ident_idx = @bitCast(e.ident_idx),
            } });
        },
        .e_lookup_required => |e| {
            // For required lookups (platform requires clause), store the index
            node.tag = .expr_required_lookup;
            node.setPayload(.{ .expr_required_lookup = .{
                .requires_idx = e.requires_idx.toU32(),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .e_num => |e| {
            node.tag = .expr_num;

            // Store i128 value in extra_data
            const extra_data_start: u32 = @intCast(store.extra_data.len());

            // Store the IntLiteralValue as i128 (16 bytes = 4 u32s)
            // We always store as i128 internally
            const value_as_i128: i128 = @bitCast(e.value.bytes);
            const value_as_u32s: [4]u32 = @bitCast(value_as_i128);
            for (value_as_u32s) |word| {
                _ = try store.extra_data.append(store.gpa, word);
            }

            node.setPayload(.{ .expr_num = .{
                .kind = @intFromEnum(e.kind),
                .val_kind = @intFromEnum(e.value.kind),
                .extra_data_idx = extra_data_start,
            } });
        },
        .e_list => |e| {
            node.tag = .expr_list;
            node.setPayload(.{ .expr_list = .{
                .elems_start = e.elems.span.start,
                .elems_len = e.elems.span.len,
                ._unused = 0,
            } });
        },
        .e_empty_list => |_| {
            node.tag = .expr_empty_list;
        },
        .e_tuple => |e| {
            node.tag = .expr_tuple;
            node.setPayload(.{ .expr_tuple = .{
                .elems_start = e.elems.span.start,
                .elems_len = e.elems.span.len,
                ._unused = 0,
            } });
        },
        .e_frac_f32 => |e| {
            node.tag = Node.Tag.expr_frac_f32;
            node.setPayload(.{ .expr_frac_f32 = .{
                .value = @bitCast(e.value),
                .has_suffix = @intFromBool(e.has_suffix),
                ._unused = 0,
            } });
        },
        .e_frac_f64 => |e| {
            node.tag = .expr_frac_f64;
            const raw: [2]u32 = @bitCast(e.value);
            node.setPayload(.{ .expr_frac_f64 = .{
                .value_lo = raw[0],
                .value_hi = raw[1],
                .has_suffix = @intFromBool(e.has_suffix),
            } });
        },
        .e_dec => |e| {
            node.tag = .expr_dec;

            // Store the RocDec value in extra_data
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            const value_as_i128: i128 = e.value.num;
            const value_as_u32s: [4]u32 = @bitCast(value_as_i128);
            for (value_as_u32s) |word| {
                _ = try store.extra_data.append(store.gpa, word);
            }

            node.setPayload(.{ .expr_dec = .{
                .extra_data_idx = extra_data_start,
                .has_suffix = @intFromBool(e.has_suffix),
                ._unused = 0,
            } });
        },
        .e_dec_small => |e| {
            node.tag = .expr_dec_small;

            // Pack small dec data
            node.setPayload(.{ .expr_dec_small = .{
                .numerator = @as(u32, @bitCast(@as(i32, e.value.numerator))),
                .denom_power = @as(u32, e.value.denominator_power_of_ten),
                .has_suffix = @intFromBool(e.has_suffix),
            } });
        },
        .e_typed_int => |e| {
            node.tag = .expr_typed_int;

            // Store type_name in data_1, value kind in data_2, value in extra_data
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            const value_as_i128: i128 = @bitCast(e.value.bytes);
            const value_as_u32s: [4]u32 = @bitCast(value_as_i128);
            for (value_as_u32s) |word| {
                _ = try store.extra_data.append(store.gpa, word);
            }
            node.setPayload(.{ .raw = .{
                .data_1 = @bitCast(e.type_name),
                .data_2 = @intFromEnum(e.value.kind),
                .data_3 = extra_data_start,
            } });
        },
        .e_typed_frac => |e| {
            node.tag = .expr_typed_frac;

            // Store type_name in data_1, value kind in data_2, value in extra_data
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            const value_as_i128: i128 = @bitCast(e.value.bytes);
            const value_as_u32s: [4]u32 = @bitCast(value_as_i128);
            for (value_as_u32s) |word| {
                _ = try store.extra_data.append(store.gpa, word);
            }
            node.setPayload(.{ .raw = .{
                .data_1 = @bitCast(e.type_name),
                .data_2 = @intFromEnum(e.value.kind),
                .data_3 = extra_data_start,
            } });
        },
        .e_str_segment => |e| {
            node.tag = .expr_string_segment;
            node.setPayload(.{ .raw = .{
                .data_1 = @intFromEnum(e.literal),
                .data_2 = 0,
                .data_3 = 0,
            } });
        },
        .e_str => |e| {
            node.tag = .expr_string;
            node.setPayload(.{ .expr_string = .{
                .segments_start = e.span.span.start,
                .segments_len = e.span.span.len,
                ._unused = 0,
            } });
        },
        .e_tag => |e| {
            node.tag = .expr_tag;
            node.setPayload(.{ .expr_tag = .{
                .name = @bitCast(e.name),
                .args_start = e.args.span.start,
                .args_len = e.args.span.len,
            } });
        },
        .e_nominal => |e| {
            node.tag = .expr_nominal;
            node.setPayload(.{ .raw = .{
                .data_1 = @intFromEnum(e.nominal_type_decl),
                .data_2 = @intFromEnum(e.backing_expr),
                .data_3 = @intFromEnum(e.backing_type),
            } });
        },
        .e_nominal_external => |e| {
            node.tag = .expr_nominal_external;
            const extra_idx: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.backing_expr));
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.backing_type));
            node.setPayload(.{ .raw = .{
                .data_1 = @intFromEnum(e.module_idx),
                .data_2 = @intCast(e.target_node_idx),
                .data_3 = extra_idx,
            } });
        },
        .e_dot_access => |e| {
            node.tag = .expr_dot_access;
            // Store extra data: field_name_region (2 u32s) + optional args (1 u32)
            const extra_idx: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, e.field_name_region.start.offset);
            _ = try store.extra_data.append(store.gpa, e.field_name_region.end.offset);
            if (e.args) |args| {
                std.debug.assert(FunctionArgs.canFit(args.span));
                const packed_span = FunctionArgs.fromDataSpanUnchecked(args.span);
                _ = try store.extra_data.append(store.gpa, packed_span.toU32() + OPTIONAL_VALUE_OFFSET);
            } else {
                _ = try store.extra_data.append(store.gpa, 0);
            }
            node.setPayload(.{ .expr_dot_access = .{
                .receiver = @intFromEnum(e.receiver),
                .field_name = @bitCast(e.field_name),
                .extra_data_idx = extra_idx,
            } });
        },
        .e_runtime_error => |e| {
            node.tag = .malformed;
            node.setPayload(.{ .diagnostic = .{
                .primary = @intFromEnum(e.diagnostic),
                .secondary = 0,
                .tertiary = 0,
            } });
        },
        .e_crash => |c| {
            node.tag = .expr_crash;
            node.setPayload(.{ .raw = .{
                .data_1 = @intFromEnum(c.msg),
                .data_2 = 0,
                .data_3 = 0,
            } });
        },
        .e_dbg => |d| {
            node.tag = .expr_dbg;
            node.setPayload(.{ .raw = .{
                .data_1 = @intFromEnum(d.expr),
                .data_2 = 0,
                .data_3 = 0,
            } });
        },
        .e_ellipsis => |_| {
            node.tag = .expr_ellipsis;
        },
        .e_anno_only => |anno| {
            node.tag = .expr_anno_only;
            node.setPayload(.{ .raw = .{
                .data_1 = @bitCast(anno.ident),
                .data_2 = 0,
                .data_3 = 0,
            } });
        },
        .e_return => |ret| {
            node.tag = .expr_return;
            node.setPayload(.{ .raw = .{
                .data_1 = @intFromEnum(ret.expr),
                .data_2 = 0,
                .data_3 = 0,
            } });
        },
        .e_type_var_dispatch => |tvd| {
            node.tag = .expr_type_var_dispatch;
            // data_1 = type_var_alias_stmt (Statement.Idx)
            // data_2 = method_name (Ident.Idx)
            // extra_data: args span start, args span len
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, tvd.args.span.start);
            _ = try store.extra_data.append(store.gpa, tvd.args.span.len);

            node.setPayload(.{ .raw = .{
                .data_1 = @intFromEnum(tvd.type_var_alias_stmt),
                .data_2 = @bitCast(tvd.method_name),
                .data_3 = extra_data_start,
            } });
        },
        .e_hosted_lambda => |hosted| {
            node.tag = .expr_hosted_lambda;
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, hosted.args.span.start);
            _ = try store.extra_data.append(store.gpa, hosted.args.span.len);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(hosted.body));

            node.setPayload(.{ .expr_hosted_lambda = .{
                .symbol_name = @bitCast(hosted.symbol_name),
                .index = hosted.index,
                .extra_data_idx = extra_data_start,
            } });
        },
        .e_low_level_lambda => |low_level| {
            node.tag = .expr_low_level;
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, low_level.args.span.start);
            _ = try store.extra_data.append(store.gpa, low_level.args.span.len);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(low_level.body));

            node.setPayload(.{ .expr_low_level = .{
                .op = @intFromEnum(low_level.op),
                .extra_data_idx = extra_data_start,
                ._unused = 0,
            } });
        },
        .e_match => |e| {
            node.tag = .expr_match;
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.cond));
            _ = try store.extra_data.append(store.gpa, e.branches.span.start);
            _ = try store.extra_data.append(store.gpa, e.branches.span.len);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.exhaustive));
            _ = try store.extra_data.append(store.gpa, @intFromBool(e.is_try_suffix));

            node.setPayload(.{ .expr_match = .{
                .extra_data_idx = extra_data_start,
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .e_if => |e| {
            node.tag = .expr_if_then_else;
            const extra_start: u32 = @intCast(store.extra_data.len());
            const num_extra_items: u32 = 3;
            _ = try store.extra_data.append(store.gpa, e.branches.span.start);
            _ = try store.extra_data.append(store.gpa, e.branches.span.len);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.final_else));

            node.setPayload(.{ .expr_if_then_else = .{
                .extra_data_idx = extra_start,
                .extra_data_end = extra_start + num_extra_items,
                ._unused = 0,
            } });
            std.debug.assert(extra_start + num_extra_items == store.extra_data.len());
        },
        .e_call => |e| {
            node.tag = .expr_call;
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, e.args.span.start);
            _ = try store.extra_data.append(store.gpa, e.args.span.len);

            node.setPayload(.{ .expr_call = .{
                .func = @intFromEnum(e.func),
                .extra_data_idx = extra_data_start,
                .called_via = @intFromEnum(e.called_via),
            } });
        },
        .e_record => |e| {
            node.tag = .expr_record;
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, e.fields.span.start);
            _ = try store.extra_data.append(store.gpa, e.fields.span.len);
            const ext_value = if (e.ext) |ext| @intFromEnum(ext) else 0;
            _ = try store.extra_data.append(store.gpa, ext_value);

            node.setPayload(.{ .expr_record = .{
                .extra_data_idx = extra_data_start,
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .e_empty_record => |_| {
            node.tag = .expr_empty_record;
        },
        .e_zero_argument_tag => |e| {
            node.tag = .expr_zero_argument_tag;
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, @bitCast(e.closure_name));
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.variant_var));
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.ext_var));
            _ = try store.extra_data.append(store.gpa, @bitCast(e.name));

            node.setPayload(.{ .expr_zero_argument_tag = .{
                .extra_data_idx = extra_data_start,
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .e_closure => |e| {
            node.tag = .expr_closure;
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.lambda_idx));
            _ = try store.extra_data.append(store.gpa, e.captures.span.start);
            _ = try store.extra_data.append(store.gpa, e.captures.span.len);
            _ = try store.extra_data.append(store.gpa, @bitCast(e.tag_name));

            node.setPayload(.{ .expr_closure = .{
                .extra_data_idx = extra_data_start,
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .e_lambda => |e| {
            node.tag = .expr_lambda;
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, e.args.span.start);
            _ = try store.extra_data.append(store.gpa, e.args.span.len);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(e.body));

            node.setPayload(.{ .expr_lambda = .{
                .extra_data_idx = extra_data_start,
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .e_binop => |e| {
            node.tag = .expr_bin_op;
            node.setPayload(.{ .expr_bin_op = .{
                .op = @intFromEnum(e.op),
                .lhs = @intFromEnum(e.lhs),
                .rhs = @intFromEnum(e.rhs),
            } });
        },
        .e_unary_minus => |e| {
            node.tag = .expr_unary_minus;
            node.setPayload(.{ .expr_unary = .{
                .expr = @intFromEnum(e.expr),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .e_unary_not => |e| {
            node.tag = .expr_unary_not;
            node.setPayload(.{ .expr_unary = .{
                .expr = @intFromEnum(e.expr),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .e_block => |e| {
            node.tag = .expr_block;
            node.setPayload(.{ .expr_block = .{
                .stmts_start = e.stmts.span.start,
                .stmts_len = e.stmts.span.len,
                .final_expr = @intFromEnum(e.final_expr),
            } });
        },
        .e_expect => |e| {
            node.tag = .expr_expect;
            node.setPayload(.{ .expr_expect = .{
                .body = @intFromEnum(e.body),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .e_for => |e| {
            node.tag = .expr_for;
            node.setPayload(.{ .expr_for = .{
                .patt = @intFromEnum(e.patt),
                .expr = @intFromEnum(e.expr),
                .body = @intFromEnum(e.body),
            } });
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
pub fn addRecordField(store: *NodeStore, recordField: CIR.RecordField, region: base.Region) Allocator.Error!CIR.RecordField.Idx {
    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = .record_field };
    node.setPayload(.{ .record_field = .{
        .name = @bitCast(recordField.name),
        .expr = @intFromEnum(recordField.value),
        ._unused = 0,
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a record destructuring to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addRecordDestruct(store: *NodeStore, record_destruct: CIR.Pattern.RecordDestruct, region: base.Region) Allocator.Error!CIR.Pattern.RecordDestruct.Idx {
    const extra_data_start: u32 = @intCast(store.extra_data.len());

    // Store kind in extra_data
    switch (record_destruct.kind) {
        .Required => |pattern_idx| {
            _ = try store.extra_data.append(store.gpa, 0);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(pattern_idx));
        },
        .SubPattern => |pattern_idx| {
            _ = try store.extra_data.append(store.gpa, 1);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(pattern_idx));
        },
    }

    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = .record_destruct };
    node.setPayload(.{ .record_destruct = .{
        .label = @bitCast(record_destruct.label),
        .ident = @bitCast(record_destruct.ident),
        .extra_data_idx = extra_data_start,
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a capture to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addCapture(store: *NodeStore, capture: CIR.Expr.Capture, region: base.Region) Allocator.Error!CIR.Expr.Capture.Idx {
    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = .lambda_capture };
    node.setPayload(.{ .lambda_capture = .{
        .name = @bitCast(capture.name),
        .scope_depth = capture.scope_depth,
        .pattern_idx = @intFromEnum(capture.pattern_idx),
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a 'match' branch to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addMatchBranch(store: *NodeStore, branch: CIR.Expr.Match.Branch, region: base.Region) Allocator.Error!CIR.Expr.Match.Branch.Idx {
    const extra_data_start: u32 = @intCast(store.extra_data.len());
    _ = try store.extra_data.append(store.gpa, branch.patterns.span.start);
    _ = try store.extra_data.append(store.gpa, branch.patterns.span.len);
    _ = try store.extra_data.append(store.gpa, @intFromEnum(branch.value));
    const guard_idx = if (branch.guard) |g| @intFromEnum(g) else 0;
    _ = try store.extra_data.append(store.gpa, guard_idx);
    _ = try store.extra_data.append(store.gpa, @intFromEnum(branch.redundant));

    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = .match_branch };
    node.setPayload(.{ .match_branch = .{
        .extra_data_idx = extra_data_start,
        ._unused1 = 0,
        ._unused2 = 0,
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a 'match' branch to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addMatchBranchPattern(store: *NodeStore, branchPattern: CIR.Expr.Match.BranchPattern, region: base.Region) Allocator.Error!CIR.Expr.Match.BranchPattern.Idx {
    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = .match_branch_pattern };
    node.setPayload(.{ .match_branch_pattern = .{
        .pattern = @intFromEnum(branchPattern.pattern),
        .degenerate = @intFromBool(branchPattern.degenerate),
        ._unused = 0,
    } });
    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a 'where' clause to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addWhereClause(store: *NodeStore, whereClause: CIR.WhereClause, region: base.Region) Allocator.Error!CIR.WhereClause.Idx {
    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = undefined };

    switch (whereClause) {
        .w_method => |where_method| {
            node.tag = .where_method;
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, where_method.args.span.start);
            _ = try store.extra_data.append(store.gpa, where_method.args.span.len);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(where_method.ret));
            node.setPayload(.{ .where_clause = .{
                .var_idx = @intFromEnum(where_method.var_),
                .name = @bitCast(where_method.method_name),
                .extra_data_idx = extra_data_start,
            } });
        },
        .w_alias => |mod_alias| {
            node.tag = .where_alias;
            node.setPayload(.{ .where_clause = .{
                .var_idx = @intFromEnum(mod_alias.var_),
                .name = @bitCast(mod_alias.alias_name),
                .extra_data_idx = 0,
            } });
        },
        .w_malformed => |malformed| {
            node.tag = .where_malformed;
            node.setPayload(.{ .where_malformed = .{
                .diagnostic = @intFromEnum(malformed.diagnostic),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a pattern to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addPattern(store: *NodeStore, pattern: CIR.Pattern, region: base.Region) Allocator.Error!CIR.Pattern.Idx {
    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = undefined };

    switch (pattern) {
        .assign => |p| {
            node.tag = .pattern_identifier;
            node.setPayload(.{ .pattern_identifier = .{
                .ident = @bitCast(p.ident),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .as => |p| {
            node.tag = .pattern_as;
            node.setPayload(.{ .pattern_as = .{
                .ident = @bitCast(p.ident),
                .pattern = @intFromEnum(p.pattern),
                ._unused = 0,
            } });
        },
        .applied_tag => |p| {
            node.tag = .pattern_applied_tag;
            node.setPayload(.{ .pattern_applied_tag = .{
                .args_start = p.args.span.start,
                .args_len = p.args.span.len,
                .name = @bitCast(p.name),
            } });
        },
        .nominal => |n| {
            node.tag = .pattern_nominal;
            node.setPayload(.{ .pattern_nominal = .{
                .nominal_type_decl = @intFromEnum(n.nominal_type_decl),
                .backing_pattern = @intFromEnum(n.backing_pattern),
                .backing_type = @intFromEnum(n.backing_type),
            } });
        },
        .nominal_external => |n| {
            node.tag = .pattern_nominal_external;
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, @intFromEnum(n.backing_pattern));
            _ = try store.extra_data.append(store.gpa, @intFromEnum(n.backing_type));
            node.setPayload(.{ .pattern_nominal_external = .{
                .module_idx = @intFromEnum(n.module_idx),
                .target_node_idx = @intCast(n.target_node_idx),
                .extra_data_idx = extra_data_start,
            } });
        },
        .record_destructure => |p| {
            node.tag = .pattern_record_destructure;
            node.setPayload(.{ .pattern_record_destructure = .{
                .destructs_start = p.destructs.span.start,
                .destructs_len = p.destructs.span.len,
                ._unused = 0,
            } });
        },
        .list => |p| {
            node.tag = .pattern_list;
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, p.patterns.span.start);
            _ = try store.extra_data.append(store.gpa, p.patterns.span.len);

            if (p.rest_info) |rest| {
                _ = try store.extra_data.append(store.gpa, 1);
                _ = try store.extra_data.append(store.gpa, rest.index);
                if (rest.pattern) |pattern_idx| {
                    _ = try store.extra_data.append(store.gpa, 1);
                    _ = try store.extra_data.append(store.gpa, @intFromEnum(pattern_idx));
                } else {
                    _ = try store.extra_data.append(store.gpa, 0);
                }
            } else {
                _ = try store.extra_data.append(store.gpa, 0);
            }

            node.setPayload(.{ .pattern_list = .{
                .extra_data_idx = extra_data_start,
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .tuple => |p| {
            node.tag = .pattern_tuple;
            node.setPayload(.{ .pattern_tuple = .{
                .patterns_start = p.patterns.span.start,
                .patterns_len = p.patterns.span.len,
                ._unused = 0,
            } });
        },
        .num_literal => |p| {
            node.tag = .pattern_num_literal;
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            const value_as_u32s: [4]u32 = @bitCast(p.value.bytes);
            for (value_as_u32s) |word| {
                _ = try store.extra_data.append(store.gpa, word);
            }
            node.setPayload(.{ .pattern_num_literal = .{
                .kind = @intFromEnum(p.kind),
                .value_kind = @intFromEnum(p.value.kind),
                .extra_data_idx = extra_data_start,
            } });
        },
        .small_dec_literal => |p| {
            node.tag = .pattern_small_dec_literal;
            node.setPayload(.{ .pattern_small_dec_literal = .{
                .numerator = @bitCast(@as(i32, p.value.numerator)),
                .denominator_power = @as(u32, p.value.denominator_power_of_ten),
                .has_suffix = @intFromBool(p.has_suffix),
            } });
        },
        .dec_literal => |p| {
            node.tag = .pattern_dec_literal;
            const extra_data_start: u32 = @intCast(store.extra_data.len());
            const value_as_u32s: [4]u32 = @bitCast(p.value.num);
            for (value_as_u32s) |word| {
                _ = try store.extra_data.append(store.gpa, word);
            }
            node.setPayload(.{ .pattern_dec_literal = .{
                .extra_data_idx = extra_data_start,
                .has_suffix = @intFromBool(p.has_suffix),
                ._unused = 0,
            } });
        },
        .str_literal => |p| {
            node.tag = .pattern_str_literal;
            node.setPayload(.{ .pattern_str_literal = .{
                .literal = @intFromEnum(p.literal),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .frac_f32_literal => |p| {
            node.tag = Node.Tag.pattern_f32_literal;
            node.setPayload(.{ .pattern_frac_f32 = .{
                .value = @bitCast(p.value),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .frac_f64_literal => |p| {
            node.tag = Node.Tag.pattern_f64_literal;
            const raw: u64 = @bitCast(p.value);
            node.setPayload(.{ .pattern_frac_f64 = .{
                .value_lo = @intCast(raw & 0xFFFFFFFF),
                .value_hi = @intCast(raw >> 32),
                ._unused = 0,
            } });
        },
        .underscore => {
            node.tag = .pattern_underscore;
        },
        .runtime_error => |e| {
            node.tag = .malformed;
            node.setPayload(.{ .pattern_malformed = .{
                .diagnostic = @intFromEnum(e.diagnostic),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
    }

    const node_idx = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Adds a type annotation to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addTypeAnno(store: *NodeStore, typeAnno: CIR.TypeAnno, region: base.Region) Allocator.Error!CIR.TypeAnno.Idx {
    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = undefined };

    switch (typeAnno) {
        .apply => |a| {
            node.tag = .ty_apply;
            const ed_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, a.args.span.len);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(@as(CIR.TypeAnno.LocalOrExternal.Tag, std.meta.activeTag(a.base))));
            switch (a.base) {
                .builtin => |builtin_type| {
                    _ = try store.extra_data.append(store.gpa, @intFromEnum(builtin_type));
                },
                .local => |local| {
                    _ = try store.extra_data.append(store.gpa, @intFromEnum(local.decl_idx));
                },
                .external => |ext| {
                    _ = try store.extra_data.append(store.gpa, @intFromEnum(ext.module_idx));
                    _ = try store.extra_data.append(store.gpa, @intCast(ext.target_node_idx));
                },
            }
            node.setPayload(.{ .ty_apply = .{
                .name = @bitCast(a.name),
                .args_start = a.args.span.start,
                .extra_data_idx = ed_start,
            } });
        },
        .rigid_var => |tv| {
            node.tag = .ty_rigid_var;
            node.setPayload(.{ .ty_rigid_var = .{
                .name = @bitCast(tv.name),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .rigid_var_lookup => |tv| {
            node.tag = .ty_rigid_var_lookup;
            node.setPayload(.{ .ty_rigid_var = .{
                .name = @intFromEnum(tv.ref),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .underscore => |_| {
            node.tag = .ty_underscore;
        },
        .lookup => |t| {
            node.tag = .ty_lookup;
            const ed_start: u32 = @intCast(store.extra_data.len());
            switch (t.base) {
                .builtin => |builtin_type| {
                    _ = try store.extra_data.append(store.gpa, @intFromEnum(builtin_type));
                },
                .local => |local| {
                    _ = try store.extra_data.append(store.gpa, @intFromEnum(local.decl_idx));
                },
                .external => |ext| {
                    _ = try store.extra_data.append(store.gpa, @intFromEnum(ext.module_idx));
                    _ = try store.extra_data.append(store.gpa, @intCast(ext.target_node_idx));
                },
            }
            node.setPayload(.{ .ty_lookup = .{
                .name = @bitCast(t.name),
                .base = @intFromEnum(@as(CIR.TypeAnno.LocalOrExternal.Tag, std.meta.activeTag(t.base))),
                .extra_data_idx = ed_start,
            } });
        },
        .tag_union => |tu| {
            node.tag = .ty_tag_union;
            node.setPayload(.{ .ty_tag_union = .{
                .tags_start = tu.tags.span.start,
                .tags_len = tu.tags.span.len,
                .ext_plus_one = if (tu.ext) |ext| @intFromEnum(ext) + OPTIONAL_VALUE_OFFSET else 0,
            } });
        },
        .tag => |t| {
            node.tag = .ty_tag;
            node.setPayload(.{ .ty_tag = .{
                .name = @bitCast(t.name),
                .args_start = t.args.span.start,
                .args_len = t.args.span.len,
            } });
        },
        .tuple => |t| {
            node.tag = .ty_tuple;
            node.setPayload(.{ .ty_tuple = .{
                .elems_start = t.elems.span.start,
                .elems_len = t.elems.span.len,
                ._unused = 0,
            } });
        },
        .record => |r| {
            node.tag = .ty_record;
            node.setPayload(.{ .ty_record = .{
                .fields_start = r.fields.span.start,
                .fields_len = r.fields.span.len,
                .ext_plus_one = if (r.ext) |ext| @intFromEnum(ext) + OPTIONAL_VALUE_OFFSET else 0,
            } });
        },
        .@"fn" => |f| {
            node.tag = .ty_fn;
            const ed_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, if (f.effectful) @as(u32, 1) else 0);
            _ = try store.extra_data.append(store.gpa, @intFromEnum(f.ret));
            node.setPayload(.{ .ty_fn = .{
                .args_start = f.args.span.start,
                .args_len = f.args.span.len,
                .extra_data_idx = ed_start,
            } });
        },
        .parens => |p| {
            node.tag = .ty_parens;
            node.setPayload(.{ .ty_parens = .{
                .anno = @intFromEnum(p.anno),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
        },
        .malformed => |m| {
            node.tag = .ty_malformed;
            node.setPayload(.{ .ty_malformed = .{
                .diagnostic = @intFromEnum(m.diagnostic),
                ._unused1 = 0,
                ._unused2 = 0,
            } });
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
pub fn addTypeHeader(store: *NodeStore, typeHeader: CIR.TypeHeader, region: base.Region) Allocator.Error!CIR.TypeHeader.Idx {
    std.debug.assert(typeHeader.args.span.start <= std.math.maxInt(u16));
    std.debug.assert(typeHeader.args.span.len <= std.math.maxInt(u16));
    const packed_args: u32 = (@as(u32, @intCast(typeHeader.args.span.start)) << 16) | @as(u32, @intCast(typeHeader.args.span.len));

    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = .type_header };
    node.setPayload(.{ .type_header = .{
        .name = @bitCast(typeHeader.name),
        .relative_name = @bitCast(typeHeader.relative_name),
        .packed_args = packed_args,
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an annotation record field to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addAnnoRecordField(store: *NodeStore, annoRecordField: CIR.TypeAnno.RecordField, region: base.Region) Allocator.Error!CIR.TypeAnno.RecordField.Idx {
    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = .ty_record_field };
    node.setPayload(.{ .ty_record_field = .{
        .name = @bitCast(annoRecordField.name),
        .ty = @intFromEnum(annoRecordField.ty),
        ._unused = 0,
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an annotation to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addAnnotation(store: *NodeStore, annotation: CIR.Annotation, region: base.Region) Allocator.Error!CIR.Annotation.Idx {
    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = .annotation };

    if (annotation.where) |where_clause| {
        const extra_start: u32 = @intCast(store.extra_data.len());
        _ = try store.extra_data.append(store.gpa, where_clause.span.start);
        _ = try store.extra_data.append(store.gpa, where_clause.span.len);
        node.setPayload(.{ .annotation = .{
            .anno = @intFromEnum(annotation.anno),
            .has_where = 1,
            .extra_data_idx = extra_start,
        } });
    } else {
        node.setPayload(.{ .annotation = .{
            .anno = @intFromEnum(annotation.anno),
            .has_where = 0,
            .extra_data_idx = 0,
        } });
    }

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an exposed item to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addExposedItem(store: *NodeStore, exposedItem: CIR.ExposedItem, region: base.Region) Allocator.Error!CIR.ExposedItem.Idx {
    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = .exposed_item };
    node.setPayload(.{ .exposed_item = .{
        .name = @bitCast(exposedItem.name),
        .alias = if (exposedItem.alias) |alias| @bitCast(alias) else 0,
        .is_wildcard = @intFromBool(exposedItem.is_wildcard),
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a definition to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addDef(store: *NodeStore, def: CIR.Def, region: base.Region) Allocator.Error!CIR.Def.Idx {
    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = .def };

    const extra_start: u32 = @intCast(store.extra_data.len());
    _ = try store.extra_data.append(store.gpa, @intFromEnum(def.pattern));
    _ = try store.extra_data.append(store.gpa, @intFromEnum(def.expr));
    const kind_encoded = def.kind.encode();
    _ = try store.extra_data.append(store.gpa, kind_encoded[0]);
    _ = try store.extra_data.append(store.gpa, kind_encoded[1]);
    const anno_idx = if (def.annotation) |anno| @intFromEnum(anno) else 0;
    _ = try store.extra_data.append(store.gpa, anno_idx);

    node.setPayload(.{ .def = .{
        .extra_data_idx = extra_start,
        ._unused1 = 0,
        ._unused2 = 0,
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @enumFromInt(@intFromEnum(nid));
}

/// Retrieves a definition from the store.
pub fn getDef(store: *const NodeStore, def_idx: CIR.Def.Idx) CIR.Def {
    const nid: Node.Idx = @enumFromInt(@intFromEnum(def_idx));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .def);

    const payload = node.getPayload();
    const extra_start = payload.def.extra_data_idx;
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

/// Updates the expression field of an existing definition.
/// This is used during constant folding to replace expressions with their folded equivalents.
pub fn setDefExpr(store: *NodeStore, def_idx: CIR.Def.Idx, new_expr: CIR.Expr.Idx) void {
    const nid: Node.Idx = @enumFromInt(@intFromEnum(def_idx));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .def);

    const payload = node.getPayload();
    const extra_start = payload.def.extra_data_idx;
    store.extra_data.items.items[extra_start + 1] = @intFromEnum(new_expr);
}

/// Retrieves a capture from the store.
pub fn getCapture(store: *const NodeStore, capture_idx: CIR.Expr.Capture.Idx) CIR.Expr.Capture {
    const nid: Node.Idx = @enumFromInt(@intFromEnum(capture_idx));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .lambda_capture);

    const payload = node.getPayload();
    return CIR.Expr.Capture{
        .name = @bitCast(payload.lambda_capture.name),
        .scope_depth = payload.lambda_capture.scope_depth,
        .pattern_idx = @enumFromInt(payload.lambda_capture.pattern_idx),
    };
}

/// Retrieves a record field from the store.
pub fn getRecordField(store: *const NodeStore, idx: CIR.RecordField.Idx) CIR.RecordField {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    const payload = node.getPayload();
    return CIR.RecordField{
        .name = @bitCast(payload.record_field.name),
        .value = @enumFromInt(payload.record_field.expr),
    };
}

/// Retrieves a record destructure from the store.
pub fn getRecordDestruct(store: *const NodeStore, idx: CIR.Pattern.RecordDestruct.Idx) CIR.Pattern.RecordDestruct {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
    const node = store.nodes.get(node_idx);

    std.debug.assert(node.tag == .record_destruct);

    const payload = node.getPayload();
    const extra_start = payload.record_destruct.extra_data_idx;
    const extra_data = store.extra_data.items.items[extra_start..][0..2];
    const kind_tag = extra_data[0];

    const kind = switch (kind_tag) {
        0 => CIR.Pattern.RecordDestruct.Kind{ .Required = @enumFromInt(extra_data[1]) },
        1 => CIR.Pattern.RecordDestruct.Kind{ .SubPattern = @enumFromInt(extra_data[1]) },
        else => unreachable,
    };

    return CIR.Pattern.RecordDestruct{
        .label = @bitCast(payload.record_destruct.label),
        .ident = @bitCast(payload.record_destruct.ident),
        .kind = kind,
    };
}

/// Retrieves an if branch from the store.
pub fn getIfBranch(store: *const NodeStore, if_branch_idx: CIR.Expr.IfBranch.Idx) CIR.Expr.IfBranch {
    const nid: Node.Idx = @enumFromInt(@intFromEnum(if_branch_idx));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .if_branch);

    const payload = node.getPayload();
    return CIR.Expr.IfBranch{
        .cond = @enumFromInt(payload.if_branch.cond),
        .body = @enumFromInt(payload.if_branch.body),
    };
}

/// Check if a raw node index refers to a definition node.
/// This is useful when exposed items might be either definitions or type declarations.
pub fn isDefNode(store: *const NodeStore, node_idx: u16) bool {
    const nid: Node.Idx = @enumFromInt(node_idx);
    const node = store.nodes.get(nid);
    return node.tag == .def;
}

/// Generic function to get the top of any scratch buffer
pub fn scratchTop(store: *NodeStore, comptime field_name: []const u8) u32 {
    return @field(store.scratch.?, field_name).top();
}

/// Generic function to add an item to any scratch buffer
pub fn addScratch(store: *NodeStore, comptime field_name: []const u8, idx: anytype) Allocator.Error!void {
    try @field(store.scratch.?, field_name).append(idx);
}

/// Generic function to clear any scratch buffer from a given position
pub fn clearScratchFrom(store: *NodeStore, comptime field_name: []const u8, start: u32) void {
    @field(store.scratch.?, field_name).clearFrom(start);
}

/// Generic function to create a span from any scratch buffer
pub fn spanFrom(store: *NodeStore, comptime field_name: []const u8, comptime SpanType: type, start: u32) Allocator.Error!SpanType {
    const scratch_field = &@field(store.scratch.?, field_name);
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
    return store.scratchTop("exprs");
}

/// Adds a scratch expression to temporary storage.
pub fn addScratchExpr(store: *NodeStore, idx: CIR.Expr.Idx) Allocator.Error!void {
    try store.addScratch("exprs", idx);
}

/// Adds a capture index to the scratch captures list for building spans.
pub fn addScratchCapture(store: *NodeStore, idx: CIR.Expr.Capture.Idx) Allocator.Error!void {
    try store.addScratch("captures", idx);
}

/// Adds a statement index to the scratch statements list for building spans.
pub fn addScratchStatement(store: *NodeStore, idx: CIR.Statement.Idx) Allocator.Error!void {
    try store.addScratch("statements", idx);
}

/// Computes the span of an expression starting from a given index.
pub fn exprSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Expr.Span {
    return try store.spanFrom("exprs", CIR.Expr.Span, start);
}

/// Computes the span of captures starting from a given index.
pub fn capturesSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Expr.Capture.Span {
    return try store.spanFrom("captures", CIR.Expr.Capture.Span, start);
}

/// Creates a statement span from the given start position to the current top of scratch statements.
pub fn statementSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Statement.Span {
    return try store.spanFrom("statements", CIR.Statement.Span, start);
}

/// Clears scratch expressions starting from a specified index.
pub fn clearScratchExprsFrom(store: *NodeStore, start: u32) void {
    store.clearScratchFrom("exprs", start);
}

/// Returns a slice of expressions from the scratch space.
pub fn exprSlice(store: *const NodeStore, span: CIR.Expr.Span) []CIR.Expr.Idx {
    return store.sliceFromSpan(CIR.Expr.Idx, span.span);
}

/// Returns the top index for scratch definitions.
pub fn scratchDefTop(store: *NodeStore) u32 {
    return store.scratchTop("defs");
}

/// Adds a scratch definition to temporary storage.
pub fn addScratchDef(store: *NodeStore, idx: CIR.Def.Idx) Allocator.Error!void {
    try store.addScratch("defs", idx);
}

/// Adds a type annotation to the scratch buffer.
pub fn addScratchTypeAnno(store: *NodeStore, idx: CIR.TypeAnno.Idx) Allocator.Error!void {
    try store.addScratch("type_annos", idx);
}

/// Adds a where clause to the scratch buffer.
pub fn addScratchWhereClause(store: *NodeStore, idx: CIR.WhereClause.Idx) Allocator.Error!void {
    try store.addScratch("where_clauses", idx);
}

/// Returns the current top of the scratch type annotations buffer.
pub fn scratchTypeAnnoTop(store: *NodeStore) u32 {
    return store.scratchTop("type_annos");
}

/// Returns the current top of the scratch where clauses buffer.
pub fn scratchWhereClauseTop(store: *NodeStore) u32 {
    return store.scratchTop("where_clauses");
}

/// Clears scratch type annotations from the given index.
pub fn clearScratchTypeAnnosFrom(store: *NodeStore, from: u32) void {
    store.clearScratchFrom("type_annos", from);
}

/// Clears scratch where clauses from the given index.
pub fn clearScratchWhereClausesFrom(store: *NodeStore, from: u32) void {
    store.clearScratchFrom("where_clauses", from);
}

/// Creates a span from the scratch type annotations starting at the given index.
pub fn typeAnnoSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.TypeAnno.Span {
    return try store.spanFrom("type_annos", CIR.TypeAnno.Span, start);
}

/// Returns a span from the scratch anno record fields starting at the given index.
pub fn annoRecordFieldSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.TypeAnno.RecordField.Span {
    return try store.spanFrom("anno_record_fields", CIR.TypeAnno.RecordField.Span, start);
}

/// Returns a span from the scratch record fields starting at the given index.
pub fn recordFieldSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.RecordField.Span {
    return try store.spanFrom("record_fields", CIR.RecordField.Span, start);
}

/// Returns a span from the scratch where clauses starting at the given index.
pub fn whereClauseSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.WhereClause.Span {
    return try store.spanFrom("where_clauses", CIR.WhereClause.Span, start);
}

/// Returns the current top of the scratch exposed items buffer.
pub fn scratchExposedItemTop(store: *NodeStore) u32 {
    return store.scratchTop("exposed_items");
}

/// Adds an exposed item to the scratch buffer.
pub fn addScratchExposedItem(store: *NodeStore, idx: CIR.ExposedItem.Idx) Allocator.Error!void {
    try store.addScratch("exposed_items", idx);
}

/// Creates a span from the scratch exposed items starting at the given index.
pub fn exposedItemSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.ExposedItem.Span {
    return try store.spanFrom("exposed_items", CIR.ExposedItem.Span, start);
}

/// Clears scratch exposed items from the given index.
pub fn clearScratchExposedItemsFrom(store: *NodeStore, start: u32) void {
    store.clearScratchFrom("exposed_items", start);
}

/// Returns the start position for a new Span of annoRecordFieldIdxs in scratch
pub fn scratchAnnoRecordFieldTop(store: *NodeStore) u32 {
    return store.scratchTop("anno_record_fields");
}

/// Places a new CIR.TypeAnno.RecordField.Idx in the scratch. Will panic on OOM.
pub fn addScratchAnnoRecordField(store: *NodeStore, idx: CIR.TypeAnno.RecordField.Idx) Allocator.Error!void {
    try store.addScratch("anno_record_fields", idx);
}

/// Clears any AnnoRecordFieldIds added to scratch from start until the end.
pub fn clearScratchAnnoRecordFieldsFrom(store: *NodeStore, start: u32) void {
    store.clearScratchFrom("anno_record_fields", start);
}

/// Returns a new AnnoRecordField slice so that the caller can iterate through
/// all items in the span.
pub fn annoRecordFieldSlice(store: *NodeStore, span: CIR.TypeAnno.RecordField.Span) []CIR.TypeAnno.RecordField.Idx {
    return store.sliceFromSpan(CIR.TypeAnno.RecordField.Idx, span.span);
}

/// Computes the span of a definition starting from a given index.
pub fn defSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Def.Span {
    return try store.spanFrom("defs", CIR.Def.Span, start);
}

/// Retrieves a slice of record destructures from the store.
pub fn recordDestructSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Pattern.RecordDestruct.Span {
    return try store.spanFrom("record_destructs", CIR.Pattern.RecordDestruct.Span, start);
}

/// Returns the current top of the scratch patterns buffer.
pub fn scratchPatternTop(store: *NodeStore) u32 {
    return store.scratchTop("patterns");
}

/// Adds a pattern to the scratch patterns list for building spans.
pub fn addScratchPattern(store: *NodeStore, idx: CIR.Pattern.Idx) Allocator.Error!void {
    try store.addScratch("patterns", idx);
}

/// Returns the current top of the scratch record destructures buffer.
pub fn scratchRecordDestructTop(store: *NodeStore) u32 {
    return store.scratchTop("record_destructs");
}

/// Adds a record destructure to the scratch record destructures list for building spans.
pub fn addScratchRecordDestruct(store: *NodeStore, idx: CIR.Pattern.RecordDestruct.Idx) Allocator.Error!void {
    try store.addScratch("record_destructs", idx);
}

/// Creates a pattern span from the given start position to the current top of scratch patterns.
pub fn patternSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Pattern.Span {
    return try store.spanFrom("patterns", CIR.Pattern.Span, start);
}

/// Clears scratch definitions starting from a specified index.
pub fn clearScratchDefsFrom(store: *NodeStore, start: u32) void {
    store.clearScratchFrom("defs", start);
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
    return store.scratchTop("if_branches");
}

/// Adds an if branch to the scratch if branches list for building spans.
pub fn addScratchIfBranch(store: *NodeStore, if_branch_idx: CIR.Expr.IfBranch.Idx) Allocator.Error!void {
    try store.addScratch("if_branches", if_branch_idx);
}

/// Creates an if branch span from the given start position to the current top of scratch if branches.
pub fn ifBranchSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Expr.IfBranch.Span {
    return try store.spanFrom("if_branches", CIR.Expr.IfBranch.Span, start);
}

/// Adds an if branch to the store and returns its index.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addIfBranch(store: *NodeStore, if_branch: CIR.Expr.IfBranch, region: base.Region) Allocator.Error!CIR.Expr.IfBranch.Idx {
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
pub fn addDiagnostic(store: *NodeStore, reason: CIR.Diagnostic) Allocator.Error!CIR.Diagnostic.Idx {
    var node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = undefined };
    var region = base.Region.zero();

    switch (reason) {
        .not_implemented => |r| {
            node.tag = .diag_not_implemented;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @intFromEnum(r.feature), .secondary = 0, .tertiary = 0 } });
        },
        .invalid_num_literal => |r| {
            node.tag = .diag_invalid_num_literal;
            region = r.region;
        },
        .empty_tuple => |r| {
            node.tag = .diag_empty_tuple;
            region = r.region;
        },
        .ident_already_in_scope => |r| {
            node.tag = .diag_ident_already_in_scope;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.ident), .secondary = 0, .tertiary = 0 } });
        },
        .exposed_but_not_implemented => |r| {
            node.tag = .diagnostic_exposed_but_not_implemented;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.ident), .secondary = 0, .tertiary = 0 } });
        },
        .redundant_exposed => |r| {
            node.tag = .diag_redundant_exposed;
            region = r.region;
            const extra_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, r.original_region.start.offset);
            _ = try store.extra_data.append(store.gpa, r.original_region.end.offset);
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.ident), .secondary = extra_start, .tertiary = 0 } });
        },
        .ident_not_in_scope => |r| {
            node.tag = .diag_ident_not_in_scope;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.ident), .secondary = 0, .tertiary = 0 } });
        },
        .self_referential_definition => |r| {
            node.tag = .diag_self_referential_definition;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.ident), .secondary = 0, .tertiary = 0 } });
        },
        .qualified_ident_does_not_exist => |r| {
            node.tag = .diag_qualified_ident_does_not_exist;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.ident), .secondary = 0, .tertiary = 0 } });
        },
        .invalid_top_level_statement => |r| {
            node.tag = .diag_invalid_top_level_statement;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @intFromEnum(r.stmt), .secondary = 0, .tertiary = 0 } });
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
        .if_expr_without_else => |r| {
            node.tag = .diag_if_expr_without_else;
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
        .type_module_missing_matching_type => |r| {
            node.tag = .diag_type_module_missing_matching_type;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.module_name), .secondary = 0, .tertiary = 0 } });
        },
        .default_app_missing_main => |r| {
            node.tag = .diag_default_app_missing_main;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.module_name), .secondary = 0, .tertiary = 0 } });
        },
        .default_app_wrong_arity => |r| {
            node.tag = .diag_default_app_wrong_arity;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = r.arity, .secondary = 0, .tertiary = 0 } });
        },
        .cannot_import_default_app => |r| {
            node.tag = .diag_cannot_import_default_app;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.module_name), .secondary = 0, .tertiary = 0 } });
        },
        .execution_requires_app_or_default_app => |r| {
            node.tag = .diag_execution_requires_app_or_default_app;
            region = r.region;
        },
        .type_name_case_mismatch => |r| {
            node.tag = .diag_type_name_case_mismatch;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.module_name), .secondary = @bitCast(r.type_name), .tertiary = 0 } });
        },
        .module_header_deprecated => |r| {
            node.tag = .diag_module_header_deprecated;
            region = r.region;
        },
        .redundant_expose_main_type => |r| {
            node.tag = .diag_redundant_expose_main_type;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.type_name), .secondary = @bitCast(r.module_name), .tertiary = 0 } });
        },
        .invalid_main_type_rename_in_exposing => |r| {
            node.tag = .diag_invalid_main_type_rename_in_exposing;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.type_name), .secondary = @bitCast(r.alias), .tertiary = 0 } });
        },
        .var_across_function_boundary => |r| {
            node.tag = .diag_var_across_function_boundary;
            region = r.region;
        },
        .shadowing_warning => |r| {
            node.tag = .diag_shadowing_warning;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.ident), .secondary = r.original_region.start.offset, .tertiary = r.original_region.end.offset } });
        },
        .type_redeclared => |r| {
            node.tag = .diag_type_redeclared;
            region = r.redeclared_region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.name), .secondary = r.original_region.start.offset, .tertiary = r.original_region.end.offset } });
        },
        .undeclared_type => |r| {
            node.tag = .diag_undeclared_type;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.name), .secondary = 0, .tertiary = 0 } });
        },
        .type_alias_but_needed_nominal => |r| {
            node.tag = .diag_type_alias_but_needed_nominal;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.name), .secondary = 0, .tertiary = 0 } });
        },
        .undeclared_type_var => |r| {
            node.tag = .diag_undeclared_type_var;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.name), .secondary = 0, .tertiary = 0 } });
        },
        .type_alias_redeclared => |r| {
            node.tag = .diag_type_alias_redeclared;
            region = r.redeclared_region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.name), .secondary = r.original_region.start.offset, .tertiary = r.original_region.end.offset } });
        },
        .tuple_elem_not_canonicalized => |r| {
            node.tag = .diag_tuple_elem_not_canonicalized;
            region = r.region;
        },
        .module_not_found => |r| {
            node.tag = .diag_module_not_found;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.module_name), .secondary = 0, .tertiary = 0 } });
        },
        .value_not_exposed => |r| {
            node.tag = .diag_value_not_exposed;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.module_name), .secondary = @bitCast(r.value_name), .tertiary = 0 } });
        },
        .type_not_exposed => |r| {
            node.tag = .diag_type_not_exposed;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.module_name), .secondary = @bitCast(r.type_name), .tertiary = 0 } });
        },
        .type_from_missing_module => |r| {
            node.tag = .diag_type_from_missing_module;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.module_name), .secondary = @bitCast(r.type_name), .tertiary = 0 } });
        },
        .module_not_imported => |r| {
            node.tag = .diag_module_not_imported;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.module_name), .secondary = 0, .tertiary = 0 } });
        },
        .nested_type_not_found => |r| {
            node.tag = .diag_nested_type_not_found;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.parent_name), .secondary = @bitCast(r.nested_name), .tertiary = 0 } });
        },
        .nested_value_not_found => |r| {
            node.tag = .diag_nested_value_not_found;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.parent_name), .secondary = @bitCast(r.nested_name), .tertiary = 0 } });
        },
        .too_many_exports => |r| {
            node.tag = .diag_too_many_exports;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = r.count, .secondary = 0, .tertiary = 0 } });
        },
        .nominal_type_redeclared => |r| {
            node.tag = .diag_nominal_type_redeclared;
            region = r.redeclared_region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.name), .secondary = r.original_region.start.offset, .tertiary = r.original_region.end.offset } });
        },
        .type_shadowed_warning => |r| {
            node.tag = .diag_type_shadowed_warning;
            region = r.region;
            const extra_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, r.original_region.start.offset);
            _ = try store.extra_data.append(store.gpa, r.original_region.end.offset);
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.name), .secondary = @intFromBool(r.cross_scope), .tertiary = extra_start } });
        },
        .type_parameter_conflict => |r| {
            node.tag = .diag_type_parameter_conflict;
            region = r.region;
            const extra_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, r.original_region.start.offset);
            _ = try store.extra_data.append(store.gpa, r.original_region.end.offset);
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.name), .secondary = @bitCast(r.parameter_name), .tertiary = extra_start } });
        },
        .unused_variable => |r| {
            node.tag = .diag_unused_variable;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.ident), .secondary = 0, .tertiary = 0 } });
        },
        .used_underscore_variable => |r| {
            node.tag = .diag_used_underscore_variable;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.ident), .secondary = 0, .tertiary = 0 } });
        },
        .duplicate_record_field => |r| {
            node.tag = .diag_duplicate_record_field;
            region = r.duplicate_region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.field_name), .secondary = r.original_region.start.offset, .tertiary = r.original_region.end.offset } });
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
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.name), .secondary = @bitCast(r.suggested_name), .tertiary = 0 } });
        },
        .type_var_marked_unused => |r| {
            node.tag = .diag_type_var_marked_unused;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.name), .secondary = @bitCast(r.suggested_name), .tertiary = 0 } });
        },
        .type_var_starting_with_dollar => |r| {
            node.tag = .diag_type_var_starting_with_dollar;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.name), .secondary = @bitCast(r.suggested_name), .tertiary = 0 } });
        },
        .underscore_in_type_declaration => |r| {
            node.tag = .diag_underscore_in_type_declaration;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @intFromBool(r.is_alias), .secondary = 0, .tertiary = 0 } });
        },
        .break_outside_loop => |r| {
            node.tag = .diag_break_outside_loop;
            region = r.region;
        },
        .mutually_recursive_type_aliases => |r| {
            node.tag = .diag_mutually_recursive_type_aliases;
            region = r.region;
            const extra_start: u32 = @intCast(store.extra_data.len());
            _ = try store.extra_data.append(store.gpa, r.other_region.start.offset);
            _ = try store.extra_data.append(store.gpa, r.other_region.end.offset);
            node.setPayload(.{ .diagnostic = .{ .primary = @bitCast(r.name), .secondary = @bitCast(r.other_name), .tertiary = extra_start } });
        },
        .deprecated_number_suffix => |r| {
            node.tag = .diag_deprecated_number_suffix;
            region = r.region;
            node.setPayload(.{ .diagnostic = .{ .primary = @intFromEnum(r.suffix), .secondary = @intFromEnum(r.suggested), .tertiary = 0 } });
        },
    }

    const nid = @intFromEnum(try store.nodes.append(store.gpa, node));
    _ = try store.regions.append(store.gpa, region);

    // append to our scratch so we can get a span later of all our diagnostics
    try store.addScratch("diagnostics", @as(CIR.Diagnostic.Idx, @enumFromInt(nid)));

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
pub fn addMalformed(store: *NodeStore, diagnostic_idx: CIR.Diagnostic.Idx, region: Region) Allocator.Error!Node.Idx {
    var malformed_node = Node{ .data_1 = 0, .data_2 = 0, .data_3 = 0, .tag = .malformed };
    malformed_node.setPayload(.{ .diagnostic = .{
        .primary = @intFromEnum(diagnostic_idx),
        .secondary = 0,
        .tertiary = 0,
    } });
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
    const p = node.getPayload().diagnostic;

    switch (node.tag) {
        .diag_not_implemented => return CIR.Diagnostic{ .not_implemented = .{
            .feature = @enumFromInt(p.primary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_invalid_num_literal => return CIR.Diagnostic{ .invalid_num_literal = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_empty_tuple => return CIR.Diagnostic{ .empty_tuple = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_ident_already_in_scope => return CIR.Diagnostic{ .ident_already_in_scope = .{
            .ident = @bitCast(p.primary),
            .region = store.getRegionAt(node_idx),
        } },
        .diagnostic_exposed_but_not_implemented => return CIR.Diagnostic{ .exposed_but_not_implemented = .{
            .ident = @bitCast(p.primary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_redundant_exposed => {
            const extra_data = store.extra_data.items.items[p.secondary..];
            const original_start = extra_data[0];
            const original_end = extra_data[1];
            return CIR.Diagnostic{ .redundant_exposed = .{
                .ident = @bitCast(p.primary),
                .region = store.getRegionAt(node_idx),
                .original_region = Region{
                    .start = .{ .offset = original_start },
                    .end = .{ .offset = original_end },
                },
            } };
        },
        .diag_ident_not_in_scope => return CIR.Diagnostic{ .ident_not_in_scope = .{
            .ident = @bitCast(p.primary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_self_referential_definition => return CIR.Diagnostic{ .self_referential_definition = .{
            .ident = @bitCast(p.primary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_qualified_ident_does_not_exist => return CIR.Diagnostic{ .qualified_ident_does_not_exist = .{
            .ident = @bitCast(p.primary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_invalid_top_level_statement => return CIR.Diagnostic{ .invalid_top_level_statement = .{
            .stmt = @enumFromInt(p.primary),
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
        .diag_if_expr_without_else => return CIR.Diagnostic{ .if_expr_without_else = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_var_across_function_boundary => return CIR.Diagnostic{ .var_across_function_boundary = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_shadowing_warning => return CIR.Diagnostic{ .shadowing_warning = .{
            .ident = @bitCast(p.primary),
            .region = store.getRegionAt(node_idx),
            .original_region = .{
                .start = .{ .offset = p.secondary },
                .end = .{ .offset = p.tertiary },
            },
        } },
        .diag_type_redeclared => return CIR.Diagnostic{ .type_redeclared = .{
            .name = @bitCast(p.primary),
            .redeclared_region = store.getRegionAt(node_idx),
            .original_region = .{
                .start = .{ .offset = p.secondary },
                .end = .{ .offset = p.tertiary },
            },
        } },
        .diag_undeclared_type => return CIR.Diagnostic{ .undeclared_type = .{
            .name = @bitCast(p.primary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_type_alias_but_needed_nominal => return CIR.Diagnostic{ .type_alias_but_needed_nominal = .{
            .name = @bitCast(p.primary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_tuple_elem_not_canonicalized => return CIR.Diagnostic{ .tuple_elem_not_canonicalized = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_module_not_found => return CIR.Diagnostic{ .module_not_found = .{
            .module_name = @as(base.Ident.Idx, @bitCast(p.primary)),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_value_not_exposed => return CIR.Diagnostic{ .value_not_exposed = .{
            .module_name = @as(base.Ident.Idx, @bitCast(p.primary)),
            .value_name = @as(base.Ident.Idx, @bitCast(p.secondary)),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_type_not_exposed => return CIR.Diagnostic{ .type_not_exposed = .{
            .module_name = @as(base.Ident.Idx, @bitCast(p.primary)),
            .type_name = @as(base.Ident.Idx, @bitCast(p.secondary)),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_type_from_missing_module => return CIR.Diagnostic{ .type_from_missing_module = .{
            .module_name = @as(base.Ident.Idx, @bitCast(p.primary)),
            .type_name = @as(base.Ident.Idx, @bitCast(p.secondary)),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_module_not_imported => return CIR.Diagnostic{ .module_not_imported = .{
            .module_name = @as(base.Ident.Idx, @bitCast(p.primary)),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_nested_type_not_found => return CIR.Diagnostic{ .nested_type_not_found = .{
            .parent_name = @as(base.Ident.Idx, @bitCast(p.primary)),
            .nested_name = @as(base.Ident.Idx, @bitCast(p.secondary)),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_nested_value_not_found => return CIR.Diagnostic{ .nested_value_not_found = .{
            .parent_name = @as(base.Ident.Idx, @bitCast(p.primary)),
            .nested_name = @as(base.Ident.Idx, @bitCast(p.secondary)),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_too_many_exports => return CIR.Diagnostic{ .too_many_exports = .{
            .count = p.primary,
            .region = store.getRegionAt(node_idx),
        } },
        .diag_undeclared_type_var => return CIR.Diagnostic{ .undeclared_type_var = .{
            .name = @bitCast(p.primary),
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
        .diag_type_module_missing_matching_type => return CIR.Diagnostic{ .type_module_missing_matching_type = .{
            .module_name = @bitCast(p.primary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_default_app_missing_main => return CIR.Diagnostic{ .default_app_missing_main = .{
            .module_name = @bitCast(p.primary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_default_app_wrong_arity => return CIR.Diagnostic{ .default_app_wrong_arity = .{
            .arity = p.primary,
            .region = store.getRegionAt(node_idx),
        } },
        .diag_cannot_import_default_app => return CIR.Diagnostic{ .cannot_import_default_app = .{
            .module_name = @bitCast(p.primary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_execution_requires_app_or_default_app => return CIR.Diagnostic{ .execution_requires_app_or_default_app = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_type_name_case_mismatch => return CIR.Diagnostic{ .type_name_case_mismatch = .{
            .module_name = @bitCast(p.primary),
            .type_name = @bitCast(p.secondary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_module_header_deprecated => return CIR.Diagnostic{ .module_header_deprecated = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_redundant_expose_main_type => return CIR.Diagnostic{ .redundant_expose_main_type = .{
            .type_name = @bitCast(p.primary),
            .module_name = @bitCast(p.secondary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_invalid_main_type_rename_in_exposing => return CIR.Diagnostic{ .invalid_main_type_rename_in_exposing = .{
            .type_name = @bitCast(p.primary),
            .alias = @bitCast(p.secondary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_type_alias_redeclared => return CIR.Diagnostic{ .type_alias_redeclared = .{
            .name = @bitCast(p.primary),
            .redeclared_region = store.getRegionAt(node_idx),
            .original_region = .{
                .start = .{ .offset = @intCast(p.secondary) },
                .end = .{ .offset = @intCast(p.tertiary) },
            },
        } },
        .diag_nominal_type_redeclared => return CIR.Diagnostic{ .nominal_type_redeclared = .{
            .name = @bitCast(p.primary),
            .redeclared_region = store.getRegionAt(node_idx),
            .original_region = .{
                .start = .{ .offset = @intCast(p.secondary) },
                .end = .{ .offset = @intCast(p.tertiary) },
            },
        } },
        .diag_type_shadowed_warning => {
            const extra_data = store.extra_data.items.items[p.tertiary..];
            const original_start = extra_data[0];
            const original_end = extra_data[1];
            return CIR.Diagnostic{ .type_shadowed_warning = .{
                .name = @bitCast(p.primary),
                .region = store.getRegionAt(node_idx),
                .cross_scope = p.secondary != 0,
                .original_region = .{
                    .start = .{ .offset = original_start },
                    .end = .{ .offset = original_end },
                },
            } };
        },
        .diag_type_parameter_conflict => {
            const extra_data = store.extra_data.items.items[p.tertiary..];
            const original_start = extra_data[0];
            const original_end = extra_data[1];
            return CIR.Diagnostic{ .type_parameter_conflict = .{
                .name = @bitCast(p.primary),
                .parameter_name = @bitCast(p.secondary),
                .region = store.getRegionAt(node_idx),
                .original_region = .{
                    .start = .{ .offset = original_start },
                    .end = .{ .offset = original_end },
                },
            } };
        },
        .diag_unused_variable => return CIR.Diagnostic{ .unused_variable = .{
            .ident = @bitCast(p.primary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_used_underscore_variable => return CIR.Diagnostic{ .used_underscore_variable = .{
            .ident = @bitCast(p.primary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_duplicate_record_field => return CIR.Diagnostic{ .duplicate_record_field = .{
            .field_name = @bitCast(p.primary),
            .duplicate_region = store.getRegionAt(node_idx),
            .original_region = .{
                .start = .{ .offset = @intCast(p.secondary) },
                .end = .{ .offset = @intCast(p.tertiary) },
            },
        } },
        .diag_crash_expects_string => return CIR.Diagnostic{ .crash_expects_string = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_f64_pattern_literal => return CIR.Diagnostic{ .f64_pattern_literal = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_unused_type_var_name => return CIR.Diagnostic{ .unused_type_var_name = .{
            .name = @bitCast(p.primary),
            .suggested_name = @bitCast(p.secondary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_type_var_marked_unused => return CIR.Diagnostic{ .type_var_marked_unused = .{
            .name = @bitCast(p.primary),
            .suggested_name = @bitCast(p.secondary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_type_var_starting_with_dollar => return CIR.Diagnostic{ .type_var_starting_with_dollar = .{
            .name = @bitCast(p.primary),
            .suggested_name = @bitCast(p.secondary),
            .region = store.getRegionAt(node_idx),
        } },
        .diag_underscore_in_type_declaration => return CIR.Diagnostic{ .underscore_in_type_declaration = .{
            .is_alias = p.primary != 0,
            .region = store.getRegionAt(node_idx),
        } },
        .diag_break_outside_loop => return CIR.Diagnostic{ .break_outside_loop = .{
            .region = store.getRegionAt(node_idx),
        } },
        .diag_mutually_recursive_type_aliases => {
            const extra_data = store.extra_data.items.items[p.tertiary..];
            const other_start = extra_data[0];
            const other_end = extra_data[1];
            return CIR.Diagnostic{ .mutually_recursive_type_aliases = .{
                .name = @bitCast(p.primary),
                .other_name = @bitCast(p.secondary),
                .region = store.getRegionAt(node_idx),
                .other_region = .{
                    .start = .{ .offset = other_start },
                    .end = .{ .offset = other_end },
                },
            } };
        },
        .diag_deprecated_number_suffix => return CIR.Diagnostic{ .deprecated_number_suffix = .{
            .suffix = @enumFromInt(p.primary),
            .suggested = @enumFromInt(p.secondary),
            .region = store.getRegionAt(node_idx),
        } },
        else => {
            @panic("getDiagnostic called with non-diagnostic node - this indicates a compiler bug");
        },
    }
}

/// Computes the span of a diagnostic starting from a given index.
pub fn diagnosticSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Diagnostic.Span {
    return try store.spanFrom("diagnostics", CIR.Diagnostic.Span, start);
}

/// Ensure the node store has capacity for at least the requested number of
/// slots. Then return the *final* index.
pub fn predictNodeIndex(store: *NodeStore, count: u32) Allocator.Error!Node.Idx {
    const start_idx = store.nodes.len();
    try store.nodes.ensureTotalCapacity(store.gpa, start_idx + count);
    // Return where the LAST node will actually be placed
    return @enumFromInt(start_idx + count - 1);
}

/// Adds an type variable slot to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addTypeVarSlot(store: *NodeStore, parent_node_idx: Node.Idx, region: base.Region) Allocator.Error!Node.Idx {
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
pub fn fillInTypeVarSlotsThru(store: *NodeStore, target_idx: Node.Idx, parent_node_idx: Node.Idx, region: Region) Allocator.Error!void {
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
    return store.scratchTop("match_branches");
}

/// Add a match branch index to the scratch buffer.
pub fn addScratchMatchBranch(store: *NodeStore, branch_idx: CIR.Expr.Match.Branch.Idx) Allocator.Error!void {
    try store.addScratch("match_branches", branch_idx);
}

/// Create a span from the scratch match branches starting at the given index.
pub fn matchBranchSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Expr.Match.Branch.Span {
    return try store.spanFrom("match_branches", CIR.Expr.Match.Branch.Span, start);
}

/// Return the current top index for scratch match branch patterns.
pub fn scratchMatchBranchPatternTop(store: *NodeStore) u32 {
    return store.scratchTop("match_branch_patterns");
}

/// Add a match branch pattern index to the scratch buffer.
pub fn addScratchMatchBranchPattern(store: *NodeStore, pattern_idx: CIR.Expr.Match.BranchPattern.Idx) Allocator.Error!void {
    try store.addScratch("match_branch_patterns", pattern_idx);
}

/// Create a span from the scratch match branch patterns starting at the given index.
pub fn matchBranchPatternSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Expr.Match.BranchPattern.Span {
    return try store.spanFrom("match_branch_patterns", CIR.Expr.Match.BranchPattern.Span, start);
}

/// Serialized representation of NodeStore
/// Uses extern struct to guarantee consistent field layout across optimization levels.
pub const Serialized = extern struct {
    gpa: [2]u64, // Reserve enough space for 2 64-bit pointers
    nodes: Node.List.Serialized,
    regions: Region.List.Serialized,
    extra_data: collections.SafeList(u32).Serialized,
    scratch: u64, // Reserve enough space for a 64-bit pointer

    /// Serialize a NodeStore into this Serialized struct, appending data to the writer
    pub fn serialize(
        self: *Serialized,
        store: *const NodeStore,
        allocator: Allocator,
        writer: *CompactWriter,
    ) Allocator.Error!void {
        // Serialize nodes
        try self.nodes.serialize(&store.nodes, allocator, writer);
        // Serialize regions
        try self.regions.serialize(&store.regions, allocator, writer);
        // Serialize extra_data
        try self.extra_data.serialize(&store.extra_data, allocator, writer);
    }

    /// Deserialize this Serialized struct into a NodeStore
    /// The base_addr parameter is the base address of the serialized buffer in memory.
    pub fn deserialize(self: *Serialized, base_addr: usize, gpa: Allocator) *NodeStore {
        // Note: Serialized may be smaller than the runtime struct.
        // On 32-bit platforms, deserializing nodes in-place corrupts the adjacent
        // regions and extra_data fields. We must deserialize in REVERSE order (last to first)
        // so that each deserialization doesn't corrupt fields that haven't been deserialized yet.

        // Deserialize in reverse order: extra_data, regions, then nodes
        const deserialized_extra_data = self.extra_data.deserialize(base_addr).*;
        const deserialized_regions = self.regions.deserialize(base_addr).*;
        const deserialized_nodes = self.nodes.deserialize(base_addr).*;

        // Overwrite ourself with the deserialized version, and return our pointer after casting it to NodeStore
        const store = @as(*NodeStore, @ptrFromInt(@intFromPtr(self)));

        store.* = NodeStore{
            .gpa = gpa,
            .nodes = deserialized_nodes,
            .regions = deserialized_regions,
            .extra_data = deserialized_extra_data,
            .scratch = null, // A deserialized NodeStore is read-only, so it has no need for scratch memory!
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
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and deserialize
    const serialized_ptr: *NodeStore.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserialize(@intFromPtr(buffer.ptr), gpa);

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
    const node1_idx = try original.nodes.append(gpa, node1);

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
    const region1_idx = try original.regions.append(gpa, region);

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
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and deserialize
    const serialized_ptr: *NodeStore.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserialize(@intFromPtr(buffer.ptr), gpa);

    // Verify nodes
    try testing.expectEqual(@as(usize, 1), deserialized.nodes.len());
    const retrieved_node = deserialized.nodes.get(node1_idx);
    try testing.expectEqual(Node.Tag.expr_int, retrieved_node.tag);
    try testing.expectEqual(@as(u32, 0), retrieved_node.getPayload().raw.data_1);

    // Verify extra_data
    try testing.expectEqual(@as(usize, 4), deserialized.extra_data.len());
    const retrieved_u32s = deserialized.extra_data.items.items[0..4];
    const retrieved_bytes: [16]u8 = @bitCast(retrieved_u32s.*);
    const retrieved_value: i128 = @bitCast(retrieved_bytes);
    try testing.expectEqual(@as(i128, 42), retrieved_value);

    // Verify regions
    try testing.expectEqual(@as(usize, 1), deserialized.regions.len());
    const retrieved_region = deserialized.regions.get(region1_idx);
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
    const var_node_idx = try original.nodes.append(gpa, var_node);

    // Add expression list node
    const list_node = Node{
        .tag = .expr_list,
        .data_1 = 10, // elems start
        .data_2 = 3, // elems len
        .data_3 = 0,
    };
    const list_node_idx = try original.nodes.append(gpa, list_node);

    // Add float node with extra data
    const float_node = Node{
        .tag = .expr_frac_f64,
        .data_1 = 0, // extra_data index
        .data_2 = 0,
        .data_3 = 0,
    };
    const float_node_idx = try original.nodes.append(gpa, float_node);

    // Add float value to extra_data
    const float_value: f64 = 3.14159;
    const float_as_u64: u64 = @bitCast(float_value);
    const float_as_u32s: [2]u32 = @bitCast(float_as_u64);
    for (float_as_u32s) |u32_val| {
        _ = try original.extra_data.append(gpa, u32_val);
    }

    // Add regions for each node
    const region1 = Region{ .start = .{ .offset = 0 }, .end = .{ .offset = 5 } };
    const region2 = Region{ .start = .{ .offset = 10 }, .end = .{ .offset = 20 } };
    const region3 = Region{ .start = .{ .offset = 25 }, .end = .{ .offset = 32 } };
    const region1_idx = try original.regions.append(gpa, region1);
    const region2_idx = try original.regions.append(gpa, region2);
    const region3_idx = try original.regions.append(gpa, region3);

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
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and deserialize
    const serialized_ptr: *NodeStore.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserialize(@intFromPtr(buffer.ptr), gpa);

    // Verify nodes
    try testing.expectEqual(@as(usize, 3), deserialized.nodes.len());

    // Verify var node using captured index
    const retrieved_var = deserialized.nodes.get(var_node_idx);
    try testing.expectEqual(Node.Tag.expr_var, retrieved_var.tag);
    try testing.expectEqual(@as(u32, 5), retrieved_var.data_1);

    // Verify list node using captured index
    const retrieved_list = deserialized.nodes.get(list_node_idx);
    try testing.expectEqual(Node.Tag.expr_list, retrieved_list.tag);
    try testing.expectEqual(@as(u32, 10), retrieved_list.data_1);
    try testing.expectEqual(@as(u32, 3), retrieved_list.data_2);

    // Verify float node and extra data using captured index
    const retrieved_float = deserialized.nodes.get(float_node_idx);
    try testing.expectEqual(Node.Tag.expr_frac_f64, retrieved_float.tag);
    const retrieved_float_u32s = deserialized.extra_data.items.items[0..2];
    const retrieved_float_u64: u64 = @bitCast(retrieved_float_u32s.*);
    const retrieved_float_value: f64 = @bitCast(retrieved_float_u64);
    try testing.expectApproxEqAbs(float_value, retrieved_float_value, 0.0001);

    // Verify regions using captured indices
    try testing.expectEqual(@as(usize, 3), deserialized.regions.len());
    const retrieved_region1 = deserialized.regions.get(region1_idx);
    try testing.expectEqual(region1.start.offset, retrieved_region1.start.offset);
    try testing.expectEqual(region1.end.offset, retrieved_region1.end.offset);
    const retrieved_region2 = deserialized.regions.get(region2_idx);
    try testing.expectEqual(region2.start.offset, retrieved_region2.start.offset);
    try testing.expectEqual(region2.end.offset, retrieved_region2.end.offset);
    const retrieved_region3 = deserialized.regions.get(region3_idx);
    try testing.expectEqual(region3.start.offset, retrieved_region3.start.offset);
    try testing.expectEqual(region3.end.offset, retrieved_region3.end.offset);

    // Verify scratch is null (deserialized NodeStores don't allocate scratch)
    try testing.expect(deserialized.scratch == null);
}
