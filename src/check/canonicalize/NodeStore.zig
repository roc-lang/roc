//! Stores AST nodes and provides scratch arrays for working with nodes.

const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const collections = @import("../../collections.zig");
const Node = @import("Node.zig");
const CIR = @import("CIR.zig");
const RocDec = @import("../../builtins/dec.zig").RocDec;
const PackedDataSpan = @import("../../base/PackedDataSpan.zig");

const DataSpan = base.DataSpan;
const Region = base.Region;
const StringLiteral = base.StringLiteral;
const Diagnostic = @import("Diagnostic.zig");

const exitOnOom = collections.exitOnOom;

const NodeStore = @This();

gpa: std.mem.Allocator,
nodes: Node.List,
extra_data: std.ArrayListUnmanaged(u32),
scratch_statements: base.Scratch(CIR.Statement.Idx),
scratch_exprs: base.Scratch(CIR.Expr.Idx),
scratch_record_fields: base.Scratch(CIR.RecordField.Idx),
scratch_when_branches: base.Scratch(CIR.WhenBranch.Idx),
scratch_if_branches: base.Scratch(CIR.IfBranch.Idx),
scratch_where_clauses: base.Scratch(CIR.WhereClause.Idx),
scratch_patterns: base.Scratch(CIR.Pattern.Idx),
scratch_pattern_record_fields: base.Scratch(CIR.PatternRecordField.Idx),
scratch_record_destructs: base.Scratch(CIR.RecordDestruct.Idx),
scratch_type_annos: base.Scratch(CIR.TypeAnno.Idx),
scratch_anno_record_fields: base.Scratch(CIR.AnnoRecordField.Idx),
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
        .extra_data = std.ArrayListUnmanaged(u32).initCapacity(gpa, capacity / 2) catch |err| exitOnOom(err),
        .scratch_statements = base.Scratch(CIR.Statement.Idx).init(gpa),
        .scratch_exprs = base.Scratch(CIR.Expr.Idx).init(gpa),
        .scratch_patterns = base.Scratch(CIR.Pattern.Idx).init(gpa),
        .scratch_record_fields = base.Scratch(CIR.RecordField.Idx).init(gpa),
        .scratch_pattern_record_fields = base.Scratch(CIR.PatternRecordField.Idx).init(gpa),
        .scratch_record_destructs = base.Scratch(CIR.RecordDestruct.Idx).init(gpa),
        .scratch_when_branches = base.Scratch(CIR.WhenBranch.Idx).init(gpa),
        .scratch_if_branches = base.Scratch(CIR.IfBranch.Idx).init(gpa),
        .scratch_type_annos = base.Scratch(CIR.TypeAnno.Idx).init(gpa),
        .scratch_anno_record_fields = base.Scratch(CIR.AnnoRecordField.Idx).init(gpa),
        .scratch_exposed_items = base.Scratch(CIR.ExposedItem.Idx).init(gpa),
        .scratch_defs = base.Scratch(CIR.Def.Idx).init(gpa),
        .scratch_where_clauses = base.Scratch(CIR.WhereClause.Idx).init(gpa),
        .scratch_diagnostics = base.Scratch(CIR.Diagnostic.Idx).init(gpa),
    };
}

/// Deinitializes the NodeStore, freeing any allocated resources.
pub fn deinit(store: *NodeStore) void {
    store.nodes.deinit(store.gpa);
    store.extra_data.deinit(store.gpa);
    store.scratch_statements.items.deinit(store.gpa);
    store.scratch_exprs.items.deinit(store.gpa);
    store.scratch_patterns.items.deinit(store.gpa);
    store.scratch_record_fields.items.deinit(store.gpa);
    store.scratch_pattern_record_fields.items.deinit(store.gpa);
    store.scratch_record_destructs.items.deinit(store.gpa);
    store.scratch_when_branches.items.deinit(store.gpa);
    store.scratch_if_branches.items.deinit(store.gpa);
    store.scratch_type_annos.items.deinit(store.gpa);
    store.scratch_anno_record_fields.items.deinit(store.gpa);
    store.scratch_exposed_items.items.deinit(store.gpa);
    store.scratch_defs.items.deinit(store.gpa);
    store.scratch_where_clauses.items.deinit(store.gpa);
    store.scratch_diagnostics.items.deinit(store.gpa);
}

/// Retrieves a region from node from the store.
pub fn getNodeRegion(store: *const NodeStore, node_idx: Node.Idx) Region {
    const node = store.nodes.get(node_idx);
    return node.region;
}

/// Retrieves a statement node from the store.
pub fn getStatement(store: *NodeStore, statement: CIR.Statement.Idx) CIR.Statement {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(statement));
    const node = store.nodes.get(node_idx);

    switch (node.tag) {
        .statement_decl => return CIR.Statement{ .decl = .{
            .region = node.region,
            .expr = @enumFromInt(node.data_1),
            .pattern = @enumFromInt(node.data_2),
        } },
        .statement_var => return CIR.Statement{ .@"var" = .{
            .region = node.region,
            .expr = @enumFromInt(node.data_1),
            .pattern_idx = @enumFromInt(node.data_2),
        } },
        .statement_reassign => return CIR.Statement{ .reassign = .{
            .region = node.region,
            .expr = @enumFromInt(node.data_1),
            .pattern_idx = @enumFromInt(node.data_2),
        } },
        .statement_crash => return CIR.Statement{ .crash = .{
            .msg = @enumFromInt(node.data_1),
            .region = node.region,
        } },
        .statement_expr => return .{ .expr = .{
            .expr = @enumFromInt(node.data_1),
            .region = node.region,
        } },
        .statement_expect => return CIR.Statement{ .expect = .{
            .region = node.region,
            .body = @enumFromInt(node.data_1),
        } },
        .statement_for => return CIR.Statement{ .@"for" = .{
            .region = node.region,
            .body = @enumFromInt(node.data_1),
            .expr = @enumFromInt(node.data_2),
            .patt = @enumFromInt(node.data_3),
        } },
        .statement_return => return CIR.Statement{ .@"return" = .{
            .region = node.region,
            .expr = @enumFromInt(node.data_1),
        } },
        .statement_import => return CIR.Statement{
            .import = .{
                .region = node.region,
                .module_name_tok = @bitCast(node.data_1),
                .exposes = DataSpan.init(node.data_2, node.data_3).as(CIR.ExposedItem.Span),
                .alias_tok = null, // TODO save these in extra_data and then insert them here
                .qualifier_tok = null, // TODO save these in extra_data and then insert them here
            },
        },
        .statement_type_decl => {
            const extra_start = node.data_1;
            const extra_data = store.extra_data.items[extra_start..];

            const anno: CIR.TypeAnno.Idx = @enumFromInt(extra_data[0]);
            const header: CIR.TypeHeader.Idx = @enumFromInt(extra_data[1]);

            return CIR.Statement{
                .type_decl = .{
                    .region = node.region,
                    .anno = anno,
                    .header = header,
                    .where = null, // Where clauses not implemented yet
                },
            };
        },
        .statement_type_anno => return CIR.Statement{
            .type_anno = .{
                .region = node.region,
                .name = @bitCast(node.data_2),
                .anno = @enumFromInt(node.data_1),
                .where = null, // TODO save these in extra_data and then insert them here
            },
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
            return CIR.Expr{ .lookup = .{ .local = .{
                .pattern_idx = @enumFromInt(node.data_1),
                .region = node.region,
            } } };
        },
        .expr_external_lookup => {
            // Handle external lookups
            return CIR.Expr{ .lookup = .{ .external = @enumFromInt(node.data_1) } };
        },
        .expr_int => {
            // Read i128 from extra_data (stored as 4 u32s in data_1)
            const value_as_u32s = store.extra_data.items[node.data_1..][0..4];

            // Retrieve type variable from data_2 and requirements from data_3
            const int_var = @as(types.Var, @enumFromInt(node.data_2));

            return .{
                .int = .{
                    .num_var = int_var,
                    .value = .{ .bytes = @bitCast(value_as_u32s.*), .kind = .i128 },
                    .region = node.region,
                },
            };
        },
        .expr_list => {
            return .{
                .list = .{
                    .elems = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
                    .elem_var = @enumFromInt(node.data_3),
                    .region = node.region,
                },
            };
        },
        .expr_tuple => {
            return .{
                .tuple = .{
                    .elems = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
                    .region = node.region,
                },
            };
        },
        .expr_call => {
            return .{
                .call = .{
                    .args = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
                    .region = node.region,
                    .called_via = @enumFromInt(node.data_3),
                },
            };
        },
        .expr_frac_f64 => {
            // Retrieve type variable from data_2
            const frac_var = @as(types.Var, @enumFromInt(node.data_2));

            // Get value from extra_data
            const extra_data_idx = node.data_1;
            const value_as_u32s = store.extra_data.items[extra_data_idx..][0..2];
            const value_as_u64: u64 = @bitCast(value_as_u32s.*);
            const value: f64 = @bitCast(value_as_u64);

            return CIR.Expr{
                .frac_f64 = .{
                    .frac_var = frac_var,
                    .value = value,
                    .region = node.region,
                },
            };
        },
        .expr_frac_dec => {
            // Retrieve type variable from data_2
            const frac_var = @as(types.Var, @enumFromInt(node.data_2));

            // Get value from extra_data
            const extra_data_idx = node.data_1;
            const value_as_u32s = store.extra_data.items[extra_data_idx..][0..4];
            const value_as_i128: i128 = @bitCast(value_as_u32s.*);

            return CIR.Expr{
                .frac_dec = .{
                    .frac_var = frac_var,
                    .value = RocDec{ .num = value_as_i128 },
                    .region = node.region,
                },
            };
        },
        .expr_dec_small => {
            // Retrieve type variable from data_2
            const num_var = @as(types.Var, @enumFromInt(node.data_2));

            // Unpack small dec data from data_1 and data_3
            // data_1: numerator (i16) stored as u32
            // data_3: denominator_power_of_ten (u8) in lower 8 bits
            const numerator = @as(i16, @intCast(@as(i32, @bitCast(node.data_1))));
            const denominator_power_of_ten = @as(u8, @truncate(node.data_3));

            return CIR.Expr{
                .dec_small = .{
                    .num_var = num_var,
                    .numerator = numerator,
                    .denominator_power_of_ten = denominator_power_of_ten,
                    .region = node.region,
                },
            };
        },
        .expr_string_segment => return CIR.Expr.initStrSegment(
            @enumFromInt(node.data_1),
            node.region,
        ),
        .expr_string => return CIR.Expr.initStr(
            DataSpan.init(node.data_1, node.data_2).as(CIR.Expr.Span),
            node.region,
        ),
        .expr_tag => {
            return .{
                .tag = .{
                    .ext_var = @enumFromInt(0), // Placeholder
                    .name = @bitCast(@as(base.Ident.Idx, @bitCast(node.data_1))),
                    .args = .{ .span = .{ .start = 0, .len = 0 } }, // Empty args for now
                    .region = node.region,
                },
            };
        },
        .expr_bin_op => {
            return CIR.Expr{ .binop = CIR.Expr.Binop.init(
                @enumFromInt(node.data_1),
                @enumFromInt(node.data_2),
                @enumFromInt(node.data_3),
                node.region,
            ) };
        },
        .expr_lambda => {
            return CIR.Expr{ .lambda = .{
                .args = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
                .body = @enumFromInt(node.data_3),
                .region = node.region,
            } };
        },
        .expr_block => {
            return CIR.Expr{ .block = .{
                .stmts = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
                .final_expr = @enumFromInt(node.data_3),
                .region = node.region,
            } };
        },
        .expr_empty_record => {
            return CIR.Expr{ .empty_record = .{
                .region = node.region,
            } };
        },
        .expr_record => {
            return CIR.Expr{
                .record = .{
                    .ext_var = @enumFromInt(node.data_1),
                    .fields = .{ .span = .{ .start = node.data_2, .len = node.data_3 } },
                    .region = node.region,
                },
            };
        },
        .expr_field_access,
        .expr_static_dispatch,
        .expr_apply,
        .expr_record_update,
        .expr_unary,
        .expr_suffix_single_question,
        .expr_match,
        .expr_dbg,
        .expr_ellipsis,
        .expr_record_builder,
        => {
            std.log.debug("TODO: implement getExpr for node type {?}", .{node.tag});
            return CIR.Expr{ .runtime_error = .{
                .diagnostic = @enumFromInt(0),
                .region = node.region,
            } };
        },
        .expr_if_then_else => {
            // Reconstruct the if expression from node data
            const branches_span = CIR.IfBranch.Span{ .span = .{
                .start = node.data_1,
                .len = node.data_2,
            } };
            const final_else = @as(CIR.Expr.Idx, @enumFromInt(node.data_3));

            // Type variables are stored in extra_data, but for now we'll create fresh ones
            // TODO: Properly restore type variables from extra_data
            const cond_var = @as(types.Var, @enumFromInt(0));
            const branch_var = @as(types.Var, @enumFromInt(0));

            return CIR.Expr{ .@"if" = .{
                .cond_var = cond_var,
                .branch_var = branch_var,
                .branches = branches_span,
                .final_else = final_else,
                .region = node.region,
            } };
        },
        .expr_dot_access => {
            const args_span = if (node.data_3 != 0) blk: {
                const packed_span = PackedDataSpan.FunctionArgs.fromU32(node.data_3);
                const data_span = packed_span.toDataSpan();
                break :blk CIR.Expr.Span{ .span = data_span };
            } else null;

            return CIR.Expr{ .dot_access = .{
                .receiver = @enumFromInt(node.data_1),
                .field_name = @bitCast(node.data_2),
                .args = args_span,
                .region = node.region,
            } };
        },
        .malformed => {
            return CIR.Expr{ .runtime_error = .{
                .diagnostic = @enumFromInt(node.data_1),
                .region = node.region,
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

/// Retrieves a 'when' branch from the store.
pub fn getWhenBranch(store: *const NodeStore, whenBranch: CIR.WhenBranch.Idx) CIR.WhenBranch {
    _ = store;
    _ = whenBranch;
    @panic("TODO: implement getWhenBranch");
}

/// Retrieves a 'where' clause from the store.
pub fn getWhereClause(store: *NodeStore, whereClause: CIR.WhereClause.Idx) CIR.WhereClause {
    _ = store;
    _ = whereClause;
    @panic("TODO: implement getWhereClause");
}

/// Retrieves a pattern from the store.
pub fn getPattern(store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) CIR.Pattern {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));
    const node = store.nodes.get(node_idx);

    switch (node.tag) {
        .pattern_identifier => return CIR.Pattern{
            .assign = .{
                .ident = @bitCast(node.data_1),
                .region = node.region,
            },
        },
        .pattern_as => return CIR.Pattern{
            .as = .{
                .ident = @bitCast(node.data_1),
                .pattern = @enumFromInt(node.data_2),
                .region = node.region,
            },
        },
        .pattern_applied_tag => return CIR.Pattern{
            .applied_tag = .{
                .region = node.region,
                .arguments = DataSpan.init(node.data_1, node.data_2).as(CIR.Pattern.Span),
                .tag_name = @bitCast(node.data_3),

                .ext_var = @enumFromInt(0), // TODO need to store and retrieve from extra_data
            },
        },
        .pattern_record_destructure => return CIR.Pattern{
            .record_destructure = .{
                .region = node.region,
                .destructs = DataSpan.init(node.data_1, node.data_2).as(CIR.RecordDestruct.Span),
                .ext_var = @enumFromInt(0), // TODO need to store and retrieve from extra_data
                .whole_var = @enumFromInt(0), // TODO need to store and retrieve from extra_data
            },
        },
        .pattern_list => return CIR.Pattern{
            .list = .{
                .region = node.region,
                .patterns = DataSpan.init(node.data_1, node.data_2).as(CIR.Pattern.Span),
                .elem_var = @enumFromInt(0), // TODO need to store elem_var separately
                .list_var = @enumFromInt(node.data_3),
            },
        },
        .pattern_tuple => return CIR.Pattern{
            .tuple = .{
                .region = node.region,
                .patterns = DataSpan.init(node.data_1, node.data_2).as(CIR.Pattern.Span),
            },
        },
        .pattern_num_literal => {
            // Retrieve value from extra_data
            const extra_data_idx = node.data_1;
            const value_as_u32s = store.extra_data.items[extra_data_idx..][0..4];
            const value_as_i128: i128 = @bitCast(value_as_u32s.*);

            return CIR.Pattern{
                .int_literal = .{
                    .region = node.region,
                    .num_var = @as(types.Var, @enumFromInt(node.data_2)),
                    .value = .{ .bytes = @bitCast(value_as_i128), .kind = .i128 },
                },
            };
        },
        .pattern_int_literal => return CIR.Pattern{
            .int_literal = .{
                .region = node.region,
                .num_var = @enumFromInt(0), // TODO need to store and retrieve from extra_data
                .value = .{ .bytes = @bitCast(@as(i128, 0)), .kind = .i128 }, // TODO need to store and retrieve from extra_data
            },
        },
        .pattern_dec_literal => return CIR.Pattern{
            .dec_literal = .{
                .region = node.region,
                .num_var = @enumFromInt(0), // TODO need to store and retrieve from extra_data
                .value = RocDec{ .num = 0 }, // TODO need to store and retrieve from extra_data
            },
        },
        .pattern_f64_literal => return CIR.Pattern{
            .f64_literal = .{
                .region = node.region,
                .num_var = @enumFromInt(0), // TODO need to store and retrieve from extra_data
                .value = 0.0, // TODO need to store and retrieve from extra_data
            },
        },
        .pattern_str_literal => return CIR.Pattern{ .str_literal = .{
            .region = node.region,
            .literal = @enumFromInt(node.data_1),
        } },
        .pattern_char_literal => return CIR.Pattern{
            .char_literal = .{
                .region = node.region,
                .num_var = @enumFromInt(0), // TODO need to store and retrieve from extra_data
                .requirements = .{ .sign_needed = false, .bits_needed = .@"32" }, // TODO need to store and retrieve from extra_data
                .value = node.data_1,
            },
        },
        .pattern_underscore => return CIR.Pattern{ .underscore = .{
            .region = node.region,
        } },
        .malformed => {
            return CIR.Pattern{ .runtime_error = .{
                .diagnostic = @enumFromInt(node.data_1),
                .region = node.region,
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
    @panic("TODO: implement getPatternRecordField");
}

/// Retrieves a type annotation from the store.
pub fn getTypeAnno(store: *const NodeStore, typeAnno: CIR.TypeAnno.Idx) CIR.TypeAnno {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(typeAnno));
    const node = store.nodes.get(node_idx);

    switch (node.tag) {
        .ty_apply => return CIR.TypeAnno{ .apply = .{
            .symbol = @bitCast(node.data_1),
            .args = .{ .span = .{ .start = node.data_2, .len = node.data_3 } },
            .region = node.region,
        } },
        .ty_var => return CIR.TypeAnno{ .ty_var = .{
            .name = @bitCast(node.data_1),
            .region = node.region,
        } },
        .ty_underscore => return CIR.TypeAnno{ .underscore = .{
            .region = node.region,
        } },
        .ty_ident => return CIR.TypeAnno{ .ty = .{
            .symbol = @bitCast(node.data_1),
            .region = node.region,
        } },
        .ty_mod => return CIR.TypeAnno{ .mod_ty = .{
            .mod_symbol = @bitCast(node.data_1),
            .ty_symbol = @bitCast(node.data_2),
            .region = node.region,
        } },
        .ty_tag_union => return CIR.TypeAnno{ .tag_union = .{
            .tags = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
            .open_anno = if (node.data_3 != 0) @enumFromInt(node.data_3) else null,
            .region = node.region,
        } },
        .ty_tuple => return CIR.TypeAnno{ .tuple = .{
            .annos = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
            .region = node.region,
        } },
        .ty_record => return CIR.TypeAnno{ .record = .{
            .fields = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
            .region = node.region,
        } },
        .ty_fn => {
            const ret_and_effectful = node.data_3;
            const ret: CIR.TypeAnno.Idx = @enumFromInt(ret_and_effectful & 0x7FFFFFFF);
            const effectful = (ret_and_effectful & (1 << 31)) != 0;
            return CIR.TypeAnno{ .@"fn" = .{
                .args = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
                .ret = ret,
                .effectful = effectful,
                .region = node.region,
            } };
        },
        .ty_parens => return CIR.TypeAnno{ .parens = .{
            .anno = @enumFromInt(node.data_1),
            .region = node.region,
        } },
        .malformed => return CIR.TypeAnno{ .malformed = .{
            .diagnostic = @enumFromInt(node.data_1),
            .region = node.region,
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
        .region = node.region,
    };
}

/// Retrieves an annotation record field from the store.
pub fn getAnnoRecordField(store: *const NodeStore, annoRecordField: CIR.AnnoRecordField.Idx) CIR.AnnoRecordField {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(annoRecordField)));
    return .{
        .name = @bitCast(node.data_1),
        .ty = @enumFromInt(node.data_2),
        .region = node.region,
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
        .region = node.region,
    };
}

/// Retrieves an exposed item from the store.
pub fn getExposedItem(store: *NodeStore, exposedItem: CIR.ExposedItem.Idx) CIR.ExposedItem {
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

/// Adds a statement node to the store.
pub fn addStatement(store: *NodeStore, statement: CIR.Statement) CIR.Statement.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .region = base.Region.zero(),
        .tag = @enumFromInt(0),
    };

    switch (statement) {
        .decl => |s| {
            node.tag = .statement_decl;
            node.region = s.region;
            node.data_1 = @intFromEnum(s.expr);
            node.data_2 = @intFromEnum(s.pattern);
        },
        .@"var" => |s| {
            node.tag = .statement_var;
            node.region = s.region;
            node.data_1 = @intFromEnum(s.expr);
            node.data_2 = @intFromEnum(s.pattern_idx);
        },
        .reassign => |s| {
            node.tag = .statement_reassign;
            node.region = s.region;
            node.data_1 = @intFromEnum(s.expr);
            node.data_2 = @intFromEnum(s.pattern_idx);
        },
        .crash => |s| {
            node.tag = .statement_crash;
            node.region = s.region;
            node.data_1 = @intFromEnum(s.msg);
        },
        .expr => |s| {
            node.tag = .statement_expr;
            node.data_1 = @intFromEnum(s.expr);
            node.region = s.region;
        },
        .expect => |s| {
            node.tag = .statement_expect;
            node.region = s.region;
            node.data_1 = @intFromEnum(s.body);
        },
        .@"for" => |s| {
            node.tag = .statement_for;
            node.region = s.region;
            node.data_1 = @intFromEnum(s.body);
            node.data_2 = @intFromEnum(s.expr);
            node.data_3 = @intFromEnum(s.patt);
        },
        .@"return" => |s| {
            node.tag = .statement_return;
            node.region = s.region;
            node.data_1 = @intFromEnum(s.expr);
        },
        .import => |s| {
            node.tag = .statement_import;
            node.region = s.region;
            node.data_1 = @bitCast(s.module_name_tok);
            node.data_2 = s.exposes.span.start;
            node.data_3 = s.exposes.span.len;
            // TODO store alias_tok and qualifier_tok in extra_data
        },
        .type_decl => |s| {
            node.tag = .statement_type_decl;
            node.region = s.region;

            // Store type_decl data in extra_data
            const extra_start = @as(u32, @intCast(store.extra_data.items.len));

            // Store anno idx
            store.extra_data.append(store.gpa, @intFromEnum(s.anno)) catch |err| exitOnOom(err);
            // Store header idx
            store.extra_data.append(store.gpa, @intFromEnum(s.header)) catch |err| exitOnOom(err);
            // Where clauses not implemented yet, so we don't store them

            // Store the extra data start position in the node
            node.data_1 = extra_start;
        },
        .type_anno => |s| {
            node.tag = .statement_type_anno;
            node.region = s.region;
            node.data_1 = @intFromEnum(s.anno);
            node.data_2 = @bitCast(s.name);
            // TODO store the optional where clause data in extra_data
        },
    }

    return @enumFromInt(@intFromEnum(store.nodes.append(store.gpa, node)));
}

/// Adds an expression node to the store.
pub fn addExpr(store: *NodeStore, expr: CIR.Expr) CIR.Expr.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .region = base.Region.zero(),
        .tag = @enumFromInt(0),
    };

    switch (expr) {
        .lookup => |e| {
            switch (e) {
                .local => |local| {
                    node.region = local.region;
                    node.tag = .expr_var;
                    node.data_1 = @intFromEnum(local.pattern_idx);
                },
                .external => |external_idx| {
                    // For external lookups, store the external decl index
                    // Use external lookup tag to distinguish from local lookups
                    node.region = base.Region.zero();
                    node.tag = .expr_external_lookup;
                    node.data_1 = @intFromEnum(external_idx);
                },
            }
        },
        .int => |e| {
            node.region = e.region;
            node.tag = .expr_int;

            // Store type variable in data_2
            node.data_2 = @intFromEnum(e.num_var);

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
        .list => |e| {
            node.region = e.region;
            node.tag = .expr_list;
            node.data_1 = e.elems.span.start;
            node.data_2 = e.elems.span.len;
            node.data_3 = @intFromEnum(e.elem_var);
        },
        .tuple => |e| {
            node.region = e.region;
            node.tag = .expr_tuple;
            node.data_1 = e.elems.span.start;
            node.data_2 = e.elems.span.len;
        },
        .frac_f64 => |e| {
            node.region = e.region;
            node.tag = .expr_frac_f64;

            // Store type variable in data_2
            node.data_2 = @intFromEnum(e.frac_var);

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
        .frac_dec => |e| {
            node.region = e.region;
            node.tag = .expr_frac_dec;

            // Store type variable in data_2
            node.data_2 = @intFromEnum(e.frac_var);

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
        .dec_small => |e| {
            node.region = e.region;
            node.tag = .expr_dec_small;

            // Store type variable in data_2
            node.data_2 = @intFromEnum(e.num_var);

            // Pack small dec data into data_1 and data_3
            // data_1: numerator (i16) - fits in lower 16 bits
            // data_3: denominator_power_of_ten (u8) in lower 8 bits
            node.data_1 = @as(u32, @bitCast(@as(i32, e.numerator)));
            node.data_3 = @as(u32, e.denominator_power_of_ten);
        },
        .str_segment => |e| {
            node.region = e.region;
            node.tag = .expr_string_segment;
            node.data_1 = @intFromEnum(e.literal);
        },
        .str => |e| {
            node.region = e.region;
            node.tag = .expr_string;
            node.data_1 = e.span.span.start;
            node.data_2 = e.span.span.len;
        },
        .tag => |e| {
            node.region = e.region;
            node.tag = .expr_tag;
            // Store the full Ident.Idx as a u32
            node.data_1 = @bitCast(@as(u32, @bitCast(e.name)));
        },
        .dot_access => |e| {
            node.region = e.region;
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
        .runtime_error => |e| {
            node.region = e.region;
            node.data_1 = @intFromEnum(e.diagnostic);
            node.tag = .malformed;
        },
        .num => |e| {
            node.region = e.region;
            @panic("TODO addExpr num");
        },

        .single_quote => |e| {
            node.region = e.region;
            @panic("TODO addExpr single_quote");
        },
        .when => |e| {
            node.region = e.region;
            @panic("TODO addExpr when");
        },
        .@"if" => |e| {
            node.region = e.region;
            node.tag = .expr_if_then_else;
            // Store branches span start and len
            node.data_1 = e.branches.span.start;
            node.data_2 = e.branches.span.len;
            // Store final_else expression index
            node.data_3 = @intFromEnum(e.final_else);
        },
        .call => |e| {
            node.region = e.region;
            node.tag = .expr_call;
            // Store the args span
            node.data_1 = e.args.span.start;
            node.data_2 = e.args.span.len;
            node.data_3 = @intFromEnum(e.called_via);
        },
        .record => |e| {
            node.region = e.region;
            node.tag = .expr_record;
            node.data_1 = @intFromEnum(e.ext_var);
            if (e.fields.span.len > 0) {
                node.data_2 = e.fields.span.start;
                node.data_3 = e.fields.span.len;
            }
        },
        .empty_record => |e| {
            node.region = e.region;
            node.tag = .expr_empty_record;
        },
        .record_access => |e| {
            node.region = e.region;
            @panic("TODO addExpr record_access");
        },
        .zero_argument_tag => |e| {
            node.region = e.region;
            @panic("TODO addExpr zero_argument_tag");
        },
        .lambda => |e| {
            node.region = e.region;
            node.tag = .expr_lambda;
            node.data_1 = e.args.span.start;
            node.data_2 = e.args.span.len;
            node.data_3 = @intFromEnum(e.body);
        },
        .binop => |e| {
            node.region = e.region;
            node.tag = .expr_bin_op;
            node.data_1 = @intFromEnum(e.op);
            node.data_2 = @intFromEnum(e.lhs);
            node.data_3 = @intFromEnum(e.rhs);
        },
        .block => |e| {
            node.region = e.region;
            node.tag = .expr_block;
            node.data_1 = e.stmts.span.start;
            node.data_2 = e.stmts.span.len;
            node.data_3 = @intFromEnum(e.final_expr);
        },
    }

    return @enumFromInt(@intFromEnum(store.nodes.append(store.gpa, node)));
}

/// Adds a record field to the store.
pub fn addRecordField(store: *NodeStore, recordField: CIR.RecordField) CIR.RecordField.Idx {
    const node = Node{
        .data_1 = @bitCast(recordField.name),
        .data_2 = @intFromEnum(recordField.value),
        .data_3 = 0,
        .region = base.Region.zero(),
        .tag = .record_field,
    };

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a record destructuring to the store.
pub fn addRecordDestruct(store: *NodeStore, recordDestruct: CIR.RecordDestruct) CIR.RecordDestruct.Idx {
    const node = Node{
        .data_1 = @bitCast(recordDestruct.label),
        .data_2 = @bitCast(recordDestruct.ident),
        .data_3 = 0,
        .region = recordDestruct.region,
        .tag = .record_destruct,
    };

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a 'when' branch to the store.
pub fn addWhenBranch(store: *NodeStore, whenBranch: CIR.WhenBranch) CIR.WhenBranch.Idx {
    _ = store;
    _ = whenBranch;

    return .{ .id = @enumFromInt(0) };
}

/// Adds a 'where' clause to the store.
pub fn addWhereClause(store: *NodeStore, whereClause: CIR.WhereClause) CIR.WhereClause.Idx {
    _ = store;
    _ = whereClause;

    return .{ .id = @enumFromInt(0) };
}

/// Adds a pattern to the store.
pub fn addPattern(store: *NodeStore, pattern: CIR.Pattern) CIR.Pattern.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .region = base.Region.zero(),
        .tag = @enumFromInt(0),
    };

    switch (pattern) {
        .assign => |p| {
            node.data_1 = @bitCast(p.ident);
            node.tag = .pattern_identifier;
            node.region = p.region;
        },
        .as => |p| {
            node.tag = .pattern_as;
            node.region = p.region;
            node.data_1 = @bitCast(p.ident);
            node.data_2 = @intFromEnum(p.pattern);
        },
        .applied_tag => |p| {
            node.tag = .pattern_applied_tag;
            node.region = p.region;
            node.data_1 = p.arguments.span.start;
            node.data_2 = p.arguments.span.len;
            node.data_3 = @bitCast(p.tag_name);
            // TODO store type vars in extra data
        },
        .record_destructure => |p| {
            node.tag = .pattern_record_destructure;
            node.region = p.region;
            node.data_1 = p.destructs.span.start;
            node.data_2 = p.destructs.span.len;
            // TODO store type vars in extra data
        },
        .list => |p| {
            node.tag = .pattern_list;
            node.region = p.region;
            node.data_1 = p.patterns.span.start;
            node.data_2 = p.patterns.span.len;
            node.data_3 = @intFromEnum(p.list_var);
        },
        .tuple => |p| {
            node.tag = .pattern_tuple;
            node.region = p.region;
            node.data_1 = p.patterns.span.start;
            node.data_2 = p.patterns.span.len;
        },
        .int_literal => |p| {
            node.tag = .pattern_int_literal;
            node.region = p.region;
            // Store the value in extra_data
            const extra_data_start = store.extra_data.items.len;
            const value_as_u32s: [4]u32 = @bitCast(p.value.bytes);
            for (value_as_u32s) |word| {
                store.extra_data.append(store.gpa, word) catch |err| exitOnOom(err);
            }
            node.data_1 = @intCast(extra_data_start);
            // TODO store num_var
        },
        .small_dec_literal => |p| {
            node.tag = .pattern_small_dec_literal;
            node.region = p.region;
            // TODO store num_var, numerator, denominator_power_of_ten
        },
        .dec_literal => |p| {
            node.tag = .pattern_dec_literal;
            node.region = p.region;
            // TODO store num_var and value
        },
        .f64_literal => |p| {
            node.tag = .pattern_f64_literal;
            node.region = p.region;
            // TODO store num_var and value
        },

        .str_literal => |p| {
            node.tag = .pattern_str_literal;
            node.region = p.region;
            node.data_1 = @intFromEnum(p.literal);
            // TODO store other data
        },
        .char_literal => |p| {
            node.tag = .pattern_char_literal;
            node.region = p.region;
            node.data_1 = p.value;
            // TODO store other data
        },
        .underscore => |p| {
            node.tag = .pattern_underscore;
            node.region = p.region;
        },
        .runtime_error => |e| {
            node.tag = .malformed;
            node.region = e.region;
            node.data_1 = @intFromEnum(e.diagnostic);
        },
    }

    return @enumFromInt(@intFromEnum(store.nodes.append(store.gpa, node)));
}

/// Adds a pattern record field to the store.
pub fn addPatternRecordField(store: *NodeStore, patternRecordField: CIR.PatternRecordField) CIR.PatternRecordField.Idx {
    _ = store;
    _ = patternRecordField;

    return .{ .id = @enumFromInt(0) };
}

/// Adds a type annotation to the store.
pub fn addTypeAnno(store: *NodeStore, typeAnno: CIR.TypeAnno) CIR.TypeAnno.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .region = base.Region.zero(),
        .tag = @enumFromInt(0),
    };

    switch (typeAnno) {
        .apply => |a| {
            node.region = a.region;
            node.data_1 = @bitCast(a.symbol);
            node.data_2 = a.args.span.start;
            node.data_3 = a.args.span.len;
            node.tag = .ty_apply;
        },
        .ty_var => |tv| {
            node.region = tv.region;
            node.data_1 = @bitCast(tv.name);
            node.tag = .ty_var;
        },
        .underscore => |u| {
            node.region = u.region;
            node.tag = .ty_underscore;
        },
        .ty => |t| {
            node.region = t.region;
            node.data_1 = @bitCast(t.symbol);
            node.tag = .ty_ident;
        },
        .mod_ty => |mt| {
            node.region = mt.region;
            node.data_1 = @bitCast(mt.mod_symbol);
            node.data_2 = @bitCast(mt.ty_symbol);
            node.tag = .ty_mod;
        },
        .tag_union => |tu| {
            node.region = tu.region;
            node.data_1 = tu.tags.span.start;
            node.data_2 = tu.tags.span.len;
            node.data_3 = if (tu.open_anno) |open| @intFromEnum(open) else 0;
            node.tag = .ty_tag_union;
        },
        .tuple => |t| {
            node.region = t.region;
            node.data_1 = t.annos.span.start;
            node.data_2 = t.annos.span.len;
            node.tag = .ty_tuple;
        },
        .record => |r| {
            node.region = r.region;
            node.data_1 = r.fields.span.start;
            node.data_2 = r.fields.span.len;
            node.tag = .ty_record;
        },
        .@"fn" => |f| {
            node.region = f.region;
            node.data_1 = f.args.span.start;
            node.data_2 = f.args.span.len;
            node.data_3 = @intFromEnum(f.ret) | (if (f.effectful) @as(u32, 1) << 31 else 0);
            node.tag = .ty_fn;
        },
        .parens => |p| {
            node.region = p.region;
            node.data_1 = @intFromEnum(p.anno);
            node.tag = .ty_parens;
        },
        .malformed => |m| {
            node.region = m.region;
            node.data_1 = @intFromEnum(m.diagnostic);
            node.tag = .ty_malformed;
        },
    }

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a type header to the store.
pub fn addTypeHeader(store: *NodeStore, typeHeader: CIR.TypeHeader) CIR.TypeHeader.Idx {
    const node = Node{
        .data_1 = @bitCast(typeHeader.name),
        .data_2 = typeHeader.args.span.start,
        .data_3 = typeHeader.args.span.len,
        .region = typeHeader.region,
        .tag = .type_header,
    };

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an annotation record field to the store.
pub fn addAnnoRecordField(store: *NodeStore, annoRecordField: CIR.AnnoRecordField) CIR.AnnoRecordField.Idx {
    const node = Node{
        .data_1 = @bitCast(annoRecordField.name),
        .data_2 = @intFromEnum(annoRecordField.ty),
        .data_3 = 0,
        .region = annoRecordField.region,
        .tag = .ty_record_field,
    };

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an annotation to the store.
pub fn addAnnotation(store: *NodeStore, annotation: CIR.Annotation) CIR.Annotation.Idx {
    const node = Node{
        .data_1 = @intFromEnum(annotation.signature),
        .data_2 = @intFromEnum(annotation.type_anno),
        .data_3 = 0,
        .region = annotation.region,
        .tag = .annotation,
    };

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an exposed item to the store.
pub fn addExposedItem(store: *NodeStore, exposedItem: CIR.ExposedItem) CIR.ExposedItem.Idx {
    const node = Node{
        .data_1 = @bitCast(exposedItem.name),
        .data_2 = if (exposedItem.alias) |alias| @bitCast(alias) else 0,
        .data_3 = @intFromBool(exposedItem.is_wildcard),
        .region = base.Region.zero(),
        .tag = .exposed_item,
    };

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a definition to the store.
pub fn addDef(store: *NodeStore, def: CIR.Def) CIR.Def.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .region = def.pattern_region, // Use pattern region as the def's region
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
    // Store expr_region start and end
    store.extra_data.append(store.gpa, def.expr_region.start.offset) catch |err| exitOnOom(err);
    store.extra_data.append(store.gpa, def.expr_region.end.offset) catch |err| exitOnOom(err);

    // Store the extra data range in the node
    node.data_1 = extra_start;
    node.data_2 = 7; // Number of extra data items

    return @enumFromInt(@intFromEnum(store.nodes.append(store.gpa, node)));
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
    const kind_encoded = .{ extra_data[2], extra_data[3] };
    const kind = CIR.Def.Kind.decode(kind_encoded);
    const anno_idx = extra_data[4];
    const expr_region_start = extra_data[5];
    const expr_region_end = extra_data[6];

    const annotation: ?CIR.Annotation.Idx = if (anno_idx == 0) null else @enumFromInt(anno_idx);

    return CIR.Def{
        .pattern = pattern,
        .pattern_region = node.region, // Stored as node region
        .expr = expr,
        .expr_region = base.Region{
            .start = .{ .offset = expr_region_start },
            .end = .{ .offset = expr_region_end },
        },
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
pub fn getRecordDestruct(store: *const NodeStore, idx: CIR.RecordDestruct.Idx) CIR.RecordDestruct {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    return CIR.RecordDestruct{
        .label = @bitCast(node.data_1),
        .ident = @bitCast(node.data_2),
        .region = node.region,
        .kind = .Required, // TODO: Need to store and retrieve kind from extra_data
    };
}

/// Retrieves an if branch from the store.
pub fn getIfBranch(store: *const NodeStore, if_branch_idx: CIR.IfBranch.Idx) CIR.IfBranch {
    const nid: Node.Idx = @enumFromInt(@intFromEnum(if_branch_idx));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .if_branch);

    return CIR.IfBranch{
        .cond = @enumFromInt(node.data_1),
        .body = @enumFromInt(node.data_2),
    };
}

/// Returns the top index for scratch expressions.
pub fn scratchExprTop(store: *NodeStore) u32 {
    return store.scratch_exprs.top();
}

/// Adds a scratch expression to temporary storage.
pub fn addScratchExpr(store: *NodeStore, idx: CIR.Expr.Idx) void {
    store.scratch_exprs.append(store.gpa, idx);
}

/// TODO
pub fn addScratchStatement(store: *NodeStore, idx: CIR.Statement.Idx) void {
    store.scratch_statements.append(store.gpa, idx);
}

/// Computes the span of an expression starting from a given index.
pub fn exprSpanFrom(store: *NodeStore, start: u32) CIR.Expr.Span {
    const end = store.scratch_exprs.top();
    defer store.scratch_exprs.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_exprs.items.items[i])) catch |err| exitOnOom(err);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// TODO
pub fn statementSpanFrom(store: *NodeStore, start: u32) CIR.Statement.Span {
    const end = store.scratch_statements.top();
    defer store.scratch_statements.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_statements.items.items[i])) catch |err| exitOnOom(err);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears scratch expressions starting from a specified index.
pub fn clearScratchExprsFrom(store: *NodeStore, start: u32) void {
    store.scratch_exprs.clearFrom(start);
}

/// Returns a slice of expressions from the scratch space.
pub fn exprSlice(store: *const NodeStore, span: CIR.Expr.Span) []CIR.Expr.Idx {
    const slice = store.extra_data.items[span.span.start..(span.span.start + span.span.len)];
    const result: []CIR.Expr.Idx = @ptrCast(@alignCast(slice));
    return result;
}

/// Returns the top index for scratch definitions.
pub fn scratchDefTop(store: *NodeStore) u32 {
    return store.scratch_defs.top();
}

/// Adds a scratch definition to temporary storage.
pub fn addScratchDef(store: *NodeStore, idx: CIR.Def.Idx) void {
    store.scratch_defs.append(store.gpa, idx);
}

/// Adds a type annotation to the scratch buffer.
pub fn addScratchTypeAnno(store: *NodeStore, idx: CIR.TypeAnno.Idx) void {
    store.scratch_type_annos.append(store.gpa, idx);
}

/// Returns the current top of the scratch type annotations buffer.
pub fn scratchTypeAnnoTop(store: *NodeStore) u32 {
    return store.scratch_type_annos.top();
}

/// Clears scratch type annotations from the given index.
pub fn clearScratchTypeAnnosFrom(store: *NodeStore, from: u32) void {
    store.scratch_type_annos.items.shrinkRetainingCapacity(from);
}

/// Creates a span from the scratch type annotations starting at the given index.
pub fn typeAnnoSpanFrom(store: *NodeStore, start: u32) CIR.TypeAnno.Span {
    const end = store.scratch_type_annos.top();
    defer store.scratch_type_annos.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_type_annos.items.items[i])) catch |err| exitOnOom(err);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Returns a span from the scratch anno record fields starting at the given index.
pub fn annoRecordFieldSpanFrom(store: *NodeStore, start: u32) CIR.AnnoRecordField.Span {
    const end = store.scratch_anno_record_fields.top();
    defer store.scratch_anno_record_fields.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_anno_record_fields.items.items[i])) catch |err| exitOnOom(err);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Returns a span from the scratch record fields starting at the given index.
pub fn recordFieldSpanFrom(store: *NodeStore, start: u32) CIR.RecordField.Span {
    const end = store.scratch_record_fields.top();
    defer store.scratch_record_fields.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_record_fields.items.items[i])) catch |err| exitOnOom(err);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Returns the current top of the scratch exposed items buffer.
pub fn scratchExposedItemTop(store: *NodeStore) u32 {
    return store.scratch_exposed_items.top();
}

/// Adds an exposed item to the scratch buffer.
pub fn addScratchExposedItem(store: *NodeStore, idx: CIR.ExposedItem.Idx) void {
    store.scratch_exposed_items.append(store.gpa, idx);
}

/// Creates a span from the scratch exposed items starting at the given index.
pub fn exposedItemSpanFrom(store: *NodeStore, start: u32) CIR.ExposedItem.Span {
    const end = store.scratch_exposed_items.top();
    defer store.scratch_exposed_items.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_exposed_items.items.items[i])) catch |err| exitOnOom(err);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears scratch exposed items from the given index.
pub fn clearScratchExposedItemsFrom(store: *NodeStore, start: u32) void {
    store.scratch_exposed_items.clearFrom(start);
}

/// Returns the start position for a new Span of annoRecordFieldIdxs in scratch
pub fn scratchAnnoRecordFieldTop(store: *NodeStore) u32 {
    return store.scratch_anno_record_fields.top();
}

/// Places a new CIR.AnnoRecordField.Idx in the scratch. Will panic on OOM.
pub fn addScratchAnnoRecordField(store: *NodeStore, idx: CIR.AnnoRecordField.Idx) void {
    store.scratch_anno_record_fields.append(store.gpa, idx);
}

/// Clears any AnnoRecordFieldIds added to scratch from start until the end.
pub fn clearScratchAnnoRecordFieldsFrom(store: *NodeStore, start: u32) void {
    store.scratch_anno_record_fields.clearFrom(start);
}

/// Returns a new AnnoRecordField slice so that the caller can iterate through
/// all items in the span.
pub fn annoRecordFieldSlice(store: *NodeStore, span: CIR.AnnoRecordField.Span) []CIR.AnnoRecordField.Idx {
    return store.sliceFromSpan(CIR.AnnoRecordField.Idx, span.span);
}

/// Computes the span of a definition starting from a given index.
pub fn defSpanFrom(store: *NodeStore, start: u32) CIR.Def.Span {
    const end = store.scratch_defs.top();
    defer store.scratch_defs.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_defs.items.items[i])) catch |err| exitOnOom(err);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Retrieves a slice of record destructures from the store.
pub fn recordDestructSpanFrom(store: *NodeStore, start: u32) CIR.RecordDestruct.Span {
    const end = store.scratch_record_destructs.top();
    defer store.scratch_record_destructs.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_record_destructs.items.items[i])) catch |err| exitOnOom(err);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Gets the current top index of the scratch patterns array.
pub fn scratchPatternTop(store: *NodeStore) u32 {
    return store.scratch_patterns.top();
}

/// Adds a pattern index to the scratch patterns array.
pub fn addScratchPattern(store: *NodeStore, idx: CIR.Pattern.Idx) void {
    store.scratch_patterns.append(store.gpa, idx);
}

/// Gets the current top index of the scratch record destructures array.
pub fn scratchRecordDestructTop(store: *NodeStore) u32 {
    return store.scratch_record_destructs.top();
}

/// Adds a record destruct index to the scratch record destructures array.
pub fn addScratchRecordDestruct(store: *NodeStore, idx: CIR.RecordDestruct.Idx) void {
    store.scratch_record_destructs.append(store.gpa, idx);
}

/// Computes the span of a pattern starting from a given index.
pub fn patternSpanFrom(store: *NodeStore, start: u32) CIR.Pattern.Span {
    const end = store.scratch_patterns.top();
    defer store.scratch_patterns.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_patterns.items.items[i])) catch |err| exitOnOom(err);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears scratch definitions starting from a specified index.
pub fn clearScratchDefsFrom(store: *NodeStore, start: u32) void {
    store.scratch_defs.clearFrom(start);
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

/// Returns a slice of if branches from the store.
pub fn scratchIfBranchTop(store: *NodeStore) u32 {
    return store.scratch_if_branches.top();
}

/// TODO
pub fn addScratchIfBranch(store: *NodeStore, if_branch: CIR.IfBranch) void {
    const if_branch_idx = store.addIfBranch(if_branch);
    store.scratch_if_branches.append(store.gpa, if_branch_idx);
}

/// TODO
pub fn ifBranchSpanFrom(store: *NodeStore, start: u32) CIR.IfBranch.Span {
    const end = store.scratch_if_branches.top();
    defer store.scratch_if_branches.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_if_branches.items.items[i])) catch |err| exitOnOom(err);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// TODO
pub fn sliceIfBranches(store: *const NodeStore, span: CIR.IfBranch.Span) []CIR.IfBranch.Idx {
    return store.sliceFromSpan(CIR.IfBranch.Idx, span.span);
}

/// TODO
pub fn addIfBranch(store: *NodeStore, if_branch: CIR.IfBranch) CIR.IfBranch.Idx {
    const node = Node{
        .data_1 = @intFromEnum(if_branch.cond),
        .data_2 = @intFromEnum(if_branch.body),
        .data_3 = 0,
        .region = Region.zero(),
        .tag = .if_branch,
    };
    const node_idx = store.nodes.append(store.gpa, node);
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
pub fn sliceAnnoRecordFields(store: *const NodeStore, span: CIR.AnnoRecordField.Span) []CIR.AnnoRecordField.Idx {
    return store.sliceFromSpan(CIR.AnnoRecordField.Idx, span.span);
}

/// Returns a slice of record destruct fields from the store.
pub fn sliceRecordDestructs(store: *const NodeStore, span: CIR.RecordDestruct.Span) []CIR.RecordDestruct.Idx {
    return store.sliceFromSpan(CIR.RecordDestruct.Idx, span.span);
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
        .region = base.Region.zero(),
        .tag = @enumFromInt(0),
    };

    switch (reason) {
        .not_implemented => |r| {
            node.tag = .diag_not_implemented;
            node.region = r.region;
            node.data_1 = @intFromEnum(r.feature);
        },
        .invalid_num_literal => |r| {
            node.tag = .diag_invalid_num_literal;
            node.region = r.region;
        },
        .ident_already_in_scope => |r| {
            node.tag = .diag_ident_already_in_scope;
            node.region = r.region;
            node.data_1 = @bitCast(r.ident);
        },
        .ident_not_in_scope => |r| {
            node.tag = .diag_ident_not_in_scope;
            node.region = r.region;
            node.data_1 = @bitCast(r.ident);
        },
        .invalid_top_level_statement => |r| {
            node.tag = .diag_invalid_top_level_statement;
            node.data_1 = @intFromEnum(r.stmt);
        },
        .expr_not_canonicalized => |r| {
            node.tag = .diag_expr_not_canonicalized;
            node.region = r.region;
        },
        .invalid_string_interpolation => |r| {
            node.tag = .diag_invalid_string_interpolation;
            node.region = r.region;
        },
        .pattern_arg_invalid => |r| {
            node.tag = .diag_pattern_arg_invalid;
            node.region = r.region;
        },
        .pattern_not_canonicalized => |r| {
            node.tag = .diag_pattern_not_canonicalized;
            node.region = r.region;
        },
        .can_lambda_not_implemented => |r| {
            node.tag = .diag_can_lambda_not_implemented;
            node.region = r.region;
        },
        .lambda_body_not_canonicalized => |r| {
            node.tag = .diag_lambda_body_not_canonicalized;
            node.region = r.region;
        },
        .if_condition_not_canonicalized => |r| {
            node.tag = .diag_if_condition_not_canonicalized;
            node.region = r.region;
        },
        .if_then_not_canonicalized => |r| {
            node.tag = .diag_if_then_not_canonicalized;
            node.region = r.region;
        },
        .if_else_not_canonicalized => |r| {
            node.tag = .diag_if_else_not_canonicalized;
            node.region = r.region;
        },
        .malformed_type_annotation => |r| {
            node.tag = .diag_malformed_type_annotation;
            node.region = r.region;
        },
        .var_across_function_boundary => |r| {
            node.tag = .diag_var_across_function_boundary;
            node.region = r.region;
        },
        .shadowing_warning => |r| {
            node.tag = .diag_shadowing_warning;
            node.region = r.region;
            node.data_1 = @bitCast(r.ident);
            node.data_2 = r.original_region.start.offset;
            node.data_3 = r.original_region.end.offset;
        },
        .type_redeclared => |r| {
            node.tag = .diag_type_redeclared;
            node.region = r.redeclared_region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = r.original_region.start.offset;
            node.data_3 = r.original_region.end.offset;
        },
        .undeclared_type => |r| {
            node.tag = .diag_undeclared_type;
            node.region = r.region;
            node.data_1 = @bitCast(r.name);
        },
        .undeclared_type_var => |r| {
            node.tag = .diag_undeclared_type_var;
            node.region = r.region;
            node.data_1 = @bitCast(r.name);
        },
        .type_alias_redeclared => |r| {
            node.tag = .diag_type_alias_redeclared;
            node.region = r.redeclared_region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = r.original_region.start.offset;
            node.data_3 = r.original_region.end.offset;
        },
        .custom_type_redeclared => |r| {
            node.tag = .diag_custom_type_redeclared;
            node.region = r.redeclared_region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = r.original_region.start.offset;
            node.data_3 = r.original_region.end.offset;
        },
        .type_shadowed_warning => |r| {
            node.tag = .diag_type_shadowed_warning;
            node.region = r.region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = r.original_region.start.offset;
            node.data_3 = r.original_region.end.offset | (@as(u32, @intFromBool(r.cross_scope)) << 31);
        },
        .type_parameter_conflict => |r| {
            node.tag = .diag_type_parameter_conflict;
            node.region = r.region;
            node.data_1 = @bitCast(r.name);
            node.data_2 = @bitCast(r.parameter_name);
            node.data_3 = r.original_region.start.offset;
        },
        .unused_variable => |r| {
            node.tag = .diag_unused_variable;
            node.region = r.region;
            node.data_1 = @bitCast(r.ident);
        },
        .used_underscore_variable => |r| {
            node.tag = .diag_used_underscore_variable;
            node.region = r.region;
            node.data_1 = @bitCast(r.ident);
        },
        .duplicate_record_field => |r| {
            node.tag = .diag_duplicate_record_field;
            node.region = r.duplicate_region;
            node.data_1 = @bitCast(r.field_name);
            node.data_2 = r.original_region.start.offset;
            node.data_3 = r.original_region.end.offset;
        },
    }

    const nid = @intFromEnum(store.nodes.append(store.gpa, node));

    // append to our scratch so we can get a span later of all our diagnostics
    store.scratch_diagnostics.append(store.gpa, @enumFromInt(nid));

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
        // TODO add a toRegion() helper on the Diagnostic type
        .region = reason.toRegion(),
        .tag = .malformed,
    };

    const malformed_nid = @intFromEnum(store.nodes.append(store.gpa, malformed_node));
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
            .region = node.region,
        } },
        .diag_invalid_num_literal => return CIR.Diagnostic{ .invalid_num_literal = .{
            .region = node.region,
        } },
        .diag_ident_already_in_scope => return CIR.Diagnostic{ .ident_already_in_scope = .{
            .ident = @bitCast(node.data_1),
            .region = node.region,
        } },
        .diag_ident_not_in_scope => return CIR.Diagnostic{ .ident_not_in_scope = .{
            .ident = @bitCast(node.data_1),
            .region = node.region,
        } },
        .diag_invalid_top_level_statement => return CIR.Diagnostic{ .invalid_top_level_statement = .{
            .stmt = @enumFromInt(node.data_1),
        } },
        .diag_expr_not_canonicalized => return CIR.Diagnostic{ .expr_not_canonicalized = .{
            .region = node.region,
        } },
        .diag_invalid_string_interpolation => return CIR.Diagnostic{ .invalid_string_interpolation = .{
            .region = node.region,
        } },
        .diag_pattern_arg_invalid => return CIR.Diagnostic{ .pattern_arg_invalid = .{
            .region = node.region,
        } },
        .diag_pattern_not_canonicalized => return CIR.Diagnostic{ .pattern_not_canonicalized = .{
            .region = node.region,
        } },
        .diag_can_lambda_not_implemented => return CIR.Diagnostic{ .can_lambda_not_implemented = .{
            .region = node.region,
        } },
        .diag_lambda_body_not_canonicalized => return CIR.Diagnostic{ .lambda_body_not_canonicalized = .{
            .region = node.region,
        } },
        .diag_var_across_function_boundary => return CIR.Diagnostic{ .var_across_function_boundary = .{
            .region = node.region,
        } },
        .diag_shadowing_warning => return CIR.Diagnostic{ .shadowing_warning = .{
            .ident = @bitCast(node.data_1),
            .region = node.region,
            .original_region = .{
                .start = .{ .offset = node.data_2 },
                .end = .{ .offset = node.data_3 },
            },
        } },
        .diag_type_redeclared => return CIR.Diagnostic{ .type_redeclared = .{
            .name = @bitCast(node.data_1),
            .redeclared_region = node.region,
            .original_region = .{
                .start = .{ .offset = node.data_2 },
                .end = .{ .offset = node.data_3 },
            },
        } },
        .diag_undeclared_type => return CIR.Diagnostic{ .undeclared_type = .{
            .name = @bitCast(node.data_1),
            .region = node.region,
        } },
        .diag_undeclared_type_var => return CIR.Diagnostic{ .undeclared_type_var = .{
            .name = @bitCast(node.data_1),
            .region = node.region,
        } },
        .diag_malformed_type_annotation => return CIR.Diagnostic{ .malformed_type_annotation = .{
            .region = node.region,
        } },
        .diag_type_alias_redeclared => return CIR.Diagnostic{ .type_alias_redeclared = .{
            .name = @bitCast(node.data_1),
            .redeclared_region = node.region,
            .original_region = .{
                .start = .{ .offset = @intCast(node.data_2) },
                .end = .{ .offset = @intCast(node.data_3) },
            },
        } },
        .diag_custom_type_redeclared => return CIR.Diagnostic{ .custom_type_redeclared = .{
            .name = @bitCast(node.data_1),
            .redeclared_region = node.region,
            .original_region = .{
                .start = .{ .offset = @intCast(node.data_2) },
                .end = .{ .offset = @intCast(node.data_3 & 0x7FFFFFFF) },
            },
        } },
        .diag_type_shadowed_warning => return CIR.Diagnostic{ .type_shadowed_warning = .{
            .name = @bitCast(node.data_1),
            .region = node.region,
            .original_region = .{
                .start = .{ .offset = @intCast(node.data_2) },
                .end = .{ .offset = @intCast(node.data_3 & 0x7FFFFFFF) },
            },
            .cross_scope = (node.data_3 & 0x80000000) != 0,
        } },
        .diag_type_parameter_conflict => return CIR.Diagnostic{ .type_parameter_conflict = .{
            .name = @bitCast(node.data_1),
            .parameter_name = @bitCast(node.data_2),
            .region = node.region,
            .original_region = .{
                .start = .{ .offset = @intCast(node.data_3) },
                .end = .{ .offset = @intCast(node.data_3) },
            },
        } },
        .diag_unused_variable => return CIR.Diagnostic{ .unused_variable = .{
            .ident = @bitCast(node.data_1),
            .region = node.region,
        } },
        .diag_used_underscore_variable => return CIR.Diagnostic{ .used_underscore_variable = .{
            .ident = @bitCast(node.data_1),
            .region = node.region,
        } },
        .diag_duplicate_record_field => return CIR.Diagnostic{ .duplicate_record_field = .{
            .field_name = @bitCast(node.data_1),
            .duplicate_region = node.region,
            .original_region = .{
                .start = .{ .offset = @intCast(node.data_2) },
                .end = .{ .offset = @intCast(node.data_3) },
            },
        } },
        else => {
            @panic("unreachable, node is not a diagnostic tag");
        },
    }
}

/// Computes the span of a diagnostic starting from a given index.
pub fn diagnosticSpanFrom(store: *NodeStore, start: u32) CIR.Diagnostic.Span {
    const end = store.scratch_diagnostics.top();
    defer store.scratch_diagnostics.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_diagnostics.items.items[i])) catch |err| exitOnOom(err);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
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
        .region = region,
    });
    return @enumFromInt(@intFromEnum(nid));
}

/// Calculate the size needed to serialize this NodeStore
pub fn serializedSize(self: *const NodeStore) usize {
    // We only serialize nodes and extra_data (the scratch arrays are transient)
    return self.nodes.serializedSize() +
        @sizeOf(u32) + // extra_data length
        (self.extra_data.items.len * @sizeOf(u32));
}

/// Serialize this NodeStore into the provided buffer
/// Buffer must be at least serializedSize() bytes and properly aligned
pub fn serializeInto(self: *const NodeStore, buffer: []align(@alignOf(Node)) u8) ![]u8 {
    const size = self.serializedSize();
    if (buffer.len < size) return error.BufferTooSmall;

    var offset: usize = 0;

    // Serialize nodes - cast to proper alignment for Node type
    const nodes_buffer = @as([]align(@alignOf(Node)) u8, @alignCast(buffer[offset..]));
    const nodes_slice = try self.nodes.serializeInto(nodes_buffer);
    offset += nodes_slice.len;

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

    return buffer[0..offset];
}

/// Deserialize a NodeStore from the provided buffer
pub fn deserializeFrom(buffer: []align(@alignOf(Node)) const u8, allocator: std.mem.Allocator) !NodeStore {
    var offset: usize = 0;

    // Deserialize nodes - cast to proper alignment for Node type
    const nodes_buffer = @as([]align(@alignOf(Node)) const u8, @alignCast(buffer[offset..]));
    const nodes = try Node.List.deserializeFrom(nodes_buffer, allocator);
    offset += nodes.serializedSize();

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
        .extra_data = extra_data,
        // All scratch arrays start empty
        .scratch_statements = base.Scratch(CIR.Statement.Idx){ .items = .{} },
        .scratch_exprs = base.Scratch(CIR.Expr.Idx){ .items = .{} },
        .scratch_record_fields = base.Scratch(CIR.RecordField.Idx){ .items = .{} },
        .scratch_when_branches = base.Scratch(CIR.WhenBranch.Idx){ .items = .{} },
        .scratch_if_branches = base.Scratch(CIR.IfBranch.Idx){ .items = .{} },
        .scratch_where_clauses = base.Scratch(CIR.WhereClause.Idx){ .items = .{} },
        .scratch_patterns = base.Scratch(CIR.Pattern.Idx){ .items = .{} },
        .scratch_pattern_record_fields = base.Scratch(CIR.PatternRecordField.Idx){ .items = .{} },
        .scratch_type_annos = base.Scratch(CIR.TypeAnno.Idx){ .items = .{} },
        .scratch_anno_record_fields = base.Scratch(CIR.AnnoRecordField.Idx){ .items = .{} },
        .scratch_exposed_items = base.Scratch(CIR.ExposedItem.Idx){ .items = .{} },
        .scratch_defs = base.Scratch(CIR.Def.Idx){ .items = .{} },
        .scratch_diagnostics = base.Scratch(CIR.Diagnostic.Idx){ .items = .{} },
        .scratch_record_destructs = base.Scratch(CIR.RecordDestruct.Idx){ .items = .{} },
    };
}
