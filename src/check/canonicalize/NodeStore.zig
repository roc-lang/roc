//! Stores AST nodes and provides scratch arrays for working with nodes.

const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const collections = @import("../../collections.zig");
const Node = @import("Node.zig");
const CIR = @import("CIR.zig");

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
scratch_where_clauses: base.Scratch(CIR.WhereClause.Idx),
scratch_patterns: base.Scratch(CIR.Pattern.Idx),
scratch_pattern_record_fields: base.Scratch(CIR.PatternRecordField.Idx),
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
        .scratch_when_branches = base.Scratch(CIR.WhenBranch.Idx).init(gpa),
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
    store.scratch_when_branches.items.deinit(store.gpa);
    store.scratch_type_annos.items.deinit(store.gpa);
    store.scratch_anno_record_fields.items.deinit(store.gpa);
    store.scratch_exposed_items.items.deinit(store.gpa);
    store.scratch_defs.items.deinit(store.gpa);
    store.scratch_where_clauses.items.deinit(store.gpa);
    store.scratch_diagnostics.items.deinit(store.gpa);
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
        .statement_type_decl => return CIR.Statement{
            .type_decl = .{
                .region = node.region,
                .anno = @enumFromInt(node.data_1),
                .header = @enumFromInt(0), // TODO save these in extra_data and then insert them here
                .where = null, // TODO save these in extra_data and then insert them here
            },
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
            return CIR.Expr{ .lookup = .{
                .pattern_idx = @enumFromInt(node.data_1),
                .region = node.region,
            } };
        },
        .expr_int => {
            // Unpack the literal index from lower 16 bits of data_1
            const literal: base.StringLiteral.Idx = @enumFromInt(node.data_1 & 0xFFFF);

            // Retrieve type variable from data_2 and requirements from data_3
            const int_var = @as(types.Var, @enumFromInt(node.data_2));
            const requirements = @as(types.Num.Int.Requirements, @bitCast(@as(u5, @intCast(node.data_3))));

            // Extract extra_data index from packed data_1
            const extra_idx = node.data_1 >> 16;

            // Read i128 from extra_data (stored as 4 u32s)
            const value_as_u32s = store.extra_data.items[extra_idx..][0..4];
            const value: i128 = @bitCast(value_as_u32s.*);

            // We store all values as i128 and reconstruct based on what was originally stored
            // For now, default to i128 - the actual variant should be determined by the type system
            const literal_value = CIR.IntLiteralValue{ .value = value };

            return .{
                .int = .{
                    .int_var = int_var,
                    .requirements = requirements,
                    .literal = literal,
                    .value = literal_value,
                    .region = node.region,
                },
            };
        },
        .expr_list => {
            return .{
                .list = .{
                    .elems = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
                    .elem_var = @enumFromInt(0), // TODO: get from extra_data
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
        .expr_frac => {
            // Retrieve the literal index from data_1
            const literal: base.StringLiteral.Idx = @enumFromInt(node.data_1);

            // Retrieve type variable from data_2 and requirements from data_3
            const frac_var = @as(types.Var, @enumFromInt(node.data_2));
            const requirements = @as(types.Num.Frac.Requirements, @bitCast(@as(u3, @truncate(node.data_3))));

            // TODO get value and bound from extra_data

            return CIR.Expr{
                .frac = .{
                    .frac_var = frac_var,
                    .requirements = requirements,
                    .literal = literal,
                    .value = 0,
                    // TODO shouldn't this be a flex_var?
                    .region = node.region,
                },
            };
        },
        .expr_string_segment => return CIR.Expr.init_str_segment(
            @enumFromInt(node.data_1),
            node.region,
        ),
        .expr_string => return CIR.Expr.init_str(
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
        .expr_tuple,
        .expr_record,
        .expr_field_access,
        .expr_static_dispatch,
        .expr_apply,
        .expr_record_update,
        .expr_unary,
        .expr_suffix_single_question,
        .expr_if_then_else,
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
                .elem_var = @enumFromInt(0), // TODO need to store and retrieve from extra_data
                .list_var = @enumFromInt(0), // TODO need to store and retrieve from extra_data
            },
        },
        .pattern_num_literal => return CIR.Pattern{
            .num_literal = .{
                .region = node.region,
                .literal = @enumFromInt(node.data_1),
                .num_var = @enumFromInt(0), // TODO need to store and retrieve from extra_data
                .value = CIR.IntLiteralValue{ .value = 0 }, // TODO need to store and retrieve from extra_data
            },
        },
        .pattern_int_literal => return CIR.Pattern{
            .int_literal = .{
                .region = node.region,
                .literal = @enumFromInt(node.data_1),
                .requirements = .{ .sign_needed = false, .bits_needed = .@"7" }, // TODO need to store and retrieve from extra_data
                .num_var = @enumFromInt(0), // TODO need to store and retrieve from extra_data
                .value = CIR.IntLiteralValue{ .value = 0 }, // TODO need to store and retrieve from extra_data
            },
        },
        .pattern_frac_literal => return CIR.Pattern{
            .frac_literal = .{
                .region = node.region,
                .literal = @enumFromInt(node.data_1),
                .requirements = .{ .fits_in_f32 = true, .fits_in_f64 = true, .fits_in_dec = true }, // TODO need to store and retrieve from extra_data
                .num_var = @enumFromInt(0), // TODO need to store and retrieve from extra_data
                .value = 42, // TODO need to store and retrieve from extra_data
            },
        },
        .pattern_str_literal => return CIR.Pattern{ .str_literal = .{
            .region = node.region,
            .literal = @enumFromInt(node.data_1),
        } },
        .pattern_char_literal => return CIR.Pattern{
            .char_literal = .{
                .region = node.region,
                .value = node.data_1,
                .num_var = @enumFromInt(0), // TODO need to store and retrieve from extra_data
                .requirements = .{ .sign_needed = false, .bits_needed = .@"7" }, // TODO need to store and retrieve from extra_data
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
pub fn getTypeAnno(store: *NodeStore, typeAnno: CIR.TypeAnno.Idx) CIR.TypeAnno {
    _ = store;
    _ = typeAnno;
    @panic("TODO: implement getTypeAnno");
}

/// Retrieves an annotation record field from the store.
pub fn getAnnoRecordField(store: *NodeStore, annoRecordField: CIR.AnnoRecordField.Idx) CIR.AnnoRecordField {
    _ = store;
    _ = annoRecordField;
    @panic("TODO: implement getAnnoRecordField");
}

/// Retrieves an exposed item from the store.
pub fn getExposedItem(store: *NodeStore, exposedItem: CIR.ExposedItem.Idx) CIR.ExposedItem {
    _ = store;
    _ = exposedItem;
    @panic("TODO: implement getExposedItem");
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
            node.data_1 = @intFromEnum(s.anno);
            // TODO store header and where clause data in extra_data
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
            node.region = e.region;
            node.tag = .expr_var;
            node.data_1 = @intFromEnum(e.pattern_idx);
        },
        .int => |e| {
            node.region = e.region;
            node.tag = .expr_int;

            // Store the literal index in data_1
            node.data_1 = @intFromEnum(e.literal);

            // Store type variable in data_2 and requirements in data_3
            node.data_2 = @intFromEnum(e.int_var);
            node.data_3 = @as(u32, @intCast(@as(u5, @bitCast(e.requirements))));

            // Store i128 value in extra_data
            const extra_data_start = store.extra_data.items.len;

            // Store the IntLiteralValue as i128 (16 bytes = 4 u32s)
            // We always store as i128 internally
            const value_as_i128: i128 = e.value.value;
            const value_as_u32s: [4]u32 = @bitCast(value_as_i128);
            for (value_as_u32s) |word| {
                store.extra_data.append(store.gpa, word) catch |err| exitOnOom(err);
            }

            // Store the extra_data index in the node (reuse data_1 which has literal)
            // We'll pack it: high 16 bits = extra_data index, low 16 bits = literal index
            const literal_idx: u16 = @intCast(@intFromEnum(e.literal));
            const extra_idx: u16 = @intCast(extra_data_start);
            node.data_1 = (@as(u32, extra_idx) << 16) | literal_idx;
        },
        .list => |e| {
            node.region = e.region;
            node.tag = .expr_list;
            // TODO: Store list data properly. For now, just store placeholder values
            node.data_1 = e.elems.span.start;
            node.data_2 = e.elems.span.len;
        },
        .frac => |e| {
            node.region = e.region;
            node.tag = .expr_frac;

            // Store the literal index in data_1
            node.data_1 = @intFromEnum(e.literal);

            // Store type variable in data_2 and requirements in data_3
            node.data_2 = @intFromEnum(e.frac_var);
            node.data_3 = @as(u32, @intCast(@as(u3, @bitCast(e.requirements))));

            // TODO for storing the value and bound, use extra_data
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
            @panic("TODO addExpr if");
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
            @panic("TODO addExpr record");
        },
        .empty_record => |e| {
            node.region = e.region;
            @panic("TODO addExpr empty_record");
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
    _ = store;
    _ = recordField;

    return .{ .id = @enumFromInt(0) };
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
            // TODO store type vars in extra data
        },
        .num_literal => |p| {
            node.tag = .pattern_num_literal;
            node.region = p.region;
            node.data_1 = @intFromEnum(p.literal);
            // TODO store other data in extra_data
        },
        .int_literal => |p| {
            node.tag = .pattern_int_literal;
            node.region = p.region;
            node.data_1 = @intFromEnum(p.literal);
            // TODO store other data
        },
        .frac_literal => |p| {
            node.tag = .pattern_frac_literal;
            node.region = p.region;
            node.data_1 = @intFromEnum(p.literal);
            // TODO store other data
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
    const node = Node{};

    switch (typeAnno) {
        else => {
            std.debug.panic("Type Annotation of type {s} not yet implemented in Can\n", .{@tagName(typeAnno)});
        },
    }

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(nid);
}

/// Adds an annotation record field to the store.
pub fn addAnnoRecordField(store: *NodeStore, annoRecordField: CIR.AnnoRecordField) CIR.AnnoRecordField.Idx {
    _ = store;
    _ = annoRecordField;

    return @enumFromInt(0);
}

/// Adds an exposed item to the store.
pub fn addExposedItem(store: *NodeStore, exposedItem: CIR.ExposedItem) CIR.ExposedItem.Idx {
    const node = Node{};

    switch (exposedItem) {
        else => {
            std.debug.panic("Exposed Item of type {s} not yet implemented in Can\n", .{@tagName(exposedItem)});
        },
    }

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(nid);
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
pub fn getDef(store: *NodeStore, def_idx: CIR.Def.Idx) CIR.Def {
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
pub fn getRecordField(store: *NodeStore, recordField: CIR.RecordField.Idx) CIR.RecordField {
    _ = store;
    _ = recordField;

    return CIR.RecordField{};
}

// pub fn getIfBranch(store: *const NodeStore, if_branch_idx: IfBranch.Idx) IfBranch {
//     const nid: Node.Idx = @enumFromInt(@intFromEnum(if_branch_idx));
//     const node = store.nodes.get(nid);

//     std.debug.assert(node.tag == .if_branch);

//     return IfBranch{

//     };
// }

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

/// TODO
pub fn sliceStatements(store: *const NodeStore, span: CIR.Statement.Span) []CIR.Statement.Idx {
    return store.sliceFromSpan(CIR.Statement.Idx, span.span);
}

/// TODO
pub fn sliceIfBranch(store: *const NodeStore, span: CIR.IfBranch.Span) []CIR.IfBranch.Idx {
    return store.sliceFromSpan(CIR.IfBranch.Idx, span.span);
}

/// Returns a slice of diagnostics from the store.
pub fn sliceDiagnostics(store: *const NodeStore, span: CIR.Diagnostic.Span) []CIR.Diagnostic.Idx {
    return store.sliceFromSpan(CIR.Diagnostic.Idx, span.span);
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
            node.data_1 = @intFromEnum(r.literal);
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
        .region = switch (reason) {
            .not_implemented => |r| r.region,
            .invalid_num_literal => |r| r.region,
            .ident_already_in_scope => |r| r.region,
            .ident_not_in_scope => |r| r.region,
            .invalid_top_level_statement => Region.zero(),
            .expr_not_canonicalized => |r| r.region,
            .invalid_string_interpolation => |r| r.region,
            .pattern_arg_invalid => |r| r.region,
            .pattern_not_canonicalized => |r| r.region,
            .can_lambda_not_implemented => |r| r.region,
            .lambda_body_not_canonicalized => |r| r.region,
            .var_across_function_boundary => |r| r.region,
            .shadowing_warning => |r| r.region,
        },
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
            .literal = @enumFromInt(node.data_1),
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
