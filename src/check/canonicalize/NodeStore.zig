//! Stores AST nodes and provides scratch arrays for working with nodes.

const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const collections = @import("../../collections.zig");
const Node = @import("Node.zig");
const CIR = @import("CIR.zig");

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
}

/// Retrieves a statement node from the store.
pub fn getStatement(store: *NodeStore, statement: CIR.Statement.Idx) CIR.Statement {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(statement));
    const node = store.nodes.get(node_idx);

    switch (node.tag) {
        .statement_expr => {
            return .{ .expr = .{
                .expr = node.data_1,
                .region = node.region,
            } };
        },
        // .statement_decl => {},
        // .statement_var => {},
        // .statement_for => {},
        // .statement_expect => {},
        // .statement_return => {},
        // .statement_import => {},
        // .statement_type_decl => {},
        // .statement_type_anno => {},
        // .statement_crash => {},
        else => @panic("TODO: implement other statement variants"),
        // not a statement node
        // else => unreachable,
    }
}

/// Retrieves an expression node from the store.
pub fn getExpr(store: *const NodeStore, expr: CIR.Expr.Idx) CIR.Expr {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(expr));
    const node = store.nodes.get(node_idx);

    switch (node.tag) {
        .expr_var => {
            const ident_idx: base.Ident.Idx = @bitCast(@as(u32, @bitCast(node.data_1)));
            return CIR.Expr{
                .lookup = .{
                    .ident = ident_idx,
                },
            };
        },
        .expr_int => {
            // Retrieve the literal index from data_1
            const literal: base.StringLiteral.Idx = @enumFromInt(node.data_1);

            // Retrieve type variables from data_2 and data_3
            const num_var = @as(types.Var, @enumFromInt(node.data_2));
            const precision_var = @as(types.Var, @enumFromInt(node.data_3));

            // TODO get value and bound from extra_data

            return .{
                .int = .{
                    .num_var = num_var,
                    .precision_var = precision_var,
                    .literal = literal,
                    .value = CIR.IntValue{ // Placeholder value
                        .bytes = [16]u8{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
                        .kind = .i128,
                    },
                    // TODO shouldn't this be a flex_var?
                    .bound = types.Num.Compact.Int.Precision.i128,
                },
            };
        },
        .expr_list => {
            return .{
                .list = .{
                    .elems = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
                    .elem_var = @enumFromInt(0), // TODO: get from extra_data
                },
            };
        },
        .expr_call => {
            return .{
                .call = .{
                    .args = .{ .span = .{ .start = node.data_1, .len = node.data_2 } },
                },
            };
        },
        .expr_float => {
            // Retrieve the literal index from data_1
            const literal: base.StringLiteral.Idx = @enumFromInt(node.data_1);

            // Retrieve type variables from data_2 and data_3
            const num_var = @as(types.Var, @enumFromInt(node.data_2));
            const precision_var = @as(types.Var, @enumFromInt(node.data_3));

            // TODO get value and bound from extra_data

            return CIR.Expr{
                .float = .{
                    .num_var = num_var,
                    .precision_var = precision_var,
                    .literal = literal,
                    .value = 0,
                    // TODO shouldn't this be a flex_var?
                    .bound = types.Num.Compact.Frac.Precision.dec,
                },
            };
        },
        .expr_string => {
            return .{
                .str = @enumFromInt(node.data_1),
            };
        },
        .expr_tag => {
            return .{
                .tag = .{
                    .tag_union_var = @enumFromInt(0), // Placeholder
                    .ext_var = @enumFromInt(0), // Placeholder
                    .name = @bitCast(@as(base.Ident.Idx, @bitCast(node.data_1))),
                    .args = .{ .span = .{ .start = 0, .len = 0 } }, // Empty args for now
                },
            };
        },
        .malformed => {
            return CIR.Expr{
                .runtime_error = @enumFromInt(node.data_1),
            };
        },
        .statement_expr,
        .statement_decl,
        .statement_var,
        .statement_for,
        .statement_expect,
        .statement_return,
        .statement_import,
        .statement_type_decl,
        .statement_type_anno,
        .statement_crash,
        .expr_tuple,
        .expr_record,
        .expr_field_access,
        .expr_static_dispatch,
        .expr_apply,
        .expr_string_part,
        .expr_lambda,
        .expr_record_update,
        .expr_bin_op,
        .expr_unary,
        .expr_suffix_single_question,
        .expr_if_then_else,
        .expr_match,
        .expr_dbg,
        .expr_block,
        .expr_ellipsis,

        .expr_record_builder,
        .type_decl_header,
        .type_anno_apply,
        .type_anno_var,
        .type_anno_ty,
        .type_anno_underscore,
        .type_anno_mod_ty,
        .type_anno_union,
        .type_anno_tuple,
        .type_anno_record,
        .type_anno_fn,
        .type_anno_parens,
        .pattern_identifier,
        .pattern_as,
        .pattern_applied_tag,
        .def,
        .if_branch,
        => {
            std.log.debug("TODO: implement getExpr for node type {?}", .{node.tag});
            return CIR.Expr{ .runtime_error = @enumFromInt(0) };
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
pub fn getPattern(store: *NodeStore, pattern_idx: CIR.Pattern.Idx) CIR.Pattern {
    const node_idx: Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));
    const node = store.nodes.get(node_idx);

    switch (node.tag) {
        .pattern_identifier => {
            const ident_idx: base.Ident.Idx = @bitCast(node.data_1);
            return CIR.Pattern{ .assign = ident_idx };
        },
        else => {
            std.log.debug("TODO: implement pattern {}", .{node.tag});
            @panic("unimplemented");
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
    const node = Node{};

    switch (statement) {
        .expr => |stmt| {
            node.data_1 = stmt.expr;
            node.region = stmt.region;
        },
        else => {
            std.debug.panic("Statement of type {s} not yet implemented in Can\n", .{@tagName(statement)});
        },
    }

    return store.add(node);
}

/// Adds an expression node to the store.
pub fn addExpr(store: *NodeStore, expr: CIR.ExprAtRegion) CIR.Expr.Idx {
    var node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .region = base.Region.zero(),
        .tag = @enumFromInt(0),
    };
    node.region = expr.region;

    switch (expr.expr) {
        .lookup => |e| {
            node.tag = .expr_var;
            node.data_1 = @bitCast(@as(u32, @bitCast(e.ident)));
        },
        .int => |e| {
            node.tag = .expr_int;

            // Store the literal index in data_1
            node.data_1 = @intFromEnum(e.literal);

            // Store type variables in data_2 and data_3
            node.data_2 = @intFromEnum(e.num_var);
            node.data_3 = @intFromEnum(e.precision_var);

            // TODO for storing the value and bound, use extra_data
        },
        .list => |e| {
            node.tag = .expr_list;
            // TODO: Store list data properly. For now, just store placeholder values
            node.data_1 = e.elems.span.start;
            node.data_2 = e.elems.span.len;
        },
        .float => |e| {
            node.tag = .expr_float;

            // Store the literal index in data_1
            node.data_1 = @intFromEnum(e.literal);

            // Store type variables in data_2 and data_3
            node.data_2 = @intFromEnum(e.num_var);
            node.data_3 = @intFromEnum(e.precision_var);

            // TODO for storing the value and bound, use extra_data
        },
        .str => |e| {
            node.tag = .expr_string;
            // TODO: Store string data properly. For now, just store the literal idx
            node.data_1 = @intCast(@intFromEnum(e));
        },
        .tag => |e| {
            node.tag = .expr_tag;
            // Store the full Ident.Idx as a u32
            node.data_1 = @bitCast(@as(u32, @bitCast(e.name)));
        },
        .runtime_error => |err| {
            node.data_1 = @intFromEnum(err);
            node.tag = .malformed;
        },
        .num => {
            @panic("TODO addExpr num");
        },
        .single_quote => {
            @panic("TODO addExpr single_quote");
        },
        .when => {
            @panic("TODO addExpr when");
        },
        .@"if" => {
            @panic("TODO addExpr if");
        },
        .call => {
            node.tag = .expr_call;
            // Store the args span
            node.data_1 = expr.expr.call.args.span.start;
            node.data_2 = expr.expr.call.args.span.len;
        },
        .record => {
            @panic("TODO addExpr record");
        },
        .empty_record => {
            @panic("TODO addExpr empty_record");
        },
        .record_access => {
            @panic("TODO addExpr record_access");
        },
        .zero_argument_tag => {
            @panic("TODO addExpr zero_argument_tag");
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
        .assign => |ident_idx| {
            node.data_1 = @bitCast(ident_idx);
            node.tag = .pattern_identifier;
        },
        .as,
        .applied_tag,
        .record_destructure,
        .list,
        .num_literal,
        .int_literal,
        .float_literal,
        .str_literal,
        .char_literal,
        .Underscore,
        => {
            std.debug.panic("Pattern of type {s} not yet implemented in Can\n", .{@tagName(pattern)});
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
    // Store expr_var
    store.extra_data.append(store.gpa, @intFromEnum(def.expr_var)) catch |err| exitOnOom(err);
    // Store kind tag
    store.extra_data.append(store.gpa, @intFromEnum(def.kind)) catch |err| exitOnOom(err);
    // Store annotation idx (0 if null)
    const anno_idx = if (def.annotation) |anno| @intFromEnum(anno) else 0;
    store.extra_data.append(store.gpa, anno_idx) catch |err| exitOnOom(err);

    // Store the extra data range in the node
    node.data_1 = extra_start;
    node.data_2 = 5; // Number of extra data items

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
    const expr_var: types.Var = @enumFromInt(extra_data[2]);
    const kind_tag = extra_data[3];
    const anno_idx = extra_data[4];

    const kind: CIR.Def.Kind = switch (kind_tag) {
        @intFromEnum(CIR.Def.Kind.Let) => .Let,
        else => .{ .Stmt = @enumFromInt(0) }, // TODO: implement proper kind deserialization
    };

    const annotation: ?CIR.Annotation.Idx = if (anno_idx == 0) null else @enumFromInt(anno_idx);

    return CIR.Def{
        .pattern = pattern,
        .pattern_region = node.region, // Stored as node region
        .expr = expr,
        .expr_region = base.Region.zero(), // TODO store and retrieve expr region
        .expr_var = expr_var,
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

/// Clears scratch definitions starting from a specified index.
pub fn clearScratchDefsFrom(store: *NodeStore, start: u32) void {
    store.scratch_defs.clearFrom(start);
}

/// Creates a slice corresponding to a span.
pub fn sliceFromSpan(store: *NodeStore, comptime T: type, span: base.DataSpan) []T {
    return @ptrCast(store.extra_data.items[span.start..][0..span.len]);
}

/// Returns a slice of definitions from the store.
pub fn sliceDefs(store: *NodeStore, span: CIR.Def.Span) []CIR.Def.Idx {
    return store.sliceFromSpan(CIR.Def.Idx, span.span);
}

/// Returns a slice of `CanIR.Pattern.Idx`
pub fn slicePatterns(store: *NodeStore, span: CIR.Pattern.Span) []CIR.Pattern.Idx {
    return store.sliceFromSpan(CIR.Pattern.Idx, span.span);
}

/// Returns a slice of `CanIR.IfBranch.Idx`
pub fn sliceIfBranch(store: *const NodeStore, span: CIR.IfBranch.Span) []CIR.IfBranch.Idx {
    return store.sliceFromSpan(CIR.IfBranch.Idx, span.span);
}
