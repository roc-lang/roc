const std = @import("std");
const testing = std.testing;
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const problem = @import("../../problem.zig");
const collections = @import("../../collections.zig");
const Alias = @import("./Alias.zig");
const sexpr = @import("../../base/sexpr.zig");
const exitOnOom = @import("../../collections/utils.zig").exitOnOom;
const Scratch = base.Scratch;
const DataSpan = base.DataSpan;
const Ident = base.Ident;
const Region = base.Region;
const ModuleImport = base.ModuleImport;
const ModuleEnv = base.ModuleEnv;
const StringLiteral = base.StringLiteral;
const TypeVar = types.Var;
const Problem = problem.Problem;

const IR = @This();

env: base.ModuleEnv,
store: NodeStore,
ingested_files: IngestedFile.List,
imports: ModuleImport.Store,
type_store: types.Store,
top_level_defs: Def.Span,

/// Initialize the IR for a module's canonicalization info.
///
/// When caching the can IR for a siloed module, we can avoid
/// manual deserialization of the cached data into IR by putting
/// the entirety of the IR into an arena that holds nothing besides
/// the IR. We can then load the cached binary data back into memory
/// with only 2 syscalls.
///
/// Since the can IR holds indices into the `ModuleEnv`, we need
/// the `ModuleEnv` to also be owned by the can IR to cache it.
///
/// Takes ownership of the module_env and type_store
pub fn init(env: ModuleEnv, type_store: types.Store) IR {
    // TODO: Figure out what capacity should be
    return IR.initCapacity(env, type_store, 1000);
}

/// Initialize the IR for a module's canonicalization info with a specified capacity.
/// For more information refer to documentation on [init] as well
pub fn initCapacity(env: ModuleEnv, type_store: types.Store, capacity: usize) IR {
    var ident_store = env.idents;

    return IR{
        .env = env,
        .store = NodeStore.initCapacity(env.gpa, capacity),
        .ingested_files = .{},
        .imports = ModuleImport.Store.init(&.{}, &ident_store, env.gpa),
        .type_store = type_store,
        .top_level_defs = .{ .span = .{ .start = 0, .len = 0 } },
    };
}

/// Deinit the IR's memory.
pub fn deinit(self: *IR) void {
    self.store.deinit();
    self.ingested_files.deinit(self.env.gpa);
    self.imports.deinit(self.env.gpa);
    self.type_store.deinit();
}

// Helper to add type index info
fn appendTypeVarChild(node: *sexpr.Expr, gpa: std.mem.Allocator, name: []const u8, type_idx: TypeVar) void {
    var type_node = sexpr.Expr.init(gpa, name);
    type_node.appendUnsignedIntChild(gpa, @intCast(@intFromEnum(type_idx)));
    node.appendNodeChild(gpa, &type_node);
}

// Helper to add identifier info
fn appendIdentChild(node: *sexpr.Expr, gpa: std.mem.Allocator, ir: *const IR, name: []const u8, ident_idx: Ident.Idx) void {
    const ident_text = ir.env.idents.getText(ident_idx);

    // Create a node with no pre-allocated children to avoid aliasing issues
    const ident_node = sexpr.Expr{
        .node = .{
            .value = gpa.dupe(u8, name) catch @panic("Failed to duplicate name"),
            .children = .{}, // Empty ArrayListUnmanaged - no allocation
        },
    };

    // Append the node to the parent first
    switch (node.*) {
        .node => |*n| {
            n.children.append(gpa, ident_node) catch @panic("Failed to append node");

            // Now add the string child directly to the node in its final location
            const last_idx = n.children.items.len - 1;
            n.children.items[last_idx].appendStringChild(gpa, ident_text);
        },
        else => @panic("appendIdentChild called on non-node"),
    }
}

test "Node is 24 bytes" {
    try testing.expectEqual(24, @sizeOf(Node));
}

/// A single meaningful node in the Abstract Syntax Tree.
/// Should always be inserted and fetched from a Node Store.
///
/// The Tag represents what type of Node it is, and
/// therefore how it's data and main_token fields should
/// be interpreted.
pub const Node = struct {
    data_1: u32,
    data_2: u32,
    data_3: u32,
    region: Region,
    tag: Tag,

    pub const List = collections.SafeMultiList(Node);

    /// Internal representation for where a node is stored
    /// in the tree.
    pub const Idx = List.Idx;

    /// This is the tag associated with a raw Node in the list
    pub const Tag = enum {
        // Statements
        statement_expr,
        statement_decl,
        statement_var,
        statement_for,
        statement_expect,
        statement_return,
        statement_import,
        statement_type_decl,
        statement_type_anno,
        statement_crash,
        // Expressions
        expr_var,
        expr_tuple,
        expr_list,
        expr_call,
        expr_record,
        expr_field_access,
        expr_static_dispatch,
        expr_apply,
        expr_string,
        expr_string_part,
        expr_int,
        expr_float,
        expr_tag,
        expr_lambda,
        expr_record_update,
        expr_bin_op,
        expr_unary,
        expr_suffix_single_question,
        expr_if_then_else,
        expr_match,
        expr_dbg,
        expr_block,
        expr_ellipsis,
        expr_record_builder,
        // Type Header
        type_decl_header,
        // Type Annotation
        type_anno_apply,
        type_anno_var,
        type_anno_ty,
        type_anno_underscore,
        type_anno_mod_ty,
        type_anno_union,
        type_anno_tuple,
        type_anno_record,
        type_anno_fn,
        type_anno_parens,
        // Patterns
        pattern_identifier,
        pattern_as,
        pattern_applied_tag,
        // Definitions
        def,
        // Runtime Error
        malformed,
    };
};

/// Stores AST nodes and provides scratch arrays for working with nodes.
pub const NodeStore = struct {
    gpa: std.mem.Allocator,
    nodes: Node.List,
    extra_data: std.ArrayListUnmanaged(u32),
    scratch_statements: base.Scratch(Statement.Idx),
    scratch_exprs: base.Scratch(Expr.Idx),
    scratch_record_fields: base.Scratch(RecordField.Idx),
    scratch_when_branches: base.Scratch(WhenBranch.Idx),
    scratch_where_clauses: base.Scratch(WhereClause.Idx),
    scratch_patterns: base.Scratch(Pattern.Idx),
    scratch_pattern_record_fields: base.Scratch(PatternRecordField.Idx),
    scratch_type_annos: base.Scratch(TypeAnno.Idx),
    scratch_anno_record_fields: base.Scratch(AnnoRecordField.Idx),
    scratch_exposed_items: base.Scratch(ExposedItem.Idx),
    scratch_defs: base.Scratch(Def.Idx),

    pub fn init(gpa: std.mem.Allocator) NodeStore {
        // TODO determine what capacity to use
        // maybe these should be moved to build/compile flags?
        return NodeStore.initCapacity(gpa, 128);
    }

    pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) NodeStore {
        return .{
            .gpa = gpa,
            .nodes = Node.List.initCapacity(gpa, capacity),
            .extra_data = std.ArrayListUnmanaged(u32).initCapacity(gpa, capacity / 2) catch |err| exitOnOom(err),
            .scratch_statements = base.Scratch(Statement.Idx).init(gpa),
            .scratch_exprs = base.Scratch(Expr.Idx).init(gpa),
            .scratch_patterns = base.Scratch(Pattern.Idx).init(gpa),
            .scratch_record_fields = base.Scratch(RecordField.Idx).init(gpa),
            .scratch_pattern_record_fields = base.Scratch(PatternRecordField.Idx).init(gpa),
            .scratch_when_branches = base.Scratch(WhenBranch.Idx).init(gpa),
            .scratch_type_annos = base.Scratch(TypeAnno.Idx).init(gpa),
            .scratch_anno_record_fields = base.Scratch(AnnoRecordField.Idx).init(gpa),
            .scratch_exposed_items = base.Scratch(ExposedItem.Idx).init(gpa),
            .scratch_defs = base.Scratch(Def.Idx).init(gpa),
            .scratch_where_clauses = base.Scratch(WhereClause.Idx).init(gpa),
        };
    }

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

    pub fn getStatement(store: *NodeStore, statement: Statement.Idx) Statement {
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

    pub fn getExpr(store: *const NodeStore, expr: Expr.Idx) Expr {
        const node_idx: Node.Idx = @enumFromInt(@intFromEnum(expr));
        const node = store.nodes.get(node_idx);

        switch (node.tag) {
            .expr_var => {
                const ident_idx: base.Ident.Idx = @bitCast(@as(u32, @bitCast(node.data_1)));
                return Expr{
                    .lookup = .{
                        .ident = ident_idx,
                    },
                };
            },
            .expr_int => {
                // Retrieve the literal index from data_1
                const literal: StringLiteral.Idx = @enumFromInt(node.data_1);

                // Retrieve type variables from data_2 and data_3
                const num_var = @as(types.Var, @enumFromInt(node.data_2));
                const precision_var = @as(types.Var, @enumFromInt(node.data_3));

                // TODO get value and bound from extra_data

                return .{
                    .int = .{
                        .num_var = num_var,
                        .precision_var = precision_var,
                        .literal = literal,
                        .value = IntValue{ // Placeholder value
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
                const literal: StringLiteral.Idx = @enumFromInt(node.data_1);

                // Retrieve type variables from data_2 and data_3
                const num_var = @as(types.Var, @enumFromInt(node.data_2));
                const precision_var = @as(types.Var, @enumFromInt(node.data_3));

                // TODO get value and bound from extra_data

                return Expr{
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
                return .{
                    .RuntimeError = @enumFromInt(node.data_1),
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
            => {
                std.log.debug("TODO: implement getExpr for node type {?}", .{node.tag});
                return .{ .RuntimeError = @enumFromInt(0) };
            },
        }
    }

    pub fn getWhenBranch(store: *const NodeStore, whenBranch: WhenBranch.Idx) WhenBranch {
        _ = store;
        _ = whenBranch;
        @panic("TODO: implement getWhenBranch");
    }

    pub fn whenBranchSlice(store: *NodeStore, range: WhenBranch.Range) []WhenBranch {
        return store.nodes.rangeToSlice(range);
    }

    pub fn getWhereClause(store: *NodeStore, whereClause: WhereClause.Idx) WhereClause {
        _ = store;
        _ = whereClause;
        @panic("TODO: implement getWhereClause");
    }
    pub fn getPattern(store: *NodeStore, pattern_idx: Pattern.Idx) Pattern {
        const node_idx: Node.Idx = @enumFromInt(@intFromEnum(pattern_idx));
        const node = store.nodes.get(node_idx);

        switch (node.tag) {
            .pattern_identifier => {
                const ident_idx: Ident.Idx = @bitCast(node.data_1);
                return Pattern{ .identifier = ident_idx };
            },
            else => {
                std.log.debug("TODO: implement pattern {}", .{node.tag});
                @panic("unimplemented");
            },
        }
    }
    pub fn getPatternRecordField(store: *NodeStore, patternRecordField: PatternRecordField.Idx) PatternRecordField {
        _ = store;
        _ = patternRecordField;
        @panic("TODO: implement getPatternRecordField");
    }
    pub fn getTypeAnno(store: *NodeStore, typeAnno: TypeAnno.Idx) TypeAnno {
        _ = store;
        _ = typeAnno;
        @panic("TODO: implement getTypeAnno");
    }
    pub fn getAnnoRecordField(store: *NodeStore, annoRecordField: AnnoRecordField.Idx) AnnoRecordField {
        _ = store;
        _ = annoRecordField;
        @panic("TODO: implement getAnnoRecordField");
    }
    pub fn getExposedItem(store: *NodeStore, exposedItem: ExposedItem.Idx) ExposedItem {
        _ = store;
        _ = exposedItem;
        @panic("TODO: implement getExposedItem");
    }

    // Add nodes
    pub fn addStatement(store: *NodeStore, statement: Statement) Statement.Idx {
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

    pub fn addExpr(store: *NodeStore, expr: ExprAtRegion) Expr.Idx {
        var node = Node{
            .data_1 = 0,
            .data_2 = 0,
            .data_3 = 0,
            .region = Region.zero(),
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
            .RuntimeError => |err| {
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

    pub fn addRecordField(store: *NodeStore, recordField: RecordField) RecordField.Idx {
        _ = store;
        _ = recordField;

        return .{ .id = @enumFromInt(0) };
    }

    pub fn addWhenBranch(store: *NodeStore, whenBranch: WhenBranch) WhenBranch.Idx {
        _ = store;
        _ = whenBranch;

        return .{ .id = @enumFromInt(0) };
    }

    pub fn addWhereClause(store: *NodeStore, whereClause: WhereClause) WhereClause.Idx {
        _ = store;
        _ = whereClause;

        return .{ .id = @enumFromInt(0) };
    }

    pub fn addPattern(store: *NodeStore, pattern: Pattern) Pattern.Idx {
        var node = Node{
            .data_1 = 0,
            .data_2 = 0,
            .data_3 = 0,
            .region = Region.zero(),
            .tag = @enumFromInt(0),
        };

        switch (pattern) {
            .identifier => |ident_idx| {
                node.data_1 = @bitCast(ident_idx);
                node.tag = .pattern_identifier;
            },
            // as: struct {
            //     pattern: Pattern.Idx,
            //     region: Region,
            //     ident: Ident.Idx,
            // },
            // applied_tag: struct {
            //     whole_var: TypeVar,
            //     ext_var: TypeVar,
            //     tag_name: Ident.Idx,
            //     arguments: TypedPatternAtRegion.Range,
            // },
            // record_destructure: struct {
            //     whole_var: TypeVar,
            //     ext_var: TypeVar,
            //     destructs: RecordDestruct.Range,
            // },
            // list: struct {
            //     list_var: TypeVar,
            //     elem_var: TypeVar,
            //     patterns: Pattern.Span,
            // },
            // num_literal: struct {
            //     num_var: TypeVar,
            //     literal: StringLiteral.Idx,
            //     value: IntValue,
            //     bound: types.Num,
            // },
            // int_literal: struct {
            //     num_var: TypeVar,
            //     precision_var: TypeVar, // <- can probably be removed
            //     literal: StringLiteral.Idx,
            //     value: IntValue,
            //     bound: types.Num.Int,
            // },
            // float_literal: struct {
            //     num_var: TypeVar,
            //     precision_var: TypeVar, // <- can probably be removed
            //     literal: StringLiteral.Idx,
            //     value: f64,
            //     bound: types.Num.Frac,
            // },
            // str_literal: StringLiteral.Idx,
            // char_literal: struct {
            //     num_var: TypeVar,
            //     precision_var: TypeVar, // <- can probably be removed
            //     value: u32,
            //     bound: types.Num.Int,
            // },
            // Underscore,
            else => {
                std.debug.panic("Pattern of type {s} not yet implemented in Can\n", .{@tagName(pattern)});
            },
        }

        return @enumFromInt(@intFromEnum(store.nodes.append(store.gpa, node)));
    }

    pub fn addPatternRecordField(store: *NodeStore, patternRecordField: PatternRecordField) PatternRecordField.Idx {
        _ = store;
        _ = patternRecordField;

        return .{ .id = @enumFromInt(0) };
    }

    pub fn addTypeAnno(store: *NodeStore, typeAnno: TypeAnno) TypeAnno.Idx {
        const node = Node{};

        switch (typeAnno) {
            else => {
                std.debug.panic("Type Annotation of type {s} not yet implemented in Can\n", .{@tagName(typeAnno)});
            },
        }

        const nid = store.nodes.append(store.gpa, node);
        return @enumFromInt(nid);
    }

    pub fn addAnnoRecordField(store: *NodeStore, annoRecordField: AnnoRecordField) AnnoRecordField.Idx {
        _ = store;
        _ = annoRecordField;

        return @enumFromInt(0);
    }

    pub fn addExposedItem(store: *NodeStore, exposedItem: ExposedItem) ExposedItem.Idx {
        const node = Node{};

        switch (exposedItem) {
            else => {
                std.debug.panic("Exposed Item of type {s} not yet implemented in Can\n", .{@tagName(exposedItem)});
            },
        }

        const nid = store.nodes.append(store.gpa, node);
        return @enumFromInt(nid);
    }

    pub fn addDef(store: *NodeStore, def: Def) Def.Idx {
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

    pub fn getDef(store: *NodeStore, def_idx: Def.Idx) Def {
        const nid: Node.Idx = @enumFromInt(@intFromEnum(def_idx));
        const node = store.nodes.get(nid);

        std.debug.assert(node.tag == .def);

        const extra_start = node.data_1;
        const extra_data = store.extra_data.items[extra_start..];

        const pattern: Pattern.Idx = @enumFromInt(extra_data[0]);
        const expr: Expr.Idx = @enumFromInt(extra_data[1]);
        const expr_var: TypeVar = @enumFromInt(extra_data[2]);
        const kind_tag = extra_data[3];
        const anno_idx = extra_data[4];

        const kind: Def.Kind = switch (kind_tag) {
            @intFromEnum(Def.Kind.Let) => .Let,
            else => .{ .Stmt = @enumFromInt(0) }, // TODO: implement proper kind deserialization
        };

        const annotation: ?Annotation.Idx = if (anno_idx == 0) null else @enumFromInt(anno_idx);

        return Def{
            .pattern = pattern,
            .pattern_region = node.region, // Stored as node region
            .expr = expr,
            .expr_region = Region.zero(), // TODO store and retrieve expr region
            .expr_var = expr_var,
            .annotation = annotation,
            .kind = kind,
        };
    }

    pub fn getRecordField(store: *NodeStore, recordField: RecordField.Idx) RecordField {
        _ = store;
        _ = recordField;

        return RecordField{};
    }

    pub fn sliceFromSpan(store: *NodeStore, comptime T: type, span: anytype) []T {
        return store.extra_data.items[span.start..][0..span.len];
    }

    pub fn scratchExprTop(store: *NodeStore) u32 {
        return store.scratch_exprs.top();
    }

    pub fn addScratchExpr(store: *NodeStore, idx: Expr.Idx) void {
        store.scratch_exprs.append(store.gpa, idx);
    }

    pub fn exprSpanFrom(store: *NodeStore, start: u32) Expr.Span {
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

    pub fn clearScratchExprsFrom(store: *NodeStore, start: u32) void {
        store.scratch_exprs.clearFrom(start);
    }

    pub fn exprSlice(store: *const NodeStore, span: Expr.Span) []Expr.Idx {
        const slice = store.extra_data.items[span.span.start..(span.span.start + span.span.len)];
        const result: []Expr.Idx = @ptrCast(@alignCast(slice));
        return result;
    }

    pub fn scratchDefTop(store: *NodeStore) u32 {
        return store.scratch_defs.top();
    }

    pub fn addScratchDef(store: *NodeStore, idx: Def.Idx) void {
        store.scratch_defs.append(store.gpa, idx);
    }

    pub fn defSpanFrom(store: *NodeStore, start: u32) Def.Span {
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

    pub fn clearScratchDefsFrom(store: *NodeStore, start: u32) void {
        store.scratch_defs.clearFrom(start);
    }

    pub fn sliceDefs(store: *const NodeStore, span: Def.Span) []Def.Idx {
        const slice = store.extra_data.items[span.span.start..(span.span.start + span.span.len)];
        const result: []Def.Idx = @ptrCast(@alignCast(slice));
        return result;
    }

    /// Returns a slice of `Pattern.Idx`
    pub fn slicePatterns(store: *NodeStore, span: Pattern.Span) []Pattern.Idx {
        return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
    }
};

/// A single statement - either at the top-level or within a block.
pub const Statement = union(enum) {
    decl: Decl,
    @"var": Var,
    crash: Crash,
    expr: ExprStmt,
    expect: Expect,
    @"for": For,
    @"return": Return,
    import: Import,
    type_decl: TypeDecl,
    type_anno: Statement.TypeAnno,

    /// A simple immutable declaration
    pub const Decl = struct {
        pattern: Pattern.Idx,
        expr: Expr.Idx,
    };
    /// A rebindable declaration using the "var" keyword
    /// Not valid at the top level of a module
    pub const Var = struct {
        ident: Ident.Idx,
        expr: Expr.Idx,
    };
    /// The "crash" keyword
    /// Not valid at the top level of a module
    pub const Crash = struct {
        msg: Expr.Idx,
    };
    /// Just an expression - usually the return value for a block
    /// Not valid at the top level of a module
    pub const ExprStmt = struct {
        expr: Expr.Idx,
        region: Region,
    };
    /// An expression that will cause a panic (or some other error handling mechanism) if it evaluates to false
    pub const Expect = struct {
        body: Expr.Idx,
        region: Region,
    };
    /// A block of code that will be ran multiple times for each item in a list.
    /// Not valid at the top level of a module
    pub const For = struct {
        patt: Pattern.Idx,
        expr: Expr.Idx,
        body: Expr.Idx,
        region: Region,
    };
    /// A early return of the enclosing function.
    /// Not valid at the top level of a module
    pub const Return = struct {
        expr: Expr.Idx,
        region: Region,
    };
    /// Brings in another module for use in the current module, optionally exposing only certain members of that module.
    /// Only valid at the top level of a module
    pub const Import = struct {
        module_name_tok: Ident.Idx,
        qualifier_tok: ?Ident.Idx,
        alias_tok: ?Ident.Idx,
        exposes: ExposedItem.Span,
        region: Region,
    };
    /// A declaration of a new type - whether an alias or a new nominal custom type
    /// Only valid at the top level of a module
    pub const TypeDecl = struct {
        header: TypeHeader.Idx,
        anno: IR.TypeAnno.Idx,
        where: ?WhereClause.Span,
        region: Region,
    };
    /// A type annotation, declaring that the value referred to by an ident in the same scope should be a given type.
    pub const TypeAnno = struct {
        name: Ident.Idx,
        anno: IR.TypeAnno.Idx,
        where: ?WhereClause.Span,
        region: Region,
    };

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// A working representation of a record field
pub const RecordField = struct {
    name: Ident.Idx,
    value: Expr.Idx,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// TODO: implement WhereClause
const WhereClause = union(enum) {
    alias: WhereClause.Alias,
    method: Method,
    mod_method: ModuleMethod,

    pub const Alias = struct {
        var_tok: Ident.Idx,
        alias_tok: Ident.Idx,
        region: Region,
    };
    pub const Method = struct {
        var_tok: Ident.Idx,
        name_tok: Ident.Idx,
        args: TypeAnno.Span,
        ret_anno: TypeAnno.Idx,
        region: Region,
    };
    pub const ModuleMethod = struct {
        var_tok: Ident.Idx,
        name_tok: Ident.Idx,
        args: TypeAnno.Span,
        ret_anno: TypeAnno.Span,
        region: Region,
    };

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// TODO: implement PatternRecordField
const PatternRecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// TODO: implement TypeAnno
const TypeAnno = union(enum) {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// TODO: implement TypeHeader
const TypeHeader = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// TODO: implement AnnoRecordField
const AnnoRecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// TODO: implement ExposedItem
const ExposedItem = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// Type variables that have been explicitly named, e.g. `a` in `items : List a`.
pub const RigidVariables = struct {
    named: std.AutoHashMap(TypeVar, Ident.Idx),
    // with_methods: std.AutoHashMap(TypeVar, WithMethods),

    // pub const WithMethods = struct {
    //     name: Ident.Idx,
    //     methods: MethodSet,
    // };
};

// TODO: don't use symbol in this module, no imports really exist yet?
/// An expression that has been canonicalized.
pub const Expr = union(enum) {
    // Literals

    // Num stores the `a` variable in `Num a`. Not the same as the variable
    // stored in Int and Float below, which is strictly for better error messages
    num: struct {
        num_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.Num.Compact,
    },

    // Int and Float store a variable to generate better error messages
    int: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.Num.Compact.Int.Precision,
    },
    float: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: f64,
        bound: types.Num.Compact.Frac.Precision,
    },
    str: StringLiteral.Idx,
    // Number variable, precision variable, value, bound
    single_quote: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        value: u32,
        bound: types.Num.Compact.Int.Precision,
    },
    lookup: Lookup,
    list: struct {
        elem_var: TypeVar,
        elems: Expr.Span,
    },

    when: When.Idx,
    @"if": struct {
        cond_var: TypeVar,
        branch_var: TypeVar,
        branches: IfBranch.Span,
        final_else: Expr.Idx,
    },

    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    call: struct {
        // TODO:
        // Box<(Variable, Loc<Expr>, Variable, Variable)>,
        args: Expr.Span,
        // called_via: base.CalledVia,
    },

    // Closure: ClosureData,

    // Product Types
    record: struct {
        record_var: TypeVar,
        // TODO:
        // fields: SendMap<Lowercase, Field>,
    },

    /// Empty record constant
    empty_record,

    /// Look up exactly one field on a record, e.g. (expr).foo.
    record_access: struct {
        record_var: TypeVar,
        ext_var: TypeVar,
        field_var: TypeVar,
        loc_expr: Expr.Idx,
        field: Ident.Idx,
    },

    // Sum Types
    tag: struct {
        tag_union_var: TypeVar,
        ext_var: TypeVar,
        name: Ident.Idx,
        args: Expr.Span,
    },

    zero_argument_tag: struct {
        closure_name: Ident.Idx,
        variant_var: TypeVar,
        ext_var: TypeVar,
        name: Ident.Idx,
    },

    /// Compiles, but will crash if reached
    RuntimeError: Problem.Idx,

    const Lookup = struct {
        ident: Ident.Idx,
    };

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const IR, line_starts: std.ArrayList(u32)) sexpr.Expr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .num => |num_expr| {
                var num_node = sexpr.Expr.init(gpa, "num");

                // Add num_var
                var num_var_node = sexpr.Expr.init(gpa, "num_var");
                const num_var_str = num_expr.num_var.allocPrint(gpa);
                defer gpa.free(num_var_str);
                num_var_node.appendStringChild(gpa, num_var_str);
                num_node.appendNodeChild(gpa, &num_var_node);

                // Add literal
                var literal_node = sexpr.Expr.init(gpa, "literal");
                const literal_str = ir.env.strings.get(num_expr.literal);
                literal_node.appendStringChild(gpa, literal_str);
                num_node.appendNodeChild(gpa, &literal_node);

                // Add value info
                var value_node = sexpr.Expr.init(gpa, "value");
                // TODO: Format the actual integer value properly
                value_node.appendStringChild(gpa, "TODO");
                num_node.appendNodeChild(gpa, &value_node);

                // Add bound info
                var bound_node = sexpr.Expr.init(gpa, "bound");
                bound_node.appendStringChild(gpa, @tagName(num_expr.bound));
                num_node.appendNodeChild(gpa, &bound_node);

                return num_node;
            },
            .int => |int_expr| {
                var int_node = sexpr.Expr.init(gpa, "int");

                // Add num_var
                var num_var_node = sexpr.Expr.init(gpa, "num_var");
                const num_var_str = int_expr.num_var.allocPrint(gpa);
                defer gpa.free(num_var_str);
                num_var_node.appendStringChild(gpa, num_var_str);
                int_node.appendNodeChild(gpa, &num_var_node);

                // Add precision_var
                var prec_var_node = sexpr.Expr.init(gpa, "precision_var");
                const prec_var_str = int_expr.precision_var.allocPrint(gpa);
                defer gpa.free(prec_var_str);
                prec_var_node.appendStringChild(gpa, prec_var_str);
                int_node.appendNodeChild(gpa, &prec_var_node);

                // Add literal
                var literal_node = sexpr.Expr.init(gpa, "literal");
                const literal_str = ir.env.strings.get(int_expr.literal);
                literal_node.appendStringChild(gpa, literal_str);
                int_node.appendNodeChild(gpa, &literal_node);

                // Add value info
                var value_node = sexpr.Expr.init(gpa, "value");
                value_node.appendStringChild(gpa, "TODO");
                int_node.appendNodeChild(gpa, &value_node);

                // Add bound info
                var bound_node = sexpr.Expr.init(gpa, "bound");
                bound_node.appendStringChild(gpa, @tagName(int_expr.bound));
                int_node.appendNodeChild(gpa, &bound_node);

                return int_node;
            },
            .float => |float_expr| {
                var float_node = sexpr.Expr.init(gpa, "float");

                // Add num_var
                var num_var_node = sexpr.Expr.init(gpa, "num_var");
                const num_var_str = float_expr.num_var.allocPrint(gpa);
                defer gpa.free(num_var_str);
                num_var_node.appendStringChild(gpa, num_var_str);
                float_node.appendNodeChild(gpa, &num_var_node);

                // Add precision_var
                var prec_var_node = sexpr.Expr.init(gpa, "precision_var");
                const prec_var_str = float_expr.precision_var.allocPrint(gpa);
                defer gpa.free(prec_var_str);
                prec_var_node.appendStringChild(gpa, prec_var_str);
                float_node.appendNodeChild(gpa, &prec_var_node);

                // Add literal
                var literal_node = sexpr.Expr.init(gpa, "literal");
                const literal = ir.env.strings.get(float_expr.literal);
                literal_node.appendStringChild(gpa, literal);
                float_node.appendNodeChild(gpa, &literal_node);

                // Add value
                var value_node = sexpr.Expr.init(gpa, "value");
                const value_str = std.fmt.allocPrint(gpa, "{d}", .{float_expr.value}) catch |err| exitOnOom(err);
                defer gpa.free(value_str);
                value_node.appendStringChild(gpa, value_str);
                float_node.appendNodeChild(gpa, &value_node);

                // Add bound info
                var bound_node = sexpr.Expr.init(gpa, "bound");
                bound_node.appendStringChild(gpa, @tagName(float_expr.bound));
                float_node.appendNodeChild(gpa, &bound_node);

                return float_node;
            },
            .str => |str_idx| {
                const value = ir.env.strings.get(str_idx);
                var str_node = sexpr.Expr.init(gpa, "str");
                str_node.appendStringChild(gpa, value);
                return str_node;
            },
            .single_quote => |e| {
                var single_quote_node = sexpr.Expr.init(gpa, "single_quote");

                // Add num_var
                var num_var_node = sexpr.Expr.init(gpa, "num_var");
                const num_var_str = e.num_var.allocPrint(gpa);
                defer gpa.free(num_var_str);
                num_var_node.appendStringChild(gpa, num_var_str);
                single_quote_node.appendNodeChild(gpa, &num_var_node);

                // Add precision_var
                var prec_var_node = sexpr.Expr.init(gpa, "precision_var");
                const prec_var_str = e.precision_var.allocPrint(gpa);
                defer gpa.free(prec_var_str);
                prec_var_node.appendStringChild(gpa, prec_var_str);
                single_quote_node.appendNodeChild(gpa, &prec_var_node);

                // Add value
                var value_node = sexpr.Expr.init(gpa, "value");
                const value_str = std.fmt.allocPrint(gpa, "'\\u{{{x}}}'", .{e.value}) catch |err| exitOnOom(err);
                defer gpa.free(value_str);
                value_node.appendStringChild(gpa, value_str);
                single_quote_node.appendNodeChild(gpa, &value_node);

                // Add bound info
                var bound_node = sexpr.Expr.init(gpa, "bound");
                bound_node.appendStringChild(gpa, @tagName(e.bound));
                single_quote_node.appendNodeChild(gpa, &bound_node);

                return single_quote_node;
            },
            .list => |l| {
                var list_node = sexpr.Expr.init(gpa, "list");

                // Add elem_var
                var elem_var_node = sexpr.Expr.init(gpa, "elem_var");
                const elem_var_str = l.elem_var.allocPrint(gpa);
                defer gpa.free(elem_var_str);
                elem_var_node.appendStringChild(gpa, elem_var_str);
                list_node.appendNodeChild(gpa, &elem_var_node);

                // TODO print list elems
                // implement proper span access when collection is available
                var elems_node = sexpr.Expr.init(gpa, "elems");
                elems_node.appendStringChild(gpa, "TODO each element");
                list_node.appendNodeChild(gpa, &elems_node);

                return list_node;
            },
            .lookup => |l| {
                var lookup_node = sexpr.Expr.init(gpa, "lookup");

                var ident_node = sexpr.Expr.init(gpa, "ident");
                const ident_str = ir.env.idents.getText(l.ident);
                ident_node.appendStringChild(gpa, ident_str);
                lookup_node.appendNodeChild(gpa, &ident_node);

                return lookup_node;
            },
            .when => |_| {
                var when_branch_node = sexpr.Expr.init(gpa, "when");
                when_branch_node.appendStringChild(gpa, "TODO when branch");

                return when_branch_node;
            },
            .@"if" => |if_expr| {
                var if_node = sexpr.Expr.init(gpa, "if");

                // Add cond_var
                var cond_var_node = sexpr.Expr.init(gpa, "cond_var");
                const cond_var_str = if_expr.cond_var.allocPrint(gpa);
                defer gpa.free(cond_var_str);
                cond_var_node.appendStringChild(gpa, cond_var_str);
                if_node.appendNodeChild(gpa, &cond_var_node);

                // Add branch_var
                var branch_var_node = sexpr.Expr.init(gpa, "branch_var");
                const branch_var_str = if_expr.branch_var.allocPrint(gpa);
                defer gpa.free(branch_var_str);
                branch_var_node.appendStringChild(gpa, branch_var_str);
                if_node.appendNodeChild(gpa, &branch_var_node);

                // Add branches
                var branches_node = sexpr.Expr.init(gpa, "branches");
                // TODO: Implement proper branch access when collection is available
                // for (ir.if_branches.rangeToSlice(i.branches).items(.cond), ir.if_branches.rangeToSlice(i.branches).items(.body)) |cond, body| {
                //     var cond_node = cond.toSExpr(env, ir);
                //     var body_node = body.toSExpr(env, ir);
                //     var branch_node = sexpr.Expr.init(gpa, "branch");
                //     branch_node.appendNodeChild(gpa, &cond_node);
                //     branch_node.appendNodeChild(gpa, &body_node);
                //     branches_node.appendNodeChild(gpa, &branch_node);
                // }
                // node.appendNodeChild(gpa, &branches_node);

                // var else_node = sexpr.Expr.init(gpa, "else");
                // const final_else_expr = ir.exprs_at_regions.get(i.final_else);
                // var else_sexpr = final_else_expr.toSExpr(env, ir);
                // else_node.appendNodeChild(gpa, &else_sexpr);
                // node.appendNodeChild(gpa, &else_node);
                branches_node.appendStringChild(gpa, "TODO: access if branches");
                if_node.appendNodeChild(gpa, &branches_node);

                // Add final_else
                var else_node = sexpr.Expr.init(gpa, "else");
                // TODO: Implement proper final_else access
                else_node.appendStringChild(gpa, "TODO: access final else");
                if_node.appendNodeChild(gpa, &else_node);

                return if_node;
            },
            .call => |c| {
                var call_node = sexpr.Expr.init(gpa, "call");

                // Get all expressions from the args span
                const all_exprs = ir.store.exprSlice(c.args);

                // First element is the function being called
                if (all_exprs.len > 0) {
                    const fn_expr = ir.store.getExpr(all_exprs[0]);
                    var fn_node = fn_expr.toSExpr(ir, line_starts);
                    call_node.appendNodeChild(gpa, &fn_node);
                }

                // Remaining elements are the arguments
                if (all_exprs.len > 1) {
                    for (all_exprs[1..]) |arg_idx| {
                        const arg_expr = ir.store.getExpr(arg_idx);
                        var arg_node = arg_expr.toSExpr(ir, line_starts);
                        call_node.appendNodeChild(gpa, &arg_node);
                    }
                }

                return call_node;
            },
            .record => |record_expr| {
                var record_node = sexpr.Expr.init(gpa, "record");

                // Add record_var
                var record_var_node = sexpr.Expr.init(gpa, "record_var");
                const record_var_str = record_expr.record_var.allocPrint(gpa);
                defer gpa.free(record_var_str);
                record_var_node.appendStringChild(gpa, record_var_str);
                record_node.appendNodeChild(gpa, &record_var_node);

                // TODO: Add fields when implemented
                var fields_node = sexpr.Expr.init(gpa, "fields");
                fields_node.appendStringChild(gpa, "TODO");
                record_node.appendNodeChild(gpa, &fields_node);

                return record_node;
            },
            .empty_record => {
                return sexpr.Expr.init(gpa, "empty_record");
            },
            .record_access => |access_expr| {
                var access_node = sexpr.Expr.init(gpa, "record_access");

                // Add record_var
                var record_var_node = sexpr.Expr.init(gpa, "record_var");
                const record_var_str = access_expr.record_var.allocPrint(gpa);
                defer gpa.free(record_var_str);
                record_var_node.appendStringChild(gpa, record_var_str);
                access_node.appendNodeChild(gpa, &record_var_node);

                // Add ext_var
                var ext_var_node = sexpr.Expr.init(gpa, "ext_var");
                const ext_var_str = access_expr.ext_var.allocPrint(gpa);
                defer gpa.free(ext_var_str);
                ext_var_node.appendStringChild(gpa, ext_var_str);
                access_node.appendNodeChild(gpa, &ext_var_node);

                // Add field_var
                var field_var_node = sexpr.Expr.init(gpa, "field_var");
                const field_var_str = access_expr.field_var.allocPrint(gpa);
                defer gpa.free(field_var_str);
                field_var_node.appendStringChild(gpa, field_var_str);
                access_node.appendNodeChild(gpa, &field_var_node);

                // Add loc_expr
                var loc_expr = ir.store.getExpr(access_expr.loc_expr);
                var loc_expr_node = loc_expr.toSExpr(ir, line_starts);
                access_node.appendNodeChild(gpa, &loc_expr_node);

                // Add field
                var field_node = sexpr.Expr.init(gpa, "field");
                const field_str = ir.env.idents.getText(access_expr.field);
                field_node.appendStringChild(gpa, field_str);
                access_node.appendNodeChild(gpa, &field_node);

                return access_node;
            },
            .tag => |tag_expr| {
                var tag_node = sexpr.Expr.init(gpa, "tag");

                // Add tag_union_var
                var tag_union_var_node = sexpr.Expr.init(gpa, "tag_union_var");
                const tag_union_var_str = tag_expr.tag_union_var.allocPrint(gpa);
                defer gpa.free(tag_union_var_str);
                tag_union_var_node.appendStringChild(gpa, tag_union_var_str);
                tag_node.appendNodeChild(gpa, &tag_union_var_node);

                // Add ext_var
                var ext_var_node = sexpr.Expr.init(gpa, "ext_var");
                const ext_var_str = tag_expr.ext_var.allocPrint(gpa);
                defer gpa.free(ext_var_str);
                ext_var_node.appendStringChild(gpa, ext_var_str);
                tag_node.appendNodeChild(gpa, &ext_var_node);

                // Add name
                var name_node = sexpr.Expr.init(gpa, "name");
                const name_str = ir.env.idents.getText(tag_expr.name);
                name_node.appendStringChild(gpa, name_str);
                tag_node.appendNodeChild(gpa, &name_node);

                // Add args
                var args_node = sexpr.Expr.init(gpa, "args");
                // const args_slice = ir.typed_exprs_at_regions.rangeToSlice(tag_expr.args);
                args_node.appendStringChild(gpa, "TODO");
                tag_node.appendNodeChild(gpa, &args_node);

                return tag_node;
            },
            .zero_argument_tag => |tag_expr| {
                var tag_node = sexpr.Expr.init(gpa, "zero_argument_tag");

                // Add closure_name
                var closure_name_node = sexpr.Expr.init(gpa, "closure_name");
                const closure_name_str = ir.env.idents.getText(tag_expr.closure_name);
                closure_name_node.appendStringChild(gpa, closure_name_str);
                tag_node.appendNodeChild(gpa, &closure_name_node);

                // Add variant_var
                var variant_var_node = sexpr.Expr.init(gpa, "variant_var");
                const variant_var_str = tag_expr.variant_var.allocPrint(gpa);
                defer gpa.free(variant_var_str);
                variant_var_node.appendStringChild(gpa, variant_var_str);
                tag_node.appendNodeChild(gpa, &variant_var_node);

                // Add ext_var
                var ext_var_node = sexpr.Expr.init(gpa, "ext_var");
                const ext_var_str = tag_expr.ext_var.allocPrint(gpa);
                defer gpa.free(ext_var_str);
                ext_var_node.appendStringChild(gpa, ext_var_str);
                tag_node.appendNodeChild(gpa, &ext_var_node);

                // Add name
                var name_node = sexpr.Expr.init(gpa, "name");
                const name_str = ir.env.idents.getText(tag_expr.name);
                name_node.appendStringChild(gpa, name_str);
                tag_node.appendNodeChild(gpa, &name_node);

                return tag_node;
            },
            .RuntimeError => |problem_idx| {
                var runtime_err_node = sexpr.Expr.init(gpa, "runtime_error");

                const p = ir.env.problems.get(problem_idx);

                var buf = std.ArrayList(u8).init(gpa);
                defer buf.deinit();

                p.toStr(gpa, "", buf.writer()) catch |err| {
                    // This definitely isn't clean... fix me
                    // using our oom helper doesn't work here because
                    // the error set is different
                    std.debug.print("Error: {}\n", .{err});
                };

                runtime_err_node.appendStringChild(gpa, buf.items);

                return runtime_err_node;
            },
        }
    }
};

/// A file of any type that has been ingested into a Roc module
/// as raw data, e.g. `import "lookups.txt" as lookups : Str`.
///
/// These ingestions aren't resolved until the import resolution
/// compiler stage.
pub const IngestedFile = struct {
    relative_path: StringLiteral.Idx,
    ident: Ident.Idx,
    type: Annotation,

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Range = List.Range;
    pub const NonEmptyRange = List.NonEmptyRange;

    pub fn toSExpr(self: *const @This(), ir: *const IR, line_starts: std.ArrayList(u32)) sexpr.Expr {
        _ = line_starts;
        const gpa = ir.env.gpa;
        var node = sexpr.Expr.init(gpa, "ingested_file");
        node.appendStringChild(gpa, "path"); // TODO: use self.relative_path
        appendIdentChild(&node, gpa, ir.env, "ident", self.ident);
        var type_node = self.type.toSExpr(ir);
        node.appendNodeChild(gpa, &type_node);
        return node;
    }
};

/// A definition of a value (or destructured values) that
/// takes its value from an expression.
pub const Def = struct {
    pattern: Pattern.Idx,
    pattern_region: Region,
    expr: Expr.Idx,
    expr_region: Region,
    expr_var: TypeVar,
    // TODO:
    // pattern_vars: SendMap<Symbol, Variable>,
    annotation: ?Annotation.Idx,
    kind: Kind,

    const Kind = union(enum) {
        /// A def that introduces identifiers
        Let,
        /// A standalone statement with an fx variable
        Stmt: TypeVar,
        /// Ignored result, must be effectful
        Ignored: TypeVar,

        pub fn toSExpr(self: *const @This(), gpa: std.mem.Allocator) sexpr.Expr {
            switch (self.*) {
                .Let => return sexpr.Expr.init(gpa, "Let"),
                .Stmt => {
                    const node = sexpr.Expr.init(gpa, "Stmt");
                    return node;
                },
                .Ignored => {
                    const node = sexpr.Expr.init(gpa, "Ignored");
                    return node;
                },
            }
        }
    };

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
    pub const Range = struct { start: u32, len: u32 };

    pub fn toSExpr(self: *const @This(), ir: *IR, line_starts: std.ArrayList(u32), source: []const u8) sexpr.Expr {
        const gpa = ir.env.gpa;
        var node = sexpr.Expr.init(gpa, "def");

        var kind_node = self.kind.toSExpr(gpa);
        node.appendNodeChild(gpa, &kind_node);

        var pattern_node = sexpr.Expr.init(gpa, "pattern");
        pattern_node.appendRegionChild(gpa, regionInfo(source, self.pattern_region, line_starts));

        const pattern = ir.store.getPattern(self.pattern);
        var pattern_sexpr = pattern.toSExpr(ir, line_starts, source);
        pattern_node.appendNodeChild(gpa, &pattern_sexpr);
        node.appendNodeChild(gpa, &pattern_node);

        var expr_node = sexpr.Expr.init(gpa, "expr");
        expr_node.appendRegionChild(gpa, regionInfo(source, self.expr_region, line_starts));

        const expr = ir.store.getExpr(self.expr);
        var expr_sexpr = expr.toSExpr(ir, line_starts);
        expr_node.appendNodeChild(gpa, &expr_sexpr);
        node.appendNodeChild(gpa, &expr_node);

        const expr_var = self.expr_var.allocPrint(gpa);
        defer gpa.free(expr_var);
        node.appendStringChild(gpa, expr_var);

        if (self.annotation) |anno_idx| {
            _ = anno_idx; // TODO: implement annotation lookup
            // var anno_node = anno.toSExpr(env, ir);
            // node.appendNodeChild(env.gpa, &anno_node);
        }

        return node;
    }
};

/// todo
pub const Annotation = struct {
    signature: TypeVar,
    // introduced_variables: IntroducedVariables,
    // aliases: VecMap<Symbol, Alias>,
    region: Region,

    pub const Idx = enum(u32) { _ };

    pub fn toSExpr(self: *const @This(), ir: *const IR, line_starts: std.ArrayList(u32)) sexpr.Expr {
        _ = self;
        _ = line_starts;
        const gpa = ir.env.gpa;
        const node = sexpr.Expr.init(gpa, "annotation");
        // TODO add signature info
        return node;
    }
};

/// todo
pub const IntValue = struct {
    bytes: [16]u8,
    kind: Kind,

    /// todo
    pub const Kind = enum { i128, u128 };
};

/// todo
pub const ExprAtRegion = struct {
    expr: Expr,
    region: Region,

    pub fn toSExpr(self: *const @This(), ir: *const IR, line_starts: std.ArrayList(u32)) sexpr.Expr {
        const gpa = ir.env.gpa;
        var node = sexpr.Expr.init(gpa, "expr_at_region");

        node.appendRegionChild(gpa, self.region);

        const expr = ir.store.getExpr(self.expr);
        var expr_sexpr = expr.toSExpr(ir, line_starts);

        node.appendNodeChild(gpa, &expr_sexpr);

        return expr.toSExpr(ir, line_starts);
    }
};

/// todo
pub const TypedExprAtRegion = struct {
    expr: Expr.Idx,
    type_var: TypeVar,
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const IR, line_starts: std.ArrayList(u32), source: []const u8) sexpr.Expr {
        const gpa = ir.env.gpa;

        var typed_expr_node = sexpr.Expr.init(gpa, "typed_expr_at_region");
        typed_expr_node.appendRegionChild(gpa, regionInfo(source, self.region, line_starts));

        const expr = ir.store.getExpr(self.expr);
        typed_expr_node.appendNodeChild(gpa, &expr.toSExpr(ir));

        return typed_expr_node;
    }
};

/// todo
pub const Function = struct {
    return_var: TypeVar,
    fx_var: TypeVar,
    function_var: TypeVar,
    expr: Expr.Idx,
    region: Region,

    // TODO: Add toSExpr if needed, might be part of Expr.Closure?
};

/// todo - evaluate if we need this?
pub const IfBranch = struct {
    cond: ExprAtRegion,
    body: ExprAtRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    // Note: toSExpr is handled within Expr.if because the slice reference is there
};

/// todo - evaluate if we need this?
pub const When = struct {
    /// The actual condition of the when expression.
    loc_cond: Expr.Idx,
    cond_var: TypeVar,
    /// Type of each branch (and therefore the type of the entire `when` expression)
    expr_var: TypeVar,
    region: Region,
    /// The branches of the when, and the type of the condition that they expect to be matched
    /// against.
    branches: WhenBranch.Span,
    branches_cond_var: TypeVar,
    /// Whether the branches are exhaustive.
    exhaustive: ExhaustiveMark,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const IR, line_starts: std.ArrayList(u32)) sexpr.Expr {
        const gpa = ir.env.gpa;
        var node = sexpr.Expr.init(gpa, "when");

        node.appendRegionChild(gpa, self.region);

        var cond_node = sexpr.Expr.init(gpa, "cond");
        const cond_expr = ir.store.getExpr(self.loc_cond);
        var cond_sexpr = cond_expr.toSExpr(ir, line_starts);
        cond_node.appendNodeChild(gpa, &cond_sexpr);

        node.appendNodeChild(gpa, &cond_node);

        appendTypeVarChild(&node, gpa, "cond_var", self.cond_var);
        appendTypeVarChild(&node, gpa, "expr_var", self.expr_var);
        appendTypeVarChild(&node, gpa, "branches_cond_var", self.branches_cond_var);
        appendTypeVarChild(&node, gpa, "exhaustive_mark", self.exhaustive);

        var branches_node = sexpr.Expr.init(gpa, "branches");
        for (ir.store.whenBranchSlice(self.branches)) |branch_idx| {
            const branch = ir.store.getWhenBranch(branch_idx);

            var branch_sexpr = branch.toSExpr(ir);
            branches_node.appendNodeChild(gpa, &branch_sexpr);
        }
        node.appendNodeChild(gpa, &branches_node);

        return node;
    }
};

/// todo - evaluate if we need this?
pub const WhenBranchPattern = struct {
    pattern: PatternAtRegion,
    /// Degenerate branch patterns are those that don't fully bind symbols that the branch body
    /// needs. For example, in `A x | B y -> x`, the `B y` pattern is degenerate.
    /// Degenerate patterns emit a runtime error if reached in a program.
    degenerate: bool,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const IR, line_starts: std.ArrayList(u32)) sexpr.Expr {
        _ = line_starts;
        const gpa = ir.gpa;
        var node = sexpr.Expr.init(gpa, "when_branch_pattern");
        var pattern_sexpr = self.pattern.toSExpr(ir);
        node.appendNodeChild(gpa, &pattern_sexpr);
        if (self.degenerate) {
            node.appendStringChild(gpa, "degenerate=true");
        }
        return node;
    }
};

/// todo - evaluate if we need this?
pub const WhenBranch = struct {
    patterns: WhenBranchPattern.Span,
    value: Expr.Idx,
    guard: ?Expr.Idx,
    /// Whether this branch is redundant in the `when` it appears in
    redundant: RedundantMark,

    pub fn toSExpr(self: *const @This(), ir: *const IR, line_starts: std.ArrayList(u32)) sexpr.Expr {
        const gpa = ir.env.gpa;
        var node = sexpr.Expr.init(gpa, "when_branch");

        var patterns_node = sexpr.Expr.init(gpa, "patterns");
        // Need WhenBranchPattern.List storage in IR to resolve slice
        // Assuming `ir.when_branch_patterns` exists:
        // for (ir.when_branch_patterns.getSlice(self.patterns)) |patt| {
        //     var patt_sexpr = patt.toSExpr(env, ir);
        //     patterns_node.appendNodeChild(gpa, &patt_sexpr);
        // }
        patterns_node.appendStringChild(gpa, "TODO: Store and represent WhenBranchPattern slice");
        node.appendNodeChild(gpa, &patterns_node);

        var value_node = sexpr.Expr.init(gpa, "value");
        const value_expr = ir.exprs_at_regions.get(self.value);
        var value_sexpr = value_expr.toSExpr(ir, line_starts);
        value_node.appendNodeChild(gpa, &value_sexpr);
        node.appendNodeChild(gpa, &value_node);

        if (self.guard) |guard_idx| {
            var guard_node = sexpr.Expr.init(gpa, "guard");
            const guard_expr = ir.exprs_at_regions.get(guard_idx);
            var guard_sexpr = guard_expr.toSExpr(ir, line_starts);
            guard_node.appendNodeChild(gpa, &guard_sexpr);
            node.appendNodeChild(gpa, &guard_node);
        }

        return node;
    }

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
pub const Pattern = union(enum) {
    identifier: Ident.Idx,
    as: struct {
        pattern: Pattern.Idx,
        region: Region,
        ident: Ident.Idx,
    },
    applied_tag: struct {
        whole_var: TypeVar,
        ext_var: TypeVar,
        tag_name: Ident.Idx,
        arguments: TypedPatternAtRegion.Span,
    },
    record_destructure: struct {
        whole_var: TypeVar,
        ext_var: TypeVar,
        destructs: RecordDestruct.Span,
    },
    list: struct {
        list_var: TypeVar,
        elem_var: TypeVar,
        patterns: Pattern.Span,
    },
    num_literal: struct {
        num_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.Num.Compact,
    },
    int_literal: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.Num.Compact.Int.Precision,
    },
    float_literal: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: f64,
        bound: types.Num.Compact.Frac.Precision,
    },
    str_literal: StringLiteral.Idx,
    char_literal: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        value: u32,
        bound: types.Num.Compact.Int.Precision,
    },
    Underscore,

    // TODO: do we want these runtime exceptions here?
    // // Runtime Exceptions
    // Shadowed(Region, Loc<Ident>, Symbol),
    // OpaqueNotInScope(Loc<Ident>),
    // // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    // UnsupportedPattern(Region),
    // parse error patterns
    // MalformedPattern: .{ MalformedPatternProblem, Region },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *IR, line_starts: std.ArrayList(u32), source: []const u8) sexpr.Expr {
        const gpa = ir.env.gpa;
        switch (self.*) {
            .identifier => |ident_idx| {
                var node = sexpr.Expr.init(gpa, "pattern_ident");
                appendIdentChild(&node, gpa, ir, "ident", ident_idx);
                return node;
            },
            .as => |a| {
                var node = sexpr.Expr.init(gpa, "pattern_as");
                node.appendRegionChild(gpa, regionInfo(source, a.region, line_starts));
                appendIdentChild(&node, gpa, ir, "ident", a.ident);
                var inner_patt_node = sexpr.Expr.init(gpa, "pattern");
                const inner_patt = ir.store.getPattern(a.pattern);
                var inner_patt_sexpr = inner_patt.toSExpr(ir, line_starts, source);
                inner_patt_node.appendNodeChild(gpa, &inner_patt_sexpr);
                node.appendNodeChild(gpa, &inner_patt_node);
                return node;
            },
            .applied_tag => |_| {
                @panic("TODO pattern applied_tag");
                // var node = sexpr.Expr.init(gpa, "pattern_applied_tag");
                // appendIdentChild(&node, gpa, ir, "tag_name", t.tag_name);
                // var args_node = sexpr.Expr.init(gpa, "arguments");
                // ir.store.pa
                // for (ir.patterns_at_regions.rangeToSlice(t.arguments).items(.pattern), ir.typed_patterns_at_regions.rangeToSlice(t.arguments).items(.type_var)) |arg, type_var| {
                //     var arg_sexpr = ir.patterns.get(arg).toSExpr(ir);
                //     var pat_ty_var = sexpr.Expr.init(gpa, "argty");
                //     pat_ty_var.appendNodeChild(gpa, &arg_sexpr);
                //     pat_ty_var.appendUnsignedIntChild(gpa, @intFromEnum(type_var)); // TODO: use a type var name or something
                //     args_node.appendNodeChild(gpa, &pat_ty_var);
                // }
                // node.appendNodeChild(gpa, &args_node);
                // return node;
            },
            .record_destructure => {
                var node = sexpr.Expr.init(gpa, "pattern_record_destructure");
                var destructs_node = sexpr.Expr.init(gpa, "destructs");
                // Need RecordDestruct storage in IR
                // Assuming ir.record_destructs exists:
                // for (ir.record_destructs.getSlice(r.destructs)) |destruct| {
                //     var d_sexpr = destruct.toSExpr(env, ir);
                //     destructs_node.appendNodeChild(gpa, &d_sexpr);
                // }
                destructs_node.appendStringChild(gpa, "TODO: Store and represent RecordDestruct slice");
                node.appendNodeChild(gpa, &destructs_node);
                return node;
            },
            .list => |l| {
                var pattern_list_node = sexpr.Expr.init(gpa, "pattern_list");
                var patterns_node = sexpr.Expr.init(gpa, "patterns");

                for (ir.store.slicePatterns(l.patterns)) |patt_idx| {
                    const patt = ir.store.getPattern(patt_idx);
                    var patt_sexpr = patt.toSExpr(ir, line_starts, source);
                    patterns_node.appendNodeChild(gpa, &patt_sexpr);
                }

                pattern_list_node.appendNodeChild(gpa, &patterns_node);

                return pattern_list_node;
            },
            .num_literal => |l| {
                var node = sexpr.Expr.init(gpa, "pattern_num");
                node.appendStringChild(gpa, "literal"); // TODO: use l.literal
                node.appendStringChild(gpa, "value=<int_value>");
                node.appendStringChild(gpa, @tagName(l.bound));
                return node;
            },
            .int_literal => |l| {
                var node = sexpr.Expr.init(gpa, "pattern_int");
                node.appendStringChild(gpa, "literal"); // TODO: use l.literal
                node.appendStringChild(gpa, "value=<int_value>");
                node.appendStringChild(gpa, @tagName(l.bound));
                return node;
            },
            .float_literal => |l| {
                var node = sexpr.Expr.init(gpa, "pattern_float");
                node.appendStringChild(gpa, "literal"); // TODO: use l.literal
                const val_str = std.fmt.allocPrint(gpa, "{d}", .{l.value}) catch "<oom>";
                defer gpa.free(val_str);
                node.appendStringChild(gpa, val_str);
                node.appendStringChild(gpa, @tagName(l.bound));
                return node;
            },
            .str_literal => |str_idx| {
                _ = str_idx; // str_idx not used currently, but keep for signature consistency
                var node = sexpr.Expr.init(gpa, "pattern_str");
                node.appendStringChild(gpa, "value"); // TODO: use str_idx
                return node;
            },
            .char_literal => |l| {
                var node = sexpr.Expr.init(gpa, "pattern_char");
                const char_str = std.fmt.allocPrint(gpa, "'\\u({d})'", .{l.value}) catch "<oom>";
                defer gpa.free(char_str);
                node.appendStringChild(gpa, char_str);
                node.appendStringChild(gpa, @tagName(l.bound));
                return node;
            },
            .Underscore => return sexpr.Expr.init(gpa, "pattern_underscore"),
        }
    }
};

/// todo - evaluate if we need this?
pub const PatternAtRegion = struct {
    pattern: Pattern.Idx,
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const IR, line_starts: std.ArrayList(u32)) sexpr.Expr {
        const gpa = ir.env.gpa;

        var pattern_node = sexpr.Expr.init(gpa, "pattern_at_region");
        pattern_node.appendRegionChild(gpa, self.region);

        const pattern = ir.patterns.get(self.pattern);
        var pattern_sexpr = pattern.toSExpr(ir, line_starts);
        pattern_node.appendNodeChild(gpa, &pattern_sexpr);

        return pattern_node;
    }
};

/// todo - evaluate if we need this?
pub const TypedPatternAtRegion = struct {
    pattern: Pattern.Idx,
    region: Region,
    type_var: TypeVar,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: *const @This(), ir: *const IR, line_starts: std.ArrayList(u32), source: []const u8) sexpr.Expr {
        const gpa = ir.env.gpa;

        var type_pattern_node = sexpr.Expr.init(gpa, "typed_pattern_at_region");
        type_pattern_node.appendRegionChild(gpa, self.region);

        const pattern = ir.patterns.get(self.pattern);
        var pattern_sexpr = pattern.toSExpr(ir, line_starts, source);
        type_pattern_node.appendNodeChild(gpa, &pattern_sexpr);

        return type_pattern_node;
    }
};

/// todo
pub const RecordDestruct = struct {
    type_var: TypeVar,
    region: Region,
    label: Ident.Idx,
    ident: Ident.Idx,
    kind: Kind,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    /// todo
    pub const Kind = union(enum) {
        Required,
        Guard: TypedPatternAtRegion.Idx,

        pub fn toSExpr(self: *const @This(), ir: *const IR, line_starts: std.ArrayList(u32)) sexpr.Expr {
            const gpa = ir.env.gpa;

            switch (self.*) {
                .Required => return sexpr.Expr.init(gpa, "required"),
                .Guard => |guard_idx| {
                    var guard_kind_node = sexpr.Expr.init(gpa, "guard");

                    const guard_patt = ir.typed_patterns_at_regions.get(guard_idx);
                    var guard_sexpr = guard_patt.toSExpr(ir.env, ir, line_starts);
                    guard_kind_node.appendNodeChild(gpa, &guard_sexpr);

                    return guard_kind_node;
                },
            }
        }
    };

    pub fn toSExpr(self: *const @This(), ir: *const IR, line_starts: std.ArrayList(u32), source: []const u8) sexpr.Expr {
        const gpa = ir.env.gpa;

        var record_destruct_node = sexpr.Expr.init(gpa, "record_destruct");

        record_destruct_node.appendTypeVarChild(&record_destruct_node, gpa, "type_var", self.type_var);
        record_destruct_node.appendRegionChild(gpa, regionInfo(source, self.region, line_starts));

        appendIdentChild(&record_destruct_node, gpa, ir, "label", self.label);
        appendIdentChild(&record_destruct_node, gpa, ir, "ident", self.ident);

        var kind_node = self.kind.toSExpr(ir);
        record_destruct_node.appendNodeChild(gpa, &kind_node);

        return record_destruct_node;
    }
};

/// Marks whether a when branch is redundant using a variable.
pub const RedundantMark = TypeVar;

/// Marks whether a when expression is exhaustive using a variable.
pub const ExhaustiveMark = TypeVar;

/// Helper function to convert the entire Canonical IR to a string in S-expression format
/// and write it to the given writer.
///
/// If a single expression is provided we only print that expression
pub fn toSExprStr(ir: *IR, writer: std.io.AnyWriter, maybe_expr_idx: ?Expr.Idx, line_starts: std.ArrayList(u32), source: []const u8) !void {
    const gpa = ir.env.gpa;

    if (maybe_expr_idx) |expr_idx| {
        // Get the expression from the store
        const expr = ir.store.getExpr(expr_idx);

        var expr_node = expr.toSExpr(ir, line_starts);
        defer expr_node.deinit(gpa);

        expr_node.toStringPretty(writer);
    } else {
        var root_node = sexpr.Expr.init(gpa, "can_ir");
        defer root_node.deinit(gpa);

        var defs_node = sexpr.Expr.init(gpa, "top_level_defs");

        // Iterate over each top-level definition and convert it to an S-expression
        const defs_slice = ir.store.sliceDefs(ir.top_level_defs);

        if (defs_slice.len == 0) {
            defs_node.appendStringChild(gpa, "empty");
        }

        for (defs_slice) |def_idx| {
            const d = ir.store.getDef(def_idx);
            var def_node = d.toSExpr(ir, line_starts, source);
            defs_node.appendNodeChild(gpa, &def_node);
        }

        root_node.appendNodeChild(gpa, &defs_node);

        root_node.toStringPretty(writer);
    }
}

/// todo - evaluate if we need this?
/// I think Types are now implemented in `src/types.zig` etc...
pub const Content = union(enum) {
    /// A type variable which the user did not name in an annotation,
    ///
    /// When we auto-generate a type var name, e.g. the "a" in (a -> a), we
    /// change the Option in here from None to Some.
    FlexVar: ?Ident.Idx,
    /// name given in a user-written annotation
    RigidVar: Ident.Idx,
    /// name given to a recursion variable
    RecursionVar: struct {
        structure: TypeVar,
        opt_name: ?Ident.Idx,
    },
    Structure: FlatType,
    Alias: struct {
        ident: Ident.Idx,
        // vars: AliasVariables,
        type_var: TypeVar,
        kind: Alias.Kind,
    },
    RangedNumber: types.num.NumericRange,
    Error,
    /// The fx type variable for a given function
    Pure,
    Effectful,
};

/// todo - evaluate if we need this?
/// I think Types are now implemented in `src/types.zig` etc...
pub const FlatType = union(enum) {
    Apply: struct {
        ident: Ident.Idx,
        vars: collections.SafeList(TypeVar).Range,
    },
    Func: struct {
        arg_vars: collections.SafeList(TypeVar).Range,
        ret_var: TypeVar,
        fx: TypeVar,
    },
    /// A function that we know nothing about yet except that it's effectful
    EffectfulFunc,
    Record: struct {
        whole_var: TypeVar,
        fields: FlatType.RecordField.Range,
    },
    // TagUnion: struct {
    //     union_tags: UnionTags,
    //     ext: TagExt,
    // },

    // /// `A` might either be a function
    // ///   x -> A x : a -> [A a, B a, C a]
    // /// or a tag `[A, B, C]`
    // FunctionOrTagUnion: struct {
    //     name: Ident.Idx,
    //     ident: Ident.Idx,
    //     ext: TagExt,
    // },

    // RecursiveTagUnion: struct {
    //     type_var: TypeVar,
    //     union_tags: UnionTags,
    //     ext: TagExt,
    // },

    EmptyRecord,
    EmptyTagUnion,

    /// todo
    pub const RecordField = struct {
        name: Ident.Idx,
        type_var: TypeVar,

        // pub const List = collections.SafeMultiList(@This());
        // pub const Range = List.Range;
        pub const Idx = enum(u32) { _ };
    };
};

/// todo
pub const TagExt = union(enum) {
    /// This tag extension variable measures polymorphism in the openness of the tag,
    /// or the lack thereof. It can only be unified with
    ///   - an empty tag union, or
    ///   - a rigid extension variable
    ///
    /// Openness extensions are used when tag annotations are introduced, since tag union
    /// annotations may contain hidden extension variables which we want to reflect openness,
    /// but not growth in the monomorphic size of the tag. For example, openness extensions enable
    /// catching
    ///
    /// ```ignore
    /// f : [A]
    /// f = if Bool.true then A else B
    /// ```
    ///
    /// as an error rather than resolving as [A][B].
    Openness: TypeVar,
    /// This tag extension can grow unboundedly.
    Any: TypeVar,
};

test "NodeStore - init and deinit" {
    var store = IR.NodeStore.init(testing.allocator);
    defer store.deinit();

    try testing.expect(store.nodes.len() == 0);
    try testing.expect(store.extra_data.items.len == 0);
}

/// Returns diagnostic position information for the given region.
/// This is a standalone utility function that takes the source text as a parameter
/// to avoid storing it in the cacheable IR structure.
pub fn regionInfo(source: []const u8, region: Region, line_starts: std.ArrayList(u32)) base.DiagnosticPosition {
    // In the Can IR, regions store byte offsets directly, not token indices.
    // We can use these offsets directly to calculate the diagnostic position.
    const info = base.DiagnosticPosition.position(source, line_starts, region.start.offset, region.end.offset) catch {
        // Return a zero position if we can't calculate it
        return .{
            .start_line_idx = 0,
            .start_col_idx = 0,
            .end_line_idx = 0,
            .end_col_idx = 0,
            .line_text = "",
        };
    };

    return info;
}
