const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const problem = @import("../../problem.zig");
const collections = @import("../../collections.zig");
const Alias = @import("./Alias.zig");
const sexpr = @import("../../base/sexpr.zig");
const exitOnOom = @import("../../collections/utils.zig").exitOnOom;

const Ident = base.Ident;
const Region = base.Region;
const ModuleImport = base.ModuleImport;
const StringLiteral = base.StringLiteral;
const TypeVar = types.Var;
const Problem = problem.Problem;
const Self = @This();

env: *base.ModuleEnv,
store: NodeStore,
ingested_files: IngestedFile.List,
imports: ModuleImport.List,

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
pub fn init(env: *base.ModuleEnv) Self {
    return Self{
        .env = env,
        .store = NodeStore.init(env.gpa),
        .ingested_files = .{},
        .imports = .{},
    };
}

/// Deinit the IR's memory.
pub fn deinit(self: *Self) void {
    self.store.deinit();
    // self.type_var_names.deinit(self.env.gpa);
    self.ingested_files.deinit(self.env.gpa);
    self.imports.deinit(self.env.gpa);
}

// Helper to add type index info
fn appendTypeVarChild(node: *sexpr.Expr, gpa: std.mem.Allocator, name: []const u8, type_idx: TypeVar) void {
    var type_node = sexpr.Expr.init(gpa, name);
    type_node.appendUnsignedIntChild(gpa, @intCast(@intFromEnum(type_idx)));
    node.appendNodeChild(gpa, &type_node);
}

// Helper to add identifier info
fn appendIdentChild(node: *sexpr.Expr, gpa: std.mem.Allocator, env: *const base.ModuleEnv, name: []const u8, ident_idx: Ident.Idx) void {
    var ident_node = sexpr.Expr.init(gpa, name);
    const ident_text = env.idents.getText(ident_idx);
    ident_node.appendStringChild(gpa, ident_text);
    node.appendNodeChild(gpa, &ident_node);
}

test {
    const testing = std.testing;
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
    };
};

/// todo: describe NodeStore
pub const NodeStore = struct {
    gpa: std.mem.Allocator,
    nodes: Node.List,
    extra_data: std.ArrayListUnmanaged(u32),
    scratch_statements: Scratch(Statement.Idx),
    scratch_exprs: Scratch(Expr.Idx),
    // todo do we need this?
    //scratch_record_fields: Scratch(RecordField.Idx),
    scratch_when_branches: Scratch(WhenBranch.Idx),
    scratch_where_clauses: Scratch(WhereClause.Idx),
    scratch_patterns: Scratch(Pattern.Idx),
    scratch_pattern_record_fields: Scratch(PatternRecordField.Idx),
    scratch_type_annos: Scratch(TypeAnno.Idx),
    scratch_anno_record_fields: Scratch(AnnoRecordField.Idx),
    scratch_exposed_items: Scratch(ExposedItem.Idx),

    pub fn init(gpa: std.mem.Allocator) NodeStore {
        return initCapacity(gpa, 128);
    }

    pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) NodeStore {
        const scratch_90th_percentile_capacity = std.math.ceilPowerOfTwoAssert(usize, 64);
        return .{
            .gpa = gpa,
            .nodes = Node.List.initCapacity(gpa, capacity),
            .extra_data = std.ArrayListUnmanaged(u32).initCapacity(gpa, capacity / 2) catch |err| exitOnOom(err),
            .scratch_statements = Scratch(Statement.Idx).init(gpa, scratch_90th_percentile_capacity),
            .scratch_exprs = Scratch(Expr.Idx).init(gpa, scratch_90th_percentile_capacity),
            .scratch_patterns = Scratch(Pattern.Idx).init(gpa, scratch_90th_percentile_capacity),
            .scratch_pattern_record_fields = Scratch(PatternRecordField.Idx).init(gpa, scratch_90th_percentile_capacity),
            .scratch_when_branches = Scratch(WhenBranch.Idx).init(gpa, scratch_90th_percentile_capacity),
            .scratch_type_annos = Scratch(TypeAnno.Idx).init(gpa, scratch_90th_percentile_capacity),
            .scratch_anno_record_fields = Scratch(AnnoRecordField.Idx).init(gpa, scratch_90th_percentile_capacity),
            .scratch_exposed_items = Scratch(ExposedItem.Idx).init(gpa, scratch_90th_percentile_capacity),
            .scratch_where_clauses = Scratch(WhereClause.Idx).init(gpa, scratch_90th_percentile_capacity),
        };
    }

    pub fn deinit(store: *NodeStore) void {
        store.nodes.deinit(store.gpa);
        store.extra_data.deinit(store.gpa);
        store.scratch_statements.items.deinit(store.gpa);
        store.scratch_exprs.items.deinit(store.gpa);
        store.scratch_patterns.items.deinit(store.gpa);
        store.scratch_pattern_record_fields.items.deinit(store.gpa);
        store.scratch_when_branches.items.deinit(store.gpa);
        store.scratch_type_annos.items.deinit(store.gpa);
        store.scratch_anno_record_fields.items.deinit(store.gpa);
        store.scratch_exposed_items.items.deinit(store.gpa);
        store.scratch_where_clauses.items.deinit(store.gpa);
    }

    pub fn addStatement(store: *NodeStore, statement: Statement) Statement.Idx {
        _ = store;
        _ = statement;
        @panic("TODO: implement addStatement");
    }
    pub fn addExpr(store: *NodeStore, expr: Expr) Expr.Idx {
        var node = Node{
            .data_1 = 0,
            .data_2 = 0,
            .data_3 = 0,
            .region = .{ .start = .{ .offset = 0 }, .end = .{ .offset = 0 } },
            .tag = .expr_var, // default, will be overridden
        };
        
        switch (expr) {
            .@"var" => |v| {
                node.tag = .expr_var;
                node.data_1 = @bitCast(v.ident);
            },
            else => @panic("TODO: implement other expr variants"),
        }
        
        const node_idx = store.nodes.append(store.gpa, node);
        return @enumFromInt(@intFromEnum(node_idx));
    }

    pub fn addWhenBranch(store: *NodeStore, whenBranch: WhenBranch) WhenBranch.Idx {
        _ = store;
        _ = whenBranch;
        @panic("TODO: implement addWhenBranch");
    }
    pub fn addWhereClause(store: *NodeStore, whereClause: WhereClause) WhereClause.Idx {
        _ = store;
        _ = whereClause;
        @panic("TODO: implement addWhereClause");
    }
    pub fn addPattern(store: *NodeStore, pattern: Pattern) Pattern.Idx {
        _ = store;
        _ = pattern;
        @panic("TODO: implement addPattern");
    }
    pub fn addPatternRecordField(store: *NodeStore, patternRecordField: PatternRecordField) PatternRecordField.Idx {
        _ = store;
        _ = patternRecordField;
        @panic("TODO: implement addPatternRecordField");
    }
    pub fn addTypeAnno(store: *NodeStore, typeAnno: TypeAnno) TypeAnno.Idx {
        _ = store;
        _ = typeAnno;
        @panic("TODO: implement addTypeAnno");
    }
    pub fn addAnnoRecordField(store: *NodeStore, annoRecordField: AnnoRecordField) AnnoRecordField.Idx {
        _ = store;
        _ = annoRecordField;
        @panic("TODO: implement addAnnoRecordField");
    }
    pub fn addExposedItem(store: *NodeStore, exposedItem: ExposedItem) ExposedItem.Idx {
        _ = store;
        _ = exposedItem;
        @panic("TODO: implement addExposedItem");
    }

    pub fn getStatement(store: *NodeStore, statement: Statement.Idx) Statement {
        _ = store;
        _ = statement;
        @panic("TODO: implement getStatement");
    }
    pub fn getExpr(store: *NodeStore, expr: Expr.Idx) Expr {
        const node_idx: Node.Idx = @enumFromInt(@intFromEnum(expr));
        const node = store.nodes.get(node_idx);
        
        switch (node.tag) {
            .expr_var => {
                return .{ .@"var" = .{
                    .ident = @bitCast(node.data_1),
                } };
            },
            else => @panic("TODO: implement other expr variants"),
        }
    }

    pub fn getWhenBranch(store: *NodeStore, whenBranch: WhenBranch.Idx) WhenBranch {
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
    pub fn getPattern(store: *NodeStore, pattern: Pattern.Idx) Pattern {
        _ = store;
        _ = pattern;
        @panic("TODO: implement getPattern");
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

    pub const DataSpan = struct {
        start: u32,
        len: u32,
    };

    pub const ExprSpan = struct { span: DataSpan };
    pub const StatementSpan = struct { span: DataSpan };
    pub const PatternSpan = struct { span: DataSpan };
    pub const PatternRecordFieldSpan = struct { span: DataSpan };
    pub const WhenBranchSpan = struct { span: DataSpan };
    pub const TypeAnnoSpan = struct { span: DataSpan };
    pub const AnnoRecordFieldSpan = struct { span: DataSpan };
    pub const ExposedItemSpan = struct { span: DataSpan };
    pub const WhereClauseSpan = struct { span: DataSpan };

    pub fn sliceFromSpan(store: *NodeStore, comptime T: type, span: anytype) []T {
        return store.extra_data.items[span.start..][0..span.len];
    }

    pub fn Scratch(comptime T: type) type {
        return struct {
            items: std.ArrayListUnmanaged(T),

            fn init(gpa: std.mem.Allocator, capacity: usize) @This() {
                return .{
                    .items = std.ArrayListUnmanaged(T).initCapacity(gpa, capacity) catch |err| exitOnOom(err),
                };
            }

            const ScratchType = @This();

            /// Returns the start position for a new Span of items in scratch
            pub fn top(self: *ScratchType) u32 {
                return @as(u32, @intCast(self.items.items.len));
            }

            /// Places a new item in the scratch. Will panic on OOM.
            pub fn append(self: *ScratchType, gpa: std.mem.Allocator, item: T) void {
                self.items.append(gpa, item) catch |err| exitOnOom(err);
            }

            /// Creates a new span starting at start.  Moves the items from scratch
            /// to extra_data as appropriate.
            pub fn spanFromStart(self: *ScratchType, start: u32, gpa: std.mem.Allocator, data: *std.ArrayListUnmanaged(u32), comptime SpanType: type) SpanType {
                const end = self.items.items.len;
                defer self.items.shrinkRetainingCapacity(start);
                var i = @as(usize, @intCast(start));
                const data_start = @as(u32, @intCast(data.items.len));
                while (i < end) {
                    data.append(gpa, @intFromEnum(self.items.items[i])) catch |err| exitOnOom(err);
                    i += 1;
                }
                return .{ .span = .{ .start = data_start, .len = @as(u32, @intCast(end)) - start } };
            }

            /// Clears any items added to scratch from start until the end.
            /// Should be used wherever the scratch items will not be used,
            /// as in when parsing fails.
            pub fn clearFrom(self: *ScratchType, start: u32) void {
                self.items.shrinkRetainingCapacity(start);
            }
        };
    }
};

/// Todo: describe Statement
pub const Statement = union(enum) {
    decl: Decl,

    pub const Decl = struct {};
    pub const Idx = enum(u32) { _ };
};

/// TODO: implement WhereClause
pub const WhereClause = struct {
    pub const Idx = enum(u32) { _ };
};

/// TODO: implement PatternRecordField
pub const PatternRecordField = struct {
    pub const Idx = enum(u32) { _ };
};

/// TODO: implement TypeAnno
pub const TypeAnno = struct {
    pub const Idx = enum(u32) { _ };
};

/// TODO: implement AnnoRecordField
pub const AnnoRecordField = struct {
    pub const Idx = enum(u32) { _ };
};

/// TODO: implement ExposedItem
pub const ExposedItem = struct {
    pub const Idx = enum(u32) { _ };
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
        bound: types.Num,
    },

    // Int and Float store a variable to generate better error messages
    int: struct {
        num_var: TypeVar,
        precision_var: TypeVar, // <- can probably be removed
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.Num.Int,
    },
    float: struct {
        num_var: TypeVar,
        precision_var: TypeVar, // <- can probably be removed
        literal: StringLiteral.Idx,
        value: f64,
        bound: types.Num.Frac,
    },
    str: StringLiteral.Idx,
    // Number variable, precision variable, value, bound
    single_quote: struct {
        num_var: TypeVar,
        precision_var: TypeVar, // <- can probably be removed
        value: u32,
        bound: types.Num.Int,
    },
    list: struct {
        elem_var: TypeVar,
        elems: ExprAtRegion.Range,
    },

    @"var": struct {
        ident: Ident.Idx,
    },

    when: When.Idx,
    @"if": struct {
        cond_var: TypeVar,
        branch_var: TypeVar,
        branches: IfBranch.Range,
        final_else: ExprAtRegion.Idx,
    },

    let: struct {
        defs: Def.Range,
        cont: ExprAtRegion.Idx,
        // cycle_mark: IllegalCycleMark,
    },

    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    call: struct {
        // TODO:
        // Box<(Variable, Loc<Expr>, Variable, Variable)>,
        args: TypedExprAtRegion.Range,
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

    /// The "crash" keyword
    crash: struct {
        msg: ExprAtRegion.Idx,
        ret_var: TypeVar,
    },

    /// Look up exactly one field on a record, e.g. (expr).foo.
    record_access: struct {
        record_var: TypeVar,
        ext_var: TypeVar,
        field_var: TypeVar,
        loc_expr: ExprAtRegion.Idx,
        field: Ident.Idx,
    },

    // Sum Types
    tag: struct {
        tag_union_var: TypeVar,
        ext_var: TypeVar,
        name: Ident.Idx,
        args: TypedExprAtRegion.Range,
    },

    zero_argument_tag: struct {
        closure_name: Ident.Idx,
        variant_var: TypeVar,
        ext_var: TypeVar,
        name: Ident.Idx,
    },

    /// Compiles, but will crash if reached
    RuntimeError: Problem.Idx,

    pub const Idx = enum(u32) { _ };

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        switch (self.*) {
            .num => |e| {
                var node = sexpr.Expr.init(gpa, "num");
                node.appendStringChild(gpa, "literal"); // TODO: use e.literal
                // TODO: Represent IntValue better
                node.appendStringChild(gpa, "value=<int_value>");
                appendTypeVarChild(&node, gpa, "num_var", e.num_var);
                node.appendStringChild(gpa, @tagName(e.bound));
                return node;
            },
            .int => |e| {
                var node = sexpr.Expr.init(gpa, "int");
                node.appendStringChild(gpa, "literal"); // TODO: use e.literal
                // TODO: Represent IntValue better
                node.appendStringChild(gpa, "value=<int_value>");
                appendTypeVarChild(&node, gpa, "num_var", e.num_var);
                appendTypeVarChild(&node, gpa, "precision_var", e.precision_var);
                node.appendStringChild(gpa, @tagName(e.bound));
                return node;
            },
            .float => |e| {
                var node = sexpr.Expr.init(gpa, "float");
                node.appendStringChild(gpa, "literal"); // TODO: use e.literal
                const val_str = std.fmt.allocPrint(gpa, "{d}", .{e.value}) catch "<oom>";
                defer gpa.free(val_str);
                node.appendStringChild(gpa, val_str);
                appendTypeVarChild(&node, gpa, "num_var", e.num_var);
                appendTypeVarChild(&node, gpa, "precision_var", e.precision_var);
                node.appendStringChild(gpa, @tagName(e.bound));
                return node;
            },
            .str => |str_idx| {
                _ = str_idx; // str_idx not used currently, but keep for signature consistency
                var node = sexpr.Expr.init(gpa, "str");
                node.appendStringChild(gpa, "value"); // TODO: use str_idx
                return node;
            },
            .single_quote => |e| {
                var node = sexpr.Expr.init(gpa, "single_quote");
                // TODO: Format char value
                const char_str = std.fmt.allocPrint(gpa, "'\\u({d})'", .{e.value}) catch "<oom>";
                defer gpa.free(char_str);
                node.appendStringChild(gpa, char_str);
                appendTypeVarChild(&node, gpa, "num_var", e.num_var);
                appendTypeVarChild(&node, gpa, "precision_var", e.precision_var);
                node.appendStringChild(gpa, @tagName(e.bound));
                return node;
            },
            .list => |l| {
                var node = sexpr.Expr.init(gpa, "list");
                appendTypeVarChild(&node, gpa, "elem_var", l.elem_var);
                var elems_node = sexpr.Expr.init(gpa, "elems");
                for (ir.exprs_at_regions.rangeToSlice(l.elems).items(.expr)) |elem| {
                    var elem_sexpr = ir.exprs.get(elem).toSExpr(env, ir);
                    elems_node.appendNodeChild(gpa, &elem_sexpr);
                }
                node.appendNodeChild(gpa, &elems_node);
                return node;
            },
            .@"var" => |v| {
                var node = sexpr.Expr.init(gpa, "var");
                appendIdentChild(&node, gpa, env, "ident", v.ident);
                return node;
            },
            .when => |when_idx| {
                var node = sexpr.Expr.init(gpa, "when");
                const when_data = ir.when_branches.get(when_idx); // Assuming when stores When data
                var when_sexpr = when_data.toSExpr(env, ir);
                node.appendNodeChild(gpa, &when_sexpr);
                return node;
            },
            .@"if" => |i| {
                var node = sexpr.Expr.init(gpa, "if");
                appendTypeVarChild(&node, gpa, "cond_var", i.cond_var);
                appendTypeVarChild(&node, gpa, "branch_var", i.branch_var);

                var branches_node = sexpr.Expr.init(gpa, "branches");
                for (ir.if_branches.rangeToSlice(i.branches).items(.cond), ir.if_branches.rangeToSlice(i.branches).items(.body)) |cond, body| {
                    var cond_node = cond.toSExpr(env, ir);
                    var body_node = body.toSExpr(env, ir);
                    var branch_node = sexpr.Expr.init(gpa, "branch");
                    branch_node.appendNodeChild(gpa, &cond_node);
                    branch_node.appendNodeChild(gpa, &body_node);
                    branches_node.appendNodeChild(gpa, &branch_node);
                }
                node.appendNodeChild(gpa, &branches_node);

                var else_node = sexpr.Expr.init(gpa, "else");
                const final_else_expr = ir.exprs_at_regions.get(i.final_else);
                var else_sexpr = final_else_expr.toSExpr(env, ir);
                else_node.appendNodeChild(gpa, &else_sexpr);
                node.appendNodeChild(gpa, &else_node);

                return node;
            },
            .let => |l| {
                var node = sexpr.Expr.init(gpa, "let");
                var defs_node = sexpr.Expr.init(gpa, "defs");
                for (ir.defs.rangeToSlice(l.defs)) |def| {
                    var def_sexpr = def.toSExpr(env, ir);
                    defs_node.appendNodeChild(gpa, &def_sexpr);
                }
                node.appendNodeChild(gpa, &defs_node);

                var cont_node = sexpr.Expr.init(gpa, "cont");
                const cont_expr = ir.exprs_at_regions.get(l.cont);
                var cont_sexpr = cont_expr.toSExpr(env, ir);
                cont_node.appendNodeChild(gpa, &cont_sexpr);
                node.appendNodeChild(gpa, &cont_node);

                return node;
            },
            .call => |c| {
                var node = sexpr.Expr.init(gpa, "call");
                node.appendStringChild(gpa, "fn=<TODO: store and print fn expr>"); // The called expression needs to be stored
                var args_node = sexpr.Expr.init(gpa, "args");
                for (ir.typed_exprs_at_regions.rangeToSlice(c.args).items(.expr), ir.typed_exprs_at_regions.rangeToSlice(c.args).items(.type_var)) |arg, ty_var| {
                    var arg_sexpr = ir.exprs.get(arg).toSExpr(env, ir);
                    var argty_sexpr = sexpr.Expr.init(gpa, "argty");
                    argty_sexpr.appendNodeChild(gpa, &arg_sexpr);
                    argty_sexpr.appendUnsignedIntChild(gpa, @intFromEnum(ty_var)); // TODO: use a type var name or something
                    args_node.appendNodeChild(gpa, &arg_sexpr);
                }
                node.appendNodeChild(gpa, &args_node);
                return node;
            },
            .record => |r| {
                var node = sexpr.Expr.init(gpa, "record");
                appendTypeVarChild(&node, gpa, "record_var", r.record_var);
                node.appendStringChild(gpa, "fields=<TODO: represent fields>");
                return node;
            },
            .empty_record => {
                return sexpr.Expr.init(gpa, "empty_record");
            },
            .crash => |c| {
                var node = sexpr.Expr.init(gpa, "crash");
                var msg_node = sexpr.Expr.init(gpa, "msg");
                const msg_expr = ir.exprs_at_regions.get(c.msg);
                var msg_sexpr = msg_expr.toSExpr(env, ir);
                msg_node.appendNodeChild(gpa, &msg_sexpr);
                node.appendNodeChild(gpa, &msg_node);
                appendTypeVarChild(&node, gpa, "ret_var", c.ret_var);
                return node;
            },
            .record_access => |ra| {
                var node = sexpr.Expr.init(gpa, "record_access");
                var expr_node = sexpr.Expr.init(gpa, "expr");
                const loc_expr = ir.exprs_at_regions.get(ra.loc_expr);
                var expr_sexpr = loc_expr.toSExpr(env, ir);
                expr_node.appendNodeChild(gpa, &expr_sexpr);
                node.appendNodeChild(gpa, &expr_node);

                appendIdentChild(&node, gpa, env, "field", ra.field);
                appendTypeVarChild(&node, gpa, "record_var", ra.record_var);
                appendTypeVarChild(&node, gpa, "ext_var", ra.ext_var);
                appendTypeVarChild(&node, gpa, "field_var", ra.field_var);
                return node;
            },
            .tag => |t| {
                var node = sexpr.Expr.init(gpa, "tag");
                appendIdentChild(&node, gpa, env, "name", t.name);
                appendTypeVarChild(&node, gpa, "tag_union_var", t.tag_union_var);
                appendTypeVarChild(&node, gpa, "ext_var", t.ext_var);
                var args_node = sexpr.Expr.init(gpa, "args");
                for (ir.typed_exprs_at_regions.rangeToSlice(t.args).items(.expr), ir.typed_exprs_at_regions.rangeToSlice(t.args).items(.type_var)) |arg, ty_var| {
                    var arg_sexpr = ir.exprs.get(arg).toSExpr(env, ir);
                    var argty_sexpr = sexpr.Expr.init(gpa, "argty");
                    argty_sexpr.appendNodeChild(gpa, &arg_sexpr);
                    argty_sexpr.appendUnsignedIntChild(gpa, @intFromEnum(ty_var)); // TODO: use a type var name or something
                    args_node.appendNodeChild(gpa, &arg_sexpr);
                }
                node.appendNodeChild(gpa, &args_node);
                return node;
            },
            .zero_argument_tag => |t| {
                var node = sexpr.Expr.init(gpa, "zero_argument_tag");
                appendIdentChild(&node, gpa, env, "name", t.name);
                appendTypeVarChild(&node, gpa, "variant_var", t.variant_var);
                appendTypeVarChild(&node, gpa, "ext_var", t.ext_var);
                return node;
            },
            .RuntimeError => |problem_idx| {
                var node = sexpr.Expr.init(gpa, "runtime_error");
                // TODO: Maybe resolve problem description?
                node.appendUnsignedIntChild(gpa, @intFromEnum(problem_idx));
                return node;
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

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        var node = sexpr.Expr.init(gpa, "ingested_file");
        node.appendStringChild(gpa, "path"); // TODO: use self.relative_path
        appendIdentChild(&node, gpa, env, "ident", self.ident);
        var type_node = self.type.toSExpr(env, ir);
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
    annotation: ?Annotation,
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
                .Stmt => |fx_var| {
                    var node = sexpr.Expr.init(gpa, "Stmt");
                    appendTypeVarChild(&node, gpa, "fx_var", fx_var);
                    return node;
                },
                .Ignored => |fx_var| {
                    var node = sexpr.Expr.init(gpa, "Ignored");
                    appendTypeVarChild(&node, gpa, "fx_var", fx_var);
                    return node;
                },
            }
        }
    };

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Range = List.Range;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "def");

        var kind_node = self.kind.toSExpr(env.gpa);
        node.appendNodeChild(env.gpa, &kind_node);

        var pattern_node = sexpr.Expr.init(env.gpa, "pattern");

        pattern_node.appendRegionChild(env.gpa, self.pattern_region);

        const pattern = ir.patterns.get(self.pattern);
        var pattern_sexpr = pattern.toSExpr(env, ir);
        pattern_node.appendNodeChild(env.gpa, &pattern_sexpr);
        node.appendNodeChild(env.gpa, &pattern_node);

        var expr_node = sexpr.Expr.init(env.gpa, "expr");
        // expr_node.appendRegionChild(env.gpa, self.expr_region);
        const expr = ir.exprs.get(self.expr);
        var expr_sexpr = expr.toSExpr(env, ir);
        expr_node.appendNodeChild(env.gpa, &expr_sexpr);
        node.appendNodeChild(env.gpa, &expr_node);

        appendTypeVarChild(&node, env.gpa, "expr_var", self.expr_var);

        if (self.annotation) |anno| {
            var anno_node = anno.toSExpr(env, ir);
            node.appendNodeChild(env.gpa, &anno_node);
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

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        _ = ir; // ir not needed currently, but keep for signature consistency
        const gpa = env.gpa;
        var node = sexpr.Expr.init(gpa, "annotation");
        // node.appendRegionChild(gpa, self.region);
        appendTypeVarChild(&node, gpa, "signature", self.signature);
        // TODO: Add introduced_variables, aliases if needed
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
    expr: Expr.Idx,
    region: Region,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Range = List.Range;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "expr_at_region");

        node.appendRegionChild(env.gpa, self.region);

        const expr = ir.store.getExpr(self.expr);
        var expr_sexpr = expr.toSExpr(env, ir);

        node.appendNodeChild(env.gpa, &expr_sexpr);

        return expr.toSExpr(env, ir);
    }
};

/// todo
pub const TypedExprAtRegion = struct {
    expr: Expr.Idx,
    type_var: TypeVar,
    region: Region,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Range = List.Range;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "typed_expr_at_region");

        node.appendRegionChild(env.gpa, self.region);

        const expr = ir.store.getExpr(self.expr);
        node.appendNodeChild(env.gpa, &expr.toSExpr(env, ir));

        return node;
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

/// todo
pub const IfBranch = struct {
    cond: ExprAtRegion,
    body: ExprAtRegion,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Range = List.Range;

    // Note: toSExpr is handled within Expr.if because the slice reference is there
};

/// todo
pub const When = struct {
    /// The actual condition of the when expression.
    loc_cond: ExprAtRegion.Idx,
    cond_var: TypeVar,
    /// Type of each branch (and therefore the type of the entire `when` expression)
    expr_var: TypeVar,
    region: Region,
    /// The branches of the when, and the type of the condition that they expect to be matched
    /// against.
    branches: WhenBranch.Range,
    branches_cond_var: TypeVar,
    /// Whether the branches are exhaustive.
    exhaustive: ExhaustiveMark,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        var node = sexpr.Expr.init(env.gpa, "when");

        node.appendRegionChild(env.gpa, self.region);

        var cond_node = sexpr.Expr.init(env.gpa, "cond");
        const cond_expr = ir.store.getExpr(self.loc_cond);
        var cond_sexpr = cond_expr.toSExpr(env, ir);
        cond_node.appendNodeChild(env.gpa, &cond_sexpr);

        node.appendNodeChild(env.gpa, &cond_node);

        appendTypeVarChild(&node, env.gpa, "cond_var", self.cond_var);
        appendTypeVarChild(&node, env.gpa, "expr_var", self.expr_var);
        appendTypeVarChild(&node, env.gpa, "branches_cond_var", self.branches_cond_var);
        appendTypeVarChild(&node, env.gpa, "exhaustive_mark", self.exhaustive);

        var branches_node = sexpr.Expr.init(env.gpa, "branches");
        for (ir.store.whenBranchSlice(self.branches)) |branch_idx| {
            const branch = ir.store.getWhenBranch(branch_idx);
            var branch_sexpr = branch.toSExpr(env, ir);
            branches_node.appendNodeChild(env.gpa, &branch_sexpr);
        }
        node.appendNodeChild(env.gpa, &branches_node);

        return node;
    }
};

/// todo
pub const WhenBranchPattern = struct {
    pattern: PatternAtRegion,
    /// Degenerate branch patterns are those that don't fully bind symbols that the branch body
    /// needs. For example, in `A x | B y -> x`, the `B y` pattern is degenerate.
    /// Degenerate patterns emit a runtime error if reached in a program.
    degenerate: bool,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Range = List.Range;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        var node = sexpr.Expr.init(gpa, "when_branch_pattern");
        var pattern_sexpr = self.pattern.toSExpr(env, ir);
        node.appendNodeChild(gpa, &pattern_sexpr);
        if (self.degenerate) {
            node.appendStringChild(gpa, "degenerate=true");
        }
        return node;
    }
};

/// todo
pub const WhenBranch = struct {
    patterns: WhenBranchPattern.Range,
    value: ExprAtRegion.Idx,
    guard: ?ExprAtRegion.Idx,
    /// Whether this branch is redundant in the `when` it appears in
    redundant: RedundantMark,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Range = List.Range;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
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
        var value_sexpr = value_expr.toSExpr(env, ir);
        value_node.appendNodeChild(gpa, &value_sexpr);
        node.appendNodeChild(gpa, &value_node);

        if (self.guard) |guard_idx| {
            var guard_node = sexpr.Expr.init(gpa, "guard");
            const guard_expr = ir.exprs_at_regions.get(guard_idx);
            var guard_sexpr = guard_expr.toSExpr(env, ir);
            guard_node.appendNodeChild(gpa, &guard_sexpr);
            node.appendNodeChild(gpa, &guard_node);
        }

        appendTypeVarChild(&node, gpa, "redundant_mark", self.redundant);

        return node;
    }
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
        arguments: TypedPatternAtRegion.Range,
    },
    record_destructure: struct {
        whole_var: TypeVar,
        ext_var: TypeVar,
        destructs: RecordDestruct.Range,
    },
    list: struct {
        list_var: TypeVar,
        elem_var: TypeVar,
        patterns: Pattern.Range,
    },
    num_literal: struct {
        num_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.Num,
    },
    int_literal: struct {
        num_var: TypeVar,
        precision_var: TypeVar, // <- can probably be removed
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.Num.Int,
    },
    float_literal: struct {
        num_var: TypeVar,
        precision_var: TypeVar, // <- can probably be removed
        literal: StringLiteral.Idx,
        value: f64,
        bound: types.Num.Frac,
    },
    str_literal: StringLiteral.Idx,
    char_literal: struct {
        num_var: TypeVar,
        precision_var: TypeVar, // <- can probably be removed
        value: u32,
        bound: types.Num.Int,
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

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Range = List.Range;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        switch (self.*) {
            .identifier => |ident_idx| {
                var node = sexpr.Expr.init(gpa, "pattern_ident");
                appendIdentChild(&node, gpa, env, "ident", ident_idx);
                return node;
            },
            .as => |a| {
                var node = sexpr.Expr.init(gpa, "pattern_as");
                // node.appendRegionChild(gpa, a.region);
                appendIdentChild(&node, gpa, env, "ident", a.ident);
                var inner_patt_node = sexpr.Expr.init(gpa, "pattern");
                const inner_patt = ir.patterns.get(a.pattern);
                var inner_patt_sexpr = inner_patt.toSExpr(env, ir);
                inner_patt_node.appendNodeChild(gpa, &inner_patt_sexpr);
                node.appendNodeChild(gpa, &inner_patt_node);
                return node;
            },
            .applied_tag => |t| {
                var node = sexpr.Expr.init(gpa, "pattern_applied_tag");
                appendIdentChild(&node, gpa, env, "tag_name", t.tag_name);
                appendTypeVarChild(&node, gpa, "whole_var", t.whole_var);
                appendTypeVarChild(&node, gpa, "ext_var", t.ext_var);
                var args_node = sexpr.Expr.init(gpa, "arguments");
                for (ir.patterns_at_regions.rangeToSlice(t.arguments).items(.pattern), ir.typed_patterns_at_regions.rangeToSlice(t.arguments).items(.type_var)) |arg, type_var| {
                    var arg_sexpr = ir.patterns.get(arg).toSExpr(env, ir);
                    var pat_ty_var = sexpr.Expr.init(gpa, "argty");
                    pat_ty_var.appendNodeChild(gpa, &arg_sexpr);
                    pat_ty_var.appendUnsignedIntChild(gpa, @intFromEnum(type_var)); // TODO: use a type var name or something
                    args_node.appendNodeChild(gpa, &pat_ty_var);
                }
                node.appendNodeChild(gpa, &args_node);
                return node;
            },
            .record_destructure => |r| {
                var node = sexpr.Expr.init(gpa, "pattern_record_destructure");
                appendTypeVarChild(&node, gpa, "whole_var", r.whole_var);
                appendTypeVarChild(&node, gpa, "ext_var", r.ext_var);
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
                var node = sexpr.Expr.init(gpa, "pattern_list");
                appendTypeVarChild(&node, gpa, "list_var", l.list_var);
                appendTypeVarChild(&node, gpa, "elem_var", l.elem_var);
                var patterns_node = sexpr.Expr.init(gpa, "patterns");
                for (ir.patterns.rangeToSlice(l.patterns)) |patt| {
                    var patt_sexpr = patt.toSExpr(env, ir);
                    patterns_node.appendNodeChild(gpa, &patt_sexpr);
                }
                node.appendNodeChild(gpa, &patterns_node);
                return node;
            },
            .num_literal => |l| {
                var node = sexpr.Expr.init(gpa, "pattern_num");
                node.appendStringChild(gpa, "literal"); // TODO: use l.literal
                node.appendStringChild(gpa, "value=<int_value>");
                appendTypeVarChild(&node, gpa, "num_var", l.num_var);
                node.appendStringChild(gpa, @tagName(l.bound));
                return node;
            },
            .int_literal => |l| {
                var node = sexpr.Expr.init(gpa, "pattern_int");
                node.appendStringChild(gpa, "literal"); // TODO: use l.literal
                node.appendStringChild(gpa, "value=<int_value>");
                appendTypeVarChild(&node, gpa, "num_var", l.num_var);
                appendTypeVarChild(&node, gpa, "precision_var", l.precision_var);
                node.appendStringChild(gpa, @tagName(l.bound));
                return node;
            },
            .float_literal => |l| {
                var node = sexpr.Expr.init(gpa, "pattern_float");
                node.appendStringChild(gpa, "literal"); // TODO: use l.literal
                const val_str = std.fmt.allocPrint(gpa, "{d}", .{l.value}) catch "<oom>";
                defer gpa.free(val_str);
                node.appendStringChild(gpa, val_str);
                appendTypeVarChild(&node, gpa, "num_var", l.num_var);
                appendTypeVarChild(&node, gpa, "precision_var", l.precision_var);
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
                appendTypeVarChild(&node, gpa, "num_var", l.num_var);
                appendTypeVarChild(&node, gpa, "precision_var", l.precision_var);
                node.appendStringChild(gpa, @tagName(l.bound));
                return node;
            },
            .Underscore => return sexpr.Expr.init(gpa, "pattern_underscore"),
        }
    }
};

/// todo
pub const PatternAtRegion = struct {
    pattern: Pattern.Idx,
    region: Region,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Range = List.Range;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        var node = sexpr.Expr.init(gpa, "pattern_at_region");
        // node.appendRegionChild(gpa, self.region);
        const pattern = ir.patterns.get(self.pattern);
        var pattern_sexpr = pattern.toSExpr(env, ir);
        node.appendNodeChild(gpa, &pattern_sexpr);
        return node;
    }
};

/// todo
pub const TypedPatternAtRegion = struct {
    pattern: Pattern.Idx,
    region: Region,
    type_var: TypeVar,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Range = List.Range;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        var node = sexpr.Expr.init(gpa, "typed_pattern_at_region");
        // node.appendRegionChild(gpa, self.region);
        appendTypeVarChild(&node, gpa, "type_var", self.type_var);
        const pattern = ir.patterns.get(self.pattern);
        var pattern_sexpr = pattern.toSExpr(env, ir);
        node.appendNodeChild(gpa, &pattern_sexpr);
        return node;
    }
};

/// todo
pub const RecordDestruct = struct {
    type_var: TypeVar,
    region: Region,
    label: Ident.Idx,
    ident: Ident.Idx,
    kind: Kind,

    /// todo
    pub const Kind = union(enum) {
        Required,
        Guard: TypedPatternAtRegion.Idx,

        pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
            const gpa = env.gpa;
            switch (self.*) {
                .Required => return sexpr.Expr.init(gpa, "Required"),
                .Guard => |guard_idx| {
                    var node = sexpr.Expr.init(gpa, "Guard");
                    const guard_patt = ir.typed_patterns_at_regions.get(guard_idx);
                    var guard_sexpr = guard_patt.toSExpr(env, ir);
                    node.appendNodeChild(gpa, &guard_sexpr);
                    return node;
                },
            }
        }
    };

    pub const List = collections.SafeMultiList(@This());
    pub const Range = List.Range;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        var node = sexpr.Expr.init(gpa, "record_destruct");

        node.appendRegionChild(gpa, self.region);

        appendIdentChild(&node, gpa, env, "label", self.label);
        appendIdentChild(&node, gpa, env, "ident", self.ident);
        appendTypeVarChild(&node, gpa, "type_var", self.type_var);

        var kind_node = self.kind.toSExpr(env, ir);
        node.appendNodeChild(gpa, &kind_node);

        return node;
    }
};

/// Marks whether a when branch is redundant using a variable.
pub const RedundantMark = TypeVar;

/// Marks whether a when expression is exhaustive using a variable.
pub const ExhaustiveMark = TypeVar;

/// Helper function to convert the entire Canonical IR to a string in S-expression format
/// and write it to the given writer.
pub fn toSExprStr(ir: *const Self, module_env: *base.ModuleEnv, writer: std.io.AnyWriter) !void {
    _ = module_env; // module_env not needed currently, but keep for signature consistency

    const gpa = ir.env.gpa;
    var root_node = sexpr.Expr.init(gpa, "canonical_ir");
    defer root_node.deinit(gpa);

    // Represent top-level definitions
    var defs_node = sexpr.Expr.init(gpa, "defs");
    // // Need a way to iterate all defs, assuming indices 0..ir.defs.len
    // var iter = ir.defs.iterIndices();
    // while (iter.next()) |i| {
    //     const def = ir.defs.get(i);
    //     var def_sexpr = def.toSExpr(ir.env, ir);
    //     defs_node.appendNodeChild(gpa, &def_sexpr);
    // }
    defs_node.appendStringChild(gpa, "TODO: Implement using NodeStore");
    root_node.appendNodeChild(gpa, &defs_node);

    // TODO: Optionally add other top-level elements like imports, aliases, ingested files
    // var imports_node = sexpr.Expr.init(gpa, "imports");
    // ... iterate ir.imports ...
    // root_node.appendNodeChild(gpa, &imports_node);

    // var aliases_node = sexpr.Expr.init(gpa, "aliases");
    // ... iterate ir.aliases ...
    // root_node.appendNodeChild(gpa, &aliases_node);

    // var ingested_node = sexpr.Expr.init(gpa, "ingested_files");
    // ... iterate ir.ingested_files ...
    // root_node.appendNodeChild(gpa, &ingested_node);

    root_node.toStringPretty(writer);
}

/// todo
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

/// todo: describe FlatType
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
        fields: RecordField.Range,
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

        pub const List = collections.SafeMultiList(@This());
        pub const Range = List.Range;
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
