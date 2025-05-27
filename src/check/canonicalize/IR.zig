const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const problem = @import("../../problem.zig");
const collections = @import("../../collections.zig");
const Alias = @import("./Alias.zig");
const sexpr = @import("../../base/sexpr.zig"); // Added import
const Scratch = @import("../../base/Scratch.zig").Scratch;
const DataSpan = @import("../../base/DataSpan.zig");
const exitOnOom = collections.utils.exitOnOom;

const Ident = base.Ident;
const Region = base.Region;
const ModuleImport = base.ModuleImport;
const ModuleEnv = base.ModuleEnv;
const StringLiteral = base.StringLiteral;
const TypeVar = types.Var;
const Problem = problem.Problem;
const Self = @This();

env: ModuleEnv,
store: NodeStore,
ingested_files: IngestedFile.List,
imports: ModuleImport.Store,

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
pub fn init(env: ModuleEnv) Self {
    var ident_store = env.idents;
    return Self{
        .env = env,
        .store = NodeStore.initCapacity(env.gpa, 1000), // TODO: Figure out what this should be
        // .type_var_names = Ident.Store.init(gpa),
        .ingested_files = .{},
        .imports = ModuleImport.Store.init(&.{}, &ident_store, env.gpa),
    };
}

/// Initialize the IR for a module's canonicalization info with a specified capacity.
/// For more information refer to documentation on [init] as well
pub fn initCapacity(env: ModuleEnv, capacity: usize) Self {
    var ident_store = env.idents;
    return Self{
        .env = env,
        .store = NodeStore.initCapacity(env.gpa, capacity), // TODO: Figure out what this should be
        // .type_var_names = Ident.Store.init(gpa),
        .ingested_files = .{},
        .imports = ModuleImport.Store.init(&.{}, &ident_store, env.gpa),
    };
}

/// Deinit the IR's memory.
pub fn deinit(self: *Self) void {
    self.store.deinit();
    if (self.ingested_files.items.items.len > 0) {
        self.ingested_files.deinit(self.env.gpa);
    }
    self.imports.deinit(self.env.gpa);
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

/// Stores AST nodes and provides scratch arrays for working with nodes.
pub const NodeStore = struct {
    gpa: std.mem.Allocator,
    nodes: Node.List,
    extra_data: std.ArrayListUnmanaged(u32),
    scratch_statements: Scratch(Statement.Idx),
    scratch_exprs: Scratch(Expr.Idx),
    scratch_record_fields: Scratch(RecordField.Idx),
    scratch_when_branches: Scratch(WhenBranch.Idx),
    scratch_where_clauses: Scratch(WhereClause.Idx),
    scratch_patterns: Scratch(Pattern.Idx),
    scratch_pattern_record_fields: Scratch(PatternRecordField.Idx),
    scratch_type_annos: Scratch(TypeAnno.Idx),
    scratch_anno_record_fields: Scratch(AnnoRecordField.Idx),
    scratch_exposed_items: Scratch(ExposedItem.Idx),

    pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) NodeStore {
        return .{
            .gpa = gpa,
            .nodes = Node.List.initCapacity(gpa, capacity),
            .extra_data = std.ArrayListUnmanaged(u32).initCapacity(gpa, capacity / 2) catch |err| exitOnOom(err),
            .scratch_statements = Scratch(Statement.Idx).init(gpa),
            .scratch_exprs = Scratch(Expr.Idx).init(gpa),
            .scratch_patterns = Scratch(Pattern.Idx).init(gpa),
            .scratch_record_fields = Scratch(RecordField.Idx).init(gpa),
            .scratch_pattern_record_fields = Scratch(PatternRecordField.Idx).init(gpa),
            .scratch_when_branches = Scratch(WhenBranch.Idx).init(gpa),
            .scratch_type_annos = Scratch(TypeAnno.Idx).init(gpa),
            .scratch_anno_record_fields = Scratch(AnnoRecordField.Idx).init(gpa),
            .scratch_exposed_items = Scratch(ExposedItem.Idx).init(gpa),
            .scratch_where_clauses = Scratch(WhereClause.Idx).init(gpa),
        };
    }

    pub fn deinit(store: *NodeStore) void {
        store.nodes.deinit(store.gpa);
        store.extra_data.deinit(store.gpa);
        store.scratch_statements.deinit(store.gpa);
        store.scratch_exprs.deinit(store.gpa);
        store.scratch_patterns.deinit(store.gpa);
        store.scratch_record_fields.deinit(store.gpa);
        store.scratch_pattern_record_fields.deinit(store.gpa);
        store.scratch_when_branches.deinit(store.gpa);
        store.scratch_type_annos.deinit(store.gpa);
        store.scratch_anno_record_fields.deinit(store.gpa);
        store.scratch_exposed_items.deinit(store.gpa);
        store.scratch_where_clauses.deinit(store.gpa);
    }

    // Add nodes

    pub fn addStatement(store: *NodeStore, statement: Statement) Statement.Idx {
        const node = Node{};

        switch (statement) {
            else => {
                std.debug.panic("Statement of type {s} not yet implemented in Can\n", .{@tagName(statement)});
            },
        }

        return @bitCast(store.nodes.append(store.gpa, node));
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
                node.data_1 = @intCast(e.ident.idx);
            },
            else => {
                std.debug.panic("Expression of type {s} not yet implemented in Can\n", .{@tagName(expr.expr)});
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
        const node = Node{};

        switch (pattern) {
            else => {
                std.debug.panic("Pattern of type {s} not yet implemented in Can\n", .{@tagName(pattern)});
            },
        }

        return .{ .id = store.nodes.append(store.gpa, node) };
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

    // Get nodes

    pub fn getStatement(store: *NodeStore, statement: Statement.Idx) Statement {
        _ = store;
        _ = statement;

        return Statement{};
    }
    pub fn getExpr(store: *NodeStore, expr: Expr.Idx) Expr {
        _ = store;
        _ = expr;

        return Expr{};
    }
    pub fn getRecordField(store: *NodeStore, recordField: RecordField.Idx) RecordField {
        _ = store;
        _ = recordField;

        return RecordField{};
    }
    pub fn getWhenBranch(store: *NodeStore, whenBranch: WhenBranch.Idx) WhenBranch {
        _ = store;
        _ = whenBranch;

        return WhenBranch{};
    }
    pub fn getWhereClause(store: *NodeStore, whereClause: WhereClause.Idx) WhereClause {
        _ = store;
        _ = whereClause;

        return WhereClause{};
    }
    pub fn getPattern(store: *NodeStore, pattern: Pattern.Idx) Pattern {
        _ = store;
        _ = pattern;

        return Pattern{};
    }
    pub fn getPatternRecordField(store: *NodeStore, patternRecordField: PatternRecordField.Idx) PatternRecordField {
        _ = store;
        _ = patternRecordField;

        return PatternRecordField{};
    }
    pub fn getTypeAnno(store: *NodeStore, typeAnno: TypeAnno.Idx) TypeAnno {
        _ = store;
        _ = typeAnno;

        return TypeAnno{};
    }
    pub fn getAnnoRecordField(store: *NodeStore, annoRecordField: AnnoRecordField.Idx) AnnoRecordField {
        _ = store;
        _ = annoRecordField;

        return AnnoRecordField{};
    }
    pub fn getExposedItem(store: *NodeStore, exposedItem: ExposedItem.Idx) ExposedItem {
        _ = store;
        _ = exposedItem;

        return ExposedItem{};
    }

    pub fn sliceFromSpan(store: *NodeStore, comptime T: type, span: anytype) []T {
        return @as([]T, @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]));
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
        anno: Self.TypeAnno.Idx,
        where: ?WhereClause.Span,
        region: Region,
    };
    /// A type annotation, declaring that the value referred to by an ident in the same scope should be a given type.
    pub const TypeAnno = struct {
        name: Ident.Idx,
        anno: Self.TypeAnno.Idx,
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

const PatternRecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};
const TypeAnno = union(enum) {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};
const TypeHeader = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};
const AnnoRecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: DataSpan };
};
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
    lookup: Lookup,
    list: struct {
        elem_var: TypeVar,
        elems: Expr.Range,
    },

    when: When.Idx,
    @"if": struct {
        cond_var: TypeVar,
        branch_var: TypeVar,
        branches: IfBranch.Range,
        final_else: Expr.Idx,
    },

    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    call: struct {
        // TODO:
        // Box<(Variable, Loc<Expr>, Variable, Variable)>,
        args: Expr.Range,
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
        args: Expr.Range,
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

    /// A list of canonicalized expressions.
    pub const List = collections.SafeList(@This());
    /// An index into a list of canonicalized expressions.
    pub const Idx = List.Idx;
    /// A range of canonicalized expressions.
    pub const Range = List.Range;
    /// A non-empty slice of canonicalized expressions.
    pub const NonEmptyRange = List.NonEmptyRange;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        switch (self.*) {
            .num => |e| {
                var node = sexpr.Expr.init(gpa, "num");
                node.appendStringChild(gpa, "literal"); // TODO: use e.literal
                // TODO: Represent IntValue better
                node.appendStringChild(gpa, "value=<int_value>");
                node.appendStringChild(gpa, @tagName(e.bound));
                return node;
            },
            .int => |e| {
                var node = sexpr.Expr.init(gpa, "int");
                node.appendStringChild(gpa, "literal"); // TODO: use e.literal
                // TODO: Represent IntValue better
                node.appendStringChild(gpa, "value=<int_value>");
                node.appendStringChild(gpa, @tagName(e.bound));
                return node;
            },
            .float => |e| {
                var node = sexpr.Expr.init(gpa, "float");
                node.appendStringChild(gpa, "literal"); // TODO: use e.literal
                const val_str = std.fmt.allocPrint(gpa, "{d}", .{e.value}) catch "<oom>";
                defer gpa.free(val_str);
                node.appendStringChild(gpa, val_str);
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
                node.appendStringChild(gpa, @tagName(e.bound));
                return node;
            },
            .list => |l| {
                var node = sexpr.Expr.init(gpa, "list");
                var elems_node = sexpr.Expr.init(gpa, "elems");
                for (ir.exprs_at_regions.rangeToSlice(l.elems).items(.expr)) |elem| {
                    var elem_sexpr = ir.exprs.get(elem).toSExpr(env, ir);
                    elems_node.appendNodeChild(gpa, &elem_sexpr);
                }
                node.appendNodeChild(gpa, &elems_node);
                return node;
            },
            .lookup => |v| {
                var node = sexpr.Expr.init(gpa, "lookup");
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
            .record => {
                var node = sexpr.Expr.init(gpa, "record");
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
                return node;
            },
            .tag => |t| {
                var node = sexpr.Expr.init(gpa, "tag");
                appendIdentChild(&node, gpa, env, "name", t.name);
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

    /// A list of ingested files.
    pub const List = collections.SafeList(@This());
    /// In index into a list of ingested files.
    pub const Idx = List.Idx;
    /// A range of ingested files.
    pub const Range = List.Range;
    /// A non-empty slice of ingested files.
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

    /// todo
    pub const List = collections.SafeList(@This());
    /// todo
    pub const Idx = List.Idx;
    /// todo
    pub const Range = List.Range;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        var node = sexpr.Expr.init(gpa, "def");

        var kind_node = self.kind.toSExpr(gpa);
        node.appendNodeChild(gpa, &kind_node);

        var pattern_node = sexpr.Expr.init(gpa, "pattern");
        // pattern_node.appendRegionChild(gpa, self.pattern_region);
        const pattern = ir.patterns.get(self.pattern);
        var pattern_sexpr = pattern.toSExpr(env, ir);
        pattern_node.appendNodeChild(gpa, &pattern_sexpr);
        node.appendNodeChild(gpa, &pattern_node);

        var expr_node = sexpr.Expr.init(gpa, "expr");
        // expr_node.appendRegionChild(gpa, self.expr_region);
        const expr = ir.exprs.get(self.expr);
        var expr_sexpr = expr.toSExpr(env, ir);
        expr_node.appendNodeChild(gpa, &expr_sexpr);
        node.appendNodeChild(gpa, &expr_node);

        if (self.annotation) |anno| {
            var anno_node = anno.toSExpr(env, ir);
            node.appendNodeChild(gpa, &anno_node);
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
        _ = self;
        _ = ir; // ir not needed currently, but keep for signature consistency
        const gpa = env.gpa;
        const node = sexpr.Expr.init(gpa, "annotation");
        // node.appendRegionChild(gpa, self.region);
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
    expr: Expr,
    region: Region,

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        var node = sexpr.Expr.init(gpa, "expr_at_region");
        node.appendRegionChild(gpa, self.region);
        var expr_sexpr = self.expr.toSEXPR(env, ir);
        node.appendNodeChild(gpa, &expr_sexpr);
        return node;
    }
};

/// todo
pub const TypedExprAtRegion = struct {
    expr: Expr.Idx,
    type_var: TypeVar,
    region: Region,

    /// todo
    pub const List = collections.SafeMultiList(@This());
    /// todo
    pub const Range = List.Range;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        var node = sexpr.Expr.init(gpa, "typed_expr_at_region");
        // node.appendRegionChild(gpa, self.region);
        const expr = ir.exprs.get(self.expr);
        var expr_sexpr = expr.toSExpr(env, ir);
        node.appendNodeChild(gpa, &expr_sexpr);
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

    /// todo
    pub const List = collections.SafeMultiList(@This());
    /// todo
    pub const Range = List.Range;

    // Note: toSExpr is handled within Expr.if because the slice reference is there
};

/// todo
pub const When = struct {
    /// The actual condition of the when expression.
    loc_cond: Expr.Idx,
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

    /// todo
    pub const List = collections.SafeMultiList(@This());
    /// todo
    pub const Idx = List.Idx;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        var node = sexpr.Expr.init(gpa, "when_data"); // Renamed to avoid clash with Expr.when
        // node.appendRegionChild(gpa, self.region);

        var cond_node = sexpr.Expr.init(gpa, "cond");
        const cond_expr = ir.exprs_at_regions.get(self.loc_cond);
        var cond_sexpr = cond_expr.toSExpr(env, ir);
        cond_node.appendNodeChild(gpa, &cond_sexpr);
        node.appendNodeChild(gpa, &cond_node);

        var branches_node = sexpr.Expr.init(gpa, "branches");
        for (ir.when_branches.getSlice(self.branches)) |branch_idx| {
            const branch = ir.when_branches.get(branch_idx);
            var branch_sexpr = branch.toSExpr(env, ir);
            branches_node.appendNodeChild(gpa, &branch_sexpr);
        }
        node.appendNodeChild(gpa, &branches_node);

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

    /// todo
    pub const List = collections.SafeMultiList(@This());
    /// todo
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
    value: Expr.Idx,
    guard: ?Expr.Idx,
    /// Whether this branch is redundant in the `when` it appears in
    redundant: RedundantMark,

    /// todo
    pub const List = collections.SafeMultiList(@This());
    /// todo
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
                var node = sexpr.Expr.init(gpa, "pattern_list");
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

    /// todo
    pub const List = collections.SafeMultiList(@This());

    /// todo
    pub const Range = List.Range;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        var node = sexpr.Expr.init(gpa, "record_destruct");
        // node.appendRegionChild(gpa, self.region);
        appendIdentChild(&node, gpa, env, "label", self.label);
        appendIdentChild(&node, gpa, env, "ident", self.ident);
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
    _ = ir;
    const gpa = module_env.gpa;
    var root_node = sexpr.Expr.init(gpa, "canonical_ir");
    defer root_node.deinit(gpa);

    // // Represent top-level definitions
    // var defs_node = sexpr.Expr.init(gpa, "defs");
    // // Need a way to iterate all defs, assuming indices 0..ir.defs.len
    // var iter = ir.defs.iterIndices();
    // while (iter.next()) |i| {
    //     const def = ir.defs.get(i);
    //     var def_sexpr = def.toSExpr(ir.env, ir);
    //     defs_node.appendNodeChild(gpa, &def_sexpr);
    // }
    // root_node.appendNodeChild(gpa, &defs_node);

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

/// todo
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
        // type: Reco,

        /// todo
        pub const List = collections.SafeMultiList(@This());
        /// todo
        pub const Range = List.Range;
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
