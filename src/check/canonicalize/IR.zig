const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const problem = @import("../../problem.zig");
const collections = @import("../../collections.zig");
const Alias = @import("./Alias.zig");
const sexpr = @import("../../base/sexpr.zig"); // Added import

const Ident = base.Ident;
const Region = base.Region;
const ModuleImport = base.ModuleImport;
const StringLiteral = base.StringLiteral;
const TypeVar = types.Var;
const Problem = problem.Problem;
const Self = @This();

env: base.ModuleEnv,
aliases: Alias.List,
imports: ModuleImport.Store,
defs: Def.List,
exprs: Expr.List,
exprs_at_regions: ExprAtRegion.List,
typed_exprs_at_regions: TypedExprAtRegion.List,
if_branches: IfBranch.List,
when_branches: WhenBranch.List,
patterns: Pattern.List,
patterns_at_regions: PatternAtRegion.List,
typed_patterns_at_regions: TypedPatternAtRegion.List,
type_indices: collections.SafeList(TypeVar),
// type_var_names: Ident.Store,
ingested_files: IngestedFile.List,

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
pub fn init(gpa: std.mem.Allocator) Self {
    var env = base.ModuleEnv.init(gpa);

    return Self{
        .env = env,
        .aliases = .{},
        .imports = ModuleImport.Store.init(&.{}, &env.idents, gpa),
        .defs = .{},
        .exprs = .{},
        .exprs_at_regions = .{},
        .typed_exprs_at_regions = .{},
        .if_branches = .{},
        .when_branches = .{},
        .patterns = .{},
        .patterns_at_regions = .{},
        .typed_patterns_at_regions = .{},
        .type_indices = .{},
        // .type_var_names = Ident.Store.init(gpa),
        .ingested_files = .{},
    };
}

/// Deinit the IR's memory.
pub fn deinit(self: *Self) void {
    self.env.deinit();
    self.aliases.deinit(self.env.gpa);
    self.imports.deinit(self.env.gpa);
    self.defs.deinit(self.env.gpa);
    self.exprs.deinit(self.env.gpa);
    self.exprs_at_regions.deinit(self.env.gpa);
    self.typed_exprs_at_regions.deinit(self.env.gpa);
    self.if_branches.deinit(self.env.gpa);
    self.when_branches.deinit(self.env.gpa);
    self.patterns.deinit(self.env.gpa);
    self.patterns_at_regions.deinit(self.env.gpa);
    self.typed_patterns_at_regions.deinit(self.env.gpa);
    self.type_indices.deinit(self.env.gpa);
    // self.type_var_names.deinit(self.env.gpa);
    self.ingested_files.deinit(self.env.gpa);
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
        bound: types.NumCompact,
    },

    // Int and Float store a variable to generate better error messages
    int: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.NumCompact.Int.Precision,
    },
    float: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: f64,
        bound: types.NumCompact.Frac.Precision,
    },
    str: StringLiteral.Idx,
    // Number variable, precision variable, value, bound
    single_quote: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        value: u32,
        bound: types.NumCompact.Int.Precision,
    },
    list: struct {
        elem_var: TypeVar,
        elems: ExprAtRegion.Range,
    },

    @"var": struct {
        ident: Ident.Idx,
        type_var: TypeVar,
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
                appendTypeVarChild(&node, gpa, "type_var", v.type_var);
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

        appendTypeVarChild(&node, gpa, "expr_var", self.expr_var);

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

    /// todo
    pub const List = collections.SafeMultiList(@This());
    /// todo
    pub const Idx = List.Idx;
    /// todo
    pub const Range = List.Range;

    pub fn toSExpr(self: *const @This(), env: *const base.ModuleEnv, ir: *const Self) sexpr.Expr {
        const gpa = env.gpa;
        var node = sexpr.Expr.init(gpa, "expr_at_region");
        // node.appendRegionChild(gpa, self.region);
        const expr = ir.exprs.get(self.expr);
        var expr_sexpr = expr.toSExpr(env, ir);
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
        appendTypeVarChild(&node, gpa, "type_var", self.type_var);
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

        appendTypeVarChild(&node, gpa, "cond_var", self.cond_var);
        appendTypeVarChild(&node, gpa, "expr_var", self.expr_var);
        appendTypeVarChild(&node, gpa, "branches_cond_var", self.branches_cond_var);
        appendTypeVarChild(&node, gpa, "exhaustive_mark", self.exhaustive);

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
    value: ExprAtRegion.Idx,
    guard: ?ExprAtRegion.Idx,
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
        bound: types.NumCompact,
    },
    int_literal: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.NumCompact.Int.Precision,
    },
    float_literal: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: f64,
        bound: types.NumCompact.Frac.Precision,
    },
    str_literal: StringLiteral.Idx,
    char_literal: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        value: u32,
        bound: types.NumCompact.Int.Precision,
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
    // Need a way to iterate all defs, assuming indices 0..ir.defs.len
    var iter = ir.defs.iterIndices();
    while (iter.next()) |i| {
        const def = ir.defs.get(i);
        var def_sexpr = def.toSExpr(&ir.env, ir);
        defs_node.appendNodeChild(gpa, &def_sexpr);
    }
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
