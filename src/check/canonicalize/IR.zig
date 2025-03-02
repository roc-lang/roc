const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const problem = @import("../../problem.zig");
const collections = @import("../../collections.zig");

const Alias = @import("./Alias.zig");

const Ident = base.Ident;
const Region = base.Region;
const ModuleImport = base.ModuleImport;
const StringLiteral = base.StringLiteral;
const TypeIdx = types.Type.Idx;
const Problem = problem.Problem;

const Self = @This();

env: base.ModuleEnv,
aliases: Alias.List,
defs: Def.List,
exprs: Expr.List,
exprs_at_regions: ExprAtRegion.List,
typed_exprs_at_regions: TypedExprAtRegion.List,
when_branches: WhenBranch.List,
patterns: Pattern.List,
patterns_at_regions: PatternAtRegion.List,
typed_patterns_at_regions: TypedPatternAtRegion.List,
type_indices: collections.SafeList(TypeIdx),
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
    return Self{
        .env = base.ModuleEnv.init(gpa),
        .aliases = Alias.List.init(gpa),
        .defs = Def.List.init(gpa),
        .exprs = Expr.List.init(gpa),
        .exprs_at_regions = ExprAtRegion.List.init(gpa),
        .typed_exprs_at_regions = TypedExprAtRegion.List.init(gpa),
        .when_branches = WhenBranch.List.init(gpa),
        .patterns = Pattern.List.init(gpa),
        .patterns_at_regions = PatternAtRegion.List.init(gpa),
        .typed_patterns_at_regions = TypedPatternAtRegion.List.init(gpa),
        .type_indices = collections.SafeList(TypeIdx).init(gpa),
        // .type_var_names = Ident.Store.init(gpa),
        .ingested_files = IngestedFile.List.init(gpa),
    };
}

pub fn deinit(self: *Self) void {
    self.env.deinit();
    self.aliases.deinit();
    self.defs.deinit();
    self.exprs.deinit();
    self.exprs_at_regions.deinit();
    self.typed_exprs_at_regions.deinit();
    self.when_branches.deinit();
    self.patterns.deinit();
    self.patterns_at_regions.deinit();
    self.typed_patterns_at_regions.deinit();
    self.type_indices.deinit();
    // self.type_var_names.deinit();
    self.ingested_files.deinit();
}

pub const RigidVariables = struct {
    named: std.AutoHashMap(TypeIdx, Ident.Idx),
    // with_methods: std.AutoHashMap(TypeVar, WithMethods),

    // pub const WithMethods = struct {
    //     name: Ident.Idx,
    //     methods: MethodSet,
    // };
};

// TODO: don't use symbol in this module, no imports really exist yet?

pub const Expr = union(enum) {
    // Literals

    // Num stores the `a` variable in `Num a`. Not the same as the variable
    // stored in Int and Float below, which is strictly for better error messages
    num: struct {
        num_var: TypeIdx,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.num.Bound.Num,
    },

    // Int and Float store a variable to generate better error messages
    int: struct {
        num_var: TypeIdx,
        precision_var: TypeIdx,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.num.Bound.Int,
    },
    float: struct {
        num_var: TypeIdx,
        precision_var: TypeIdx,
        literal: StringLiteral.Idx,
        value: f64,
        bound: types.num.Bound.Float,
    },
    str: StringLiteral.Idx,
    // Number variable, precision variable, value, bound
    single_quote: struct {
        num_var: TypeIdx,
        precision_var: TypeIdx,
        value: u32,
        bound: types.num.Bound.SingleQuote,
    },
    list: struct {
        elem_var: TypeIdx,
        elems: ExprAtRegion.Slice,
    },

    @"var": struct {
        ident: Ident.Idx,
        type_var: TypeIdx,
    },

    when: When.Idx,
    @"if": struct {
        cond_var: TypeIdx,
        branch_var: TypeIdx,
        branches: IfBranch.Slice,
        final_else: ExprAtRegion.Idx,
    },

    let: struct {
        defs: Def.Slice,
        cont: ExprAtRegion.Idx,
        // cycle_mark: IllegalCycleMark,
    },

    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    call: struct {
        // TODO:
        // Box<(Variable, Loc<Expr>, Variable, Variable)>,
        args: TypedExprAtRegion.Slice,
        // called_via: base.CalledVia,
    },

    // Closure: ClosureData,

    // Product Types
    record: struct {
        record_var: TypeIdx,
        // TODO:
        // fields: SendMap<Lowercase, Field>,
    },

    /// Empty record constant
    empty_record,

    /// The "crash" keyword
    crash: struct {
        msg: ExprAtRegion.Idx,
        ret_var: TypeIdx,
    },

    /// Look up exactly one field on a record, e.g. (expr).foo.
    record_access: struct {
        record_var: TypeIdx,
        ext_var: TypeIdx,
        field_var: TypeIdx,
        loc_expr: ExprAtRegion.Idx,
        field: Ident.Idx,
    },

    // Sum Types
    tag: struct {
        tag_union_var: TypeIdx,
        ext_var: TypeIdx,
        name: Ident.Idx,
        arguments: TypedExprAtRegion.Slice,
    },

    zero_argument_tag: struct {
        closure_name: Ident.Idx,
        variant_var: TypeIdx,
        ext_var: TypeIdx,
        name: Ident.Idx,
    },

    /// Compiles, but will crash if reached
    RuntimeError: Problem.Idx,

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;
};

pub const IngestedFile = struct {
    relative_path: StringLiteral.Idx,
    ident: Ident.Idx,
    type: Annotation,

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;
};

pub const Def = struct {
    pattern: Pattern.Idx,
    pattern_region: Region,
    expr: Expr.Idx,
    expr_region: Region,
    expr_var: TypeIdx,
    // TODO:
    // pattern_vars: SendMap<Symbol, Variable>,
    annotation: ?Annotation,
    kind: Kind,

    const Kind = union(enum) {
        /// A def that introduces identifiers
        Let,
        /// A standalone statement with an fx variable
        Stmt: TypeIdx,
        /// Ignored result, must be effectful
        Ignored: TypeIdx,
    };

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
};

pub const Annotation = struct {
    signature: TypeIdx,
    // introduced_variables: IntroducedVariables,
    // aliases: VecMap<Symbol, Alias>,
    region: Region,
};

pub const IntValue = struct {
    bytes: [16]u8,
    kind: Kind,

    pub const Kind = enum { i128, u128 };
};

pub const ExprAtRegion = struct {
    expr: Expr.Idx,
    region: Region,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
};

pub const TypedExprAtRegion = struct {
    expr: Expr.Idx,
    type_var: TypeIdx,
    region: Region,

    pub const List = collections.SafeMultiList(@This());
    pub const Slice = List.Slice;
};

pub const Function = struct {
    return_var: TypeIdx,
    fx_var: TypeIdx,
    function_var: TypeIdx,
    expr: Expr.Idx,
    region: Region,
};

pub const IfBranch = struct {
    cond: ExprAtRegion,
    body: ExprAtRegion,

    pub const List = collections.SafeMultiList(@This());
    pub const Slice = List.Slice;
};

pub const When = struct {
    /// The actual condition of the when expression.
    loc_cond: ExprAtRegion.Idx,
    cond_var: TypeIdx,
    /// Type of each branch (and therefore the type of the entire `when` expression)
    expr_var: TypeIdx,
    region: Region,
    /// The branches of the when, and the type of the condition that they expect to be matched
    /// against.
    branches: WhenBranch.Slice,
    branches_cond_var: TypeIdx,
    /// Whether the branches are exhaustive.
    exhaustive: ExhaustiveMark,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
};

pub const WhenBranchPattern = struct {
    pattern: PatternAtRegion,
    /// Degenerate branch patterns are those that don't fully bind symbols that the branch body
    /// needs. For example, in `A x | B y -> x`, the `B y` pattern is degenerate.
    /// Degenerate patterns emit a runtime error if reached in a program.
    degenerate: bool,

    pub const List = collections.SafeMultiList(@This());
    pub const Slice = List.Slice;
};

pub const WhenBranch = struct {
    patterns: WhenBranchPattern.Slice,
    value: ExprAtRegion.Idx,
    guard: ?ExprAtRegion.Idx,
    /// Whether this branch is redundant in the `when` it appears in
    redundant: RedundantMark,

    pub const List = collections.SafeMultiList(@This());
    pub const Slice = List.Slice;
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
        whole_var: TypeIdx,
        ext_var: TypeIdx,
        tag_name: Ident.Idx,
        arguments: TypedPatternAtRegion.Slice,
    },
    record_destructure: struct {
        whole_var: TypeIdx,
        ext_var: TypeIdx,
        destructs: RecordDestruct.Slice,
    },
    list: struct {
        list_var: TypeIdx,
        elem_var: TypeIdx,
        patterns: Pattern.List,
    },
    num_literal: struct {
        num_var: TypeIdx,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.num.Bound.Num,
    },
    int_literal: struct {
        num_var: TypeIdx,
        precision_var: TypeIdx,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.num.Bound.Int,
    },
    float_literal: struct {
        num_var: TypeIdx,
        precision_var: TypeIdx,
        literal: StringLiteral.Idx,
        value: f64,
        bound: types.num.Bound.Float,
    },
    str_literal: StringLiteral.Idx,
    char_literal: struct {
        num_var: TypeIdx,
        precision_var: TypeIdx,
        value: u32,
        bound: types.num.Bound.SingleQuote,
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
    pub const Slice = List.Slice;
};

pub const PatternAtRegion = struct {
    pattern: Pattern.Idx,
    region: Region,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
};

pub const TypedPatternAtRegion = struct {
    pattern: Pattern.Idx,
    region: Region,
    type_var: TypeIdx,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
};

pub const RecordDestruct = struct {
    type_var: TypeIdx,
    region: Region,
    label: Ident.Idx,
    ident: Ident.Idx,
    kind: Kind,

    pub const Kind = union(enum) {
        Required,
        Guard: TypedPatternAtRegion.Idx,
    };

    pub const List = collections.SafeMultiList(@This());
    pub const Slice = List.Slice;
};

/// Marks whether a when branch is redundant using a variable.
pub const RedundantMark = TypeIdx;

/// Marks whether a when expression is exhaustive using a variable.
pub const ExhaustiveMark = TypeIdx;

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
        structure: TypeIdx,
        opt_name: ?Ident.Idx,
    },
    Structure: FlatType,
    Alias: struct {
        ident: Ident.Idx,
        // vars: AliasVariables,
        type_var: TypeIdx,
        kind: Alias.Kind,
    },
    RangedNumber: types.num.NumericRange,
    Error,
    /// The fx type variable for a given function
    Pure,
    Effectful,
};

pub const FlatType = union(enum) {
    Apply: struct {
        ident: Ident.Idx,
        vars: collections.SafeList(TypeIdx).Slice,
    },
    Func: struct {
        arg_vars: collections.SafeList(TypeIdx).Slice,
        ret_var: TypeIdx,
        fx: TypeIdx,
    },
    /// A function that we know nothing about yet except that it's effectful
    EffectfulFunc,
    Record: struct {
        whole_var: TypeIdx,
        fields: RecordField.Slice,
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

    pub const RecordField = struct {
        name: Ident.Idx,
        type_var: TypeIdx,
        // type: Reco,

        pub const List = collections.SafeMultiList(@This());
        pub const Slice = List.Slice;
    };
};

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
    Openness: TypeIdx,
    /// This tag extension can grow unboundedly.
    Any: TypeIdx,
};
