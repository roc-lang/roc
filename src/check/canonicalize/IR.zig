const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const collections = @import("../../collections.zig");

const Ident = base.Ident;
const Region = base.Region;
const TypeVar = types.TypeVar;
const TagName = collections.TagName;
const FieldName = collections.FieldName;
const StringLiteral = collections.StringLiteral;

const Self = @This();

env: base.ModuleEnv,
aliases: std.AutoHashMap(Ident.Idx, Alias.WithVisibility),
exprs: Expr.List,
exprs_at_regions: ExprAtRegion.List,
typed_exprs_at_regions: TypedExprAtRegion.List,
when_branches: WhenBranch.List,
patterns: Pattern.List,
patterns_at_regions: PatternAtRegion.List,
typed_patterns_at_regions: TypedPatternAtRegion.List,
type_vars: collections.SafeList(TypeVar),

pub fn init(allocator: std.mem.Allocator) Self {
    return Self{
        .env = base.ModuleEnv.init(allocator),
        .aliases = std.AutoHashMap(Ident.Idx, Alias.WithVisibility).init(allocator),
        .exprs = Expr.List.init(allocator),
        .exprs_at_regions = ExprAtRegion.List.init(allocator),
        .typed_exprs_at_regions = TypedExprAtRegion.List.init(allocator),
        .when_branches = WhenBranch.List.init(allocator),
        .patterns = Pattern.List.init(allocator),
        .patterns_at_regions = PatternAtRegion.List.init(allocator),
        .typed_patterns_at_regions = TypedPatternAtRegion.List.init(allocator),
        .type_vars = collections.SafeList(TypeVar).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.env.deinit();
    self.exprs.deinit();
    self.exprs_at_regions.deinit();
    self.typed_exprs_at_regions.deinit();
    self.when_branches.deinit();
    self.patterns.deinit();
    self.patterns_at_regions.deinit();
    self.typed_patterns_at_regions.deinit();
    self.type_vars.deinit();
}

pub const RigidVariables = struct {
    named: std.AutoHashMap(TypeVar, Ident.Idx),
    // with_methods: std.AutoHashMap(TypeVar, WithMethods),

    // pub const WithMethods = struct {
    //     name: Ident.Idx,
    //     methods: MethodSet,
    // };
};

pub const Alias = struct {
    name: Ident.Idx,
    region: Region,
    type_variables: Alias.Var.Slice,
    /// Extension variables that should be inferred in output positions, and closed in input
    /// positions.
    infer_ext_in_output_variables: collections.SafeList(TypeVar).Slice,
    recursion_variables: std.AutoHashMap(TypeVar, Ident.Idx),

    //     pub typ: Type,
    kind: Kind,

    pub const Kind = enum {
        Structural,
        /// Aliases for types that are defined in Zig instead of Roc,
        /// like List and Box.
        Builtin,
        // Enable custom types in the future
        //
        // Custom,
    };

    pub const Var = struct {
        name: Ident.Idx,
        region: Region,
        type_var: TypeVar,
        // /// `Some` if this variable is bound to abilities; `None` otherwise.
        // pub opt_bound_abilities: Option<AbilitySet>,

        pub const List = collections.SafeMultiList(@This());
        pub const Slice = List.Slice;
    };

    pub const WithVisibility = struct {
        visible: bool,
        alias: Alias,
    };
};

// TODO: don't use symbol in this module, no imports really exist yet?

pub const Expr = union(enum) {
    // Literals

    // Num stores the `a` variable in `Num a`. Not the same as the variable
    // stored in Int and Float below, which is strictly for better error messages
    Num: struct {
        num_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.num.Bound.Num,
    },

    // Int and Float store a variable to generate better error messages
    Int: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.num.Bound.Int,
    },
    Float: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: f64,
        bound: types.num.Bound.Float,
    },
    Str: collections.StringLiteral.Idx,
    // Number variable, precision variable, value, bound
    SingleQuote: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        value: u32,
        bound: types.num.Bound.SingleQuote,
    },
    List: struct {
        elem_var: TypeVar,
        elems: ExprAtRegion.Slice,
    },

    Var: struct {
        ident: Ident.Idx,
        type_var: TypeVar,
    },

    When: When.Idx,
    If: struct {
        cond_var: TypeVar,
        branch_var: TypeVar,
        branches: IfBranch.Slice,
        final_else: ExprAtRegion.Idx,
    },

    Let: struct {
        defs: Def.Slice,
        cont: ExprAtRegion.Idx,
        // cycle_mark: IllegalCycleMark,
    },

    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    Call: struct {
        // TODO:
        // Box<(Variable, Loc<Expr>, Variable, Variable)>,
        args: TypedExprAtRegion.Slice,
        called_via: base.CalledVia,
    },

    // Closure: ClosureData,

    // Product Types
    Record: struct {
        record_var: TypeVar,
        // TODO:
        // fields: SendMap<Lowercase, Field>,
    },

    /// Empty record constant
    EmptyRecord,

    /// The "crash" keyword
    Crash: struct {
        msg: ExprAtRegion.Idx,
        ret_var: TypeVar,
    },

    /// Look up exactly one field on a record, e.g. (expr).foo.
    RecordAccess: struct {
        record_var: TypeVar,
        ext_var: TypeVar,
        field_var: TypeVar,
        loc_expr: ExprAtRegion.Idx,
        field: Ident.Idx,
    },

    // Sum Types
    Tag: struct {
        tag_union_var: TypeVar,
        ext_var: TypeVar,
        name: FieldName.Idx,
        arguments: TypedExprAtRegion.Slice,
    },

    ZeroArgumentTag: struct {
        closure_name: Ident.Idx,
        variant_var: TypeVar,
        ext_var: TypeVar,
        name: Ident.Idx,
    },

    /// Compiles, but will crash if reached
    // RuntimeError: RuntimeError,

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
    };

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
};

pub const Annotation = struct {
    signature: types.TypeVar,
    // introduced_variables: IntroducedVariables,
    // aliases: VecMap<Symbol, Alias>,
    region: Region,
};

pub const IntValue = struct {
    bytes: [16]u8,
    kind: Kind,

    pub const Kind = enum { I128, U128 };
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
    type_var: TypeVar,
    region: Region,

    pub const List = collections.SafeMultiList(@This());
    pub const Slice = List.Slice;
};

pub const Function = struct {
    return_var: TypeVar,
    fx_var: TypeVar,
    function_var: TypeVar,
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
    cond_var: TypeVar,
    /// Type of each branch (and therefore the type of the entire `when` expression)
    expr_var: TypeVar,
    region: Region,
    /// The branches of the when, and the type of the condition that they expect to be matched
    /// against.
    branches: WhenBranch.Slice,
    branches_cond_var: TypeVar,
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
    Identifier: base.Module.Idx,
    As: struct {
        pattern: Pattern.Idx,
        region: Region,
        ident: Ident.Idx,
    },
    AppliedTag: struct {
        whole_var: TypeVar,
        ext_var: TypeVar,
        tag_name: Ident.Idx,
        arguments: TypedPatternAtRegion.Slice,
    },
    RecordDestructure: struct {
        whole_var: TypeVar,
        ext_var: TypeVar,
        destructs: RecordDestruct.Slice,
    },
    List: struct {
        list_var: TypeVar,
        elem_var: TypeVar,
        patterns: Pattern.List,
    },
    NumLiteral: struct {
        num_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.num.Bound.Num,
    },
    IntLiteral: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: IntValue,
        bound: types.num.Bound.Int,
    },
    FloatLiteral: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: StringLiteral.Idx,
        value: f64,
        bound: types.num.Bound.Float,
    },
    StrLiteral: StringLiteral.Idx,
    CharLiteral: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
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
    type_var: TypeVar,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
};

pub const RecordDestruct = struct {
    type_var: TypeVar,
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
pub const RedundantMark = TypeVar;

/// Marks whether a when expression is exhaustive using a variable.
pub const ExhaustiveMark = TypeVar;

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

pub const FlatType = union(enum) {
    Apply: struct {
        ident: Ident.Idx,
        vars: collections.SafeList(TypeVar).Slice,
    },
    Func: struct {
        arg_vars: collections.SafeList(TypeVar).Slice,
        ret_var: TypeVar,
        fx: TypeVar,
    },
    /// A function that we know nothing about yet except that it's effectful
    EffectfulFunc,
    Record: struct {
        whole_var: TypeVar,
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
    //     name: TagName.Idx,
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
        type_var: TypeVar,
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
    Openness: TypeVar,
    /// This tag extension can grow unboundedly.
    Any: TypeVar,
};
