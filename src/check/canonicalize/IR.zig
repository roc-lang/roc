const std = @import("std");
const base = @import("../../base.zig");
const collections = @import("../../collections.zig");
const types = @import("../../types.zig");

const TypeVar = types.Type.Var;

const IR = @This();

env: base.ModuleEnv,
// referenced_values: VecSet<Symbol>,
/// all aliases. `bool` indicates whether it is exposed
// aliases: MutMap<Symbol, (bool, Alias)>,
// rigid_variables: RigidVariables,
exprs: Expr.List,
exprs_at_regions: ExprAtRegion.List,
typed_exprs_at_regions: TypedExprAtRegion.List,
when_branches: WhenBranch.List,
patterns: Pattern.List,
patterns_at_regions: PatternAtRegion.List,
typed_patterns_at_regions: TypedPatternAtRegion.List,

pub fn init(allocator: std.mem.Allocator) IR {
    return IR{
        .env = base.ModuleEnv.init(allocator),
        .exprs = Expr.List.init(allocator),
        .exprs_at_regions = ExprAtRegion.List.init(allocator),
        .typed_exprs_at_regions = TypedExprAtRegion.List.init(allocator),
        .when_branches = WhenBranch.List.init(allocator),
        .patterns = Pattern.List.init(allocator),
        .patterns_at_regions = PatternAtRegion.List.init(allocator),
        .typed_patterns_at_regions = TypedPatternAtRegion.List.init(allocator),
    };
}

pub fn deinit(self: *IR) void {
    self.env.deinit();
    self.exprs.deinit();
    self.exprs_at_regions.deinit();
    self.typed_exprs_at_regions.deinit();
    self.when_branches.deinit();
    self.patterns.deinit();
    self.patterns_at_regions.deinit();
    self.typed_patterns_at_regions.deinit();
}

// TODO: don't use symbol in this module, no imports really exist yet?

pub const Expr = union(enum) {
    // Literals

    // Num stores the `a` variable in `Num a`. Not the same as the variable
    // stored in Int and Float below, which is strictly for better error messages
    Num: struct {
        num_var: TypeVar,
        literal: collections.StringLiteral.Idx,
        value: IntValue,
        bound: NumBound,
    },

    // Int and Float store a variable to generate better error messages
    Int: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: collections.StringLiteral.Idx,
        value: IntValue,
        bound: IntBound,
    },
    Float: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: collections.StringLiteral.Idx,
        value: f64,
        bound: FloatBound,
    },
    Str: collections.StringLiteral.Idx,
    // Number variable, precision variable, value, bound
    SingleQuote: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        value: u32,
        bound: SingleQuoteBound,
    },
    List: struct {
        elem_var: TypeVar,
        loc_elems: ExprAtRegion.Slice,
    },

    Var: struct {
        symbol: base.Symbol,
        type_var: TypeVar,
    },

    When: When.Id,
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
        field: base.Ident.Idx,
    },

    // Sum Types
    Tag: struct {
        tag_union_var: TypeVar,
        ext_var: TypeVar,
        name: collections.FieldName.Idx,
        arguments: TypedExprAtRegion.Slice,
    },

    ZeroArgumentTag: struct {
        closure_name: base.Ident.Idx,
        variant_var: TypeVar,
        ext_var: TypeVar,
        name: base.Ident.Idx,
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
    pattern_region: base.Region,
    expr: Expr.Idx,
    expr_region: base.Region,
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
};

pub const Annotation = struct {
    signature: types.Type,
    // introduced_variables: IntroducedVariables,
    // aliases: VecMap<Symbol, Alias>,
    region: base.Region,
};

pub const IntValue = struct {
    bytes: [16]u8,
    kind: Kind,

    pub const Kind = enum { I128, U128 };
};

pub const ExprAtRegion = struct {
    expr: Expr.Idx,
    region: base.Region,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
};

pub const TypedExprAtRegion = struct {
    expr: Expr.Idx,
    type_var: TypeVar,
    region: base.Region,

    pub const List = collections.SafeMultiList(@This());
    pub const Slice = List.Slice;
};

pub const Function = struct {
    return_var: TypeVar,
    fx_var: TypeVar,
    function_var: TypeVar,
    expr: Expr.Idx,
    region: base.Region,
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
    region: base.Region,
    /// The branches of the when, and the type of the condition that they expect to be matched
    /// against.
    branches: WhenBranch.Slice,
    branches_cond_var: TypeVar,
    /// Whether the branches are exhaustive.
    exhaustive: ExhaustiveMark,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
};

pub const WhenBranchPatternSlice = collections.SafeMultiList(WhenBranchPattern).Slice;

pub const WhenBranchPattern = struct {
    pattern: PatternAtRegion,
    /// Degenerate branch patterns are those that don't fully bind symbols that the branch body
    /// needs. For example, in `A x | B y -> x`, the `B y` pattern is degenerate.
    /// Degenerate patterns emit a runtime error if reached in a program.
    degenerate: bool,
};

pub const WhenBranchSlice = collections.SafeMultiList(WhenBranch).Slice;

pub const WhenBranch = struct {
    patterns: WhenBranchPatternSlice,
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
    Identifier: base.Module.Ident,
    As: struct {
        pattern: Pattern.Idx,
        region: base.Region,
        symbol: base.Symbol,
    },
    AppliedTag: struct {
        whole_var: TypeVar,
        ext_var: TypeVar,
        tag_name: base.IdentId,
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
        literal: collections.LargeStringId,
        value: IntValue,
        bound: NumBound,
    },
    IntLiteral: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: collections.LargeStringId,
        value: IntValue,
        bound: IntBound,
    },
    FloatLiteral: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: collections.LargeStringId,
        value: f64,
        bound: FloatBound,
    },
    StrLiteral: collections.LargeStringId,
    CharLiteral: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        value: u32,
        bound: SingleQuoteBound,
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
    region: base.Region,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
};

pub const TypedPatternAtRegion = struct {
    pattern: Pattern.Idx,
    region: base.Region,
    type_var: TypeVar,

    pub const List = collections.SafeMultiList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
};

pub const RecordDestruct = struct {
    type_var: TypeVar,
    region: base.Region,
    label: base.Ident.Idx,
    module_ident: base.ModuleIdent,
    kind: Kind,

    pub const Kind = union(enum) {
        Required,
        Guard: TypedPatternAtRegion.Idx,
    };

    pub const List = collections.SafeMultiList(@This());
    pub const Slice = List.Slice;
};

/// Describes a bound on the width of an integer.
pub const IntBound = union(enum) {
    /// There is no bound on the width.
    None,
    /// Must have an exact width.
    Exact: base.Primitive.Num,
    /// Must have a certain sign and a minimum width.
    AtLeast: struct {
        sign: SignDemand,
        width: base.Primitive.Num,
    },
};

pub const FloatBound = union(enum) {
    None,
    Exact: FloatWidth,
};

pub const NumBound = union(enum) {
    None,
    /// Must be an integer of a certain size, or any float.
    AtLeastIntOrFloat: struct {
        sign: SignDemand,
        width: base.Primitive.Num,
    },
};

pub const SingleQuoteBound = union(enum) {
    AtLeast: struct { width: base.Primitive.Num },
};

pub const FloatWidth = enum {
    Dec,
    F32,
    F64,
};

pub const SignDemand = enum {
    /// Can be signed or unsigned.
    NoDemand,
    /// Must be signed.
    Signed,
};

/// Marks whether a when branch is redundant using a variable.
pub const RedundantMark = TypeVar;

/// Marks whether a when expression is exhaustive using a variable.
pub const ExhaustiveMark = TypeVar;
