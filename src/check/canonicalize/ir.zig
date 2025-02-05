const std = @import("std");
const base = @import("../../base.zig");
const cols = @import("../../collections.zig");

const TypeVar = base.TypeVar;

pub const IR = struct {
    env: *base.SoloModuleEnv,
    module_id: base.ModuleId,
    // exposed_imports: std.AutoHashMap(comptime K: type, comptime V: type)
    //  MutMap<Symbol, Region>,
    // exposed_symbols: std.AutoHashMap(base.IdentId, .{}),
    // referenced_values: VecSet<Symbol>,
    /// all aliases. `bool` indicates whether it is exposed
    // aliases: MutMap<Symbol, (bool, Alias)>,
    rigid_variables: RigidVariables,
    exprs: cols.SafeList(Expr),
    exprs_at_regions: cols.SafeMultiList(ExprAtRegion),
    typed_exprs_at_regions: cols.SafeMultiList(TypedExprAtRegion),
};

// TODO: don't use symbol in this module, no imports really exist yet?

const ExprId = cols.SafeList(Expr).Id;

pub const Expr = union(enum) {
    // Literals

    // Num stores the `a` variable in `Num a`. Not the same as the variable
    // stored in Int and Float below, which is strictly for better error messages
    Num: struct {
        num_var: TypeVar,
        literal: base.SmallStringId,
        value: IntValue,
        bound: NumBound,
    },

    // Int and Float store a variable to generate better error messages
    Int: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: base.SmallStringId,
        value: IntValue,
        bound: IntBound,
    },
    Float: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        literal: base.SmallStringId,
        value: f64,
        bound: FloatBound,
    },
    Str: base.LargeStringId,
    // Number variable, precision variable, value, bound
    SingleQuote: struct {
        num_var: TypeVar,
        precision_var: TypeVar,
        value: u32,
        bound: SingleQuoteBound,
    },
    List: struct {
        elem_var: TypeVar,
        loc_elems: ExprAtRegionSlice,
    },

    // Lookups
    Var: struct {
        symbol: base.Symbol,
        type_var: TypeVar,
    },

    // Branching
    When: WhenId,
    If: struct {
        cond_var: TypeVar,
        branch_var: TypeVar,
        branches: IfBranchSlice,
        final_else: ExprAtRegionId,
    },

    // Let
    LetRec: struct {
        defs: DefSlice,
        cont: ExprAtRegionId,
        cycle_mark: IllegalCycleMark,
    },

    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    Call: struct {
        // TODO:
        // Box<(Variable, Loc<Expr>, Variable, Variable)>,
        args: TypedExprAtRegionSlice,
        called_via: CalledVia,
    },

    Closure: ClosureData,

    // Product Types
    Record: struct {
        record_var: Variable,
        // TODO:
        // fields: SendMap<Lowercase, Field>,
    },

    /// Empty record constant
    EmptyRecord,

    Tuple: struct {
        tuple_var: TypeVar,
        elems: TypedExprAtRegionSlice,
    },

    /// The "crash" keyword
    Crash: struct {
        msg: ExprAtRegionId,
        ret_var: Variable,
    },

    /// Look up exactly one field on a record, e.g. (expr).foo.
    RecordAccess: struct {
        record_var: TypeVar,
        ext_var: TypeVar,
        field_var: TypeVar,
        loc_expr: ExprAtRegionId,
        field: Lowercase,
    },

    // Sum Types
    Tag: struct {
        tag_union_var: TypeVar,
        ext_var: TypeVar,
        name: TagName,
        arguments: TypedExprAtRegionSlice,
    },

    ZeroArgumentTag: struct {
        closure_name: Symbol,
        variant_var: TypeVar,
        ext_var: TypeVar,
        name: TagName,
    },

    /// Compiles, but will crash if reached
    RuntimeError: RuntimeError,
};

pub const IntValue = struct {
    bytes: [16]u8,
    kind: Kind,

    pub const Kind = enum { I128, U128 };
};

pub const ExprAtRegionId = cols.SafeMultiList(ExprAtRegion).Id;
pub const ExprAtRegionSlice = cols.SafeMultiList(ExprAtRegion).Slice;

pub const ExprAtRegion = struct {
    expr: ExprId,
    region: base.Region,
};

pub const TypedExprAtRegionSlice = cols.SafeMultiList(TypedExprAtRegion).Slice;

pub const TypedExprAtRegion = struct {
    expr: ExprId,
    type_var: TypeVar,
    region: base.Region,
};

pub const PatternAtRegion = struct {
    pattern: PatternId,
    region: base.Region,
};

pub const Function = struct {
    return_var: TypeVar,
    fx_var: TypeVar,
    function_var: TypeVar,
    expr: ExprId,
    region: base.Region,
};

pub const IfBranchSlice = cols.SafeMultiList(IfBranch).Slice;

pub const IfBranch = struct {
    cond: ExprAtRegion,
    body: ExprAtRegion,
};

pub const WhenId = cols.SafeMultiList(When).Id;

pub const When = struct {
    /// The actual condition of the when expression.
    loc_cond: ExprAtRegionId,
    cond_var: TypeVar,
    /// Type of each branch (and therefore the type of the entire `when` expression)
    expr_var: TypeVar,
    region: base.Region,
    /// The branches of the when, and the type of the condition that they expect to be matched
    /// against.
    branches: WhenBranchSlice,
    branches_cond_var: TypeVar,
    /// Whether the branches are exhaustive.
    exhaustive: ExhaustiveMark,
};

pub const WhenBranchPatternSlice = cols.SafeMultiList(WhenBranchPattern).Slice;

pub const WhenBranchPattern = struct {
    pattern: PatternAtRegion,
    /// Degenerate branch patterns are those that don't fully bind symbols that the branch body
    /// needs. For example, in `A x | B y -> x`, the `B y` pattern is degenerate.
    /// Degenerate patterns emit a runtime error if reached in a program.
    degenerate: bool,
};

pub const WhenBranchSlice = cols.SafeMultiList(WhenBranch).Slice;

pub const WhenBranch = struct {
    patterns: WhenBranchPatternSlice,
    value: ExprAtRegionId,
    guard: ?ExprAtRegionId,
    /// Whether this branch is redundant in the `when` it appears in
    redundant: RedundantMark,
};


/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
pub const Pattern = union(enum) {
    Identifier: Symbol,
    As(Box<Loc<Pattern>>, Symbol),
    AppliedTag {
        whole_var: Variable,
        ext_var: Variable,
        tag_name: TagName,
        arguments: Vec<(Variable, Loc<Pattern>)>,
    },
    UnwrappedOpaque {
        whole_var: Variable,
        opaque: Symbol,
        argument: Box<(Variable, Loc<Pattern>)>,

        // The following help us link this opaque reference to the type specified by its
        // definition, which we then use during constraint generation. For example
        // suppose we have
        //
        //   Id n := [Id U64 n]
        //   strToBool : Str -> Bool
        //
        //   f = \@Id who -> strToBool who
        //
        // Then `opaque` is "Id", `argument` is "who", but this is not enough for us to
        // infer the type of the expression as "Id Str" - we need to link the specialized type of
        // the variable "n".
        // That's what `specialized_def_type` and `type_arguments` are for; they are specialized
        // for the expression from the opaque definition. `type_arguments` is something like
        // [(n, fresh1)], and `specialized_def_type` becomes "[Id U64 fresh1]".
        specialized_def_type: Box<Type>,
        type_arguments: Vec<OptAbleVar>,
        lambda_set_variables: Vec<LambdaSet>,
    },
    RecordDestructure {
        whole_var: Variable,
        ext_var: Variable,
        destructs: Vec<Loc<RecordDestruct>>,
    },
    TupleDestructure {
        whole_var: Variable,
        ext_var: Variable,
        destructs: Vec<Loc<TupleDestruct>>,
    },
    List {
        list_var: Variable,
        elem_var: Variable,
        patterns: ListPatterns,
    },
    NumLiteral(Variable, Box<str>, IntValue, NumBound),
    IntLiteral(Variable, Variable, Box<str>, IntValue, IntBound),
    FloatLiteral(Variable, Variable, Box<str>, f64, FloatBound),
    StrLiteral(Box<str>),
    SingleQuote(Variable, Variable, char, SingleQuoteBound),
    Underscore,

    /// An identifier that marks a specialization of an ability member.
    /// For example, given an ability member definition `hash : a -> U64 where a implements Hash`,
    /// there may be the specialization `hash : Bool -> U64`. In this case we generate a
    /// new symbol for the specialized "hash" identifier.
    AbilityMemberSpecialization {
        /// The symbol for this specialization.
        ident: Symbol,
        /// The ability name being specialized.
        specializes: Symbol,
    },

    // Runtime Exceptions
    Shadowed(Region, Loc<Ident>, Symbol),
    OpaqueNotInScope(Loc<Ident>),
    // Example: (5 = 1 + 2) is an unsupported pattern in an assignment; Int patterns aren't allowed in assignments!
    UnsupportedPattern(Region),
    // parse error patterns
    MalformedPattern(MalformedPatternProblem, Region),
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

