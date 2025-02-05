const std = @import("std");
const base = @import("../../base.zig");
const cols = @import("../../collections.zig");
const problem = @import("../../problem.zig");

pub const IR = struct {
    env: *base.ModuleEnv,
    exprs: cols.SafeList(Expr),
    expr_regions: cols.SafeList(base.Region),
    patterns: cols.SafeList(Pattern),
    types: cols.SafeList(Type),

    pub fn init(env: *base.ModuleEnv, allocator: std.mem.Allocator) IR {
        return IR{
            .env = env,
            .exprs = cols.SafeList(Expr).init(allocator),
            .expr_regions = cols.SafeList(base.Region).init(allocator),
            .patterns = cols.SafeList(Pattern).init(allocator),
            .types = cols.SafeList(Type).init(allocator),
        };
    }

    pub fn deinit(self: *IR) void {
        self.exprs.deinit();
        self.expr_regions.deinit();
        self.patterns.deinit();
        self.types.deinit();
    }
};

pub const TypeId = cols.SafeList(Type).Id;
pub const TypeSlice = cols.SafeList(Type).Slice;
pub const TypeNonEmptySlice = cols.SafeList(Type).NonEmptySlice;

pub const Type = union(enum) {
    Primitive: base.Primitive,
    Box: TypeId,
    List: TypeId,
    Struct: TypeNonEmptySlice,
    TagUnion: TypeNonEmptySlice,
    FunctionPack: struct {
        /// zero fields means no captures
        opt_fields: TypeSlice,
    },
};

const ExprId = cols.SafeList(Expr).Id;
const ExprSlice = cols.SafeList(Expr).Slice;
const ExprNonEmptySlice = cols.SafeList(Expr).NonEmptySlice;

const Expr = union(enum) {
    Let: Def,
    Str: cols.LargeStringId,
    Number: base.Number,
    List: struct {
        elem_type: TypeId,
        elems: ExprSlice,
    },
    LocalLookup: struct {
        ident: base.IdentId,
        type: TypeId,
    },
    ModuleLookup: struct {
        ident: base.IdentId,
        module: base.ModuleId,
        type: TypeId,
    },

    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    Call: struct {
        fn_type: TypeId,
        fn_expr: ExprId,
        args: TypedExprSlice,
    },

    FunctionPack: struct {
        fn_ident: base.IdentId,
        captures: TypedPatternSlice,
    },

    Unit,

    Tag: struct {
        discriminant: u16,
        tag_union_type: TypeId,
        args: TypedExprSlice,
    },

    When: struct {
        /// The value being matched on
        value: ExprId,
        /// The type of the value being matched on
        value_type: TypeId,
        /// The return type of all branches and thus the whole when expression
        branch_type: TypeId,
        /// The branches of the when expression
        branches: cols.NonEmptySlice(WhenBranch),
    },

    CompilerBug: problem.LiftFunctionsProblem,
};

const Def = struct {
    pattern: PatternId,
    /// Named variables in the pattern, e.g. `a` in `Ok a ->`
    pattern_vars: TypedIdentSlice,
    expr: ExprId,
    expr_type: TypeId,
};

const WhenBranch = struct {
    /// The pattern(s) to match the value against
    patterns: PatternNonEmptySlice,
    /// A boolean expression that must be true for this branch to be taken
    guard: ?ExprId,
    /// The expression to produce if the pattern matches
    value: ExprId,
};

const WhenBranches = struct {
    branches: cols.SafeList(WhenBranch),
};

pub const PatternId = cols.SafeList(Pattern).Id;
pub const PatternSlice = cols.SafeList(Pattern).Slice;
pub const PatternNonEmptySlice = cols.SafeList(Pattern).NonEmptySlice;

pub const Pattern = union(enum) {
    Identifier: base.IdentId,
    As: struct {
        inner_pattern: PatternId,
        ident: base.IdentId,
    },
    StrLiteral: cols.LargeStringId,
    NumberLiteral: base.NumberLiteral,
    AppliedTag: struct {
        tag_union_type: TypeId,
        tag_name: base.IdentId,
        args: PatternSlice,
    },
    StructDestructure: struct {
        struct_type: TypeId,
        destructs: RecordDestructSlice,
        opt_spread: ?TypedPattern,
    },
    List: struct {
        elem_type: TypeId,
        patterns: PatternSlice,

        /// Where a rest pattern splits patterns before and after it, if it does at all.
        /// If present, patterns at index >= the rest index appear after the rest pattern.
        /// For example:
        ///   [ .., A, B ] -> patterns = [A, B], rest = 0
        ///   [ A, .., B ] -> patterns = [A, B], rest = 1
        ///   [ A, B, .. ] -> patterns = [A, B], rest = 2
        /// Optionally, the rest pattern can be named - e.g. `[ A, B, ..others ]`
        opt_rest: ?.{ u16, ?base.IdentId },
    },
    Underscore,
    CompilerBug: problem.LiftFunctionsProblem,
};

const RecordDestructSlice = cols.SafeMultiList(RecordDestruct).Slice;

pub const RecordDestruct = struct {
    ident: base.IdentId,
    field: base.FieldNameId,
    type: DestructType,
};

pub const DestructType = union(enum) {
    Required,
    Guard: TypedPattern,
};

const TypedExpr = struct { type: TypeId, expr: ExprId };
const TypedIdent = struct { ident: base.IdentId, type: TypeId };
const TypedPattern = struct { pattern: PatternId, type: TypeId };

const TypedExprSlice = cols.SafeMultiList(TypedExpr).Slice;
const TypedIdentSlice = cols.SafeMultiList(TypedIdent).Slice;
const TypedPatternSlice = cols.SafeMultiList(TypedPattern).Slice;
