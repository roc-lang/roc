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
};

pub const ExprId = cols.SafeList(Expr).Id;
pub const ExprSlice = cols.SafeList(Expr).Slice;
pub const ExprNonEmptySlice = cols.SafeList(Expr).NonEmptySlice;

pub const Expr = union(enum) {
    Let: Def,
    Str: cols.LargeStringId,
    Number: base.NumberLiteral,
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
    FunctionCall: struct {
        fn_type: TypeId,
        fn_ident: base.IdentId,
        args: std.MultiArrayList(TypedExpr).Slice,
    },

    Unit,

    Struct: ExprNonEmptySlice,
    StructAccess: struct {
        record_expr: ExprId,
        record_type: TypeId,
        field_type: TypeId,
        field_id: base.FieldNameId,
    },

    Tag: struct {
        discriminant: u16,
        tag_union_type: TypeId,
        args: std.MultiArrayList(TypedExpr).Slice,
    },

    When: struct {
        /// The value being matched on
        value: ExprId,
        /// The type of the value being matched on
        value_type: TypeId,
        /// The return type of all branches and thus the whole when expression
        branch_type: TypeId,
        /// The branches of the when expression
        branches: cols.SafeList(WhenBranch).NonEmptySlice,
    },

    CompilerBug: problem.SpecializeFunctionsProblem,
};

pub const Def = struct {
    pattern: PatternId,
    /// Named variables in the pattern, e.g. `a` in `Ok a ->`
    pattern_vars: std.MultiArrayList(TypedIdent).Slice,
    expr: ExprId,
    expr_type: TypeId,
};

pub const WhenBranch = struct {
    /// The pattern(s) to match the value against
    patterns: PatternNonEmptySlice,
    /// A boolean expression that must be true for this branch to be taken
    guard: ?ExprId,
    /// The expression to produce if the pattern matches
    value: ExprId,
};

pub const WhenBranches = struct {
    // branches: Vec<MaybeUninit<WhenBranch>>,
};

pub const PatternId = cols.SafeList(Pattern).Id;
pub const PatternSlice = cols.SafeList(Pattern).Slice;
pub const PatternNonEmptySlice = cols.SafeList(Pattern).NonEmptySlice;

pub const Pattern = union(enum) {
    Identifier: base.IdentId,
    As: struct {
        pattern: PatternId,
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
        destructs: std.MultiArrayList(StructDestruct).Slice,
        opt_spread: ?.TypedPattern,
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
        opt_rest: ?struct { position: u16, ident: ?base.IdentId },
    },
    Underscore,
    CompilerBug: problem.SpecializeFunctionsProblem,
};

pub const StructDestruct = struct {
    ident: base.IdentId,
    field: base.FieldNameId,
    destruct_type: DestructType,
};

pub const DestructType = union(enum) {
    Required,
    Guard: TypedPattern,
};

pub const TypedExpr = struct { expr: ExprId, type: TypeId };
pub const TypedIdent = struct { ident: base.IdentId, type: TypeId };
pub const TypedPattern = struct { pattern: PatternId, type: TypeId };

pub const TypedExprSlice = cols.SafeMultiList(TypedExpr).Slice;
pub const TypedIdentSlice = cols.SafeMultiList(TypedIdent).Slice;
pub const TypedPatternSlice = cols.SafeMultiList(TypedPattern).Slice;
