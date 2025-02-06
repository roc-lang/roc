const std = @import("std");
const base = @import("../../base.zig");
const cols = @import("../../collections.zig");
const problem = @import("../../problem.zig");
const types = @import("../../types.zig");

const IR = @This();

/// All IR data representing a single module that has
/// had its functions lifted.
env: *base.ModuleEnv,
types: Type.List,
exprs: Expr.List,
expr_regions: cols.SafeList(base.Region),
typed_exprs: Expr.Typed.List,
patterns: Pattern.List,
typed_patterns: Pattern.Typed.List,
typed_idents: TypedIdent.List,
when_branches: WhenBranch.List,

pub fn init(env: *base.ModuleEnv, allocator: std.mem.Allocator) IR {
    return IR{
        .env = env,
        .types = Type.List.init(allocator),
        .exprs = Expr.List.init(allocator),
        .expr_regions = cols.SafeList(base.Region).init(allocator),
        .typed_exprs = Expr.Typed.List.init(allocator),
        .patterns = Pattern.List.init(allocator),
        .typed_patterns = Pattern.Typed.List.init(allocator),
        .typed_idents = TypedIdent.List.init(allocator),
        .when_branches = WhenBranch.List.init(allocator),
    };
}

pub fn deinit(self: *IR) void {
    self.types.deinit();
    self.exprs.deinit();
    self.expr_regions.deinit();
    self.typed_exprs.deinit();
    self.patterns.deinit();
    self.typed_patterns.deinit();
    self.typed_idents.deinit();
    self.when_branches.deinit();
}

pub const Type = union(enum) {
    Primitive: types.Primitive,
    Box: Type.Idx,
    List: Type.Idx,
    /// Slice of field types, ordered alphabetically by field name (or by tuple elem index).
    Struct: Type.NonEmptySlice,
    TagUnion: Type.NonEmptySlice,
    FunctionPack: struct {
        /// zero fields means no captures
        opt_fields: Type.Slice,
    },

    pub const List = cols.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;
};

const Expr = union(enum) {
    Let: Def,
    Str: cols.StringLiteral.Idx,
    Number: base.Number,
    List: struct {
        elem_type: Type.Idx,
        elems: Expr.Slice,
    },
    Lookup: struct {
        ident: base.ModuleIdent,
        type: Type.Idx,
    },

    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    Call: struct {
        fn_type: Type.Idx,
        fn_expr: Expr.Idx,
        args: Expr.Typed.Slice,
    },

    FunctionPack: struct {
        fn_ident: base.Ident.Idx,
        captures: Pattern.Typed.Slice,
    },

    Unit,

    Tag: struct {
        discriminant: u16,
        tag_union_type: Type.Idx,
        args: Expr.Typed.Slice,
    },

    When: struct {
        /// The value being matched on
        value: Expr.Idx,
        /// The type of the value being matched on
        value_type: Type.Idx,
        /// The return type of all branches and thus the whole when expression
        branch_type: Type.Idx,
        /// The branches of the when expression
        branches: WhenBranch.List.NonEmptySlice,
    },

    CompilerBug: problem.LiftFunctionsProblem,

    pub const List = cols.SafeList(@This());
    pub const Id = List.Id;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;

    pub const Typed = struct {
        type: Type.Idx,
        expr: Expr.Idx,

        pub const List = cols.SafeMultiList(@This());
        pub const Slice = Typed.List.Slice;
    };
};

const Def = struct {
    pattern: Pattern.Idx,
    /// Named variables in the pattern, e.g. `a` in `Ok a ->`
    pattern_vars: TypedIdent.Slice,
    expr: Expr.Idx,
    expr_type: Type.Idx,
};

const WhenBranch = struct {
    /// The pattern(s) to match the value against
    patterns: Pattern.NonEmptySlice,
    /// A boolean expression that must be true for this branch to be taken
    guard: ?Expr.Idx,
    /// The expression to produce if the pattern matches
    value: Expr.Idx,

    pub const List = cols.SafeList(@This());
    pub const NonEmptySlice = List.NonEmptySlice;
};

pub const Pattern = union(enum) {
    Identifier: base.Ident.Idx,
    As: struct {
        inner_pattern: Pattern.Idx,
        ident: base.Ident.Idx,
    },
    StrLiteral: cols.LargeString.Idx,
    NumberLiteral: base.NumberLiteral,
    AppliedTag: struct {
        tag_union_type: Type.Idx,
        tag_name: cols.TagName.Idx,
        args: Pattern.Slice,
    },
    StructDestructure: struct {
        struct_type: Type.Idx,
        destructs: RecordDestruct.Slice,
        opt_spread: ?Pattern.Typed,
    },
    List: struct {
        elem_type: Type.Idx,
        patterns: Pattern.Slice,

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

    pub const List = cols.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;

    pub const Typed = struct {
        pattern: Pattern.Idx,
        type: Type.Idx,

        pub const List = cols.SafeMultiList(@This());
        pub const Slice = Typed.List.Slice;
    };
};

pub const RecordDestruct = struct {
    ident: base.Ident.Idx,
    field: cols.FieldName.Idx,
    Kind: Kind,

    pub const Kind = union(enum) {
        Required,
        Guard: Pattern.Typed,
    };

    pub const List = cols.SafeMultiList(@This());
    pub const Slice = List.Slice;
};

const TypedIdent = struct {
    ident: base.Ident.Idx,
    type: Type.Idx,

    pub const List = cols.SafeMultiList(@This());
    pub const Slice = List.Slice;
};
