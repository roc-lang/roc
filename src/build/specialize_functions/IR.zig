const std = @import("std");
const base = @import("../../base.zig");
const cols = @import("../../collections.zig");
const problem = @import("../../problem.zig");

pub const IR = @This();

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
    Primitive: base.Primitive,
    Box: Type.Idx,
    List: Type.Idx,
    Struct: Type.NonEmptySlice,
    TagUnion: Type.NonEmptySlice,

    pub const List = cols.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;
};

pub const Expr = union(enum) {
    Let: Def,
    Str: cols.StringLiteral.Idx,
    Number: base.NumberLiteral,
    List: struct {
        elem_type: Type.Idx,
        elems: Expr.Slice,
    },
    LocalLookup: struct {
        ident: base.Ident.Idx,
        type: Type.Idx,
    },
    ModuleLookup: struct {
        ident: base.Ident.Idx,
        module: base.Module.Idx,
        type: Type.Idx,
    },
    FunctionCall: struct {
        fn_type: Type.Idx,
        fn_ident: base.Ident.Idx,
        args: Expr.Typed.Slice,
    },

    Unit,

    Struct: Expr.NonEmptySlice,
    StructAccess: struct {
        record_expr: Expr.Idx,
        record_type: Type.Idx,
        field_type: Type.Idx,
        field_id: cols.FieldName.Idx,
    },

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
        branches: WhenBranch.NonEmptySlice,
    },

    CompilerBug: problem.SpecializeFunctionsProblem,

    pub const List = cols.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;

    pub const Typed = struct {
        expr: Expr.Idx,
        type: Type.Idx,

        pub const List = cols.SafeMultiList(@This());
        pub const Idx = Typed.List.Idx;
        pub const Slice = Typed.List.Slice;
    };
};

pub const Def = struct {
    pattern: Pattern.Idx,
    /// Named variables in the pattern, e.g. `a` in `Ok a ->`
    pattern_vars: TypedIdent.Slice,
    expr: Expr.Idx,
    expr_type: Type.Idx,
};

pub const WhenBranch = struct {
    /// The pattern(s) to match the value against
    patterns: Pattern.NonEmptySlice,
    /// A boolean expression that must be true for this branch to be taken
    guard: ?Expr.Idx,
    /// The expression to produce if the pattern matches
    value: Expr.Idx,
};

pub const Pattern = union(enum) {
    Identifier: base.Ident.Idx,
    As: struct {
        pattern: Pattern.Idx,
        ident: base.Ident.Idx,
    },
    StrLiteral: cols.StringLiteral.Idx,
    NumberLiteral: base.NumberLiteral,
    AppliedTag: struct {
        tag_union_type: Type.Idx,
        tag_name: base.Ident.Idx,
        args: Pattern.Slice,
    },
    StructDestructure: struct {
        struct_type: Type.Idx,
        destructs: StructDestruct.Slice,
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
        opt_rest: ?struct { position: u16, ident: ?base.Ident.Idx },
    },
    Underscore,
    CompilerBug: problem.SpecializeFunctionsProblem,

    pub const List = cols.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;

    pub const Typed = struct {
        pattern: Pattern.Idx,
        type: Type.Idx,

        pub const List = cols.SafeList(@This());
        pub const Slice = Typed.List.Slice;
    };
};

pub const StructDestruct = struct {
    ident: base.Ident.Idx,
    field: cols.FieldName.Idx,
    kind: Kind,

    pub const Kind = union(enum) {
        Required,
        Guard: Pattern.Typed,
    };
};

pub const TypedIdent = struct {
    ident: base.Ident.Idx,
    type: Type.Idx,

    pub const List = cols.SafeList(@This());
    pub const Slice = List.Slice;
};
