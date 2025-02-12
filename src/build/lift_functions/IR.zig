const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const problem = @import("../../problem.zig");
const collections = @import("../../collections.zig");

const Ident = base.Ident;
const Problem = problem.Problem;
const FieldName = collections.FieldName;
const StringLiteral = collections.StringLiteral;

const Self = @This();

env: *base.ModuleEnv,
exposed_values: std.AutoHashMap(Ident.Idx, Expr.Idx),
exposed_functions: std.AutoHashMap(Ident.Idx, Function),
types: Type.List,
exprs: Expr.List,
typed_exprs: Expr.Typed.List,
patterns: Pattern.List,
typed_patterns: Pattern.Typed.List,
typed_idents: TypedIdent.List,
when_branches: WhenBranch.List,

pub fn init(env: *base.ModuleEnv, allocator: std.mem.Allocator) Self {
    return Self{
        .env = env,
        .exposed_values = std.AutoHashMap(Ident.Idx, Expr.Idx).init(allocator),
        .exposed_functions = std.AutoHashMap(Ident.Idx, Function).init(allocator),
        .types = Type.List.init(allocator),
        .exprs = Expr.List.init(allocator),
        .typed_exprs = Expr.Typed.List.init(allocator),
        .patterns = Pattern.List.init(allocator),
        .typed_patterns = Pattern.Typed.List.init(allocator),
        .typed_idents = TypedIdent.List.init(allocator),
        .when_branches = WhenBranch.List.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.exposed_values.deinit();
    self.exposed_functions.deinit();
    self.types.deinit();
    self.exprs.deinit();
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
    Struct: Type.NonEmptySlice,
    TagUnion: Type.NonEmptySlice,
    Func: struct {
        ret_then_args: Type.NonEmptySlice,
    },

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;
};

pub const Expr = union(enum) {
    Let: Def,
    Str: StringLiteral,
    Number: base.Literal.Num,
    List: struct {
        elem_type: Type.Idx,
        elems: Expr.Slice,
    },
    Lookup: struct {
        ident: Ident.Idx,
        type: Type.Idx,
    },

    Call: struct {
        fn_type: Type.Idx,
        fn_expr: Expr.Idx,
        args: Expr.Typed.Slice,
    },

    FunctionPack: struct {
        ident: Ident.Idx,
        // empty slice means no captures
        captures: Expr.Slice,
    },

    Unit,

    Struct: Expr.NonEmptySlice,

    StructAccess: struct {
        record_expr: Expr.Idx,
        record_type: Type.Idx,
        field_type: Type.Idx,
        field_id: FieldName.Idx,
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

    CompilerBug: Problem.Compiler.LiftFunctions,

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;

    pub const Typed = struct {
        expr: Expr.Idx,
        type: Type.Idx,

        pub const List = collections.SafeMultiList(@This());
        pub const Slice = Typed.List.Slice;
    };
};

/// A definition, e.g. `x = foo`
pub const Def = struct {
    pattern: Pattern.Idx,
    /// Named variables in the pattern, e.g. `a` in `Ok a ->`
    pattern_vars: TypedIdent.Slice,
    expr: Expr.Idx,
    expr_type: Type.Idx,

    pub const List = collections.SafeMultiList(@This());
    pub const Slice = List.Slice;
};

pub const WhenBranch = struct {
    /// The pattern(s) to match the value against
    patterns: Pattern.NonEmptySlice,
    /// A boolean expression that must be true for this branch to be taken
    guard: ?Expr.Idx,
    /// The expression to produce if the pattern matches
    value: Expr.Idx,

    pub const List = collections.SafeList(@This());
    pub const NonEmptySlice = List.NonEmptySlice;
};

pub const Function = struct {
    args: Pattern.Slice,
    return_type: Type.Idx,
    expr: Expr.Idx,
};

pub const StructDestruct = struct {
    ident: Ident.Idx,
    field: FieldName.Idx,
    kind: Kind,

    pub const Kind = union(enum) {
        Required,
        Guard: Pattern.Typed,
    };

    pub const List = collections.SafeMultiList(@This());
    pub const Slice = List.Slice;
};

pub const Pattern = union(enum) {
    Identifier: Ident.Idx,
    As: struct {
        pattern: Pattern.Idx,
        ident: Ident.Idx,
    },
    StrLiteral: StringLiteral.Idx,
    NumberLiteral: base.Literal.Num,
    AppliedTag: struct {
        tag_union_type: Type.Idx,
        tag_name: Ident.Idx,
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

        opt_rest: ?struct {
            offset: u16,
            name: ?Ident.Idx,
        },
    },
    Underscore,
    CompilerBug: Problem.Compiler.LiftFunctions,

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;

    pub const Typed = struct {
        pattern: Pattern.Idx,
        type: Type.Idx,

        pub const List = collections.SafeMultiList(@This());
        pub const Slice = Typed.List.Slice;
    };
};

pub const TypedIdent = struct {
    pattern: Pattern.Idx,
    type: Type.Idx,

    pub const List = collections.SafeMultiList(@This());
    pub const Slice = List.Slice;
};
