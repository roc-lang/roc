const std = @import("std");
const base = @import("../../base.zig");
const types_module = @import("../../types.zig");
const problem = @import("../../problem.zig");
const collections = @import("../../collections.zig");

const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const Problem = problem.Problem;

const Self = @This();

env: *base.ModuleEnv,
exposed_values: std.AutoHashMap(Ident.Idx, Expr.Idx),
exposed_functions: std.AutoHashMap(Ident.Idx, Function),
/// ??? all types in the module?
types: Type.List,
/// ??? all expressions in the module?
exprs: Expr.List,
/// ??? all typed expressions in the module?
typed_exprs: Expr.Typed.List,
/// ??? all patterns in the module?
patterns: Pattern.List,
/// ??? all typed patterns in the module?
typed_patterns: Pattern.Typed.List,
/// ??? all typed identifiers in the module?
typed_idents: TypedIdent.List,
/// ??? all when branches in the module?
when_branches: WhenBranch.List,

pub fn init(env: *base.ModuleEnv) Self {
    return Self{
        .env = env,
        .exposed_values = std.AutoHashMap(Ident.Idx, Expr.Idx).init(env.gpa),
        .exposed_functions = std.AutoHashMap(Ident.Idx, Function).init(env.gpa),
        .types = .{},
        .exprs = .{},
        .typed_exprs = .{},
        .patterns = .{},
        .typed_patterns = .{},
        .typed_idents = .{},
        .when_branches = .{},
    };
}

pub fn deinit(self: *Self) void {
    self.exposed_values.deinit(self.env.gpa);
    self.exposed_functions.deinit(self.env.gpa);
    self.types.deinit(self.env.gpa);
    self.exprs.deinit(self.env.gpa);
    self.typed_exprs.deinit(self.env.gpa);
    self.patterns.deinit(self.env.gpa);
    self.typed_patterns.deinit(self.env.gpa);
    self.typed_idents.deinit(self.env.gpa);
    self.when_branches.deinit(self.env.gpa);
}

/// Represents a Roc type e.g. `U64`, `Str`, `List(U64)`
pub const Type = union(enum) {
    /// e.g. Int, Bool, Str
    primitive: types_module.Primitive,
    /// Holds unknown Roc type for use with platform or can be used to improve perf in rare cases roc-lang.org/builtins/Box
    box: Type.Idx,
    /// e.g. List(U64)
    list: Type.Idx,
    /// ??? Record?
    @"struct": Type.NonEmptySlice,
    /// e.g. `[Red, Yellow, Green]`
    tag_union: Type.NonEmptySlice,
    func: struct {
        /// ???
        ret_then_args: Type.NonEmptySlice,
    },
    /// list of Type
    pub const List = collections.SafeList(@This());
    /// Index into Type.List
    pub const Idx = List.Idx;
    /// Sublist reference of Type.List
    pub const Slice = List.Slice;
    /// Sublist reference into Type.List that is guaranteed to be non-empty
    pub const NonEmptySlice = List.NonEmptySlice;
};

/// Represents a Roc expression
pub const Expr = union(enum) {
    /// e.g. `x = 1`
    let: Def,
    /// e.g. "abc"
    str: StringLiteral,
    /// e.g. 123
    number: base.Literal.Num,
    /// e.g. [1, 2, 3]
    list: struct {
        elem_type: Type.Idx,
        elems: Expr.Slice,
    },
    /// e.g. `pf.Stdout` or `x`
    lookup: struct {
        ident: Ident.Idx,
        type: Type.Idx,
    },
    /// e.g `fibo(5)`
    call: struct {
        fn_type: Type.Idx,
        // ???
        fn_expr: Expr.Idx,
        args: Expr.Typed.Slice,
    },
    /// e.g. `|x| x * 2`
    lambda: struct {
        fn_type: Type.Idx,
        arguments: Pattern.Typed.Slice,
        body: Expr.Idx,
        recursive: base.Recursive,
    },
    /// ??? What's this?
    unit,
    /// ??? What's this?
    @"struct": Expr.NonEmptySlice,
    /// Should this be called record access?
    struct_access: struct {
        record_expr: Expr.Idx,
        record_type: Type.Idx,
        field_type: Type.Idx,
        field_id: Ident.Idx,
    },
    /// e.g. `Ok(1)`
    tag: struct {
        /// ??? Numeric representation of the tag variant? How to find the name of the tag?
        discriminant: u16,
        /// e.g. `Result` for `Ok(1)`
        tag_union_type: Type.Idx,
        /// e.g. `1` for `Ok(1)`
        args: Expr.Typed.Slice,
    },
    /// ??? Didn't we change `when` to `match`?
    when: struct {
        /// The value being matched on, e.g. `when value is`
        value: Expr.Idx,
        /// The type of the value being matched on
        value_type: Type.Idx,
        /// The return type of all branches and thus the whole when expression
        branch_type: Type.Idx,
        /// The branches of the when expression
        branches: WhenBranch.NonEmptySlice,
    },

    compiler_bug: Problem.Compiler,

    /// List of Expr
    pub const List = collections.SafeList(@This());
    /// Index into an Expr.List
    pub const Idx = List.Idx;
    /// Sublist reference of Expr.List
    pub const Slice = List.Slice;
    /// Sublist reference into Expr.List that is guaranteed to be non-empty
    pub const NonEmptySlice = List.NonEmptySlice;
    /// ??? Type annotated expression e.g. `1: U64` ?
    pub const Typed = struct {
        expr: Expr.Idx,
        type: Type.Idx,
        /// List of Expr.Typed
        pub const List = collections.SafeMultiList(@This());
        /// Sublist reference into Expr.Typed.List
        pub const Slice = Typed.List.Slice;
    };
};

/// A definition, e.g. `x = foo`
pub const Def = struct {
    /// ??? Is pattern for what's to the left of `=`?
    pattern: Pattern.Idx,
    /// Named variables in the pattern, e.g. `a` in `Ok a ->` ??? A when branch is not a Def right?
    pattern_vars: TypedIdent.Slice,
    /// expression to the right of `=`
    expr: Expr.Idx,
    /// type of the expression to the right of `=`
    expr_type: Type.Idx,
    /// List of Def
    pub const List = collections.SafeMultiList(@This());
    /// Sublist reference into Def.List
    pub const Slice = List.Slice;
};

/// Branch of a `when` expression, e.g. `Green -> "green"`
pub const WhenBranch = struct {
    /// The pattern(s) to match the value against
    patterns: Pattern.NonEmptySlice,
    /// A boolean expression that must be true for this branch to be taken
    guard: ?Expr.Idx,
    /// The expression to produce if the pattern matches
    value: Expr.Idx,
    /// List of WhenBranch
    pub const List = collections.SafeList(@This());
    /// Reference to non-empty sublist into WhenBranch.List
    pub const NonEmptySlice = List.NonEmptySlice;
};

/// e.g. `|x| x * 2`
pub const Function = struct {
    args: Pattern.Slice,
    return_type: Type.Idx,
    /// ??? Is expr the body of the function?
    expr: Expr.Idx,
};

/// ??? For Record destructuring, e.g. `{ x, y } = coords(point)`
pub const StructDestruct = struct {
    /// ???
    ident: Ident.Idx,
    /// ???
    field: Ident.Idx,
    kind: Kind,
    /// ???
    pub const Kind = union(enum) {
        /// ???
        required,
        /// ???
        guard: Pattern.Typed,
    };
    /// List of StructDestruct
    pub const List = collections.SafeMultiList(@This());
    /// Reference to sublist into StructDestruct.List
    pub const Slice = List.Slice;
};

/// Represents a pattern used in pattern matching e.g. `Ok x` as part of a when branch.
pub const Pattern = union(enum) {
    /// e.g. `x`
    identifier: Ident.Idx,
    /// e.g. `.. as tail` in `[head, .. as tail] -> prepend(head, tail)`
    as: struct {
        /// e.g. `..` in `.. as tail`
        pattern: Pattern.Idx,
        /// e.g. `tail` in `.. as tail`
        ident: Ident.Idx,
    },
    str_literal: StringLiteral.Idx,
    number_literal: base.Literal.Num,
    /// e.g. `Ok(x)` in `Ok(x) ->`
    applied_tag: struct {
        /// Type of tag union e.g. `Result`
        tag_union_type: Type.Idx,
        tag_name: Ident.Idx,
        /// e.g. `x` in `Ok(x)`
        args: Pattern.Slice,
    },
    /// e.g. `{x, y}` in `{x, y} -> Point(x, y)`
    struct_destructure: struct {
        struct_type: Type.Idx,
        /// ??? e.g. `x` and `y` in `{x, y}`
        destructs: StructDestruct.Slice,
        /// e.g. `..` in `{x, y, ..}`
        opt_spread: ?Pattern.Typed,
    },
    /// e.g. [Foo, Bar, ..]
    list: struct {
        elem_type: Type.Idx,
        /// e.g. Foo and Bar in `[Foo, Bar, ..]`
        patterns: Pattern.Slice,
        /// refers to e.g. `.. as tail` in `[head, .. as tail]`
        opt_rest: ?struct {
            /// position in list of `..`; e.g. 0 in `[.., Foo, Bar]`
            offset: u16,
            /// e.g. tail in `.. as tail`
            name: ?Ident.Idx,
        },
    },
    /// _ is used as a catch-all, it matches anything
    underscore,
    compiler_bug: Problem.Compiler,
    /// List of Pattern
    pub const List = collections.SafeList(@This());
    /// Index into Pattern.List
    pub const Idx = List.Idx;
    /// Sublist reference of Pattern.List
    pub const Slice = List.Slice;
    /// Sublist reference into Pattern.List that is guaranteed to be non-empty
    pub const NonEmptySlice = List.NonEmptySlice;
    /// ??? Type annotated pattern e.g. `x: U64`, do we properly support this in a when branch in Roc?
    pub const Typed = struct {
        /// e.g `x` in `x: U64`
        pattern: Pattern.Idx,
        /// e.g `U64` in `x: U64`
        type: Type.Idx,
        /// List of Pattern.Typed
        pub const List = collections.SafeMultiList(@This());
        /// Reference to sublist into Pattern.Typed.List
        pub const Slice = Typed.List.Slice;
    };
};

/// Typed Identifier, e.g. `x: U64`
pub const TypedIdent = struct {
    /// e.g. `x` in `x: U64`
    pattern: Pattern.Idx,
    /// e.g. `U64` in `x: U64`
    type: Type.Idx,
    /// List of TypedIdent
    pub const List = collections.SafeMultiList(@This());
    /// Reference to sublist into TypedIdent.List
    pub const Slice = List.Slice;
};
