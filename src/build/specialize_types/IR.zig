//! The intermediate representation (IR) for a Roc module that has been monomorphized.
//!
//! Monomorphization, also known as type specialization, is the process of creating a distinct copy
//! of each instance of a generic function or value based on all specific usages in a program.
//! For example; a function with the type `Num a -> Num a` may only be called in the program with a
//! `U64` and a `I64`. Specialization will then create two functions with the types `U64 -> U64` and
//! `I64 -> I64`.
//! This trades off some compile time for a much better runtime performance, since we don't need to
//! look up which implementation to call at runtime (AKA dynamic dispatch).
//!
//! Doing type specialization as the first build stage helps simplify compilation of lambda sets, or
//! values captured by closures.
//!
//! The values in this module represent all data for a single type-specialized module, except for
//! interned data which resides in the `ModuleEnv`. This IR has a very similar structure to the next
//! stage's IR (`lift_functions`), but not quite the same. For now, we have designed our compiler
//! stages to be simple and correct at the cost of not deduplicating similar code. In the future,
//! we may decide to combine the IRs of some build stages to avoid needing to convert lots of
//! equivalent data.

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
/// All types in the module
types: Type.List,
/// All expressions in the module
exprs: Expr.List,
/// All typed expressions in the module
typed_exprs: Expr.Typed.List,
/// All patterns (for pattern matching, destructuring, var name) in the module
patterns: Pattern.List,
/// All typed patterns in the module
typed_patterns: Pattern.Typed.List,
/// All typed identifiers in the module
typed_idents: TypedIdent.List,
/// All match branches in the module
match_branches: MatchBranch.List,

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
        .match_branches = .{},
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
    self.match_branches.deinit(self.env.gpa);
}

/// Represents a concrete (no type variables,...) Roc type e.g. `U64`, `Str`, `List(U64)`.
pub const Type = union(enum) {
    /// e.g. Int, Bool, Str
    primitive: types_module.Primitive,
    /// Holds unknown Roc type for use with platform or can be used to improve perf in rare cases https://www.roc-lang.org/builtins/Box
    box: Type.Idx,
    /// e.g. List(U64)
    list: Type.Idx,
    /// Records, tuples and tag union payload become structs
    @"struct": Type.NonEmptySlice,
    /// e.g. `[Red, Yellow, Green]`
    tag_union: Type.NonEmptySlice,
    /// A function that has a return value and 0 or more arguments
    func: struct {
        /// A slice containing the return value followed by the arguments
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
    /// Represents the empty record `{}`
    unit,
    /// Record or tuple becomes struct
    @"struct": Expr.NonEmptySlice,
    /// Record or tuple access
    struct_access: struct {
        record_expr: Expr.Idx,
        record_type: Type.Idx,
        field_type: Type.Idx,
        field_id: Ident.Idx,
    },
    /// e.g. `Ok(1)`
    tag: struct {
        /// Numeric representation of the tag variant
        discriminant: u16,
        /// e.g. `Result` for `Ok(1)`
        tag_union_type: Type.Idx,
        /// e.g. `1` for `Ok(1)`
        args: Expr.Typed.Slice,
    },
    /// from pattern matching with `match`
    match: struct {
        /// The value being matched on, e.g. `match value is`
        value: Expr.Idx,
        /// The type of the value being matched on
        value_type: Type.Idx,
        /// The return type of all branches and thus the whole match expression
        branch_type: Type.Idx,
        /// The branches of the match expression
        branches: MatchBranch.NonEmptySlice,
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
    /// Expression with accompanying type
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
    /// For what's to the left of `=`, could be a var name or destructuring
    pattern: Pattern.Idx,
    /// Named variables in the pattern, e.g. `a` and `b` in `{a, b} = ...`
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

/// Branch of a `match` expression, e.g. `Green -> "green"`
pub const MatchBranch = struct {
    /// The pattern(s) to match the value against
    patterns: Pattern.NonEmptySlice,
    /// A boolean expression that must be true for this branch to be taken
    guard: ?Expr.Idx,
    /// The expression to produce if the pattern matches
    value: Expr.Idx,
    /// List of MatchBranch
    pub const List = collections.SafeList(@This());
    /// Reference to non-empty sublist into MatchBranch.List
    pub const NonEmptySlice = List.NonEmptySlice;
};

/// e.g. `|x| x * 2`
pub const Function = struct {
    args: Pattern.Slice,
    return_type: Type.Idx,
    /// ??? Is expr the body of the function?
    expr: Expr.Idx,
};

/// For record/tuple destructuring, e.g. `{ x, y: 123 } -> ...` contains two StructDestruct: one for `x` and one for `y`.
pub const StructDestruct = struct {
    ident: Ident.Idx,
    field: Ident.Idx,
    kind: Kind,
    pub const Kind = union(enum) {
        /// e.g `x` in `{ x, y: 123 }` is required
        required,
        /// e.g. .{ .num_literal = 123 } in `{ x, y: 123 }`. Keeping the value of y around is not required
        guard: Pattern.Typed,
    };
    /// List of StructDestruct
    pub const List = collections.SafeMultiList(@This());
    /// Reference to sublist into StructDestruct.List
    pub const Slice = List.Slice;
};

/// Represents a pattern used in pattern matching e.g. `Ok x` as part of a match branch
pub const Pattern = union(enum) {
    /// e.g. `x`
    identifier: Ident.Idx,
    /// e.g. `{ x, y } as data` in `{ x, y } as data -> call(data)`
    as: struct {
        /// e.g. `{ x, y }` in `{ x, y } as data`
        pattern: Pattern.Idx,
        /// e.g. `data` in `{ x, y } as data`
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
        /// e.g. for `{ x, y: 123 } -> ...` there are two StructDestruct; one for x and one for y
        destructs: StructDestruct.Slice,
        /// e.g. `..` in `{x, y, ..}`
        opt_spread: ?Pattern.Typed,
    },
    /// e.g. [Foo, Bar, ..]
    list: struct {
        elem_type: Type.Idx,
        /// e.g. Foo and Bar in `[Foo, Bar, ..]`
        patterns: Pattern.Slice,
        /// refers to e.g. `..tail` in `[head, ..tail]`
        opt_rest: ?struct {
            /// position in list of `..`; e.g. 0 in `[.., Foo, Bar]`
            offset: u16,
            /// e.g. tail in `..tail`
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
    /// Pattern with accompanying type
    pub const Typed = struct {
        pattern: Pattern.Idx,
        type: Type.Idx,
        /// List of Pattern.Typed
        pub const List = collections.SafeMultiList(@This());
        /// Reference to sublist into Pattern.Typed.List
        pub const Slice = Typed.List.Slice;
    };
};

/// Identifier along with its type
pub const TypedIdent = struct {
    /// e.g. `x`
    pattern: Pattern.Idx,
    type: Type.Idx,
    /// List of TypedIdent
    pub const List = collections.SafeMultiList(@This());
    /// Reference to sublist into TypedIdent.List
    pub const Slice = List.Slice;
};
