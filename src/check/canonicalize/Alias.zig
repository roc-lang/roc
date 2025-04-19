//! A type that references another type.

const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const problem_mod = @import("../../problem.zig");
const collections = @import("../../collections.zig");

const Region = base.Region;
const Problem = problem_mod.Problem;
const TypeIdx = types.Type.Idx;
const Ident = base.Ident;

name: Ident.Idx,
region: Region,
/// Aliases for types that are defined in Zig instead of Roc,
/// like List and Box.
is_builtin: bool,
// pub typ: Type,
kind: Kind,

/// A list of aliases.
pub const List = collections.SafeMultiList(@This());
/// An index into a SafeMultiList of aliases.
pub const Idx = List.Idx;
/// A range in a list of aliases.
pub const Range = List.Range;

/// The kind of an alias, as exposed in a siloed module.
pub const Kind = union(enum) {
    /// An alias that could be either structural or nominal.
    imported_unknown,
    /// An alias that must be nominal because it has exposed tags
    /// associated with its import.
    imported_nominal_union,
    /// A locally-defined nominal alias.
    nominal: Nominal,
    /// A locally-defined structural alias.
    structural: Structural,
    /// An invalid alias that can still be referred to by definitions.
    malformed: Malformed,
};

/// The data for a nominal alias, e.g. `Foo := [Foo(Str)]`
pub const Nominal = struct {
    type_variables: Var.Range,
    recursion_variables: std.AutoHashMap(TypeIdx, Ident.Idx),
};
/// The data for a structural alias, e.g. `Foo : { bar : Str }`
pub const Structural = struct {
    type_variables: Var.Range,
};
/// A malformed alias that can still be referred to by other entities.
pub const Malformed = struct {
    problem: Problem,
};

/// A type variable defined at header of an alias to refer to a type
/// variable within its definition.
pub const Var = struct {
    name: Ident.Idx,
    region: Region,
    type_var: TypeIdx,

    /// A list of alias type variables.
    pub const List = collections.SafeMultiList(@This());
    /// A slice of alias type variables.
    pub const Range = Var.List.Range;
};
