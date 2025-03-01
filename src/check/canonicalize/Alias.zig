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

pub const List = collections.SafeMultiList(@This());
pub const Idx = List.Idx;
pub const Slice = List.Slice;

pub const Kind = union(enum) {
    ImportedUnknown,
    ImportedCustomUnion,
    Custom: Custom,
    Structural: Structural,
    Malformed: Malformed,
};

pub const Custom = struct {
    type_variables: Var.Slice,
    recursion_variables: std.AutoHashMap(TypeIdx, Ident.Idx),
};

pub const Structural = struct {
    type_variables: Var.Slice,
};

pub const Malformed = struct {
    problem: Problem,
};

pub const Var = struct {
    name: Ident.Idx,
    region: Region,
    type_var: TypeIdx,

    pub const List = collections.SafeMultiList(@This());
    pub const Slice = Var.List.Slice;
};
