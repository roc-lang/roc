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

/// todo
pub const List = collections.SafeMultiList(@This());
/// todo
pub const Idx = List.Idx;
/// todo
pub const Slice = List.Slice;
/// todo
pub const Kind = union(enum) {
    ImportedUnknown,
    ImportedCustomUnion,
    Custom: Custom,
    Structural: Structural,
    Malformed: Malformed,
};
/// todo
pub const Custom = struct {
    type_variables: Var.Slice,
    recursion_variables: std.AutoHashMap(TypeIdx, Ident.Idx),
};
/// todo
pub const Structural = struct {
    type_variables: Var.Slice,
};
/// todo
pub const Malformed = struct {
    problem: Problem,
};
/// todo
pub const Var = struct {
    name: Ident.Idx,
    region: Region,
    type_var: TypeIdx,
    /// todo
    pub const List = collections.SafeMultiList(@This());
    /// todo
    pub const Slice = Var.List.Slice;
};
