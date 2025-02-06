const base = @import("base.zig");
const collections = @import("collections.zig");

const Problem = @This();

pub const List = collections.SafeList(union(enum) {
    parse: Problem.Parse,
    specialize_types: Problem.SpecializeTypes,
    lift_functions: Problem.LiftFunctions,
    solve_functions: Problem.SolveFunctions,
    specialize_functions: Problem.SpecializeFunctions,
    lower_ir: Problem.LowerIr,
    reference_count: Problem.ReferenceCount,
}).Slice;

pub const Parse = enum {};
pub const SpecializeTypes = enum {};
pub const LiftFunctions = enum {};
pub const SolveFunctions = enum {};
pub const SpecializeFunctions = enum {};
pub const LowerIr = enum {};
pub const ReferenceCount = enum {};
