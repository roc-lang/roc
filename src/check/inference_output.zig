//! Publication inputs produced by inference, before evaluation. These records
//! accompany the solved source in the inference cache; a hit republishes them
//! against current implementation artifacts.

const std = @import("std");
const collections = @import("collections");
const can = @import("can");
const types = @import("types");
const roots = @import("hoist_roots.zig");
const requirement = @import("requirement_solution.zig");
const serde = @import("artifact_serialize.zig");

const Self = @This();
const Allocator = std.mem.Allocator;

/// A platform solution whose identity variables occupy one explicit side-list range.
pub const Solution = struct {
    requires_idx: u32,
    def: can.CIR.Def.Idx,
    solved_var: types.Var,
    is_function: bool,
    outcome: requirement.SolutionInput.Outcome,
    identities_start: u32,
    identities_len: u32,
};

hoisted_roots: []const roots.SelectedHoistedRoot = &.{},
promoted_local_procedures: []const roots.PromotedLocalProcedure = &.{},
solutions: []const Solution = &.{},
identity_vars: []const types.Var = &.{},

/// Independently relocatable inference publication metadata.
pub const Serialized = extern struct {
    hoisted_roots: serde.SerializedSlice(roots.SelectedHoistedRoot) = .{},
    promoted_local_procedures: serde.SerializedSlice(roots.PromotedLocalProcedure) = .{},
    solutions: serde.SerializedSlice(Solution) = .{},
    identity_vars: serde.SerializedSlice(types.Var) = .{},

    pub const root_contract: collections.serde_validation.RootContract = .{ .Owner = Self };
    const Serde = serde.SliceStoreSerde(Self, @This());
    pub const serialize = Serde.serialize;
    pub const deserialize = Serde.deserialize;
};

/// The caller supplies an arena; the root slices borrow the checker until the
/// capsule has been written, while solution rows flatten their identity lists.
pub fn fromChecker(allocator: Allocator, checker: anytype) Allocator.Error!Self {
    var solutions = std.ArrayList(Solution).empty;
    var identities = std.ArrayList(types.Var).empty;
    for (checker.platformRequirementSolutions()) |solution| {
        try solutions.append(allocator, .{
            .requires_idx = solution.requires_idx,
            .def = solution.def,
            .solved_var = solution.solved_var,
            .is_function = solution.is_function,
            .outcome = solution.outcome,
            .identities_start = std.math.cast(u32, identities.items.len) orelse return error.OutOfMemory,
            .identities_len = std.math.cast(u32, solution.identity_vars.len) orelse return error.OutOfMemory,
        });
        try identities.appendSlice(allocator, solution.identity_vars);
    }
    return .{
        .hoisted_roots = checker.selectedHoistedRoots(),
        .promoted_local_procedures = checker.promotedLocalProcedures(),
        .solutions = solutions.items,
        .identity_vars = identities.items,
    };
}

/// Validate side-list ranges before installing untrusted cache metadata.
pub fn validate(self: Self) error{CorruptArtifact}!void {
    for (self.solutions) |solution| {
        if (@as(u64, solution.identities_start) + solution.identities_len > self.identity_vars.len)
            return error.CorruptArtifact;
    }
}

/// Restore the owning stage's explicit outputs. Initializing the checker only
/// provides diagnostics/publication services; checkFile is never called on a hit.
pub fn install(self: Self, checker: anytype) Allocator.Error!void {
    const allocator = checker.gpa;
    try checker.selected_hoisted_roots.appendSlice(allocator, self.hoisted_roots);
    try checker.promoted_local_procedures.appendSlice(allocator, self.promoted_local_procedures);
    for (self.solutions) |solution| {
        const identities = try allocator.dupe(types.Var, self.identity_vars[solution.identities_start..][0..solution.identities_len]);
        errdefer allocator.free(identities);
        try checker.platform_requirement_solutions.append(allocator, .{
            .requires_idx = solution.requires_idx,
            .def = solution.def,
            .solved_var = solution.solved_var,
            .is_function = solution.is_function,
            .outcome = solution.outcome,
            .identity_vars = identities,
        });
    }
}
