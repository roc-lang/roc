//! Publication inputs produced by inference, before evaluation. These records
//! accompany the solved source in the inference cache; a hit republishes them
//! against current implementation artifacts.

const std = @import("std");
const collections = @import("collections");
const can = @import("can");
const types = @import("types");
const roots = @import("hoist_roots.zig");
const dependencies = @import("checking_dependencies.zig");
const base = @import("base");
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

/// One producer-owned slot and its source index in the stored dependency.
pub const ForeignReference = extern struct { owner: u32, node: u32, slot: u32 };

outcome: @import("checking_outcome.zig") = .{},
foreign_references: []const ForeignReference = &.{},
hoisted_roots: []const roots.SelectedHoistedRoot = &.{},
promoted_local_procedures: []const roots.PromotedLocalProcedure = &.{},
solutions: []const Solution = &.{},
identity_vars: []const types.Var = &.{},

/// Independently relocatable inference publication metadata.
pub const Serialized = extern struct {
    outcome: @import("checking_outcome.zig").Serialized = .{},
    foreign_references: serde.SerializedSlice(ForeignReference) = .{},
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
pub fn fromChecker(allocator: Allocator, checker: anytype, scope: *const dependencies.Scope) Allocator.Error!Self {
    var references = std.ArrayList(ForeignReference).empty;
    for (scope.owners.items, 0..) |hash, owner| {
        if (owner == 0 or scope.ordinal(hash) != owner) continue;
        const env = scope.environments.get(hash) orelse
            base.invariant("foreign reference tables belong to explicit dependency environments", .{});
        for (env.checking_dependency.references.items.items, 0..) |node, slot| {
            try references.append(allocator, .{ .owner = @intCast(owner), .node = node, .slot = @intCast(slot) });
        }
    }
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
        .outcome = try @import("checking_outcome.zig").capture(allocator, checker, scope),
        .foreign_references = references.items,
        .hoisted_roots = checker.selectedHoistedRoots(),
        .promoted_local_procedures = checker.promotedLocalProcedures(),
        .solutions = solutions.items,
        .identity_vars = identities.items,
    };
}

/// Validate side-list ranges before installing untrusted cache metadata.
pub fn validate(self: Self) error{CorruptArtifact}!void {
    try self.outcome.validate();
    for (self.solutions) |solution| {
        if (@as(u64, solution.identities_start) + solution.identities_len > self.identity_vars.len)
            return error.CorruptArtifact;
    }
}

/// Restore the owning stage's explicit outputs. Initializing the checker only
/// provides diagnostics/publication services; checkFile is never called on a hit.
pub fn install(self: Self, checker: anytype, scope: *const dependencies.Scope) Allocator.Error!void {
    const allocator = checker.gpa;
    try self.outcome.install(checker);
    var binding = self.makeBinding(checker.cir, scope) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.IncompatibleReferences => base.invariant("admitted inference references remain valid during diagnostic installation", .{}),
    };
    defer binding.deinit(allocator);
    for (0..checker.snapshots.record_fields.len()) |i| {
        const id = @as(snapshotFieldIndex(), @fromBackingInt(@as(u32, @intCast(i))));
        var field = checker.snapshots.record_fields.get(id);
        types.store.rebindSourceReferenceValue(&field, &binding) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.IncompatibleReferences => base.invariant("admission established snapshot default correspondence", .{}),
        };
        checker.snapshots.record_fields.set(id, field);
    }
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

/// Admission is complete before the restored source replaces the live source.
/// A missing correspondence rejects the optional cache entry, never a compiler
/// invariant or a guessed mapping.
pub fn bindReferences(self: Self, env: *can.ModuleEnv, scope: *const dependencies.Scope) (Allocator.Error || error{IncompatibleReferences})!void {
    var binding = try self.makeBinding(env, scope);
    defer binding.deinit(env.gpa);
    for (self.outcome.record_fields) |record| {
        var field = record;
        try types.store.rebindSourceReferenceValue(&field, &binding);
    }
    try @import("reference_binding.zig").source(env, &binding);
}

fn makeBinding(self: Self, env: *can.ModuleEnv, scope: *const dependencies.Scope) Binding.Error!Binding {
    var binding = Binding{ .env = env, .scope = scope };
    errdefer binding.deinit(env.gpa);
    // All references share one owner domain. Preserve first-occurrence
    // ordinals once, instead of scanning that domain for every foreign node.
    var ordinals = std.AutoHashMapUnmanaged(base.ModuleIdentity.Hash, u32).empty;
    defer ordinals.deinit(env.gpa);
    for (env.checking_dependency.owners.items.items, 0..) |hash, ordinal| {
        const entry = try ordinals.getOrPut(env.gpa, hash);
        if (!entry.found_existing) entry.value_ptr.* = @intCast(ordinal);
    }
    binding.owners = try env.gpa.alloc(?Binding.Owner, env.module_identities.count());
    for (binding.owners, 0..) |*owner, index| {
        owner.* = null;
        const hash = env.moduleIdentityHash(@fromBackingInt(@as(u32, @intCast(index))));
        const ordinal = ordinals.get(hash.*) orelse continue;
        if (ordinal >= scope.owners.items.len) continue;
        const current = scope.environments.get(scope.owners.items[ordinal]) orelse continue;
        owner.* = .{ .ordinal = ordinal, .builtin = current.module_role == .builtin };
    }
    for (self.foreign_references) |reference| {
        if (reference.owner >= scope.owners.items.len) return error.IncompatibleReferences;
        const current = scope.environments.get(scope.owners.items[reference.owner]) orelse return error.IncompatibleReferences;
        if (reference.slot >= current.checking_dependency.references.items.items.len) return error.IncompatibleReferences;
        const entry = try binding.references.getOrPut(env.gpa, .{ .owner = reference.owner, .node = reference.node });
        if (entry.found_existing) return error.IncompatibleReferences;
        entry.value_ptr.* = current.checking_dependency.references.items.items[reference.slot];
    }
    return binding;
}

fn snapshotFieldIndex() type {
    return @import("snapshot.zig").SnapshotRecordFieldSafeList.Idx;
}

const Binding = struct {
    pub const Error = Allocator.Error || error{IncompatibleReferences};
    const Key = struct { owner: u32, node: u32 };
    const Owner = struct { ordinal: u32, builtin: bool };
    env: *can.ModuleEnv,
    scope: *const dependencies.Scope,
    owners: []?Owner = &.{},
    references: std.AutoHashMapUnmanaged(Key, u32) = .empty,

    fn deinit(self: *Binding, allocator: Allocator) void {
        allocator.free(self.owners);
        self.references.deinit(allocator);
    }

    pub fn importNode(self: *Binding, env: *const can.ModuleEnv, import_idx: u32, raw: u32) Error!u32 {
        const resolved = env.imports.getResolvedModule(@fromBackingInt(import_idx)) orelse return error.IncompatibleReferences;
        if (resolved >= self.scope.imported.len) return error.IncompatibleReferences;
        const current = self.scope.imported[resolved];
        if (current.module_role == .builtin) return raw;
        return self.references.get(.{ .owner = self.scope.ordinal(current.contentIdentityHash().?.*), .node = raw }) orelse error.IncompatibleReferences;
    }

    pub fn node(self: *Binding, owner: base.ModuleIdentity.Idx, raw: u32) Error!u32 {
        if (owner == self.env.selfModuleIdentity()) return raw;
        const bound = self.owners[@backingInt(owner)] orelse return error.IncompatibleReferences;
        if (bound.builtin) return raw;
        return self.references.get(.{ .owner = bound.ordinal, .node = raw }) orelse error.IncompatibleReferences;
    }
};

test "inference references require exact producer slots" {
    try testInferenceReferenceBinding(false);
}

test "inference reference indexes release partial allocation state" {
    try testInferenceReferenceBinding(true);
}

fn testInferenceReferenceBinding(allocation_failures: bool) error{ IncompatibleReferences, MemoryLeakDetected, NondeterministicMemoryUsage, OutOfMemory, SwallowedOutOfMemoryError, TestExpectedEqual, TestExpectedError, TestUnexpectedError }!void {
    const allocator = std.testing.allocator;
    var env = try can.ModuleEnv.init(allocator, "");
    defer env.deinit();
    try env.initCIRFields("Importer");
    try env.setContentIdentity(@splat(1));
    const foreign = try env.internModuleIdentity(&@as(base.ModuleIdentity.Hash, @splat(2)), base.Ident.Idx.NONE);
    const missing = try env.internModuleIdentity(&@as(base.ModuleIdentity.Hash, @splat(3)), base.Ident.Idx.NONE);
    var dependency = try can.ModuleEnv.init(allocator, "");
    defer dependency.deinit();
    try dependency.initCIRFields("Dependency");
    try dependency.setContentIdentity(@splat(2));
    dependency.checking_dependency.published = true;
    _ = try dependency.checking_dependency.references.append(allocator, 42);
    var scope = try dependencies.Scope.init(allocator, @splat(1));
    defer scope.deinit();
    try scope.appendDependency(&dependency);
    // Repeated owner parameters must still resolve through the first slot.
    _ = try scope.appendOwner(@splat(2));
    env.checking_dependency.owners = try scope.copyOwners(allocator);

    var references = [_]ForeignReference{.{ .owner = 1, .node = 17, .slot = 0 }};
    const output = Self{ .foreign_references = &references };
    if (allocation_failures) {
        // Admission only reads the already-owned source and dependency data.
        // Fail allocations in the new binding, independently of fixture setup.
        return std.testing.checkAllAllocationFailures(allocator, testBindingAllocation, .{ &env, &scope, output });
    }
    var binding = try output.makeBinding(&env, &scope);
    defer binding.deinit(allocator);
    try std.testing.expectEqual(@as(u32, 42), try binding.node(foreign, 17));
    try std.testing.expectEqual(@as(u32, 17), try binding.node(env.selfModuleIdentity(), 17));
    try std.testing.expectError(error.IncompatibleReferences, binding.node(missing, 17));
    try std.testing.expectError(error.IncompatibleReferences, binding.node(foreign, 42));
    references[0].slot = 1;
    try std.testing.expectError(error.IncompatibleReferences, output.makeBinding(&env, &scope));
    references[0] = .{ .owner = 3, .node = 17, .slot = 0 };
    try std.testing.expectError(error.IncompatibleReferences, output.makeBinding(&env, &scope));
    references[0] = .{ .owner = 1, .node = 17, .slot = 0 };
    const duplicates = Self{ .foreign_references = &.{ references[0], references[0] } };
    try std.testing.expectError(error.IncompatibleReferences, duplicates.makeBinding(&env, &scope));
}

fn testBindingAllocation(allocator: Allocator, env: *const can.ModuleEnv, scope: *const dependencies.Scope, output: Self) error{ IncompatibleReferences, OutOfMemory }!void {
    var borrowed = env.*;
    borrowed.gpa = allocator;
    var binding = try output.makeBinding(&borrowed, scope);
    defer binding.deinit(allocator);
}
