//! Package environment and metadata for multi-phase, dependency-aware builds.

const std = @import("std");
const SmallStringInterner = @import("mod.zig").SmallStringInterner;

/// A small integer identifier for a package, backed by the small string interner.
pub const PackageId = SmallStringInterner.Idx;

/// The phase of a package within the build pipeline.
pub const Phase = enum(u8) {
    /// Phase 1: discover dependencies (e.g. parse header) and report back to the build environment.
    Phase1_DiscoverDeps,
    /// Phase 2: fully canonicalize the package. After Phase 2, the package must wait
    /// for all dependencies to complete before beginning Phase 3.
    Phase2_Canonicalize,
    /// Phase 2 complete; waiting for dependencies to finish Phase 3.
    WaitingForDependencies,
    /// Phase 3: type-check using dependencies' types. This is the final phase.
    Phase3_Typecheck,
    /// Fully complete. No more work remains for this package.
    Done,
};

/// Per-package state shared across the build.
pub const PackageEnv = struct {
    /// Interned identifier of this package (interned path).
    id: PackageId,

    /// A human-readable path for debugging; points into the interner's storage.
    path: []const u8,

    /// Current phase of the package. Access to this value must be synchronized
    /// by the owning BuildEnv.
    phase: Phase = .Phase1_DiscoverDeps,

    /// Dependencies this package requires, represented as interned PackageIds.
    dependencies: std.ArrayListUnmanaged(PackageId) = .{},

    /// Dependents which require this package. Used to notify when we complete.
    dependents: std.ArrayListUnmanaged(PackageId) = .{},

    /// Number of dependencies which still need to fully complete (reach Done)
    /// before this package can begin Phase 3. This is derived from `dependencies`
    /// and is updated as dependencies finish.
    remaining_deps_to_finish: usize = 0,

    pub fn init(id: PackageId, path: []const u8) PackageEnv {
        return .{ .id = id, .path = path };
    }

    pub fn deinit(self: *PackageEnv, gpa: std.mem.Allocator) void {
        self.dependencies.deinit(gpa);
        self.dependents.deinit(gpa);
    }
};
