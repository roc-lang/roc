//! The work done by a compiler stage for a module in a package, usually an IR.
//!
//! The [ModuleGraph] will determine the correct compilation order of packages
//! at time of import resolution by putting them in a row dependency-first.
//! Once that is done, all compiler stages will use the exact same order for
//! module compilation, meaning we can use the same indices for accessing data
//! in other modules based on the order of work in a [ModuleWork.Store].

const std = @import("std");
const base = @import("../base.zig");
const can = @import("../check/canonicalize.zig");
const collections = @import("../collections.zig");

const Package = base.Package;
const exitOnOom = collections.utils.exitOnOom;

/// An index that works for all ModuleWork.Store collections.
///
/// This allows references to a specific module's work for any value,
/// irrespective of which compiler stage is being referenced.
pub const ModuleWorkIdx = enum(u32) { _ };

/// An iterator over all module work indices.
pub const ModuleWorkIndexIter = struct {
    current: u32,
    len: u32,

    pub fn next(self: *ModuleWorkIndexIter) ?ModuleWorkIdx {
        if (self.current == self.len) return null;

        const out: ModuleWorkIdx = @enumFromInt(self.current);
        self.current += 1;

        return out;
    }
};

/// The work done by a compiler stage for a module in a package, usually an IR.
pub fn ModuleWork(comptime Work: type) type {
    return struct {
        package_idx: Package.Idx,
        module_idx: Package.Module.Idx,
        work: Work,

        /// A collection of the work for all modules in all packages.
        ///
        /// This does not allow addition of individual modules which ensures
        /// that `ModuleWorkIdx`s are valid and correct (AKA point to the same module)
        /// across all types of work during compilation.
        pub const Store = struct {
            items: std.MultiArrayList(ModuleWork(Work)),

            /// Create a `ModuleWork.Store` from
            ///  create a Store from a slice of ModuleWork(can.IR)
            pub fn fromCanIrs(
                gpa: std.mem.Allocator,
                can_irs: []const ModuleWork(can.CIR),
            ) Store {
                var items = std.MultiArrayList(ModuleWork(Work)){};
                items.ensureTotalCapacity(gpa, can_irs.len) catch |err| exitOnOom(err);

                for (can_irs) |work| {
                    items.appendAssumeCapacity(.{
                        .package_idx = work.package_idx,
                        .module_idx = work.module_idx,
                        .work = work.work,
                    });
                }

                return Store{ .items = items };
            }

            /// Create a `ModuleWork.Store` using the `ModuleEnv`s and same module order
            /// of a different `ModuleWork.Store`.
            ///
            /// All `ModuleWork.Store`s should be made with the method except for the initial
            /// `can.IR` store. The `Work` type must define an `init` method that takes a pointer
            /// to a `ModuleEnv` struct.
            pub fn initFromCanIrs(
                gpa: std.mem.Allocator,
                can_irs: *const ModuleWork(can.CIR).Store,
            ) Store {
                var items = std.MultiArrayList(ModuleWork(Work)){};
                items.ensureTotalCapacity(gpa, can_irs.items.len) catch |err| exitOnOom(err);

                for (0..can_irs.items.len) |index| {
                    const work_idx: ModuleWorkIdx = @enumFromInt(index);

                    items.appendAssumeCapacity(.{
                        .package_idx = can_irs.getPackageIdx(work_idx),
                        .module_idx = can_irs.getModuleIdx(work_idx),
                        .work = Work.init(&can_irs.getWork(work_idx).env),
                    });
                }

                return Store{ .items = items };
            }

            /// Deinitialize a `ModuleWork.Store`'s memory.
            pub fn deinit(self: *Store, gpa: std.mem.Allocator) void {
                for (0..self.items.len) |index| {
                    self.items.items(.work)[index].deinit();
                }

                self.items.deinit(gpa);
            }

            /// Iterate over all indices of the work in this store.
            pub fn iterIndices(self: *const Store) ModuleWorkIndexIter {
                return ModuleWorkIndexIter{
                    .current = 0,
                    .len = @truncate(self.items.len),
                };
            }

            /// Get the `Package.Idx` for this work.
            pub fn getPackageIdx(self: *const Store, idx: ModuleWorkIdx) Package.Idx {
                return self.items.items(.package_idx)[@as(usize, @intFromEnum(idx))];
            }

            /// Get the `Package.Module.Idx` for this work.
            pub fn getModuleIdx(self: *const Store, idx: ModuleWorkIdx) Package.Module.Idx {
                return self.items.items(.module_idx)[@as(usize, @intFromEnum(idx))];
            }

            /// Get the work from this container.
            pub fn getWork(self: *const Store, idx: ModuleWorkIdx) *Work {
                return &self.items.items(.work)[@as(usize, @intFromEnum(idx))];
            }

            /// Get the name of the package module for this container.
            pub fn getModule(self: *const Store, idx: ModuleWorkIdx, packages: *const Package.Store) *Package.Module {
                const package_idx = self.getPackageIdx(idx);
                const package = packages.packages.get(package_idx);
                const module_idx = self.getModuleIdx(idx);
                const module = package.modules.get(module_idx);

                return module;
            }
        };
    };
}
