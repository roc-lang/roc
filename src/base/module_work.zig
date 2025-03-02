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

pub fn ModuleWork(comptime Work: type) type {
    return struct {
        package_idx: Package.Idx,
        module_idx: Package.Module.Idx,
        work: Work,

        pub const Store = struct {
            items: std.MultiArrayList(ModuleWork(Work)),

            pub fn fromCanIrs(
                gpa: std.mem.Allocator,
                can_irs: []ModuleWork(can.IR),
            ) Store {
                var items = std.MultiArrayList(ModuleWork(Work)){};
                items.ensureTotalCapacity(gpa, can_irs.len) catch exitOnOom();

                for (can_irs) |work| {
                    items.appendAssumeCapacity(.{
                        .package_idx = work.package_idx,
                        .module_idx = work.module_idx,
                        .work = work.work,
                    });
                }

                return Store{ .items = items };
            }

            pub fn initFromCanIrs(
                gpa: std.mem.Allocator,
                can_irs: *const ModuleWork(can.IR).Store,
                init_work_with_env: *const fn (work: *Work, env: *base.ModuleEnv, gpa: std.mem.Allocator) void,
            ) Store {
                var items = std.MultiArrayList(ModuleWork(Work)){};
                items.ensureTotalCapacity(gpa, can_irs.items.len) catch exitOnOom();

                for (0..can_irs.items.len) |index| {
                    const work_idx: ModuleWorkIdx = @enumFromInt(index);

                    items.appendAssumeCapacity(.{
                        .package_idx = can_irs.getPackageIdx(work_idx),
                        .module_idx = can_irs.getModuleIdx(work_idx),
                        .work = undefined,
                    });

                    init_work_with_env(&items.items(.work)[index], &can_irs.getWork(work_idx).env, gpa);
                }

                return Store{ .items = items };
            }

            pub fn deinit(self: *Store, gpa: std.mem.Allocator) void {
                self.items.deinit(gpa);
            }

            pub fn iterIndices(self: *const Store) ModuleWorkIndexIter {
                return ModuleWorkIndexIter{
                    .current = 0,
                    .len = @truncate(self.items.len),
                };
            }

            pub fn getPackageIdx(self: *const Store, idx: ModuleWorkIdx) Package.Idx {
                return self.items.items(.package_idx)[@as(usize, @intFromEnum(idx))];
            }

            pub fn getModuleIdx(self: *const Store, idx: ModuleWorkIdx) Package.Module.Idx {
                return self.items.items(.module_idx)[@as(usize, @intFromEnum(idx))];
            }

            pub fn getWork(self: *const Store, idx: ModuleWorkIdx) *Work {
                return &self.items.items(.work)[@as(usize, @intFromEnum(idx))];
            }

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
