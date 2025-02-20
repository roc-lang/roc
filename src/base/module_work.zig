const std = @import("std");
const base = @import("../base.zig");
const collections = @import("../collections.zig");

const Package = base.Package;
const exitOnOom = collections.utils.exitOnOom;

/// An index that works for all ModuleWork.Store collections.
///
/// This allows references to a specific module's work for any value,
/// irrespective of which compiler stage is being referenced.
pub const ModuleWorkIdx = enum(u32) { _ };

pub fn ModuleWork(comptime Work: type) type {
    return struct {
        package_idx: Package.Idx,
        module_idx: Package.Module.Idx,
        work: *Work,

        pub const Store = struct {
            items: std.ArrayList(ModuleWork(Work)),

            pub fn init(allocator: std.mem.Allocator) Store {
                return Store{
                    .items = std.ArrayList(ModuleWork(Work)).init(allocator),
                };
            }

            pub fn deinit(self: *Store) void {
                self.items.deinit();
            }

            pub fn insert(
                self: *Store,
                comptime prior_work_type: type,
                prior_work: ModuleWork(prior_work_type),
                work: ModuleWork(Work),
            ) ModuleWorkIdx {
                const len: u32 = @truncate(self.items.items.len);

                self.items.append(ModuleWork(prior_work_type){
                    .package_idx = prior_work.package_idx,
                    .module_idx = prior_work.module_idx,
                    .work = work,
                }) catch exitOnOom();

                return @enumFromInt(len);
            }

            pub fn get(self: *Store, idx: ModuleWorkIdx) ModuleWork(Work) {
                return self.items.items[@as(usize, @intFromEnum(idx))];
            }
        };
    };
}
