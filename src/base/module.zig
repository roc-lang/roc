const std = @import("std");
const cols = @import("../collections.zig");

pub const ModuleId = struct { id: u32 };

pub const Module = struct {
    name: []u8,
    package_shorthand: ?[]u8,
    is_builtin: bool,
};

pub const ModuleStore = struct {
    modules: cols.SafeMultiList(Module),

    pub fn init(allocator: std.mem.Allocator) ModuleStore {
        const modules = cols.SafeMultiList(Module).init(allocator);
        modules.append(Module{
            .id = 0,
            .name = &.{},
            .base_name = &.{},
            .package_shorthand = null,
            .is_builtin = false,
        });

        // TODO: insert builtins automatically?

        return ModuleStore{ .modules = modules };
    }

    pub fn deinit(self: *ModuleStore) void {
        self.modules.deinit();
    }

    pub fn lookup(self: *ModuleStore, name: []const u8, package_shorthand: ?[]const u8) ?ModuleId {
        const items = self.modules.items;

        for (0..self.modules.len()) |index| {
            const other_name = items.items(.name_segments)[index];
            if (name == other_name) {
                const other_package_shorthand = items.items(.package_shorthand)[index];
                if (other_package_shorthand == package_shorthand) {
                    return ModuleId{ .id = @as(u32, index) };
                }
            }
        }

        return null;
    }

    pub fn getOrInsert(
        self: *ModuleStore,
        name: []const u8,
        package_shorthand: ?[]const u8,
    ) ModuleId {
        if (self.lookup(name, package_shorthand)) |id| {
            return id;
        } else {
            const new_id = self.modules.insert(Module{
                .name = name,
                .package_shorthand = package_shorthand,
                .is_builtin = false,
            });

            return ModuleId{ .id = new_id.id };
        }
    }

    pub fn getName(self: *ModuleStore, id: ModuleId) []u8 {
        return self.modules.items.items(.name)[@as(usize, id.id)];
    }

    pub fn getPackageShorthand(self: *ModuleStore, id: ModuleId) ?[]u8 {
        return self.modules.items.items(.package_shorthand)[@as(usize, id.id)];
    }
};
