const std = @import("std");
const cols = @import("../collections.zig");
const ident = @import("Ident.zig");
const region = @import("Region.zig");
const problem = @import("../problem.zig");

pub const Module = @This();

name: []u8,
package_shorthand: ?[]u8,
is_builtin: bool,
ident_store: ident.Ident.Store,

pub const List = cols.SafeMultiList(Module);
pub const Idx = List.Idx;

/// Index for a specific identifier and a specific module.
pub const Ident = struct {
    module_id: Idx,
    ident_id: ident.Ident.Idx,
};

pub const Store = struct {
    modules: List,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Store {
        const modules = cols.SafeMultiList(Module).init(allocator);
        modules.append(Module{
            .name = &.{},
            .package_shorthand = null,
            .is_builtin = false,
            .ident_store = ident.Ident.Store.init(allocator),
        });

        // TODO: insert builtins automatically?

        return Store{
            .modules = modules,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Store) void {
        for (self.modules.items.items(.ident_store)) |store| {
            store.deinit();
        }

        self.modules.deinit();
    }

    pub fn lookup(
        self: *Store,
        name: []const u8,
        package_shorthand: ?[]const u8,
    ) ?Idx {
        // TODO: this doesn't handle multiple packages with the same shorthand yet,
        // e.g. my app with json: "..." depends also on yaml: "..." which depends on json: "..."
        const items = self.modules.items;

        for (0..self.modules.len()) |index| {
            const other_name = items.items(.name_segments)[index];
            if (name == other_name) {
                const other_package_shorthand = items.items(.package_shorthand)[index];
                if (other_package_shorthand == package_shorthand) {
                    return Idx{ .id = @as(u32, index) };
                }
            }
        }

        return null;
    }

    pub fn getOrInsert(
        self: *Store,
        name: []const u8,
        package_shorthand: ?[]const u8,
    ) Idx {
        if (self.lookup(name, package_shorthand)) |id| {
            return id;
        } else {
            const new_id = self.modules.insert(Module{
                .name = name,
                .package_shorthand = package_shorthand,
                .is_builtin = false,
                .ident_store = ident.Ident.Store.init(self.allocator),
            });

            return Idx{ .id = new_id.id };
        }
    }

    pub fn getName(self: *Store, id: Idx) []u8 {
        return self.modules.items.items(.name)[@as(usize, id.id)];
    }

    pub fn getPackageShorthand(self: *Store, id: Idx) ?[]u8 {
        return self.modules.items.items(.package_shorthand)[@as(usize, id.id)];
    }

    pub fn insertIdent(
        self: *Store,
        ident_: ident.Ident,
        region_: region.Region,
        module_id: Idx,
        problems: *std.ArrayList(problem.Problem),
    ) Ident {
        const index = @as(usize, module_id.id);
        const ident_store = self.modules.items.items(.ident_store)[index];
        const ident_id = ident_store.insert(ident_, region_, problems);

        return Ident{ .ident_id = ident_id, .module_id = module_id };
    }

    pub fn getIdentText(self: *Store, module_ident: Ident) []u8 {
        const index = @as(usize, module_ident.module_id.id);
        const ident_store = self.modules.items.items(.ident_store)[index];
        return ident_store.getText(module_ident.ident_id);
    }

    pub fn getIdentRegion(self: *Store, module_ident: Ident) region.Region {
        const index = @as(usize, module_ident.module_id.id);
        const ident_store = self.modules.items.items(.ident_store)[index];
        return ident_store.getRegion(module_ident.ident_id);
    }
};
