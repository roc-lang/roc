//! Manages module imports and their metadata

const std = @import("std");

const Import = @This();

/// Index into the Import Store
pub const Idx = enum(u16) { _ };

/// A store for interning imported module names
pub const Store = struct {
    /// Map from module name string to Import.Idx
    map: std.StringHashMapUnmanaged(Import.Idx) = .{},
    /// List of imports indexed by Import.Idx
    imports: std.ArrayListUnmanaged([]u8) = .{},
    /// Storage for module name strings
    strings: std.ArrayListUnmanaged(u8) = .{},

    pub fn init() Store {
        return .{};
    }

    pub fn deinit(self: *Store, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
        for (self.imports.items) |import| {
            allocator.free(import);
        }
        self.imports.deinit(allocator);
        self.strings.deinit(allocator);
    }

    pub fn intern(self: *Store, allocator: std.mem.Allocator, module_name: []const u8) !Import.Idx {
        if (self.map.get(module_name)) |idx| {
            return idx;
        }

        const idx = @as(Import.Idx, @enumFromInt(self.imports.items.len));
        const owned_name = try allocator.dupe(u8, module_name);
        try self.imports.append(allocator, owned_name);
        try self.map.put(allocator, owned_name, idx);
        return idx;
    }

    pub fn get(self: *const Store, idx: Import.Idx) []const u8 {
        return self.imports.items[@intFromEnum(idx)];
    }
};
