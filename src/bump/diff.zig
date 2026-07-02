//! Compares two `PackageApi` values into a semver magnitude and a structured
//! change list, following Elm's rules:
//!
//! - Added module -> MINOR. Removed module -> MAJOR.
//! - Within a module: added item -> MINOR; changed or removed item -> MAJOR.
//! - Overall magnitude is the maximum across all changes; no changes -> PATCH.
//!
//! Item equality is byte-equality of canonical S-expressions (see
//! `PackageApi`), so alpha-renaming of type variables is patch-level and any
//! other signature difference — loosening or tightening alike — is major.

const std = @import("std");
const base = @import("base");

const PackageApi = @import("PackageApi.zig");

pub const Magnitude = enum(u8) {
    patch = 0,
    minor = 1,
    major = 2,

    pub fn combine(self: Magnitude, other: Magnitude) Magnitude {
        return @enumFromInt(@max(@intFromEnum(self), @intFromEnum(other)));
    }

    pub fn name(self: Magnitude) []const u8 {
        return switch (self) {
            .patch => "PATCH",
            .minor => "MINOR",
            .major => "MAJOR",
        };
    }
};

/// Compute the next version from the old version and the required magnitude.
pub fn nextVersion(old: base.url.Version, magnitude: Magnitude) base.url.Version {
    return switch (magnitude) {
        .major => old.bumpMajor(),
        .minor => old.bumpMinor(),
        .patch => old.bumpPatch(),
    };
}

pub const ChangeKind = enum {
    module_added,
    module_removed,
    item_added,
    item_removed,
    item_changed,
};

pub const Change = struct {
    magnitude: Magnitude,
    kind: ChangeKind,
    module: []const u8,
    /// Dotted item path; empty for module-level changes.
    path: []const u8,
    /// Rendered signature on the old side (removed/changed items).
    old_rendered: ?[]const u8,
    /// Rendered signature on the new side (added/changed items).
    new_rendered: ?[]const u8,
};

pub const DiffResult = struct {
    arena: std.heap.ArenaAllocator,
    magnitude: Magnitude,
    changes: []Change,

    pub fn deinit(self: *DiffResult) void {
        self.arena.deinit();
    }
};

/// Both APIs must have been normalized.
pub fn diff(gpa: std.mem.Allocator, old: *const PackageApi, new: *const PackageApi) std.mem.Allocator.Error!DiffResult {
    var arena = std.heap.ArenaAllocator.init(gpa);
    errdefer arena.deinit();
    const alloc = arena.allocator();

    var changes = std.ArrayListUnmanaged(Change).empty;
    var magnitude = Magnitude.patch;

    // Merge-walk the name-sorted module lists.
    const old_modules = old.modules.items;
    const new_modules = new.modules.items;
    var old_i: usize = 0;
    var new_i: usize = 0;
    while (old_i < old_modules.len or new_i < new_modules.len) {
        const order: std.math.Order = if (old_i >= old_modules.len)
            .gt
        else if (new_i >= new_modules.len)
            .lt
        else
            std.mem.order(u8, old_modules[old_i].name, new_modules[new_i].name);

        switch (order) {
            .lt => {
                try changes.append(alloc, .{
                    .magnitude = .major,
                    .kind = .module_removed,
                    .module = try alloc.dupe(u8, old_modules[old_i].name),
                    .path = "",
                    .old_rendered = null,
                    .new_rendered = null,
                });
                magnitude = magnitude.combine(.major);
                old_i += 1;
            },
            .gt => {
                try changes.append(alloc, .{
                    .magnitude = .minor,
                    .kind = .module_added,
                    .module = try alloc.dupe(u8, new_modules[new_i].name),
                    .path = "",
                    .old_rendered = null,
                    .new_rendered = null,
                });
                magnitude = magnitude.combine(.minor);
                new_i += 1;
            },
            .eq => {
                const module_magnitude = try diffModule(
                    gpa,
                    alloc,
                    old,
                    &old_modules[old_i],
                    new,
                    &new_modules[new_i],
                    &changes,
                );
                magnitude = magnitude.combine(module_magnitude);
                old_i += 1;
                new_i += 1;
            },
        }
    }

    return .{
        .arena = arena,
        .magnitude = magnitude,
        .changes = changes.items,
    };
}

fn diffModule(
    gpa: std.mem.Allocator,
    alloc: std.mem.Allocator,
    old: *const PackageApi,
    old_module: *const PackageApi.ModuleApi,
    new: *const PackageApi,
    new_module: *const PackageApi.ModuleApi,
    changes: *std.ArrayListUnmanaged(Change),
) std.mem.Allocator.Error!Magnitude {
    var magnitude = Magnitude.patch;
    const module_name = try alloc.dupe(u8, new_module.name);

    // Merge-walk the path-sorted item lists.
    const old_items = old_module.items.items;
    const new_items = new_module.items.items;
    var old_i: usize = 0;
    var new_i: usize = 0;
    while (old_i < old_items.len or new_i < new_items.len) {
        const order: std.math.Order = if (old_i >= old_items.len)
            .gt
        else if (new_i >= new_items.len)
            .lt
        else
            std.mem.order(u8, old_items[old_i].path, new_items[new_i].path);

        switch (order) {
            .lt => {
                try changes.append(alloc, .{
                    .magnitude = .major,
                    .kind = .item_removed,
                    .module = module_name,
                    .path = try alloc.dupe(u8, old_items[old_i].path),
                    .old_rendered = try renderItem(gpa, alloc, old, old_items[old_i]),
                    .new_rendered = null,
                });
                magnitude = magnitude.combine(.major);
                old_i += 1;
            },
            .gt => {
                try changes.append(alloc, .{
                    .magnitude = .minor,
                    .kind = .item_added,
                    .module = module_name,
                    .path = try alloc.dupe(u8, new_items[new_i].path),
                    .old_rendered = null,
                    .new_rendered = try renderItem(gpa, alloc, new, new_items[new_i]),
                });
                magnitude = magnitude.combine(.minor);
                new_i += 1;
            },
            .eq => {
                const old_canonical = try old.itemCanonicalString(gpa, old_items[old_i]);
                defer gpa.free(old_canonical);
                const new_canonical = try new.itemCanonicalString(gpa, new_items[new_i]);
                defer gpa.free(new_canonical);

                if (!std.mem.eql(u8, old_canonical, new_canonical)) {
                    try changes.append(alloc, .{
                        .magnitude = .major,
                        .kind = .item_changed,
                        .module = module_name,
                        .path = try alloc.dupe(u8, new_items[new_i].path),
                        .old_rendered = try renderItem(gpa, alloc, old, old_items[old_i]),
                        .new_rendered = try renderItem(gpa, alloc, new, new_items[new_i]),
                    });
                    magnitude = magnitude.combine(.major);
                }
                old_i += 1;
                new_i += 1;
            },
        }
    }

    return magnitude;
}

fn renderItem(
    gpa: std.mem.Allocator,
    alloc: std.mem.Allocator,
    api: *const PackageApi,
    item: PackageApi.Item,
) std.mem.Allocator.Error![]const u8 {
    var out = std.Io.Writer.Allocating.init(gpa);
    defer out.deinit();
    api.renderItemSignature(gpa, item, &out.writer) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.WriteFailed => return error.OutOfMemory, // Allocating writer only fails on OOM.
    };
    return try alloc.dupe(u8, out.written());
}
