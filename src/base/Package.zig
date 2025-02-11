//! A package imported at the root of a Roc application/platform/package.
const std = @import("std");
const base = @import("../base.zig");
const collections = @import("../collections.zig");

// TODO: this is half-baked, we should finish it when we get to saving/loading packages

const Package = @This();

/// The full download URL for the package, including the name, content hash, and version.
download_url: []u8,

/// The BLAKE3 hash of the tarball's contents. Also the .tar filename on disk.
content_hash: []u8,

/// On disk, this will be the subfolder inside the cache dir where the package lives
cache_subdir: []u8,

/// The version of this package, AKA 1.2.3
version_string: []u8,

/// Other code will default this to main.roc, but this module isn't concerned with that default.
root_module_filename: ?[]u8,

/// All files in this package, relative to the packages root.
relative_file_paths: [][]u8,

dependencies: std.AutoHashMap([]u8, Dependency),

const List = collections.SafeMultiList(@This());
pub const Idx = List.Idx;

pub const Dependency = struct {
    package: Idx,
    shorthand_region: base.Region,
    url_region: base.Region,

    pub const AddResult = union(enum) {
        Success,
        DuplicateShorthand: Dependency,
        DuplicateUrl: Dependency,
    };
};

pub const Store = struct {
    packages: List,
    allocator: std.mem.Allocator,

    pub fn init(
        primary_filename: []u8,
        all_primary_relative_paths: [][]u8,
        allocator: std.mem.Allocator,
    ) !Store {
        var packages = List.init(allocator);
        try packages.items.append(allocator, Package{
            .content_hash = &.{},
            .download_url = &.{},
            .cache_subdir = &.{},
            .version_string = &.{},
            .root_module_filename = primary_filename,
            .relative_file_paths = all_primary_relative_paths,
            .dependencies = std.AutoHashMap([]u8, Dependency).init(allocator),
        });

        return Store{ .packages = packages, .allocator = allocator };
    }

    pub fn deinit(self: *Store) void {
        self.packages.deinit();
    }

    pub fn insert(self: *Store, package: Package) !void {
        try self.packages.items.append(self.allocator, package);
    }

    pub fn addDependencyToPackage(
        self: *Store,
        package_idx: Idx,
        dependency: Dependency,
    ) Dependency.AddResult {
        const idx = @intFromEnum(package_idx);
        const pkg = self.packages.items.get(idx);
        var dep_iter = pkg.dependencies.iterator();
        while (dep_iter.next()) |entry| {
            if (std.meta.eql(entry.value_ptr.shorthand_region, dependency.shorthand_region)) {
                return Dependency.AddResult{
                    .DuplicateShorthand = entry.value_ptr.*,
                };
            }

            const dep_idx = @intFromEnum(entry.value_ptr.package);
            const dep_download_url = self.packages.items.items(.download_url)[dep_idx];
            if (std.mem.eql(u8, pkg.download_url, dep_download_url)) {
                return Dependency.AddResult{
                    .DuplicateUrl = entry.value_ptr.*,
                };
            }
        }

        // TODO: Insert the new dependency into the hashmap
        // Not sure what the dependency idx should be here
        // pkg.dependencies.put(dep_idx, dependency);

        return Dependency.AddResult.Success;
    }
};
