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

dependencies: std.AutoHashMap([]u8, Dependency),

pub const List = collections.SafeMultiList(@This());
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

    pub fn init(root_filename: []u8, allocator: std.mem.Allocator) Store {
        const packages = List.init(allocator);
        packages.append(Package{
            .content_hash = &.{},
            .cache_subdir = &.{},
            .version_string = &.{},
            .root_module_filename = root_filename,
            .dependencies = std.AutoHashMap([]u8, Dependency).init(allocator),
        });

        return Store{ .packages = packages, .allocator = allocator };
    }

    pub fn deinit(self: *Store) void {
        self.packages.deinit();
    }

    pub fn insert(self: *Store, package: Package) void {
        self.packages.append(package);
    }

    pub fn addDependencyToPackage(
        self: *Store,
        package_idx: Idx,
        dependency: Dependency,
    ) Dependency.AddResult {
        const idx = @intFromEnum(package_idx);
        const download_url = self.packages.items.items(.download_url)[idx];
        const package_deps = self.packages.items.items(.dependencies)[idx];

        var dep_iter = package_deps.iterator();
        while (dep_iter.next()) |entry| {
            if (entry.key_ptr == dependency.shorthand_region) {
                return Dependency.AddResult{
                    .DuplicateShorthand = entry.value_ptr.*,
                };
            }

            const dep_idx = @intFromEnum(entry.value_ptr.package);
            const dep_download_url = self.packages.items.items(.download_url)[dep_idx];
            if (download_url == dep_download_url) {
                return Dependency.AddResult{
                    .DuplicateUrl = entry.value_ptr.*,
                };
            }
        }

        package_deps.append(dependency);

        return Dependency.AddResult{.Success};
    }
};
