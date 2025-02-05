const std = @import("std");

pub const PackageId = struct { id: u32 };

pub const Package = struct {
    id: PackageId,
    /// The BLAKE3 hash of the tarball's contents. Also the .tar filename on disk.
    content_hash: []u8,
    /// On disk, this will be the subfolder inside the cache dir where the package lives
    cache_subdir: []u8,
    /// Other code will default this to main.roc, but this module isn't concerned with that default.
    root_module_filename: ?[]u8,
};

pub const PackageStore = struct {
    allocator: std.mem.Allocator,
    packages: std.MultiArrayList(Package),

    pub fn init(allocator: std.mem.Allocator) PackageStore {
        return PackageStore{
            .allocator = allocator,
            .packages = std.MultiArrayList(Package),
        };
    }

    pub fn deinit(self: *PackageStore) void {
        self.packages.deinit();
    }
};
