const std = @import("std");

const Package = @This();

id: Id,

/// The BLAKE3 hash of the tarball's contents. Also the .tar filename on disk.
content_hash: []u8,

/// On disk, this will be the subfolder inside the cache dir where the package lives
cache_subdir: []u8,

/// Other code will default this to main.roc, but this module isn't concerned with that default.
root_module_filename: ?[]u8,

pub const Id = struct { id: u32 };

pub const Store = struct {
    allocator: std.mem.Allocator,
    packages: std.MultiArrayList(Package),

    pub fn init(allocator: std.mem.Allocator) Store {
        return Store{
            .allocator = allocator,
            .packages = std.MultiArrayList(Package),
        };
    }

    pub fn deinit(self: *Store) void {
        self.packages.deinit();
    }
};
