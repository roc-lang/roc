const std = @import("std");
const base = @import("base.zig");
const canonicalize = @import("check/canonicalize.zig");

const Package = base.Package;

/// TODO: implement
pub fn getCanIrForHashAndRocVersion(file_hash: []const u8, roc_version: []const u8) ?canonicalize.IR {
    _ = file_hash;
    _ = roc_version;

    return null;
}

/// TODO: implement
pub fn getPackageRootAbsDir(url_data: Package.Url, gpa: std.mem.Allocator) []const u8 {
    _ = url_data;
    _ = gpa;

    @panic("not implemented");
}
