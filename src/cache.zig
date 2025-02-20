const canonicalize = @import("check/canonicalize.zig");
pub const Package = @import("./cache/Package.zig");

// TODO: implement
pub fn getCanIrForHashAndRocVersion(file_hash: []u8, roc_version: []u8) !?canonicalize.IR {
    _ = file_hash;
    _ = roc_version;

    return null;
}
