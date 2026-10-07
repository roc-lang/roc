//! Recognize immutable Nix store objects used by compiler dependency identity.

const std = @import("std");

/// Only immutable Nix store objects may replace content hashing with their
/// path identity. Reject traversal paths even if their literal prefix matches.
pub fn isImmutableStorePath(path: []const u8) bool {
    const prefix = "/nix/store/";
    if (!std.mem.startsWith(u8, path, prefix)) return false;
    var components = std.mem.splitScalar(u8, path[prefix.len..], '/');
    const object = components.next().?;
    if (object.len < 34 or object[32] != '-') return false;
    for (object[0..32]) |character| {
        if (std.mem.findScalar(u8, "0123456789abcdfghijklmnpqrsvwxyz", character) == null) return false;
    }
    while (components.next()) |component| {
        if (std.mem.eql(u8, component, ".") or std.mem.eql(u8, component, "..")) return false;
    }
    return true;
}

test "immutable store identity requires a store object and no traversal" {
    const object = "/nix/store/0123456789abcdfghijklmnpqrsvwxyz-roc-deps";
    try std.testing.expect(isImmutableStorePath(object));
    try std.testing.expect(isImmutableStorePath(object ++ "/lib"));
    try std.testing.expect(!isImmutableStorePath(object ++ "/../mutable"));
    try std.testing.expect(!isImmutableStorePath(object ++ "/lib/../../mutable"));
    try std.testing.expect(!isImmutableStorePath(object ++ "/./lib"));
    try std.testing.expect(!isImmutableStorePath("/nix/store/../../tmp/bundle"));
    try std.testing.expect(!isImmutableStorePath("/nix/store/mutable-bundle"));
    try std.testing.expect(!isImmutableStorePath("/nix/store/0123456789abcdfghijklmnpqrsvwxye-roc-deps"));
    try std.testing.expect(!isImmutableStorePath("/tmp/bundle"));
}
