//! Cache-entry admission versioning.
//!
//! Every persisted cache entry carries an entry-version hash in its header, and a
//! read admits the entry only when that hash equals the running compiler's hash
//! for the entry's kind. This module is the only place those hashes are composed:
//! each entry kind's hash folds its envelope version and the layout fingerprint of
//! every serialization root its bodies hold, so the composition rule (what folds
//! into admission, in what order) exists exactly once. A layout fingerprint that
//! is not folded here guards no cache entry.
//!
//! Layout fingerprints come from `check.rootLayoutVersionHash`, which accepts only
//! a `Serialized` type that declares a `collections.serde_validation.RootContract`,
//! so every root an entry admits has had its owner/serialized field set audited.

const std = @import("std");
const can = @import("can");
const check = @import("check");

const canonicalized_cache_entry = @import("canonicalized_cache_entry.zig");
const Constants = @import("cache_config.zig").Constants;

const CheckedModuleArtifact = check.CheckedArtifact.CheckedModuleArtifact;
const PlatformPairing = check.CheckedArtifact.PlatformPairing;

/// A kind of persisted cache entry.
pub const Entry = enum {
    /// One fully checked module: an env body and an artifact body.
    checked_module,
    /// One exact platform/app composition's changed columns, written in the
    /// checked-module envelope with an empty env body.
    platform_pairing,
    /// One module's canonicalization output: an env body and the encoded
    /// parse-stage record.
    canonicalized_module,
};

/// Layout fingerprint of `ModuleEnv.Serialized`.
pub const module_env_layout: [32]u8 = check.rootLayoutVersionHash(can.ModuleEnv.Serialized, Constants.CACHE_VERSION);

/// Version of the checked-module envelope: its magic, header layout, and body
/// order. Platform-pairing entries share the envelope.
const checked_envelope_version: u32 = 13;

/// Version of the canonicalized-module envelope: its magic, header layout, and
/// body order.
const canonicalized_envelope_version: u32 = 3;

/// The running compiler's admission hash for `entry`.
pub fn entryVersionHash(comptime entry: Entry) [32]u8 {
    return comptime switch (entry) {
        .checked_module => compose(
            "roc-checked-module-entry-v1",
            &.{checked_envelope_version},
            &.{ CheckedModuleArtifact.SERIALIZED_VERSION_HASH, module_env_layout },
        ),
        .platform_pairing => compose(
            "roc-platform-pairing-entry-v1",
            &.{checked_envelope_version},
            &.{ CheckedModuleArtifact.SERIALIZED_VERSION_HASH, PlatformPairing.SERIALIZED_VERSION_HASH },
        ),
        .canonicalized_module => compose(
            "roc-canonicalized-module-entry-v1",
            &.{ canonicalized_envelope_version, canonicalized_cache_entry.format_version },
            &.{module_env_layout},
        ),
    };
}

/// Fold a domain tag, the manual versions, and the layout fingerprints, in that
/// order, with FNV-1a, and expand the fold to 32 bytes via splitmix64 so a change
/// to any input perturbs the whole hash.
fn compose(
    comptime domain: []const u8,
    comptime versions: []const u32,
    comptime fingerprints: []const [32]u8,
) [32]u8 {
    @setEvalBranchQuota(100_000);
    var state: u64 = 0xcbf29ce484222325;
    foldBytes(&state, domain);
    for (versions) |version| {
        inline for (0..4) |i| foldByte(&state, @truncate(version >> (i * 8)));
    }
    for (fingerprints) |fingerprint| foldBytes(&state, &fingerprint);

    var result: [32]u8 = undefined;
    var split_state = state;
    inline for (0..4) |lane| {
        split_state = splitmix64(split_state);
        inline for (0..8) |byte_i| {
            result[lane * 8 + byte_i] = @truncate(split_state >> (byte_i * 8));
        }
    }
    return result;
}

fn foldBytes(state: *u64, bytes: []const u8) void {
    for (bytes) |byte| foldByte(state, byte);
}

fn foldByte(state: *u64, byte: u8) void {
    state.* = (state.* ^ byte) *% 0x100000001b3;
}

fn splitmix64(x: u64) u64 {
    var z = x +% 0x9E3779B97F4A7C15;
    z = (z ^ (z >> 30)) *% 0xBF58476D1CE4E5B9;
    z = (z ^ (z >> 27)) *% 0x94D049BB133111EB;
    return z ^ (z >> 31);
}

test "every entry kind has a distinct admission hash" {
    const hashes = [_][32]u8{
        entryVersionHash(.checked_module),
        entryVersionHash(.platform_pairing),
        entryVersionHash(.canonicalized_module),
    };
    for (hashes, 0..) |a, i| {
        for (hashes[i + 1 ..]) |b| try std.testing.expect(!std.mem.eql(u8, &a, &b));
    }
}

test "each admission hash moves with every input it folds" {
    const base_hash = comptime compose("domain", &.{ 1, 2 }, &.{ @splat(3), @splat(4) });
    const variants = comptime [_][32]u8{
        compose("other-domain", &.{ 1, 2 }, &.{ @splat(3), @splat(4) }),
        compose("domain", &.{ 9, 2 }, &.{ @splat(3), @splat(4) }),
        compose("domain", &.{ 1, 9 }, &.{ @splat(3), @splat(4) }),
        compose("domain", &.{ 1, 2 }, &.{ @splat(9), @splat(4) }),
        compose("domain", &.{ 1, 2 }, &.{ @splat(3), @splat(9) }),
        compose("domain", &.{ 1, 2 }, &.{ @splat(4), @splat(3) }),
    };
    for (variants) |variant| try std.testing.expect(!std.mem.eql(u8, &base_hash, &variant));
}

test "module_env_layout golden value" {
    // Tripwire: an *accidental* change to `ModuleEnv.Serialized`'s layout silently
    // invalidates every on-disk module cache. It flips this hash and fails here. On
    // an *intentional* layout change, bump `Constants.CACHE_VERSION` and replace the
    // golden bytes below with the ones this assertion prints.
    const golden: [32]u8 = .{
        0x15, 0x25, 0x50, 0x0D, 0x08, 0x7F, 0x72, 0xED, 0x30, 0x3B, 0x7D, 0xCC, 0xDB, 0x3B, 0x45, 0x11,
        0x04, 0xA3, 0xD5, 0x4F, 0x6E, 0x4C, 0x7A, 0x49, 0xF1, 0x45, 0x15, 0x3F, 0x21, 0x12, 0xE1, 0xEE,
    };
    try std.testing.expectEqualSlices(u8, &golden, &module_env_layout);
}
