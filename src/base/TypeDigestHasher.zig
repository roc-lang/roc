//! SHA-256 digests for compiler type and evidence identity.
//!
//! Why these digests are cryptographic: a Roc package is pure Roc code that a
//! user compiles without auditing, and a package author controls every byte
//! that reaches these digests: record field names, tag names, type names, the
//! structure of every type the package declares. Several consumers treat two
//! equal digests as one type with no structural comparison behind them --
//! `CheckedTypeStore.root_index` when an imported type is projected into the
//! app's checked type store, the layout store's recursive-graph index, erased
//! callable and generated nominal identities inside `typeEql`, format-codec
//! call addresses -- because the digest is exactly what gives a type an
//! identity that survives module and cache boundaries. If a package could
//! produce two structurally different types with one digest, the checker would
//! accept its code (unification is structural) and post-check lowering would
//! then compile expressions of both types with a single payload, which is a
//! miscompile with memory-unsafe consequences inside the app's process. A
//! non-cryptographic hash makes crafting such a pair cheap; the package author
//! supplies both colliding types, so even a cryptographic hash has to be wide
//! enough that a birthday collision is out of reach: 256 bits (2^128 work),
//! not 128 (2^64, which is within reach of a well-funded attacker).
//!
//! The digests are also persisted in the checked-module and specialization
//! caches and must compare equal across machines, so they cannot be keyed by a
//! per-machine secret; a public, deterministic, cryptographic function is the
//! only construction that satisfies both constraints. The digests are
//! computed by `Sha256`, which runs on the CPU's SHA-256 instructions nearly
//! everywhere.
//!
//! Module identities and artifact cache keys hash with `Sha256` directly; this
//! type exists so that every type-identity producer shares one hasher, one
//! width, and one place to document the requirement above.

const std = @import("std");
const Sha256 = @import("sha256.zig").Sha256;
const TypeDigestHasher = @This();

/// Width of canonical type and evidence digest outputs in bytes.
pub const digest_length = Sha256.digest_length;

inner: Sha256,

/// Start an empty digest.
pub fn init() TypeDigestHasher {
    return .{ .inner = Sha256.init(.{}) };
}

/// Feed part of one canonical encoding.
pub fn update(self: *TypeDigestHasher, bytes: []const u8) void {
    self.inner.update(bytes);
}

/// Feed a constant canonical tag with its little-endian u32 byte length.
/// Combining the prefix and text avoids two updates without changing any bytes.
pub fn updateTag(self: *TypeDigestHasher, comptime tag: []const u8) void {
    const encoded = comptime blk: {
        var bytes: [4 + tag.len]u8 = undefined;
        std.mem.writeInt(u32, bytes[0..4], tag.len, .little);
        @memcpy(bytes[4..], tag);
        break :blk bytes;
    };
    self.update(&encoded);
}

/// Finish the digest. The hasher must not be used afterwards.
pub fn finalResult(self: *TypeDigestHasher) [digest_length]u8 {
    return self.inner.finalResult();
}

/// Hash one complete canonical encoding.
pub fn hash(bytes: []const u8) [digest_length]u8 {
    var out: [digest_length]u8 = undefined;
    Sha256.hash(bytes, &out, .{});
    return out;
}

test "constant tags preserve encoding across hash block boundaries" {
    const prefix = @as([128]u8, @splat(43));
    for (0..prefix.len + 1) |len| {
        var split = init();
        var batched = init();
        split.update(prefix[0..len]);
        batched.update(prefix[0..len]);
        inline for (.{ "", "record", "longer tag to cross multiple hash block boundaries abcdefghijklmnopqrstuvwxyz0123456789" }) |tag| {
            var length: [4]u8 = undefined;
            std.mem.writeInt(u32, &length, tag.len, .little);
            split.update(&length);
            split.update(tag);
            batched.updateTag(tag);
        }
        try std.testing.expectEqual(split.finalResult(), batched.finalResult());
    }
}
