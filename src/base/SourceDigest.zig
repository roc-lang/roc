//! The digest of one compiler input file's bytes: a module's source text, or
//! a data file a module imports.
//!
//! `of` is the only place the compiler hashes such bytes. A module's source is
//! digested once, where its bytes are first in hand, and the digest then
//! travels with the module (`ModuleEnv.source_digest`). Everything that names
//! the source afterwards (the canonicalized-module cache key, the module's
//! content identity, the checked-module key, the typing key, watch-mode file
//! states) hashes these 32 bytes, never the source text again.
//!
//! The digest is BLAKE3-256. Source files are the compiler's large hash
//! inputs, where BLAKE3 outruns hardware SHA-256; the small fixed-size inputs
//! that combine a digest with names, versions, and dependency facts stay on
//! `base.Sha256`. It is cryptographic and 256 bits wide for the reason
//! design.md gives for type digests: a package author controls every source
//! byte, equal digests are treated as equal sources with no byte comparison,
//! and digests are persisted in caches, so the bound that matters is a
//! birthday collision.

const std = @import("std");

const SourceDigest = @This();

bytes: [32]u8,

/// Digest `source`. Callers hold on to the result instead of calling this
/// again for the same bytes.
pub fn of(source: []const u8) SourceDigest {
    var digest: SourceDigest = undefined;
    std.crypto.hash.Blake3.hash(source, &digest.bytes, .{});
    return digest;
}

/// Whether two digests name the same bytes: a single 256-bit compare.
pub fn eql(a: SourceDigest, b: SourceDigest) bool {
    return @as(u256, @bitCast(a.bytes)) == @as(u256, @bitCast(b.bytes));
}

test "a source digest is the BLAKE3-256 of the bytes" {
    const source = "module [answer]\n\nanswer = 42\n";
    var expected: [32]u8 = undefined;
    std.crypto.hash.Blake3.hash(source, &expected, .{});
    try std.testing.expectEqualSlices(u8, &expected, &SourceDigest.of(source).bytes);

    // The published BLAKE3 test vector for the empty input.
    const empty = [_]u8{
        0xaf, 0x13, 0x49, 0xb9, 0xf5, 0xf9, 0xa1, 0xa6, 0xa0, 0x40, 0x4d, 0xea, 0x36, 0xdc, 0xc9, 0x49,
        0x9b, 0xcb, 0x25, 0xc9, 0xad, 0xc1, 0x12, 0xb7, 0xcc, 0x9a, 0x93, 0xca, 0xe4, 0x1f, 0x32, 0x62,
    };
    try std.testing.expectEqualSlices(u8, &empty, &SourceDigest.of("").bytes);
}

test "source digests compare by value" {
    try std.testing.expect(SourceDigest.of("a = 1").eql(SourceDigest.of("a = 1")));
    try std.testing.expect(!SourceDigest.of("a = 1").eql(SourceDigest.of("a = 2")));
}
