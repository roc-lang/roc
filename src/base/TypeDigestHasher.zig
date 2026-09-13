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
//! only construction that satisfies both constraints. Every 64-bit compiler
//! target is built with the CPU's SHA-256 instructions enabled (see
//! `getReleaseTargetQuery` in build.zig), so this costs one hardware
//! compression per 64 bytes; 32-bit targets such as wasm32 use the portable
//! rounds.
//!
//! Module identities and artifact cache keys hash with
//! `std.crypto.hash.sha2.Sha256` directly; this type exists so that every
//! type-identity producer shares one hasher, one width, and one place to
//! document the requirement above.

const std = @import("std");
const builtin = @import("builtin");
const rounds = @import("sha256_rounds.zig");
const TypeDigestHasher = @This();

/// Width of canonical type and evidence digest outputs in bytes.
pub const digest_length = 32;

comptime {
    // Every 64-bit target must carry the SHA-256 instructions: there is no
    // software path for them, by decision. build.zig adds the feature to the
    // baseline CPU; a `-Dcpu` that drops it is an unsupported target.
    if (!rounds.hasHardwareSupport) {
        switch (builtin.cpu.arch) {
            .x86_64 => @compileError("roc requires the x86 SHA extension (`sha`) on x86_64 targets; CPUs without SHA-256 instructions are not supported"),
            .aarch64 => @compileError("roc requires the ARMv8 `sha2` extension on aarch64 targets; CPUs without SHA-256 instructions are not supported"),
            else => if (@sizeOf(usize) == 8) {
                @compileError("roc requires SHA-256 instructions on 64-bit targets, and has no SHA-256 implementation for this architecture");
            },
        }
    }
}

state: rounds.State = rounds.initial_state,
buf: rounds.Block = undefined,
buf_len: u8 = 0,
total_len: u64 = 0,

pub fn init() TypeDigestHasher {
    return .{};
}

fn compress(state: *rounds.State, blocks: []const rounds.Block) void {
    if (comptime rounds.hasHardwareSupport) {
        rounds.compressHardware(state, blocks);
    } else {
        rounds.compressPortable(state, blocks);
    }
}

/// Feed part of one canonical encoding.
pub fn update(self: *TypeDigestHasher, bytes: []const u8) void {
    var off: usize = 0;

    // Complete a partially filled block first.
    if (self.buf_len != 0 and @as(usize, self.buf_len) + bytes.len >= 64) {
        off += 64 - self.buf_len;
        @memcpy(self.buf[self.buf_len..][0..off], bytes[0..off]);
        compress(&self.state, @as(*const [1]rounds.Block, &self.buf));
        self.buf_len = 0;
    }

    // Full middle blocks straight from the input.
    const full_blocks = (bytes.len - off) / 64;
    if (full_blocks != 0) {
        const blocks: [*]const rounds.Block = @ptrCast(bytes[off..].ptr);
        compress(&self.state, blocks[0..full_blocks]);
        off += full_blocks * 64;
    }

    // Keep any remainder for the next call.
    const rest = bytes[off..];
    @memcpy(self.buf[self.buf_len..][0..rest.len], rest);
    self.buf_len += @intCast(rest.len);

    self.total_len += bytes.len;
}

/// Finish the digest. The hasher must not be used afterwards.
pub fn finalResult(self: *TypeDigestHasher) [digest_length]u8 {
    // Padding: a 1 bit, zeros, then the bit length as a big-endian u64.
    @memset(self.buf[self.buf_len..], 0);
    self.buf[self.buf_len] = 0x80;
    self.buf_len += 1;
    if (64 - @as(usize, self.buf_len) < 8) {
        compress(&self.state, @as(*const [1]rounds.Block, &self.buf));
        @memset(&self.buf, 0);
    }
    std.mem.writeInt(u64, self.buf[56..64], self.total_len * 8, .big);
    compress(&self.state, @as(*const [1]rounds.Block, &self.buf));

    var result: [digest_length]u8 = undefined;
    for (self.state, 0..) |word, i| {
        std.mem.writeInt(u32, result[4 * i ..][0..4], word, .big);
    }
    return result;
}

/// Hash one complete canonical encoding.
pub fn hash(bytes: []const u8) [digest_length]u8 {
    var hasher = init();
    hasher.update(bytes);
    return hasher.finalResult();
}

test "type digests are standard SHA-256" {
    // FIPS 180-4 known answers, so the digest bytes are pinned independently
    // of this implementation and of the CPU path that computed them.
    const abc = [digest_length]u8{
        0xba, 0x78, 0x16, 0xbf, 0x8f, 0x01, 0xcf, 0xea, 0x41, 0x41, 0x40, 0xde, 0x5d, 0xae, 0x22, 0x23,
        0xb0, 0x03, 0x61, 0xa3, 0x96, 0x17, 0x7a, 0x9c, 0xb4, 0x10, 0xff, 0x61, 0xf2, 0x00, 0x15, 0xad,
    };
    try std.testing.expectEqual(abc, hash("abc"));
    const empty = [digest_length]u8{
        0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14, 0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f, 0xb9, 0x24,
        0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c, 0xa4, 0x95, 0x99, 0x1b, 0x78, 0x52, 0xb8, 0x55,
    };
    try std.testing.expectEqual(empty, hash(""));
    const two_blocks = [digest_length]u8{
        0x24, 0x8d, 0x6a, 0x61, 0xd2, 0x06, 0x38, 0xb8, 0xe5, 0xc0, 0x26, 0x93, 0x0c, 0x3e, 0x60, 0x39,
        0xa3, 0x3c, 0xe4, 0x59, 0x64, 0xff, 0x21, 0x67, 0xf6, 0xec, 0xed, 0xd4, 0x19, 0xdb, 0x06, 0xc1,
    };
    try std.testing.expectEqual(two_blocks, hash("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"));
}

test "type digests agree with std.crypto for every length and chunking" {
    var input: [300]u8 = undefined;
    for (&input, 0..) |*byte, i| byte.* = @truncate(i *% 0x9d +% 0x11);
    for ([_]usize{ 0, 1, 55, 56, 57, 63, 64, 65, 119, 120, 128, 200, 255, 256, 300 }) |len| {
        var expected: [32]u8 = undefined;
        std.crypto.hash.sha2.Sha256.hash(input[0..len], &expected, .{});
        try std.testing.expectEqual(expected, hash(input[0..len]));
        var split: usize = 0;
        while (split <= len) : (split += 7) {
            var hasher = init();
            hasher.update(input[0..split]);
            hasher.update(input[split..len]);
            try std.testing.expectEqual(expected, hasher.finalResult());
        }
    }
}

test "portable rounds agree with the target's rounds" {
    var block: rounds.Block = undefined;
    for (&block, 0..) |*byte, i| byte.* = @truncate(i *% 0x3b +% 0x5);
    var portable = rounds.initial_state;
    rounds.compressPortable(&portable, @as(*const [1]rounds.Block, &block));
    var target = rounds.initial_state;
    compress(&target, @as(*const [1]rounds.Block, &block));
    try std.testing.expectEqual(portable, target);
}
