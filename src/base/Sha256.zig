//! SHA-256 on the CPU's SHA-256 instructions wherever the compiler target has
//! them. aarch64 compiler targets are built with the instructions enabled (see
//! `getReleaseTargetQuery` in build.zig) and released x86_64 compilers for
//! anything but macOS detect them once per process (see
//! `dispatches_at_runtime` in sha256_rounds.zig), so this costs one hardware
//! compression per 64 bytes nearly everywhere; 32-bit targets such as wasm32,
//! x86_64 macOS (see `uses_software_rounds` there) and x86-64 CPUs without the
//! SHA extension use the portable rounds, which produce the same digest bytes
//! more slowly.
//!
//! The interface mirrors `std.crypto.hash.sha2.Sha256` and the digests are the
//! standard SHA-256 bytes, so every identity and cache key the compiler hashes
//! is the same whichever implementation computed it.

const std = @import("std");
const rounds = @import("sha256_rounds.zig");
const Sha256 = @This();

/// Width of a digest in bytes.
pub const digest_length = 32;
/// Width of one compressed block in bytes.
pub const block_length = 64;
/// Accepted for `std.crypto.hash.sha2.Sha256` compatibility; SHA-256 has no options.
pub const Options = struct {};

state: rounds.State = rounds.initial_state,
buf: rounds.Block = undefined,
buf_len: u8 = 0,
total_len: u64 = 0,

/// Start an empty digest.
pub fn init(_: Options) Sha256 {
    // Digest metadata is also used by frozen-image consumers that never hash.
    // Enforce the hashing CPU contract when constructing a hasher.
    comptime {
        // Every 64-bit target other than x86_64 must carry the SHA-256
        // instructions: there is no software path for them, by decision. build.zig
        // adds the feature to the baseline CPU; a `-Dcpu` that drops it is an
        // unsupported target. x86_64 always has a path: the hardware rounds,
        // runtime dispatch, or the portable rounds (see `dispatches_at_runtime`
        // and `uses_software_rounds` in sha256_rounds.zig).
        switch (rounds.arch_class) {
            .x86_64 => {},
            .aarch64 => if (!rounds.hasHardwareSupport) {
                @compileError("roc requires the ARMv8 `sha2` extension on aarch64 targets; CPUs without SHA-256 instructions are not supported");
            },
            .other => if (@sizeOf(usize) == 8) {
                @compileError("roc requires SHA-256 instructions on 64-bit targets, and has no SHA-256 implementation for this architecture");
            },
        }
    }
    return .{};
}

fn compress(state: *rounds.State, blocks: []const rounds.Block) void {
    if (comptime rounds.hasHardwareSupport) {
        rounds.compressHardware(state, blocks);
    } else if (comptime rounds.dispatches_at_runtime) {
        rounds.compressDispatched(state, blocks);
    } else {
        rounds.compressPortable(state, blocks);
    }
}

/// Feed the next bytes of the message.
pub fn update(self: *Sha256, bytes: []const u8) void {
    var off: usize = 0;

    // Complete a partially filled block first.
    if (self.buf_len != 0 and @as(usize, self.buf_len) + bytes.len >= block_length) {
        off += block_length - self.buf_len;
        @memcpy(self.buf[self.buf_len..][0..off], bytes[0..off]);
        compress(&self.state, @as(*const [1]rounds.Block, &self.buf));
        self.buf_len = 0;
    }

    // Full middle blocks straight from the input.
    const full_blocks = (bytes.len - off) / block_length;
    if (full_blocks != 0) {
        const blocks: [*]const rounds.Block = @ptrCast(bytes[off..].ptr);
        compress(&self.state, blocks[0..full_blocks]);
        off += full_blocks * block_length;
    }

    // Keep any remainder for the next call.
    const rest = bytes[off..];
    @memcpy(self.buf[self.buf_len..][0..rest.len], rest);
    self.buf_len += @intCast(rest.len);

    self.total_len += bytes.len;
}

/// Finish the digest. The hasher must not be used afterwards.
pub fn finalResult(self: *Sha256) [digest_length]u8 {
    // Padding: a 1 bit, zeros, then the bit length as a big-endian u64.
    @memset(self.buf[self.buf_len..], 0);
    self.buf[self.buf_len] = 0x80;
    self.buf_len += 1;
    if (block_length - @as(usize, self.buf_len) < 8) {
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

/// Finish the digest into `out`. The hasher must not be used afterwards.
pub fn final(self: *Sha256, out: *[digest_length]u8) void {
    out.* = self.finalResult();
}

/// Digest one complete message into `out`.
pub fn hash(bytes: []const u8, out: *[digest_length]u8, options: Options) void {
    var hasher = init(options);
    hasher.update(bytes);
    hasher.final(out);
}

fn hashed(bytes: []const u8) [digest_length]u8 {
    var out: [digest_length]u8 = undefined;
    hash(bytes, &out, .{});
    return out;
}

test "digests are standard SHA-256" {
    // FIPS 180-4 known answers, so the digest bytes are pinned independently
    // of this implementation and of the CPU path that computed them.
    const abc = [digest_length]u8{
        0xba, 0x78, 0x16, 0xbf, 0x8f, 0x01, 0xcf, 0xea, 0x41, 0x41, 0x40, 0xde, 0x5d, 0xae, 0x22, 0x23,
        0xb0, 0x03, 0x61, 0xa3, 0x96, 0x17, 0x7a, 0x9c, 0xb4, 0x10, 0xff, 0x61, 0xf2, 0x00, 0x15, 0xad,
    };
    try std.testing.expectEqual(abc, hashed("abc"));
    const empty = [digest_length]u8{
        0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14, 0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f, 0xb9, 0x24,
        0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c, 0xa4, 0x95, 0x99, 0x1b, 0x78, 0x52, 0xb8, 0x55,
    };
    try std.testing.expectEqual(empty, hashed(""));
    const two_blocks = [digest_length]u8{
        0x24, 0x8d, 0x6a, 0x61, 0xd2, 0x06, 0x38, 0xb8, 0xe5, 0xc0, 0x26, 0x93, 0x0c, 0x3e, 0x60, 0x39,
        0xa3, 0x3c, 0xe4, 0x59, 0x64, 0xff, 0x21, 0x67, 0xf6, 0xec, 0xed, 0xd4, 0x19, 0xdb, 0x06, 0xc1,
    };
    try std.testing.expectEqual(two_blocks, hashed("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"));
}

test "digests agree with std.crypto for every length and chunking" {
    var input: [300]u8 = undefined;
    for (&input, 0..) |*byte, i| byte.* = @truncate(i *% 0x9d +% 0x11);
    for ([_]usize{ 0, 1, 55, 56, 57, 63, 64, 65, 119, 120, 128, 200, 255, 256, 300 }) |len| {
        var expected: [32]u8 = undefined;
        std.crypto.hash.sha2.Sha256.hash(input[0..len], &expected, .{});
        try std.testing.expectEqual(expected, hashed(input[0..len]));
        var split: usize = 0;
        while (split <= len) : (split += 7) {
            var hasher = init(.{});
            hasher.update(input[0..split]);
            hasher.update(input[split..len]);
            var out: [digest_length]u8 = undefined;
            hasher.final(&out);
            try std.testing.expectEqual(expected, out);
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

test "runtime dispatch resolves to rounds that agree with the portable rounds" {
    // On a build that dispatches, the first call through `compress` runs
    // CPUID and every later call goes through the resolved implementation;
    // both must produce the portable state.
    var block: rounds.Block = undefined;
    for (&block, 0..) |*byte, i| byte.* = @truncate(i *% 0x7d +% 0x31);
    var portable = rounds.initial_state;
    rounds.compressPortable(&portable, @as(*const [1]rounds.Block, &block));
    for (0..3) |_| {
        var dispatched = rounds.initial_state;
        compress(&dispatched, @as(*const [1]rounds.Block, &block));
        try std.testing.expectEqual(portable, dispatched);
    }
    // The hardware rounds must agree even in a baseline build, where only the
    // dispatch reaches them. Only a build that can assemble them may name
    // them: the self-hosted x86_64 backend rejects them without the feature.
    if (comptime rounds.hasHardwareSupport or rounds.dispatches_at_runtime) {
        if (rounds.hasHardwareSupport or rounds.x86HasShaExtension()) {
            var hardware = rounds.initial_state;
            rounds.compressHardware(&hardware, @as(*const [1]rounds.Block, &block));
            try std.testing.expectEqual(portable, hardware);
        }
    }
}

test "digests agree with std.crypto across many consecutive blocks" {
    var input: [64 * 1024 + 37]u8 = undefined;
    for (&input, 0..) |*byte, i| byte.* = @truncate(i *% 0x2f +% (i >> 9));
    var expected: [digest_length]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(&input, &expected, .{});
    try std.testing.expectEqual(expected, hashed(&input));
    for ([_]usize{ 1, 64, 1000, 4096 }) |split| {
        var hasher = init(.{});
        hasher.update(input[0..split]);
        hasher.update(input[split..]);
        try std.testing.expectEqual(expected, hasher.finalResult());
    }
}
