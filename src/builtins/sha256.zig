//! SHA-256 block compression and the incremental hasher built on it, shared
//! by the `Crypto` builtins and the compiler's own hashing (`base.Sha256`).
//!
//! There are two hardware implementations (the x86 SHA extension and the
//! ARMv8 `sha2` extension) and one portable implementation, and all three
//! produce identical state transitions. Which one a piece of code uses is
//! decided by the CPU features it is compiled for (`Rounds.forCpu`), never by
//! asking the CPU it runs on: builtins compiled for a target always use that
//! target's rounds. The compiler binary adds a CPUID dispatch of its own on
//! x86_64 (see `src/base/sha256.zig`); nothing here selects rounds at runtime.

const std = @import("std");
const builtin = @import("builtin");
const math = std.math;
const mem = std.mem;

const ArchClass = enum { x86_64, aarch64, other };

fn classifyArch(arch: std.Target.Cpu.Arch) ArchClass {
    return switch (arch) {
        .x86_64 => .x86_64,
        .aarch64 => .aarch64,
        .aarch64_be,
        .alpha,
        .amdgcn,
        .arc,
        .arceb,
        .arm,
        .armeb,
        .avr,
        .bpfeb,
        .bpfel,
        .csky,
        .hexagon,
        .hppa,
        .hppa64,
        .kalimba,
        .kvx,
        .lanai,
        .loongarch32,
        .loongarch64,
        .m68k,
        .microblaze,
        .microblazeel,
        .mips,
        .mipsel,
        .mips64,
        .mips64el,
        .msp430,
        .nvptx,
        .nvptx64,
        .or1k,
        .powerpc,
        .powerpcle,
        .powerpc64,
        .powerpc64le,
        .propeller,
        .riscv32,
        .riscv32be,
        .riscv64,
        .riscv64be,
        .s390x,
        .sh,
        .sheb,
        .sparc,
        .sparc64,
        .spirv32,
        .spirv64,
        .thumb,
        .thumbeb,
        .ve,
        .wasm32,
        .wasm64,
        .x86_16,
        .x86,
        .xcore,
        .xtensa,
        .xtensaeb,
        => .other,
    };
}

/// One SHA-256 message block.
pub const Block = [64]u8;
/// The eight-word SHA-256 chaining state.
pub const State = [8]u32;

/// The initial chaining state defined by FIPS 180-4.
pub const initial_state = State{
    0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A,
    0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19,
};

/// The architecture category of the CPU this code is compiled for.
pub const arch_class = classifyArch(builtin.cpu.arch);

/// The block compression a CPU feature set supports.
pub const Rounds = enum {
    /// Portable integer arithmetic, for every CPU.
    portable,
    /// The x86 SHA extension, plus the SSSE3 `palignr` the schedule uses.
    /// Every CPU with the SHA extension has SSSE3.
    x86_sha,
    /// The ARMv8 `sha2` crypto extension.
    aarch64_sha2,

    /// The fastest rounds code compiled for `cpu` may use: the hardware rounds
    /// exactly when its feature set carries the SHA-256 instructions.
    pub fn forCpu(cpu: std.Target.Cpu) Rounds {
        return switch (classifyArch(cpu.arch)) {
            .x86_64 => if (cpu.hasAll(.x86, &.{ .sha, .ssse3 })) .x86_sha else .portable,
            .aarch64 => if (cpu.has(.aarch64, .sha2)) .aarch64_sha2 else .portable,
            .other => .portable,
        };
    }
};

/// Whether the CPU features this code is compiled for carry the SHA-256
/// instructions. The C backend cannot emit the inline assembly the hardware
/// rounds are written in, so it always compiles the portable rounds.
pub const hasHardwareSupport = builtin.zig_backend != .stage2_c and Rounds.forCpu(builtin.cpu) != .portable;

/// The rounds `compressForTarget` runs: fixed by the CPU features this code
/// is compiled for.
pub const target_rounds: Rounds = if (hasHardwareSupport) Rounds.forCpu(builtin.cpu) else .portable;

/// Compress every block into `state` with `target_rounds`.
pub fn compressForTarget(state: *State, blocks: []const Block) void {
    switch (target_rounds) {
        .portable => compressPortable(state, blocks),
        .x86_sha, .aarch64_sha2 => compressHardware(state, blocks),
    }
}

/// The C ABI of the compression function the LLVM builtins bitcode links in
/// for its target (see `Sha256` in crypto.zig): `count` blocks starting at
/// `blocks`, compressed into `state`.
pub const CompressAbi = fn (state: *State, blocks: [*]const Block, count: usize) callconv(.c) void;

/// The symbol the LLVM builtins bitcode references for its compression
/// function, defined by `sha256_rounds_lib.zig` compiled for the target's
/// `Rounds`.
pub const compress_symbol = "roc_builtins_sha256_compress";

/// An incremental SHA-256 hasher that compresses its blocks with `compress`.
/// The interface mirrors `std.crypto.hash.sha2.Sha256` and the digests are the
/// standard SHA-256 bytes whichever rounds `compress` runs.
///
/// The fields are the complete SHA-256 mid-state (chaining words, the bytes of
/// the partial block, and the message length so far), so a hasher can be
/// saved and resumed by recording them.
pub fn Hasher(comptime compress: fn (*State, []const Block) void) type {
    return struct {
        const Self = @This();

        /// Width of a digest in bytes.
        pub const digest_length = 32;
        /// Width of one compressed block in bytes.
        pub const block_length = 64;
        /// Accepted for `std.crypto.hash.sha2.Sha256` compatibility; SHA-256 has no options.
        pub const Options = struct {};

        state: State = initial_state,
        buf: Block = undefined,
        buf_len: u8 = 0,
        total_len: u64 = 0,

        /// Start an empty digest.
        pub fn init(_: Options) Self {
            return .{};
        }

        /// Feed the next bytes of the message.
        pub fn update(self: *Self, bytes: []const u8) void {
            var off: usize = 0;

            // Complete a partially filled block first.
            if (self.buf_len != 0 and @as(usize, self.buf_len) + bytes.len >= block_length) {
                off += block_length - self.buf_len;
                @memcpy(self.buf[self.buf_len..][0..off], bytes[0..off]);
                compress(&self.state, @as(*const [1]Block, &self.buf));
                self.buf_len = 0;
            }

            // Full middle blocks straight from the input.
            const full_blocks = (bytes.len - off) / block_length;
            if (full_blocks != 0) {
                const blocks: [*]const Block = @ptrCast(bytes[off..].ptr);
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
        pub fn finalResult(self: *Self) [digest_length]u8 {
            // Padding: a 1 bit, zeros, then the bit length as a big-endian u64.
            @memset(self.buf[self.buf_len..], 0);
            self.buf[self.buf_len] = 0x80;
            self.buf_len += 1;
            if (block_length - @as(usize, self.buf_len) < 8) {
                compress(&self.state, @as(*const [1]Block, &self.buf));
                @memset(&self.buf, 0);
            }
            std.mem.writeInt(u64, self.buf[56..64], self.total_len * 8, .big);
            compress(&self.state, @as(*const [1]Block, &self.buf));

            var result: [digest_length]u8 = undefined;
            for (self.state, 0..) |word, i| {
                std.mem.writeInt(u32, result[4 * i ..][0..4], word, .big);
            }
            return result;
        }

        /// Finish the digest into `out`. The hasher must not be used afterwards.
        pub fn final(self: *Self, out: *[digest_length]u8) void {
            out.* = self.finalResult();
        }

        /// Digest one complete message into `out`.
        pub fn hash(bytes: []const u8, out: *[digest_length]u8, options: Options) void {
            var hasher = init(options);
            hasher.update(bytes);
            hasher.final(out);
        }
    };
}

const K = [64]u32{
    0x428A2F98, 0x71374491, 0xB5C0FBCF, 0xE9B5DBA5, 0x3956C25B, 0x59F111F1, 0x923F82A4, 0xAB1C5ED5,
    0xD807AA98, 0x12835B01, 0x243185BE, 0x550C7DC3, 0x72BE5D74, 0x80DEB1FE, 0x9BDC06A7, 0xC19BF174,
    0xE49B69C1, 0xEFBE4786, 0x0FC19DC6, 0x240CA1CC, 0x2DE92C6F, 0x4A7484AA, 0x5CB0A9DC, 0x76F988DA,
    0x983E5152, 0xA831C66D, 0xB00327C8, 0xBF597FC7, 0xC6E00BF3, 0xD5A79147, 0x06CA6351, 0x14292967,
    0x27B70A85, 0x2E1B2138, 0x4D2C6DFC, 0x53380D13, 0x650A7354, 0x766A0ABB, 0x81C2C92E, 0x92722C85,
    0xA2BFE8A1, 0xA81A664B, 0xC24B8B70, 0xC76C51A3, 0xD192E819, 0xD6990624, 0xF40E3585, 0x106AA070,
    0x19A4C116, 0x1E376C08, 0x2748774C, 0x34B0BCB5, 0x391C0CB3, 0x4ED8AA4A, 0x5B9CCA4F, 0x682E6FF3,
    0x748F82EE, 0x78A5636F, 0x84C87814, 0x8CC70208, 0x90BEFFFA, 0xA4506CEB, 0xBEF9A3F7, 0xC67178F2,
};

/// Compress every block into `state` using the SHA-256 instructions. Only
/// callable when the CPU running it has them: when `hasHardwareSupport` is
/// true, or when the compiler's own x86 dispatch (`src/base/sha256.zig`) has
/// confirmed them with CPUID.
pub fn compressHardware(state: *State, blocks: []const Block) void {
    switch (arch_class) {
        .aarch64 => for (blocks) |*block| roundAarch64Sha2(state, block),
        .x86_64 => compressX86Sha(state, blocks),
        .other => @compileError("SHA-256 hardware compression is only implemented for aarch64 and x86_64"),
    }
}

/// Compress every block into `state` with portable integer arithmetic.
pub fn compressPortable(state: *State, blocks: []const Block) void {
    for (blocks) |*block| roundPortable(state, block);
}

fn loadSchedule(block: *const Block, s: *[64]u32) void {
    for (@as(*align(1) const [16]u32, @ptrCast(block)), 0..) |*elem, i| {
        s[i] = mem.readInt(u32, mem.asBytes(elem), .big);
    }
}

fn roundAarch64Sha2(state: *State, block: *const Block) void {
    const V4u32 = @Vector(4, u32);
    var s: [64]u32 align(16) = undefined;
    loadSchedule(block, &s);
    var x: V4u32 = state[0..4].*;
    var y: V4u32 = state[4..8].*;
    const s_v = @as(*[16]V4u32, @ptrCast(&s));

    comptime var k: u8 = 0;
    inline while (k < 16) : (k += 1) {
        if (k > 3) {
            s_v[k] = asm (
                \\sha256su0.4s %[w0_3], %[w4_7]
                \\sha256su1.4s %[w0_3], %[w8_11], %[w12_15]
                : [w0_3] "=&w" (-> V4u32),
                : [_] "0" (s_v[k - 4]),
                  [w4_7] "w" (s_v[k - 3]),
                  [w8_11] "w" (s_v[k - 2]),
                  [w12_15] "w" (s_v[k - 1]),
            );
        }

        const w: V4u32 = s_v[k] +% @as(V4u32, K[4 * k ..][0..4].*);
        asm volatile (
            \\mov.4s v0, %[x]
            \\sha256h.4s %[x], %[y], %[w]
            \\sha256h2.4s %[y], v0, %[w]
            : [x] "=w" (x),
              [y] "=w" (y),
            : [_] "0" (x),
              [_] "1" (y),
              [w] "w" (w),
            : .{ .v0 = true });
    }

    state[0..4].* = x +% @as(V4u32, state[0..4].*);
    state[4..8].* = y +% @as(V4u32, state[4..8].*);
}

/// The x86 SHA rounds over consecutive blocks. The state stays in the two
/// vectors `sha256rnds2` works on from the first block to the last, and each
/// block's message words load with one byte swap per four words, so the only
/// per-block work outside the SHA instructions is the schedule and the
/// feed-forward addition.
fn compressX86Sha(state: *State, blocks: []const Block) void {
    const V4u32 = @Vector(4, u32);
    var x: V4u32 = [_]u32{ state[5], state[4], state[1], state[0] };
    var y: V4u32 = [_]u32{ state[7], state[6], state[3], state[2] };

    for (blocks) |*block| {
        const x_in = x;
        const y_in = y;
        var s_v: [16]V4u32 = undefined;
        inline for (0..4) |i| {
            const words: *align(1) const V4u32 = @ptrCast(block[16 * i ..][0..16]);
            s_v[i] = @byteSwap(words.*);
        }

        comptime var k: u8 = 0;
        inline while (k < 16) : (k += 1) {
            if (k < 12) {
                var tmp = s_v[k];
                s_v[k + 4] = asm (
                    \\ sha256msg1 %[w4_7], %[tmp]
                    \\ movdqa %[w12_15], %[result]
                    \\ palignr $0x4, %[w8_11], %[result]
                    \\ paddd %[tmp], %[result]
                    \\ sha256msg2 %[w12_15], %[result]
                    : [tmp] "=&x" (tmp),
                      [result] "=&x" (-> V4u32),
                    : [_] "0" (tmp),
                      [w4_7] "x" (s_v[k + 1]),
                      [w8_11] "x" (s_v[k + 2]),
                      [w12_15] "x" (s_v[k + 3]),
                );
            }

            const w: V4u32 = s_v[k] +% @as(V4u32, K[4 * k ..][0..4].*);
            y = asm ("sha256rnds2 %[x], %[y]"
                : [y] "=x" (-> V4u32),
                : [_] "0" (y),
                  [x] "x" (x),
                  [_] "{xmm0}" (w),
            );

            x = asm ("sha256rnds2 %[y], %[x]"
                : [x] "=x" (-> V4u32),
                : [_] "0" (x),
                  [y] "x" (y),
                  [_] "{xmm0}" (@as(V4u32, @bitCast(@as(u128, @bitCast(w)) >> 64))),
            );
        }

        x +%= x_in;
        y +%= y_in;
    }

    state[0] = x[3];
    state[1] = x[2];
    state[4] = x[1];
    state[5] = x[0];
    state[2] = y[3];
    state[3] = y[2];
    state[6] = y[1];
    state[7] = y[0];
}

const RoundParam = struct { a: usize, b: usize, c: usize, d: usize, e: usize, f: usize, g: usize, h: usize, i: usize };

fn roundParam(a: usize, b: usize, c: usize, d: usize, e: usize, f: usize, g: usize, h: usize, i: usize) RoundParam {
    return .{ .a = a, .b = b, .c = c, .d = d, .e = e, .f = f, .g = g, .h = h, .i = i };
}

const round_params = blk: {
    var params: [64]RoundParam = undefined;
    for (0..64) |i| {
        const shift = (8 - (i % 8)) % 8;
        params[i] = roundParam(
            (0 + shift) % 8,
            (1 + shift) % 8,
            (2 + shift) % 8,
            (3 + shift) % 8,
            (4 + shift) % 8,
            (5 + shift) % 8,
            (6 + shift) % 8,
            (7 + shift) % 8,
            i,
        );
    }
    break :blk params;
};

fn roundPortable(state: *State, block: *const Block) void {
    var s: [64]u32 align(16) = undefined;
    loadSchedule(block, &s);

    var i: usize = 16;
    while (i < 64) : (i += 1) {
        s[i] = s[i - 16] +% s[i - 7] +% (math.rotr(u32, s[i - 15], @as(u32, 7)) ^ math.rotr(u32, s[i - 15], @as(u32, 18)) ^ (s[i - 15] >> 3)) +% (math.rotr(u32, s[i - 2], @as(u32, 17)) ^ math.rotr(u32, s[i - 2], @as(u32, 19)) ^ (s[i - 2] >> 10));
    }

    var v: [8]u32 = state.*;
    inline for (round_params) |r| {
        v[r.h] = v[r.h] +% (math.rotr(u32, v[r.e], @as(u32, 6)) ^ math.rotr(u32, v[r.e], @as(u32, 11)) ^ math.rotr(u32, v[r.e], @as(u32, 25))) +% (v[r.g] ^ (v[r.e] & (v[r.f] ^ v[r.g]))) +% K[r.i] +% s[r.i];
        v[r.d] = v[r.d] +% v[r.h];
        v[r.h] = v[r.h] +% (math.rotr(u32, v[r.a], @as(u32, 2)) ^ math.rotr(u32, v[r.a], @as(u32, 13)) ^ math.rotr(u32, v[r.a], @as(u32, 22))) +% ((v[r.a] & (v[r.b] | v[r.c])) | (v[r.b] & v[r.c]));
    }

    for (state, v) |*dv, vv| dv.* +%= vv;
}

test "portable round parameters match the SHA-256 rotation schedule" {
    // Round i rotates the working variables by i positions: round 0 is
    // (a..h) = (0..7), round 1 is (7,0,1,...,6), and so on.
    try std.testing.expectEqual(roundParam(0, 1, 2, 3, 4, 5, 6, 7, 0), round_params[0]);
    try std.testing.expectEqual(roundParam(7, 0, 1, 2, 3, 4, 5, 6, 1), round_params[1]);
    try std.testing.expectEqual(roundParam(1, 2, 3, 4, 5, 6, 7, 0, 7), round_params[7]);
    try std.testing.expectEqual(roundParam(0, 1, 2, 3, 4, 5, 6, 7, 8), round_params[8]);
    try std.testing.expectEqual(roundParam(1, 2, 3, 4, 5, 6, 7, 0, 63), round_params[63]);
}

const TargetHasher = Hasher(compressForTarget);
const PortableHasher = Hasher(compressPortable);
const Digest = [TargetHasher.digest_length]u8;

fn hashedWith(comptime H: type, bytes: []const u8) Digest {
    var out: Digest = undefined;
    H.hash(bytes, &out, .{});
    return out;
}

test "digests are standard SHA-256" {
    // FIPS 180-4 known answers, so the digest bytes are pinned independently
    // of this implementation and of the rounds that computed them.
    const abc = Digest{
        0xba, 0x78, 0x16, 0xbf, 0x8f, 0x01, 0xcf, 0xea, 0x41, 0x41, 0x40, 0xde, 0x5d, 0xae, 0x22, 0x23,
        0xb0, 0x03, 0x61, 0xa3, 0x96, 0x17, 0x7a, 0x9c, 0xb4, 0x10, 0xff, 0x61, 0xf2, 0x00, 0x15, 0xad,
    };
    const empty = Digest{
        0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14, 0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f, 0xb9, 0x24,
        0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c, 0xa4, 0x95, 0x99, 0x1b, 0x78, 0x52, 0xb8, 0x55,
    };
    const two_blocks = Digest{
        0x24, 0x8d, 0x6a, 0x61, 0xd2, 0x06, 0x38, 0xb8, 0xe5, 0xc0, 0x26, 0x93, 0x0c, 0x3e, 0x60, 0x39,
        0xa3, 0x3c, 0xe4, 0x59, 0x64, 0xff, 0x21, 0x67, 0xf6, 0xec, 0xed, 0xd4, 0x19, 0xdb, 0x06, 0xc1,
    };
    inline for (.{ TargetHasher, PortableHasher }) |H| {
        try std.testing.expectEqual(abc, hashedWith(H, "abc"));
        try std.testing.expectEqual(empty, hashedWith(H, ""));
        try std.testing.expectEqual(two_blocks, hashedWith(H, "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"));
    }
}

test "digests agree with std.crypto for every length and chunking" {
    var input: [300]u8 = undefined;
    for (&input, 0..) |*byte, i| byte.* = @truncate(i *% 0x9d +% 0x11);
    inline for (.{ TargetHasher, PortableHasher }) |H| {
        for ([_]usize{ 0, 1, 55, 56, 57, 63, 64, 65, 119, 120, 128, 200, 255, 256, 300 }) |len| {
            var expected: Digest = undefined;
            std.crypto.hash.sha2.Sha256.hash(input[0..len], &expected, .{});
            try std.testing.expectEqual(expected, hashedWith(H, input[0..len]));
            var split: usize = 0;
            while (split <= len) : (split += 7) {
                var hasher = H.init(.{});
                hasher.update(input[0..split]);
                hasher.update(input[split..len]);
                try std.testing.expectEqual(expected, hasher.finalResult());
            }
        }
    }
}

test "digests agree with std.crypto across many consecutive blocks" {
    var input: [64 * 1024 + 37]u8 = undefined;
    for (&input, 0..) |*byte, i| byte.* = @truncate(i *% 0x2f +% (i >> 9));
    var expected: Digest = undefined;
    std.crypto.hash.sha2.Sha256.hash(&input, &expected, .{});
    try std.testing.expectEqual(expected, hashedWith(TargetHasher, &input));
    for ([_]usize{ 1, 64, 1000, 4096 }) |split| {
        var hasher = TargetHasher.init(.{});
        hasher.update(input[0..split]);
        hasher.update(input[split..]);
        try std.testing.expectEqual(expected, hasher.finalResult());
    }
}

test "the target's rounds agree with the portable rounds" {
    var block: Block = undefined;
    for (&block, 0..) |*byte, i| byte.* = @truncate(i *% 0x3b +% 0x5);
    var portable = initial_state;
    compressPortable(&portable, @as(*const [1]Block, &block));
    var target = initial_state;
    compressForTarget(&target, @as(*const [1]Block, &block));
    try std.testing.expectEqual(portable, target);
}

test "rounds follow the SHA-256 instructions in a CPU's feature set" {
    const x86_64 = std.Target.Cpu.Arch.x86_64;
    var x86_baseline = std.Target.x86.cpu.x86_64.toCpu(x86_64);
    try std.testing.expectEqual(Rounds.portable, Rounds.forCpu(x86_baseline));
    try std.testing.expectEqual(Rounds.portable, Rounds.forCpu(std.Target.x86.cpu.x86_64_v3.toCpu(x86_64)));
    x86_baseline.features.addFeature(@intFromEnum(std.Target.x86.Feature.sha));
    x86_baseline.features.addFeature(@intFromEnum(std.Target.x86.Feature.ssse3));
    try std.testing.expectEqual(Rounds.x86_sha, Rounds.forCpu(x86_baseline));

    const aarch64 = std.Target.Cpu.Arch.aarch64;
    var aarch64_generic = std.Target.aarch64.cpu.generic.toCpu(aarch64);
    try std.testing.expectEqual(Rounds.portable, Rounds.forCpu(aarch64_generic));
    aarch64_generic.features.addFeature(@intFromEnum(std.Target.aarch64.Feature.sha2));
    try std.testing.expectEqual(Rounds.aarch64_sha2, Rounds.forCpu(aarch64_generic));
    try std.testing.expectEqual(Rounds.aarch64_sha2, Rounds.forCpu(std.Target.aarch64.cpu.apple_m1.toCpu(aarch64)));

    try std.testing.expectEqual(Rounds.portable, Rounds.forCpu(std.Target.wasm.cpu.generic.toCpu(.wasm32)));
}
