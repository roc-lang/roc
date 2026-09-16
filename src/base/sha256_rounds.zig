//! SHA-256 block compression for `TypeDigestHasher`.
//!
//! Two hardware implementations (x86 SHA extensions and the ARMv8 `sha2`
//! extension) and one portable implementation, selected by `compress`. aarch64
//! compiler targets require the hardware instructions -- build.zig adds them to
//! the baseline CPU, and `TypeDigestHasher` refuses to compile for such a target
//! without them. x86_64 targets other than macOS carry both implementations and
//! pick one with CPUID the first time a process compresses a block (see
//! `dispatches_at_runtime`), because Intel's 2015-2020 Skylake through Comet
//! Lake cores have no SHA extension. The portable rounds alone serve 32-bit
//! targets such as wasm32, x86_64 macOS (see `uses_software_rounds`), and
//! self-hosted-backend x86_64 builds for a CPU without the extension. All
//! three produce identical state transitions; the tests in
//! `TypeDigestHasher.zig` compare them against `std.crypto.hash.sha2.Sha256`.

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

/// The target architecture category used to select SHA-256 rounds.
pub const arch_class = classifyArch(builtin.cpu.arch);

/// Whether the compilation target carries the SHA-256 instructions. On x86_64
/// that is the SHA extension plus SSSE3 (for `palignr`), which every CPU with
/// the SHA extension has; on aarch64 it is the `sha2` crypto extension.
pub const hasHardwareSupport = switch (arch_class) {
    .aarch64 => builtin.zig_backend != .stage2_c and builtin.cpu.has(.aarch64, .sha2),
    .x86_64 => builtin.zig_backend != .stage2_c and builtin.cpu.hasAll(.x86, &.{ .sha, .ssse3 }),
    .other => false,
};

/// Whether this target computes digests with the portable rounds only, never
/// asking the CPU for its SHA-256 instructions. x86_64 macOS is the only
/// 64-bit target that does: Apple's Intel Macs are Skylake through Comet Lake,
/// whose cores have no SHA extension (only the 2020 Ice Lake MacBook Air
/// does), so a macos_x86_64 build with the extension in its baseline dies of
/// SIGILL on nearly every Intel Mac -- including the Coffee Lake i7-8700B that
/// GitHub's macos-15-intel runner builds the nightly on. Digest bytes are
/// identical either way; only the speed differs. An Intel Mac that does have
/// the extension still uses the hardware rounds when the build names its CPU
/// (`-Dcpu=native`), because that sets `hasHardwareSupport`.
pub const uses_software_rounds = arch_class == .x86_64 and builtin.os.tag == .macos;

/// Whether this build chooses between the hardware and portable rounds at
/// runtime. That is every x86_64 target whose compilation CPU lacks the SHA
/// extension, other than macOS: the build keeps x86_64 at the architecture
/// baseline so that one released binary runs on every x86-64 CPU, and the
/// SHA extension is missing from Intel's Skylake through Comet Lake cores,
/// which are still common on Linux and Windows machines. A build that names a
/// CPU with the extension (`-Dcpu=native` on such a CPU, or any `-Dcpu` at or
/// above Ice Lake / Zen) takes `hasHardwareSupport` instead and pays no
/// dispatch.
///
/// Only LLVM assembles `roundX86Sha` for a CPU without the extension: its
/// assembler accepts any x86 instruction, whereas Zig's self-hosted x86_64
/// backend (the Debug backend) encodes only instructions in the target CPU's
/// feature set and rejects the SHA and SSSE3 ones otherwise. So a self-hosted
/// build without the feature has no hardware rounds to dispatch to and uses
/// the portable rounds; build.zig keeps Debug builds on a machine with the
/// extension at `hasHardwareSupport` (see `withSha256Floor` there).
pub const dispatches_at_runtime = arch_class == .x86_64 and !hasHardwareSupport and !uses_software_rounds and builtin.zig_backend == .stage2_llvm;

const CompressFn = *const fn (*State, []const Block) void;

/// `compress` for a build that dispatches at runtime: the function the next
/// call goes through. It starts as `compressDetecting`, which asks CPUID once
/// and then replaces it with the answer, so every later call is one load and
/// one indirect call. Every value the slot ever holds is a valid function to
/// call, and every store writes the same answer for the CPU this process runs
/// on, so racing first calls all agree and no ordering beyond the atomic
/// accesses is needed: the targets are code, not data the resolver publishes.
var runtime_compress: CompressFn = &compressDetecting;

fn compressDetecting(state: *State, blocks: []const Block) void {
    const chosen: CompressFn = if (x86HasShaExtension()) &compressHardware else &compressPortable;
    @atomicStore(CompressFn, &runtime_compress, chosen, .monotonic);
    chosen(state, blocks);
}

/// Whether the CPU running this process reports the x86 SHA extension, along
/// with the SSSE3 that `roundX86Sha` also uses for `palignr`. Only meaningful
/// on x86_64.
pub fn x86HasShaExtension() bool {
    if (comptime arch_class != .x86_64) return false;
    // Leaf 0 reports the highest basic leaf; the SHA bit lives in leaf 7.
    if (cpuid(0, 0).eax < 7) return false;
    const ssse3 = (cpuid(1, 0).ecx & (1 << 9)) != 0;
    const sha = (cpuid(7, 0).ebx & (1 << 29)) != 0;
    return ssse3 and sha;
}

const CpuidRegisters = struct { eax: u32, ebx: u32, ecx: u32, edx: u32 };

fn cpuid(leaf: u32, sub_leaf: u32) CpuidRegisters {
    var eax: u32 = undefined;
    var ebx: u32 = undefined;
    var ecx: u32 = undefined;
    var edx: u32 = undefined;
    asm volatile ("cpuid"
        : [_] "={eax}" (eax),
          [_] "={ebx}" (ebx),
          [_] "={ecx}" (ecx),
          [_] "={edx}" (edx),
        : [_] "{eax}" (leaf),
          [_] "{ecx}" (sub_leaf),
    );
    return .{ .eax = eax, .ebx = ebx, .ecx = ecx, .edx = edx };
}

/// Compress every block into `state` with the rounds this build uses: the
/// hardware instructions when the compilation CPU has them, whichever of the
/// two `runtime_compress` resolved to for the CPU running this process when
/// the build dispatches, and otherwise the portable rounds.
pub fn compress(state: *State, blocks: []const Block) void {
    if (comptime hasHardwareSupport) {
        compressHardware(state, blocks);
    } else if (comptime dispatches_at_runtime) {
        @atomicLoad(CompressFn, &runtime_compress, .monotonic)(state, blocks);
    } else {
        compressPortable(state, blocks);
    }
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
/// callable when `hasHardwareSupport` is true or, on a target that
/// `dispatches_at_runtime`, when `x86HasShaExtension` reports them.
pub fn compressHardware(state: *State, blocks: []const Block) void {
    switch (arch_class) {
        .aarch64 => for (blocks) |*block| roundAarch64Sha2(state, block),
        .x86_64 => for (blocks) |*block| roundX86Sha(state, block),
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

fn roundX86Sha(state: *State, block: *const Block) void {
    const V4u32 = @Vector(4, u32);
    var s: [64]u32 align(16) = undefined;
    loadSchedule(block, &s);
    var x: V4u32 = [_]u32{ state[5], state[4], state[1], state[0] };
    var y: V4u32 = [_]u32{ state[7], state[6], state[3], state[2] };
    const s_v = @as(*[16]V4u32, @ptrCast(&s));

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

    state[0] +%= x[3];
    state[1] +%= x[2];
    state[4] +%= x[1];
    state[5] +%= x[0];
    state[2] +%= y[3];
    state[3] +%= y[2];
    state[6] +%= y[1];
    state[7] +%= y[0];
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
