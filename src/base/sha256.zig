//! The compiler's SHA-256, on the CPU's SHA-256 instructions wherever the
//! compiler target has them. aarch64 compiler targets are built with the
//! instructions enabled (see `getReleaseTargetQuery` in build.zig) and released
//! x86_64 compilers for anything but macOS detect them once per process (see
//! `dispatches_at_runtime`), so this costs one hardware compression per 64
//! bytes nearly everywhere; 32-bit targets such as wasm32, x86_64 macOS (see
//! `uses_software_rounds`) and x86-64 CPUs without the SHA extension use the
//! portable rounds, which produce the same digest bytes more slowly.
//!
//! The rounds and the hasher are the ones the `Crypto` builtins use (see
//! `src/builtins/sha256.zig`); only the runtime dispatch here is the
//! compiler's own. The interface mirrors `std.crypto.hash.sha2.Sha256` and
//! the digests are the standard SHA-256 bytes, so every identity and cache
//! key the compiler hashes is the same whichever implementation computed it.

const std = @import("std");
const builtin = @import("builtin");
const rounds = @import("builtins").sha256;

comptime {
    // Every 64-bit target other than x86_64 must carry the SHA-256
    // instructions: there is no software path for them, by decision. build.zig
    // adds the feature to the baseline CPU; a `-Dcpu` that drops it is an
    // unsupported target. x86_64 always has a path: the hardware rounds,
    // runtime dispatch, or the portable rounds (see `dispatches_at_runtime`
    // and `uses_software_rounds`).
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

/// The compiler's incremental SHA-256 hasher.
pub const Sha256 = rounds.Hasher(compress);

/// Whether this compiler computes digests with the portable rounds only,
/// never asking the CPU for its SHA-256 instructions. x86_64 macOS is the only
/// 64-bit target that does: Apple's Intel Macs are Skylake through Comet Lake,
/// whose cores have no SHA extension (only the 2020 Ice Lake MacBook Air
/// does), so a macos_x86_64 build with the extension in its baseline dies of
/// SIGILL on nearly every Intel Mac -- including the Coffee Lake i7-8700B that
/// GitHub's macos-15-intel runner builds the nightly on. Digest bytes are
/// identical either way; only the speed differs. An Intel Mac that does have
/// the extension still uses the hardware rounds when the build names its CPU
/// (`-Dcpu=native`), because that sets `hasHardwareSupport`.
pub const uses_software_rounds = rounds.arch_class == .x86_64 and builtin.os.tag == .macos;

/// Whether this compiler chooses between the hardware and portable rounds at
/// runtime. That is every x86_64 target whose compilation CPU lacks the SHA
/// extension, other than macOS: the build keeps x86_64 at the architecture
/// baseline so that one released binary runs on every x86-64 CPU, and the
/// SHA extension is missing from Intel's Skylake through Comet Lake cores,
/// which are still common on Linux and Windows machines. A build that names a
/// CPU with the extension (`-Dcpu=native` on such a CPU, or any `-Dcpu` at or
/// above Ice Lake / Zen) takes `hasHardwareSupport` instead and pays no
/// dispatch.
///
/// Only LLVM assembles the x86 hardware rounds for a CPU without the
/// extension: its assembler accepts any x86 instruction, whereas Zig's
/// self-hosted x86_64 backend (the Debug backend) encodes only instructions in
/// the target CPU's feature set and rejects the SHA and SSSE3 ones otherwise.
/// So a self-hosted build without the feature has no hardware rounds to
/// dispatch to and uses the portable rounds; build.zig keeps Debug builds on a
/// machine with the extension at `hasHardwareSupport` (see `withSha256Floor`
/// there).
///
/// This dispatch is for the compiler binary only. Code compiled into Roc
/// programs, including the `Crypto` builtins, never selects rounds at runtime.
pub const dispatches_at_runtime = rounds.arch_class == .x86_64 and !rounds.hasHardwareSupport and !uses_software_rounds and builtin.zig_backend == .stage2_llvm;

fn compress(state: *rounds.State, blocks: []const rounds.Block) void {
    if (comptime dispatches_at_runtime) {
        @atomicLoad(CompressFn, &runtime_compress, .monotonic)(state, blocks);
    } else {
        rounds.compressForTarget(state, blocks);
    }
}

const CompressFn = *const fn (*rounds.State, []const rounds.Block) void;

/// The function `compress` goes through next on a build that
/// `dispatches_at_runtime`. It starts as `compressDetecting`, which asks CPUID
/// once and then replaces it with the answer, so every later call is one load
/// and one indirect call. Every value the slot ever holds is a valid function
/// to call, and every store writes the same answer for the CPU this process
/// runs on, so racing first calls all agree and no ordering beyond the atomic
/// accesses is needed: the targets are code, not data the resolver publishes.
var runtime_compress: CompressFn = &compressDetecting;

fn compressDetecting(state: *rounds.State, blocks: []const rounds.Block) void {
    const chosen: CompressFn = if (x86HasShaExtension()) &rounds.compressHardware else &rounds.compressPortable;
    @atomicStore(CompressFn, &runtime_compress, chosen, .monotonic);
    chosen(state, blocks);
}

/// Whether the CPU running this process reports the x86 SHA extension, along
/// with the SSSE3 that the x86 hardware rounds also use for `palignr`. Only
/// meaningful on x86_64.
fn x86HasShaExtension() bool {
    if (comptime rounds.arch_class != .x86_64) return false;
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

test "the compiler's digests agree with std.crypto" {
    var input: [64 * 3 + 17]u8 = undefined;
    for (&input, 0..) |*byte, i| byte.* = @truncate(i *% 0x9d +% 0x11);
    for ([_]usize{ 0, 3, 64, 100, input.len }) |len| {
        var expected: [Sha256.digest_length]u8 = undefined;
        std.crypto.hash.sha2.Sha256.hash(input[0..len], &expected, .{});
        var actual: [Sha256.digest_length]u8 = undefined;
        Sha256.hash(input[0..len], &actual, .{});
        try std.testing.expectEqual(expected, actual);
    }
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
    if (comptime rounds.hasHardwareSupport or dispatches_at_runtime) {
        if (rounds.hasHardwareSupport or x86HasShaExtension()) {
            var hardware = rounds.initial_state;
            rounds.compressHardware(&hardware, @as(*const [1]rounds.Block, &block));
            try std.testing.expectEqual(portable, hardware);
        }
    }
}
