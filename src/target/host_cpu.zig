//! What the CPU running this compiler can execute.
//!
//! The `roc` binary is built for the architecture baseline so that one release
//! runs on every CPU of that architecture, which means `builtin.cpu` describes
//! the floor the binary was compiled to and says nothing about the machine it
//! ended up on. Code Roc generates to run on that machine — compile-time
//! execution, in-memory dev runs, and the target a command picks when the user
//! names none — must be held to what the machine can actually execute, and
//! only a runtime query answers that: CPUID on x86-64, the OS feature report
//! on aarch64.
//!
//! `llvmTargetQuery` defines what the `.default` CPU level promises. This
//! module states that same promise as the runtime queries which confirm it,
//! and the comptime check below fails the build if the two stop agreeing.

const std = @import("std");
const builtin = @import("builtin");

const target_mod = @import("mod.zig");

const CpuLevel = target_mod.CpuLevel;
const RocTarget = target_mod.RocTarget;

/// Where CPUID reports one feature `.default` code generation may use.
const X86Feature = struct {
    /// The LLVM feature this bit stands for, which is how `llvmTargetQuery`
    /// names it.
    feature: std.Target.x86.Feature,
    leaf: u32,
    subleaf: u32 = 0,
    register: enum { eax, ebx, ecx, edx },
    bit: u5,
    /// AVX-encoded instructions fault unless the OS saves YMM state, so the
    /// CPUID bit alone does not make them usable.
    needs_ymm_state: bool = false,
};

/// The x86-64 features `.default` code generation may use: the `x86-64-v2` and
/// `x86-64-v3` psABI levels, plus the two extensions Roc names on top of them
/// for its SIMD builtins.
///
/// `x86-64-v3` also implies `crc32`, which is absent here on purpose: Intel
/// reports it in the SSE4.2 bit below, and no Roc code generation emits the
/// instruction in the first place.
const x86_64_default_level_features = [_]X86Feature{
    // x86-64-v2
    .{ .feature = .sse3, .leaf = 0x1, .register = .ecx, .bit = 0 },
    .{ .feature = .ssse3, .leaf = 0x1, .register = .ecx, .bit = 9 },
    .{ .feature = .cx16, .leaf = 0x1, .register = .ecx, .bit = 13 },
    .{ .feature = .sse4_1, .leaf = 0x1, .register = .ecx, .bit = 19 },
    .{ .feature = .sse4_2, .leaf = 0x1, .register = .ecx, .bit = 20 },
    .{ .feature = .popcnt, .leaf = 0x1, .register = .ecx, .bit = 23 },
    .{ .feature = .sahf, .leaf = 0x8000_0001, .register = .ecx, .bit = 0 },

    // x86-64-v3
    .{ .feature = .fma, .leaf = 0x1, .register = .ecx, .bit = 12, .needs_ymm_state = true },
    .{ .feature = .movbe, .leaf = 0x1, .register = .ecx, .bit = 22 },
    .{ .feature = .xsave, .leaf = 0x1, .register = .ecx, .bit = 26 },
    .{ .feature = .avx, .leaf = 0x1, .register = .ecx, .bit = 28, .needs_ymm_state = true },
    .{ .feature = .f16c, .leaf = 0x1, .register = .ecx, .bit = 29, .needs_ymm_state = true },
    .{ .feature = .bmi, .leaf = 0x7, .register = .ebx, .bit = 3 },
    .{ .feature = .avx2, .leaf = 0x7, .register = .ebx, .bit = 5, .needs_ymm_state = true },
    .{ .feature = .bmi2, .leaf = 0x7, .register = .ebx, .bit = 8 },
    .{ .feature = .lzcnt, .leaf = 0x8000_0001, .register = .ecx, .bit = 5 },

    // Named on top of the level for the SIMD builtins.
    .{ .feature = .pclmul, .leaf = 0x1, .register = .ecx, .bit = 1 },
    .{ .feature = .aes, .leaf = 0x1, .register = .ecx, .bit = 25 },
};

/// AArch64 `HWCAP` bits from the Linux ABI, for the features `.default` code
/// generation may use beyond Armv8.0-A.
///
/// LLVM's `aes` covers both the AES instructions and the PMULL64 that carryless
/// multiply lowers to, which Linux reports separately.
const hwcap_aes = 1 << 3;
const hwcap_pmull = 1 << 4;
const hwcap_asimddp = 1 << 20;

comptime {
    // A target with a `v1` twin promises two floors, and this module decides
    // which of them a machine gets. Pin the shape of the `.default` floor, so
    // that raising it fails here rather than emitting instructions the runtime
    // query never asked the CPU about.
    for (std.enums.values(RocTarget)) |target| {
        if (target.cpuLevel() != .default) continue;
        if (target.baselineCpuTarget() == null) continue;

        const query = target.llvmTargetQuery();
        var expected_features = std.Target.Cpu.Feature.Set.empty;
        const expected_model = switch (target.toCpuArch()) {
            .x86_64 => model: {
                expected_features.addFeature(@intFromEnum(std.Target.x86.Feature.aes));
                expected_features.addFeature(@intFromEnum(std.Target.x86.Feature.pclmul));
                break :model &std.Target.x86.cpu.x86_64_v3;
            },
            .aarch64, .aarch64_be => model: {
                expected_features.addFeature(@intFromEnum(std.Target.aarch64.Feature.aes));
                expected_features.addFeature(@intFromEnum(std.Target.aarch64.Feature.dotprod));
                break :model &std.Target.aarch64.cpu.generic;
            },
            // A wasm runtime's feature set is the embedder's to decide and is
            // not reportable from inside the module, so `detect` answers with
            // the level every runtime executes and nothing here constrains it.
            else => continue,
        };

        const model_matches = switch (query.cpu_model) {
            .explicit => |model| model == expected_model,
            .baseline, .determined_by_arch_os, .native => false,
        };
        if (!model_matches or !query.cpu_features_add.eql(expected_features)) {
            @compileError("the CPU floor of " ++ target.toName() ++
                " moved; teach src/target/host_cpu.zig to detect the features it now requires");
        }
    }
}

/// A Linux auxiliary-vector lookup that answers in the released binary.
///
/// `std.os.linux.getauxval` resolves to Zig's own implementation whenever libc
/// is linked, and that implementation reads `std.os.linux.elf_aux_maybe` — a
/// variable assigned only by `posixCallMainAndExit`, the startup path Zig uses
/// when it owns `_start`. The `roc` binary sets `link_libc = true`, so libc
/// owns `_start`, Zig never sees the auxiliary vector, and every lookup
/// answers 0 rather than failing to build. Ask libc itself in that case: it
/// captured the vector during its own startup, and both musl and glibc expose
/// it under this name.
fn getauxval(index: usize) usize {
    if (comptime builtin.link_libc) return @intCast(std.c.getauxval(@intCast(index)));
    return std.os.linux.getauxval(index);
}

/// `0` means "not detected yet"; every other value is `encode`d.
var cached_level = std.atomic.Value(u8).init(0);

fn encode(level_value: CpuLevel) u8 {
    return @as(u8, @intFromEnum(level_value)) + 1;
}

fn decode(cached: u8) ?CpuLevel {
    if (cached == 0) return null;
    return @enumFromInt(cached - 1);
}

/// The highest CPU level this machine executes.
///
/// Detection runs once; it is idempotent, so racing callers agree.
pub fn level() CpuLevel {
    // A native target with no `v1` twin already sits on the architecture's
    // oldest revision, so every CPU that runs this binary runs its code.
    if (comptime RocTarget.detectNative().baselineCpuTarget() == null) return .default;

    if (decode(cached_level.load(.monotonic))) |cached| return cached;

    const detected = detect();
    cached_level.store(encode(detected), .monotonic);
    return detected;
}

/// The native target, spelled for the CPU level this machine executes.
///
/// This is what a command compiles for when the user names no `--target`: the
/// artifact runs here, so its floor is this machine's.
pub fn nativeTarget() RocTarget {
    const native = comptime RocTarget.detectNative();
    const baseline = comptime native.baselineCpuTarget();

    if (baseline) |baseline_target| {
        if (level() == .v1) return baseline_target;
    }

    return native;
}

fn detect() CpuLevel {
    switch (comptime builtin.cpu.arch) {
        .x86_64 => return detectX86_64(),
        .aarch64, .aarch64_be => return detectAarch64(),
        // A wasm module cannot ask its runtime which proposals the runtime
        // implements, so it gets the level every runtime executes. The
        // architectures left over have no `v1` twin, so `level` answers them
        // before reaching here.
        else => return .v1,
    }
}

const CpuidLeaf = struct { eax: u32, ebx: u32, ecx: u32, edx: u32 };

fn cpuid(leaf: u32, subleaf: u32) CpuidLeaf {
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
          [_] "{ecx}" (subleaf),
    );

    return .{ .eax = eax, .ebx = ebx, .ecx = ecx, .edx = edx };
}

/// XCR0, which reports which extended CPU state the OS has committed to saving
/// across context switches.
fn xcr0() u32 {
    return asm volatile (
        \\ xor %%ecx, %%ecx
        \\ xgetbv
        : [_] "={eax}" (-> u32),
        :
        : .{ .edx = true, .ecx = true });
}

fn bitIsSet(value: u32, bit: u5) bool {
    return (value >> bit) & 1 != 0;
}

/// The parts of a CPUID query that every feature check shares.
const X86Cpuid = struct {
    max_leaf: u32,
    max_extended_leaf: u32,
    ymm_state_saved: bool,

    fn query() X86Cpuid {
        // Whether the OS saves the SSE and AVX halves of YMM state. Reading
        // XCR0 at all takes the OSXSAVE bit, which is what says the OS enabled
        // XGETBV.
        const ymm_state_saved = saved: {
            if (!bitIsSet(cpuid(0x1, 0).ecx, 27)) break :saved false;
            const state = xcr0();
            break :saved bitIsSet(state, 1) and bitIsSet(state, 2);
        };

        return .{
            .max_leaf = cpuid(0, 0).eax,
            .max_extended_leaf = cpuid(0x8000_0000, 0).eax,
            .ymm_state_saved = ymm_state_saved,
        };
    }

    fn reports(self: X86Cpuid, required: X86Feature) bool {
        const leaf_available = if (required.leaf >= 0x8000_0000)
            required.leaf <= self.max_extended_leaf
        else
            required.leaf <= self.max_leaf;
        if (!leaf_available) return false;

        if (required.needs_ymm_state and !self.ymm_state_saved) return false;

        const leaf = cpuid(required.leaf, required.subleaf);
        const register = switch (required.register) {
            .eax => leaf.eax,
            .ebx => leaf.ebx,
            .ecx => leaf.ecx,
            .edx => leaf.edx,
        };

        return bitIsSet(register, required.bit);
    }
};

fn detectX86_64() CpuLevel {
    const cpu = X86Cpuid.query();

    for (x86_64_default_level_features) |required| {
        if (!cpu.reports(required)) return .v1;
    }

    return .default;
}

fn detectAarch64() CpuLevel {
    switch (comptime builtin.os.tag) {
        .linux => {
            const required = hwcap_aes | hwcap_pmull | hwcap_asimddp;
            const hwcap = getauxval(std.elf.AT_HWCAP);
            return if (hwcap & required == required) .default else .v1;
        },
        .windows => {
            const windows = std.os.windows;
            if (!windows.IsProcessorFeaturePresent(.ARM_V8_CRYPTO_INSTRUCTIONS_AVAILABLE)) return .v1;
            if (!windows.IsProcessorFeaturePresent(.ARM_V82_DP_INSTRUCTIONS_AVAILABLE)) return .v1;
            return .default;
        },
        else => return .v1,
    }
}

test "the auxiliary vector is readable however this binary was started" {
    if (comptime builtin.os.tag != .linux) return error.SkipZigTest;

    // `AT_PAGESZ` is present in every Linux process on every architecture and
    // is never 0, so a 0 here is the lookup coming back empty rather than the
    // machine lacking something. That is the failure this wrapper exists for:
    // aarch64 detection reads `AT_HWCAP`, and a lookup that always answers 0
    // reports every aarch64 machine as `.v1` without any sign of trouble.
    try std.testing.expect(getauxval(std.elf.AT_PAGESZ) != 0);
}

test "libc-linked builds do not read the auxiliary vector through Zig's startup path" {
    if (comptime builtin.os.tag != .linux) return error.SkipZigTest;
    if (comptime !builtin.link_libc) return error.SkipZigTest;

    // The reason `getauxval` above exists, pinned so that a Zig upgrade which
    // makes `std.os.linux.getauxval` work under libc shows up here as a
    // failing test rather than as a wrapper nobody can justify removing.
    try std.testing.expect(std.os.linux.elf_aux_maybe == null);
    try std.testing.expectEqual(@as(usize, 0), std.os.linux.getauxval(std.elf.AT_PAGESZ));
}

test "detection answers a level the native target has a spelling for" {
    const detected = level();
    const native = RocTarget.detectNative();

    if (native.baselineCpuTarget() == null) {
        try std.testing.expectEqual(CpuLevel.default, detected);
    }
}

test "the native target carries the detected level" {
    const native = nativeTarget();

    try std.testing.expectEqual(RocTarget.detectNative(), native.defaultCpuTarget());
    try std.testing.expectEqual(level(), native.cpuLevel());
}

test "detection is cached rather than repeated" {
    const first = level();
    try std.testing.expectEqual(first, level());
    try std.testing.expectEqual(first, decode(cached_level.load(.monotonic)) orelse first);
}

test "every CPUID bit this checks belongs to a feature the default level names" {
    if (comptime builtin.cpu.arch != .x86_64) return error.SkipZigTest;

    // The floor is `x86_64_v3` plus the two extensions named on top of it, and
    // the table below is how a machine is asked whether it has that floor. A
    // feature in the table that the floor does not include would reject CPUs
    // over an instruction Roc never emits.
    var floor = std.Target.x86.cpu.x86_64_v3.features;
    floor.addFeature(@intFromEnum(std.Target.x86.Feature.aes));
    floor.addFeature(@intFromEnum(std.Target.x86.Feature.pclmul));
    floor.populateDependencies(std.Target.Cpu.Arch.x86_64.allFeaturesList());

    for (x86_64_default_level_features) |required| {
        try std.testing.expect(floor.isEnabled(@intFromEnum(required.feature)));
    }
}

test "CPUID reports every feature this binary was built to use" {
    if (comptime builtin.cpu.arch != .x86_64) return error.SkipZigTest;

    // Detection is a claim about the machine running this test, and the fact
    // available to check it against is that this process is running: a feature
    // the build enabled is a feature this CPU has, so detection must find it.
    const cpu = X86Cpuid.query();

    for (x86_64_default_level_features) |required| {
        if (!builtin.cpu.features.isEnabled(@intFromEnum(required.feature))) continue;
        try std.testing.expect(cpu.reports(required));
    }
}
