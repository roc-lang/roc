const std = @import("std");
const stack_budget = @import("src/base/stack_budget.zig");
const builtin = @import("builtin");
const modules = @import("src/build/modules.zig");
const glibc_stub_build = @import("src/build/glibc_stub.zig");
const ci_steps = @import("src/build/ci_steps.zig");
const roc_target = @import("src/target/mod.zig");
const TestFixturePlan = @import("src/build/test_fixtures.zig").Plan;
const isImmutableNixStorePath = @import("src/build/nix_path.zig").isImmutableStorePath;
var test_fixtures: TestFixturePlan = undefined;
const Dependency = std.Build.Dependency;
const OptimizeMode = std.builtin.OptimizeMode;
const ResolvedTarget = std.Build.ResolvedTarget;
const Step = std.Build.Step;

var build_checks_exe: ?*Step.Compile = null;

fn buildChecksRun(b: *std.Build, command: []const u8) *Step.Run {
    const exe = build_checks_exe orelse blk: {
        const tool = b.addExecutable(.{
            .name = "roc-build-checks",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/build/check_runner.zig"),
                .target = b.graph.host,
                .optimize = .fast,
            }),
        });
        build_checks_exe = tool;
        break :blk tool;
    };
    const run = b.addRunArtifact(exe);
    run.step.name = command;
    run.addArg(command);
    run.setCwd(b.path("."));
    // Check commands operate on declared, staged source trees. Git/JJ and
    // mutation commands intentionally execute every time.
    if (std.mem.startsWith(u8, command, "check-") and
        !std.mem.eql(u8, command, "check-snapshot-diff"))
    {
        const inputs = b.addWriteFiles();
        _ = inputs.addCopyDirectory(b.path("src"), "src", .{ .include_extensions = &.{ ".zig", ".roc", ".pl" } });
        _ = inputs.addCopyDirectory(b.path("test"), "test", .{ .include_extensions = &.{".roc"} });
        _ = inputs.addCopyDirectory(b.path("ci"), "ci", .{ .exclude_extensions = &.{ ".pyc", ".pyo" } });
        _ = inputs.addCopyFile(b.path("design.md"), "design.md");
        run.setCwd(inputs.getDirectory());
        run.expectExitCode(0);
    }
    return run;
}

// Cross-compile target definitions

/// Cross-compile target specification
const CrossTarget = struct {
    name: []const u8,
    query: std.Target.Query,
};

/// Musl-only cross-compile targets (static linking)
const musl_cross_targets = [_]CrossTarget{
    .{ .name = "x64musl", .query = .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .musl } },
    .{ .name = "arm64musl", .query = .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .musl } },
};

/// Glibc cross-compile targets (dynamic linking)
const glibc_cross_targets = [_]CrossTarget{
    .{ .name = "x64glibc", .query = .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .gnu } },
    .{ .name = "arm64glibc", .query = .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .gnu } },
};

/// Windows cross-compile targets
const windows_cross_targets = [_]CrossTarget{
    .{ .name = "x64win", .query = .{ .cpu_arch = .x86_64, .os_tag = .windows, .abi = .msvc } },
    .{ .name = "x64mingw", .query = .{ .cpu_arch = .x86_64, .os_tag = .windows, .abi = .gnu } },
    .{ .name = "arm64win", .query = .{ .cpu_arch = .aarch64, .os_tag = .windows, .abi = .msvc } },
    .{ .name = "arm64mingw", .query = .{ .cpu_arch = .aarch64, .os_tag = .windows, .abi = .gnu } },
};

/// MinGW C runtime files that a `*mingw` target must find next to `host.lib`.
///
/// A MinGW link passes `-lldmingw /nodefaultlib` and then only the inputs the
/// platform declares (see `src/cli/linker.zig`), so the startup object, the
/// compiled runtime archives, and the UCRT/Win32 import libraries all have to
/// live in the platform's `targets/<target>/` directory. `test/fx` holds the
/// checked-in copy (regenerate with `ci/vendor_mingw_runtime.py`) and the build
/// copies it into the other test platforms rather than committing duplicates.
const mingw_runtime_files = @import("src/echo_platform/mingw_runtime.zig").files;

/// Copies fx's checked-in MinGW C runtime into another test platform's
/// `platform/targets/<target_name>/` directory, so a `*mingw` link finds every
/// input the platform declares without committing more copies of the same
/// binaries. Returns the step so callers can order it after the host build.
fn copyMingwRuntimeToTestPlatform(
    b: *std.Build,
    platform_dir: []const u8,
    target_name: []const u8,
) *Step {
    const copy = b.addWriteFiles();
    for (mingw_runtime_files) |runtime_filename| {
        test_fixtures.copy(
            copy,
            b.path(b.pathJoin(&.{ "test/fx/platform/targets", target_name, runtime_filename })),
            b.pathJoin(&.{ "test", platform_dir, "platform/targets", target_name, runtime_filename }),
        );
    }
    return &copy.step;
}

/// BSD cross-compile targets
const bsd_cross_targets = [_]CrossTarget{
    .{ .name = "x64freebsd", .query = .{ .cpu_arch = .x86_64, .os_tag = .freebsd, .abi = .none } },
    .{ .name = "x64openbsd", .query = .{ .cpu_arch = .x86_64, .os_tag = .openbsd, .abi = .none } },
    .{ .name = "x64netbsd", .query = .{ .cpu_arch = .x86_64, .os_tag = .netbsd, .abi = .none } },
};

/// All Linux cross-compile targets (musl + glibc)
const linux_cross_targets = musl_cross_targets ++ glibc_cross_targets;

comptime {
    // The prebuilt builtins objects these targets produce are what
    // `roc build --opt=dev --target=X` links, and `BuiltinsObjects.forTarget`
    // hands the same object to a `v1` target as to its default twin. That is
    // only sound while these queries name no CPU model, because Zig then
    // resolves them to the architecture baseline. Naming a model here would
    // put instructions the baseline lacks into every `v1` binary, so it has to
    // come with per-CPU-level objects instead.
    for (musl_cross_targets ++ glibc_cross_targets ++ windows_cross_targets ++ bsd_cross_targets) |cross_target| {
        if (cross_target.query.cpu_model != .determined_by_arch_os) {
            @compileError("cross-compile target " ++ cross_target.name ++
                " names a CPU model; baseline (v1) targets would link non-baseline builtins");
        }
        if (!cross_target.query.cpu_features_add.isEmpty()) {
            @compileError("cross-compile target " ++ cross_target.name ++
                " adds CPU features; baseline (v1) targets would link non-baseline builtins");
        }
    }
}

/// Test platform directories that need host libraries built
const all_test_platform_dirs = [_][]const u8{ "str", "int", "fx", "fx-open", "dylib", "archive", "alloc-count", "box-model-uniqueness" };

fn mustUseLlvm(target: ResolvedTarget) bool {
    return target.result.os.tag == .macos and target.result.cpu.arch == .x86_64;
}

fn isGlibcTestHost(target: ResolvedTarget) bool {
    return target.result.os.tag == .linux and target.result.abi == .gnu;
}

fn testHostNeedsLlvm(target: ResolvedTarget) bool {
    // macOS x86_64 must use the LLVM backend. All Linux test hosts use it too:
    // the symbol-ABI tests need the ELF @export visibility metadata Zig's LLVM
    // backend emits, and glibc hosts additionally rely on LLVM for DCE.
    return mustUseLlvm(target) or target.result.os.tag == .linux;
}

fn testHostNeedsCompilerRt(target: ResolvedTarget) bool {
    return target.result.os.tag == .linux or
        mustUseLlvm(target) or
        (target.result.os.tag == .windows and target.result.cpu.arch == .aarch64);
}

/// Every executable that links the whole compiler emits more `.text` than an
/// ARM32 branch can reach. LLD places range-extension thunks *between* input
/// sections, so a monolithic `.text` leaves it nowhere to put one and the link
/// fails with "InputSection too large for range extension thunk". Splitting per
/// function and per datum gives LLD those insertion points, and lets the final
/// link discard unused compiler code on every target.
fn splitCompilerSections(step: *Step.Compile) void {
    step.link_function_sections = true;
    step.link_data_sections = true;
}

fn configureBackend(step: *Step.Compile, target: ResolvedTarget) void {
    if (mustUseLlvm(target)) {
        step.use_llvm = true;
    }
}

/// The watch module uses real macOS FSEvents (CoreFoundation/CoreServices) when building
/// natively for macOS, and kernel32 for directory-change notifications on Windows. Any step
/// that compiles the watch module (the roc exe and the tests that import it) must link these.
fn linkWatchPlatformLibs(step: *Step.Compile, target: ResolvedTarget) void {
    if (target.result.os.tag == .macos and
        builtin.target.os.tag == .macos and
        target.result.cpu.arch == builtin.target.cpu.arch and
        target.result.abi == builtin.target.abi)
    {
        step.root_module.linkFramework("CoreFoundation", .{});
        step.root_module.linkFramework("CoreServices", .{});
    } else if (target.result.os.tag == .windows) {
        step.root_module.linkSystemLibrary("kernel32", .{});
    }
}

const TestHostOptions = struct {
    uses_stack_handler: bool = false,
    /// Extra source files compiled as their own objects and added to the host
    /// archive as separate members, for platforms that test multi-member hosts.
    extra_sources: []const []const u8 = &.{},
};

/// The dylib host keeps its hosted functions in a second archive member that
/// only the app references, so `run-test-dylib` fails if a link ever stops
/// rooting the hosted symbols it uses (see test/dylib/platform/host_hosted.zig).
fn testPlatformExtraHostSources(platform_dir: []const u8) []const []const u8 {
    if (std.mem.eql(u8, platform_dir, "dylib")) return &.{"test/dylib/platform/host_hosted.zig"};
    return &.{};
}

fn testPlatformUsesStackHandler(platform_dir: []const u8) bool {
    return std.mem.eql(u8, platform_dir, "fx") or std.mem.eql(u8, platform_dir, "fx-open");
}

fn testPlatformRequiresSectionDceHost(platform_dir: []const u8) bool {
    return std.mem.eql(u8, platform_dir, "dylib") or std.mem.eql(u8, platform_dir, "archive");
}

/// The v1 twin that needs its own test-platform host build.
fn muslBaselineTestTargetName(target_name: []const u8) ?[]const u8 {
    if (std.mem.eql(u8, target_name, "x64musl")) return "x64v1musl";
    if (std.mem.eql(u8, target_name, "arm64musl")) return "arm64v1musl";
    return null;
}

fn testHostNeedsLibc(options: TestHostOptions, target: ResolvedTarget) bool {
    if (!options.uses_stack_handler) return false;

    return switch (target.result.os.tag) {
        .linux,
        .macos,
        .ios,
        .tvos,
        .watchos,
        .visionos,
        .freebsd,
        .dragonfly,
        .netbsd,
        .openbsd,
        => true,
        .freestanding,
        .other,
        .contiki,
        .fuchsia,
        .hermit,
        .managarm,
        .haiku,
        .hurd,
        .illumos,
        .plan9,
        .rtems,
        .serenity,
        .driverkit,
        .maccatalyst,
        .windows,
        .uefi,
        .wiiu,
        .@"switch",
        .gba,
        .psx,
        .tios,
        .ashetos,
        .@"3ds",
        .ps3,
        .ps4,
        .ps5,
        .psp,
        .vita,
        .emscripten,
        .wasi,
        .amdhsa,
        .amdpal,
        .cuda,
        .mesa3d,
        .nvcl,
        .opencl,
        .opengl,
        .vulkan,
        => false,
    };
}

fn isNativeishOrMusl(target: ResolvedTarget) bool {
    return target.result.cpu.arch == builtin.target.cpu.arch and
        target.result.os.tag == builtin.target.os.tag and
        (target.query.isNativeAbi() or target.result.abi.isMusl());
}

const NativeSharedArchiveTarget = struct {
    resolved: ResolvedTarget,
    roc_name: []const u8,
};

fn nativeSharedArchiveTarget(b: *std.Build, target: ResolvedTarget) NativeSharedArchiveTarget {
    if (target.result.os.tag == .linux) {
        const arch = target.result.cpu.arch;
        return if (arch == .x86_64) .{
            .resolved = b.resolveTargetQuery(.{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .gnu }),
            .roc_name = "x64glibc",
        } else if (arch == .aarch64) .{
            .resolved = b.resolveTargetQuery(.{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .gnu }),
            .roc_name = "arm64glibc",
        } else .{
            .resolved = target,
            .roc_name = roc_target.RocTarget.fromStdTarget(target.result).toName(),
        };
    }

    return .{
        .resolved = target,
        .roc_name = roc_target.RocTarget.fromStdTarget(target.result).toName(),
    };
}

fn withRocMacosDeploymentTarget(b: *std.Build, target: ResolvedTarget) ResolvedTarget {
    if (target.result.os.tag != .macos) return target;

    return b.resolveTargetQuery(roc_target.macos_deployment.query(target.result.cpu.arch));
}

/// Returns the target query for release builds on the current host.
///
/// `-Dtarget` and `-Dcpu` select the release target, so a packager can trade
/// portability for speed. With neither flag, the defaults are:
/// - Linux: Uses musl for fully static binaries
/// - x86_64 and aarch64: Uses the baseline CPU, so a single released binary
///   runs on every CPU of that architecture
///
/// What that floor costs the parts of this binary which aren't declared as
/// dependencies in build.zig.zon:
///
/// - Zig's standard library has no runtime CPU dispatch anywhere. Its vector
///   width is picked at comptime by `std.simd.suggestVectorLength` from
///   `builtin.cpu`: SSE gives 128 bits, AVX2 gives 256, AVX-512 gives 512. So a
///   baseline build halves the width used by `std.mem`, `std.unicode`,
///   `std.crypto.blake3` (which is what `roc bundle` hashes with),
///   `std.compress.flate`, and `std.http.HeadParser`. It does not make them
///   scalar.
/// - musl has no ifunc and no x86_64 string assembly, so it is unaffected.
/// - compiler_rt has no dispatch and nothing above baseline to give up.
/// - Zig string primitives use scalar/word operations and standard-library
///   helpers. Roc SIMD operations go through the compiler's explicit lowering.
///   SHA rounds separately use target-specific assembly with runtime dispatch.
///
/// build.zig.zon carries the same accounting for each declared dependency.
fn getReleaseTargetQuery(b: *std.Build, target: ResolvedTarget) std.Target.Query {
    // A `-Dtarget` triple names the entire target, ABI included, so it replaces
    // every default below.
    if (b.user_input_options.contains("target")) return target.query;

    // `-Dcpu` names only the CPU, so it carries over onto the release ABI
    // defaults rather than replacing them.
    const explicit_cpu = b.user_input_options.contains("cpu");
    var query: std.Target.Query = if (explicit_cpu) target.query else .{};

    // Use musl on Linux for static linking
    if (builtin.target.os.tag == .linux) {
        query.abi = .musl;
    }

    if (!explicit_cpu) {
        // An otherwise-empty query means "native CPU", which bakes the build
        // machine's CPU features into the released binary. Pin the CPU model
        // explicitly so releases stay portable.
        const arch = builtin.target.cpu.arch;
        if (arch == .x86_64 or arch == .aarch64) {
            // Baseline x86-64 is the 2003 instruction set, which every x86-64
            // CPU supports. A higher floor makes the binary die of SIGILL
            // before it can print anything on any CPU below that floor: pinning
            // x86_64_v3 requires AVX2 and BMI2, which excludes Intel's Celeron
            // and Pentium Silver lines along with everything pre-Haswell.
            query.cpu_model = .baseline;
            // Baseline aarch64 is armv8.0-a, which every aarch64 device supports.
            // Building natively on an armv9 CI runner emitted SVE (plus LSE and
            // RCPC) instructions, which crashed with SIGILL on older phones and
            // Raspberry Pis. On macOS, baseline is apple_m1, i.e. all Apple Silicon.
        }
        addSha256Floor(&query);
    }

    return query;
}

/// `addSha256Floor` applied to the target `zig build` compiles for, unless the
/// query named a CPU model explicitly (`-Dcpu`), in which case the caller's
/// choice stands and `TypeDigestHasher` reports a missing feature at compile
/// time.
///
/// A Debug build for this machine's own x86_64 architecture and OS also gets
/// the SHA extension (with the SSSE3 the rounds use) when this machine has
/// it. Debug uses Zig's self-hosted x86_64 backend, which encodes only
/// instructions in the target CPU's feature set, so without the feature such
/// a build has no hardware rounds at all and computes every digest with the
/// portable rounds (see `dispatches_at_runtime` in
/// src/base/sha256.zig). The floor keeps Debug builds and test suites
/// on hardware rounds where the CPU allows it; a machine without the
/// extension gets the portable rounds, and every LLVM build stays at the
/// architecture baseline and dispatches at runtime, like a released binary.
fn withSha256Floor(b: *std.Build, target: ResolvedTarget, optimize: std.builtin.OptimizeMode) ResolvedTarget {
    var query = target.query;
    switch (query.cpu_model) {
        .determined_by_arch_os, .baseline => {},
        .native, .explicit => return target,
    }
    addSha256Floor(&query);
    if (optimize == .debug and hostBuildsDebugWithSha(target)) {
        query.cpu_features_add.addFeature(@backingInt(std.Target.x86.Feature.sha));
        query.cpu_features_add.addFeature(@backingInt(std.Target.x86.Feature.ssse3));
    }
    return b.resolveTargetQuery(query);
}

/// `target` without the SHA-256 instructions `withSha256Floor` adds, for the
/// native builtins objects. Those objects are linked into programs for Roc
/// targets whose CPU contract may lack the instructions (`BuiltinsObjects.forTarget`
/// in src/cli/main.zig hands a `v1` target its default twin's object), and the
/// `Crypto` builtins compress with the SHA-256 instructions exactly when their
/// object is compiled with them (see `Rounds` in src/builtins/sha256.zig). A
/// `-Dcpu` choice stands, as it does for `withSha256Floor`.
fn withoutSha256Floor(b: *std.Build, target: ResolvedTarget) ResolvedTarget {
    var query = target.query;
    switch (query.cpu_model) {
        .determined_by_arch_os, .baseline => {},
        .native, .explicit => return target,
    }
    switch (roc_target.classifyCpuArch(target.result.cpu.arch)) {
        .x86_64 => query.cpu_features_add.removeFeature(@backingInt(std.Target.x86.Feature.sha)),
        .aarch64 => query.cpu_features_add.removeFeature(@backingInt(std.Target.aarch64.Feature.sha2)),
        .aarch64_be, .arm, .wasm32, .other => return target,
    }
    return b.resolveTargetQuery(query);
}

/// Whether `target` is this machine's own x86_64 architecture and OS and this
/// machine's CPU has the SHA extension and SSSE3. `builtin.cpu` here is the
/// CPU the build runner was compiled for, which Zig detects from the machine.
/// x86_64 macOS never takes the floor: it computes digests with the portable
/// rounds unless `-Dcpu` names its CPU (see `uses_software_rounds` in
/// src/base/sha256.zig).
fn hostBuildsDebugWithSha(target: ResolvedTarget) bool {
    if (builtin.target.cpu.arch != .x86_64) return false;
    if (target.result.cpu.arch != .x86_64 or target.result.os.tag != builtin.target.os.tag) return false;
    if (target.result.os.tag == .macos) return false;
    return builtin.cpu.hasAll(.x86, &.{ .sha, .ssse3 });
}

/// Raise an aarch64 compiler target's CPU floor to include the SHA-256
/// instructions. Type digests are cryptographic SHA-256 (see
/// `src/base/TypeDigestHasher.zig` for why) and aarch64 computes them in
/// hardware with no software rounds, so an aarch64 CPU without these
/// instructions is not a supported host for the compiler. This is the only
/// feature added above the architecture baseline: the `sha2` crypto extension,
/// present on all Apple Silicon, Graviton, Ampere and Raspberry Pi 5, absent on
/// the Cortex-A53/A72 in Raspberry Pi 4 and earlier. A `-Dcpu` that omits the
/// feature fails to compile `Sha256` rather than silently getting a
/// slower binary.
///
/// x86_64 gets no floor here. Its SHA extension is missing from Intel's
/// 2015-2020 Skylake through Comet Lake cores, which are every Intel Mac and
/// still a common Linux and Windows machine, so the binary stays at the
/// architecture baseline and the rounds are chosen for the CPU it runs on: at
/// runtime by CPUID on every x86_64 target except macOS (see
/// `dispatches_at_runtime` in src/base/sha256.zig), and always the
/// portable rounds on x86_64 macOS (see `uses_software_rounds` there). Debug
/// builds for this machine are the one exception, in `withSha256Floor`.
fn addSha256Floor(query: *std.Target.Query) void {
    const arch = query.cpu_arch orelse builtin.target.cpu.arch;
    switch (roc_target.classifyCpuArch(arch)) {
        .aarch64 => query.cpu_features_add.addFeature(@backingInt(std.Target.aarch64.Feature.sha2)),
        .x86_64, .aarch64_be, .arm, .wasm32, .other => {},
    }
}

const TestsSummaryStep = struct {
    step: *Step,
    run: *Step.Run,
    serialize_runs: bool = false,
    last_run: ?*Step = null,

    fn create(b: *std.Build, test_filters: []const []const u8, forced_passes: usize) *TestsSummaryStep {
        const self = b.allocator.create(TestsSummaryStep) catch @panic("OOM");
        const run = buildChecksRun(b, "tests-summary");
        run.addArg(b.fmt("{d}", .{forced_passes}));
        run.addArg(b.fmt("{d}", .{test_filters.len}));
        run.addArgs(test_filters);
        self.* = .{ .step = &run.step, .run = run };
        return self;
    }

    fn setRunSerialization(self: *TestsSummaryStep) void {
        self.serialize_runs = true;
    }

    fn addRun(self: *TestsSummaryStep, run_step: *Step) void {
        const run: *Step.Run = @fieldParentPtr("step", run_step);
        // Dynamic passthrough deliberately keeps tests runnable on every
        // invocation. Zig 0.17 uses the argument hash for those output paths,
        // so the basename must also distinguish unrelated test producers.
        const report = run.addPrefixedOutputFileArg(
            "--roc-test-report=",
            run.step.owner.fmt("{s}.tsv", .{run.producer.?.name}),
        );
        self.run.addArg(run.producer.?.name);
        self.run.addFileArg(report);
        if (self.serialize_runs) {
            if (self.last_run) |last_run| run_step.dependOn(last_run);
            self.last_run = run_step;
        }
        self.step.dependOn(run_step);
    }
};

/// One environment variable, applied identically to both runs of a test suite.
const TestSuiteEnv = struct {
    key: []const u8,
    value: []const u8,
};

/// Everything that differs between the Zig test suites registered with
/// `TestsSummaryStep`. Anything not expressible here is applied uniformly by
/// `TestSuiteRegistry.register`. If a suite ever needs a knob that is missing
/// (a working directory, `has_side_effects`, ...), add a field here rather than
/// hand-wiring that one suite at its call site -- see the note on `register`.
const TestSuiteSpec = struct {
    /// Suffix appended to `run-test-zig-`. This is also the MiniCI job name in
    /// `src/build/minici.zig`, so it must stay stable.
    step_suffix: []const u8,
    /// Description for the public `run-test-zig-<step_suffix>` step.
    description: []const u8,
    /// The test binary. Both runs execute this one `Step.Compile`, so it is
    /// compiled once no matter which step was asked for.
    compile: *Step.Compile,
    /// Extra edges both runs need: copied host libraries, installed prebuilt
    /// apps, the `roc` CLI, and so on.
    deps: []const *Step = &.{},
    /// Environment variables both runs need.
    env: []const TestSuiteEnv = &.{},
    /// Whether a bare `zig build` should also build this test binary.
    default_step: bool = false,
    /// Run fixture consumers in a private prepared project directory.
    fixture_root: bool = false,
};

/// Configures Roc's registered Zig unit tests to use Zig's stock runner with
/// stack-trace capture controlled by `-Ddebug-gpa-traces`.
const UnitTestRunner = struct {
    path: std.Build.LazyPath,
    build_options: *std.Build.Module,
    zig_default_test_runner: *std.Build.Module,

    fn configure(self: UnitTestRunner, compile: *Step.Compile) void {
        compile.root_module.addImport("build_options", self.build_options);
        compile.root_module.addImport("zig_default_test_runner", self.zig_default_test_runner);
        compile.test_runner = .{
            .path = self.path,
            .mode = runnerMode(compile),
        };
    }

    /// Keep in sync with `std.Build.addRunArtifact` and Zig's stock test
    /// runner. These backends use the simple exit-code protocol; all others
    /// use `std.zig.Server`.
    fn runnerMode(compile: *Step.Compile) @FieldType(Step.Compile.TestRunner, "mode") {
        if (compile.use_llvm == false) {
            const arch = compile.rootModuleTarget().cpu.arch;
            return if (arch == .aarch64 or
                arch == .aarch64_be or
                arch == .powerpc or
                arch == .powerpcle or
                arch == .powerpc64 or
                arch == .powerpc64le or
                arch == .riscv64) .simple else .server;
        }
        return .server;
    }
};

/// Owns every cross-cutting edge a Zig test suite needs, so that a suite is
/// wired up in exactly one call.
///
/// The failure this type exists to prevent: on Windows `TestsSummaryStep`
/// chains its registered runs (see `setRunSerialization`) so that test binaries
/// start one at a time. If a granular `run-test-zig-<suite>` step is pointed at
/// the *same* `Step.Run` that the chain owns, it inherits the entire prefix of
/// that chain. `run-test-zig-minici` is a std-only suite of ~11 string-parsing
/// tests; wired that way it re-ran 41 unrelated test binaries and took 441s
/// instead of ~2s. MiniCI runs each granular step as its own `zig build`
/// process, so it replayed that prefix once per affected job -- which is what
/// pushed the Windows CI job past its two-hour limit.
///
/// `register` builds *both* runs from one spec, so the summary's run and the
/// public step's run can neither be the same object nor drift apart in their
/// configuration. `assertGranularTestStepsAreIsolated` re-checks the finished
/// graph, in case a suite is ever wired by hand anyway.
const TestSuiteRegistry = struct {
    b: *std.Build,
    summary: *TestsSummaryStep,
    build_test_zig_step: *Step,
    unit_test_runner: UnitTestRunner,
    /// Non-null only while the test-wiring check is enumerating test binaries.
    wiring_run: ?*Step.Run,
    /// Args forwarded from `zig build -- <args>`, e.g. `--test-filter`.
    run_args: []const []const u8,

    fn register(self: TestSuiteRegistry, spec: TestSuiteSpec) void {
        const b = self.b;

        self.unit_test_runner.configure(spec.compile);

        // Build-side wiring, uniform for every suite.
        self.build_test_zig_step.dependOn(&spec.compile.step);
        if (spec.default_step) b.default_step.dependOn(&spec.compile.step);
        if (self.wiring_run) |wiring| wiring.addArtifactArg(spec.compile);

        // Two runs over one compile. Only the summary's run joins the
        // serialization chain (currently enabled only on Windows); the public
        // step gets a run with no chain edges.
        self.summary.addRun(&self.configuredRun(spec).step);

        const public_step = b.step(
            b.fmt("run-test-zig-{s}", .{spec.step_suffix}),
            spec.description,
        );
        public_step.dependOn(&self.configuredRun(spec).step);
    }

    /// Both runs are produced by this one function from one spec, so there is
    /// never a second copy of the configuration to fall out of sync.
    fn configuredRun(self: TestSuiteRegistry, spec: TestSuiteSpec) *Step.Run {
        const run = self.b.addRunArtifact(spec.compile);
        if (self.run_args.len != 0) run.addArgs(self.run_args);
        run.addPassthruArgs();
        for (spec.env) |entry| run.setEnvironmentVariable(entry.key, entry.value);
        for (spec.deps) |dep| run.step.dependOn(dep);
        if (spec.fixture_root) run.setCwd(test_fixtures.mutableRoot(spec.deps));
        return run;
    }
};

/// Build step that checks for forbidden patterns in the type checker code.
///
/// During type checking, we NEVER do string or byte comparisons because:
/// 1. They take linear time, which can cause performance issues
/// 2. They are brittle to changes that type-checking should not be sensitive to
///
/// Instead, we always compare indices - either into node stores or to interned string indices.
/// This step enforces that rule by failing the build if `std.mem.` is found in src/canonicalize/, src/check/, src/layout/, or src/eval/.
const CheckTypeCheckerPatternsStep = struct {
    fn create(b: *std.Build) *Step.Run {
        return buildChecksRun(b, "check-type-checker-patterns");
    }
};

/// Build step that checks for @enumFromInt(0) usage in all .zig files.
///
/// We forbid @enumFromInt(0) because it hides bugs and makes them harder to debug.
/// If we need a placeholder value that we believe will never be read, we should
/// use `undefined` instead - that way our intent is clear, and it can fail in a
/// more obvious way if our assumption is incorrect.
const CheckEnumFromIntZeroStep = struct {
    fn create(b: *std.Build) *Step.Run {
        return buildChecksRun(b, "check-enum-from-int-zero");
    }
};

/// Build step that checks for unused variable suppression patterns.
///
/// In this codebase, we don't use `_ = variable;` to suppress unused variable warnings.
/// Instead, we delete the unused variable/argument and update all call sites as necessary.
const CheckUnusedSuppressionStep = struct {
    fn create(b: *std.Build) *Step.Run {
        return buildChecksRun(b, "check-unused-suppression");
    }
};

/// Build step that checks for deleted post-check architecture APIs being reintroduced.
///
/// This enforces the cor-style lowering contract:
/// - no output/canonicalization layer in post-check lowering
/// - no workspace/source-var remapping layer in monotype
/// - no canonical-source specialization lookup in compilation stages
const CheckPostcheckArchitectureStep = struct {
    fn create(b: *std.Build) *Step.Run {
        return buildChecksRun(b, "check-postcheck-architecture");
    }
};

const CheckWasmBuiltinRoutingStep = struct {
    fn create(b: *std.Build) *Step.Run {
        return buildChecksRun(b, "check-wasm-builtin-routing");
    }
};

/// Build step that fails when tracked snapshots differ from the freshly regenerated
/// output. Detects whether the build root is backed by Git or JJ and invokes the
/// matching diff command directly. This deliberately avoids shelling out through
/// `sh -c`, which is not available on Windows (it fails to spawn with FileNotFound).
const CheckSnapshotDiffStep = struct {
    fn create(b: *std.Build) *Step.Run {
        return buildChecksRun(b, "check-snapshot-diff");
    }
};

/// Build step that checks for @panic and std.debug.panic usage in interpreter and builtins.
///
/// In Roc's design philosophy, compile-time errors become runtime errors with helpful messages.
/// Users can run apps despite errors, and we provide actionable feedback. Using @panic unwinds
/// the stack and prevents showing helpful error messages.
///
/// Additionally, in WASM builds, @panic compiles to the `unreachable` instruction with no
/// message output, making debugging impossible. All runtime code must use roc_ops.crash()
/// to ensure error messages are properly displayed.
const CheckPanicStep = struct {
    fn create(b: *std.Build) *Step.Run {
        return buildChecksRun(b, "check-panic-usage");
    }
};

/// Build step that checks for global stdio usage in CLI code.
///
/// The CLI code uses a context-based I/O pattern where stdout/stderr are accessed
/// through `ctx.io.stdout()` and `ctx.io.stderr()`. This prepares for Zig's upcoming
/// I/O interface changes where I/O is passed through functions (like Allocator).
///
/// This step enforces that pattern by failing the build if direct global stdio
/// access is found in src/cli/main.zig.
const CheckCliGlobalStdioStep = struct {
    fn create(b: *std.Build) *Step.Run {
        return buildChecksRun(b, "check-cli-global-stdio");
    }
};

/// Build step that parses kcov JSON output and prints coverage summary.
/// Used by the `coverage` build step to report parser code coverage statistics.
const CoverageSummaryStep = struct {
    fn create(b: *std.Build, dir: []const u8, exe: []const u8) *Step.Run {
        return createWithOptions(b, dir, exe, "PARSER", 28.0);
    }
    fn createWithOptions(b: *std.Build, dir: []const u8, exe: []const u8, label: []const u8, minimum: f64) *Step.Run {
        const run = buildChecksRun(b, "coverage-summary");
        run.addArgs(&.{ dir, exe, label, b.fmt("{d}", .{minimum}) });
        return run;
    }
};

const CheckTestAssetCoverageStep = struct {
    fn create(b: *std.Build) *Step.Run {
        return buildChecksRun(b, "check-test-asset-coverage-inner");
    }
};

/// Separate processes are essential: ASLR-dependent data can remain stable
/// across multiple bakes within a single process.
const CheckBuiltinBakeReproducibleStep = struct {
    fn create(b: *std.Build, exe: *Step.Compile) *Step.Run {
        const inputs = b.addWriteFiles();
        const sources = inputs.addCopyDirectory(b.path("src/build/roc"), "roc", .{});
        const compare = buildChecksRun(b, "check-builtin-bake-reproducible");
        for (0..3) |index| {
            const bake = b.addRunArtifact(exe);
            bake.addFileArg(sources.path(b, "Builtin.roc"));
            for ([_][]const u8{ "Builtin.bin", "builtin_indices.zig", "Builtin.artifact.bin" }) |name| {
                // Distinct declared output names produce distinct Run keys.
                // Identical keys could reuse one process's output three times,
                // defeating this ASLR reproducibility check.
                compare.addFileArg(bake.addOutputFileArg(b.fmt("bake-{d}-{s}", .{ index, name })));
            }
        }
        return compare;
    }
};

const BuiltinCompilerRun = struct {
    exe: *Step.Compile,
    run: *Step.Run,
    builtin_bin: std.Build.LazyPath,
    builtin_indices_zig: std.Build.LazyPath,
    builtin_artifact_bin: std.Build.LazyPath,
};

fn createAndRunBuiltinCompiler(
    b: *std.Build,
    roc_modules: modules.RocModules,
    flag_enable_tracy: ?[]const u8,
    roc_files: []const []const u8,
) BuiltinCompilerRun {
    // Build and run the compiler
    const builtin_compiler_exe = b.addExecutable(.{
        .name = "builtin_compiler",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/builtin_compiler/main.zig"),
            .target = b.graph.host, // this runs at build time on the *host* machine!
            // Kept Debug deliberately: this exe publishes + serializes the
            // builtin CheckedModuleArtifact (a few seconds of Debug run), but building
            // it ReleaseFast would optimize the whole check/eval closure (incl. the
            // large checked_artifact.zig), adding far more `zig build` wall-clock than
            // the one-time bake run saves. Debug compile + Debug bake is the faster total.
            .optimize = .debug,
            // ctx.CoreCtx reads env vars via std.c.getenv; Zig 0.16 requires
            // link_libc=true on any compile unit that references std.c.*.
            // (add_tracy below also sets this when tracy is enabled, but tracy is
            // always disabled for the build-time builtin compiler.)
            .link_libc = true,
        }),
    });
    configureBackend(builtin_compiler_exe, b.graph.host);

    // Add only the minimal modules needed for parsing/checking
    builtin_compiler_exe.root_module.addImport("base", roc_modules.base);
    builtin_compiler_exe.root_module.addImport("collections", roc_modules.collections);
    builtin_compiler_exe.root_module.addImport("types", roc_modules.types);
    builtin_compiler_exe.root_module.addImport("parse", roc_modules.parse);
    builtin_compiler_exe.root_module.addImport("can", roc_modules.can);
    builtin_compiler_exe.root_module.addImport("check", roc_modules.check);
    builtin_compiler_exe.root_module.addImport("reporting", roc_modules.reporting);
    builtin_compiler_exe.root_module.addImport("builtins", roc_modules.builtins);

    // The builtin compiler publishes the Builtin module to a CheckedModuleArtifact
    // and must run the same compile-time finalizer the runtime uses (Builtin has
    // compile-time roots that must be evaluated into its ConstStore). The finalizer
    // and its interpreter dependencies live in the eval module, but the eval
    // module's root imports `compiled_builtins` (this compiler's own output). The
    // finalizer's own transitive closure does NOT touch `compiled_builtins`, so it
    // is added here as a standalone module rooted at compile_time_finalization.zig.
    const comptime_finalizer_module = b.createModule(.{
        .root_source_file = b.path("src/eval/compile_time_finalization.zig"),
        .target = b.graph.host,
        .optimize = .debug,
        .link_libc = true,
        .imports = &.{
            .{ .name = "base", .module = roc_modules.base },
            .{ .name = "build_options", .module = roc_modules.build_options },
            .{ .name = "backend", .module = roc_modules.backend },
            .{ .name = "builtins", .module = roc_modules.builtins },
            .{ .name = "can", .module = roc_modules.can },
            .{ .name = "check", .module = roc_modules.check },
            .{ .name = "collections", .module = roc_modules.collections },
            .{ .name = "layout", .module = roc_modules.layout },
            .{ .name = "lir", .module = roc_modules.lir },
            .{ .name = "roc_target", .module = roc_modules.roc_target },
            .{ .name = "static_data", .module = roc_modules.static_data },
            .{ .name = "sljmp", .module = roc_modules.sljmp },
        },
    });
    // The interpreter's hosted-call trampoline is hand-written assembly; attach
    // it so the finalizer's interpreter links (arch-guarded, empty on wasm).
    comptime_finalizer_module.addAssemblyFile(b.path("src/eval/host_trampoline.S"));
    builtin_compiler_exe.root_module.addImport("comptime_finalizer", comptime_finalizer_module);

    // Add tracy support (required by parse/can/check modules)
    add_tracy(b, roc_modules.build_options, builtin_compiler_exe, b.graph.host, false, flag_enable_tracy);

    // Run the builtin compiler to generate .bin files in zig-out/builtins/
    const run_builtin_compiler = b.addRunArtifact(builtin_compiler_exe);

    // Add all .roc files as explicit file inputs so Zig's cache tracks them
    for (roc_files) |roc_path| {
        run_builtin_compiler.addFileArg(b.path(roc_path));
    }

    const builtin_bin = run_builtin_compiler.addOutputFileArg("Builtin.bin");
    const builtin_indices_zig = run_builtin_compiler.addOutputFileArg("builtin_indices.zig");
    const builtin_artifact_bin = run_builtin_compiler.addOutputFileArg("Builtin.artifact.bin");

    return .{
        .exe = builtin_compiler_exe,
        .run = run_builtin_compiler,
        .builtin_bin = builtin_bin,
        .builtin_indices_zig = builtin_indices_zig,
        .builtin_artifact_bin = builtin_artifact_bin,
    };
}

fn createTestPlatformHostLib(
    b: *std.Build,
    name: []const u8,
    host_path: []const u8,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    roc_modules: modules.RocModules,
    strip: bool,
    omit_frame_pointer: ?bool,
    options: TestHostOptions,
) *Step.Compile {
    const host_target = withRocMacosDeploymentTarget(b, target);
    const lib = b.addLibrary(.{
        .name = name,
        .linkage = .static,
        .root_module = b.createModule(.{
            .root_source_file = b.path(host_path),
            .target = host_target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            // These archives are linked into Roc-produced ELF outputs without
            // a Zig runtime; keep safe-mode stack probes out of that ABI.
            .stack_check = if (isGlibcTestHost(host_target)) false else null,
            .pic = true, // Enable Position Independent Code for PIE compatibility
            // Only linked so host code can set up stack overflow handling.
            .link_libc = testHostNeedsLibc(options, host_target),
        }),
    });
    configureBackend(lib, host_target);
    if (testHostNeedsLlvm(host_target)) {
        // The symbol-ABI platform tests depend on the visibility declared in
        // @export options: default-visibility host functions are public shared
        // library exports, while hidden runtime and hosted symbols are internal
        // link inputs. Zig's LLVM backend emits that ELF visibility metadata.
        lib.use_llvm = true;
    }
    for (options.extra_sources) |source| {
        const obj = b.addObject(.{
            .name = b.fmt("{s}_{s}", .{ name, std.fs.path.stem(source) }),
            .root_module = b.createModule(.{
                .root_source_file = b.path(source),
                .target = host_target,
                .optimize = optimize,
                .strip = strip,
                .omit_frame_pointer = omit_frame_pointer,
                .stack_check = if (isGlibcTestHost(host_target)) false else null,
                .pic = true,
            }),
        });
        configureBackend(obj, host_target);
        if (testHostNeedsLlvm(host_target)) obj.use_llvm = true;
        obj.link_function_sections = true;
        obj.link_data_sections = true;
        lib.root_module.addObject(obj);
    }
    if (options.uses_stack_handler) {
        lib.root_module.addImport("base", roc_modules.base);
        const crash_handlers = b.createModule(.{
            .root_source_file = b.path("test/host_crash_handlers.zig"),
            .target = host_target,
            .optimize = optimize,
        });
        crash_handlers.addImport("base", roc_modules.base);
        lib.root_module.addImport("host_crash_handlers", crash_handlers);
    }
    lib.root_module.addImport("builtins", roc_modules.builtins);
    lib.root_module.addImport("build_options", roc_modules.build_options);
    lib.root_module.addImport("host_alloc", roc_modules.host_alloc);
    lib.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    lib.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    lib.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    // Bundle compiler_rt when generated host object code may call compiler_rt
    // routines that are not supplied by the OS libraries. Linux and x86_64
    // macOS LLVM builds can emit symbols like __zig_probe_stack; ARM64 Windows
    // Zig code can emit stack-protector calls to __stack_chk_fail.
    lib.bundle_compiler_rt = testHostNeedsCompilerRt(host_target);
    // Per-function/data sections so symbol-ABI links can strip unused host code.
    lib.link_function_sections = true;
    lib.link_data_sections = true;

    return lib;
}

/// Builds a test platform host library and sets up a step to copy it to the target-specific directory.
/// Returns the final step for dependency wiring.
fn buildAndCopyTestPlatformHostLib(
    b: *std.Build,
    platform_dir: []const u8,
    target: ResolvedTarget,
    target_name: []const u8,
    optimize: OptimizeMode,
    roc_modules: modules.RocModules,
    strip: bool,
    omit_frame_pointer: ?bool,
) *Step {
    const options = TestHostOptions{
        .uses_stack_handler = testPlatformUsesStackHandler(platform_dir),
        .extra_sources = testPlatformExtraHostSources(platform_dir),
    };
    // The dylib/archive tests assert that unused hosted symbols and their
    // canary data are removed by the final link. Zig Debug emits host objects
    // with one monolithic .text and default-visible exports, so those tests
    // need optimized host objects with hidden symbols and per-section output.
    const host_optimize: OptimizeMode = if (testPlatformRequiresSectionDceHost(platform_dir)) .small else optimize;

    const lib = createTestPlatformHostLib(
        b,
        b.fmt("test_platform_{s}_host_{s}", .{ platform_dir, target_name }),
        b.pathJoin(&.{ "test", platform_dir, "platform/host.zig" }),
        target,
        host_optimize,
        roc_modules,
        strip,
        omit_frame_pointer,
        options,
    );
    const baseline_target_name = muslBaselineTestTargetName(target_name);
    const baseline_lib = if (baseline_target_name) |name| createTestPlatformHostLib(
        b,
        b.fmt("test_platform_{s}_host_{s}", .{ platform_dir, name }),
        b.pathJoin(&.{ "test", platform_dir, "platform/host.zig" }),
        b.resolveTargetQuery(roc_target.RocTarget.fromString(name).?.llvmTargetQuery()),
        host_optimize,
        roc_modules,
        strip,
        omit_frame_pointer,
        options,
    ) else null;

    // The dylib platform produces a Windows DLL, and a DLL only exposes symbols
    // that carry dllexport storage. Unlike ELF/Mach-O shared objects (which
    // export all global symbols by default), a static host .lib linked into a
    // DLL exports nothing unless its `export fn` API (e.g. roc_run_app) is
    // marked dllexport. `-fdll-export-fns` emits the needed `.drectve /EXPORT:`
    // directives that lld-link honors, without exporting bundled compiler_rt.
    if (target.result.os.tag == .windows and std.mem.eql(u8, platform_dir, "dylib")) {
        lib.dll_export_fns = true;
    }

    // Use correct filename for target platform
    const host_filename = if (target.result.os.tag == .windows) "host.lib" else "libhost.a";
    const archive_path = b.pathJoin(&.{ "test", platform_dir, "platform/targets", target_name, host_filename });
    const baseline_archive_path = if (baseline_target_name) |name|
        b.pathJoin(&.{ "test", platform_dir, "platform/targets", name, host_filename })
    else
        null;

    const copy_step = b.addWriteFiles();
    const host_archive = if (target.result.os.tag == .windows)
        lib.getEmittedBin()
    else
        FixArchivePaddingStep.create(b, lib.getEmittedBin());
    test_fixtures.copy(copy_step, host_archive, archive_path);
    if (baseline_archive_path) |path| {
        const baseline_archive = if (target.result.os.tag == .windows)
            baseline_lib.?.getEmittedBin()
        else
            FixArchivePaddingStep.create(b, baseline_lib.?.getEmittedBin());
        test_fixtures.copy(copy_step, baseline_archive, path);

        inline for (.{ "crt1.o", "libc.a" }) |runtime_filename| {
            test_fixtures.copy(
                copy_step,
                b.path(b.pathJoin(&.{ "test/fx/platform/targets", target_name, runtime_filename })),
                b.pathJoin(&.{ "test", platform_dir, "platform/targets", baseline_target_name.?, runtime_filename }),
            );
        }
    }

    return &copy_step.step;
}

// Custom step to remove a directory tree (replaces removed addRemoveDirTree)
const RemoveDirTreeStep = struct {
    fn create(b: *std.Build, path: []const u8) *Step.Run {
        const run = buildChecksRun(b, "remove-dir-tree");
        run.addArg(path);
        return run;
    }
};

/// Build the wasm test platform host as a relocatable .wasm object (not an archive).
/// Surgical linking operates on a single relocatable object with linking/reloc sections.
fn buildAndCopyWasmHostObject(
    b: *std.Build,
    target: ResolvedTarget,
    target_name: []const u8,
    optimize: OptimizeMode,
    roc_modules: modules.RocModules,
    strip: bool,
    omit_frame_pointer: ?bool,
) *Step {
    const obj = b.addObject(.{
        .name = "host",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/wasm/platform/host.zig"),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true,
        }),
    });
    configureBackend(obj, target);
    obj.root_module.addImport("builtins", roc_modules.builtins);
    obj.root_module.addImport("build_options", roc_modules.build_options);
    obj.root_module.addImport("host_alloc", roc_modules.host_alloc);
    // Per-function/data sections so wasm final-link DCE can strip unused host code.
    obj.link_function_sections = true;
    obj.link_data_sections = true;
    // Match production Rust hosts, which contribute weak compiler-rt symbols.
    // Wasm LLD must resolve these against Roc's strong builtins definitions.
    obj.bundle_compiler_rt = true;

    const dest_path = b.fmt("test/wasm/platform/targets/{s}/host.wasm", .{target_name});
    const copy_step = b.addWriteFiles();
    test_fixtures.copy(copy_step, obj.getEmittedBin(), dest_path);

    return &copy_step.step;
}

/// Build a wasm host which owns a compiler-rt-spelled symbol with strong
/// binding, as stable Rust hosts must when they implement an intrinsic by
/// hand. Roc's sealed object must not expose or resolve against this symbol.
fn buildAndCopyStrongIntrinsicWasmHostObject(
    b: *std.Build,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    roc_modules: modules.RocModules,
    strip: bool,
    omit_frame_pointer: ?bool,
) *Step {
    const obj = b.addObject(.{
        .name = "strong_intrinsic_host",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/wasm/issue_10827_strong_intrinsic_host/platform/wasm_host.zig"),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true,
        }),
    });
    configureBackend(obj, target);
    obj.root_module.addImport("host_alloc", roc_modules.host_alloc);
    obj.link_function_sections = true;
    obj.link_data_sections = true;
    obj.bundle_compiler_rt = false;

    const copy_step = b.addWriteFiles();
    test_fixtures.copy(
        copy_step,
        obj.getEmittedBin(),
        "test/wasm/issue_10827_strong_intrinsic_host/platform/targets/wasm32/host.wasm",
    );
    return &copy_step.step;
}

/// Build the minimal wasm host shared by the `exports:` fixtures, whose
/// platform headers differ only in what they declare in that field.
fn buildAndCopyExportsFixtureWasmHostObject(
    b: *std.Build,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    roc_modules: modules.RocModules,
    strip: bool,
    omit_frame_pointer: ?bool,
) *Step {
    const obj = b.addObject(.{
        .name = "exports_fixture_host",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/wasm/issue_10951_wasm_exports/wasm_host.zig"),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true,
        }),
    });
    configureBackend(obj, target);
    obj.root_module.addImport("host_alloc", roc_modules.host_alloc);
    obj.link_function_sections = true;
    obj.link_data_sections = true;
    obj.bundle_compiler_rt = false;

    const copy_step = b.addWriteFiles();
    test_fixtures.copy(
        copy_step,
        obj.getEmittedBin(),
        "test/wasm/issue_10951_wasm_exports/missing_exports/platform/targets/wasm32/host.wasm",
    );
    test_fixtures.copy(
        copy_step,
        obj.getEmittedBin(),
        "test/wasm/issue_10951_wasm_exports/unknown_export/platform/targets/wasm32/host.wasm",
    );
    test_fixtures.copy(
        copy_step,
        obj.getEmittedBin(),
        "test/wasm/issue_10951_wasm_exports/empty_exports/platform/targets/wasm32/host.wasm",
    );
    return &copy_step.step;
}

// Workaround for Zig bug https://codeberg.org/ziglang/zig/issues/30572
const FixArchivePaddingStep = struct {
    fn create(b: *std.Build, input: std.Build.LazyPath) std.Build.LazyPath {
        const run = buildChecksRun(b, "fix-archive-padding");
        run.addFileArg(input);
        const output = run.addOutputFileArg("libhost.a");
        run.expectExitCode(0);
        return output;
    }
};

const PrintBuildSuccessStep = struct {
    fn create(b: *std.Build) *Step.Run {
        return buildChecksRun(b, "print-build-success");
    }
};

const WasmTestHosts = struct {
    wasm32: *Step,
    wasm32v1: *Step,
};

fn setupTestPlatforms(
    b: *std.Build,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    roc_modules: modules.RocModules,
    build_test_hosts_step: *Step,
    strip: bool,
    omit_frame_pointer: ?bool,
    platform_filter: ?[]const u8,
) WasmTestHosts {
    // Compile and copy host fixtures through ordinary build dependencies.
    const prepared_hosts_step = b.step("test-hosts-prepared", "Prepare test platform host libraries");
    const native_target_name = roc_target.RocTarget.fromStdTarget(target.result).toName();

    // Matrix targets already have a baseline host below. Avoid competing
    // native and cross producers for the same fixture path.
    const native_in_matrix = for (linux_cross_targets ++ windows_cross_targets) |cross_target| {
        if (std.mem.eql(u8, native_target_name, cross_target.name)) break true;
    } else false;

    // Build native hosts only when the cross-target matrix does not cover them.
    for (if (native_in_matrix) &.{} else &all_test_platform_dirs) |platform_dir| {
        if (platform_filter) |filter| {
            if (!std.mem.eql(u8, platform_dir, filter)) continue;
        }
        const copy_step = buildAndCopyTestPlatformHostLib(
            b,
            platform_dir,
            target,
            native_target_name,
            optimize,
            roc_modules,
            strip,
            omit_frame_pointer,
        );
        prepared_hosts_step.dependOn(copy_step);
    }

    // Cross-compile for all Linux targets. `roc build` selects the host
    // platform's native target by default, which is commonly x64glibc even
    // when this Zig build compiles the `roc` binary as native-musl.
    for (linux_cross_targets) |cross_target| {
        const cross_resolved_target = b.resolveTargetQuery(cross_target.query);

        for (all_test_platform_dirs) |platform_dir| {
            if (platform_filter) |filter| {
                if (!std.mem.eql(u8, platform_dir, filter)) continue;
            }
            const copy_step = buildAndCopyTestPlatformHostLib(
                b,
                platform_dir,
                cross_resolved_target,
                cross_target.name,
                optimize,
                roc_modules,
                strip,
                omit_frame_pointer,
            );
            prepared_hosts_step.dependOn(copy_step);

            // Allocation-sensitive test platforms declare fx's musl runtime
            // objects as link inputs; copy them alongside their host library
            // rather than committing more copies of the same binaries.
            if ((std.mem.eql(u8, platform_dir, "alloc-count") or std.mem.eql(u8, platform_dir, "box-model-uniqueness")) and std.mem.endsWith(u8, cross_target.name, "musl")) {
                const copy_musl_runtime = b.addWriteFiles();
                test_fixtures.copy(
                    copy_musl_runtime,
                    b.path(b.pathJoin(&.{ "test/fx/platform/targets", cross_target.name, "crt1.o" })),
                    b.pathJoin(&.{ "test", platform_dir, "platform/targets", cross_target.name, "crt1.o" }),
                );
                test_fixtures.copy(
                    copy_musl_runtime,
                    b.path(b.pathJoin(&.{ "test/fx/platform/targets", cross_target.name, "libc.a" })),
                    b.pathJoin(&.{ "test", platform_dir, "platform/targets", cross_target.name, "libc.a" }),
                );
                prepared_hosts_step.dependOn(&copy_musl_runtime.step);
            }
        }
    }

    // Cross-compile for Windows targets
    for (windows_cross_targets) |cross_target| {
        const cross_resolved_target = b.resolveTargetQuery(cross_target.query);

        for (all_test_platform_dirs) |platform_dir| {
            if (platform_filter) |filter| {
                if (!std.mem.eql(u8, platform_dir, filter)) continue;
            }
            const copy_step = buildAndCopyTestPlatformHostLib(
                b,
                platform_dir,
                cross_resolved_target,
                cross_target.name,
                optimize,
                roc_modules,
                strip,
                omit_frame_pointer,
            );
            prepared_hosts_step.dependOn(copy_step);

            // MinGW targets link only what the platform declares, so each one
            // needs fx's checked-in C runtime alongside its host library.
            // test/fx is the source of those files, so it needs no copy.
            if (std.mem.endsWith(u8, cross_target.name, "mingw") and !std.mem.eql(u8, platform_dir, "fx")) {
                prepared_hosts_step.dependOn(copyMingwRuntimeToTestPlatform(b, platform_dir, cross_target.name));
            }
        }
    }

    // Build the wasm test platform host as a relocatable .wasm object for surgical linking
    const wasm_target = b.resolveTargetQuery(.{ .cpu_arch = .wasm32, .os_tag = .freestanding, .abi = .none });
    const wasm_host_step = buildAndCopyWasmHostObject(
        b,
        wasm_target,
        "wasm32",
        optimize,
        roc_modules,
        strip,
        omit_frame_pointer,
    );
    const wasm_v1_host_step = buildAndCopyWasmHostObject(
        b,
        b.resolveTargetQuery(roc_target.RocTarget.wasm32v1.llvmTargetQuery()),
        "wasm32v1",
        optimize,
        roc_modules,
        strip,
        omit_frame_pointer,
    );
    prepared_hosts_step.dependOn(wasm_host_step);
    prepared_hosts_step.dependOn(wasm_v1_host_step);
    prepared_hosts_step.dependOn(buildAndCopyStrongIntrinsicWasmHostObject(
        b,
        wasm_target,
        optimize,
        roc_modules,
        strip,
        omit_frame_pointer,
    ));
    prepared_hosts_step.dependOn(buildAndCopyExportsFixtureWasmHostObject(
        b,
        wasm_target,
        optimize,
        roc_modules,
        strip,
        omit_frame_pointer,
    ));

    for (glibc_cross_targets) |cross_target| {
        if (generateGlibcStub(b, b.resolveTargetQuery(cross_target.query), cross_target.name)) |stubs| {
            prepared_hosts_step.dependOn(&stubs.step);
        }
    }

    b.getInstallStep().dependOn(prepared_hosts_step);
    build_test_hosts_step.dependOn(prepared_hosts_step);

    return .{ .wasm32 = wasm_host_step, .wasm32v1 = wasm_v1_host_step };
}

const WasmStaticLibAppBuild = struct {
    run: *Step.Run,
    wasm: std.Build.LazyPath,
};

/// `roc build --target=wasm32` of one app staged in `sources`, which holds the
/// app, its platform's Roc modules, and the wasm host. Every file the build
/// reads lives under that staged directory, whose path is derived from all of
/// their contents, so the step is cached exactly when its inputs are unchanged.
///
/// The run follows `build_roc_step`, which installs `roc`: a run hashes an
/// artifact by its installed path once installed and by its cache path before,
/// so ordering it after the install keeps its cache key from depending on
/// which of the two the scheduler happened to reach first.
fn addWasmStaticLibAppBuild(
    b: *std.Build,
    roc_exe: *Step.Compile,
    build_roc_step: *Step,
    build_test_hosts_step: *Step,
    sources: *Step.WriteFile,
    app: []const u8,
    options: []const []const u8,
    output_basename: []const u8,
) WasmStaticLibAppBuild {
    return addWasmStaticLibAppBuildForTarget(b, roc_exe, build_roc_step, build_test_hosts_step, sources, app, options, output_basename, "wasm32");
}

fn addWasmStaticLibAppBuildForTarget(
    b: *std.Build,
    roc_exe: *Step.Compile,
    build_roc_step: *Step,
    build_test_hosts_step: *Step,
    sources: *Step.WriteFile,
    app: []const u8,
    options: []const []const u8,
    output_basename: []const u8,
    target_name: []const u8,
) WasmStaticLibAppBuild {
    const run = b.addRunArtifact(roc_exe);
    run.step.dependOn(build_roc_step);
    // Fixture compilation needs the complete prepared host libraries.
    run.step.dependOn(build_test_hosts_step);
    run.addArg("build");
    run.addFileArg(sources.getDirectory().path(b, app));
    run.addArgs(options);
    run.addArg(b.fmt("--target={s}", .{target_name}));
    const wasm = run.addPrefixedOutputFileArg("--output=", output_basename);
    return .{ .run = run, .wasm = wasm };
}

pub fn build(b: *std.Build) void {
    test_fixtures = .{ .b = b };

    // Build/run split used by MiniCI:
    // - `build-*` steps own compile, install, generation, and prep work.
    // - `run-*` steps own execution of checks/tests/tools after prep is done.
    // - If a `run-*` step needs a binary or generated input, wire that work into
    //   a `build-*` step and add it to `build-ci`.
    // - The user-facing step for building the Roc CLI is `roc`.
    // - Do not add duplicate alias steps. References should use the exact
    //   `roc`, `build-*`, `run-*`, `run-check-*`, or `run-test-*` step name.
    // - MiniCI runs `build-ci` once and then runs leaf `run-*` jobs. Keep
    //   aggregate steps out of MiniCI so each job remains independently
    //   reportable and re-runnable.
    // MiniCI intentionally does not parse Zig summaries to detect misplaced
    // build work; this convention is the source of truth.
    const build_ci_step = b.step("build-ci", "Build all binaries used by MiniCI");
    const build_roc_step = b.step("roc", "Build the roc compiler without running it");
    const run_roc_step = b.step("run-roc", "Build and run the roc cli");
    const build_check_tools_step = b.step("build-check-tools", "Build host check tools used by CI");
    const run_check_zig_format_step = b.step("run-check-zig-format", "Check formatting of all zig code");
    const run_check_zig_lints_step = b.step("run-check-zig-lints", "Run Zig lints");
    const run_check_source_bidi_step = b.step("run-check-source-bidi", "Reject bidirectional controls in tracked source and paths");
    const run_check_tidy_step = b.step("run-check-tidy", "Run code tidiness checks");
    const run_check_git_lints_step = b.step("run-check-git-lints", "Run Git-backed code checks");
    const run_check_test_asset_coverage_step = b.step("run-check-test-asset-coverage", "Check that every app .roc file in spec-driven test asset dirs has a spec entry");
    const run_check_type_checker_patterns_step = b.step("run-check-type-checker-patterns", "Check forbidden type-checker patterns");
    const run_check_enum_from_int_zero_step = b.step("run-check-enum-from-int-zero", "Check forbidden @enumFromInt(0) usage");
    const run_check_unused_suppression_step = b.step("run-check-unused-suppression", "Check unused-variable suppression patterns");
    const run_check_semantic_audit_step = b.step("run-check-semantic-audit", "Run the checked-data audit gate");
    const run_check_postcheck_architecture_step = b.step("run-check-postcheck-architecture", "Check that deleted post-check output/remapping APIs stay gone");
    const run_check_wasm_builtin_routing_step = b.step("run-check-wasm-builtin-routing", "Check that WASM builtin calls use explicit host/relocation routing");
    const run_check_panic_step = b.step("run-check-panic", "Check forbidden panic usage in interpreter and builtins");
    const run_check_cli_global_stdio_step = b.step("run-check-cli-global-stdio", "Check forbidden global stdio usage in CLI code");
    const run_check_test_wiring_step = b.step("run-check-test-wiring", "Check test files are wired");
    const run_check_builtin_format_step = b.step("run-check-builtin-format", "Check Builtin.roc formatting");
    const run_check_glue_abi_step = b.step("run-check-glue-abi", "Check generated Zig glue against the canonical host ABI");
    const run_check_simd_codegen_step = b.step("run-check-simd-codegen", "Check that optimized integer SIMD kernels select native instructions");
    const run_check_match_extension_codegen_step = b.step("run-check-match-extension-codegen", "Check the pinned instruction counts for the match-extension loop");
    const run_check_baseline_codegen_step = b.step("run-check-baseline-codegen", "Check that v1 targets emit no instruction above the architecture baseline");
    const run_check_str_eq_same_allocation_step = b.step("run-check-str-eq-same-allocation", "Check that comparing a string against itself does not read its bytes");
    const build_snapshot_tool_step = b.step("build-snapshot-tool", "Build the snapshot tool");
    const run_check_snapshots_step = b.step("run-check-snapshots", "Regenerate snapshots and fail if tracked snapshots changed");
    const build_test_zig_step = b.step("build-test-zig", "Build Zig unit-test binaries");
    const run_test_zig_step = b.step("run-test-zig", "Run Zig unit tests");
    const build_test_lsp_integration_runner_step = b.step("build-test-lsp-integration-runner", "Build LSP integration test harness");
    const build_test_eval_runner_step = b.step("build-test-eval-runner", "Build eval test runner");
    const run_test_eval_step = b.step("run-test-eval", "Run eval tests in parallel across enabled backends");
    const run_test_simd_differential_step = b.step("run-test-simd-differential", "Run the exhaustive integer-SIMD oracle corpus through every compiler consumer");
    const build_test_eval_host_effects_runner_step = b.step("build-test-eval-host-effects-runner", "Build runtime host-effects eval test runner");
    const run_test_eval_host_effects_step = b.step("run-test-eval-host-effects", "Run runtime host-effects eval tests across supported backends");
    const build_test_lambda_mono_differential_step = b.step("build-test-lambda-mono-differential", "Build the Lambda Mono differential harness");
    const run_test_lambda_mono_differential_step = b.step("run-test-lambda-mono-differential", "Run the Lambda Mono body-lowering differential harness (Debug only)");
    const build_playground_step = b.step("build-playground", "Build the WASM playground");
    const build_playground_wasm_archive_step = b.step("build-playground-wasm-archive", "Build playground.wasm and zstd-compress it under zig-out/lib/playground");
    const build_repl_wasm_step = b.step("build-repl-wasm", "Build the dedicated REPL WebAssembly module");
    const build_repl_wasm_archive_step = b.step("build-repl-wasm-archive", "Build repl.wasm and zstd-compress it under zig-out/lib/repl");
    const run_test_repl_wasm_step = b.step("run-test-repl-wasm", "Run the dedicated REPL WebAssembly protocol tests");
    const build_web_step = b.step("build-web", "Build the playground, REPL, and echo web artifacts");
    const build_test_playground_runner_step = b.step("build-test-playground-runner", "Build the integration test suite for the WASM playground");
    const run_test_playground_step = b.step("run-test-playground", "Run the integration test suite for the WASM playground");
    const build_test_cli_runners_step = b.step("build-test-cli-runners", "Build CLI integration test runners");
    const run_test_cli_step = b.step("run-test-cli", "Run all CLI integration tests (platforms + subcommands + echo + glue)");
    const build_test_serialization_sizes_step = b.step("build-test-serialization-sizes", "Build serialization size checks");
    const run_test_serialization_sizes_step = b.step("run-test-serialization-sizes", "Verify Serialized types have platform-independent sizes");
    const build_test_builtin_bake_reproducible_step = b.step("build-test-builtin-bake-reproducible", "Build the builtin compiler the bake reproducibility check runs");
    const run_test_builtin_bake_reproducible_step = b.step("run-test-builtin-bake-reproducible", "Bake the builtins in three separate processes and compare every output byte");
    const build_test_wasm_static_lib_runner_step = b.step("build-test-wasm-static-lib-runner", "Build WASM static library test runner");
    const run_test_wasm_static_lib_step = b.step("run-test-wasm-static-lib", "Run WASM static library test runner");
    const repro_issue_11529_step = b.step("repro-issue-11529", "Build and run the wasm32 top-level boxed function regression");
    const run_test_dylib_step = b.step("run-test-dylib", "Build a Roc shared library and run it through the loader test");
    const run_test_archive_step = b.step("run-test-archive", "Build a Roc static archive, link a consumer against it, and run it");
    const run_check_machine_code_shim_archive_step = b.step("run-check-machine-code-shim-archive", "Check that the machine-code shim keeps compiler-private support local");
    const build_coverage_tools_step = b.step("build-coverage-tools", "Build parser coverage tools");
    const run_coverage_parser_step = b.step("run-coverage-parser", "Run parser tests with kcov code coverage");
    const run_minici_step = b.step("minici", "Run a subset of CI build and test steps");
    const run_fmt_zig_step = b.step("run-fmt-zig", "Format all zig code");
    const run_snapshot_tool_step = b.step("run-snapshot-tool", "Run the snapshot tool to update snapshot files");
    const echo_wasm_step = b.step("build-echo-wasm", "Build the echo platform to zig-out/lib/echo/echo.wasm");
    const echo_wasm_archive_step = b.step("build-echo-wasm-archive", "Build echo.wasm and zstd-compress it under zig-out/lib/echo");
    const build_glue_release_step = b.step("build-glue-release", "Build release-ready glue specs");

    const build_test_hosts_step = b.step("build-test-hosts", "Build test platform host libraries");
    const build_release_step = b.step("build-release", "Build optimized release binary for distribution");

    // general configuration
    const optimize = b.standardOptimizeOption(.{});
    const target = blk: {
        var default_target_query: std.Target.Query = .{
            .abi = if (builtin.target.os.tag == .linux) .musl else null,
        };

        // Pin baseline x86-64 instead of inheriting the build machine's CPU.
        // This matches the release target (getReleaseTargetQuery), so a build
        // from source runs everywhere a released binary does, and it keeps
        // Valgrind working: Valgrind 3.22 can't emulate the AVX-512 EVEX
        // instructions a native build emits into musl startup code.
        if (builtin.target.cpu.arch == .x86_64) {
            default_target_query.cpu_model = .baseline;
        }

        break :blk withSha256Floor(b, b.standardTargetOptions(.{ .default_target = default_target_query }), optimize);
    };
    const strip_flag = b.option(bool, "strip", "Omit debug information");
    const no_bin = b.option(bool, "no-bin", "Skip emitting binaries (important for fast incremental compilation)") orelse false;
    const trace_eval = b.option(bool, "trace-eval", "Enable detailed evaluation tracing for debugging") orelse false;
    const trace_refcount = b.option(bool, "trace-refcount", "Enable detailed refcount tracing for debugging memory issues") orelse false;
    const trace_modules = b.option(bool, "trace-modules", "Enable module compilation and import resolution tracing") orelse false;
    const platform_filter = b.option([]const u8, "platform", "Filter which test platform to build (e.g., fx, str, int, fx-open)");
    const cli_test_llvm = b.option(bool, "cli-test-llvm", "Include LLVM size/speed backend jobs in CLI platform tests") orelse false;
    const trace_build = b.option(bool, "trace-build", "Enable detailed build pipeline tracing") orelse false;
    const debug_gpa = b.option(bool, "debug-gpa", "Use the leak-checking DebugAllocator for the roc binary even when libc is linked (default: off, so libc's malloc and its ASan/Valgrind/LD_PRELOAD tooling are used)") orelse false;
    const linker_warnings = b.option(bool, "linker-warnings", "Surface all embedded LLD linker warnings (default: off; suppressed with -w)") orelse false;
    const debug_gpa_traces = b.option(bool, "debug-gpa-traces", "Capture DebugAllocator allocation sites and enable Zig unit-test stack traces (default: off, because capturing traces dominates Debug-build runtime; leaks are detected either way)") orelse false;
    const shared_memory_size = b.option(u64, "shared-memory-size", "Explicitly set shared-memory arena sizes in bytes");
    const print_trmc = b.option(bool, "print-trmc", "Print one line for each transformed TRMC/TCE proc") orelse false;
    const print_ir_after_trmc = b.option(bool, "print-ir-after-trmc", "Print full LIR for each proc transformed by TRMC/TCE") orelse false;
    const llvm_keep_ir = b.option([]const u8, "llvm-keep-ir", "Write statement LLVM IR to this build-time path") orelse "";
    const llvm_keep_bitcode = b.option([]const u8, "llvm-keep-bitcode", "Write merged LLVM bitcode to this build-time path") orelse "";
    const llvm_keep_object = b.option([]const u8, "llvm-keep-object", "Write the temporary LLVM object file to this build-time path") orelse "";
    const test_progress_interval_ms = b.option(u64, "test-progress-interval-ms", "Print non-TTY parallel test progress every N milliseconds; 0 disables it") orelse 0;
    const eval_no_fork = b.option(bool, "eval-no-fork", "Run eval tests in-process instead of through fork isolation") orelse false;
    const eval_time_worker = b.option(bool, "eval-time-worker", "Print eval worker startup timing instrumentation") orelse false;
    const glue_release_tag = b.option([]const u8, "glue-release-tag", "Nightly release tag used in generated glue release metadata");
    const compiler_version_override = b.option([]const u8, "compiler-version", "Report this string as the compiler version instead of <build mode>-<git short sha>; nightly builds pass their release tag");
    if (compiler_version_override) |override| {
        // The version string is also a directory name (install root, cache root), so it is
        // restricted to characters that are a valid path component on every supported OS.
        if (override.len == 0) {
            std.log.err("-Dcompiler-version must not be empty", .{});
            std.process.exit(1);
        }
        for (override) |c| {
            if (!std.ascii.isAlphanumeric(c) and c != '-' and c != '.' and c != '_') {
                std.log.err("-Dcompiler-version may only contain letters, digits, '-', '.' and '_', but got \"{s}\"", .{override});
                std.process.exit(1);
            }
        }
    }
    const enable_valgrind = b.option(bool, "valgrind", "Emit Valgrind client request support") orelse false;
    if (enable_valgrind and (builtin.target.os.tag != .linux or target.result.os.tag != .linux)) {
        std.log.err("-Dvalgrind=true requires a Linux build host and Linux target", .{});
        std.process.exit(1);
    }
    const valgrind_support = if (enable_valgrind) true else null;
    if (shared_memory_size) |size| {
        if (size == 0) {
            std.log.err("-Dshared-memory-size must be greater than 0", .{});
            std.process.exit(1);
        }
    }

    const parsed_args = parseBuildArgs(b);
    const run_args = parsed_args.run_args;
    const test_filters = parsed_args.test_filters;

    // llvm configuration
    // By default, use our bundled LLVM from roc-bootstrap. Users can opt-in to system LLVM
    // (e.g., for AFL++ fuzzing which requires system LLVM).
    const use_system_llvm = b.option(bool, "system-llvm", "Use system-installed LLVM instead of bundled LLVM (required for AFL++)") orelse false;
    const user_llvm_path = b.option([]const u8, "llvm-path", "Path to a custom LLVM installation containing bin, lib and include.");
    const roc_deps_path = b.option([]const u8, "roc-deps-path", "Path to a complete roc-bootstrap target bundle containing include and lib.");
    if ((roc_deps_path != null and (user_llvm_path != null or use_system_llvm)) or
        (user_llvm_path != null and use_system_llvm))
    {
        std.log.err("-Droc-deps-path, -Dllvm-path and -Dsystem-llvm are mutually exclusive", .{});
        std.process.exit(1);
    }
    const dependency_source: DependencySource = if (roc_deps_path) |path|
        .{ .local_bundle = path }
    else if (user_llvm_path) |path|
        .{ .custom_llvm = path }
    else if (use_system_llvm)
        .system_llvm
    else
        .downloaded_bundle;
    // Since zig afl is broken currently, default to system afl.
    const use_system_afl = b.option(bool, "system-afl", "Attempt to automatically detect and use system installed afl++") orelse true;

    if (user_llvm_path) |path| {
        // Even if the llvm backend is not enabled, still add the llvm path.
        // AFL++ may use it for building fuzzing executables.
        b.addSearchPrefix(b.pathJoin(&.{ path, "bin" }));
    }

    // tracy profiler configuration
    const flag_enable_tracy = b.option([]const u8, "tracy", "Enable Tracy integration. Supply path to Tracy source");
    const flag_tracy_callstack = b.option(bool, "tracy-callstack", "Include callstack information with Tracy data. Does nothing if -Dtracy is not provided") orelse false;
    const flag_tracy_allocation = b.option(bool, "tracy-allocation", "Include allocation information with Tracy data. Does nothing if -Dtracy is not provided") orelse (flag_enable_tracy != null);
    const flag_tracy_callstack_depth: u32 = b.option(u32, "tracy-callstack-depth", "Declare callstack depth for Tracy data. Does nothing if -Dtracy_callstack is not provided") orelse 10;
    if (flag_tracy_callstack) {
        std.log.warn("Tracy callstack is enable. This can significantly skew timings, but is important for understanding source location. Be cautious when generating timing and analyzing results.", .{});
    }

    // Create compile time build options
    const build_options = b.addOptions();
    build_options.addOption(bool, "enable_tracy", flag_enable_tracy != null);
    build_options.addOption(bool, "trace_eval", trace_eval);
    build_options.addOption(bool, "trace_refcount", trace_refcount);
    build_options.addOption(bool, "trace_modules", trace_modules);
    build_options.addOption(bool, "trace_build", trace_build);
    build_options.addOption(bool, "debug_gpa", debug_gpa);
    build_options.addOption(bool, "linker_warnings", linker_warnings);
    build_options.addOption(bool, "debug_gpa_traces", debug_gpa_traces);
    build_options.addOption(bool, "has_shared_memory_size", shared_memory_size != null);
    build_options.addOption(u64, "shared_memory_size", shared_memory_size orelse 0);
    build_options.addOption(bool, "print_trmc", print_trmc);
    build_options.addOption(bool, "print_ir_after_trmc", print_ir_after_trmc);
    build_options.addOption([]const u8, "llvm_keep_ir", llvm_keep_ir);
    build_options.addOption([]const u8, "llvm_keep_bitcode", llvm_keep_bitcode);
    build_options.addOption([]const u8, "llvm_keep_object", llvm_keep_object);
    build_options.addOption(u64, "test_progress_interval_ms", test_progress_interval_ms);
    build_options.addOption(bool, "eval_no_fork", eval_no_fork);
    build_options.addOption(bool, "eval_time_worker", eval_time_worker);
    const compiler_version_git = getCompilerVersionGit(b);
    const compiler_version_options = b.addOptions();
    compiler_version_options.addOption([]const u8, "compiler_version_git", compiler_version_git);
    const compiler_identity = compilerIdentityModule(b, dependency_source, flag_enable_tracy, &.{
        b.fmt("enable-tracy={}", .{flag_enable_tracy != null}),
        b.fmt("tracy-allocation={}", .{flag_enable_tracy != null and flag_tracy_allocation}),
        b.fmt("tracy-callstack={}", .{flag_enable_tracy != null and flag_tracy_callstack}),
        b.fmt("tracy-callstack-depth={d}", .{if (flag_enable_tracy != null and flag_tracy_callstack) (if (flag_tracy_callstack_depth > 0) flag_tracy_callstack_depth else 10) else @as(u32, 0)}),
        b.fmt("valgrind={}", .{enable_valgrind}),
        b.fmt("trace-eval={}", .{trace_eval}),
        b.fmt("trace-refcount={}", .{trace_refcount}),
        b.fmt("debug-gpa={}", .{debug_gpa}),
        b.fmt("debug-gpa-traces={}", .{debug_gpa_traces}),
    }) orelse return;
    build_options.contents.appendSlice(b.allocator,
        \\// One source/dependency identity is shared, but each executable's actual
        \\// compile mode and target participate independently. Release artifacts
        \\// remain correctly identified when the surrounding graph uses Debug.
        \\pub const compiler_compatibility_hash: [32]u8 = identity: {
        \\    @setEvalBranchQuota(1000000);
        \\    const actual = @import("builtin");
        \\    const std = @import("std");
        \\    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        \\    hasher.update("roc-compiler-artifact-compatibility-v2");
        \\    hasher.update(&@import("compiler_identity").compiler_compatibility_hash);
        \\    hasher.update(std.fmt.comptimePrint(";mode={s};target={s}-{s}-{s};cpu={s};backend={s};", .{
        \\        @tagName(actual.mode), @tagName(actual.cpu.arch), @tagName(actual.os.tag),
        \\        @tagName(actual.abi), actual.cpu.model.name, @tagName(actual.zig_backend),
        \\    }));
        \\    hasher.update(std.fmt.comptimePrint(";os-range={s};object-format={s};", .{
        \\        compilerOsRangeEncoding(actual.os.versionRange()), @tagName(actual.object_format),
        \\    }));
        \\    for (0..std.Target.Cpu.Feature.Set.needed_bit_count) |index| {
        \\        hasher.update(&.{@intFromBool(actual.cpu.features.isEnabled(@intCast(index)))});
        \\    }
        \\    var digest: [32]u8 = undefined;
        \\    hasher.final(&digest);
        \\    break :identity digest;
        \\};
        \\// Debug formatting limits nested display depth, which would omit Linux
        \\// kernel bounds. JSON preserves every field and frames optional strings,
        \\// tags and unknown Windows version integers without native byte order.
        \\pub fn compilerOsRangeEncoding(comptime range: @import("std").Target.Os.TaggedVersionRange) []const u8 {
        \\    return comptime @import("std").fmt.comptimePrint("{f}", .{@import("std").json.fmt(range, .{})});
        \\}
        \\pub const compiler_compatibility_id: []const u8 = &@import("std").fmt.bytesToHex(compiler_compatibility_hash, .lower);
        \\// Checked artifacts are target independent and are baked by a Debug host
        \\// tool for consumers built in other modes. Their compiler input is the
        \\// common source/dependency/semantic-options identity.
        \\pub const compiler_artifact_hash = @import("compiler_identity").compiler_compatibility_hash;
        \\
    ) catch @panic("OOM");
    // Human version metadata is separate from compatibility/build options.
    // Git-only changes must not rebuild host tools or rebake checked builtins.
    // `compiler_version` (e.g. "release-fast-abc12345") is assembled in the generated
    // compiler_version module so its build-mode prefix comes from @import("builtin").mode—the
    // actual optimization level of each compiled binary. The prefix can't be baked here because
    // the version module is shared between the dev `roc` exe (whose mode follows -Doptimize) and the
    // `release` exe (always built ReleaseFast); a single build-time value can't be right for both.
    //
    // -Dcompiler-version replaces the whole string, and is emitted as a string literal so that
    // `compiler_version` keeps the same type either way. Nightly builds pass their release tag
    // (e.g. "nightly-2026-July-31-f5556d8"), because "release-fast-<sha>" tells a user nothing
    // about which nightly they downloaded.
    if (compiler_version_override) |override| {
        compiler_version_options.contents.appendSlice(b.allocator, b.fmt(
            \\
            \\pub const compiler_version = "{s}";
            \\
        , .{override})) catch @panic("OOM");
    } else {
        compiler_version_options.contents.appendSlice(b.allocator,
            \\
            \\pub const compiler_version = @import("std").fmt.comptimePrint("{s}-{s}", .{
            \\    switch (@import("builtin").mode) {
            \\        .debug => "debug",
            \\        .safe => "release-safe",
            \\        .fast => "release-fast",
            \\        .small => "release-small",
            \\    },
            \\    compiler_version_git,
            \\});
            \\
        ) catch @panic("OOM");
    }
    // Shared config for every first-party leak-checking DebugAllocator. Capturing a
    // stack trace per allocation dominates Debug-build runtime (~80% of `roc test`
    // wall time on macOS arm64), so traces are off unless -Ddebug-gpa-traces is
    // passed; leaks are still detected without them. Lives in the generated
    // build_options module because it is imported by everything that instantiates
    // a DebugAllocator, including minimal test platform hosts.
    build_options.contents.appendSlice(b.allocator,
        \\
        \\pub const debug_gpa_stack_trace_frames: usize =
        \\    if (debug_gpa_traces and @import("std").debug.sys_can_stack_trace) 6 else 0;
        \\
        \\/// Appended to leak reports in traceless builds; empty when traces are on.
        \\pub const debug_gpa_leak_hint: []const u8 = if (debug_gpa_stack_trace_frames == 0)
        \\    "note: leak reports above have no allocation-site stack traces; rebuild with `-Ddebug-gpa-traces` to capture them\n"
        \\else
        \\    "";
        \\
        \\/// Deinit-check for leak-checking DebugAllocators: prints the
        \\/// -Ddebug-gpa-traces hint when `check` reports a leak in a traceless
        \\/// build, then returns whether the check passed.
        \\pub fn debugGpaOk(check: @import("std").heap.Check) bool {
        \\    if (check == .leak) {
        \\        @import("std").debug.print("{s}", .{debug_gpa_leak_hint});
        \\    }
        \\    return check == .ok;
        \\}
        \\
    ) catch @panic("OOM");
    build_options.addOption(bool, "enable_tracy_callstack", flag_tracy_callstack);
    build_options.addOption(bool, "enable_tracy_allocation", flag_tracy_allocation);
    build_options.addOption(u32, "tracy_callstack_depth", flag_tracy_callstack_depth);

    // Calculate effective strip value
    // - If strip is explicitly set by user, use that (warn if tracy_callstack is also set)
    // - Otherwise, default to stripping if not debug, unless tracy_callstack is enabled
    const strip: bool = blk: {
        if (strip_flag) |strip_bool| {
            // User explicitly set strip
            if (strip_bool and flag_tracy_callstack) {
                std.log.warn("Both -Dstrip and -Dtracy-callstack are enabled. " ++
                    "Stripping will remove callstack information needed by Tracy.", .{});
            }
            break :blk strip_bool;
        } else {
            // User did not set strip - use defaults
            if (flag_tracy_callstack) {
                // Don't strip when tracy callstack is enabled (preserves debug info)
                break :blk false;
            } else {
                // Default: strip in release modes
                break :blk optimize != .debug;
            }
        }
    };

    // Don't omit frame pointer when tracy callstack is enabled (needed for callstack capture)
    const omit_frame_pointer: ?bool = if (flag_tracy_callstack) false else null;

    // Whether the host can execute binaries built for the configured target.
    // The ABI is deliberately not part of this: Linux builds default to
    // `.musl` (statically linked) while the build runner itself is gnu, and
    // such a binary runs fine on a glibc host.
    const host_can_run_target =
        target.result.os.tag == builtin.target.os.tag and
        target.result.cpu.arch == builtin.target.cpu.arch;

    // This module is also imported by native host tools, whose target differs
    // from the outer build target. Derive the value in each importing binary so
    // cross-target options do not rebuild the unchanged Debug builtin compiler.
    // CPU feature overrides still count as native for real macOS FSEvents.
    build_options.contents.appendSlice(b.allocator, b.fmt(
        \\
        \\pub const target_is_native =
        \\    @import("builtin").target.os.tag == .@"{s}" and
        \\    @import("builtin").target.cpu.arch == .@"{s}" and
        \\    @import("builtin").target.abi == .@"{s}";
        \\
    , .{
        @tagName(builtin.target.os.tag),
        @tagName(builtin.target.cpu.arch),
        @tagName(builtin.target.abi),
    })) catch @panic("OOM");

    // Path to bundled Darwin sysroot with libSystem.tbd stub
    build_options.addOptionPathDirectory("darwin_sysroot", b.path("src/cli/darwin"));

    // We use zstd for `roc bundle` and `roc unbundle` and downloading .tar.zst bundles.
    const zstd = b.dependency("zstd", .{
        .target = target,
        .optimize = optimize,
    });
    const host_zstd = b.dependency("zstd", .{
        .target = b.graph.host,
        .optimize = optimize,
    });

    const roc_modules = modules.RocModules.create(b, build_options, zstd);
    const compiler_version_module = compiler_version_options.createModule();
    roc_modules.lsp.addImport("compiler_version", compiler_version_module);
    roc_modules.build_options.addImport("compiler_identity", compiler_identity);
    const unit_test_runner = UnitTestRunner{
        .path = b.path("src/build/unit_test_runner.zig"),
        .build_options = roc_modules.build_options,
        .zig_default_test_runner = b.createModule(.{
            .root_source_file = b.path("vendor/zig_test_runner.zig"),
        }),
    };

    // Build-time compiler for builtin .roc modules
    //
    // Always rebuild builtins when building roc to ensure they match the compiler.
    // The builtin_compiler is cached by zig, so this only adds overhead when
    // compiler sources actually change.
    const builtin_roc_path = "src/build/roc/Builtin.roc";

    const write_compiled_builtins = b.addWriteFiles();

    // Always regenerate .bin files to ensure they match the current compiler
    const builtin_compiler = createAndRunBuiltinCompiler(b, roc_modules, flag_enable_tracy, &.{builtin_roc_path});
    const bake_repro = CheckBuiltinBakeReproducibleStep.create(b, builtin_compiler.exe);
    build_test_builtin_bake_reproducible_step.dependOn(&builtin_compiler.exe.step);
    run_test_builtin_bake_reproducible_step.dependOn(build_test_builtin_bake_reproducible_step);
    run_test_builtin_bake_reproducible_step.dependOn(&bake_repro.step);
    write_compiled_builtins.step.dependOn(&builtin_compiler.run.step);

    // Copy tracked outputs from the builtin compiler run step.
    _ = write_compiled_builtins.addCopyFile(
        builtin_compiler.builtin_bin,
        "Builtin.bin",
    );

    // Copy the source Builtin.roc file for embedding
    _ = write_compiled_builtins.addCopyFile(
        b.path(builtin_roc_path),
        "Builtin.roc",
    );

    // Copy the baked CheckedModuleArtifact
    _ = write_compiled_builtins.addCopyFile(
        builtin_compiler.builtin_artifact_bin,
        "Builtin.artifact.bin",
    );

    // Generate compiled_builtins.zig with hardcoded Builtin module.
    // The embedded blobs are copied by Zig at compile time into 16-byte-aligned
    // static storage, so runtime code can build views over them directly.
    const builtins_source_str =
        \\const generated_indices = @import("builtin_indices");
        \\
        \\const builtin_bin_raw = @embedFile("Builtin.bin");
        \\pub var builtin_bin: [builtin_bin_raw.len]u8 align(16) = builtin_bin_raw.*;
        \\pub const builtin_source = @embedFile("Builtin.roc");
        \\const builtin_artifact_bin_raw = @embedFile("Builtin.artifact.bin");
        \\pub var builtin_artifact_bin: [builtin_artifact_bin_raw.len]u8 align(16) = builtin_artifact_bin_raw.*;
        \\pub const builtin_indices_raw = generated_indices.builtin_indices_raw;
        \\pub fn builtinIndices(comptime CIR: type) CIR.BuiltinIndices {
        \\    return generated_indices.builtinIndices(CIR);
        \\}
        \\pub const builtin_type_registry_hash = generated_indices.builtin_type_registry_hash;
        \\pub const builtin_indices_layout_hash = generated_indices.builtin_indices_layout_hash;
        \\
    ;

    const compiled_builtins_source = write_compiled_builtins.add(
        "compiled_builtins.zig",
        builtins_source_str,
    );

    const compiled_builtins_module = b.createModule(.{
        .root_source_file = compiled_builtins_source,
    });
    compiled_builtins_module.addImport("builtin_indices", b.createModule(.{
        .root_source_file = builtin_compiler.builtin_indices_zig,
    }));

    const bytebox = b.dependency("bytebox", .{
        .target = target,
        .optimize = optimize,
    });

    const zig_lints_exe = b.addExecutable(.{
        .name = "zig_lints",
        .root_module = b.createModule(.{
            .root_source_file = b.path("ci/zig_lints.zig"),
            .target = b.graph.host,
            .optimize = .debug,
        }),
    });
    const tidy_exe = b.addExecutable(.{
        .name = "tidy",
        .root_module = b.createModule(.{
            .root_source_file = b.path("ci/tidy.zig"),
            .target = b.graph.host,
            .optimize = .debug,
        }),
    });
    const test_wiring_exe = b.addExecutable(.{
        .name = "check_test_wiring",
        .root_module = b.createModule(.{
            .root_source_file = b.path("ci/check_test_wiring.zig"),
            .target = b.graph.host,
            .optimize = .debug,
        }),
    });
    const source_bidi_module = b.createModule(.{
        .root_source_file = b.path("src/base/bidi.zig"),
        .target = b.graph.host,
        .optimize = .debug,
    });
    const source_bidi_root = b.createModule(.{
        .root_source_file = b.path("ci/check_source_bidi.zig"),
        .target = b.graph.host,
        .optimize = .debug,
    });
    source_bidi_root.addImport("bidi", source_bidi_module);
    const source_bidi_exe = b.addExecutable(.{ .name = "check-source-bidi", .root_module = source_bidi_root });
    const install_source_bidi = b.addInstallArtifact(source_bidi_exe, .{});
    build_check_tools_step.dependOn(&install_source_bidi.step);
    const run_source_bidi = b.addRunArtifact(source_bidi_exe);
    run_source_bidi.step.dependOn(&install_source_bidi.step);
    const source_bidi_tests = b.addTest(.{ .name = "source-bidi-tests", .root_module = source_bidi_root });
    run_check_source_bidi_step.dependOn(&b.addRunArtifact(source_bidi_tests).step);
    run_check_source_bidi_step.dependOn(&run_source_bidi.step);

    const minici_exe = b.addExecutable(.{
        .name = "minici",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/minici.zig"),
            .target = b.graph.host,
            .optimize = .debug,
        }),
    });
    minici_exe.root_module.addImport("build_options", roc_modules.build_options);
    minici_exe.root_module.addImport("roc_target", roc_modules.roc_target);

    const install_zig_lints = b.addInstallArtifact(zig_lints_exe, .{});
    const install_tidy = b.addInstallArtifact(tidy_exe, .{});
    const install_test_wiring = b.addInstallArtifact(test_wiring_exe, .{});
    const install_minici = b.addInstallArtifact(minici_exe, .{});
    build_check_tools_step.dependOn(&install_zig_lints.step);
    build_check_tools_step.dependOn(&install_tidy.step);
    build_check_tools_step.dependOn(&install_test_wiring.step);
    build_check_tools_step.dependOn(&install_minici.step);

    const run_zig_lints = b.addRunArtifact(zig_lints_exe);
    run_zig_lints.step.dependOn(&install_zig_lints.step);
    run_check_zig_lints_step.dependOn(&run_zig_lints.step);

    const run_tidy = b.addRunArtifact(tidy_exe);
    run_tidy.step.dependOn(&install_tidy.step);
    run_check_tidy_step.dependOn(&run_tidy.step);

    const run_git_lints = b.addRunArtifact(tidy_exe);
    run_git_lints.addArg("--git-lints");
    run_git_lints.step.dependOn(&install_tidy.step);
    run_check_git_lints_step.dependOn(&run_git_lints.step);

    const run_test_wiring = b.addRunArtifact(test_wiring_exe);
    run_test_wiring.step.dependOn(&install_test_wiring.step);
    run_check_test_wiring_step.dependOn(&run_test_wiring.step);

    // The wiring check's semantic phase runs every Zig test binary with
    // --listen=- to enumerate the tests each one actually contains (metadata
    // only; no tests execute), then requires every named test decl in src/ to
    // appear in some binary. The binary paths are appended as args at each
    // b.addTest site below. They are only passed when the host can execute
    // the configured target and no --test-filter trimmed the test set;
    // otherwise the checker falls back to its import-level check alone.
    const enumerate_tests_for_wiring_check = host_can_run_target and test_filters.len == 0;

    const run_minici = b.addRunArtifact(minici_exe);
    run_minici.addArg(b.graph.zig_exe);
    for (b.graph.search_prefixes.items) |search_prefix| {
        run_minici.addArg("--search-prefix");
        run_minici.addArg(search_prefix);
    }
    // MiniCI forwards these args to every child `zig build` it runs, so
    // `zig build minici -Ddebug-gpa-traces` gives all child steps traced
    // leak reports (CI re-runs failed leaky jobs this way).
    if (debug_gpa_traces) {
        run_minici.addArg("-Ddebug-gpa-traces");
    }
    if (run_args.len != 0) run_minici.addArgs(run_args);
    run_minici.addPassthruArgs();
    run_minici.step.dependOn(&install_minici.step);
    run_minici_step.dependOn(&run_minici.step);

    roc_modules.compile.addImport("compiled_builtins", compiled_builtins_module);
    roc_modules.eval.addImport("compiled_builtins", compiled_builtins_module);
    roc_modules.eval.addImport("bytebox", bytebox.module("bytebox"));
    roc_modules.lsp.addImport("compiled_builtins", compiled_builtins_module);
    roc_modules.lsp_unit.addImport("compiled_builtins", compiled_builtins_module);
    roc_modules.lsp_integration.addImport("compiled_builtins", compiled_builtins_module);

    const check_test_env_module = b.createModule(.{
        .root_source_file = b.path("src/check/test_env_pkg.zig"),
    });
    check_test_env_module.addImport("tracy", roc_modules.tracy);
    check_test_env_module.addImport("builtins", roc_modules.builtins);
    check_test_env_module.addImport("collections", roc_modules.collections);
    check_test_env_module.addImport("base", roc_modules.base);
    check_test_env_module.addImport("parse", roc_modules.parse);
    check_test_env_module.addImport("types", roc_modules.types);
    check_test_env_module.addImport("can", roc_modules.can);
    check_test_env_module.addImport("reporting", roc_modules.reporting);
    check_test_env_module.addImport("compiled_builtins", compiled_builtins_module);

    // Build wasm32 builtins object at build time so the eval/REPL pipeline can
    // merge real compiled builtins into WASM modules (instead of using host imports).
    const wasm32_resolved_target = b.resolveTargetQuery(.{ .cpu_arch = .wasm32, .os_tag = .freestanding, .abi = .none });
    const wasm32_boxy_runtime_obj = buildBoxyRuntimeObject(
        b,
        roc_modules,
        wasm32_resolved_target,
        strip,
        omit_frame_pointer,
        "roc_boxy_runtime_wasm32_eval",
        b.path("src/boxy_runtime/eval_main.zig"),
    );
    const wasm32_boxy_runtime_files = b.addWriteFiles();
    _ = wasm32_boxy_runtime_files.addCopyFile(
        wasmObjectArtifact(b, wasm32_boxy_runtime_obj),
        "roc_boxy_runtime.o",
    );
    const wasm32_boxy_runtime_module = b.createModule(.{
        .root_source_file = wasm32_boxy_runtime_files.add(
            "wasm32_boxy_runtime.zig",
            "pub const bytes = @embedFile(\"roc_boxy_runtime.o\");\n",
        ),
    });
    roc_modules.eval.addImport("wasm32_boxy_runtime", wasm32_boxy_runtime_module);

    const wasm32_builtins_obj = b.addObject(.{
        .name = "roc_builtins_wasm32_eval",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/static_lib.zig"),
            .target = wasm32_resolved_target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true,
        }),
    });
    wasm32_builtins_obj.root_module.addImport("tracy", b.createModule(.{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    wasm32_builtins_obj.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    wasm32_builtins_obj.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    wasm32_builtins_obj.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    wasm32_builtins_obj.bundle_compiler_rt = false;
    configureBackend(wasm32_builtins_obj, wasm32_resolved_target);

    const wasm32_compiler_rt_obj = b.addObject(.{
        .name = "compiler_rt_wasm32_eval",
        .root_module = b.createModule(.{
            .root_source_file = std.Build.LazyPath.zig_lib.path(b, "compiler_rt.zig"),
            .target = wasm32_resolved_target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true,
        }),
    });
    wasm32_compiler_rt_obj.bundle_compiler_rt = false;
    configureBackend(wasm32_compiler_rt_obj, wasm32_resolved_target);

    const link_wasm32_builtins = b.addSystemCommand(&.{ b.graph.zig_exe, "wasm-ld", "-r" });
    link_wasm32_builtins.addArg("-o");
    const merged_wasm32_builtins = link_wasm32_builtins.addOutputFileArg("roc_builtins.o");
    link_wasm32_builtins.addFileArg(wasm32_builtins_obj.getEmittedBin());
    link_wasm32_builtins.addFileArg(wasm32_compiler_rt_obj.getEmittedBin());

    const wasm32_builtins_files = b.addWriteFiles();
    _ = wasm32_builtins_files.addCopyFile(merged_wasm32_builtins, "roc_builtins.o");
    const wasm32_builtins_module = b.createModule(.{
        .root_source_file = wasm32_builtins_files.add("wasm32_builtins.zig",
            \\pub const bytes = @embedFile("roc_builtins.o");
            \\
        ),
    });
    roc_modules.eval.addImport("wasm32_builtins", wasm32_builtins_module);

    // Setup test platform host libraries
    const wasm_test_hosts = setupTestPlatforms(b, target, optimize, roc_modules, build_test_hosts_step, strip, omit_frame_pointer, platform_filter);
    const wasm_host_step = wasm_test_hosts.wasm32;
    const wasm_host_fixture_files = b.addWriteFiles();
    _ = wasm_host_fixture_files.addCopyFile(
        test_fixtures.output(wasm_host_step, "test/wasm/platform/targets/wasm32/host.wasm"),
        "host.wasm",
    );
    const wasm_host_fixture_module = b.createModule(.{
        .root_source_file = wasm_host_fixture_files.add(
            "wasm_host_fixture.zig",
            "pub const host_wasm = @embedFile(\"host.wasm\");\n",
        ),
    });
    wasm_host_fixture_files.step.dependOn(wasm_host_step);

    const llvm_codegen_module = b.addModule("llvm_codegen", .{
        .root_source_file = b.path("src/backend/llvm/MonoLlvmCodeGen.zig"),
    });
    llvm_codegen_module.addImport("base", roc_modules.base);
    llvm_codegen_module.addImport("layout", roc_modules.layout);
    llvm_codegen_module.addImport("backend", roc_modules.backend);
    llvm_codegen_module.addImport("lir", roc_modules.lir);
    llvm_codegen_module.addImport("ctx", roc_modules.ctx);
    llvm_codegen_module.addImport("builtins", roc_modules.builtins);
    llvm_codegen_module.addImport("build_options", roc_modules.build_options);
    llvm_codegen_module.addImport("roc_target", roc_modules.roc_target);
    llvm_codegen_module.addImport("vendor_llvm_ir", roc_modules.vendor_llvm_ir);

    // On macOS the linked roc executable exports every global symbol of its
    // statically linked LLVM, LLD, Binaryen, zlib and zstd, and dyld walks
    // all of that on every launch—~540M instructions before main() runs
    // (#10992). Every install of the roc CLI for a macOS target goes through
    // this tool, which removes the export trie and weak-bind info and
    // rewrites the ad-hoc code signature.
    const dyld_export_strip_module = b.createModule(.{
        .root_source_file = b.path("src/cli/macho/DyldExportStrip.zig"),
        .imports = &.{
            .{ .name = "base", .module = roc_modules.base },
            .{ .name = "vendor_macho", .module = roc_modules.vendor_macho },
        },
    });
    const strip_macho_exports_tool = b.addExecutable(.{
        .name = "strip_macho_exports",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/strip_macho_exports.zig"),
            .target = b.graph.host,
            .optimize = .safe,
            .imports = &.{
                .{ .name = "dyld_export_strip", .module = dyld_export_strip_module },
                .{ .name = "build_options", .module = roc_modules.build_options },
            },
        }),
    });

    const main_exe_result = addMainExe(b, roc_modules, target, optimize, strip, omit_frame_pointer, dependency_source, flag_enable_tracy, zstd, compiled_builtins_module, write_compiled_builtins, llvm_codegen_module, flag_enable_tracy, test_filters, true, valgrind_support) orelse return;
    const roc_exe = main_exe_result.exe;
    roc_exe.root_module.addImport("compiler_version", compiler_version_module);
    const fixture_roc = executableRuntimePath(b, roc_exe, strip_macho_exports_tool);
    const fixture_options = b.addOptions();
    fixture_options.addOptionPathUntracked("roc_binary_path", fixture_roc);
    roc_modules.addAll(roc_exe);
    const roc_install_step = install_and_run(b, no_bin, roc_exe, strip_macho_exports_tool, build_roc_step, run_roc_step, run_args);

    const run_builtin_format = b.addRunArtifact(roc_exe);
    run_builtin_format.addArgs(&.{ "fmt", "--check", "src/build/roc/Builtin.roc" });
    run_builtin_format.step.dependOn(build_roc_step);
    run_check_builtin_format_step.dependOn(&run_builtin_format.step);

    const run_simd_codegen_check = b.addSystemCommand(&.{ "bash", "ci/check_simd_codegen.sh" });
    run_simd_codegen_check.addFileArg(fixture_roc);
    run_simd_codegen_check.setCwd(test_fixtures.mutableRoot(&.{build_test_hosts_step}));
    run_simd_codegen_check.step.dependOn(build_test_hosts_step);
    run_check_simd_codegen_step.dependOn(&run_simd_codegen_check.step);

    const run_baseline_codegen_check = b.addSystemCommand(&.{ "bash", "ci/check_baseline_codegen.sh" });
    run_baseline_codegen_check.addFileArg(fixture_roc);
    run_baseline_codegen_check.setCwd(test_fixtures.mutableRoot(&.{build_test_hosts_step}));
    run_baseline_codegen_check.step.dependOn(build_test_hosts_step);
    run_check_baseline_codegen_step.dependOn(&run_baseline_codegen_check.step);

    const run_match_extension_codegen_check = b.addSystemCommand(&.{ "bash", "ci/check_match_extension_codegen.sh" });
    run_match_extension_codegen_check.addFileArg(fixture_roc);
    run_match_extension_codegen_check.setCwd(test_fixtures.mutableRoot(&.{build_test_hosts_step}));
    run_match_extension_codegen_check.step.dependOn(build_test_hosts_step);
    run_check_match_extension_codegen_step.dependOn(&run_match_extension_codegen_check.step);

    const run_str_eq_same_allocation_check = b.addSystemCommand(&.{ "bash", "ci/check_str_eq_same_allocation.sh" });
    run_str_eq_same_allocation_check.addFileArg(fixture_roc);
    run_str_eq_same_allocation_check.setCwd(test_fixtures.mutableRoot(&.{build_test_hosts_step}));
    run_str_eq_same_allocation_check.step.dependOn(build_test_hosts_step);
    run_check_str_eq_same_allocation_step.dependOn(&run_str_eq_same_allocation_check.step);

    // Glue ABI locks compile the generated bindings themselves. Zig is checked
    // for every native architecture/OS plus wasm, C is checked against the
    // vector-heavy layout probe over the same matrix, and Rust is checked for
    // the configured native target plus wasm (rustc requires an installed core
    // library for each cross target). Native integration tests execute all
    // three languages in both directions on each CI host OS/architecture.
    {
        const glue_inputs = b.addWriteFiles();
        _ = glue_inputs.addCopyDirectory(b.path("src/glue"), "src/glue", .{ .include_extensions = &.{".roc"} });
        _ = glue_inputs.addCopyDirectory(b.path("test/fx/platform"), "test/fx/platform", .{ .include_extensions = &.{".roc"} });
        _ = glue_inputs.addCopyDirectory(b.path("test/glue/layout-probe"), "test/glue/layout-probe", .{ .include_extensions = &.{".roc"} });
        _ = glue_inputs.addCopyDirectory(b.path("test/glue/tag-union-layouts"), "test/glue/tag-union-layouts", .{ .include_extensions = &.{".roc"} });
        const glue_root = glue_inputs.getDirectory();
        const run_glue_abi = b.addRunArtifact(roc_exe);
        run_glue_abi.addArgs(&.{ "glue", "--no-cache" });
        run_glue_abi.addFileArg(glue_root.path(b, "src/glue/src/ZigGlue.roc"));
        const glue_abi_dir = run_glue_abi.addOutputDirectoryArg("glue-zig-abi");
        run_glue_abi.addFileArg(glue_root.path(b, "test/fx/platform/main.roc"));
        // WriteFiles hashes the complete import tree, including membership.

        const run_zig_union_layouts = b.addRunArtifact(roc_exe);
        run_zig_union_layouts.addArgs(&.{ "glue", "--no-cache" });
        run_zig_union_layouts.addFileArg(glue_root.path(b, "src/glue/src/ZigGlue.roc"));
        const zig_union_layouts_dir = run_zig_union_layouts.addOutputDirectoryArg("glue-zig-union-layouts");
        run_zig_union_layouts.addFileArg(glue_root.path(b, "test/glue/tag-union-layouts/main.roc"));

        const lock_targets = [_]struct { name: []const u8, target: std.Build.ResolvedTarget }{
            .{ .name = "x64_linux", .target = b.resolveTargetQuery(.{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .musl }) },
            .{ .name = "arm64_linux", .target = b.resolveTargetQuery(.{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .musl }) },
            .{ .name = "x64_macos", .target = b.resolveTargetQuery(.{ .cpu_arch = .x86_64, .os_tag = .macos }) },
            .{ .name = "arm64_macos", .target = b.resolveTargetQuery(.{ .cpu_arch = .aarch64, .os_tag = .macos }) },
            .{ .name = "x64_windows", .target = b.resolveTargetQuery(.{ .cpu_arch = .x86_64, .os_tag = .windows, .abi = .msvc }) },
            .{ .name = "arm64_windows", .target = b.resolveTargetQuery(.{ .cpu_arch = .aarch64, .os_tag = .windows, .abi = .msvc }) },
            .{ .name = "wasm32", .target = b.resolveTargetQuery(.{ .cpu_arch = .wasm32, .os_tag = .freestanding, .abi = .none }) },
        };
        for (lock_targets) |lock_target| {
            const lock_obj = b.addObject(.{
                .name = b.fmt("glue_zig_abi_lock_{s}", .{lock_target.name}),
                .root_module = b.createModule(.{
                    .root_source_file = b.path("test/glue/zig_abi_lock.zig"),
                    .target = lock_target.target,
                    .optimize = optimize,
                }),
            });
            lock_obj.root_module.addImport("builtins", roc_modules.builtins);
            lock_obj.root_module.addAnonymousImport("glue_abi", .{
                .root_source_file = glue_abi_dir.path(b, "roc_platform_abi.zig"),
            });
            run_check_glue_abi_step.dependOn(&lock_obj.step);

            const union_lock_obj = b.addObject(.{
                .name = b.fmt("glue_zig_union_layouts_{s}", .{lock_target.name}),
                .root_module = b.createModule(.{
                    .root_source_file = b.path("test/glue/tag-union-layouts/compile_lock.zig"),
                    .target = lock_target.target,
                    .optimize = optimize,
                }),
            });
            union_lock_obj.root_module.addAnonymousImport("glue_abi", .{
                .root_source_file = zig_union_layouts_dir.path(b, "roc_platform_abi.zig"),
            });
            run_check_glue_abi_step.dependOn(&union_lock_obj.step);
        }

        const run_c_glue_abi = b.addRunArtifact(roc_exe);
        run_c_glue_abi.addArgs(&.{ "glue", "--no-cache" });
        run_c_glue_abi.addFileArg(glue_root.path(b, "src/glue/src/CGlue.roc"));
        const c_glue_abi_dir = run_c_glue_abi.addOutputDirectoryArg("glue-c-abi");
        run_c_glue_abi.addFileArg(glue_root.path(b, "test/glue/layout-probe/main.roc"));

        const run_rust_glue_abi = b.addRunArtifact(roc_exe);
        run_rust_glue_abi.addArgs(&.{ "glue", "--no-cache" });
        run_rust_glue_abi.addFileArg(glue_root.path(b, "src/glue/src/RustGlue.roc"));
        const rust_glue_abi_dir = run_rust_glue_abi.addOutputDirectoryArg("glue-rust-abi");
        run_rust_glue_abi.addFileArg(glue_root.path(b, "test/glue/layout-probe/main.roc"));

        const foreign_abi_generator = b.addExecutable(.{
            .name = "generate_foreign_abi_lock",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/glue/generate_abi_lock.zig"),
                .target = b.graph.host,
                .optimize = .safe,
                .imports = &.{.{ .name = "builtins", .module = roc_modules.builtins }},
            }),
        });
        const generate_foreign_lock = b.addRunArtifact(foreign_abi_generator);
        const canonical_header = generate_foreign_lock.addOutputFileArg("canonical_host_abi.h");
        generate_foreign_lock.addFileArg(rust_glue_abi_dir.path(b, "roc_platform_abi.rs"));
        const rust_with_canonical_lock = generate_foreign_lock.addOutputFileArg("roc_platform_abi_lock.rs");

        const c_lock_targets = [_]struct { name: []const u8, triple: []const u8, simd128: bool }{
            .{ .name = "x64_linux", .triple = "x86_64-linux-musl", .simd128 = false },
            .{ .name = "arm64_linux", .triple = "aarch64-linux-musl", .simd128 = false },
            .{ .name = "x64_macos", .triple = "x86_64-macos", .simd128 = false },
            .{ .name = "arm64_macos", .triple = "aarch64-macos", .simd128 = false },
            .{ .name = "x64_windows", .triple = "x86_64-windows-msvc", .simd128 = false },
            .{ .name = "arm64_windows", .triple = "aarch64-windows-msvc", .simd128 = false },
            .{ .name = "wasm32", .triple = "wasm32-freestanding", .simd128 = true },
        };
        for (c_lock_targets) |lock_target| {
            const compile_c_lock = b.addSystemCommand(&.{
                "zig",
                "cc",
                "-target",
                lock_target.triple,
                "-std=c11",
                "-Wall",
                "-Wextra",
                "-Werror",
                "-c",
            });
            if (lock_target.simd128) compile_c_lock.addArg("-msimd128");
            compile_c_lock.addArg("-I");
            compile_c_lock.addDirectoryArg(c_glue_abi_dir);
            compile_c_lock.addArg("-I");
            compile_c_lock.addDirectoryArg(canonical_header.dirname());
            compile_c_lock.addFileArg(b.path("test/glue/c_abi_compile_lock.c"));
            compile_c_lock.addArg("-o");
            _ = compile_c_lock.addOutputFileArg(b.fmt("c-abi-lock-{s}.o", .{lock_target.name}));
            run_check_glue_abi_step.dependOn(&compile_c_lock.step);
        }

        const run_rust_union_layouts = b.addRunArtifact(roc_exe);
        run_rust_union_layouts.addArgs(&.{ "glue", "--no-cache" });
        run_rust_union_layouts.addFileArg(glue_root.path(b, "src/glue/src/RustGlue.roc"));
        const rust_union_layouts_dir = run_rust_union_layouts.addOutputDirectoryArg("glue-rust-union-layouts");
        run_rust_union_layouts.addFileArg(glue_root.path(b, "test/glue/tag-union-layouts/main.roc"));

        const rust_union_lock_files = b.addWriteFiles();
        _ = rust_union_lock_files.addCopyFile(rust_union_layouts_dir.path(b, "roc_platform_abi.rs"), "roc_platform_abi.rs");
        const rust_union_lock_source = rust_union_lock_files.addCopyFile(b.path("test/glue/tag-union-layouts/compile_lock.rs"), "compile_lock.rs");

        const native_arch = target.result.cpu.arch;
        const native_rust_target: ?[]const u8 = switch (roc_target.classifyOs(target.result.os.tag)) {
            .linux => if (native_arch == .x86_64) "x86_64-unknown-linux-musl" else if (native_arch == .aarch64) "aarch64-unknown-linux-musl" else null,
            .macos => if (native_arch == .x86_64) "x86_64-apple-darwin" else if (native_arch == .aarch64) "aarch64-apple-darwin" else null,
            .windows => if (native_arch == .x86_64) "x86_64-pc-windows-msvc" else if (native_arch == .aarch64) "aarch64-pc-windows-msvc" else null,
            .freebsd, .openbsd, .netbsd, .other => null,
        };
        const rust_lock_targets = [_]?struct { name: []const u8, triple: []const u8, simd128: bool }{
            if (native_rust_target) |triple| .{ .name = "native", .triple = triple, .simd128 = false } else null,
            .{ .name = "wasm32", .triple = "wasm32-unknown-unknown", .simd128 = true },
        };
        for (rust_lock_targets) |maybe_lock_target| {
            const lock_target = maybe_lock_target orelse continue;
            const compile_rust_lock = b.addSystemCommand(&.{
                "rustc",
                "--edition=2021",
                "-D",
                "warnings",
                "--crate-type=lib",
                "--emit=metadata",
                "--cfg",
                "no_roc_std_helpers",
                "--target",
                lock_target.triple,
            });
            if (lock_target.simd128) compile_rust_lock.addArgs(&.{ "-C", "target-feature=+simd128" });
            compile_rust_lock.addFileArg(rust_with_canonical_lock);
            compile_rust_lock.addArg("-o");
            _ = compile_rust_lock.addOutputFileArg(b.fmt("rust-abi-lock-{s}.rmeta", .{lock_target.name}));
            run_check_glue_abi_step.dependOn(&compile_rust_lock.step);

            const compile_rust_union_lock = b.addSystemCommand(&.{
                "rustc",
                "--edition=2021",
                "-D",
                "warnings",
                "--crate-type=lib",
                "--emit=metadata",
                "--cfg",
                "no_roc_std_helpers",
                "--target",
                lock_target.triple,
            });
            compile_rust_union_lock.addFileArg(rust_union_lock_source);
            compile_rust_union_lock.addArg("-o");
            _ = compile_rust_union_lock.addOutputFileArg(b.fmt("rust-union-layouts-{s}.rmeta", .{lock_target.name}));
            run_check_glue_abi_step.dependOn(&compile_rust_union_lock.step);
        }
    }

    var release_exe_for_llvm_embedded: ?*Step.Compile = null;

    // Release build with platform-optimal settings
    {
        const release_target = b.resolveTargetQuery(getReleaseTargetQuery(b, target));
        // Create a release-specific zstd dependency with release settings
        const release_zstd = b.dependency("zstd", .{
            .target = release_target,
            .optimize = .fast,
        });
        const release_exe_result = addMainExe(
            b,
            roc_modules,
            release_target,
            .fast, // Always ReleaseFast for release
            true, // Always strip for release
            null, // Default frame pointer handling
            dependency_source,
            null, // No tracy for release
            release_zstd,
            compiled_builtins_module,
            write_compiled_builtins,
            llvm_codegen_module,
            null, // No tracy
            test_filters,
            false,
            valgrind_support,
        );
        if (release_exe_result) |result| {
            const exe = result.exe;
            exe.root_module.addImport("compiler_version", compiler_version_module);
            roc_modules.addAll(exe);
            exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
            exe.step.dependOn(&write_compiled_builtins.step);
            release_exe_for_llvm_embedded = exe;
            build_release_step.dependOn(addInstallMaybeStrippedExe(b, exe, strip_macho_exports_tool));
        }
    }

    const cli_test_options = b.addOptions();
    cli_test_options.addOption(bool, "binaryen", dependency_source.isBundled());

    // CLI integration tests: one harness-backed runner covers platforms,
    // subcommands, echo, and glue. Focus locally with:
    //   zig build run-test-cli -- --suite echo --filter "case name"
    if (!no_bin) {
        // install_and_run only returns null under no_bin, which this branch
        // excludes.
        const install_step = roc_install_step.?;

        const parallel_cli_runner_exe = b.addExecutable(.{
            .name = "parallel_cli_runner",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/cli/test/parallel_cli_runner.zig"),
                .target = target,
                .optimize = optimize,
                .imports = &.{
                    .{ .name = "base", .module = roc_modules.base },
                    .{ .name = "test_harness", .module = createTestHarnessModule(b, roc_modules) },
                    .{ .name = "collections", .module = roc_modules.collections },
                    .{ .name = "backend", .module = roc_modules.backend },
                    .{ .name = "builtins", .module = roc_modules.builtins },
                    .{ .name = "bytebox", .module = bytebox.module("bytebox") },
                    .{ .name = "build_options", .module = roc_modules.build_options },
                },
            }),
        });
        parallel_cli_runner_exe.root_module.link_libc = true;
        parallel_cli_runner_exe.root_module.addOptions("fixture_options", fixture_options);
        parallel_cli_runner_exe.root_module.addOptions("cli_test_options", cli_test_options);
        build_test_cli_runners_step.dependOn(&parallel_cli_runner_exe.step);

        const run_cli = b.addRunArtifact(parallel_cli_runner_exe);
        run_cli.addFileArg(fixture_roc);
        run_cli.setCwd(test_fixtures.mutableRoot(&.{build_test_hosts_step}));
        if (cli_test_llvm) {
            run_cli.addArg("--include-llvm");
        }
        for (test_filters) |f| {
            run_cli.addArg("--filter");
            run_cli.addArg(f);
        }
        if (run_args.len != 0) run_cli.addArgs(run_args);
        run_cli.addPassthruArgs();
        run_cli.step.dependOn(install_step);
        run_cli.step.dependOn(build_test_hosts_step);
        run_test_cli_step.dependOn(&run_cli.step);
    }

    // Manual rebuild command: zig build run-rebuild-builtins
    // Use this after making compiler changes to ensure those changes are reflected in builtins
    const rebuild_builtins_step = b.step(
        "run-rebuild-builtins",
        "Force rebuild of all builtin modules (*.roc -> *.bin)",
    );

    // Clean zig-out/ to ensure a fresh rebuild of builtins
    // Note: We don't delete .zig-cache because it contains build options needed during compilation.
    const clean_out_step = RemoveDirTreeStep.create(b, "zig-out");

    // Discover .roc files again for the rebuild command
    const roc_files_force = discoverBuiltinRocFiles(b) catch |err| {
        std.debug.print("Failed to discover .roc files for rebuild: {}\n", .{err});
        return;
    };

    const run_builtin_compiler_force = createAndRunBuiltinCompiler(b, roc_modules, flag_enable_tracy, roc_files_force);
    run_builtin_compiler_force.run.step.dependOn(&clean_out_step.step);
    rebuild_builtins_step.dependOn(&run_builtin_compiler_force.run.step);

    // Add the compiled builtins module to roc exe and make it depend on the builtins being ready
    roc_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    roc_exe.step.dependOn(&write_compiled_builtins.step);

    roc_modules.eval.addAnonymousImport("llvm_compile", .{
        .root_source_file = b.path("src/llvm_compile/mod.zig"),
        .imports = &.{
            .{ .name = "collections", .module = roc_modules.collections },
            .{ .name = "layout", .module = roc_modules.layout },
            .{ .name = "backend", .module = roc_modules.backend },
            .{ .name = "lir", .module = roc_modules.lir },
            .{ .name = "llvm_codegen", .module = llvm_codegen_module },
            .{ .name = "vendor_llvm_compile_bindings", .module = roc_modules.vendor_llvm_compile_bindings },
            .{ .name = "build_options", .module = roc_modules.build_options },
            .{ .name = "roc_target", .module = roc_modules.roc_target },
            .{ .name = "builtins", .module = roc_modules.builtins },
            .{ .name = "embedded_lld", .module = roc_modules.embedded_lld },
        },
    });
    const builtins64_target = b.resolveTargetQuery(.{ .cpu_arch = .wasm64, .os_tag = .freestanding, .abi = .none });
    const builtins64_bc_obj = b.addObject(.{
        .name = "roc_builtins64_bc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/static_lib.zig"),
            .target = builtins64_target,
            .optimize = .fast,
            .strip = true,
            .pic = true,
            .single_threaded = true,
        }),
    });
    builtins64_bc_obj.root_module.addImport("tracy", b.createModule(.{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    builtins64_bc_obj.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    builtins64_bc_obj.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    builtins64_bc_obj.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    builtins64_bc_obj.root_module.omit_frame_pointer = true;
    builtins64_bc_obj.root_module.stack_check = false;
    builtins64_bc_obj.root_module.link_libc = false;
    builtins64_bc_obj.use_llvm = true;
    builtins64_bc_obj.bundle_compiler_rt = false;
    _ = builtins64_bc_obj.getEmittedBin();
    const builtins64_bc_file = builtins64_bc_obj.getEmittedLlvmBc();

    const builtins64_core_bc_obj = b.addObject(.{
        .name = "roc_builtins64_core_bc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/static_lib_core.zig"),
            .target = builtins64_target,
            .optimize = .fast,
            .strip = true,
            .pic = true,
            .single_threaded = true,
        }),
    });
    builtins64_core_bc_obj.root_module.addImport("tracy", b.createModule(.{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    builtins64_core_bc_obj.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    builtins64_core_bc_obj.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    builtins64_core_bc_obj.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    builtins64_core_bc_obj.root_module.omit_frame_pointer = true;
    builtins64_core_bc_obj.root_module.stack_check = false;
    builtins64_core_bc_obj.root_module.link_libc = false;
    builtins64_core_bc_obj.use_llvm = true;
    builtins64_core_bc_obj.bundle_compiler_rt = false;
    _ = builtins64_core_bc_obj.getEmittedBin();
    const builtins64_core_bc_file = builtins64_core_bc_obj.getEmittedLlvmBc();

    const builtins32_target = b.resolveTargetQuery(.{ .cpu_arch = .wasm32, .os_tag = .freestanding, .abi = .none });
    const builtins32_bc_obj = b.addObject(.{
        .name = "roc_builtins32_bc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/static_lib.zig"),
            .target = builtins32_target,
            .optimize = .fast,
            .strip = true,
            .pic = true,
            .single_threaded = true,
        }),
    });
    builtins32_bc_obj.root_module.addImport("tracy", b.createModule(.{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    builtins32_bc_obj.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    builtins32_bc_obj.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    builtins32_bc_obj.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    builtins32_bc_obj.root_module.omit_frame_pointer = true;
    builtins32_bc_obj.root_module.stack_check = false;
    builtins32_bc_obj.use_llvm = true;
    builtins32_bc_obj.bundle_compiler_rt = false;
    _ = builtins32_bc_obj.getEmittedBin();
    const builtins32_bc_file = builtins32_bc_obj.getEmittedLlvmBc();

    const builtins32_core_bc_obj = b.addObject(.{
        .name = "roc_builtins32_core_bc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/static_lib_core.zig"),
            .target = builtins32_target,
            .optimize = .fast,
            .strip = true,
            .pic = true,
            .single_threaded = true,
        }),
    });
    builtins32_core_bc_obj.root_module.addImport("tracy", b.createModule(.{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    builtins32_core_bc_obj.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    builtins32_core_bc_obj.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    builtins32_core_bc_obj.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    builtins32_core_bc_obj.root_module.omit_frame_pointer = true;
    builtins32_core_bc_obj.root_module.stack_check = false;
    builtins32_core_bc_obj.use_llvm = true;
    builtins32_core_bc_obj.bundle_compiler_rt = false;
    _ = builtins32_core_bc_obj.getEmittedBin();
    const builtins32_core_bc_file = builtins32_core_bc_obj.getEmittedLlvmBc();

    const builtins64_extern_bc_obj = b.addObject(.{
        .name = "roc_builtins64_extern_bc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/extern_static_lib.zig"),
            .target = builtins64_target,
            .optimize = .fast,
            .strip = true,
            .pic = true,
            .single_threaded = true,
        }),
    });
    builtins64_extern_bc_obj.root_module.addImport("tracy", b.createModule(.{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    builtins64_extern_bc_obj.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    builtins64_extern_bc_obj.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    builtins64_extern_bc_obj.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    builtins64_extern_bc_obj.root_module.omit_frame_pointer = true;
    builtins64_extern_bc_obj.root_module.stack_check = false;
    builtins64_extern_bc_obj.use_llvm = true;
    builtins64_extern_bc_obj.bundle_compiler_rt = false;
    _ = builtins64_extern_bc_obj.getEmittedBin();
    const builtins64_extern_bc_file = builtins64_extern_bc_obj.getEmittedLlvmBc();

    const builtins64_core_extern_bc_obj = b.addObject(.{
        .name = "roc_builtins64_core_extern_bc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/extern_static_lib_core.zig"),
            .target = builtins64_target,
            .optimize = .fast,
            .strip = true,
            .pic = true,
            .single_threaded = true,
        }),
    });
    builtins64_core_extern_bc_obj.root_module.addImport("tracy", b.createModule(.{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    builtins64_core_extern_bc_obj.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    builtins64_core_extern_bc_obj.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    builtins64_core_extern_bc_obj.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    builtins64_core_extern_bc_obj.root_module.omit_frame_pointer = true;
    builtins64_core_extern_bc_obj.root_module.stack_check = false;
    builtins64_core_extern_bc_obj.use_llvm = true;
    builtins64_core_extern_bc_obj.bundle_compiler_rt = false;
    _ = builtins64_core_extern_bc_obj.getEmittedBin();
    const builtins64_core_extern_bc_file = builtins64_core_extern_bc_obj.getEmittedLlvmBc();

    const builtins32_extern_bc_obj = b.addObject(.{
        .name = "roc_builtins32_extern_bc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/extern_static_lib.zig"),
            .target = builtins32_target,
            .optimize = .fast,
            .strip = true,
            .pic = true,
            .single_threaded = true,
        }),
    });
    builtins32_extern_bc_obj.root_module.addImport("tracy", b.createModule(.{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    builtins32_extern_bc_obj.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    builtins32_extern_bc_obj.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    builtins32_extern_bc_obj.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    builtins32_extern_bc_obj.root_module.omit_frame_pointer = true;
    builtins32_extern_bc_obj.root_module.stack_check = false;
    builtins32_extern_bc_obj.use_llvm = true;
    builtins32_extern_bc_obj.bundle_compiler_rt = false;
    _ = builtins32_extern_bc_obj.getEmittedBin();
    const builtins32_extern_bc_file = builtins32_extern_bc_obj.getEmittedLlvmBc();

    const builtins32_core_extern_bc_obj = b.addObject(.{
        .name = "roc_builtins32_core_extern_bc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/extern_static_lib_core.zig"),
            .target = builtins32_target,
            .optimize = .fast,
            .strip = true,
            .pic = true,
            .single_threaded = true,
        }),
    });
    builtins32_core_extern_bc_obj.root_module.addImport("tracy", b.createModule(.{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    builtins32_core_extern_bc_obj.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    builtins32_core_extern_bc_obj.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    builtins32_core_extern_bc_obj.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    builtins32_core_extern_bc_obj.root_module.omit_frame_pointer = true;
    builtins32_core_extern_bc_obj.root_module.stack_check = false;
    builtins32_core_extern_bc_obj.use_llvm = true;
    builtins32_core_extern_bc_obj.bundle_compiler_rt = false;
    _ = builtins32_core_extern_bc_obj.getEmittedBin();
    const builtins32_core_extern_bc_file = builtins32_core_extern_bc_obj.getEmittedLlvmBc();

    // The 64-bit builtins bitcode references its SHA-256 compression by name
    // (see `Sha256` in src/builtins/crypto.zig), and the LLVM backend links the
    // definition for the rounds the target's CPU features select. Each of
    // these payloads is compiled for exactly the features its rounds need.
    const sha256_portable_bc_file = addSha256RoundsBitcode(b, "portable", .{ .cpu_arch = .wasm64, .os_tag = .freestanding, .abi = .none });
    const sha256_x86_sha_bc_file = addSha256RoundsBitcode(b, "x86_sha", x86_sha: {
        var query: std.Target.Query = .{ .cpu_arch = .x86_64, .os_tag = .freestanding, .abi = .none, .cpu_model = .baseline };
        query.cpu_features_add.addFeature(@backingInt(std.Target.x86.Feature.sha));
        query.cpu_features_add.addFeature(@backingInt(std.Target.x86.Feature.ssse3));
        break :x86_sha query;
    });
    const sha256_aarch64_sha2_bc_file = addSha256RoundsBitcode(b, "aarch64_sha2", aarch64_sha2: {
        var query: std.Target.Query = .{ .cpu_arch = .aarch64, .os_tag = .freestanding, .abi = .none, .cpu_model = .baseline };
        query.cpu_features_add.addFeature(@backingInt(std.Target.aarch64.Feature.sha2));
        break :aarch64_sha2 query;
    });

    const llvm_embedded_files = b.addWriteFiles();
    _ = llvm_embedded_files.addCopyFile(builtins32_bc_file, "builtins32.bc");
    _ = llvm_embedded_files.addCopyFile(builtins64_bc_file, "builtins64.bc");
    _ = llvm_embedded_files.addCopyFile(builtins32_core_bc_file, "builtins32_core.bc");
    _ = llvm_embedded_files.addCopyFile(builtins64_core_bc_file, "builtins64_core.bc");
    _ = llvm_embedded_files.addCopyFile(builtins32_extern_bc_file, "builtins32_extern.bc");
    _ = llvm_embedded_files.addCopyFile(builtins64_extern_bc_file, "builtins64_extern.bc");
    _ = llvm_embedded_files.addCopyFile(builtins32_core_extern_bc_file, "builtins32_core_extern.bc");
    _ = llvm_embedded_files.addCopyFile(builtins64_core_extern_bc_file, "builtins64_core_extern.bc");
    _ = llvm_embedded_files.addCopyFile(sha256_portable_bc_file, "sha256_portable.bc");
    _ = llvm_embedded_files.addCopyFile(sha256_x86_sha_bc_file, "sha256_x86_sha.bc");
    _ = llvm_embedded_files.addCopyFile(sha256_aarch64_sha2_bc_file, "sha256_aarch64_sha2.bc");

    const llvm_embedded_source: []const u8 =
        \\pub const builtins32_bc = @embedFile("builtins32.bc");
        \\pub const builtins64_bc = @embedFile("builtins64.bc");
        \\pub const builtins32_core_bc = @embedFile("builtins32_core.bc");
        \\pub const builtins64_core_bc = @embedFile("builtins64_core.bc");
        \\pub const builtins32_extern_bc = @embedFile("builtins32_extern.bc");
        \\pub const builtins64_extern_bc = @embedFile("builtins64_extern.bc");
        \\pub const builtins32_core_extern_bc = @embedFile("builtins32_core_extern.bc");
        \\pub const builtins64_core_extern_bc = @embedFile("builtins64_core_extern.bc");
        \\pub const sha256_portable_bc = @embedFile("sha256_portable.bc");
        \\pub const sha256_x86_sha_bc = @embedFile("sha256_x86_sha.bc");
        \\pub const sha256_aarch64_sha2_bc = @embedFile("sha256_aarch64_sha2.bc");
        \\pub const builtins_bc = builtins64_bc;
        \\
    ;

    const llvm_embedded_module = b.createModule(.{
        .root_source_file = llvm_embedded_files.add("llvm_embedded.zig", llvm_embedded_source),
    });
    roc_exe.step.dependOn(&llvm_embedded_files.step);
    roc_exe.root_module.addImport("llvm_embedded", llvm_embedded_module);
    if (release_exe_for_llvm_embedded) |exe| {
        exe.step.dependOn(&llvm_embedded_files.step);
        exe.root_module.addImport("llvm_embedded", llvm_embedded_module);
    }

    const llvm_compile_module = b.createModule(.{
        .root_source_file = b.path("src/llvm_compile/mod.zig"),
        .imports = &.{
            .{ .name = "collections", .module = roc_modules.collections },
            .{ .name = "layout", .module = roc_modules.layout },
            .{ .name = "backend", .module = roc_modules.backend },
            .{ .name = "lir", .module = roc_modules.lir },
            .{ .name = "llvm_codegen", .module = llvm_codegen_module },
            .{ .name = "vendor_llvm_compile_bindings", .module = roc_modules.vendor_llvm_compile_bindings },
            .{ .name = "build_options", .module = roc_modules.build_options },
            .{ .name = "roc_target", .module = roc_modules.roc_target },
            .{ .name = "builtins", .module = roc_modules.builtins },
            .{ .name = "llvm_embedded", .module = llvm_embedded_module },
            .{ .name = "embedded_lld", .module = roc_modules.embedded_lld },
        },
    });
    roc_modules.eval.addImport("llvm_compile", llvm_compile_module);
    roc_modules.glue.addImport("llvm_compile", llvm_compile_module);

    // Add snapshot tool
    const snapshot_exe = b.addExecutable(.{
        .name = "snapshot",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/snapshot_tool/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .valgrind = valgrind_support,
        }),
    });
    splitCompilerSections(snapshot_exe);
    configureBackend(snapshot_exe, target);
    roc_modules.addAll(snapshot_exe);
    snapshot_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    snapshot_exe.step.dependOn(&write_compiled_builtins.step);
    try addLlvmSupportToStep(
        b,
        snapshot_exe,
        target,
        dependency_source,
        roc_modules,
        llvm_codegen_module,
        llvm_embedded_module,
        zstd,
    );
    if (snapshot_exe.root_module.resolved_target.?.result.os.tag != .windows or
        snapshot_exe.root_module.resolved_target.?.result.abi != .msvc)
    {
        snapshot_exe.root_module.link_libcpp = true;
    }

    add_tracy(b, roc_modules.build_options, snapshot_exe, target, true, flag_enable_tracy);
    const snapshot_exe_install = install_and_run(
        b,
        no_bin,
        snapshot_exe,
        null,
        build_snapshot_tool_step,
        run_snapshot_tool_step,
        run_args,
    );
    const check_snapshot_diff = CheckSnapshotDiffStep.create(b);
    check_snapshot_diff.step.dependOn(run_snapshot_tool_step);
    run_check_snapshots_step.dependOn(&check_snapshot_diff.step);

    // Add parallel eval test runner
    const simd_test_sources_module = b.createModule(.{
        .root_source_file = b.path("test/simd/eval_sources.zig"),
    });
    const eval_test_exe = b.addExecutable(.{
        .name = "eval-test-runner",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/eval/test/parallel_runner.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true, // needed for sljmp/setjmp
        }),
    });
    // The deepest eval test recurses ~1000 frames; Zig 0.16 codegen pushes that past
    // the 1 MiB Windows default. Reserve a generous stack so recursive eval tests
    // don't trip our SetUnhandledExceptionFilter stack-overflow handler.
    eval_test_exe.stack_size = stack_budget.roc_stack_size;
    configureBackend(eval_test_exe, target);
    roc_modules.addAll(eval_test_exe);
    eval_test_exe.root_module.addOptions("coverage_options", blk: {
        const opts = b.addOptions();
        opts.addOption(bool, "coverage", false);
        break :blk opts;
    });
    eval_test_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    eval_test_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));
    eval_test_exe.root_module.addImport("test_harness", createTestHarnessModule(b, roc_modules));
    eval_test_exe.root_module.addImport("simd_test_sources", simd_test_sources_module);
    eval_test_exe.step.dependOn(&write_compiled_builtins.step);
    try addLlvmSupportToStep(
        b,
        eval_test_exe,
        target,
        dependency_source,
        roc_modules,
        llvm_codegen_module,
        llvm_embedded_module,
        zstd,
    );
    if (eval_test_exe.root_module.resolved_target.?.result.os.tag != .windows or
        eval_test_exe.root_module.resolved_target.?.result.abi != .msvc)
    {
        eval_test_exe.root_module.link_libcpp = true;
    }
    add_tracy(b, roc_modules.build_options, eval_test_exe, target, true, flag_enable_tracy);
    // Build eval runner args: forward all --test-filter values as --filter args.
    const eval_run_args = if (test_filters.len > 0) blk: {
        var eval_args_list = std.ArrayList([]const u8).empty;
        for (run_args) |arg| {
            eval_args_list.append(b.allocator, arg) catch @panic("OOM");
        }
        for (test_filters) |f| {
            eval_args_list.append(b.allocator, "--filter") catch @panic("OOM");
            eval_args_list.append(b.allocator, f) catch @panic("OOM");
        }
        break :blk eval_args_list.toOwnedSlice(b.allocator) catch @panic("OOM");
    } else run_args;
    _ = install_and_run(
        b,
        no_bin,
        eval_test_exe,
        null,
        build_test_eval_runner_step,
        run_test_eval_step,
        eval_run_args,
    );

    const run_simd_eval = b.addRunArtifact(eval_test_exe);
    run_simd_eval.addArgs(&.{
        "--filter",
        "SIMD full differential corpus",
        "--threads",
        "1",
        "--timeout",
        "900000",
        "--llvm",
    });

    const eval_host_effects_exe = b.addExecutable(.{
        .name = "eval-host-effects-runner",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/eval/test/host_effects_runner.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    configureBackend(eval_host_effects_exe, target);
    roc_modules.addAll(eval_host_effects_exe);
    eval_host_effects_exe.root_module.addImport("builtins", roc_modules.builtins);
    eval_host_effects_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    eval_host_effects_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));
    eval_host_effects_exe.root_module.addImport("test_harness", createTestHarnessModule(b, roc_modules));
    eval_host_effects_exe.step.dependOn(&write_compiled_builtins.step);
    try addLlvmSupportToStep(
        b,
        eval_host_effects_exe,
        target,
        dependency_source,
        roc_modules,
        llvm_codegen_module,
        llvm_embedded_module,
        zstd,
    );
    if (eval_host_effects_exe.root_module.resolved_target.?.result.os.tag != .windows or
        eval_host_effects_exe.root_module.resolved_target.?.result.abi != .msvc)
    {
        eval_host_effects_exe.root_module.link_libcpp = true;
    }
    const eval_host_effects_run_args = if (test_filters.len > 0) blk: {
        var eval_args_list = std.ArrayList([]const u8).empty;
        for (run_args) |arg| {
            eval_args_list.append(b.allocator, arg) catch @panic("OOM");
        }
        for (test_filters) |f| {
            eval_args_list.append(b.allocator, "--filter") catch @panic("OOM");
            eval_args_list.append(b.allocator, f) catch @panic("OOM");
        }
        break :blk eval_args_list.toOwnedSlice(b.allocator) catch @panic("OOM");
    } else run_args;
    _ = install_and_run(
        b,
        no_bin,
        eval_host_effects_exe,
        null,
        build_test_eval_host_effects_runner_step,
        run_test_eval_host_effects_step,
        eval_host_effects_run_args,
    );

    const lambda_mono_differential_exe = b.addExecutable(.{
        .name = "lambda-mono-differential-runner",
        // Linux compiler targets use musl so Roc-produced binaries are portable.
        // Keep this host-executed test runner portable too instead of requiring
        // the host to provide musl's dynamic loader. Other hosts retain their
        // platform's ordinary executable linkage.
        .linkage = if (target.result.os.tag == .linux and target.result.abi.isMusl()) .static else null,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/eval/test/lambda_mono_differential_runner.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    // The tree evaluator and the deepest eval corpus programs both recurse;
    // match the eval runner's generous stack.
    lambda_mono_differential_exe.stack_size = stack_budget.roc_stack_size;
    configureBackend(lambda_mono_differential_exe, target);
    roc_modules.addAll(lambda_mono_differential_exe);
    lambda_mono_differential_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    lambda_mono_differential_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));
    lambda_mono_differential_exe.root_module.addImport("test_harness", createTestHarnessModule(b, roc_modules));
    lambda_mono_differential_exe.root_module.addImport("simd_test_sources", simd_test_sources_module);
    lambda_mono_differential_exe.root_module.addOptions("coverage_options", blk: {
        const opts = b.addOptions();
        opts.addOption(bool, "coverage", false);
        break :blk opts;
    });
    lambda_mono_differential_exe.step.dependOn(&write_compiled_builtins.step);
    try addLlvmSupportToStep(
        b,
        lambda_mono_differential_exe,
        target,
        dependency_source,
        roc_modules,
        llvm_codegen_module,
        llvm_embedded_module,
        zstd,
    );
    if (lambda_mono_differential_exe.root_module.resolved_target.?.result.os.tag != .windows or
        lambda_mono_differential_exe.root_module.resolved_target.?.result.abi != .msvc)
    {
        lambda_mono_differential_exe.root_module.link_libcpp = true;
    }
    const lambda_mono_differential_run_args = if (test_filters.len > 0) blk: {
        var lm_args_list = std.ArrayList([]const u8).empty;
        for (run_args) |arg| {
            lm_args_list.append(b.allocator, arg) catch @panic("OOM");
        }
        for (test_filters) |f| {
            lm_args_list.append(b.allocator, "--filter") catch @panic("OOM");
            lm_args_list.append(b.allocator, f) catch @panic("OOM");
        }
        break :blk lm_args_list.toOwnedSlice(b.allocator) catch @panic("OOM");
    } else run_args;
    const lambda_mono_differential_install = install_and_run(
        b,
        no_bin,
        lambda_mono_differential_exe,
        null,
        build_test_lambda_mono_differential_step,
        run_test_lambda_mono_differential_step,
        lambda_mono_differential_run_args,
    );

    // One explicitly expensive proof gate owns every execution lane. Keep the
    // commands sequential: each corpus is intentionally large, and running
    // several compiler instances concurrently obscures failures and wastes RAM.
    const run_simd_runtime_dev = b.addRunArtifact(roc_exe);
    run_simd_runtime_dev.setCwd(test_fixtures.mutableRoot(&.{build_test_hosts_step}));
    run_simd_runtime_dev.addArgs(&.{ "--opt=dev", "--no-cache", "test/simd/differential.roc" });
    run_simd_runtime_dev.step.dependOn(build_test_hosts_step);

    const run_simd_runtime_speed = b.addRunArtifact(roc_exe);
    run_simd_runtime_speed.setCwd(test_fixtures.mutableRoot(&.{build_test_hosts_step}));
    run_simd_runtime_speed.addArgs(&.{ "--opt=speed", "--no-cache", "test/simd/differential.roc" });
    run_simd_runtime_speed.step.dependOn(&run_simd_runtime_dev.step);

    const run_simd_ctfe = b.addRunArtifact(roc_exe);
    run_simd_ctfe.setCwd(test_fixtures.mutableRoot(&.{build_test_hosts_step}));
    run_simd_ctfe.addArgs(&.{ "test", "--opt=speed", "--no-cache", "test/simd/differential.roc" });
    run_simd_ctfe.step.dependOn(&run_simd_runtime_speed.step);

    run_simd_eval.step.dependOn(&run_simd_ctfe.step);

    if (lambda_mono_differential_install) |install| {
        // Evaluator tests compile temporary Roc programs beneath `.zig-cache`.
        // Run the installed Lambda Mono runner so this final gate does not
        // depend on a cache artifact surviving the preceding evaluator.
        const run_simd_lambda_mono = Step.Run.create(b, "run installed lambda_mono_differential");
        run_simd_lambda_mono.addFileArg(.{ .relative = .{ .base = .install_bin, .sub_path = lambda_mono_differential_exe.out_filename } });
        run_simd_lambda_mono.addArgs(&.{
            "--filter",
            "SIMD full differential corpus",
            "--threads",
            "1",
            "--timeout",
            "900000",
            "corpus-only",
        });
        run_simd_lambda_mono.addArgs(run_args);
        run_simd_lambda_mono.addPassthruArgs();
        run_simd_lambda_mono.step.dependOn(install);
        run_simd_lambda_mono.step.dependOn(&run_simd_eval.step);
        run_test_simd_differential_step.dependOn(&run_simd_lambda_mono.step);
    } else {
        run_test_simd_differential_step.dependOn(&lambda_mono_differential_exe.step);
    }

    const wasm_archive_exe = b.addExecutable(.{
        .name = "wasm_archive",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/wasm_archive.zig"),
            .target = b.graph.host,
            .optimize = optimize,
        }),
    });
    configureBackend(wasm_archive_exe, b.graph.host);
    wasm_archive_exe.root_module.addImport("bundle", roc_modules.bundle);
    wasm_archive_exe.root_module.addImport("build_options", roc_modules.build_options);
    wasm_archive_exe.root_module.linkLibrary(host_zstd.artifact("zstd"));

    // The playground Wasm module compiles the whole compiler for wasm32, which
    // makes it the most expensive job in `build-ci`. Measured peak linker RSS
    // for the same sources: Debug 11.8 GiB (85 MB of output), ReleaseSafe
    // 9.6 GiB, ReleaseSmall 4.0 GiB. The default build does not need a Debug
    // Wasm module -- the playground is driven over a Wasm protocol rather than
    // a debugger, and the compiler code it contains is safety-checked by the
    // native Debug tests -- so an unrequested Debug default becomes
    // ReleaseSmall. That also makes the default build agree with CI, which runs
    // `run-test-playground -Doptimize=ReleaseSmall`, and with `repl_wasm` and
    // `echo`, which are pinned to ReleaseSmall for the same reason.
    //
    // Every explicitly requested mode is honored, including `-Doptimize=Debug`:
    // an omitted `-Doptimize` also resolves to `.debug`, so the two are only
    // distinguishable through `user_input_options` (as with `target` and `cpu`
    // above). Asking for a Debug playground has to keep working -- it is just
    // not what an unqualified `zig build` should spend 12 GiB on.
    const playground_wasm_optimize: std.builtin.OptimizeMode =
        if (optimize == .debug and !b.user_input_options.contains("optimize"))
            .small
        else
            optimize;

    const playground_exe = b.addExecutable(.{
        .name = "playground",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/playground_wasm/main.zig"),
            .target = b.resolveTargetQuery(.{
                .cpu_arch = .wasm32,
                .os_tag = .freestanding,
            }),
            .optimize = playground_wasm_optimize,
        }),
    });
    configureBackend(playground_exe, b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    }));
    playground_exe.entry = .disabled;
    playground_exe.rdynamic = true;
    playground_exe.link_function_sections = true;
    playground_exe.import_memory = false;
    roc_modules.addAll(playground_exe);
    playground_exe.root_module.addImport("compiler_version", compiler_version_module);
    playground_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
    playground_exe.step.dependOn(&write_compiled_builtins.step);

    add_tracy(b, roc_modules.build_options, playground_exe, b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    }), false, null);

    const playground_install = b.addInstallArtifact(playground_exe, .{});
    build_playground_step.dependOn(&playground_install.step);

    const playground_wasm_archive_cmd = b.addRunArtifact(wasm_archive_exe);
    playground_wasm_archive_cmd.addFileArg(playground_exe.getEmittedBin());
    const playground_wasm_archive_out = playground_wasm_archive_cmd.addOutputFileArg("playground.wasm.zst");
    const playground_wasm_archive_install = b.addInstallFileWithDir(playground_wasm_archive_out, .lib, "playground/playground.wasm.zst");
    build_playground_wasm_archive_step.dependOn(&playground_wasm_archive_install.step);

    const repl_wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    });
    const repl_wasm = b.addExecutable(.{
        .name = "repl",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/repl_wasm/main.zig"),
            .target = repl_wasm_target,
            .optimize = .small,
        }),
    });
    configureBackend(repl_wasm, repl_wasm_target);
    repl_wasm.entry = .disabled;
    repl_wasm.rdynamic = true;
    repl_wasm.link_function_sections = true;
    repl_wasm.import_memory = false;
    roc_modules.addAll(repl_wasm);
    repl_wasm.root_module.addImport("ReplSession.zig", b.createModule(.{
        .root_source_file = b.path("src/cli/ReplSession.zig"),
        .target = repl_wasm_target,
        .optimize = .small,
        .imports = &.{
            .{ .name = "base", .module = roc_modules.base },
            .{ .name = "can", .module = roc_modules.can },
            .{ .name = "compile", .module = roc_modules.compile },
            .{ .name = "ctx", .module = roc_modules.ctx },
            .{ .name = "eval", .module = roc_modules.eval },
            .{ .name = "lir", .module = roc_modules.lir },
            .{ .name = "parse", .module = roc_modules.parse },
            .{ .name = "reporting", .module = roc_modules.reporting },
        },
    }));
    repl_wasm.root_module.addImport("compiled_builtins", compiled_builtins_module);
    repl_wasm.step.dependOn(&write_compiled_builtins.step);
    add_tracy(b, roc_modules.build_options, repl_wasm, repl_wasm_target, false, null);

    const repl_wasm_install = b.addInstallFile(repl_wasm.getEmittedBin(), "lib/repl/repl.wasm");
    build_repl_wasm_step.dependOn(&repl_wasm_install.step);

    const repl_wasm_archive_cmd = b.addRunArtifact(wasm_archive_exe);
    repl_wasm_archive_cmd.addFileArg(repl_wasm.getEmittedBin());
    const repl_wasm_archive_out = repl_wasm_archive_cmd.addOutputFileArg("repl.wasm.zst");
    const repl_wasm_archive_install = b.addInstallFileWithDir(repl_wasm_archive_out, .lib, "repl/repl.wasm.zst");
    build_repl_wasm_archive_step.dependOn(&repl_wasm_archive_install.step);
    inline for (.{ "index.html", "app.js", "cells.js", "worker.js" }) |filename| {
        const install_file = b.addInstallFile(b.path("src/repl_wasm/www/" ++ filename), "lib/repl/" ++ filename);
        build_repl_wasm_step.dependOn(&install_file.step);
        build_web_step.dependOn(&install_file.step);
    }
    const repl_protocol_types = b.addInstallFile(b.path("src/repl_wasm/protocol.d.ts"), "lib/repl/protocol.d.ts");
    build_repl_wasm_step.dependOn(&repl_protocol_types.step);
    build_web_step.dependOn(&repl_protocol_types.step);

    const repl_wasm_test = b.addExecutable(.{
        .name = "repl_wasm_test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/repl-wasm-test/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    configureBackend(repl_wasm_test, target);
    repl_wasm_test.root_module.addImport("bytebox", bytebox.module("bytebox"));
    repl_wasm_test.root_module.addImport("build_options", roc_modules.build_options);
    const run_repl_wasm_test = b.addRunArtifact(repl_wasm_test);
    run_repl_wasm_test.addFileArg(repl_wasm.getEmittedBin());
    run_repl_wasm_test.step.dependOn(&repl_wasm.step);
    run_test_repl_wasm_step.dependOn(&run_repl_wasm_test.step);
    const run_repl_cells_test = b.addSystemCommand(&.{ "node", "--test" });
    run_repl_cells_test.addFileArg(b.path("test/repl-wasm-test/cells.test.mjs"));
    run_test_repl_wasm_step.dependOn(&run_repl_cells_test.step);

    // Build echo.wasm—echo platform compiled to wasm32-freestanding.
    // Also serves as a regression test that the compile module stays wasm-compatible.
    {
        const echo_wasm_target = b.resolveTargetQuery(.{ .cpu_arch = .wasm32, .os_tag = .freestanding });
        const echo_wasm = b.addExecutable(.{
            .name = "echo",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/echo_platform/echo.zig"),
                .target = echo_wasm_target,
                .optimize = .small,
            }),
        });
        configureBackend(echo_wasm, echo_wasm_target);
        // This embeds the recursive compiler and interpreter, so its linear-memory
        // stack needs the compiler budget rather than wasm's 1 MiB default.
        echo_wasm.stack_size = stack_budget.roc_stack_size;
        echo_wasm.entry = .disabled;
        echo_wasm.rdynamic = true;
        echo_wasm.root_module.addImport("compile", roc_modules.compile);
        echo_wasm.root_module.addImport("check", roc_modules.check);
        echo_wasm.root_module.addImport("eval", roc_modules.eval);
        echo_wasm.root_module.addImport("lir", roc_modules.lir);
        echo_wasm.root_module.addImport("layout", roc_modules.layout);
        echo_wasm.root_module.addImport("base", roc_modules.base);
        echo_wasm.root_module.addImport("can", roc_modules.can);
        echo_wasm.root_module.addImport("echo_platform", roc_modules.echo_platform);
        echo_wasm.root_module.addImport("reporting", roc_modules.reporting);
        echo_wasm.root_module.addImport("roc_target", roc_modules.roc_target);
        echo_wasm.root_module.addImport("compiled_builtins", compiled_builtins_module);
        echo_wasm.root_module.addImport("WasmFilesystem.zig", b.createModule(.{
            .root_source_file = b.path("src/playground_wasm/WasmFilesystem.zig"),
            .target = echo_wasm_target,
            .imports = &.{.{ .name = "ctx", .module = roc_modules.ctx }},
        }));
        echo_wasm.step.dependOn(&write_compiled_builtins.step);

        const echo_wasm_install = b.addInstallFile(echo_wasm.getEmittedBin(), "lib/echo/echo.wasm");
        echo_wasm_step.dependOn(&echo_wasm_install.step);

        const echo_wasm_archive_cmd = b.addRunArtifact(wasm_archive_exe);
        echo_wasm_archive_cmd.addFileArg(echo_wasm.getEmittedBin());
        const echo_wasm_archive_out = echo_wasm_archive_cmd.addOutputFileArg("echo.wasm.zst");
        const echo_wasm_archive_install = b.addInstallFileWithDir(echo_wasm_archive_out, .lib, "echo/echo.wasm.zst");
        echo_wasm_archive_step.dependOn(&echo_wasm_archive_install.step);

        // Copy the echo platform www files alongside echo.wasm
        inline for (.{ "index.html", "app.js" }) |filename| {
            const install_file = b.addInstallFile(b.path("src/echo_platform/www/" ++ filename), "lib/echo/" ++ filename);
            echo_wasm_step.dependOn(&install_file.step);
        }

        // echo_native: native binary that drives the same runEcho pipeline as
        // echo.wasm. Use it to debug compile/run failures with real stack
        // traces. `zig build run-echo -- path/to/app.roc [--with-file Name=path] ...`
        const echo_native_exe = b.addExecutable(.{
            .name = "echo_native",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/echo_platform/echo_native.zig"),
                .target = target,
                .optimize = optimize,
                // CoreCtx.default's OS vtable pulls in std.c.getenv (and
                // transitively std.net's getaddrinfo); Zig 0.16 requires
                // libc to be linked explicitly when std.c.* is referenced.
                .link_libc = true,
            }),
        });
        configureBackend(echo_native_exe, target);
        echo_native_exe.root_module.addImport("build_options", roc_modules.build_options);
        echo_native_exe.root_module.addImport("compile", roc_modules.compile);
        echo_native_exe.root_module.addImport("check", roc_modules.check);
        echo_native_exe.root_module.addImport("eval", roc_modules.eval);
        echo_native_exe.root_module.addImport("lir", roc_modules.lir);
        echo_native_exe.root_module.addImport("layout", roc_modules.layout);
        echo_native_exe.root_module.addImport("base", roc_modules.base);
        echo_native_exe.root_module.addImport("can", roc_modules.can);
        echo_native_exe.root_module.addImport("echo_platform", roc_modules.echo_platform);
        echo_native_exe.root_module.addImport("reporting", roc_modules.reporting);
        echo_native_exe.root_module.addImport("roc_target", roc_modules.roc_target);
        echo_native_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
        echo_native_exe.step.dependOn(&write_compiled_builtins.step);

        const echo_native_install = b.addInstallArtifact(echo_native_exe, .{});

        const run_echo_step = b.step("run-echo", "Run the native echo platform driver (debug helper for echo.wasm)");
        const run_echo_cmd = b.addRunArtifact(echo_native_exe);
        if (run_args.len != 0) run_echo_cmd.addArgs(run_args);
        run_echo_cmd.addPassthruArgs();
        run_echo_cmd.step.dependOn(&echo_native_install.step);
        run_echo_step.dependOn(&run_echo_cmd.step);

        // test-echo-wasm: bytebox-driven integration test that loads the
        // declared echo.wasm artifact, supplies in-process js_echo + js_stderr,
        // and asserts the tutorial example produces the expected output.
        const echo_wasm_test_exe = b.addExecutable(.{
            .name = "echo_wasm_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/echo-wasm-test/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(echo_wasm_test_exe, target);
        echo_wasm_test_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));
        echo_wasm_test_exe.root_module.addImport("build_options", roc_modules.build_options);

        const run_test_echo_wasm_step = b.step("run-test-echo-wasm", "Run echo.wasm tutorial example through bytebox");
        const run_echo_wasm_test = b.addRunArtifact(echo_wasm_test_exe);
        run_echo_wasm_test.addFileArg(echo_wasm.getEmittedBin());
        run_test_echo_wasm_step.dependOn(&run_echo_wasm_test.step);
    }

    build_web_step.dependOn(&playground_install.step);
    build_web_step.dependOn(&repl_wasm_install.step);
    build_web_step.dependOn(echo_wasm_step);

    {
        const glue_release_exe = b.addExecutable(.{
            .name = "glue_release",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/build/glue_release.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(glue_release_exe, target);
        glue_release_exe.root_module.addImport("build_options", roc_modules.build_options);

        const glue_release_cmd = b.addRunArtifact(glue_release_exe);
        glue_release_cmd.addArg(glue_release_tag orelse "nightly-local");
        const glue_release_dir = glue_release_cmd.addOutputDirectoryArg("glue-release");

        const glue_release_install = b.addInstallDirectory(.{
            .source_dir = glue_release_dir,
            .install_dir = .prefix,
            .install_subdir = "glue-release",
        });
        const clean_glue_release_install = buildChecksRun(b, "remove-dir-tree");
        clean_glue_release_install.addDirectoryArg(.{ .relative = .{ .base = .install_prefix, .sub_path = "glue-release" } });
        glue_release_install.step.dependOn(&clean_glue_release_install.step);
        build_glue_release_step.dependOn(&glue_release_install.step);
    }

    // Build playground integration tests - now enabled for all optimization modes.
    // These drive `playground_exe` itself rather than a private copy of the same
    // root source: a second full compiler-for-wasm32 build would be duplicated
    // work, and testing the module `build-web` actually ships is the point.
    const playground_test_install = blk: {
        // The native runner follows `optimize` like every other test runner.
        // It used to share the Wasm copy's optimize mode so that the two agreed
        // on `compiler_version`; that expectation is now passed in explicitly
        // below, so the runner is free to keep Debug's safety checks.
        const playground_integration_test_exe = b.addExecutable(.{
            .name = "playground_integration_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/playground-integration/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(playground_integration_test_exe, target);
        playground_integration_test_exe.root_module.addImport("compiler_version", compiler_version_module);
        playground_integration_test_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));
        playground_integration_test_exe.root_module.addImport("build_options", roc_modules.build_options);
        playground_integration_test_exe.root_module.addImport("test_harness", createTestHarnessModule(b, roc_modules));
        roc_modules.addAll(playground_integration_test_exe);

        const install = b.addInstallArtifact(playground_integration_test_exe, .{});
        install.step.dependOn(&playground_exe.step);
        build_test_playground_runner_step.dependOn(&install.step);

        const run_playground_test = b.addRunArtifact(playground_integration_test_exe);
        run_playground_test.addArg("--wasm-path");
        run_playground_test.addFileArg(playground_exe.getEmittedBin());
        // The runner cannot read the playground's version off its own
        // build_options: that prefix comes from the runner's own build mode, and
        // the two binaries are built at different optimize levels.
        run_playground_test.addArg("--playground-version");
        run_playground_test.addArg(compiler_version_override orelse
            compilerVersionForMode(b, playground_wasm_optimize, compiler_version_git));
        for (test_filters) |f| {
            run_playground_test.addArg("--filter");
            run_playground_test.addArg(f);
        }
        if (run_args.len != 0) run_playground_test.addArgs(run_args);
        run_playground_test.addPassthruArgs();
        run_playground_test.step.dependOn(&install.step);
        run_test_playground_step.dependOn(&run_playground_test.step);

        break :blk install;
    };

    // Add serialization size check
    // This verifies that Serialized types have the same size on 32-bit and 64-bit platforms
    // using compile-time assertions
    {
        // Build for native - will fail at compile time if sizes don't match expected
        const size_check_native = b.addExecutable(.{
            .name = "serialization_size_check_native",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/serialization_size_check.zig"),
                .target = target,
                .optimize = .debug,
            }),
        });
        configureBackend(size_check_native, target);
        roc_modules.addAll(size_check_native);

        // Build for wasm32 (32-bit) - will fail at compile time if sizes don't match expected
        const size_check_wasm32 = b.addExecutable(.{
            .name = "serialization_size_check_wasm32",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/serialization_size_check.zig"),
                .target = b.resolveTargetQuery(.{
                    .cpu_arch = .wasm32,
                    .os_tag = .freestanding,
                }),
                .optimize = .debug,
            }),
        });
        configureBackend(size_check_wasm32, b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
        }));
        size_check_wasm32.entry = .disabled;
        size_check_wasm32.rdynamic = true;
        roc_modules.addAll(size_check_wasm32);

        // Run the native version to confirm (wasm32 build is enough to verify 32-bit)
        const run_native = b.addRunArtifact(size_check_native);

        // The test passes if both executables build successfully (compile-time checks pass)
        // and the native one runs without error
        build_test_serialization_sizes_step.dependOn(&size_check_native.step);
        build_test_serialization_sizes_step.dependOn(&size_check_wasm32.step);
        run_test_serialization_sizes_step.dependOn(build_test_serialization_sizes_step);
        run_test_serialization_sizes_step.dependOn(&run_native.step);
    }

    // Build WASM static library fixture and test runner with bytebox.
    {
        const provided_callable_wasm_host = b.addObject(.{
            .name = "provided_callable_wasm_host",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/provided-callable-host/platform/wasm_host.zig"),
                .target = wasm32_resolved_target,
                .optimize = optimize,
                .strip = strip,
                .omit_frame_pointer = omit_frame_pointer,
                .pic = true,
            }),
        });
        configureBackend(provided_callable_wasm_host, wasm32_resolved_target);
        provided_callable_wasm_host.root_module.addImport("builtins", roc_modules.builtins);
        provided_callable_wasm_host.root_module.addImport("host_alloc", roc_modules.host_alloc);
        provided_callable_wasm_host.link_function_sections = true;
        provided_callable_wasm_host.link_data_sections = true;

        // Each app tree is staged with its platform's Roc modules and wasm
        // host, so every input a `roc build` below reads is a hashed input of
        // its cached step, and the built modules are declared outputs.
        const provided_callable_app_sources = b.addWriteFiles();
        _ = provided_callable_app_sources.addCopyDirectory(b.path("test/provided-callable-host"), ".", .{
            .include_extensions = &.{".roc"},
        });
        _ = provided_callable_app_sources.addCopyFile(provided_callable_wasm_host.getEmittedBin(), "platform/targets/wasm32/host.wasm");

        const wasm_app_sources = b.addWriteFiles();
        _ = wasm_app_sources.addCopyDirectory(b.path("test/wasm"), ".", .{
            .include_extensions = &.{".roc"},
        });
        _ = wasm_app_sources.addCopyFile(test_fixtures.output(wasm_host_step, "test/wasm/platform/targets/wasm32/host.wasm"), "platform/targets/wasm32/host.wasm");
        _ = wasm_app_sources.addCopyFile(test_fixtures.output(wasm_test_hosts.wasm32v1, "test/wasm/platform/targets/wasm32v1/host.wasm"), "platform/targets/wasm32v1/host.wasm");
        wasm_app_sources.step.dependOn(wasm_host_step);

        const build_wasm_provided_callable_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, provided_callable_app_sources, "app.roc", &.{}, "app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_provided_callable_app.run.step);

        const build_wasm_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "app.roc", &.{}, "app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_app.run.step);

        const build_wasm_list_builtin_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "list_builtin_static_lib_app.roc", &.{}, "list_builtin_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_list_builtin_app.run.step);

        const build_wasm_builtin_routing_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "builtin_routing_static_lib_app.roc", &.{"--opt=dev"}, "builtin_routing_static_lib_app.wasm.a");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_builtin_routing_app.run.step);
        const build_wasm_v1_builtin_routing_app = addWasmStaticLibAppBuildForTarget(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "builtin_routing_static_lib_app.roc", &.{"--opt=dev"}, "builtin_routing_static_lib_app_v1.wasm", "wasm32v1");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_v1_builtin_routing_app.run.step);

        const build_wasm_single_variant_hosted_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "single_variant_hosted_static_lib_app.roc", &.{"--opt=speed"}, "single_variant_hosted_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_single_variant_hosted_app.run.step);

        // Host ABI gate on wasm32: a hosted Try unwrapped with `?` into a wider
        // error row must still reach the host through its declared boundary,
        // which the cart shows by returning the host's own "ok".
        const build_wasm_hosted_try_widen_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "hosted_try_widen_static_lib_app.roc", &.{"--opt=dev"}, "hosted_try_widen_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_hosted_try_widen_app.run.step);

        const build_wasm_str_concat_join_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "str_concat_join_static_lib_app.roc", &.{"--opt=dev"}, "str_concat_join_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_str_concat_join_app.run.step);

        const build_wasm_issue_10957_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "issue_10957_json_camel_long_field_static_lib_app.roc", &.{"--opt=dev"}, "issue_10957_json_camel_long_field_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_issue_10957_app.run.step);

        const build_wasm_str_interp_leading_literal_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "str_interp_leading_literal_static_lib_app.roc", &.{"--opt=dev"}, "str_interp_leading_literal_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_str_interp_leading_literal_app.run.step);

        const build_wasm_str_concat_unique_reuse_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "str_concat_unique_reuse_static_lib_app.roc", &.{"--opt=dev"}, "str_concat_unique_reuse_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_str_concat_unique_reuse_app.run.step);

        // End-to-end cart gate for the minted-iterator `for`-loop drive. The
        // size build covers the LLVM cart path, and the dev build covers wasm
        // composite loop-state rebinding for recursive generated iterators.
        const build_wasm_iter_for_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "iter_for_static_lib_app.roc", &.{"--opt=size"}, "iter_for_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_iter_for_app.run.step);

        const build_wasm_iter_for_dev_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "iter_for_static_lib_app.roc", &.{"--opt=dev"}, "iter_for_static_lib_app_dev.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_iter_for_dev_app.run.step);

        // Dev-mode recursive iterator construction must converge at the
        // explicit forced-dynamic representation tier.
        const build_wasm_iter_recursive_concat_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "iter_recursive_concat_static_lib_app.roc", &.{"--opt=dev"}, "iter_recursive_concat_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_iter_recursive_concat_app.run.step);

        // Static-data hoisting gate: a constant list literal consumed via
        // `.iter()` must materialize as static data and allocate nothing, so the
        // whole minted chain (base list included) is zero-alloc on the cart path.
        const build_wasm_iter_list_hoist_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "iter_list_hoist_static_lib_app.roc", &.{"--opt=size"}, "iter_list_hoist_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_iter_list_hoist_app.run.step);

        // Noiter twin: the same sums over plain list literals. The runner prints
        // each cart's byte size, so the iter build minus this baseline is the
        // minted-adapter premium tracked in CI (the fusion pass's target).
        const build_wasm_iter_noiter_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "iter_for_noiter_static_lib_app.roc", &.{"--opt=size"}, "iter_for_noiter_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_iter_noiter_app.run.step);

        const build_wasm_rc_cleanup_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "rc_cleanup_static_lib_app.roc", &.{"--opt=dev"}, "rc_cleanup_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_rc_cleanup_app.run.step);

        const build_wasm_rc_cleanup_model_list_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "rc_cleanup_model_list_static_lib_app.roc", &.{"--opt=dev"}, "rc_cleanup_model_list_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_rc_cleanup_model_list_app.run.step);

        const build_wasm_box_zst_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "box_zst_static_lib_app.roc", &.{"--opt=dev"}, "box_zst_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_box_zst_app.run.step);

        const build_wasm_boxed_model_update_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "boxed_model_update_static_lib_app.roc", &.{"--opt=dev"}, "boxed_model_update_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_boxed_model_update_app.run.step);

        const build_wasm_issue_10836_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "issue_10836_boxed_low_alignment_static_lib_app.roc", &.{"--opt=dev"}, "issue_10836_boxed_low_alignment_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_issue_10836_app.run.step);

        // A constant record whose pointer fields are laid out in the opposite
        // order to their field order: its relocations must still reach the
        // object in offset order, or wasm-ld refuses it (#11419).
        const build_wasm_issue_11419_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "issue_11419_static_record_reloc_order_static_lib_app.roc", &.{"--opt=dev"}, "issue_11419_static_record_reloc_order_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_issue_11419_app.run.step);

        // Two `List.concat` calls over different refcounted element types keep
        // the element incref/decref callbacks indirect, so the generated RC
        // helpers must carry the callback ABI's signature exactly or wasm traps
        // at the `call_indirect` (#11454). Only the optimizing backend reaches
        // the indirect call, so this cart is built at `--opt=speed`.
        const build_wasm_issue_11454_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "issue_11454_concat_rc_callback_static_lib_app.roc", &.{"--opt=speed"}, "issue_11454_concat_rc_callback_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_issue_11454_app.run.step);

        // A boxed erased callable whose capture is refcounted: the helper in its
        // `Payload.on_drop` slot must carry the published host on-drop
        // signature, which wasm checks at the runtime's `call_indirect`.
        const build_wasm_on_drop_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "erased_callable_on_drop_static_lib_app.roc", &.{"--opt=speed"}, "erased_callable_on_drop_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_on_drop_app.run.step);

        // The same cart on the wasm backend, whose generated on-drop adapter is
        // a separate code path from the optimizing backend's.
        const build_wasm_on_drop_dev_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "erased_callable_on_drop_static_lib_app.roc", &.{"--opt=dev"}, "erased_callable_on_drop_dev_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_on_drop_dev_app.run.step);

        // A nominal tag union carrying `Box({})`, a box of a zero-sized
        // payload, as the payload of a multi-variant tag union matched at a
        // runtime value. The dev wasm backend must emit a module that
        // validates and runs (#11455).
        const build_wasm_issue_11455_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "issue_11455_boxed_zst_nominal_payload_static_lib_app.roc", &.{"--opt=dev"}, "issue_11455_boxed_zst_nominal_payload_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_issue_11455_app.run.step);

        const build_wasm_issue_11529_app = addWasmStaticLibAppBuild(b, roc_exe, build_roc_step, build_test_hosts_step, wasm_app_sources, "issue_11529_top_level_boxed_function_static_lib_app.roc", &.{ "--opt=dev", "--no-cache" }, "issue_11529_top_level_boxed_function_static_lib_app.wasm");
        build_test_wasm_static_lib_runner_step.dependOn(&build_wasm_issue_11529_app.run.step);

        const wasm_test_exe = b.addExecutable(.{
            .name = "wasm_static_lib_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/wasm/main.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(wasm_test_exe, target);
        wasm_test_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));
        wasm_test_exe.root_module.addImport("build_options", roc_modules.build_options);
        wasm_test_exe.root_module.addImport("shim_symbols", roc_modules.shim_symbols);

        const install = b.addInstallArtifact(wasm_test_exe, .{});
        build_test_wasm_static_lib_runner_step.dependOn(&install.step);

        const wasm_dce_check_exe = b.addExecutable(.{
            .name = "wasm_dce_check",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/archive/archive_check.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(wasm_dce_check_exe, target);
        const run_wasm_dce_check = b.addRunArtifact(wasm_dce_check_exe);
        run_wasm_dce_check.addFileArg(build_wasm_app.wasm);
        run_wasm_dce_check.addArgs(&.{
            "--absent",
            "roc_unused_host_canary_7f3a9c",
            "--absent",
            "roc_dead_private_helper_canary_41d2cb",
            "--absent",
            "ROC_DCE_WASM_DEAD_HOSTED_BLOB_0ac91d",
            "--absent",
            "ROC_DCE_WASM_DEAD_HELPER_BLOB_f25a77",
            "ROC_DCE_WASM_SHARED_BLOB_31e82f",
        });
        run_test_wasm_static_lib_step.dependOn(&run_wasm_dce_check.step);

        const run_wasm_test = b.addRunArtifact(wasm_test_exe);
        const run_wasm_issue_11529_test = b.addRunArtifact(wasm_test_exe);
        run_wasm_issue_11529_test.addArg("--wasm-path");
        run_wasm_issue_11529_test.addFileArg(build_wasm_issue_11529_app.wasm);
        run_wasm_issue_11529_test.addArgs(&.{
            "--expected",
            "x",
        });
        run_wasm_issue_11529_test.step.dependOn(&install.step);
        repro_issue_11529_step.dependOn(&run_wasm_issue_11529_test.step);
        run_wasm_test.addArg("--wasm-path");
        run_wasm_test.addFileArg(build_wasm_app.wasm);
        run_wasm_test.addPassthruArgs();
        if (run_args.len != 0) {
            run_wasm_test.addArgs(run_args);
        } else {
            run_test_wasm_static_lib_step.dependOn(&run_wasm_issue_11529_test.step);

            const run_wasm_provided_callable_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_provided_callable_test.addArg("--wasm-path");
            run_wasm_provided_callable_test.addFileArg(build_wasm_provided_callable_app.wasm);
            run_wasm_provided_callable_test.addArgs(&.{
                "--expected",
                "42",
                "--assert-alloc-balanced",
                "--min-allocs",
                "1",
            });
            run_wasm_provided_callable_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_provided_callable_test.step);

            const run_wasm_v1_builtin_routing_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_v1_builtin_routing_test.addArg("--wasm-path");
            run_wasm_v1_builtin_routing_test.addFileArg(build_wasm_v1_builtin_routing_app.wasm);
            run_wasm_v1_builtin_routing_test.addArgs(&.{ "--expected", "ok", "--assert-alloc-balanced" });
            run_wasm_v1_builtin_routing_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_v1_builtin_routing_test.step);

            const run_wasm_list_builtin_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_list_builtin_test.addArg("--wasm-path");
            run_wasm_list_builtin_test.addFileArg(build_wasm_list_builtin_app.wasm);
            run_wasm_list_builtin_test.addArgs(&.{
                "--expected",
                "ok",
            });
            run_wasm_list_builtin_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_list_builtin_test.step);

            const run_wasm_single_variant_hosted_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_single_variant_hosted_test.addArg("--wasm-path");
            run_wasm_single_variant_hosted_test.addFileArg(build_wasm_single_variant_hosted_app.wasm);
            run_wasm_single_variant_hosted_test.addArgs(&.{
                "--expected",
                "ok",
            });
            run_wasm_single_variant_hosted_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_single_variant_hosted_test.step);

            const run_wasm_hosted_try_widen_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_hosted_try_widen_test.addArg("--wasm-path");
            run_wasm_hosted_try_widen_test.addFileArg(build_wasm_hosted_try_widen_app.wasm);
            run_wasm_hosted_try_widen_test.addArgs(&.{
                "--expected",
                "ok",
            });
            run_wasm_hosted_try_widen_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_hosted_try_widen_test.step);

            const run_wasm_str_concat_join_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_str_concat_join_test.addArg("--wasm-path");
            run_wasm_str_concat_join_test.addFileArg(build_wasm_str_concat_join_app.wasm);
            run_wasm_str_concat_join_test.addArgs(&.{
                "--expected",
                "X:1Y:2",
                "--max-allocs",
                "0",
            });
            run_wasm_str_concat_join_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_str_concat_join_test.step);

            const run_wasm_issue_10957_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_issue_10957_test.addArg("--wasm-path");
            run_wasm_issue_10957_test.addFileArg(build_wasm_issue_10957_app.wasm);
            run_wasm_issue_10957_test.addArgs(&.{
                "--expected",
                "14",
            });
            run_wasm_issue_10957_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_issue_10957_test.step);

            const run_wasm_str_interp_leading_literal_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_str_interp_leading_literal_test.addArg("--wasm-path");
            run_wasm_str_interp_leading_literal_test.addFileArg(build_wasm_str_interp_leading_literal_app.wasm);
            run_wasm_str_interp_leading_literal_test.addArgs(&.{
                "--expected",
                "ok",
                "--assert-alloc-balanced",
                "--min-allocs",
                "1",
                "--max-allocs",
                "2",
            });
            run_wasm_str_interp_leading_literal_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_str_interp_leading_literal_test.step);

            const run_wasm_str_concat_unique_reuse_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_str_concat_unique_reuse_test.addArg("--wasm-path");
            run_wasm_str_concat_unique_reuse_test.addFileArg(build_wasm_str_concat_unique_reuse_app.wasm);
            run_wasm_str_concat_unique_reuse_test.addArgs(&.{
                "--expected",
                "ok",
                "--assert-alloc-balanced",
                "--min-allocs",
                "1",
                "--max-allocs",
                "1",
            });
            run_wasm_str_concat_unique_reuse_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_str_concat_unique_reuse_test.step);

            // Boot-and-play the minted-iterator `for`-loop cart; "ok" means every
            // inlined `for` over append/map/concat/chained minted chains ran to
            // completion with correct sums (i.e. the drive advanced its inner
            // iterators and terminated). `--assert-alloc-balanced` also catches a
            // per-step allocate/free leak in the drive.
            const run_wasm_iter_for_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_iter_for_test.addArg("--wasm-path");
            run_wasm_iter_for_test.addFileArg(build_wasm_iter_for_app.wasm);
            run_wasm_iter_for_test.addArgs(&.{
                "--expected",
                "ok",
                "--assert-alloc-balanced",
                // Absolute-size ceiling: the un-fused minted cart is ~48 KB
                // (premium ~18 KB over the noiter twin). This catches a gross
                // size blowup; the premium itself is read from the two printed
                // sizes and is the fusion pass's target, not a hard gate pre-fusion.
                "--max-bytes",
                "65536",
            });
            run_wasm_iter_for_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_iter_for_test.step);

            const run_wasm_iter_recursive_concat_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_iter_recursive_concat_test.addArg("--wasm-path");
            run_wasm_iter_recursive_concat_test.addFileArg(build_wasm_iter_recursive_concat_app.wasm);
            run_wasm_iter_recursive_concat_test.addArgs(&.{
                "--expected",
                "ok",
                "--assert-alloc-balanced",
                // With hidden defaultable evidence resolved at checked edges,
                // the fixed cart is ~723 KB. The prior unbounded generated
                // callable expansion exceeded 815 KB before failing to lower.
                "--max-bytes",
                "750000",
            });
            run_wasm_iter_recursive_concat_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_iter_recursive_concat_test.step);

            // Static-data hoisting: the constant list literal is materialized as
            // static data, so the whole chain allocates nothing. `--max-allocs 0`
            // is a strictly stronger assertion than `--assert-alloc-balanced`.
            const run_wasm_iter_list_hoist_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_iter_list_hoist_test.addArg("--wasm-path");
            run_wasm_iter_list_hoist_test.addFileArg(build_wasm_iter_list_hoist_app.wasm);
            run_wasm_iter_list_hoist_test.addArgs(&.{
                "--expected",
                "ok",
                "--max-allocs",
                "0",
            });
            run_wasm_iter_list_hoist_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_iter_list_hoist_test.step);

            const run_wasm_iter_for_dev_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_iter_for_dev_test.addArg("--wasm-path");
            run_wasm_iter_for_dev_test.addFileArg(build_wasm_iter_for_dev_app.wasm);
            run_wasm_iter_for_dev_test.addArgs(&.{
                "--expected",
                "ok",
                "--assert-alloc-balanced",
            });
            run_wasm_iter_for_dev_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_iter_for_dev_test.step);

            // Noiter twin—asserts correctness and prints its size so CI logs
            // carry both numbers for premium tracking.
            const run_wasm_iter_noiter_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_iter_noiter_test.addArg("--wasm-path");
            run_wasm_iter_noiter_test.addFileArg(build_wasm_iter_noiter_app.wasm);
            run_wasm_iter_noiter_test.addArgs(&.{
                "--expected",
                "ok",
            });
            run_wasm_iter_noiter_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_iter_noiter_test.step);

            const run_wasm_rc_cleanup_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_rc_cleanup_test.addArg("--wasm-path");
            run_wasm_rc_cleanup_test.addFileArg(build_wasm_rc_cleanup_app.wasm);
            run_wasm_rc_cleanup_test.addArgs(&.{
                "--expected",
                "ok",
                "--assert-alloc-balanced",
                "--min-allocs",
                "1",
            });
            run_wasm_rc_cleanup_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_rc_cleanup_test.step);

            const run_wasm_rc_cleanup_model_list_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_rc_cleanup_model_list_test.addArg("--wasm-path");
            run_wasm_rc_cleanup_model_list_test.addFileArg(build_wasm_rc_cleanup_model_list_app.wasm);
            run_wasm_rc_cleanup_model_list_test.addArgs(&.{
                "--expected",
                "ok",
                "--assert-alloc-balanced",
                "--min-allocs",
                "2",
            });
            run_wasm_rc_cleanup_model_list_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_rc_cleanup_model_list_test.step);

            const run_wasm_box_zst_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_box_zst_test.addArg("--wasm-path");
            run_wasm_box_zst_test.addFileArg(build_wasm_box_zst_app.wasm);
            run_wasm_box_zst_test.addArgs(&.{
                "--expected",
                "ok",
                "--assert-alloc-balanced",
            });
            run_wasm_box_zst_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_box_zst_test.step);

            const run_wasm_boxed_model_update_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_boxed_model_update_test.addArg("--wasm-path");
            run_wasm_boxed_model_update_test.addFileArg(build_wasm_boxed_model_update_app.wasm);
            run_wasm_boxed_model_update_test.addArgs(&.{
                "--expected",
                "ok",
            });
            run_wasm_boxed_model_update_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_boxed_model_update_test.step);

            const run_wasm_issue_10836_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_issue_10836_test.addArg("--wasm-path");
            run_wasm_issue_10836_test.addFileArg(build_wasm_issue_10836_app.wasm);
            run_wasm_issue_10836_test.addArgs(&.{
                "--expected",
                "7,9;11,13;Inc(42)",
            });
            run_wasm_issue_10836_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_issue_10836_test.step);

            const run_wasm_issue_11419_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_issue_11419_test.addArg("--wasm-path");
            run_wasm_issue_11419_test.addFileArg(build_wasm_issue_11419_app.wasm);
            run_wasm_issue_11419_test.addArgs(&.{
                "--expected",
                "id=txt",
            });
            run_wasm_issue_11419_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_issue_11419_test.step);

            const run_wasm_issue_11454_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_issue_11454_test.addArg("--wasm-path");
            run_wasm_issue_11454_test.addFileArg(build_wasm_issue_11454_app.wasm);
            run_wasm_issue_11454_test.addArgs(&.{
                "--expected",
                "{\"favoritesCount\":14} a, {\"favoritesCount\":14} b, {\"favoritesCount\":14} c, {\"favoritesCount\":14} d",
            });
            run_wasm_issue_11454_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_issue_11454_test.step);

            const run_wasm_on_drop_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_on_drop_test.addArg("--wasm-path");
            run_wasm_on_drop_test.addFileArg(build_wasm_on_drop_app.wasm);
            run_wasm_on_drop_test.addArgs(&.{
                "--expected",
                "{\"favoritesCount\":14} ok",
            });
            run_wasm_on_drop_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_on_drop_test.step);

            const run_wasm_on_drop_dev_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_on_drop_dev_test.addArg("--wasm-path");
            run_wasm_on_drop_dev_test.addFileArg(build_wasm_on_drop_dev_app.wasm);
            run_wasm_on_drop_dev_test.addArgs(&.{
                "--expected",
                "{\"favoritesCount\":14} ok",
            });
            run_wasm_on_drop_dev_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_on_drop_dev_test.step);

            const run_wasm_issue_11455_test = b.addRunArtifact(wasm_test_exe);
            run_wasm_issue_11455_test.addArg("--wasm-path");
            run_wasm_issue_11455_test.addFileArg(build_wasm_issue_11455_app.wasm);
            run_wasm_issue_11455_test.addArgs(&.{
                "--expected",
                "{\"favoritesCount\":14}",
            });
            run_wasm_issue_11455_test.step.dependOn(build_test_wasm_static_lib_runner_step);
            run_test_wasm_static_lib_step.dependOn(&run_wasm_issue_11455_test.step);
        }
        run_wasm_test.step.dependOn(build_test_wasm_static_lib_runner_step);
        run_test_wasm_static_lib_step.dependOn(&run_wasm_test.step);
    }

    // Build the shared-library test fixture with `roc build` and verify it by
    // running a separate loader executable that dlopens it and calls its C API.
    {
        const output_target = nativeSharedArchiveTarget(b, target);
        const dylib_ext = switch (roc_target.classifyOs(output_target.resolved.result.os.tag)) {
            .windows => ".dll",
            .macos => ".dylib",
            .linux, .freebsd, .openbsd, .netbsd, .other => ".so",
        };

        const build_dylib_app = b.addRunArtifact(roc_exe);
        build_dylib_app.addArgs(&.{
            "build",
            "--opt=size",
            b.fmt("--target={s}", .{output_target.roc_name}),
        });
        const dylib_sources = test_fixtures.cachedRoot(&.{build_test_hosts_step});
        build_dylib_app.addFileArg(dylib_sources.path(b, "test/dylib/app.roc"));
        const dylib_output = build_dylib_app.addPrefixedOutputFileArg("--output=", b.fmt("app{s}", .{dylib_ext}));

        const dylib_loader_exe = b.addExecutable(.{
            .name = "dylib_loader",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/dylib/loader.zig"),
                .target = output_target.resolved,
                .optimize = optimize,
                .link_libc = true,
            }),
        });
        configureBackend(dylib_loader_exe, output_target.resolved);

        const install_dylib_loader = b.addInstallArtifact(dylib_loader_exe, .{});

        const run_dylib_test = b.addRunArtifact(dylib_loader_exe);
        run_dylib_test.step.dependOn(&install_dylib_loader.step);
        run_dylib_test.addFileArg(dylib_output);
        run_dylib_test.step.dependOn(&build_dylib_app.step);
        run_test_dylib_step.dependOn(&run_dylib_test.step);

        // Dead host code must be dead-code-eliminated while live host code
        // survives, checked by byte-scanning for marker data blobs (data works
        // on every object format, unlike symbol names—PE retains no internal
        // symbol names): the dead-hosted and dead-only-helper blobs must be
        // absent, and the blob shared with the live Host.double! path must be
        // present. (That roc_run_app/roc_main are exported and the hidden
        // roc_host_double is not is checked separately by the loader.)
        const dylib_dce_check_exe = b.addExecutable(.{
            .name = "dylib_dce_check",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/archive/archive_check.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(dylib_dce_check_exe, target);
        const run_dylib_dce_check = b.addRunArtifact(dylib_dce_check_exe);
        run_dylib_dce_check.addFileArg(dylib_output);
        run_dylib_dce_check.addArgs(&.{
            "--absent",
            "ROC_DCE_CANARY_BLOB_7f3a9c",
            "--absent",
            "ROC_DCE_DEAD_HELPER_BLOB_28d0aa",
            "ROC_DCE_SHARED_BLOB_93e2c1",
        });
        run_dylib_dce_check.step.dependOn(&build_dylib_app.step);
        run_test_dylib_step.dependOn(&run_dylib_dce_check.step);
    }

    // Build the static-archive test fixture with `roc build`, link a consumer
    // executable against the produced archive, and run it. Also build the
    // hostless wasm32 archive and verify its contents.
    {
        const output_target = nativeSharedArchiveTarget(b, target);
        const archive_ext = if (output_target.resolved.result.os.tag == .windows) ".lib" else ".a";

        const build_archive_app = b.addRunArtifact(roc_exe);
        build_archive_app.addArgs(&.{
            "build",
            "--opt=dev",
            b.fmt("--target={s}", .{output_target.roc_name}),
        });
        const archive_sources = test_fixtures.cachedRoot(&.{build_test_hosts_step});
        build_archive_app.addFileArg(archive_sources.path(b, "test/archive/app.roc"));
        const archive_output = build_archive_app.addPrefixedOutputFileArg("--output=", b.fmt("app{s}", .{archive_ext}));

        const archive_consumer_exe = b.addExecutable(.{
            .name = "archive_consumer",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/archive/consumer.zig"),
                .target = output_target.resolved,
                .optimize = optimize,
                .link_libc = true,
                // A COFF /DEBUG link disables lld-link's /OPT:REF default, so
                // an unstripped Windows Debug build would keep the DCE canary
                // sections this test asserts are eliminated.
                .strip = true,
            }),
        });
        configureBackend(archive_consumer_exe, output_target.resolved);
        archive_consumer_exe.root_module.addObjectFile(archive_output);
        archive_consumer_exe.step.dependOn(&build_archive_app.step);

        archive_consumer_exe.link_gc_sections = true;

        const run_archive_consumer = b.addRunArtifact(archive_consumer_exe);
        run_test_archive_step.dependOn(&run_archive_consumer.step);

        const build_wasm_archive_app = b.addRunArtifact(roc_exe);
        build_wasm_archive_app.addArgs(&.{
            "build",
            "--opt=dev",
            "--target=wasm32",
        });
        build_wasm_archive_app.addFileArg(archive_sources.path(b, "test/archive/app.roc"));
        const wasm_archive_output = build_wasm_archive_app.addPrefixedOutputFileArg("--output=", "app-wasm32.a");

        const archive_check_exe = b.addExecutable(.{
            .name = "archive_check",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/archive/archive_check.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(archive_check_exe, target);

        const run_native_archive_dce_check = b.addRunArtifact(archive_check_exe);
        run_native_archive_dce_check.addFileArg(archive_consumer_exe.getEmittedBin());
        run_native_archive_dce_check.addArgs(&.{
            "--absent",
            "ROC_DCE_CANARY_BLOB_7f3a9c",
            "--absent",
            "ROC_DCE_DEAD_HELPER_BLOB_28d0aa",
            "ROC_DCE_SHARED_BLOB_93e2c1",
        });
        run_native_archive_dce_check.step.dependOn(&archive_consumer_exe.step);
        run_test_archive_step.dependOn(&run_native_archive_dce_check.step);

        const run_wasm_archive_check = b.addRunArtifact(archive_check_exe);
        run_wasm_archive_check.addArg("--archive");
        run_wasm_archive_check.addFileArg(wasm_archive_output);
        run_wasm_archive_check.addArg("roc_builtins");
        run_wasm_archive_check.step.dependOn(&build_wasm_archive_app.step);
        run_test_archive_step.dependOn(&run_wasm_archive_check.step);

        // The same app through the LLVM backend: `llvmObjectUsesPic` decides
        // PIC for that object, so the dev-backend archive above cannot cover it.
        const build_wasm_archive_app_llvm = b.addRunArtifact(roc_exe);
        build_wasm_archive_app_llvm.addArgs(&.{
            "build",
            "--opt=speed",
            "--target=wasm32",
        });
        build_wasm_archive_app_llvm.addFileArg(archive_sources.path(b, "test/archive/app.roc"));
        const wasm_archive_llvm_output = build_wasm_archive_app_llvm.addPrefixedOutputFileArg("--output=", "app-wasm32-speed.a");

        // A wasm32 archive is handed to a foreign linker, so it must contain no
        // absolute data/table relocations or emcc cannot build a SIDE_MODULE
        // from it. Checked for both backends that produce these objects.
        const wasm_pic_check_exe = b.addExecutable(.{
            .name = "wasm_pic_check",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/archive/wasm_pic_check.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(wasm_pic_check_exe, target);

        const run_wasm_pic_check_dev = b.addRunArtifact(wasm_pic_check_exe);
        run_wasm_pic_check_dev.addFileArg(wasm_archive_output);
        run_wasm_pic_check_dev.step.dependOn(&build_wasm_archive_app.step);
        run_test_archive_step.dependOn(&run_wasm_pic_check_dev.step);

        const run_wasm_pic_check_llvm = b.addRunArtifact(wasm_pic_check_exe);
        run_wasm_pic_check_llvm.addFileArg(wasm_archive_llvm_output);
        run_wasm_pic_check_llvm.step.dependOn(&build_wasm_archive_app_llvm.step);
        run_test_archive_step.dependOn(&run_wasm_pic_check_llvm.step);
    }

    // Check fx platform test coverage convenience step
    const check_test_asset_coverage_inner = CheckTestAssetCoverageStep.create(b);
    run_check_test_asset_coverage_step.dependOn(&check_test_asset_coverage_inner.step);

    const stack_overflow_test_helper_exe = b.addExecutable(.{
        .name = "stack_overflow_test_helper",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/stack_overflow_test_helper.zig"),
            .target = target,
            .optimize = optimize,
            // stack_overflow.zig uses std.c.write/fork/pipe; Zig 0.16 requires explicit link_libc.
            .link_libc = true,
        }),
    });
    stack_overflow_test_helper_exe.root_module.addImport("base", roc_modules.base);
    stack_overflow_test_helper_exe.root_module.addImport("sljmp", roc_modules.sljmp);
    roc_modules.addModuleDependencies(stack_overflow_test_helper_exe, .base);
    const install_stack_overflow_test_helper = b.addInstallArtifact(stack_overflow_test_helper_exe, .{});
    const stack_overflow_test_options = b.addOptions();
    stack_overflow_test_options.addOptionPathUntracked("helper_path", stack_overflow_test_helper_exe.getEmittedBin());
    const stack_overflow_test_options_module = stack_overflow_test_options.createModule();
    build_test_zig_step.dependOn(&install_stack_overflow_test_helper.step);

    // Create and add module tests
    const module_tests_result = roc_modules.createModuleTests(b, target, optimize, zstd, test_filters);
    const tests_summary = TestsSummaryStep.create(b, test_filters, module_tests_result.forced_passes);
    if (builtin.os.tag == .windows) {
        // Zig 0.16's Windows test runner IPC can time out while many Roc test
        // binaries are starting at once. Keep the same tests, but start them
        // in a deterministic order.
        //
        // Only the summary's own runs are chained. The public run-test-zig-*
        // steps get separate, unchained runs via `TestSuiteRegistry`, so asking
        // for one suite never drags in the whole chain -- see the note there.
        tests_summary.setRunSerialization();
    }

    const test_suites = TestSuiteRegistry{
        .b = b,
        .summary = tests_summary,
        .build_test_zig_step = build_test_zig_step,
        .unit_test_runner = unit_test_runner,
        .wiring_run = if (enumerate_tests_for_wiring_check) run_test_wiring else null,
        .run_args = run_args,
    };

    if (main_exe_result.machine_code_shim_test) |machine_code_shim_test| {
        test_suites.register(.{
            .step_suffix = "machine-code-shim",
            .description = "Run machine-code shim Zig tests",
            .compile = machine_code_shim_test,
        });
    }

    for (main_exe_result.boundary_link_tests, 0..) |maybe_test, index| {
        if (maybe_test) |boundary_test| test_suites.register(.{
            .step_suffix = if (index == 0) "machine-code-boundary-link" else "interpreter-boundary-link",
            .description = "Link canonical symbols through a real shim archive",
            .compile = boundary_test,
        });
    }

    if (main_exe_result.machine_code_shim_archive_test) |archive_test| {
        test_suites.register(.{
            .step_suffix = "machine-code-shim-archive",
            .description = "Run machine-code shim archive contract tests",
            .compile = archive_test,
        });
    }

    if (main_exe_result.archive_member_names_test) |names_test| {
        test_suites.register(.{
            .step_suffix = "archive-member-names",
            .description = "Run archive member-name rewriting tests",
            .compile = names_test,
        });
    }

    if (main_exe_result.machine_code_shim_archive_check) |machine_code_shim_archive_check| {
        run_check_machine_code_shim_archive_step.dependOn(machine_code_shim_archive_check);
    }

    const guarded_list_violation_exe = b.addExecutable(.{
        .name = "guarded_list_violation_test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/collections/guarded_list_violation_test.zig"),
            .target = target,
            .optimize = .debug,
            .link_libc = true,
        }),
    });
    guarded_list_violation_exe.root_module.addImport("collections", roc_modules.collections);
    guarded_list_violation_exe.root_module.addImport("check", roc_modules.check);
    guarded_list_violation_exe.root_module.addImport("layout", roc_modules.layout);
    guarded_list_violation_exe.root_module.addImport("lir", roc_modules.lir);
    guarded_list_violation_exe.root_module.addImport("postcheck", roc_modules.postcheck);

    // The RcEffect structural validator promises that a row whose fields
    // contradict each other is a compile error. This probe is a file that
    // makes such a row; the build requires compiling it to fail, and to fail
    // with the rule the row breaks.
    const rc_effect_rejected_row_probe = b.addObject(.{
        .name = "rc_effect_rejected_row_probe",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/base/rc_effect_rejected_row_probe.zig"),
            .target = target,
            .optimize = .debug,
        }),
    });
    rc_effect_rejected_row_probe.root_module.addImport("base", roc_modules.base);
    rc_effect_rejected_row_probe.expect_errors = .{
        .contains = "[rule: unique_result_without_source]",
    };

    const run_rc_effect_rejected_row_step = b.step(
        "run-test-rc-effect-rejected-row",
        "Check that a structurally invalid RcEffect row fails to compile",
    );
    run_rc_effect_rejected_row_step.dependOn(&rc_effect_rejected_row_probe.step);
    run_test_zig_step.dependOn(run_rc_effect_rejected_row_step);

    const run_guarded_list_violations_step = b.step(
        "run-test-guarded-list-violations",
        "Run guarded-list expected-failure checks",
    );
    const guarded_list_violation_cases = [_]struct {
        name: []const u8,
        list_name: []const u8,
    }{
        .{ .name = "span_append_move", .list_name = "guarded_list_violation_test.values" },
        .{ .name = "ptr_append_move", .list_name = "guarded_list_violation_test.values" },
        .{ .name = "span_ensure_move", .list_name = "guarded_list_violation_test.values" },
        .{ .name = "span_append_slice_move", .list_name = "guarded_list_violation_test.values" },
        .{ .name = "span_restore_below_range", .list_name = "guarded_list_violation_test.values" },
        .{ .name = "ptr_restore_below_index", .list_name = "guarded_list_violation_test.values" },
        .{ .name = "span_clear", .list_name = "guarded_list_violation_test.values" },
        .{ .name = "span_ownership_transfer", .list_name = "guarded_list_violation_test.values" },
        .{ .name = "lir_proc_specs", .list_name = "LirStore.proc_specs" },
        .{ .name = "lir_local_span", .list_name = "LirStore.local_ids" },
        .{ .name = "lifted_fns", .list_name = "monotype_lifted.Program.fns" },
        .{ .name = "lifted_expr_ids", .list_name = "monotype_lifted.Program.expr_ids" },
        .{ .name = "mono_exprs", .list_name = "monotype.Program.exprs" },
        .{ .name = "mono_type_spans", .list_name = "monotype.Type.Store.spans" },
        .{ .name = "mono_type_fields", .list_name = "monotype.Type.Store.fields" },
        .{ .name = "lambda_mono_expr_ids", .list_name = "lambda_mono.Program.expr_ids" },
        .{ .name = "lambda_mono_type_spans", .list_name = "lambda_mono.Type.Store.spans" },
    };
    for (guarded_list_violation_cases) |case| {
        const run_violation = b.addRunArtifact(guarded_list_violation_exe);
        run_violation.addArg(case.name);
        run_violation.expectStdErrMatch(b.fmt("guarded list invalidated: {s}", .{case.list_name}));
        run_guarded_list_violations_step.dependOn(&run_violation.step);
    }
    build_test_zig_step.dependOn(&guarded_list_violation_exe.step);
    run_test_zig_step.dependOn(run_guarded_list_violations_step);

    for (module_tests_result.tests) |module_test| {
        // Add compiled builtins to tests that canonicalize ordinary modules.
        if (std.mem.eql(u8, module_test.test_step.name, "can") or std.mem.eql(u8, module_test.test_step.name, "check") or std.mem.eql(u8, module_test.test_step.name, "eval") or std.mem.eql(u8, module_test.test_step.name, "compile") or std.mem.eql(u8, module_test.test_step.name, "lsp") or std.mem.eql(u8, module_test.test_step.name, "lsp_unit") or std.mem.eql(u8, module_test.test_step.name, "lsp_integration")) {
            module_test.test_step.root_module.addImport("compiled_builtins", compiled_builtins_module);
            module_test.test_step.step.dependOn(&write_compiled_builtins.step);
        }

        if (std.mem.eql(u8, module_test.test_step.name, "repl")) {
            module_test.test_step.root_module.addImport("bytebox", bytebox.module("bytebox"));
        }

        if (std.mem.eql(u8, module_test.test_step.name, "base")) {
            module_test.test_step.root_module.addImport("stack_overflow_test_options", stack_overflow_test_options_module);
        }

        // Compile tests lower real apps to LIR and hand the result to the LLVM
        // backend to assert on what it emits. Building the LLVM module is pure
        // Zig (the vendored IR builder), so no LLVM library linkage is needed.
        if (std.mem.eql(u8, module_test.test_step.name, "compile")) {
            module_test.test_step.root_module.addImport("llvm_codegen", llvm_codegen_module);
            module_test.test_step.root_module.addImport("postcheck", roc_modules.postcheck);
        }

        if (std.mem.eql(u8, module_test.test_step.name, "glue")) {
            const has_llvm = try addLlvmLinkSupportToStep(
                b,
                module_test.test_step,
                target,
                dependency_source,
                llvm_codegen_module,
                zstd,
            );
            if (has_llvm) {
                module_test.test_step.root_module.addImport("llvm_compile", llvm_compile_module);
            }
        }

        // Add bytebox and wasm32 builtins to eval tests for wasm backend testing
        if (std.mem.eql(u8, module_test.test_step.name, "eval")) {
            module_test.test_step.root_module.addImport("bytebox", bytebox.module("bytebox"));
            module_test.test_step.root_module.addImport("wasm32_boxy_runtime", wasm32_boxy_runtime_module);
            module_test.test_step.root_module.addImport("wasm32_builtins", wasm32_builtins_module);
            const compile_build_module = b.createModule(.{
                .root_source_file = b.path("src/compile/compile_build.zig"),
            });
            compile_build_module.addImport("tracy", roc_modules.tracy);
            compile_build_module.addImport("build_options", roc_modules.build_options);
            compile_build_module.addImport("ctx", roc_modules.ctx);
            compile_build_module.addImport("builtins", roc_modules.builtins);
            compile_build_module.addImport("collections", roc_modules.collections);
            compile_build_module.addImport("base", roc_modules.base);
            compile_build_module.addImport("types", roc_modules.types);
            compile_build_module.addImport("parse", roc_modules.parse);
            compile_build_module.addImport("can", roc_modules.can);
            compile_build_module.addImport("check", roc_modules.check);
            compile_build_module.addImport("reporting", roc_modules.reporting);
            compile_build_module.addImport("layout", roc_modules.layout);
            compile_build_module.addImport("eval", module_test.test_step.root_module);
            compile_build_module.addImport("unbundle", roc_modules.unbundle);
            compile_build_module.addImport("roc_target", roc_modules.roc_target);
            compile_build_module.addImport("compiled_builtins", compiled_builtins_module);
            compile_build_module.addImport("compiler_platform_sources", roc_modules.compiler_platform_sources);
            module_test.test_step.root_module.addImport("compile_build", compile_build_module);
            try addLlvmSupportToStep(
                b,
                module_test.test_step,
                target,
                dependency_source,
                roc_modules,
                llvm_codegen_module,
                llvm_embedded_module,
                zstd,
            );
        }

        // Backend tests need the wasm host object and builtins for WASM linking tests
        if (std.mem.eql(u8, module_test.test_step.name, "backend")) {
            module_test.test_step.step.dependOn(wasm_host_step);
            module_test.test_step.step.dependOn(&wasm_host_fixture_files.step);
            module_test.test_step.root_module.addImport("wasm_host_fixture", wasm_host_fixture_module);
            module_test.test_step.root_module.addImport("wasm32_builtins", wasm32_builtins_module);
            module_test.test_step.root_module.addImport("bytebox", bytebox.module("bytebox"));
        }

        if (std.mem.eql(u8, module_test.test_step.name, "repl")) {
            try addLlvmSupportToStep(
                b,
                module_test.test_step,
                target,
                dependency_source,
                roc_modules,
                llvm_codegen_module,
                llvm_embedded_module,
                zstd,
            );
        }

        const test_exe_name = module_test.test_step.name;

        // The `base` module's tests exec a helper binary, so it must be
        // installed first. Hoisted into an explicitly typed local because
        // peer-type resolution on an inline conditional slice literal inside a
        // struct literal field is fragile.
        const module_deps: []const *Step = if (std.mem.eql(u8, test_exe_name, "base"))
            &.{&install_stack_overflow_test_helper.step}
        else
            &.{};

        test_suites.register(.{
            .step_suffix = b.fmt("module-{s}", .{test_exe_name}),
            .description = b.fmt("Run {s} Zig module tests", .{test_exe_name}),
            .compile = module_test.test_step,
            .deps = module_deps,
            .default_step = true,
        });
    }

    const lsp_integration_test_harness_module = createTestHarnessModule(b, roc_modules);
    const lsp_integration_runner_exe = b.addExecutable(.{
        .name = "parallel_lsp_integration_runner",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/lsp/test/parallel_integration_runner.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .imports = &.{
                .{ .name = "test_harness", .module = lsp_integration_test_harness_module },
                .{ .name = "integration_specs", .module = roc_modules.lsp_integration },
            },
        }),
    });
    add_tracy(b, roc_modules.build_options, lsp_integration_runner_exe, target, false, flag_enable_tracy);
    lsp_integration_runner_exe.step.dependOn(&write_compiled_builtins.step);
    build_test_lsp_integration_runner_step.dependOn(&lsp_integration_runner_exe.step);

    const run_lsp_integration = b.addRunArtifact(lsp_integration_runner_exe);
    run_lsp_integration.stdio = .inherit;
    for (test_filters) |filter| {
        run_lsp_integration.addArg("--filter");
        run_lsp_integration.addArg(filter);
    }
    if (run_args.len != 0) run_lsp_integration.addArgs(run_args);
    run_lsp_integration.addPassthruArgs();

    const run_lsp_integration_step = b.step(
        "run-test-zig-module-lsp_integration",
        "Run LSP integration tests in parallel",
    );
    run_lsp_integration_step.dependOn(&run_lsp_integration.step);

    b.default_step.dependOn(&lsp_integration_runner_exe.step);
    build_test_zig_step.dependOn(&lsp_integration_runner_exe.step);
    run_test_zig_step.dependOn(&run_lsp_integration.step);

    // Build-helper unit tests: test_harness.zig and stack_probe.zig are only
    // consumed as module imports by executables, so they need their own test root.
    const build_helpers_test = b.addTest(.{
        .name = "build_helpers",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/helpers_test_root.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .imports = &.{
                .{ .name = "collections", .module = roc_modules.collections },
                .{ .name = "build_options", .module = roc_modules.build_options },
            },
        }),
        .filters = test_filters,
    });
    test_suites.register(.{
        .step_suffix = "build-helpers",
        .description = "Run build-helper Zig unit tests",
        .compile = build_helpers_test,
    });

    // CLI runner unit tests: parallel_cli_runner.zig is an executable root, so
    // its test decls are only collected by this dedicated test compile.
    const cli_runner_unit_test = b.addTest(.{
        .name = "cli_runner_unit",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/cli/test/parallel_cli_runner.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .imports = &.{
                .{ .name = "base", .module = roc_modules.base },
                .{ .name = "test_harness", .module = createTestHarnessModule(b, roc_modules) },
                .{ .name = "collections", .module = roc_modules.collections },
                .{ .name = "backend", .module = roc_modules.backend },
                .{ .name = "builtins", .module = roc_modules.builtins },
                .{ .name = "bytebox", .module = bytebox.module("bytebox") },
                .{ .name = "build_options", .module = roc_modules.build_options },
            },
        }),
        .filters = test_filters,
    });
    cli_runner_unit_test.root_module.addOptions("cli_test_options", cli_test_options);
    test_suites.register(.{
        .step_suffix = "cli-runner-unit",
        .description = "Run CLI runner Zig unit tests",
        .compile = cli_runner_unit_test,
    });

    // Tidy unit tests: ci/tidy.zig is an executable root, so like
    // parallel_cli_runner.zig above its test decls need a dedicated test compile.
    const tidy_unit_test = b.addTest(.{
        .name = "tidy_unit",
        .root_module = b.createModule(.{
            .root_source_file = b.path("ci/tidy.zig"),
            .target = target,
            .optimize = optimize,
        }),
        .filters = test_filters,
    });
    test_suites.register(.{
        .step_suffix = "tidy-unit",
        .description = "Run tidy Zig unit tests",
        .compile = tidy_unit_test,
    });

    // LLVM backend aggregator test: src/backend/llvm/mod.zig is not the root of
    // the llvm_codegen module, so its refAllDecls compile coverage needs its own
    // test compile.
    const backend_llvm_test = b.addTest(.{
        .name = "backend_llvm",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/backend/llvm/mod.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .imports = &.{
                .{ .name = "base", .module = roc_modules.base },
                .{ .name = "backend", .module = roc_modules.backend },
                .{ .name = "layout", .module = roc_modules.layout },
                .{ .name = "lir", .module = roc_modules.lir },
                .{ .name = "ctx", .module = roc_modules.ctx },
                .{ .name = "builtins", .module = roc_modules.builtins },
                .{ .name = "build_options", .module = roc_modules.build_options },
                .{ .name = "roc_target", .module = roc_modules.roc_target },
                .{ .name = "vendor_llvm_ir", .module = roc_modules.vendor_llvm_ir },
            },
        }),
        .filters = test_filters,
    });
    try addLlvmSupportToStep(
        b,
        backend_llvm_test,
        target,
        dependency_source,
        roc_modules,
        llvm_codegen_module,
        llvm_embedded_module,
        zstd,
    );
    if (backend_llvm_test.root_module.resolved_target.?.result.os.tag != .windows or
        backend_llvm_test.root_module.resolved_target.?.result.abi != .msvc)
    {
        backend_llvm_test.root_module.link_libcpp = true;
    }
    test_suites.register(.{
        .step_suffix = "backend-llvm",
        .description = "Run LLVM backend aggregator Zig tests",
        .compile = backend_llvm_test,
    });

    // Add snapshot tool test
    const enable_snapshot_tests = b.option(bool, "snapshot-tests", "Enable snapshot tests") orelse true;
    if (enable_snapshot_tests) {
        const snapshot_test = b.addTest(.{
            .name = "snapshot_tool_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/snapshot_tool/main.zig"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            }),
            .filters = test_filters,
        });
        roc_modules.addAll(snapshot_test);
        snapshot_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
        snapshot_test.step.dependOn(&write_compiled_builtins.step);
        try addLlvmSupportToStep(
            b,
            snapshot_test,
            target,
            dependency_source,
            roc_modules,
            llvm_codegen_module,
            llvm_embedded_module,
            zstd,
        );
        if (snapshot_test.root_module.resolved_target.?.result.os.tag != .windows or
            snapshot_test.root_module.resolved_target.?.result.abi != .msvc)
        {
            snapshot_test.root_module.link_libcpp = true;
        }

        add_tracy(b, roc_modules.build_options, snapshot_test, target, true, flag_enable_tracy);

        // The install step is optional, so hoist the dep list into a local with
        // an explicit type: peer-type resolution on an inline
        // `if (c) &.{x} else &.{}` inside a struct literal field is fragile.
        const snapshot_deps: []const *Step = if (snapshot_exe_install) |install|
            &.{install}
        else
            &.{};

        test_suites.register(.{
            .step_suffix = "snapshot-tool",
            .description = "Run snapshot tool Zig tests",
            .compile = snapshot_test,
            .deps = snapshot_deps,
        });
    }

    // Add Builtin.roc doc code-block tests. Verifies every ```roc block in
    // src/build/roc/Builtin.roc passes the in-memory equivalent of
    // `roc check`, and either runs as `roc test` (when the block is only
    // top-level expects) or evaluates.
    const enable_builtin_doc_tests = b.option(bool, "builtin-doc-tests", "Enable Builtin.roc doc code-block tests") orelse true;
    if (enable_builtin_doc_tests) {
        const builtin_doc_test = b.addTest(.{
            .name = "builtin_doc_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/eval/test/builtin_doc_tests.zig"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            }),
            .filters = test_filters,
        });
        builtin_doc_test.root_module.addOptions("fixture_options", fixture_options);
        roc_modules.addAll(builtin_doc_test);
        builtin_doc_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
        builtin_doc_test.step.dependOn(&write_compiled_builtins.step);
        try addLlvmSupportToStep(
            b,
            builtin_doc_test,
            target,
            dependency_source,
            roc_modules,
            llvm_codegen_module,
            llvm_embedded_module,
            zstd,
        );
        if (builtin_doc_test.root_module.resolved_target.?.result.os.tag != .windows or
            builtin_doc_test.root_module.resolved_target.?.result.abi != .msvc)
        {
            builtin_doc_test.root_module.link_libcpp = true;
        }
        add_tracy(b, roc_modules.build_options, builtin_doc_test, target, true, flag_enable_tracy);

        test_suites.register(.{
            .step_suffix = "builtin-doc",
            .description = "Run Builtin.roc doc code-block Zig tests",
            .compile = builtin_doc_test,
            .fixture_root = true,
        });
    }

    const lir_inline_test = b.addTest(.{
        .name = "lir_inline_test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/eval/test/lir_inline_test.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
        .filters = test_filters,
    });
    lir_inline_test.stack_size = stack_budget.roc_stack_size;
    roc_modules.addAll(lir_inline_test);
    lir_inline_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
    lir_inline_test.step.dependOn(&write_compiled_builtins.step);
    try addLlvmSupportToStep(
        b,
        lir_inline_test,
        target,
        dependency_source,
        roc_modules,
        llvm_codegen_module,
        llvm_embedded_module,
        zstd,
    );
    if (lir_inline_test.root_module.resolved_target.?.result.os.tag != .windows or
        lir_inline_test.root_module.resolved_target.?.result.abi != .msvc)
    {
        lir_inline_test.root_module.link_libcpp = true;
    }
    add_tracy(b, roc_modules.build_options, lir_inline_test, target, true, flag_enable_tracy);

    test_suites.register(.{
        .step_suffix = "lir-inline",
        .description = "Run LIR inline Zig tests",
        .compile = lir_inline_test,
    });

    const boxy_abi_test = b.addTest(.{
        .name = "boxy_abi_test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/eval/test/boxy_abi_test.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
        .filters = test_filters,
    });
    roc_modules.addAll(boxy_abi_test);
    boxy_abi_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
    boxy_abi_test.step.dependOn(&write_compiled_builtins.step);
    add_tracy(b, roc_modules.build_options, boxy_abi_test, target, true, flag_enable_tracy);
    test_suites.register(.{
        .step_suffix = "boxy-abi",
        .description = "Run boxy C-ABI wrapper Zig tests",
        .compile = boxy_abi_test,
    });

    const rc_conformance_test = b.addTest(.{
        .name = "rc_conformance_test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/eval/test/rc_conformance_tests.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
        .filters = test_filters,
    });
    roc_modules.addAll(rc_conformance_test);
    rc_conformance_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
    rc_conformance_test.step.dependOn(&write_compiled_builtins.step);
    try addLlvmSupportToStep(
        b,
        rc_conformance_test,
        target,
        dependency_source,
        roc_modules,
        llvm_codegen_module,
        llvm_embedded_module,
        zstd,
    );
    if (rc_conformance_test.root_module.resolved_target.?.result.os.tag != .windows or
        rc_conformance_test.root_module.resolved_target.?.result.abi != .msvc)
    {
        rc_conformance_test.root_module.link_libcpp = true;
    }
    add_tracy(b, roc_modules.build_options, rc_conformance_test, target, true, flag_enable_tracy);

    test_suites.register(.{
        .step_suffix = "rc-conformance",
        .description = "Run RcEffect conformance sweep Zig tests",
        .compile = rc_conformance_test,
    });

    const trmc_lir_test = b.addTest(.{
        .name = "trmc_lir_test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/eval/test/trmc_lir_test.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
        .filters = test_filters,
    });
    // Drives the interpreter thousands of frames deep to prove the Debug
    // call-depth guard fires first, which needs the same native stack the other
    // interpreter-driving binaries get; otherwise the stack runs out before the
    // guard does and the fault replaces the deterministic crash under test.
    trmc_lir_test.stack_size = stack_budget.roc_stack_size;
    roc_modules.addAll(trmc_lir_test);
    trmc_lir_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
    trmc_lir_test.step.dependOn(&write_compiled_builtins.step);
    try addLlvmSupportToStep(
        b,
        trmc_lir_test,
        target,
        dependency_source,
        roc_modules,
        llvm_codegen_module,
        llvm_embedded_module,
        zstd,
    );
    if (trmc_lir_test.root_module.resolved_target.?.result.os.tag != .windows or
        trmc_lir_test.root_module.resolved_target.?.result.abi != .msvc)
    {
        trmc_lir_test.root_module.link_libcpp = true;
    }
    add_tracy(b, roc_modules.build_options, trmc_lir_test, target, true, flag_enable_tracy);

    test_suites.register(.{
        .step_suffix = "trmc-lir",
        .description = "Run TRMC LIR Zig tests",
        .compile = trmc_lir_test,
    });

    const cli_io_writer_test_helper = b.addExecutable(.{
        .name = "cli_io_writer_test_helper",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/cli/io_writer_test_helper.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    cli_io_writer_test_helper.root_module.addImport("reporting", roc_modules.reporting);
    cli_io_writer_test_helper.root_module.addImport("ctx", roc_modules.ctx);
    const install_cli_io_writer_test_helper = b.addInstallArtifact(cli_io_writer_test_helper, .{});
    const cli_test_helpers = b.addOptions();
    cli_test_helpers.addOptionPathUntracked("cli_io_writer_test_helper_path", cli_io_writer_test_helper.getEmittedBin());

    // Add CLI test
    const enable_cli_tests = b.option(bool, "cli-tests", "Enable cli tests") orelse true;
    if (enable_cli_tests) {
        const cli_test = b.addTest(.{
            .name = "cli_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/cli/main.zig"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            }),
            .filters = test_filters,
        });
        cli_test.root_module.addImport("compiler_version", compiler_version_module);
        cli_test.root_module.addOptions("cli_test_helpers", cli_test_helpers);
        roc_modules.addAll(cli_test);
        linkWatchPlatformLibs(cli_test, target);
        cli_test.root_module.linkLibrary(zstd.artifact("zstd"));
        try addLlvmSupportToStep(
            b,
            cli_test,
            target,
            dependency_source,
            roc_modules,
            llvm_codegen_module,
            llvm_embedded_module,
            zstd,
        );
        if (cli_test.root_module.resolved_target.?.result.os.tag != .windows or
            cli_test.root_module.resolved_target.?.result.abi != .msvc)
        {
            cli_test.root_module.link_libcpp = true;
        }
        add_tracy(b, roc_modules.build_options, cli_test, target, true, flag_enable_tracy);
        cli_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
        cli_test.step.dependOn(&write_compiled_builtins.step);

        test_suites.register(.{
            .step_suffix = "cli-main",
            .description = "Run roc CLI main Zig tests",
            .compile = cli_test,
            .deps = &.{&install_cli_io_writer_test_helper.step},
        });
    }

    // Add watch tests
    const enable_watch_tests = b.option(bool, "watch-tests", "Enable watch tests") orelse true;
    if (enable_watch_tests) {
        const watch_test = b.addTest(.{
            .name = "watch_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/watch/watch.zig"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            }),
            .filters = test_filters,
        });
        roc_modules.addAll(watch_test);
        add_tracy(b, roc_modules.build_options, watch_test, target, false, flag_enable_tracy);

        // Link platform-specific libraries for file watching
        linkWatchPlatformLibs(watch_test, target);

        test_suites.register(.{
            .step_suffix = "watch-cli",
            .description = "Run watch command Zig tests",
            .compile = watch_test,
        });
    }

    // MiniCI output-filter tests.
    const minici_compile = b.addTest(.{
        .name = "minici_test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/minici.zig"),
            .target = b.graph.host,
            .optimize = .debug,
            .imports = &.{
                .{ .name = "build_options", .module = roc_modules.build_options },
                .{ .name = "roc_target", .module = roc_modules.roc_target },
            },
        }),
        .filters = test_filters,
    });
    test_suites.register(.{
        .step_suffix = "minici",
        .description = "Run MiniCI output-filter Zig tests",
        .compile = minici_compile,
    });

    // Exercise two real registry runs without compiling the full aggregate.
    // ci/test_unit_report_isolation.py checks their distinct declared reports
    // and summaries under unchanged and runtime-filtered invocations.
    const report_isolation_summary = TestsSummaryStep.create(b, test_filters, 0);
    report_isolation_summary.addRun(&test_suites.configuredRun(.{
        .step_suffix = "minici",
        .description = "MiniCI report isolation fixture",
        .compile = minici_compile,
    }).step);
    report_isolation_summary.addRun(&test_suites.configuredRun(.{
        .step_suffix = "build-helpers",
        .description = "Build helper report isolation fixture",
        .compile = build_helpers_test,
    }).step);
    b.step("run-check-zig-test-reports", "Check isolated declared reports from two Zig test producers")
        .dependOn(report_isolation_summary.step);

    // Add check for forbidden patterns in type checker code
    const check_patterns = CheckTypeCheckerPatternsStep.create(b);
    run_check_type_checker_patterns_step.dependOn(&check_patterns.step);

    // Add check for @enumFromInt(0) usage
    const check_enum_from_int = CheckEnumFromIntZeroStep.create(b);
    run_check_enum_from_int_zero_step.dependOn(&check_enum_from_int.step);

    // Add check for unused variable suppression patterns
    const check_unused = CheckUnusedSuppressionStep.create(b);
    run_check_unused_suppression_step.dependOn(&check_unused.step);

    // Add check that deleted post-check output/remapping APIs do not reappear
    const check_postcheck_architecture = CheckPostcheckArchitectureStep.create(b);
    run_check_postcheck_architecture_step.dependOn(&check_postcheck_architecture.step);

    const check_wasm_builtin_routing = CheckWasmBuiltinRoutingStep.create(b);
    run_check_wasm_builtin_routing_step.dependOn(&check_wasm_builtin_routing.step);

    // Add check that semantic compiler stages do not recover missing data.
    const run_semantic_audit = ci_steps.SemanticAuditStep.create(b);
    run_check_semantic_audit_step.dependOn(run_semantic_audit);

    // Check for @panic and std.debug.panic in interpreter and builtins
    const check_panic = CheckPanicStep.create(b);
    run_check_panic_step.dependOn(&check_panic.step);

    // Add check for global stdio usage in CLI code
    const check_cli_stdio = CheckCliGlobalStdioStep.create(b);
    run_check_cli_global_stdio_step.dependOn(&check_cli_stdio.step);

    tests_summary.run.addArg("--");
    tests_summary.run.addPassthruArgs();
    run_test_zig_step.dependOn(tests_summary.step);

    b.default_step.dependOn(build_web_step);
    {
        const install = playground_test_install;
        b.default_step.dependOn(&install.step);
    }

    // Fmt zig code.
    const fmt_paths = [_]std.Build.LazyPath{ b.path("src"), b.path("build.zig") };
    const fmt = b.addFmt(.{ .paths = &fmt_paths });
    run_fmt_zig_step.dependOn(&fmt.step);

    const check_fmt = b.addFmt(.{ .paths = &fmt_paths, .check = true });
    run_check_zig_format_step.dependOn(&check_fmt.step);

    // Parser code coverage with kcov
    // Only supported on Linux ARM64, matching CI's coverage runner. Other local
    // targets still keep the run-coverage-parser step, but it reports unsupported
    // instead of invoking a kcov binary that cannot trace reliably on that host.
    //
    // Declaring these steps is also what asks zig for the lazy kcov package, so
    // on linux-aarch64 even a build that only wants the roc binary fails without
    // it. -Dcoverage=false skips the declaration, which is how a packager builds
    // roc without supplying kcov at all.
    const enable_coverage = b.option(bool, "coverage", "Declare the kcov coverage steps, which is what pulls in the kcov dependency (default: true; only has an effect on linux-aarch64)") orelse true;
    const is_linux_arm64 = target.result.os.tag == .linux and target.result.cpu.arch == .aarch64;
    const is_coverage_supported = is_linux_arm64 and enable_coverage;
    if (is_coverage_supported and isNativeishOrMusl(target)) {
        // Get the kcov dependency and build it from source
        // lazyDependency returns null on first pass; Zig re-runs build() after fetching
        if (b.lazyDependency("kcov", .{})) |kcov_dep| {
            // Create parse module unit tests for coverage
            // We only use the parse unit tests (not snapshot tool) because:
            // 1. They test the parser directly
            // 2. They don't require LLVM dependencies
            const parse_unit_test = b.addTest(.{
                .name = "parse_unit_coverage",
                .root_module = b.createModule(.{
                    .root_source_file = b.path("src/parse/mod.zig"),
                    .target = target,
                    .optimize = .debug, // Debug required for DWARF debug info
                }),
            });
            roc_modules.addModuleDependencies(parse_unit_test, .parse);

            // Install all artifacts
            const install_parse_test = b.addInstallArtifact(parse_unit_test, .{});

            const kcov_exe = kcov_dep.artifact("kcov");
            const install_kcov = b.addInstallArtifact(kcov_exe, .{});

            // Create a step for building all coverage binaries
            const build_cov_tests = b.step("build-coverage-tests", "Build coverage test binaries to zig-out/bin/");
            build_cov_tests.dependOn(&install_parse_test.step);
            build_cov_tests.dependOn(&install_kcov.step);

            build_coverage_tools_step.dependOn(build_cov_tests);

            // Create output directories before running kcov
            const mkdir_step = b.addSystemCommand(&.{ "mkdir", "-p", "kcov-output/parser" });
            mkdir_step.setCwd(b.path("."));
            mkdir_step.step.dependOn(build_cov_tests);

            // On macOS, kcov needs to be codesigned to use task_for_pid
            // Codesign the installed binary since we run from zig-out/bin/
            if (target.result.os.tag == .macos) {
                const codesign = b.addSystemCommand(&.{"codesign"});
                codesign.setCwd(b.path("."));
                codesign.addArgs(&.{ "-s", "-", "--entitlements" });
                codesign.addFileArg(kcov_dep.path("osx-entitlements.xml"));
                codesign.addArgs(&.{ "-f", "zig-out/bin/kcov" });
                codesign.step.dependOn(&install_kcov.step);
                mkdir_step.step.dependOn(&codesign.step);
            }

            // Run kcov on parse unit tests
            const run_parse_coverage = b.addSystemCommand(&.{"zig-out/bin/kcov"});
            // kcov includes all compiled files (including zig stdlib) in coverage.
            // Use --include-pattern to filter to only src/parse files.
            run_parse_coverage.addArg("--include-pattern=/src/parse/");
            run_parse_coverage.addArgs(&.{
                "kcov-output/parser",
                "zig-out/bin/parse_unit_coverage",
            });
            run_parse_coverage.setCwd(b.path("."));
            run_parse_coverage.step.dependOn(&mkdir_step.step);
            run_parse_coverage.step.dependOn(&install_parse_test.step);

            // Add coverage summary step that parses kcov JSON output
            const summary_step = CoverageSummaryStep.create(b, "kcov-output/parser", "parse_unit_coverage");
            summary_step.step.dependOn(&run_parse_coverage.step);

            // Eval coverage: builds a separate binary with coverage=true (comptime),
            // which DCEs dev/wasm backends, disables fork isolation, and forces
            // single-threaded—so kcov can trace the interpreter in-process.
            // Run separately via: zig build run-coverage-eval
            {
                const coverage_eval_step = b.step("run-coverage-eval", "Run eval tests with kcov code coverage");

                // Build a coverage-specific binary with the coverage build option.
                const eval_coverage_exe = b.addExecutable(.{
                    .name = "eval-coverage-runner",
                    .root_module = b.createModule(.{
                        .root_source_file = b.path("src/eval/test/parallel_runner.zig"),
                        .target = target,
                        .optimize = optimize,
                        .link_libc = true,
                    }),
                });
                configureBackend(eval_coverage_exe, target);
                roc_modules.addAll(eval_coverage_exe);
                eval_coverage_exe.root_module.addOptions("coverage_options", blk: {
                    const opts = b.addOptions();
                    opts.addOption(bool, "coverage", true);
                    break :blk opts;
                });
                eval_coverage_exe.root_module.addImport("compiled_builtins", compiled_builtins_module);
                eval_coverage_exe.root_module.addImport("bytebox", bytebox.module("bytebox"));
                eval_coverage_exe.root_module.addImport("test_harness", createTestHarnessModule(b, roc_modules));
                eval_coverage_exe.root_module.addImport("simd_test_sources", simd_test_sources_module);
                eval_coverage_exe.step.dependOn(&write_compiled_builtins.step);
                try addLlvmSupportToStep(
                    b,
                    eval_coverage_exe,
                    target,
                    dependency_source,
                    roc_modules,
                    llvm_codegen_module,
                    llvm_embedded_module,
                    zstd,
                );
                if (eval_coverage_exe.root_module.resolved_target.?.result.os.tag != .windows or
                    eval_coverage_exe.root_module.resolved_target.?.result.abi != .msvc)
                {
                    eval_coverage_exe.root_module.link_libcpp = true;
                }

                const install_coverage_runner = b.addInstallArtifact(eval_coverage_exe, .{});

                const mkdir_eval = b.addSystemCommand(&.{ "mkdir", "-p", "kcov-output/eval" });
                mkdir_eval.setCwd(b.path("."));
                mkdir_eval.step.dependOn(&install_coverage_runner.step);
                mkdir_eval.step.dependOn(&install_kcov.step);

                if (target.result.os.tag == .macos) {
                    // kcov needs codesigning on macOS to use task_for_pid
                    const eval_codesign = b.addSystemCommand(&.{"codesign"});
                    eval_codesign.setCwd(b.path("."));
                    eval_codesign.addArgs(&.{ "-s", "-", "--entitlements" });
                    eval_codesign.addFileArg(kcov_dep.path("osx-entitlements.xml"));
                    eval_codesign.addArgs(&.{ "-f", "zig-out/bin/kcov" });
                    eval_codesign.step.dependOn(&install_kcov.step);
                    mkdir_eval.step.dependOn(&eval_codesign.step);
                }

                const run_eval_coverage = b.addSystemCommand(&.{"zig-out/bin/kcov"});
                run_eval_coverage.addArg("--include-pattern=/src/eval/");
                run_eval_coverage.addArgs(&.{
                    "kcov-output/eval",
                    "zig-out/bin/eval-coverage-runner",
                });
                run_eval_coverage.setCwd(b.path("."));
                run_eval_coverage.step.dependOn(&mkdir_eval.step);
                run_eval_coverage.step.dependOn(&install_coverage_runner.step);
                run_eval_coverage.step.dependOn(&install_kcov.step);

                const eval_summary_step = CoverageSummaryStep.createWithOptions(b, "kcov-output/eval", "eval-coverage-runner", "EVAL", 0.0);
                eval_summary_step.step.dependOn(&run_eval_coverage.step);

                coverage_eval_step.dependOn(&eval_summary_step.step);
            }

            // Cross-compile for Windows to verify comptime branches compile
            const windows_target = b.resolveTargetQuery(.{
                .cpu_arch = .x86_64,
                .os_tag = .windows,
                .abi = .msvc,
            });
            const windows_parse_build = b.addTest(.{
                .name = "parse_windows_comptime",
                .root_module = b.createModule(.{
                    .root_source_file = b.path("src/parse/mod.zig"),
                    .target = windows_target,
                    .optimize = .debug,
                }),
            });
            roc_modules.addModuleDependencies(windows_parse_build, .parse);
            // Just compile, don't run - verifies Windows comptime branches
            build_coverage_tools_step.dependOn(&windows_parse_build.step);

            // The coverage tools step builds and installs the instrumented test and kcov.
            build_coverage_tools_step.dependOn(&install_parse_test.step);
            build_coverage_tools_step.dependOn(&install_kcov.step);

            run_coverage_parser_step.dependOn(build_coverage_tools_step);
            run_coverage_parser_step.dependOn(&summary_step.step);
        }
    } else if (!is_coverage_supported) {
        // On unsupported platforms, print a message
        const unsupported_step = buildChecksRun(b, "coverage-unsupported");
        run_coverage_parser_step.dependOn(&unsupported_step.step);
    }
    build_ci_step.dependOn(build_roc_step);
    build_ci_step.dependOn(build_check_tools_step);
    build_ci_step.dependOn(build_snapshot_tool_step);
    build_ci_step.dependOn(build_test_zig_step);
    build_ci_step.dependOn(build_test_lsp_integration_runner_step);
    build_ci_step.dependOn(build_test_eval_runner_step);
    build_ci_step.dependOn(build_test_eval_host_effects_runner_step);
    build_ci_step.dependOn(build_web_step);
    build_ci_step.dependOn(build_test_playground_runner_step);
    build_ci_step.dependOn(build_test_cli_runners_step);
    build_ci_step.dependOn(build_test_hosts_step);
    build_ci_step.dependOn(build_test_serialization_sizes_step);
    build_ci_step.dependOn(build_test_builtin_bake_reproducible_step);
    build_ci_step.dependOn(build_test_wasm_static_lib_runner_step);
    // `run-test-simd-differential` runs the installed Lambda Mono runner; build
    // it here so MiniCI shards running with `--minici-skip-build` reuse it.
    build_ci_step.dependOn(build_test_lambda_mono_differential_step);
    build_ci_step.dependOn(build_coverage_tools_step);

    const fuzz = b.option(bool, "fuzz", "Build fuzz targets including AFL++ and tooling") orelse false;
    const is_windows = target.result.os.tag == .windows;

    // fx platform effectful functions test - only run when not cross-compiling
    if (isNativeishOrMusl(target)) {
        // Determine the appropriate target for the fx platform host library.
        // On Linux, we need to use musl explicitly because the CLI's findHostLibrary
        // looks for targets/x64musl/libhost.a first, and musl produces proper static binaries.
        const native_fx_target_dir = roc_target.RocTarget.fromStdTarget(target.result).toName();
        const fx_arch = target.result.cpu.arch;
        const fx_host_target, const fx_host_target_dir: ?[]const u8 = switch (roc_target.classifyOs(target.result.os.tag)) {
            .linux => if (fx_arch == .x86_64)
                .{ b.resolveTargetQuery(.{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .musl }), "x64musl" }
            else if (fx_arch == .aarch64)
                .{ b.resolveTargetQuery(.{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .musl }), "arm64musl" }
            else
                .{ target, native_fx_target_dir },
            // Windows: build for the native ABI. A gnu-ABI `zig build` lands in
            // x64mingw/arm64mingw and an MSVC one in x64win/arm64win, matching
            // what `roc build` picks as its default target.
            .windows => .{ target, native_fx_target_dir },
            .macos, .freebsd, .openbsd, .netbsd, .other => .{ target, native_fx_target_dir },
        };

        // Create fx test platform host static library
        const test_platform_fx_host_lib = createTestPlatformHostLib(
            b,
            "test_platform_fx_host",
            "test/fx/platform/host.zig",
            fx_host_target,
            optimize,
            roc_modules,
            strip,
            omit_frame_pointer,
            .{ .uses_stack_handler = true },
        );

        // Copy the fx test platform host library to the source directory
        const copy_test_fx_host = b.addWriteFiles();
        const test_fx_host_filename = if (target.result.os.tag == .windows) "host.lib" else "libhost.a";
        const fx_host_main_path = b.pathJoin(&.{ "test/fx/platform", test_fx_host_filename });
        const fx_host_archive = if (target.result.os.tag == .windows)
            test_platform_fx_host_lib.getEmittedBin()
        else
            FixArchivePaddingStep.create(b, test_platform_fx_host_lib.getEmittedBin());
        test_fixtures.copy(copy_test_fx_host, fx_host_archive, fx_host_main_path);

        // Also copy to the target-specific directory so findHostLibrary finds it
        const fx_host_target_path = if (fx_host_target_dir) |target_dir|
            b.pathJoin(&.{ "test/fx/platform/targets", target_dir, test_fx_host_filename })
        else
            null;
        if (fx_host_target_path) |target_path| {
            test_fixtures.copy(
                copy_test_fx_host,
                fx_host_archive,
                target_path,
            );
        }

        const final_fx_host_step = &copy_test_fx_host.step;

        b.getInstallStep().dependOn(final_fx_host_step);

        const static_data_host_target_dir = fx_host_target_dir orelse native_fx_target_dir;
        const final_static_data_host_step = buildAndCopyTestPlatformHostLib(
            b,
            "static-data-host",
            fx_host_target,
            static_data_host_target_dir,
            optimize,
            roc_modules,
            strip,
            omit_frame_pointer,
        );
        b.getInstallStep().dependOn(final_static_data_host_step);

        const final_static_data_platform_step: *Step = if (std.mem.endsWith(u8, static_data_host_target_dir, "musl")) blk: {
            const copy_musl_runtime = b.addWriteFiles();
            test_fixtures.copy(
                copy_musl_runtime,
                b.path(b.pathJoin(&.{ "test/fx/platform/targets", static_data_host_target_dir, "crt1.o" })),
                b.pathJoin(&.{ "test/static-data-host/platform/targets", static_data_host_target_dir, "crt1.o" }),
            );
            test_fixtures.copy(
                copy_musl_runtime,
                b.path(b.pathJoin(&.{ "test/fx/platform/targets", static_data_host_target_dir, "libc.a" })),
                b.pathJoin(&.{ "test/static-data-host/platform/targets", static_data_host_target_dir, "libc.a" }),
            );
            copy_musl_runtime.step.dependOn(final_static_data_host_step);
            break :blk &copy_musl_runtime.step;
        } else if (std.mem.endsWith(u8, static_data_host_target_dir, "mingw")) blk: {
            const copy_mingw_runtime = copyMingwRuntimeToTestPlatform(b, "static-data-host", static_data_host_target_dir);
            copy_mingw_runtime.dependOn(final_static_data_host_step);
            break :blk copy_mingw_runtime;
        } else final_static_data_host_step;
        b.getInstallStep().dependOn(final_static_data_platform_step);

        const final_provided_callable_host_step = buildAndCopyTestPlatformHostLib(
            b,
            "provided-callable-host",
            fx_host_target,
            static_data_host_target_dir,
            optimize,
            roc_modules,
            strip,
            omit_frame_pointer,
        );
        b.getInstallStep().dependOn(final_provided_callable_host_step);

        const final_provided_callable_platform_step: *Step = if (std.mem.endsWith(u8, static_data_host_target_dir, "musl")) blk: {
            const copy_musl_runtime = b.addWriteFiles();
            test_fixtures.copy(
                copy_musl_runtime,
                b.path(b.pathJoin(&.{ "test/fx/platform/targets", static_data_host_target_dir, "crt1.o" })),
                b.pathJoin(&.{ "test/provided-callable-host/platform/targets", static_data_host_target_dir, "crt1.o" }),
            );
            test_fixtures.copy(
                copy_musl_runtime,
                b.path(b.pathJoin(&.{ "test/fx/platform/targets", static_data_host_target_dir, "libc.a" })),
                b.pathJoin(&.{ "test/provided-callable-host/platform/targets", static_data_host_target_dir, "libc.a" }),
            );
            copy_musl_runtime.step.dependOn(final_provided_callable_host_step);
            break :blk &copy_musl_runtime.step;
        } else if (std.mem.endsWith(u8, static_data_host_target_dir, "mingw")) blk: {
            const copy_mingw_runtime = copyMingwRuntimeToTestPlatform(b, "provided-callable-host", static_data_host_target_dir);
            copy_mingw_runtime.dependOn(final_provided_callable_host_step);
            break :blk copy_mingw_runtime;
        } else final_provided_callable_host_step;
        b.getInstallStep().dependOn(final_provided_callable_platform_step);

        const fx_platform_test = b.addTest(.{
            .name = "fx_platform_test",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/cli/test/fx_platform_test.zig"),
                .target = target,
                .optimize = optimize,
                // util.buildIsolatedTestEnvMap touches std.c (Zig 0.16 requires explicit link_libc).
                .link_libc = true,
                .imports = &.{.{ .name = "test_harness", .module = createTestHarnessModule(b, roc_modules) }},
            }),
            .filters = test_filters,
        });
        fx_platform_test.root_module.addOptions("fixture_options", fixture_options);

        test_suites.register(.{
            .step_suffix = "fx-platform",
            .description = "Run fx platform Zig tests",
            .compile = fx_platform_test,
            .fixture_root = true,
            .deps = &.{
                // The host library must be copied AND fixed before the test runs.
                final_fx_host_step,
                final_static_data_platform_step,
                final_provided_callable_platform_step,
                // The tests shell out to the roc CLI.
                build_roc_step,
            },
        });

        const http_arch = target.result.cpu.arch;
        const http_host_target, const http_host_target_dir: ?[]const u8 = switch (roc_target.classifyOs(target.result.os.tag)) {
            .linux => if (http_arch == .x86_64)
                .{ b.resolveTargetQuery(.{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .musl }), "x64musl" }
            else if (http_arch == .aarch64)
                .{ b.resolveTargetQuery(.{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .musl }), "arm64musl" }
            else
                .{ target, null },
            // Windows: build for the native ABI so a gnu-ABI `zig build` lands
            // in x64mingw/arm64mingw, matching `roc build`'s default target.
            .windows => .{ target, roc_target.RocTarget.fromStdTarget(target.result).toName() },
            .macos => if (http_arch == .x86_64) .{ target, "x64mac" } else if (http_arch == .aarch64) .{ target, "arm64mac" } else .{ target, null },
            .freebsd, .openbsd, .netbsd, .other => .{ target, null },
        };

        if (http_host_target_dir) |target_dir| {
            const final_http_host_step = buildAndCopyTestPlatformHostLib(
                b,
                "http-headers",
                http_host_target,
                target_dir,
                .fast,
                roc_modules,
                strip,
                omit_frame_pointer,
            );
            b.getInstallStep().dependOn(final_http_host_step);

            // A MinGW link uses only what the platform declares, so the C
            // runtime has to sit next to the host library.
            if (std.mem.endsWith(u8, target_dir, "mingw")) {
                final_http_host_step.dependOn(copyMingwRuntimeToTestPlatform(b, "http-headers", target_dir));
            }

            const http_app_exe_name = if (http_host_target.result.os.tag == .windows)
                "http_header_decoder_server_prebuilt.exe"
            else
                "http_header_decoder_server_prebuilt";
            const prebuilt_roc_cache_root = b.root.joinString(b.allocator, ".zig-cache/roc-prebuilt-cache") catch @panic("OOM");
            const http_prebuilt_roc_cache_dir = b.pathJoin(&.{ prebuilt_roc_cache_root, "http" });
            const build_http_app = b.addRunArtifact(roc_exe);
            build_http_app.addArgs(&.{
                "build",
                "--opt=speed",
                b.fmt("--target={s}", .{target_dir}),
            });
            build_http_app.setEnvironmentVariable("ROC_CACHE_DIR", http_prebuilt_roc_cache_dir);
            build_http_app.setEnvironmentVariable("XDG_CACHE_HOME", http_prebuilt_roc_cache_dir);
            const http_app_output = build_http_app.addPrefixedOutputFileArg("--output=", http_app_exe_name);
            const http_sources = test_fixtures.cachedRoot(&.{final_http_host_step});
            build_http_app.addFileArg(http_sources.path(b, "test/http-headers/app.roc"));

            build_http_app.step.dependOn(final_http_host_step);
            build_http_app.step.dependOn(build_roc_step);

            const http_header_decoder_platform_test = b.addTest(.{
                .name = "http_header_decoder_platform_test",
                .root_module = b.createModule(.{
                    .root_source_file = b.path("src/cli/test/http_header_decoder_platform_test.zig"),
                    .target = target,
                    .optimize = optimize,
                    .link_libc = true,
                    .imports = &.{.{ .name = "test_harness", .module = createTestHarnessModule(b, roc_modules) }},
                }),
                .filters = test_filters,
            });
            http_header_decoder_platform_test.root_module.addOptions("fixture_options", fixture_options);

            const prebuilt_paths = b.addOptions();
            prebuilt_paths.addOptionPathUntracked("app", http_app_output);
            http_header_decoder_platform_test.root_module.addOptions("prebuilt_paths", prebuilt_paths);
            test_suites.register(.{
                .step_suffix = "http-header-decoder-platform",
                .description = "Run HTTP header Decoder platform Zig test",
                .compile = http_header_decoder_platform_test,
                .fixture_root = true,
                .deps = &.{
                    final_http_host_step,
                    &build_http_app.step,
                    build_roc_step,
                },
            });

            const final_json_host_step = buildAndCopyTestPlatformHostLib(
                b,
                "json-decoder",
                http_host_target,
                target_dir,
                .fast,
                roc_modules,
                strip,
                omit_frame_pointer,
            );
            b.getInstallStep().dependOn(final_json_host_step);

            // A MinGW link uses only what the platform declares, so the C
            // runtime has to sit next to the host library.
            if (std.mem.endsWith(u8, target_dir, "mingw")) {
                final_json_host_step.dependOn(copyMingwRuntimeToTestPlatform(b, "json-decoder", target_dir));
            }

            const json_exe_ext = if (http_host_target.result.os.tag == .windows) ".exe" else "";
            const json_app_exe_name = b.fmt("json_decoder_prebuilt{s}", .{json_exe_ext});
            const json_camel_app_exe_name = b.fmt("json_decoder_camel_prebuilt{s}", .{json_exe_ext});
            const json_camel_direct_app_exe_name = b.fmt("json_decoder_camel_direct_prebuilt{s}", .{json_exe_ext});

            const json_prebuilt_roc_cache_dir = b.pathJoin(&.{ prebuilt_roc_cache_root, "json" });
            const build_json_app = b.addRunArtifact(roc_exe);
            build_json_app.addArgs(&.{
                "build",
                "--opt=speed",
                b.fmt("--target={s}", .{target_dir}),
            });
            build_json_app.setEnvironmentVariable("ROC_CACHE_DIR", json_prebuilt_roc_cache_dir);
            build_json_app.setEnvironmentVariable("XDG_CACHE_HOME", json_prebuilt_roc_cache_dir);
            const json_app_output = build_json_app.addPrefixedOutputFileArg("--output=", json_app_exe_name);
            const json_sources = test_fixtures.cachedRoot(&.{final_json_host_step});
            build_json_app.addFileArg(json_sources.path(b, "test/json-decoder/app.roc"));

            build_json_app.step.dependOn(final_json_host_step);
            build_json_app.step.dependOn(build_roc_step);

            const json_camel_prebuilt_roc_cache_dir = b.pathJoin(&.{ prebuilt_roc_cache_root, "json-camel" });
            const build_json_camel_app = b.addRunArtifact(roc_exe);
            build_json_camel_app.addArgs(&.{
                "build",
                "--opt=speed",
                b.fmt("--target={s}", .{target_dir}),
            });
            build_json_camel_app.setEnvironmentVariable("ROC_CACHE_DIR", json_camel_prebuilt_roc_cache_dir);
            build_json_camel_app.setEnvironmentVariable("XDG_CACHE_HOME", json_camel_prebuilt_roc_cache_dir);
            const json_camel_app_output = build_json_camel_app.addPrefixedOutputFileArg("--output=", json_camel_app_exe_name);
            const json_camel_sources = test_fixtures.cachedRoot(&.{final_json_host_step});
            build_json_camel_app.addFileArg(json_camel_sources.path(b, "test/json-decoder/camel_app.roc"));

            build_json_camel_app.step.dependOn(final_json_host_step);
            build_json_camel_app.step.dependOn(build_roc_step);

            const json_camel_direct_prebuilt_roc_cache_dir = b.pathJoin(&.{ prebuilt_roc_cache_root, "json-camel-direct" });
            const build_json_camel_direct_app = b.addRunArtifact(roc_exe);
            build_json_camel_direct_app.addArgs(&.{
                "build",
                "--opt=speed",
                b.fmt("--target={s}", .{target_dir}),
            });
            build_json_camel_direct_app.setEnvironmentVariable("ROC_CACHE_DIR", json_camel_direct_prebuilt_roc_cache_dir);
            build_json_camel_direct_app.setEnvironmentVariable("XDG_CACHE_HOME", json_camel_direct_prebuilt_roc_cache_dir);
            const json_camel_direct_app_output = build_json_camel_direct_app.addPrefixedOutputFileArg("--output=", json_camel_direct_app_exe_name);
            const json_camel_direct_sources = test_fixtures.cachedRoot(&.{final_json_host_step});
            build_json_camel_direct_app.addFileArg(json_camel_direct_sources.path(b, "test/json-decoder/camel_direct_app.roc"));

            build_json_camel_direct_app.step.dependOn(final_json_host_step);
            build_json_camel_direct_app.step.dependOn(build_roc_step);

            const json_decoder_platform_test = b.addTest(.{
                .name = "json_decoder_platform_test",
                .root_module = b.createModule(.{
                    .root_source_file = b.path("src/cli/test/json_decoder_platform_test.zig"),
                    .target = target,
                    .optimize = optimize,
                    .link_libc = true,
                    .imports = &.{.{ .name = "test_harness", .module = createTestHarnessModule(b, roc_modules) }},
                }),
                .filters = test_filters,
            });
            json_decoder_platform_test.root_module.addOptions("fixture_options", fixture_options);

            const json_prebuilt_paths = b.addOptions();
            json_prebuilt_paths.addOptionPathUntracked("app", json_app_output);
            json_prebuilt_paths.addOptionPathUntracked("camel", json_camel_app_output);
            json_prebuilt_paths.addOptionPathUntracked("camel_direct", json_camel_direct_app_output);
            json_decoder_platform_test.root_module.addOptions("prebuilt_paths", json_prebuilt_paths);
            test_suites.register(.{
                .step_suffix = "json-decoder-platform",
                .description = "Run JSON Decoder platform Zig test",
                .compile = json_decoder_platform_test,
                .fixture_root = true,
                .deps = &.{
                    final_json_host_step,
                    &build_json_app.step,
                    &build_json_camel_app.step,
                    &build_json_camel_direct_app.step,
                    build_roc_step,
                },
            });
        }
    }

    var build_afl = false;
    if (!isNativeishOrMusl(target)) {
        std.log.warn("Cross compilation does not support fuzzing (Only building repro executables)", .{});
    } else if (is_windows) {
        // Windows does not support fuzzing - only build repro executables
    } else if (use_system_afl) {
        // If we have system afl, no need for llvm-config.
        build_afl = true;
    } else {
        // AFL++ does not work with our prebuilt static llvm.
        // Check for llvm-config program in user_llvm_path or on the system.
        // If found, let AFL++ use that.
        if (b.findProgram(.{ .names = &.{"llvm-config"} })) |_| {
            build_afl = true;
        } else {
            std.log.warn("AFL++ requires a full version of llvm from the system or passed in via -Dllvm-path, but `llvm-config` was not found (Only building repro executables)", .{});
        }
    }

    const names: []const []const u8 = &.{
        "tokenize",
        "parse",
        "canonicalize",
        "typecheck",
        "build",
        "build-errors",
    };
    for (names) |name| {
        add_fuzz_target(
            b,
            fuzz,
            build_afl,
            use_system_afl,
            no_bin,
            run_args,
            target,
            optimize,
            roc_modules,
            compiled_builtins_module,
            write_compiled_builtins,
            flag_enable_tracy,
            name,
        );
    }

    test_fixtures.addUpdateStep(&.{build_test_hosts_step});

    // Last, so that every top-level step exists -- including the ones created
    // inside addMainExe.
    assertGranularTestStepsAreIsolated(b);
}

/// Configure-time guard: a public `run-test-zig-*` step must pull exactly the
/// explicitly declared number of Zig test binaries into its graph. That is one
/// by default; custom executable-backed runners declare zero below.
///
/// On Windows `TestsSummaryStep` chains its registered runs so that test
/// binaries start one at a time. Pointing a granular step at the *same*
/// `Step.Run` that the chain owns makes that step inherit the whole chain
/// prefix: `run-test-zig-minici` (~11 std-only string-parsing tests, ~2s) once
/// re-ran 41 unrelated binaries and took 441s, and MiniCI replayed that prefix
/// once per affected job. `TestSuiteRegistry.register` makes that shape
/// unrepresentable; this re-checks the finished graph in case a suite is ever
/// wired up by hand instead.
///
/// The aggregate step is named exactly "run-test-zig" (no trailing hyphen), so
/// the prefix test excludes it without needing an allowlist.
fn assertGranularTestStepsAreIsolated(b: *std.Build) void {
    var visited: std.AutoHashMapUnmanaged(*Step, void) = .empty;
    defer visited.deinit(b.allocator);

    for (b.top_level_steps.keys(), b.top_level_steps.values()) |name, tls| {
        if (!std.mem.startsWith(u8, name, "run-test-zig-")) continue;

        visited.clearRetainingCapacity();
        var found: [4][]const u8 = undefined;
        var found_len: usize = 0;
        collectTestRuns(b, &tls.step, &visited, &found, &found_len);

        const expected_len = expectedGranularZigTestBinaries(name);
        if (found_len == expected_len) continue;

        if (found_len == 0) {
            std.debug.panic(
                "build.zig: step \"{s}\" runs no Zig test binary; granular test steps " ++
                    "must run exactly one unless they are an explicitly declared custom " ++
                    "executable-backed runner.",
                .{name},
            );
        } else if (expected_len == 0) {
            std.debug.panic(
                "build.zig: custom executable-backed step \"{s}\" unexpectedly also " ++
                    "runs {d} Zig test binaries (first: {s}); update its wiring or its " ++
                    "explicit declaration.",
                .{ name, found_len, found[0] },
            );
        } else {
            std.debug.panic(
                "build.zig: step \"{s}\" runs at least {d} test binaries ({s}, {s}, ...). " ++
                    "A granular run-test-zig-* step must run exactly one; wire the suite " ++
                    "through TestSuiteRegistry.register rather than handing the summary's " ++
                    "Step.Run to b.step().",
                .{ name, found_len, found[0], found[1] },
            );
        }
    }
}

/// Expected `Step.Compile` test producers beneath a granular test step.
///
/// The LSP integration suite is intentionally an ordinary executable: its own
/// process-pool harness discovers and runs the integration specs. Keeping that
/// exception explicit means a newly orphaned `run-test-zig-*` step cannot pass
/// configuration merely because it reaches zero Zig test binaries.
fn expectedGranularZigTestBinaries(name: []const u8) usize {
    if (std.mem.eql(u8, name, "run-test-zig-module-lsp_integration")) return 0;
    return 1;
}

/// Walks `step`'s transitive dependencies, recording the names of the test
/// binaries executed along the way. Stops recording at `found.len` names; the
/// caller only needs to know "more than one" and two names for its message.
fn collectTestRuns(
    b: *std.Build,
    step: *Step,
    visited: *std.AutoHashMapUnmanaged(*Step, void),
    found: *[4][]const u8,
    found_len: *usize,
) void {
    const gop = visited.getOrPut(b.allocator, step) catch @panic("OOM");
    if (gop.found_existing) return;

    if (step.tag == .run) {
        const run: *Step.Run = @fieldParentPtr("step", step);
        if (run.producer) |producer| {
            if (producer.kind.isTest() and found_len.* < found.len) {
                found[found_len.*] = producer.name;
                found_len.* += 1;
            }
        }
    }

    for (step.dependencies.items) |dep| collectTestRuns(b, dep, visited, found, found_len);
}

fn discoverBuiltinRocFiles(b: *std.Build) ![]const []const u8 {
    const io = b.graph.io;
    b.dependOnDirectoryContents(b.path("src/build/roc"));
    const builtin_roc_path = b.root.joinString(b.allocator, "src/build/roc") catch @panic("OOM");
    var builtin_roc_dir = try std.Io.Dir.openDirAbsolute(io, builtin_roc_path, .{ .iterate = true });
    defer builtin_roc_dir.close(io);

    var roc_files = std.ArrayList([]const u8).empty;
    errdefer roc_files.deinit(b.allocator);

    var iter = builtin_roc_dir.iterate();
    while (try iter.next(io)) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".roc")) {
            const full_path = b.fmt("src/build/roc/{s}", .{entry.name});
            try roc_files.append(b.allocator, full_path);
        }
    }

    return roc_files.toOwnedSlice(b.allocator);
}

fn add_fuzz_target(
    b: *std.Build,
    fuzz: bool,
    build_afl: bool,
    use_system_afl: bool,
    no_bin: bool,
    run_args: []const []const u8,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    roc_modules: modules.RocModules,
    compiled_builtins_module: *std.Build.Module,
    write_compiled_builtins: *Step.WriteFile,
    tracy: ?[]const u8,
    name: []const u8,
) void {
    // We always include the repro scripts (no dependencies).
    // We only include the fuzzing scripts if `-Dfuzz` is set.
    const root_source_file = b.path(b.fmt("test/fuzzing/fuzz-{s}.zig", .{name}));
    const fuzz_obj = b.addObject(.{
        .name = b.fmt("{s}_obj", .{name}),
        .root_module = b.createModule(.{
            .root_source_file = root_source_file,
            .target = target,
            // Work around instrumentation bugs on mac without giving up perf on linux.
            .optimize = if (target.result.os.tag == .macos) .debug else .safe,
        }),
    });
    configureBackend(fuzz_obj, target);
    // Required for fuzzing.
    fuzz_obj.root_module.link_libc = true;
    fuzz_obj.root_module.stack_check = false;
    // Enable coverage instrumentation for AFL++ when building fuzz targets.
    if (fuzz and build_afl) {
        fuzz_obj.sanitize_coverage_trace_pc_guard = true;
    }

    roc_modules.addAll(fuzz_obj);
    fuzz_obj.root_module.addImport("compiled_builtins", compiled_builtins_module);
    fuzz_obj.step.dependOn(&write_compiled_builtins.step);
    add_tracy(b, roc_modules.build_options, fuzz_obj, target, false, tracy);

    const name_exe = b.fmt("fuzz-{s}", .{name});
    const name_repro = b.fmt("repro-{s}", .{name});
    const build_repro_step = b.step(b.fmt("build-repro-{s}", .{name}), b.fmt("Build fuzz reproduction for {s}", .{name}));
    const run_repro_step = b.step(b.fmt("run-repro-{s}", .{name}), b.fmt("Run fuzz reproduction for {s}", .{name}));
    const repro_exe = b.addExecutable(.{
        .name = name_repro,
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/fuzzing/fuzz-repro.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    configureBackend(repro_exe, target);
    repro_exe.root_module.addImport("fuzz_test", fuzz_obj.root_module);
    repro_exe.root_module.addImport("build_options", roc_modules.build_options);

    _ = install_and_run(b, no_bin, repro_exe, null, build_repro_step, run_repro_step, run_args);

    if (fuzz and build_afl and !no_bin) {
        const fuzz_step = b.step(b.fmt("build-fuzz-{s}", .{name}), b.fmt("Build fuzz executable for {s}", .{name}));
        b.default_step.dependOn(fuzz_step);

        const fuzz_exe = if (target.result.os.tag == .macos)
            addMacosAflFuzzExe(b, target, .safe, use_system_afl, fuzz_obj) orelse return
        else
            addAflFuzzExe(b, target, .safe, use_system_afl, fuzz_obj) orelse return;
        const install_fuzz = b.addInstallBinFile(fuzz_exe, name_exe);
        fuzz_step.dependOn(&install_fuzz.step);
        b.getInstallStep().dependOn(&install_fuzz.step);
    }
}

fn addAflFuzzExe(
    b: *std.Build,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    use_system_afl: bool,
    fuzz_obj: *Step.Compile,
) ?std.Build.LazyPath {
    const afl_kit = b.lazyDependency("afl_kit", .{}) orelse return null;

    var run_afl_cc: *Step.Run = undefined;
    if (use_system_afl) {
        run_afl_cc = b.addSystemCommand(&.{
            b.findProgram(.{ .names = &.{"afl-cc"} }) orelse @panic("Could not find 'afl-cc', which is required to build"),
            "-O3",
        });
    } else {
        const afl = afl_kit.builder.lazyDependency("AFLplusplus", .{
            .target = target,
            .optimize = optimize,
            .@"llvm-config-path" = &[_][]const u8{},
        }) orelse return null;

        const install_tools = b.addInstallDirectory(.{
            .source_dir = .{ .relative = .{ .base = .install_prefix } },
            .install_dir = .prefix,
            .install_subdir = "AFLplusplus",
        });

        install_tools.step.dependOn(afl.builder.getInstallStep());
        run_afl_cc = Step.Run.create(b, "run afl-cc");
        run_afl_cc.addFileArg(.{ .relative = .{ .base = .install_bin, .sub_path = "afl-cc" } });
        run_afl_cc.addArg("-O3");
        run_afl_cc.step.dependOn(&afl.builder.top_level_steps.get("llvm_exes").?.step);
        run_afl_cc.step.dependOn(&install_tools.step);
    }

    // Keep the object output requested so Zig materializes the LLVM bitcode output.
    _ = fuzz_obj.getEmittedBin();

    run_afl_cc.addArg("-o");
    const fuzz_exe = run_afl_cc.addOutputFileArg(fuzz_obj.name);
    run_afl_cc.addFileArg(afl_kit.path("afl.c"));
    run_afl_cc.addFileArg(fuzz_obj.getEmittedLlvmBc());
    // ELF linkers resolve libraries left-to-right, so math libraries must follow the bitcode.
    run_afl_cc.addArg("-lm");
    run_afl_cc.addArg("-lquadmath");

    return fuzz_exe;
}

fn addMacosAflFuzzExe(
    b: *std.Build,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    use_system_afl: bool,
    fuzz_obj: *Step.Compile,
) ?std.Build.LazyPath {
    if (!use_system_afl) {
        std.log.warn("Vendored AFL++ does not currently support macOS fuzz executable linking; use system AFL++", .{});
        return null;
    }

    const afl_kit = b.lazyDependency("afl_kit", .{}) orelse return null;
    const afl_cc = b.findProgram(.{ .names = &.{"afl-cc"} }) orelse @panic("Could not find 'afl-cc', which is required to build");
    const afl_bin_dir = std.fs.path.dirname(afl_cc) orelse @panic("Could not determine afl-cc directory");
    const afl_compiler_rt = std.Build.LazyPath{ .cwd_relative = b.pathJoin(&.{ afl_bin_dir, "..", "lib", "afl", "afl-compiler-rt.o" }) };

    fuzz_obj.root_module.fuzz = true;
    fuzz_obj.root_module.link_libc = true;
    fuzz_obj.sanitize_coverage_trace_pc_guard = true;

    const exe = b.addExecutable(.{
        .name = fuzz_obj.name,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    configureBackend(exe, target);
    exe.root_module.addCSourceFile(.{
        .file = afl_kit.path("afl.c"),
        .flags = &.{},
    });
    exe.root_module.addObject(fuzz_obj);
    exe.root_module.addObjectFile(afl_compiler_rt);

    return exe.getEmittedBin();
}

/// Build a Boxy runtime object for `target`: the `roc_boxy_*` C-ABI wrappers
/// plus `roc_boxy_init_embedded`. The selected root configures standalone
/// extern-symbol calls or evaluator-vtable calls.
///
/// This object is linked into user programs, so its optimize mode is pinned
/// rather than following the compiler's own. A `Debug` compiler would otherwise
/// emit a `Debug` runtime into every generic program it builds, which enables
/// builtins' hosted debug diagnostics and grows this object by an order of
/// magnitude. `strip` still follows the build, so debug info for the runtime
/// remains available wherever the rest of the build carries it.
fn buildBoxyRuntimeObject(
    b: *std.Build,
    roc_modules: modules.RocModules,
    target: ResolvedTarget,
    strip: bool,
    omit_frame_pointer: ?bool,
    name: []const u8,
    root_source_file: std.Build.LazyPath,
) *Step.Compile {
    const optimize: OptimizeMode = .fast;
    const obj = b.addObject(.{
        .name = name,
        .root_module = b.createModule(.{
            .root_source_file = root_source_file,
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            // Native runtime objects can participate in shared-library links.
            // Wasm uses direct data-symbol relocations so compiler-only
            // relocatable composition can bind its app-specific Boxy sidecar.
            .pic = target.result.cpu.arch != .wasm32,
        }),
    });
    const boxy_eval_module = b.createModule(.{
        .root_source_file = b.path("src/eval/boxy_runtime_module.zig"),
        .target = target,
        .optimize = optimize,
    });
    boxy_eval_module.addImport("base", roc_modules.base);
    boxy_eval_module.addImport("layout", roc_modules.layout);
    boxy_eval_module.addImport("lir", roc_modules.lir);
    boxy_eval_module.addImport("builtins", roc_modules.builtins);
    boxy_eval_module.addImport("build_options", roc_modules.build_options);

    obj.root_module.addImport("base", roc_modules.base);
    obj.root_module.addImport("builtins", roc_modules.builtins);
    obj.root_module.addImport("eval", boxy_eval_module);
    obj.root_module.addImport("lir", roc_modules.lir);
    obj.root_module.addImport("raw_pages", roc_modules.raw_pages);
    obj.root_module.addImport("shim_io", b.createModule(
        .{ .root_source_file = b.path("src/shim_io.zig") },
    ));
    // The builtins object linked into the same program is the compiler-rt
    // carrier, so this object must not bundle its own copy: COFF rejects the
    // duplicate definitions of `memcpy` and the integer libcalls outright.
    obj.bundle_compiler_rt = false;
    configureBackend(obj, target);
    return obj;
}

/// Zig's wasm object build places the relocatable code in the emitted
/// directory beside an empty nominal output.
fn wasmObjectArtifact(b: *std.Build, obj: *Step.Compile) std.Build.LazyPath {
    const zcu_name = b.fmt("{s}_zcu.o", .{std.fs.path.stem(obj.out_filename)});
    return obj.getEmittedBinDirectory().path(b, zcu_name);
}

/// The run shim uses builtin function addresses directly. It must not link the
/// exported builtins payload used by compiled object output: those exports would
/// expose compiler internals to the platform linker.
fn addMachineCodeShimLib(
    b: *std.Build,
    roc_modules: modules.RocModules,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    strip: bool,
    omit_frame_pointer: ?bool,
    shim_host_abi_module: *std.Build.Module,
    compiled_builtins_module: *std.Build.Module,
    write_compiled_builtins: *Step.WriteFile,
) *Step.Compile {
    const machine_code_shim_lib = b.addLibrary(.{
        .name = "roc_machine_code_shim",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/machine_code_shim/main.zig"),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true,
        }),
        .linkage = .static,
    });
    configureBackend(machine_code_shim_lib, target);
    if (target.result.os.tag == .linux and target.result.cpu.arch.isArm()) {
        // RocOps crash aborts in platform hosts; no foreign exceptions cross
        // this boundary. Stack tracing is disabled by shim_io as well. Do not
        // emit EHABI personality dependencies or substitute no-op personalities.
        machine_code_shim_lib.root_module.unwind_tables = .none;
    }
    // Only the modules the shim actually imports. The full compiler module set
    // would put libc in the shim's dependency graph (the bundle module links
    // zstd), and `link_libc` is resolved over the whole graph regardless of
    // which modules are reachable from the root source file.
    machine_code_shim_lib.root_module.addImport("base", roc_modules.base);
    machine_code_shim_lib.root_module.addImport("backend", roc_modules.backend);
    machine_code_shim_lib.root_module.addImport("builtins", roc_modules.builtins);
    // The machine-code runtime needs Boxy support, not the interpreter's
    // assembly trampoline. Keep the same Zig dependencies without attaching
    // that unrelated globally-exported assembly object to the archive.
    const shim_eval = b.createModule(.{ .root_source_file = b.path("src/eval/mod.zig") });
    var eval_imports = roc_modules.eval.import_table.iterator();
    while (eval_imports.next()) |entry| shim_eval.addImport(entry.key_ptr.*, entry.value_ptr.*);
    machine_code_shim_lib.root_module.addImport("eval", shim_eval);
    machine_code_shim_lib.root_module.addImport("ipc", roc_modules.ipc);
    machine_code_shim_lib.root_module.addImport("lir", roc_modules.lir);
    machine_code_shim_lib.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    machine_code_shim_lib.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    machine_code_shim_lib.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    machine_code_shim_lib.root_module.addImport("shim_host_abi", shim_host_abi_module);
    machine_code_shim_lib.root_module.addImport("compiled_builtins", compiled_builtins_module);
    machine_code_shim_lib.step.dependOn(&write_compiled_builtins.step);
    if (target.result.os.tag == .linux and
        (target.result.cpu.arch == .x86 or target.result.cpu.arch.isArm()))
    {
        // Reuse the toolchain's arithmetic and AAPCS implementations without
        // importing its public compiler-rt root (which exports every helper).
        const private_rt = b.addWriteFiles();
        const root = private_rt.addCopyFile(b.path("src/machine_code_shim/compiler_rt.zig"), "compiler_rt.zig");
        for ([_][]const u8{
            "int.zig",                 "udivmod.zig",             "arm.zig",
            "udivmoddi4_test.zig",     "udivmodti4_test.zig",     "divti3_test.zig",
            "modti3_test.zig",         "floatundidf.zig",         "floatundisf.zig",
            "fixdfdi.zig",             "fixunsdfdi.zig",          "fixsfdi.zig",
            "fixunssfdi.zig",          "float_from_int.zig",      "int_from_float.zig",
            "float_from_int_test.zig", "int_from_float_test.zig",
        }) |file| {
            _ = private_rt.addCopyFile(std.Build.LazyPath.zig_lib.path(b, b.pathJoin(&.{ "compiler_rt", file })), b.pathJoin(&.{ "compiler_rt", file }));
        }
        machine_code_shim_lib.root_module.addImport("private_compiler_rt", b.createModule(.{
            .root_source_file = root,
            .target = target,
            .optimize = .fast,
        }));
    }
    // The shim defines its compiler-private stack probe internally. Do not
    // bundle the complete compiler-rt object: its broad set of weak definitions
    // can participate in platform symbol resolution, and COFF rejects duplicate
    // definitions of memcpy and the integer/float libcalls outright.
    machine_code_shim_lib.bundle_compiler_rt = false;
    // Linux IO uses direct syscalls. Do not pull Zig's libc-dependent IO
    // implementation into the archive. Codegen's C memory/math libcalls are
    // declared separately by the archive's target ABI contract.
    if (target.result.os.tag == .linux) machine_code_shim_lib.root_module.link_libc = false;

    return machine_code_shim_lib;
}

/// Link the checked archive against a platform that owns colliding compiler-rt
/// names. This verifies actual relocation closure, not just symbol spelling.
fn addMachineCodeShimLinkCheck(
    b: *std.Build,
    roc_modules: modules.RocModules,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    archive: std.Build.LazyPath,
) *Step.Compile {
    const host = b.addObject(.{
        .name = "machine_code_shim_link_host",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/machine_code_shim/test_host.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    host.root_module.addImport("builtins", roc_modules.builtins);
    configureBackend(host, target);
    const options = b.addOptions();
    options.addOption(bool, "is_interpreter", false);
    const consumer = b.addTest(.{
        .name = "machine_code_shim_checked_link",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/machine_code_shim/boundary_link_test.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    configureBackend(consumer, target);
    consumer.root_module.addImport("builtins", roc_modules.builtins);
    consumer.root_module.addOptions("boundary_test_options", options);
    consumer.root_module.addObject(host);
    consumer.root_module.addObjectFile(archive);
    consumer.root_module.addCSourceFile(.{ .file = b.path("src/machine_code_shim/test/compiler_rt_collisions.c") });
    // Keep every shim relocation live, even if this minimal host does not call
    // the image loader. The deliberately poisoned platform helpers are link
    // fixtures, not a runnable test runtime.
    consumer.link_gc_sections = false;
    _ = consumer.getEmittedBin();
    return consumer;
}

const MainExeResult = struct {
    exe: *Step.Compile,
    machine_code_shim_test: ?*Step.Compile,
    machine_code_shim_archive_check: ?*Step,
    boundary_link_tests: [2]?*Step.Compile,
    machine_code_shim_archive_test: ?*Step.Compile,
    archive_member_names_test: ?*Step.Compile,
};
/// LLVM bitcode for `src/builtins/sha256_rounds_lib.zig` compiled for `query`,
/// whose CPU features select the SHA-256 rounds the payload defines.
fn addSha256RoundsBitcode(b: *std.Build, comptime rounds_name: []const u8, query: std.Target.Query) std.Build.LazyPath {
    const obj = b.addObject(.{
        .name = "roc_sha256_" ++ rounds_name ++ "_bc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/sha256_rounds_lib.zig"),
            .target = b.resolveTargetQuery(query),
            .optimize = .fast,
            .strip = true,
            .pic = true,
            .single_threaded = true,
        }),
    });
    obj.root_module.omit_frame_pointer = true;
    obj.root_module.stack_check = false;
    obj.use_llvm = true;
    obj.bundle_compiler_rt = false;
    _ = obj.getEmittedBin();
    return obj.getEmittedLlvmBc();
}

fn addMainExe(
    b: *std.Build,
    roc_modules: modules.RocModules,
    target: ResolvedTarget,
    optimize: OptimizeMode,
    strip: bool,
    omit_frame_pointer: ?bool,
    dependency_source: DependencySource,
    tracy: ?[]const u8,
    zstd: *Dependency,
    compiled_builtins_module: *std.Build.Module,
    write_compiled_builtins: *Step.WriteFile,
    llvm_codegen_module: *std.Build.Module,
    flag_enable_tracy: ?[]const u8,
    test_filters: []const []const u8,
    add_machine_code_shim_test: bool,
    valgrind_support: ?bool,
) ?MainExeResult {
    const exe = b.addExecutable(.{
        .name = "roc",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/cli/main.zig"),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .link_libc = true,
            .valgrind = valgrind_support,
        }),
    });
    const embedded_assets = b.addWriteFiles();
    // The in-process interpreter (used by `--opt=interpreter`) recurses Zig stack
    // frames per Roc call. With Zig 0.16 codegen frame sizes, the Windows 1 MiB
    // default reserve isn't enough—recursion-heavy Roc programs trip our
    // SetUnhandledExceptionFilter stack-overflow handler before the interpreter
    // can catch the overflow itself. Reserve 64 MiB to match eval-test-runner.
    exe.stack_size = stack_budget.roc_stack_size;
    splitCompilerSections(exe);
    configureBackend(exe, target);
    exe.root_module.addImport("llvm_codegen", llvm_codegen_module);
    linkWatchPlatformLibs(exe, target);

    // Create builtins object file at build time with minimal dependencies.
    // This is a plain .o (not a .a archive) since we don't bundle compiler_rt here
    // (compiler_rt is bundled in the shim instead). Using .o avoids ar archive format
    // issues and is simpler since we pass it directly to the linker.
    const builtins_target = withoutSha256Floor(b, target);
    const builtins_obj = b.addObject(.{
        .name = "roc_builtins",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/static_lib.zig"),
            .target = builtins_target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true, // Enable Position Independent Code for PIE compatibility
        }),
    });
    // Provide a no-op tracy stub so host_abi.zig can do @import("tracy") without
    // pulling in the real tracy module (which requires build_options).
    builtins_obj.root_module.addImport("tracy", b.createModule(.{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    builtins_obj.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    builtins_obj.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    builtins_obj.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    // This RocOps-ABI object is not linked into built executables (the dev
    // backend links the extern-ABI object below, the LLVM backend merges
    // builtins into the app object), so it does not need compiler-rt.
    builtins_obj.bundle_compiler_rt = false;
    configureBackend(builtins_obj, target);

    // Extern-symbol-mode builtins object: same builtins, but host operations
    // go through linker-resolved symbols (the symbol ABI) instead of RocOps.
    const builtins_extern_obj = b.addObject(.{
        .name = "roc_builtins_extern",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/builtins/extern_static_lib.zig"),
            .target = builtins_target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true,
        }),
    });
    builtins_extern_obj.root_module.addImport("tracy", b.createModule(.{
        .root_source_file = b.path("src/builtins/tracy_stub.zig"),
    }));
    builtins_extern_obj.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    builtins_extern_obj.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    builtins_extern_obj.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    // Bundle compiler-rt so the float math builtins are self-contained. Zig
    // lowers @sqrt/@sin/@cos/@floor/@trunc/@log/@exp (used by acos, asin, sin,
    // cos, pow, ...) to libm libcalls (sqrt, sin, floor, ...) that are not
    // otherwise present when the dev backend links this object into a -nostdlib
    // executable. compiler-rt provides them as weak symbols, and the final
    // link's --gc-sections drops the unused ones. macOS is excluded: it always
    // links -lSystem (which provides libm) and `-fcompiler-rt` for a macOS
    // target crashes the Zig compiler under the build server (--listen).
    builtins_extern_obj.bundle_compiler_rt = target.result.os.tag != .macos;
    configureBackend(builtins_extern_obj, target);

    const shim_host_abi_module = b.createModule(.{
        .root_source_file = b.path("src/shim_host_abi.zig"),
    });
    shim_host_abi_module.addImport("builtins", roc_modules.builtins);
    shim_host_abi_module.addImport("roc_args", roc_modules.roc_args);

    // Create LIR interpreter shim static library at build time - fully static without libc
    //
    // NOTE we do NOT link libC here to avoid dynamic dependency on libC
    const interpreter_shim_lib = b.addLibrary(.{
        .name = "roc_interpreter_shim",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/interpreter_shim/main.zig"),
            .target = target,
            .optimize = optimize,
            .strip = strip,
            .omit_frame_pointer = omit_frame_pointer,
            .pic = true, // Enable Position Independent Code for PIE compatibility
        }),
        .linkage = .static,
    });
    configureBackend(interpreter_shim_lib, target);
    // Keep compiler-only modules out of this runtime archive. In particular,
    // bundle links libc through zstd even when its source is never imported.
    interpreter_shim_lib.root_module.addImport("base", roc_modules.base);
    interpreter_shim_lib.root_module.addImport("builtins", roc_modules.builtins);
    interpreter_shim_lib.root_module.addImport("eval", roc_modules.eval);
    interpreter_shim_lib.root_module.addImport("ipc", roc_modules.ipc);
    interpreter_shim_lib.root_module.addImport("layout", roc_modules.layout);
    interpreter_shim_lib.root_module.addImport("lir", roc_modules.lir);
    if (target.result.os.tag == .linux) interpreter_shim_lib.root_module.link_libc = false;
    interpreter_shim_lib.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
    interpreter_shim_lib.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
    interpreter_shim_lib.root_module.addImport("shim_io", b.createModule(.{
        .root_source_file = b.path("src/shim_io.zig"),
    }));
    interpreter_shim_lib.root_module.addImport("shim_host_abi", shim_host_abi_module);
    // Add compiled builtins module for loading builtin types
    interpreter_shim_lib.root_module.addImport("compiled_builtins", compiled_builtins_module);
    interpreter_shim_lib.step.dependOn(&write_compiled_builtins.step);
    // Include the pre-built builtins object
    interpreter_shim_lib.root_module.addObjectFile(builtins_obj.getEmittedBin());
    interpreter_shim_lib.bundle_compiler_rt = true;
    // Zig names archive members after the paths of the objects it packed,
    // which puts a `.zig-cache` key and the build machine's home directory
    // into the archive; `roc` embeds the archive, so strip the names to bare
    // file names first (see src/build/archive_member_names.zig).
    const archive_member_names_tool = b.addExecutable(.{
        .name = "archive_member_names",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/archive_member_names.zig"),
            .target = b.graph.host,
            .optimize = .safe,
        }),
    });
    configureBackend(archive_member_names_tool, b.graph.host);
    // The link inputs `roc` embeds are digested once here (see
    // src/build/embedded_digests.zig), so `roc run` never rehashes them.
    const embedded_digests_tool = b.addExecutable(.{
        .name = "embedded_digests",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/embedded_digests.zig"),
            .target = b.graph.host,
            .optimize = .fast,
        }),
    });
    configureBackend(embedded_digests_tool, b.graph.host);
    const embedded_digests = b.addRunArtifact(embedded_digests_tool);
    const embedded_digests_source = embedded_digests.addOutputFileArg("embedded_digests.zig");
    const interpreter_shim_filename = if (target.result.os.tag == .windows) "roc_interpreter_shim.lib" else "libroc_interpreter_shim.a";
    const strip_interpreter_shim_names = b.addRunArtifact(archive_member_names_tool);
    strip_interpreter_shim_names.addArg(@tagName(target.result.os.tag));
    strip_interpreter_shim_names.addFileArg(interpreter_shim_lib.getEmittedBin());
    const bare_interpreter_shim = strip_interpreter_shim_names.addOutputFileArg(interpreter_shim_filename);
    embedded_digests.addArg("interpreter_shim");
    embedded_digests.addFileArg(bare_interpreter_shim);
    // Install shim library to the output directory
    const install_interpreter_shim = b.addInstallLibFile(bare_interpreter_shim, interpreter_shim_filename);
    b.getInstallStep().dependOn(&install_interpreter_shim.step);
    // Copy the shim library to the src/ directory for embedding as binary data
    // This is because @embedFile happens at compile time and needs the file to exist already
    // and zig doesn't permit embedding files from directories outside the source tree.
    const copy_interpreter_shim = embedded_assets;
    _ = copy_interpreter_shim.addCopyFile(bare_interpreter_shim, b.pathJoin(&.{ "", interpreter_shim_filename }));

    const machine_code_shim_lib = addMachineCodeShimLib(b, roc_modules, target, optimize, strip, omit_frame_pointer, shim_host_abi_module, compiled_builtins_module, write_compiled_builtins);

    var machine_code_shim_test_for_registry: ?*Step.Compile = null;
    var machine_code_shim_archive_check_for_registry: ?*Step = null;
    var boundary_link_tests: [2]?*Step.Compile = .{ null, null };
    var machine_code_shim_archive_test_for_registry: ?*Step.Compile = null;
    var archive_member_names_test_for_registry: ?*Step.Compile = null;
    if (add_machine_code_shim_test) {
        const machine_code_shim_test = b.addTest(.{
            .name = "machine_code_shim",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/machine_code_shim/main.zig"),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
            }),
            .filters = test_filters,
        });
        configureBackend(machine_code_shim_test, target);
        roc_modules.addAll(machine_code_shim_test);
        machine_code_shim_test.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
        machine_code_shim_test.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
        machine_code_shim_test.root_module.addImport("shim_io", b.createModule(.{
            .root_source_file = b.path("src/shim_io.zig"),
        }));
        machine_code_shim_test.root_module.addImport("shim_host_abi", shim_host_abi_module);
        machine_code_shim_test.root_module.addImport("compiled_builtins", compiled_builtins_module);
        machine_code_shim_test.step.dependOn(&write_compiled_builtins.step);
        machine_code_shim_test.root_module.addObjectFile(builtins_obj.getEmittedBin());
        const machine_code_shim_test_host = b.addObject(.{
            .name = "machine_code_shim_test_host",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/machine_code_shim/test_host.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        configureBackend(machine_code_shim_test_host, target);
        machine_code_shim_test_host.root_module.addImport("builtins", roc_modules.builtins);
        machine_code_shim_test.root_module.addObject(machine_code_shim_test_host);
        machine_code_shim_test.bundle_compiler_rt = true;
        add_tracy(b, roc_modules.build_options, machine_code_shim_test, b.graph.host, false, flag_enable_tracy);
        machine_code_shim_test_for_registry = machine_code_shim_test;

        const boundary_link_step = b.step("run-test-shim-boundary-link", "Link canonical host symbols through both real shim archives");
        for ([_]*Step.Compile{ machine_code_shim_lib, interpreter_shim_lib }, 0..) |shim_lib, index| {
            const options = b.addOptions();
            options.addOption(bool, "is_interpreter", index == 1);
            const boundary_link_test = b.addTest(.{
                .name = if (index == 0) "machine-code-boundary-link" else "interpreter-boundary-link",
                .root_module = b.createModule(.{
                    .root_source_file = b.path("src/machine_code_shim/boundary_link_test.zig"),
                    .target = target,
                    .optimize = optimize,
                }),
            });
            configureBackend(boundary_link_test, target);
            boundary_link_test.root_module.addImport("builtins", roc_modules.builtins);
            boundary_link_test.root_module.addOptions("boundary_test_options", options);
            boundary_link_test.root_module.addObject(machine_code_shim_test_host);
            boundary_link_test.root_module.linkLibrary(shim_lib);
            const run_boundary_link_test = b.addRunArtifact(boundary_link_test);
            boundary_link_step.dependOn(&run_boundary_link_test.step);
            boundary_link_tests[index] = boundary_link_test;
        }
    }

    // Build-time only: validate the exact archive that will be installed and
    // embedded. This executable is never linked into Roc or a user's program.
    const archive_checker = b.addExecutable(.{
        .name = "machine_code_shim_archive_check",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/machine_code_shim/archive_check.zig"),
            .target = b.graph.host,
            .optimize = .safe,
        }),
    });
    archive_checker.root_module.addAnonymousImport("shim_symbols", .{
        .root_source_file = b.path("src/builtins/shim_symbols.zig"),
    });
    configureBackend(archive_checker, b.graph.host);
    const check_archive = b.addRunArtifact(archive_checker);
    check_archive.addArg(@tagName(target.result.os.tag));
    check_archive.addFileArg(machine_code_shim_lib.getEmittedBin());
    const machine_code_shim_filename = if (target.result.os.tag == .windows) "roc_machine_code_shim.lib" else "libroc_machine_code_shim.a";
    const checked_machine_code_shim = check_archive.addOutputFileArg(machine_code_shim_filename);
    machine_code_shim_archive_check_for_registry = &check_archive.step;
    if (add_machine_code_shim_test) {
        const selected_check = b.step("check-selected-machine-code-shim", "Check the selected target's shim symbol contract");
        selected_check.dependOn(&check_archive.step);
        if (target.result.os.tag == .linux and
            (target.result.cpu.arch == .x86 or target.result.cpu.arch.isArm()))
        {
            const link_check = addMachineCodeShimLinkCheck(b, roc_modules, target, optimize, checked_machine_code_shim);
            selected_check.dependOn(&link_check.step);
        }
    }
    const strip_machine_code_shim_names = b.addRunArtifact(archive_member_names_tool);
    strip_machine_code_shim_names.addArg(@tagName(target.result.os.tag));
    strip_machine_code_shim_names.addFileArg(checked_machine_code_shim);
    const bare_machine_code_shim = strip_machine_code_shim_names.addOutputFileArg(machine_code_shim_filename);
    embedded_digests.addArg("machine_code_shim");
    embedded_digests.addFileArg(bare_machine_code_shim);

    // Cross-check every shipped native ABI from any developer host. No target
    // executable is run: the host checker reads each target's object format.
    if (add_machine_code_shim_test) {
        const checks = b.step("check-machine-code-shim-targets", "Check all shipped shim symbol contracts");
        checks.dependOn(&check_archive.step);
        const queries = [_]std.Target.Query{
            .{ .cpu_arch = .x86, .os_tag = .linux, .abi = .musl },
            .{ .cpu_arch = .arm, .os_tag = .linux, .abi = .musleabihf },
            .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .musl },
            .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .musl },
            .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .gnu },
            .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .gnu },
            .{ .cpu_arch = .x86_64, .os_tag = .macos },
            .{ .cpu_arch = .aarch64, .os_tag = .macos },
            .{ .cpu_arch = .x86_64, .os_tag = .windows, .abi = .gnu },
            .{ .cpu_arch = .aarch64, .os_tag = .windows, .abi = .gnu },
            .{ .cpu_arch = .x86_64, .os_tag = .windows, .abi = .msvc },
            .{ .cpu_arch = .aarch64, .os_tag = .windows, .abi = .msvc },
        };
        for (queries) |query| {
            const cross_target = b.resolveTargetQuery(query);
            if (target.result.cpu.arch == cross_target.result.cpu.arch and
                target.result.os.tag == cross_target.result.os.tag and
                target.result.abi == cross_target.result.abi) continue;
            const cross_shim = addMachineCodeShimLib(b, roc_modules, cross_target, optimize, strip, omit_frame_pointer, shim_host_abi_module, compiled_builtins_module, write_compiled_builtins);
            const check_cross = b.addRunArtifact(archive_checker);
            check_cross.addArg(@tagName(cross_target.result.os.tag));
            check_cross.addFileArg(cross_shim.getEmittedBin());
            const checked_cross = check_cross.addOutputFileArg(if (cross_target.result.os.tag == .windows) "roc_machine_code_shim.lib" else "libroc_machine_code_shim.a");
            checks.dependOn(&check_cross.step);
            if (cross_target.result.os.tag == .linux and
                (cross_target.result.cpu.arch == .x86 or cross_target.result.cpu.arch.isArm()))
            {
                const link_check = addMachineCodeShimLinkCheck(b, roc_modules, cross_target, optimize, checked_cross);
                checks.dependOn(&link_check.step);
            }
        }
        // Link real COFF consumers with deliberate platform/private collisions.
        // Both target ABIs must accept the prepared archive and its rebuilt index.
        for ([_]std.Target.Cpu.Arch{ .x86_64, .aarch64 }) |arch| {
            const fixture_target = b.resolveTargetQuery(.{ .cpu_arch = arch, .os_tag = .windows, .abi = .msvc });
            const fixture = b.addLibrary(.{
                .name = "shim_contract_fixture",
                .linkage = .static,
                .root_module = b.createModule(.{ .target = fixture_target, .optimize = .debug, .link_libc = false }),
            });
            fixture.root_module.addCSourceFile(.{ .file = b.path("src/machine_code_shim/test/private_symbols.c") });
            fixture.bundle_compiler_rt = false;
            const prepare_fixture = b.addRunArtifact(archive_checker);
            prepare_fixture.addArg("windows");
            prepare_fixture.addFileArg(fixture.getEmittedBin());
            const prepared_fixture = prepare_fixture.addOutputFileArg("shim.lib");
            const consumer = b.addExecutable(.{
                .name = "shim_contract_consumer",
                .root_module = b.createModule(.{ .target = fixture_target, .optimize = .debug, .link_libc = false }),
            });
            consumer.root_module.addCSourceFile(.{ .file = b.path("src/machine_code_shim/test/private_symbols_host.c") });
            consumer.root_module.addObjectFile(prepared_fixture);
            consumer.bundle_compiler_rt = false;
            consumer.entry = .{ .symbol_name = "mainCRTStartup" };
            consumer.subsystem = .console;
            checks.dependOn(&consumer.step);
            if (b.graph.host.result.os.tag == .windows and b.graph.host.result.cpu.arch == arch) {
                const run_consumer = b.addRunArtifact(consumer);
                checks.dependOn(&run_consumer.step);
            }
        }
        const checker_tests = b.addTest(.{
            .name = "machine_code_shim_archive",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/machine_code_shim/archive_check.zig"),
                .target = b.graph.host,
                .optimize = .safe,
                .imports = &.{.{ .name = "shim_symbols", .module = roc_modules.shim_symbols }},
            }),
            .filters = test_filters,
        });
        machine_code_shim_archive_test_for_registry = checker_tests;
        const archive_member_names_tests = b.addTest(.{
            .name = "archive_member_names",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/build/archive_member_names.zig"),
                .target = b.graph.host,
                .optimize = .safe,
            }),
            .filters = test_filters,
        });
        archive_member_names_test_for_registry = archive_member_names_tests;
        configureBackend(archive_member_names_tests, b.graph.host);
        const run_archive_member_names_tests = b.addRunArtifact(archive_member_names_tests);
        checks.dependOn(&run_archive_member_names_tests.step);
        configureBackend(checker_tests, b.graph.host);
        const run_checker_tests = b.addRunArtifact(checker_tests);
        checks.dependOn(&run_checker_tests.step);
        machine_code_shim_archive_check_for_registry = checks;
    }

    const install_machine_code_shim = b.addInstallLibFile(bare_machine_code_shim, machine_code_shim_filename);
    b.getInstallStep().dependOn(&install_machine_code_shim.step);

    const copy_machine_code_shim = embedded_assets;
    _ = copy_machine_code_shim.addCopyFile(bare_machine_code_shim, b.pathJoin(&.{ "", machine_code_shim_filename }));

    // Copy builtins object for the host target for embedding into CLI
    // This is used by `roc build --opt=dev` to link the app object with builtins
    const copy_builtins = embedded_assets;
    const host_builtins_filename = if (target.result.os.tag == .windows) "roc_builtins.obj" else "roc_builtins.o";
    _ = copy_builtins.addCopyFile(builtins_obj.getEmittedBin(), b.pathJoin(&.{ "", host_builtins_filename }));

    const copy_builtins_extern = embedded_assets;
    const host_builtins_extern_filename = if (target.result.os.tag == .windows) "roc_builtins_extern.obj" else "roc_builtins_extern.o";
    _ = copy_builtins_extern.addCopyFile(builtins_extern_obj.getEmittedBin(), b.pathJoin(&.{ "", host_builtins_extern_filename }));

    // Add tracy support (required by parse/can/check modules)
    add_tracy(b, roc_modules.build_options, interpreter_shim_lib, b.graph.host, false, flag_enable_tracy);

    // Cross-compile builtins objects for all supported targets.
    // These are needed by `roc build --opt=dev --target=X` to link the app object with builtins.
    // The interpreter shim is built only for the native host target above.
    const cross_compile_builtins_targets = [_]struct { name: []const u8, query: std.Target.Query }{
        .{ .name = "x64musl", .query = .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .musl } },
        .{ .name = "arm64musl", .query = .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .musl } },
        .{ .name = "x64glibc", .query = .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .gnu } },
        .{ .name = "arm64glibc", .query = .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .gnu } },
        .{ .name = "wasm32", .query = .{ .cpu_arch = .wasm32, .os_tag = .freestanding, .abi = .none } },
        .{ .name = "x64win", .query = .{ .cpu_arch = .x86_64, .os_tag = .windows, .abi = .msvc } },
        .{ .name = "x64mingw", .query = .{ .cpu_arch = .x86_64, .os_tag = .windows, .abi = .gnu } },
        .{ .name = "arm64win", .query = .{ .cpu_arch = .aarch64, .os_tag = .windows, .abi = .msvc } },
        .{ .name = "arm64mingw", .query = .{ .cpu_arch = .aarch64, .os_tag = .windows, .abi = .gnu } },
        .{ .name = "x64freebsd", .query = .{ .cpu_arch = .x86_64, .os_tag = .freebsd, .abi = .none } },
        .{ .name = "x64openbsd", .query = .{ .cpu_arch = .x86_64, .os_tag = .openbsd, .abi = .none } },
        .{ .name = "x64netbsd", .query = .{ .cpu_arch = .x86_64, .os_tag = .netbsd, .abi = .none } },
        .{ .name = "x64mac", .query = roc_target.macos_deployment.query(.x86_64) },
        .{ .name = "arm64mac", .query = roc_target.macos_deployment.query(.aarch64) },
    };

    for (cross_compile_builtins_targets) |cross_target| {
        const cross_resolved_target = b.resolveTargetQuery(cross_target.query);
        // The extern-ABI builtins object (linked by `roc build --opt=dev`)
        // must carry compiler-rt so its float math libcalls (sqrt, sin,
        // floor, ...) resolve into the -nostdlib executable. Excluded: wasm32
        // (gets compiler-rt via the dedicated merged object below) and macOS
        // (resolves them against -lSystem at the final link). The former BSD
        // compiler-rt exclusion is unnecessary with Zig 0.17: both the minimal
        // build-runner IPC repro and Roc's actual extern builtins compile for
        // FreeBSD, OpenBSD, and NetBSD with compiler-rt bundled.
        const cross_is_wasm = std.mem.eql(u8, cross_target.name, "wasm32");
        const cross_is_macos = cross_target.query.os_tag == .macos;
        const cross_bundle_compiler_rt = !cross_is_wasm and !cross_is_macos;

        // Build builtins object file for this target.
        const cross_builtins_obj = b.addObject(.{
            .name = b.fmt("roc_builtins_{s}", .{cross_target.name}),
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/builtins/static_lib.zig"),
                .target = cross_resolved_target,
                .optimize = optimize,
                .strip = strip,
                .omit_frame_pointer = omit_frame_pointer,
                .pic = true,
            }),
        });
        // Provide a no-op tracy stub (same as for host builtins above)
        cross_builtins_obj.root_module.addImport("tracy", b.createModule(
            .{ .root_source_file = b.path("src/builtins/tracy_stub.zig") },
        ));
        cross_builtins_obj.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
        cross_builtins_obj.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
        cross_builtins_obj.root_module.addImport("shim_io", b.createModule(
            .{ .root_source_file = b.path("src/shim_io.zig") },
        ));
        // Non-extern (RocOps-ABI) object is not linked into executables; only
        // wasm32 merges compiler-rt below for the eval/REPL pipeline.
        cross_builtins_obj.bundle_compiler_rt = false;
        configureBackend(cross_builtins_obj, cross_resolved_target);

        const cross_wasm32_compiler_rt_obj: ?*Step.Compile = if (cross_is_wasm) blk: {
            const compiler_rt_obj = b.addObject(.{
                .name = "compiler_rt_wasm32",
                .root_module = b.createModule(.{
                    .root_source_file = std.Build.LazyPath.zig_lib.path(b, "compiler_rt.zig"),
                    .target = cross_resolved_target,
                    .optimize = optimize,
                    .strip = strip,
                    .omit_frame_pointer = omit_frame_pointer,
                    .pic = true,
                }),
            });
            compiler_rt_obj.bundle_compiler_rt = false;
            configureBackend(compiler_rt_obj, cross_resolved_target);
            break :blk compiler_rt_obj;
        } else null;

        const cross_builtins_bin = if (cross_wasm32_compiler_rt_obj) |compiler_rt_obj| blk: {
            const link_cross_wasm32_builtins = b.addSystemCommand(&.{ b.graph.zig_exe, "wasm-ld", "-r" });
            link_cross_wasm32_builtins.addArg("-o");
            const merged_cross_wasm32_builtins = link_cross_wasm32_builtins.addOutputFileArg("roc_builtins.o");
            link_cross_wasm32_builtins.addFileArg(cross_builtins_obj.getEmittedBin());
            link_cross_wasm32_builtins.addFileArg(compiler_rt_obj.getEmittedBin());
            break :blk merged_cross_wasm32_builtins;
        } else cross_builtins_obj.getEmittedBin();

        // Copy builtins object for this target for embedding into CLI
        // Used by `roc build --opt=dev --target=X` to link the app object with builtins
        const builtins_ext = if (cross_target.query.os_tag == .windows) "roc_builtins.obj" else "roc_builtins.o";
        const copy_cross_builtins = embedded_assets;
        _ = copy_cross_builtins.addCopyFile(
            cross_builtins_bin,
            b.pathJoin(&.{ "targets", cross_target.name, builtins_ext }),
        );

        // Standalone wasm builds compose compiler-rt into the Roc-owned object
        // and localize it before the platform link. Keep it separate from the
        // extern builtins object so compiler support is never a public archive
        // member or a platform-resolvable global.
        if (cross_wasm32_compiler_rt_obj) |compiler_rt_obj| {
            const copy_cross_compiler_rt = embedded_assets;
            _ = copy_cross_compiler_rt.addCopyFile(
                compiler_rt_obj.getEmittedBin(),
                b.pathJoin(&.{ "targets", cross_target.name, "roc_compiler_rt.o" }),
            );
        }

        // Extern-symbol-mode builtins object for this target (the symbol ABI).
        const cross_builtins_extern_obj = b.addObject(.{
            .name = b.fmt("roc_builtins_extern_{s}", .{cross_target.name}),
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/builtins/extern_static_lib.zig"),
                .target = cross_resolved_target,
                .optimize = optimize,
                .strip = strip,
                .omit_frame_pointer = omit_frame_pointer,
                .pic = true,
            }),
        });
        cross_builtins_extern_obj.root_module.addImport("tracy", b.createModule(
            .{ .root_source_file = b.path("src/builtins/tracy_stub.zig") },
        ));
        cross_builtins_extern_obj.root_module.addImport("vendor_parse_float", roc_modules.vendor_parse_float);
        cross_builtins_extern_obj.root_module.addImport("vendor_ryu", roc_modules.vendor_ryu);
        cross_builtins_extern_obj.root_module.addImport("shim_io", b.createModule(
            .{ .root_source_file = b.path("src/shim_io.zig") },
        ));
        cross_builtins_extern_obj.bundle_compiler_rt = cross_bundle_compiler_rt;
        configureBackend(cross_builtins_extern_obj, cross_resolved_target);

        const builtins_extern_ext = if (cross_target.query.os_tag == .windows) "roc_builtins_extern.obj" else "roc_builtins_extern.o";
        const copy_cross_builtins_extern = embedded_assets;
        _ = copy_cross_builtins_extern.addCopyFile(
            cross_builtins_extern_obj.getEmittedBin(),
            b.pathJoin(&.{ "targets", cross_target.name, builtins_extern_ext }),
        );

        // Boxy runtime object for this target, linked by `roc build --opt=dev`
        // into programs that emit boxy statements.
        {
            const cross_boxy_runtime_obj = buildBoxyRuntimeObject(
                b,
                roc_modules,
                cross_resolved_target,
                strip,
                omit_frame_pointer,
                b.fmt("roc_boxy_runtime_{s}", .{cross_target.name}),
                b.path("src/boxy_runtime/main.zig"),
            );
            const boxy_runtime_artifact = if (cross_is_wasm)
                wasmObjectArtifact(b, cross_boxy_runtime_obj)
            else
                cross_boxy_runtime_obj.getEmittedBin();
            const boxy_runtime_ext = if (cross_target.query.os_tag == .windows) "roc_boxy_runtime.obj" else "roc_boxy_runtime.o";
            const copy_cross_boxy_runtime = embedded_assets;
            _ = copy_cross_boxy_runtime.addCopyFile(
                boxy_runtime_artifact,
                b.pathJoin(&.{ "targets", cross_target.name, boxy_runtime_ext }),
            );
        }

        if (!cross_is_wasm) {
            const default_platform_os = cross_target.query.os_tag orelse .freestanding;
            const default_platform_root_source = if (default_platform_os == .linux)
                b.path("src/default_platform/linux_runtime.zig")
            else if (default_platform_os == .freebsd or default_platform_os == .netbsd)
                b.path("src/default_platform/bsd_runtime.zig")
            else
                b.path("src/default_platform/c_runtime.zig");

            const default_platform_runtime_obj = b.addObject(.{
                .name = b.fmt("roc_default_runtime_{s}", .{cross_target.name}),
                .root_module = b.createModule(.{
                    .root_source_file = default_platform_root_source,
                    .target = cross_resolved_target,
                    .optimize = .fast,
                    .strip = strip,
                    .omit_frame_pointer = false,
                    .pic = true,
                    .single_threaded = true,
                }),
            });
            default_platform_runtime_obj.root_module.addImport("roc_str_view", roc_modules.roc_str_view);
            default_platform_runtime_obj.root_module.addImport("roc_args", roc_modules.roc_args);
            default_platform_runtime_obj.root_module.addImport("raw_pages", roc_modules.raw_pages);
            default_platform_runtime_obj.root_module.addImport("shim_symbols", roc_modules.shim_symbols);
            const default_platform_runtime_options = b.addOptions();
            default_platform_runtime_options.addOption(bool, "include_process_entrypoint", false);
            default_platform_runtime_obj.root_module.addOptions("default_platform_options", default_platform_runtime_options);
            default_platform_runtime_obj.root_module.stack_check = false;
            default_platform_runtime_obj.root_module.link_libc = false;
            default_platform_runtime_obj.bundle_compiler_rt = false;
            configureBackend(default_platform_runtime_obj, cross_resolved_target);

            const copy_default_platform_runtime = embedded_assets;
            const default_runtime_ext = if (cross_target.query.os_tag == .windows) "roc_default_runtime.obj" else "roc_default_runtime.o";
            _ = copy_default_platform_runtime.addCopyFile(
                default_platform_runtime_obj.getEmittedBin(),
                b.pathJoin(&.{ "targets", cross_target.name, default_runtime_ext }),
            );
            embedded_digests.addArg(b.fmt("default_runtime_{s}", .{cross_target.name}));
            embedded_digests.addFileArg(default_platform_runtime_obj.getEmittedBin());

            // A shared-memory run of the synthetic Linux default platform has
            // no external platform host to provide compiler-rt. Keep that
            // carrier explicit and default-platform-owned instead of hiding it
            // in the machine-code shim, which is also linked with user hosts.
            if (default_platform_os == .linux) {
                const default_platform_compiler_rt_obj = b.addObject(.{
                    .name = b.fmt("roc_default_compiler_rt_{s}", .{cross_target.name}),
                    .root_module = b.createModule(.{
                        .root_source_file = std.Build.LazyPath.zig_lib.path(b, "compiler_rt.zig"),
                        .target = cross_resolved_target,
                        .optimize = .fast,
                        .strip = strip,
                        .omit_frame_pointer = false,
                        .pic = true,
                    }),
                });
                default_platform_compiler_rt_obj.bundle_compiler_rt = false;
                configureBackend(default_platform_compiler_rt_obj, cross_resolved_target);

                const copy_default_platform_compiler_rt = embedded_assets;
                _ = copy_default_platform_compiler_rt.addCopyFile(
                    default_platform_compiler_rt_obj.getEmittedBin(),
                    b.pathJoin(&.{ "targets", cross_target.name, "roc_default_compiler_rt.o" }),
                );
                embedded_digests.addArg(b.fmt("default_compiler_rt_{s}", .{cross_target.name}));
                embedded_digests.addFileArg(default_platform_compiler_rt_obj.getEmittedBin());
            }

            const default_platform_executable_obj = b.addObject(.{
                .name = b.fmt("roc_default_platform_{s}", .{cross_target.name}),
                .root_module = b.createModule(.{
                    .root_source_file = default_platform_root_source,
                    .target = cross_resolved_target,
                    .optimize = .fast,
                    .strip = strip,
                    .omit_frame_pointer = false,
                    .pic = true,
                    .single_threaded = true,
                }),
            });
            default_platform_executable_obj.root_module.addImport("roc_str_view", roc_modules.roc_str_view);
            default_platform_executable_obj.root_module.addImport("roc_args", roc_modules.roc_args);
            default_platform_executable_obj.root_module.addImport("raw_pages", roc_modules.raw_pages);
            default_platform_executable_obj.root_module.addImport("shim_symbols", roc_modules.shim_symbols);
            const default_platform_executable_options = b.addOptions();
            default_platform_executable_options.addOption(bool, "include_process_entrypoint", true);
            default_platform_executable_obj.root_module.addOptions("default_platform_options", default_platform_executable_options);
            default_platform_executable_obj.root_module.stack_check = false;
            default_platform_executable_obj.root_module.link_libc = false;
            default_platform_executable_obj.bundle_compiler_rt = false;
            configureBackend(default_platform_executable_obj, cross_resolved_target);

            const copy_default_platform_executable = embedded_assets;
            const default_platform_ext = if (cross_target.query.os_tag == .windows) "roc_default_platform.obj" else "roc_default_platform.o";
            _ = copy_default_platform_executable.addCopyFile(
                default_platform_executable_obj.getEmittedBin(),
                b.pathJoin(&.{ "targets", cross_target.name, default_platform_ext }),
            );
        }
    }

    const copy_default_mingw_runtime = embedded_assets;
    for ([_][]const u8{ "x64mingw", "arm64mingw" }) |target_name| {
        for (mingw_runtime_files) |filename| {
            _ = copy_default_mingw_runtime.addCopyFile(
                b.path(b.pathJoin(&.{ "test/fx/platform/targets", target_name, filename })),
                b.pathJoin(&.{ "targets", target_name, filename }),
            );
        }
    }

    const embedded_assets_source = embedded_assets.add("embedded_assets.zig",
        \\pub fn file(comptime path: []const u8) []const u8 {
        \\    return @embedFile(path);
        \\}
    );
    exe.root_module.addAnonymousImport("embedded_assets", .{ .root_source_file = embedded_assets_source });

    const use_bundled_deps = dependency_source.isBundled();

    const config = b.addOptions();
    config.addOption(bool, "llvm", true);
    config.addOption(bool, "binaryen", use_bundled_deps);
    exe.root_module.addOptions("config", config);
    exe.root_module.addAnonymousImport("legal_details", .{ .root_source_file = b.path("legal_details") });
    exe.root_module.addAnonymousImport("embedded_digests", .{ .root_source_file = embedded_digests_source });

    const llvm_paths_exe = llvmPaths(b, target, dependency_source) orelse return null;
    exe.root_module.addLibraryPath(llvm_paths_exe.lib);
    exe.root_module.addIncludePath(llvm_paths_exe.include);
    if (use_bundled_deps) {
        addStaticBinaryenOptionsToModule(exe.root_module);
    }
    try addStaticLlvmOptionsToModule(exe.root_module);

    add_tracy(b, roc_modules.build_options, exe, target, true, tracy);

    exe.root_module.linkLibrary(zstd.artifact("zstd"));

    return .{
        .exe = exe,
        .machine_code_shim_test = machine_code_shim_test_for_registry,
        .machine_code_shim_archive_check = machine_code_shim_archive_check_for_registry,
        .boundary_link_tests = boundary_link_tests,
        .machine_code_shim_archive_test = machine_code_shim_archive_test_for_registry,
        .archive_member_names_test = archive_member_names_test_for_registry,
    };
}

/// Produce a cached executable path. When `macho_strip_tool` is given and
/// the target is macOS, the runtime copy is produced by that tool (which
/// removes the dyld export trie and weak-bind info; see
/// src/cli/macho/DyldExportStrip.zig) instead of installing the linked
/// artifact directly. Fixture runners use this path independent of installs.
fn executableRuntimePath(b: *std.Build, exe: *Step.Compile, macho_strip_tool: ?*Step.Compile) std.Build.LazyPath {
    if (macho_strip_tool) |tool| {
        if (exe.root_module.resolved_target.?.result.os.tag == .macos) {
            const strip_run = b.addRunArtifact(tool);
            strip_run.addFileArg(exe.getEmittedBin());
            return strip_run.addOutputFileArg(exe.out_filename);
        }
    }
    return exe.getEmittedBin();
}

fn addInstallMaybeStrippedExe(b: *std.Build, exe: *Step.Compile, macho_strip_tool: ?*Step.Compile) *Step {
    if (exe.root_module.resolved_target.?.result.os.tag == .macos and macho_strip_tool != null) {
        return &b.addInstallBinFile(executableRuntimePath(b, exe, macho_strip_tool), exe.out_filename).step;
    }
    return &b.addInstallArtifact(exe, .{}).step;
}

fn install_and_run(
    b: *std.Build,
    no_bin: bool,
    exe: *Step.Compile,
    macho_strip_tool: ?*Step.Compile,
    build_step: *Step,
    run_step: *Step,
    run_args: []const []const u8,
) ?*Step {
    if (run_step != build_step) {
        run_step.dependOn(build_step);
    }
    if (no_bin) {
        // No build, just build, don't actually install or run.
        build_step.dependOn(&exe.step);
        b.getInstallStep().dependOn(&exe.step);
        return null;
    } else {
        const install_step = addInstallMaybeStrippedExe(b, exe, macho_strip_tool);

        // Add a step to print success message after build completes
        const success_step = PrintBuildSuccessStep.create(b);
        success_step.step.dependOn(install_step);
        build_step.dependOn(&success_step.step);

        b.getInstallStep().dependOn(install_step);

        const run = b.addRunArtifact(exe);
        run.step.dependOn(install_step);
        if (run_args.len != 0) run.addArgs(run_args);
        run.addPassthruArgs();
        run_step.dependOn(&run.step);
        return install_step;
    }
}

fn createTestHarnessModule(b: *std.Build, roc_modules: modules.RocModules) *std.Build.Module {
    return b.createModule(.{
        .root_source_file = b.path("src/build/test_harness.zig"),
        .imports = &.{
            .{ .name = "collections", .module = roc_modules.collections },
            .{ .name = "build_options", .module = roc_modules.build_options },
        },
    });
}

fn addLlvmSupportToStep(
    b: *std.Build,
    step: *Step.Compile,
    target: ResolvedTarget,
    dependency_source: DependencySource,
    roc_modules: anytype,
    llvm_codegen_module: *std.Build.Module,
    llvm_embedded_module: *std.Build.Module,
    zstd: *Dependency,
) !void {
    const has_llvm = try addLlvmLinkSupportToStep(
        b,
        step,
        target,
        dependency_source,
        llvm_codegen_module,
        zstd,
    );
    if (!has_llvm) return;
    step.root_module.addAnonymousImport("llvm_compile", .{
        .root_source_file = b.path("src/llvm_compile/mod.zig"),
        .imports = &.{
            .{ .name = "collections", .module = roc_modules.collections },
            .{ .name = "layout", .module = roc_modules.layout },
            .{ .name = "backend", .module = roc_modules.backend },
            .{ .name = "lir", .module = roc_modules.lir },
            .{ .name = "llvm_codegen", .module = llvm_codegen_module },
            .{ .name = "vendor_llvm_compile_bindings", .module = roc_modules.vendor_llvm_compile_bindings },
            .{ .name = "build_options", .module = roc_modules.build_options },
            .{ .name = "roc_target", .module = roc_modules.roc_target },
            .{ .name = "builtins", .module = roc_modules.builtins },
            .{ .name = "llvm_embedded", .module = llvm_embedded_module },
            .{ .name = "embedded_lld", .module = roc_modules.embedded_lld },
        },
    });
}

/// The most recently configured compile step that links the embedded LLVM
/// libraries, when the build host is Windows. Each such step waits for the
/// previous one there: several of their links at once exhaust a hosted
/// Windows runner's memory.
var windows_llvm_link_chain: ?*Step = null;

fn addLlvmLinkSupportToStep(
    b: *std.Build,
    step: *Step.Compile,
    target: ResolvedTarget,
    dependency_source: DependencySource,
    llvm_codegen_module: *std.Build.Module,
    zstd: *Dependency,
) !bool {
    const llvm_paths = llvmPaths(b, target, dependency_source) orelse return false;
    if (b.graph.host.result.os.tag == .windows) {
        if (windows_llvm_link_chain) |previous| step.step.dependOn(previous);
        windows_llvm_link_chain = &step.step;
    }
    step.root_module.addLibraryPath(llvm_paths.lib);
    step.root_module.addIncludePath(llvm_paths.include);
    try addStaticLlvmOptionsToModule(step.root_module);
    step.root_module.addImport("llvm_codegen", llvm_codegen_module);
    step.root_module.linkLibrary(zstd.artifact("zstd"));
    return true;
}

const ParsedBuildArgs = struct {
    run_args: []const []const u8,
    test_filters: []const []const u8,
};

fn parseBuildArgs(b: *std.Build) ParsedBuildArgs {
    return .{
        .run_args = b.option([]const []const u8, "run-arg", "Arguments passed to a run step (repeatable)") orelse &.{},
        .test_filters = b.option([]const []const u8, "test-filter", "Select tests containing a substring (repeatable)") orelse &.{},
    };
}

fn add_tracy(
    b: *std.Build,
    module_build_options: *std.Build.Module,
    base: *Step.Compile,
    target: ResolvedTarget,
    links_llvm: bool,
    tracy: ?[]const u8,
) void {
    base.root_module.addImport("build_options", module_build_options);
    if (tracy) |tracy_path| {
        const client_cpp = b.pathJoin(
            &[_][]const u8{ tracy_path, "public", "TracyClient.cpp" },
        );

        // On mingw, we need to opt into windows 7+ to get some features required by tracy.
        const tracy_c_flags: []const []const u8 = if (target.result.os.tag == .windows and target.result.abi == .gnu)
            &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined", "-D_WIN32_WINNT=0x601" }
        else
            &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined" };

        base.root_module.addIncludePath(.{ .cwd_relative = tracy_path });
        base.root_module.addCSourceFile(.{ .file = .{ .cwd_relative = client_cpp }, .flags = tracy_c_flags });
        base.root_module.addCSourceFile(.{ .file = .{ .cwd_relative = "src/build/tracy-shutdown.cpp" }, .flags = tracy_c_flags });
        if (!links_llvm) {
            base.root_module.linkSystemLibrary("c++", .{ .use_pkg_config = .no });
        }
        base.root_module.link_libc = true;

        if (target.result.os.tag == .windows) {
            base.root_module.linkSystemLibrary("dbghelp", .{});
            base.root_module.linkSystemLibrary("ws2_32", .{});
        }
    }
}

const DependencySource = union(enum) {
    downloaded_bundle,
    local_bundle: []const u8,
    custom_llvm: []const u8,
    system_llvm,

    fn isBundled(self: DependencySource) bool {
        return switch (self) {
            .downloaded_bundle, .local_bundle => true,
            .custom_llvm, .system_llvm => false,
        };
    }
};

const LlvmPaths = struct {
    include: std.Build.LazyPath,
    lib: std.Build.LazyPath,
};

fn llvmPaths(b: *std.Build, target: ResolvedTarget, source: DependencySource) ?LlvmPaths {
    switch (source) {
        .local_bundle, .custom_llvm => |path| {
            const root = b.graph.cwdRelativePath(path);
            return .{ .include = root.path(b, "include"), .lib = root.path(b, "lib") };
        },
        .system_llvm => {
            const llvm_config = b.findProgram(.{ .names = &.{"llvm-config"} }) orelse
                @panic("-Dsystem-llvm requires llvm-config on PATH");
            return .{
                .include = b.graph.cwdRelativePath(runLlvmConfig(b, llvm_config, "--includedir")),
                .lib = b.graph.cwdRelativePath(runLlvmConfig(b, llvm_config, "--libdir")),
            };
        },
        .downloaded_bundle => {
            const raw_triple = target.result.linuxTriple(b.allocator) catch @panic("OOM");
            const triple = supported_deps_triples.get(raw_triple) orelse {
                std.log.err("Target {s} is not supported by roc-bootstrap; provide -Droc-deps-path, -Dllvm-path or -Dsystem-llvm", .{raw_triple});
                std.process.exit(1);
            };
            const name = b.fmt("roc_deps_{s}", .{triple});
            const deps = b.lazyDependency(name, .{}) orelse return null;
            return .{ .include = deps.path("include"), .lib = deps.path("lib") };
        },
    }
}

const supported_deps_triples = std.StaticStringMap([]const u8).initComptime(.{
    .{ "aarch64-macos-none", "aarch64_macos_none" },
    .{ "aarch64-linux-musl", "aarch64_linux_musl" },
    .{ "aarch64-windows-gnu", "aarch64_windows_gnu" },
    .{ "arm-linux-musleabihf", "arm_linux_musleabihf" },
    .{ "x86-linux-musl", "x86_linux_musl" },
    .{ "x86_64-linux-musl", "x86_64_linux_musl" },
    .{ "x86_64-macos-none", "x86_64_macos_none" },
    .{ "x86_64-windows-gnu", "x86_64_windows_gnu" },
    // We also support the gnu linux targets.
    // For those, we just map to musl.
    .{ "aarch64-linux-gnu", "aarch64_linux_musl" },
    .{ "arm-linux-gnueabihf", "arm_linux_musleabihf" },
    .{ "x86-linux-gnu", "x86_linux_musl" },
    .{ "x86_64-linux-gnu", "x86_64_linux_musl" },
});

// The following is adapted from the Zig compiler at https://codeberg.org/ziglang/zig and licensed under the MIT license. Thanks, Zig team!
fn addStaticLlvmOptionsToModule(mod: *std.Build.Module) !void {
    const cpp_cflags = exe_cflags ++ [_][]const u8{"-DNDEBUG=1"};
    mod.addCSourceFiles(.{
        .files = &cpp_sources,
        .flags = &cpp_cflags,
    });

    const link_static = std.Build.Module.LinkSystemLibraryOptions{
        .preferred_link_mode = .static,
        .search_strategy = .mode_first,
    };
    for (lld_libs) |lib_name| {
        mod.linkSystemLibrary(lib_name, link_static);
    }

    for (llvm_libs) |lib_name| {
        mod.linkSystemLibrary(lib_name, link_static);
    }

    mod.linkSystemLibrary("z", link_static);

    if (mod.resolved_target.?.result.os.tag != .windows or mod.resolved_target.?.result.abi != .msvc) {
        // Use Zig's bundled static libc++ to keep the binary statically linked
        mod.link_libcpp = true;
    }

    if (mod.resolved_target.?.result.os.tag == .windows) {
        mod.linkSystemLibrary("ws2_32", .{});
        mod.linkSystemLibrary("version", .{});
        mod.linkSystemLibrary("uuid", .{});
        mod.linkSystemLibrary("ole32", .{});
    }
}

fn addStaticBinaryenOptionsToModule(mod: *std.Build.Module) void {
    const link_static = std.Build.Module.LinkSystemLibraryOptions{
        .preferred_link_mode = .static,
        .search_strategy = .mode_first,
    };
    mod.addCSourceFile(.{
        .file = .{ .cwd_relative = "src/build/zig_binaryen.cpp" },
        .flags = &exe_cflags,
    });
    mod.linkSystemLibrary("binaryen", link_static);
}

const cpp_sources = [_][]const u8{
    "src/build/zig_llvm.cpp",
};

const exe_cflags = [_][]const u8{
    "-std=c++17",
    "-D__STDC_CONSTANT_MACROS",
    "-D__STDC_FORMAT_MACROS",
    "-D__STDC_LIMIT_MACROS",
    "-D_GNU_SOURCE",
    "-fno-exceptions",
    "-fno-rtti",
    "-fno-stack-protector",
    "-fvisibility-inlines-hidden",
    "-Wno-type-limits",
    "-Wno-missing-braces",
    "-Wno-comment",
};
const lld_libs = [_][]const u8{
    "lldMinGW",
    "lldELF",
    "lldCOFF",
    "lldWasm",
    "lldMachO",
    "lldCommon",
};
// This list can be re-generated with `llvm-config --libfiles` and then
// reformatting using your favorite text editor. Note we do not execute
// `llvm-config` here because we are cross compiling. Also omit LLVMTableGen
// from these libs.
const llvm_libs = [_][]const u8{
    "LLVMWindowsManifest",
    "LLVMXRay",
    "LLVMLibDriver",
    "LLVMDlltoolDriver",
    "LLVMTextAPIBinaryReader",
    "LLVMCoverage",
    "LLVMLineEditor",
    "LLVMXCoreDisassembler",
    "LLVMXCoreCodeGen",
    "LLVMXCoreDesc",
    "LLVMXCoreInfo",
    "LLVMX86TargetMCA",
    "LLVMX86Disassembler",
    "LLVMX86AsmParser",
    "LLVMX86CodeGen",
    "LLVMX86Desc",
    "LLVMX86Info",
    "LLVMWebAssemblyDisassembler",
    "LLVMWebAssemblyAsmParser",
    "LLVMWebAssemblyCodeGen",
    "LLVMWebAssemblyUtils",
    "LLVMWebAssemblyDesc",
    "LLVMWebAssemblyInfo",
    "LLVMVEDisassembler",
    "LLVMVEAsmParser",
    "LLVMVECodeGen",
    "LLVMVEDesc",
    "LLVMVEInfo",
    "LLVMSystemZDisassembler",
    "LLVMSystemZAsmParser",
    "LLVMSystemZCodeGen",
    "LLVMSystemZDesc",
    "LLVMSystemZInfo",
    "LLVMSparcDisassembler",
    "LLVMSparcAsmParser",
    "LLVMSparcCodeGen",
    "LLVMSparcDesc",
    "LLVMSparcInfo",
    "LLVMRISCVTargetMCA",
    "LLVMRISCVDisassembler",
    "LLVMRISCVAsmParser",
    "LLVMRISCVCodeGen",
    "LLVMRISCVDesc",
    "LLVMRISCVInfo",
    "LLVMPowerPCDisassembler",
    "LLVMPowerPCAsmParser",
    "LLVMPowerPCCodeGen",
    "LLVMPowerPCDesc",
    "LLVMPowerPCInfo",
    "LLVMNVPTXCodeGen",
    "LLVMNVPTXDesc",
    "LLVMNVPTXInfo",
    "LLVMSPIRVAnalysis",
    "LLVMSPIRVCodeGen",
    "LLVMSPIRVDesc",
    "LLVMSPIRVInfo",
    "LLVMMSP430Disassembler",
    "LLVMMSP430AsmParser",
    "LLVMMSP430CodeGen",
    "LLVMMSP430Desc",
    "LLVMMSP430Info",
    "LLVMMipsDisassembler",
    "LLVMMipsAsmParser",
    "LLVMMipsCodeGen",
    "LLVMMipsDesc",
    "LLVMMipsInfo",
    "LLVMLoongArchDisassembler",
    "LLVMLoongArchAsmParser",
    "LLVMLoongArchCodeGen",
    "LLVMLoongArchDesc",
    "LLVMLoongArchInfo",
    "LLVMLanaiDisassembler",
    "LLVMLanaiCodeGen",
    "LLVMLanaiAsmParser",
    "LLVMLanaiDesc",
    "LLVMLanaiInfo",
    "LLVMHexagonDisassembler",
    "LLVMHexagonCodeGen",
    "LLVMHexagonAsmParser",
    "LLVMHexagonDesc",
    "LLVMHexagonInfo",
    "LLVMBPFDisassembler",
    "LLVMBPFAsmParser",
    "LLVMBPFCodeGen",
    "LLVMBPFDesc",
    "LLVMBPFInfo",
    "LLVMAVRDisassembler",
    "LLVMAVRAsmParser",
    "LLVMAVRCodeGen",
    "LLVMAVRDesc",
    "LLVMAVRInfo",
    "LLVMARMDisassembler",
    "LLVMARMAsmParser",
    "LLVMARMCodeGen",
    "LLVMARMDesc",
    "LLVMARMUtils",
    "LLVMARMInfo",
    "LLVMAMDGPUTargetMCA",
    "LLVMAMDGPUDisassembler",
    "LLVMAMDGPUAsmParser",
    "LLVMAMDGPUCodeGen",
    "LLVMAMDGPUDesc",
    "LLVMAMDGPUUtils",
    "LLVMAMDGPUInfo",
    "LLVMAArch64Disassembler",
    "LLVMAArch64AsmParser",
    "LLVMAArch64CodeGen",
    "LLVMAArch64Desc",
    "LLVMAArch64Utils",
    "LLVMAArch64Info",
    "LLVMOrcDebugging",
    "LLVMOrcJIT",
    "LLVMWindowsDriver",
    "LLVMMCJIT",
    "LLVMJITLink",
    "LLVMInterpreter",
    "LLVMExecutionEngine",
    "LLVMRuntimeDyld",
    "LLVMOrcTargetProcess",
    "LLVMOrcShared",
    "LLVMDWP",
    "LLVMDebugInfoLogicalView",
    "LLVMDebugInfoGSYM",
    "LLVMOption",
    "LLVMObjectYAML",
    "LLVMObjCopy",
    "LLVMMCA",
    "LLVMMCDisassembler",
    "LLVMDTLTO",
    "LLVMLTO",
    "LLVMPlugins",
    "LLVMPasses",
    "LLVMCGData",
    "LLVMHipStdPar",
    "LLVMCFGuard",
    "LLVMCoroutines",
    "LLVMSandboxIR",
    "LLVMipo",
    "LLVMVectorize",
    "LLVMLinker",
    "LLVMInstrumentation",
    "LLVMFrontendOpenMP",
    "LLVMFrontendDirective",
    "LLVMFrontendAtomic",
    "LLVMFrontendOffloading",
    "LLVMFrontendOpenACC",
    "LLVMFrontendHLSL",
    "LLVMFrontendDriver",
    "LLVMExtensions",
    "LLVMDWARFLinkerParallel",
    "LLVMDWARFLinkerClassic",
    "LLVMDWARFLinker",
    "LLVMGlobalISel",
    "LLVMMIRParser",
    "LLVMAsmPrinter",
    "LLVMSelectionDAG",
    "LLVMCodeGen",
    "LLVMTarget",
    "LLVMObjCARCOpts",
    "LLVMCodeGenTypes",
    "LLVMIRPrinter",
    "LLVMInterfaceStub",
    "LLVMFileCheck",
    "LLVMFuzzMutate",
    "LLVMScalarOpts",
    "LLVMInstCombine",
    "LLVMAggressiveInstCombine",
    "LLVMTransformUtils",
    "LLVMBitWriter",
    "LLVMAnalysis",
    "LLVMProfileData",
    "LLVMSymbolize",
    "LLVMDebugInfoBTF",
    "LLVMDebugInfoPDB",
    "LLVMDebugInfoMSF",
    "LLVMDebugInfoDWARF",
    "LLVMDebugInfoDWARFLowLevel",
    "LLVMObject",
    "LLVMTextAPI",
    "LLVMMCParser",
    "LLVMIRReader",
    "LLVMAsmParser",
    "LLVMMC",
    "LLVMDebugInfoCodeView",
    "LLVMBitReader",
    "LLVMFuzzerCLI",
    "LLVMCore",
    "LLVMRemarks",
    "LLVMBitstreamReader",
    "LLVMBinaryFormat",
    "LLVMTargetParser",
    "LLVMSupport",
    "LLVMDemangle",
};

/// The display version follows Git HEAD independently of cache compatibility.
/// Declare the metadata we read so Zig's cached configure phase notices commits,
/// detached HEADs, worktrees, and packed refs without running Git on every build.
fn getCompilerVersionGit(b: *std.Build) []const u8 {
    const io = b.graph.io;
    const cwd = std.Io.Dir.cwd();
    const dot_git = b.root.joinString(b.allocator, ".git") catch @panic("OOM");
    const git_stat = cwd.statFile(io, dot_git, .{}) catch {
        dependOnExistingParentDirectory(b, dot_git);
        return "no-git";
    };
    const git_dir = if (git_stat.kind == .directory) dir: {
        b.dependOnDirectoryMetadata(b.graph.cwdRelativePath(dot_git));
        break :dir dot_git;
    } else dir: {
        const pointer = readVersionFile(b, dot_git) orelse return "no-git";
        const prefix = "gitdir: ";
        if (!std.mem.startsWith(u8, pointer, prefix)) return "no-git";
        break :dir std.fs.path.resolve(b.allocator, &.{ std.fs.path.dirname(dot_git).?, pointer[prefix.len..] }) catch @panic("OOM");
    };
    const head = readVersionFile(b, b.pathJoin(&.{ git_dir, "HEAD" })) orelse return "no-git";
    if (!std.mem.startsWith(u8, head, "ref: ")) return shortCommit(head);
    const ref_name = head["ref: ".len..];
    const common_dir = if (readVersionFile(b, b.pathJoin(&.{ git_dir, "commondir" }))) |relative|
        std.fs.path.resolve(b.allocator, &.{ git_dir, relative }) catch @panic("OOM")
    else
        git_dir;
    if (readVersionFile(b, b.pathJoin(&.{ common_dir, ref_name }))) |commit| return shortCommit(commit);
    const packed_refs = readVersionFile(b, b.pathJoin(&.{ common_dir, "packed-refs" })) orelse return "no-git";
    var lines = std.mem.splitScalar(u8, packed_refs, '\n');
    while (lines.next()) |line| {
        const space = std.mem.findScalar(u8, line, ' ') orelse continue;
        if (std.mem.eql(u8, line[space + 1 ..], ref_name)) return shortCommit(line[0..space]);
    }
    return "no-git";
}

fn readVersionFile(b: *std.Build, path: []const u8) ?[]const u8 {
    const cwd = std.Io.Dir.cwd();
    const stat = cwd.statFile(b.graph.io, path, .{}) catch {
        dependOnExistingParentDirectory(b, path);
        return null;
    };
    if (stat.kind != .file) {
        dependOnExistingParentDirectory(b, path);
        return null;
    }
    b.dependOnFileContents(b.graph.cwdRelativePath(path));
    const contents = cwd.readFileAlloc(b.graph.io, path, b.allocator, .limited(1024 * 1024)) catch return null;
    return std.mem.trim(u8, contents, " \n\r\t");
}

/// Zig 0.17 cannot record contents of a missing configure input. Watching its
/// nearest existing directory detects creation without poisoning every build.
fn dependOnExistingParentDirectory(b: *std.Build, path: []const u8) void {
    const cwd = std.Io.Dir.cwd();
    var parent = std.fs.path.dirname(path) orelse ".";
    while (true) {
        if (cwd.statFile(b.graph.io, parent, .{})) |stat| {
            if (stat.kind == .directory) {
                b.dependOnDirectoryMetadata(b.graph.cwdRelativePath(parent));
                return;
            }
        } else |_| {}
        parent = std.fs.path.dirname(parent) orelse {
            b.dependOnDirectoryMetadata(b.path("."));
            return;
        };
    }
}

fn shortCommit(commit: []const u8) []const u8 {
    if (commit.len != 40 and commit.len != 64) return "no-git";
    for (commit) |char| if (!std.ascii.isHex(char)) return "no-git";
    return commit[0..8];
}

/// The `compiler_version` string a binary built at `mode` reports at runtime.
///
/// Mirrors the `@import("builtin").mode` switch in the generated build_options
/// module (see where `compiler_version` is assembled in build()). Needed when
/// one binary has to predict another's version, because the build-mode prefix
/// is resolved at each binary's own compile time rather than here.
fn compilerVersionForMode(b: *std.Build, mode: std.builtin.OptimizeMode, compiler_version_git: []const u8) []const u8 {
    return b.fmt("{s}-{s}", .{
        switch (mode) {
            .debug => "debug",
            .safe => "release-safe",
            .fast => "release-fast",
            .small => "release-small",
        },
        compiler_version_git,
    });
}

/// Generate glibc stubs at build time for cross-compilation
///
/// This is a minimal implementation that generates essential symbols needed for basic
/// cross-compilation to glibc targets. It creates assembly stubs with required symbols
/// like __libc_start_main, abort, getauxval, and _IO_stdin_used.
///
/// Future work: Parse Zig's abilists file to generate comprehensive
/// symbol coverage with proper versioning (e.g., symbol@@GLIBC_2.17). The abilists
/// contains thousands of glibc symbols across different versions and architectures
/// that could provide more complete stub coverage for complex applications.
fn generateGlibcStub(b: *std.Build, target: ResolvedTarget, target_name: []const u8) ?*Step.WriteFile {

    // Generate assembly stub with comprehensive symbols using the new build module
    var assembly_buf = std.ArrayList(u8).empty;
    defer assembly_buf.deinit(b.allocator);

    var aw = std.Io.Writer.Allocating.fromArrayList(b.allocator, &assembly_buf);
    const target_arch = target.result.cpu.arch;

    glibc_stub_build.generateComprehensiveStub(&aw.writer, target_arch) catch |err| {
        std.log.warn("Failed to generate comprehensive stub assembly for {s}: {}, using minimal ELF", .{ target_name, err });
        // Fall back to minimal ELF
        const arch = target.result.cpu.arch;
        const stub_content = if (arch == .aarch64)
            createMinimalElfArm64()
        else if (arch == .x86_64)
            createMinimalElfX64()
        else
            return null;

        const write_stub = b.addWriteFiles();
        const libc_so_6 = write_stub.add("libc.so.6", stub_content);
        const libc_so = write_stub.add("libc.so", stub_content);

        const copy_stubs = b.addWriteFiles();
        // Platforms that need glibc stubs
        const glibc_platforms = [_][]const u8{ "int", "str" };
        for (glibc_platforms) |platform| {
            test_fixtures.copy(copy_stubs, libc_so_6, b.pathJoin(&.{ "test", platform, "platform/targets", target_name, "libc.so.6" }));
            test_fixtures.copy(copy_stubs, libc_so, b.pathJoin(&.{ "test", platform, "platform/targets", target_name, "libc.so" }));
        }
        copy_stubs.step.dependOn(&write_stub.step);

        return copy_stubs;
    };

    // Write the assembly file to the targets directory
    const write_stub = b.addWriteFiles();
    assembly_buf = aw.toArrayList();
    const asm_file = write_stub.add("libc_stub.s", assembly_buf.items);

    // Compile the assembly into a proper shared library using Zig's build system
    const libc_stub = glibc_stub_build.compileAssemblyStub(b, asm_file, target, .small);

    // Copy the generated files to all platforms that use glibc targets
    const copy_stubs = b.addWriteFiles();

    // Platforms that need glibc stubs (have glibc targets defined in their .roc files)
    const glibc_platforms = [_][]const u8{ "int", "str" };
    for (glibc_platforms) |platform| {
        test_fixtures.copy(copy_stubs, libc_stub.getEmittedBin(), b.pathJoin(&.{ "test", platform, "platform/targets", target_name, "libc.so.6" }));
        test_fixtures.copy(copy_stubs, libc_stub.getEmittedBin(), b.pathJoin(&.{ "test", platform, "platform/targets", target_name, "libc.so" }));
        test_fixtures.copy(copy_stubs, asm_file, b.pathJoin(&.{ "test", platform, "platform/targets", target_name, "libc_stub.s" }));
    }
    copy_stubs.step.dependOn(&libc_stub.step);
    copy_stubs.step.dependOn(&write_stub.step);

    return copy_stubs;
}

/// Create a minimal ELF shared object for ARM64
fn createMinimalElfArm64() []const u8 {
    // ARM64 minimal ELF shared object
    return &[_]u8{
        // ELF Header (64 bytes)
        0x7F, 'E', 'L', 'F', // e_ident[EI_MAG0..3] - ELF magic
        2, // e_ident[EI_CLASS] - ELFCLASS64
        1, // e_ident[EI_DATA] - ELFDATA2LSB (little endian)
        1, // e_ident[EI_VERSION] - EV_CURRENT
        0, // e_ident[EI_OSABI] - ELFOSABI_NONE
        0, // e_ident[EI_ABIVERSION]
        0, 0, 0, 0, 0, 0, 0, // e_ident[EI_PAD] - padding
        0x03, 0x00, // e_type - ET_DYN (shared object)
        0xB7, 0x00, // e_machine - EM_AARCH64
        0x01, 0x00, 0x00, 0x00, // e_version - EV_CURRENT
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_entry (not used for shared obj)
        0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_phoff - program header offset
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_shoff - section header offset
        0x00, 0x00, 0x00, 0x00, // e_flags
        0x40, 0x00, // e_ehsize - ELF header size
        0x38, 0x00, // e_phentsize - program header entry size
        0x01, 0x00, // e_phnum - number of program headers
        0x40, 0x00, // e_shentsize - section header entry size
        0x00, 0x00, // e_shnum - number of section headers
        0x00, 0x00, // e_shstrndx - section header string table index

        // Program Header (56 bytes) - PT_LOAD
        0x01, 0x00, 0x00, 0x00, // p_type - PT_LOAD
        0x05, 0x00, 0x00, 0x00, // p_flags - PF_R | PF_X
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_offset
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_vaddr
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_paddr
        0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_filesz
        0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_memsz
        0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_align
    };
}

/// Create a minimal ELF shared object for x86-64
fn createMinimalElfX64() []const u8 {
    // x86-64 minimal ELF shared object
    return &[_]u8{
        // ELF Header (64 bytes)
        0x7F, 'E', 'L', 'F', // e_ident[EI_MAG0..3] - ELF magic
        2, // e_ident[EI_CLASS] - ELFCLASS64
        1, // e_ident[EI_DATA] - ELFDATA2LSB (little endian)
        1, // e_ident[EI_VERSION] - EV_CURRENT
        0, // e_ident[EI_OSABI] - ELFOSABI_NONE
        0, // e_ident[EI_ABIVERSION]
        0, 0, 0, 0, 0, 0, 0, // e_ident[EI_PAD] - padding
        0x03, 0x00, // e_type - ET_DYN (shared object)
        0x3E, 0x00, // e_machine - EM_X86_64
        0x01, 0x00, 0x00, 0x00, // e_version - EV_CURRENT
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_entry (not used for shared obj)
        0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_phoff - program header offset
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_shoff - section header offset
        0x00, 0x00, 0x00, 0x00, // e_flags
        0x40, 0x00, // e_ehsize - ELF header size
        0x38, 0x00, // e_phentsize - program header entry size
        0x01, 0x00, // e_phnum - number of program headers
        0x40, 0x00, // e_shentsize - section header entry size
        0x00, 0x00, // e_shnum - number of section headers
        0x00, 0x00, // e_shstrndx - section header string table index

        // Program Header (56 bytes) - PT_LOAD
        0x01, 0x00, 0x00, 0x00, // p_type - PT_LOAD
        0x05, 0x00, 0x00, 0x00, // p_flags - PF_R | PF_X
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_offset
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_vaddr
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_paddr
        0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_filesz
        0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_memsz
        0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_align
    };
}

fn runLlvmConfig(b: *std.Build, program: []const u8, argument: []const u8) []const u8 {
    b.graph.poisonCache();
    const result = std.process.run(b.allocator, b.graph.io, .{ .argv = &.{ program, argument } }) catch @panic("llvm-config could not run");
    if (result.term != .exited or result.term.exited != 0) @panic("llvm-config failed");
    return std.mem.trimEnd(u8, result.stdout, "\n");
}

/// Compiler application caches use content identity, including dirty sources.
/// Compilation never deletes another compiler's application cache.
fn compilerIdentityModule(b: *std.Build, source: DependencySource, tracy_path: ?[]const u8, semantic_options: []const []const u8) ?*std.Build.Module {
    const tool = b.addExecutable(.{
        .name = "compiler_identity",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/build/compiler_identity.zig"),
            .target = b.graph.host,
            .optimize = .fast,
        }),
    });
    const inputs = b.addWriteFiles();
    stageCompilerIdentitySources(b, inputs, "src", &.{ ".zig", ".roc", ".c", ".cpp", ".h", ".S", ".s", ".tbd", ".json" }) catch |err|
        std.debug.panic("cannot stage compiler identity sources: {t}", .{err});
    stageCompilerIdentitySources(b, inputs, "vendor", &.{ ".zig", ".roc", ".c", ".cpp", ".h", ".S", ".s", ".zon" }) catch |err|
        std.debug.panic("cannot stage compiler identity vendors: {t}", .{err});
    // Zig's executable and version do not identify a locally modified standard
    // library. Its independent cached stage tracks all contents, including an
    // explicit --zig-lib, and is reused when production sources change.
    const toolchain_inputs = b.addWriteFiles();
    _ = toolchain_inputs.addCopyDirectory(std.Build.LazyPath.zig_lib, "lib", .{});
    const toolchain_digest = compilerInputDigest(b, tool, toolchain_inputs.getDirectory(), "toolchain_identity.zig");
    // Source dependency archives are pinned by this manifest; content of
    // repository-local dependencies above participates directly as well.
    _ = inputs.addCopyFile(b.path("build.zig"), "build.zig");
    _ = inputs.addCopyFile(b.path("build.zig.zon"), "build.zig.zon");
    // A production edit must not recopy or rehash an unchanged, potentially
    // multi-gigabyte dependency bundle. Only intrinsic dependency contents feed
    // this stage; modes, targets and semantic options feed the final identity.
    const dependency_inputs = b.addWriteFiles();
    var has_dependency_inputs = false;
    if (tracy_path) |path| {
        if (!isImmutableNixStorePath(path)) {
            _ = dependency_inputs.addCopyDirectory(b.graph.cwdRelativePath(path).path(b, "public"), "tracy/public", .{});
            has_dependency_inputs = true;
        }
    }
    switch (source) {
        .local_bundle, .custom_llvm => |path| {
            if (!isImmutableNixStorePath(path)) {
                const root = b.graph.cwdRelativePath(path);
                _ = dependency_inputs.addCopyDirectory(root.path(b, "include"), "include", .{});
                _ = dependency_inputs.addCopyDirectory(root.path(b, "lib"), "lib", .{});
                has_dependency_inputs = true;
            }
        },
        .system_llvm => {
            const paths = llvmPaths(b, b.graph.host, source) orelse return null;
            _ = dependency_inputs.addCopyDirectory(paths.include, "include", .{});
            _ = dependency_inputs.addCopyDirectory(paths.lib, "lib", .{});
            has_dependency_inputs = true;
        },
        .downloaded_bundle => {},
    }
    const run = compilerIdentityRun(b, tool, inputs.getDirectory());
    run.addArgs(&.{ "--file", "toolchain-contents" });
    run.addFileArg(toolchain_digest);
    if (has_dependency_inputs) {
        const dependency_digest = compilerInputDigest(b, tool, dependency_inputs.getDirectory(), "dependency_identity.zig");
        run.addArgs(&.{ "--file", "dependency-contents" });
        run.addFileArg(dependency_digest);
    }
    for (semantic_options) |option| run.addArgs(&.{ "--option", option });
    switch (source) {
        .local_bundle, .custom_llvm => |path| {
            if (isImmutableNixStorePath(path)) run.addArgs(&.{ "--option", b.fmt("immutable-dependencies={s}", .{path}) });
        },
        .downloaded_bundle, .system_llvm => {},
    }
    if (tracy_path) |path| {
        if (isImmutableNixStorePath(path)) run.addArgs(&.{ "--option", b.fmt("immutable-tracy={s}", .{path}) });
    }
    run.addArg("--output");
    const output = run.addOutputFileArg("compiler_identity.zig");
    return b.createModule(.{ .root_source_file = output });
}

fn compilerIdentityRun(b: *std.Build, tool: *Step.Compile, sources: std.Build.LazyPath) *Step.Run {
    const run = b.addRunArtifact(tool);
    run.addArg("--source-root");
    run.addDirectoryArg(sources);
    run.addArg("--zig-exe");
    run.addFileArg(std.Build.LazyPath.zig_exe);
    return run;
}

fn compilerInputDigest(b: *std.Build, tool: *Step.Compile, sources: std.Build.LazyPath, name: []const u8) std.Build.LazyPath {
    const run = compilerIdentityRun(b, tool, sources);
    run.addArgs(&.{ "--option", b.fmt("input-stage={s}", .{name}), "--output" });
    return run.addOutputFileArg(name);
}

// These directories contain dedicated test sources. Imports from production
// files are confined to test blocks and private test helpers. Keep this list
// explicit: a new directory participates until its imports have been audited.
const compiler_identity_test_directories = [_][]const u8{
    "src/bump/test",
    "src/canonicalize/test",
    "src/check/test",
    "src/cli/test",
    "src/compile/test",
    "src/eval/test",
    "src/lsp/test",
    "src/machine_code_shim/test",
    "src/parse/test",
    "src/types/test",
};

// This entrypoint is imported only by the registered unit test runner. Its
// upstream TestFn ABI deliberately carries erased errors, unlike production.
const compiler_identity_test_files = [_][]const u8{
    "vendor/zig_test_runner.zig",
};

fn stageCompilerIdentitySources(b: *std.Build, files: *Step.WriteFile, path: []const u8, included_extensions: []const []const u8) !void {
    // Adding/removing/renaming a production import changes the configured graph.
    // Editing its bytes only reruns WriteFiles and the identity tool at make time.
    b.dependOnDirectoryContents(b.path(path));
    var directory = try std.Io.Dir.cwd().openDir(b.graph.io, try b.root.joinString(b.allocator, path), .{ .iterate = true });
    defer directory.close(b.graph.io);
    var entries: std.ArrayList(std.Io.Dir.Entry) = .empty;
    var iterator = directory.iterate();
    while (try iterator.next(b.graph.io)) |entry| {
        try entries.append(b.allocator, .{ .name = try b.allocator.dupe(u8, entry.name), .kind = entry.kind, .inode = entry.inode });
    }
    std.mem.sort(std.Io.Dir.Entry, entries.items, {}, struct {
        fn less(_: void, left: std.Io.Dir.Entry, right: std.Io.Dir.Entry) bool {
            return std.mem.lessThan(u8, left.name, right.name);
        }
    }.less);
    for (entries.items) |entry| {
        // Use logical separators for the audited exclusions on every host.
        const child = b.fmt("{s}/{s}", .{ path, entry.name });
        switch (entry.kind) {
            .directory => {
                const excluded = for (compiler_identity_test_directories) |test_path| {
                    if (std.mem.eql(u8, child, test_path)) break true;
                } else false;
                if (!excluded) try stageCompilerIdentitySources(b, files, child, included_extensions);
            },
            .file => {
                const excluded = for (compiler_identity_test_files) |test_path| {
                    if (std.mem.eql(u8, child, test_path)) break true;
                } else false;
                if (excluded) continue;
                const extension = std.fs.path.extension(child);
                for (included_extensions) |included| {
                    if (std.mem.eql(u8, extension, included)) {
                        _ = files.addCopyFile(b.path(child), child);
                        break;
                    }
                }
            },
            .block_device,
            .character_device,
            .named_pipe,
            .sym_link,
            .unix_domain_socket,
            .whiteout,
            .door,
            .event_port,
            .unknown,
            => return error.NonRegularCompilerSource,
        }
    }
}
