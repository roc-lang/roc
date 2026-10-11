//! MiniCI runner for split build/run jobs.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const target = @import("roc_target");
const modules = @import("modules.zig");

const out_dir = "zig-out/minici";
const raw_dir = out_dir ++ "/raw";
const logs_dir = out_dir ++ "/logs";
const heartbeat_env = "MINICI_HEARTBEAT_INTERVAL_MS";
const default_heartbeat_interval_ms: u64 = 30_000;
/// Override for the auto-detected `build-ci` job budget.
/// See `memoryAwareBuildJobs`. `MINICI_MAX_CPUS=0` or unset means auto.
const cpu_limit_env = "MINICI_MAX_CPUS";

/// How many bytes from the start and end of a failing step's log to echo to
/// the console. Compiler and test errors land near the top of the output, while
/// `--summary all` prints a large build tree that pushes the terminating error
/// line to the very bottom, so surfacing both ends (and eliding the noisy
/// middle) keeps the failure actionable without a re-run.
const failure_log_head_bytes: usize = 12 * 1024;
const failure_log_tail_bytes: usize = 4 * 1024;

/// Installed by `build-ci`. See `restampBuildCache`.
const restamp_exe = "zig-out/bin/restamp-zig-cache" ++ builtin.target.exeFileExt();
const restamp_log = logs_dir ++ "/restamp-zig-cache.txt";

/// Exit status when every phase passed but the cache-reuse canary did not (see
/// `CacheReuseFailure`). 1 means a phase failed and 2 means bad arguments, so
/// this status says "the checks and tests are fine; the run compiled work it
/// should have reused".
const cache_reuse_exit_code: u8 = 3;

const JobKind = enum {
    single,
    harness,
};

/// Which hosts a MiniCI job has to run on for its signal to be complete. CI
/// runs MiniCI as several lanes (see `Lane`), and each lane selects jobs by
/// placement, so every job's placement is stated here rather than inferred.
const Placement = enum {
    /// Reads only tracked source files and builds nothing beyond its own small
    /// host check tool, so its result is the same on every host. It runs once,
    /// in the `source` lane, which does not need `build-ci`.
    source,
    /// Needs `build-ci` outputs, but its result does not depend on the host
    /// OS or architecture, so it runs only in the primary (Linux x86_64) lane.
    primary_host,
    /// Exercises host-specific behavior (linking, codegen, paths, process
    /// handling, ...), so it runs on every host.
    every_host,
};

const Job = struct {
    name: []const u8,
    kind: JobKind = .single,
    args: []const []const u8 = &.{},
    skip_reason: ?[]const u8 = null,
    placement: Placement = .every_host,
    /// Compile steps this job is expected to build itself instead of reusing
    /// them from `build-ci`, each named exactly as `zig build --summary all`
    /// prints it (for example `compile obj foo debug x86_64-linux-musl`). Any
    /// other compile step that runs during the job fails the cache-reuse
    /// canary (see `cacheReuseFailure`). Empty by default: an exception is
    /// stated here, in the table, and never inferred from a job's output.
    expected_compiles: []const []const u8 = &.{},
    /// `zig build` options for this job alone, for example
    /// `-Doptimize=ReleaseSafe`. Options select a build configuration, and
    /// `build-ci` built only the default one, so a job with options of its own
    /// compiles its own outputs: the canary measures it without restricting
    /// it (see `CommandResult.own_configuration`).
    build_args: []const []const u8 = &.{},
};

/// A group of CI jobs that together run each MiniCI job exactly once per host
/// (see `shards` and the "MiniCI shards cover" tests).
const Lane = enum {
    /// Source-only checks. Runs once, on Linux, before anything is built.
    source,
    /// The Linux x86_64 lane: every job that needs `build-ci` outputs.
    primary,
    /// The macOS and Windows lanes: only the jobs whose result can differ by
    /// host. Source and primary-host jobs already ran on Linux.
    secondary,

    fn runs(self: Lane, placement: Placement) bool {
        return switch (self) {
            .source => placement == .source,
            .primary => placement == .primary_host or placement == .every_host,
            .secondary => placement == .every_host,
        };
    }

    /// The source lane runs only source checks, and those build their own
    /// small host tools, so running `build-ci` first would be pure waste.
    fn needsBuildCi(self: Lane) bool {
        return self != .source;
    }

    /// Other lanes stop at the first failing check so no test time is spent
    /// on a change that is already red. The source lane contains nothing but
    /// checks, so it runs all of them and reports every failure at once.
    fn stopsAtFailingCheck(self: Lane) bool {
        return self != .source;
    }
};

/// The host family a CI shard runs on. Used only to prove shard coverage.
const Host = enum { linux, macos, windows };

/// One CI job's slice of MiniCI: the jobs in `selection` that `lane` runs.
/// `.github/workflows/ci_manager.yml` names every shard exactly once through
/// its `minici_shard:` matrix keys, and `--minici-verify-workflow` checks that.
const Shard = struct {
    name: []const u8,
    host: Host,
    lane: Lane,
    selection: Selection = .{},
};

/// Shard boundaries are balanced from measured per-job CI timings. The
/// "MiniCI shards cover" tests prove that, for each host, the shards run every
/// job exactly once: the `source` shard and the Linux shards together run every
/// job, and each of the macOS and Windows shard sets runs every `every_host`
/// job.
const shards = [_]Shard{
    .{ .name = "source", .host = .linux, .lane = .source },
    .{ .name = "linux-core", .host = .linux, .lane = .primary, .selection = .{ .before = "run-test-eval" } },
    .{ .name = "linux-eval", .host = .linux, .lane = .primary, .selection = .{ .from = "run-test-eval", .to = "run-test-eval" } },
    .{ .name = "linux-simd", .host = .linux, .lane = .primary, .selection = .{ .from = "run-test-simd-differential", .to = "run-test-eval-host-effects" } },
    .{ .name = "linux-harness", .host = .linux, .lane = .primary, .selection = .{ .after = "run-test-eval-host-effects" } },
    .{ .name = "macos-core", .host = .macos, .lane = .secondary, .selection = .{ .before = "run-test-eval" } },
    .{ .name = "macos-eval", .host = .macos, .lane = .secondary, .selection = .{ .from = "run-test-eval", .to = "run-test-eval" } },
    .{ .name = "macos-harness", .host = .macos, .lane = .secondary, .selection = .{ .after = "run-test-eval" } },
    .{ .name = "windows-core", .host = .windows, .lane = .secondary, .selection = .{ .to = last_module_test_job } },
    .{ .name = "windows-zig", .host = .windows, .lane = .secondary, .selection = .{ .after = last_module_test_job, .before = "run-test-eval" } },
    .{ .name = "windows-eval", .host = .windows, .lane = .secondary, .selection = .{ .from = "run-test-eval", .to = "run-test-eval" } },
    .{ .name = "windows-simd", .host = .windows, .lane = .secondary, .selection = .{ .from = "run-test-simd-differential", .to = "run-test-eval-host-effects" } },
    .{ .name = "windows-harness", .host = .windows, .lane = .secondary, .selection = .{ .after = "run-test-eval-host-effects" } },
};

fn shardByName(name: []const u8) ?Shard {
    for (shards) |shard| {
        if (std.mem.eql(u8, shard.name, name)) return shard;
    }
    return null;
}

const Selection = struct {
    from: ?[]const u8 = null,
    to: ?[]const u8 = null,
    after: ?[]const u8 = null,
    before: ?[]const u8 = null,
};

const SelectionError = error{
    UnknownMiniCiFromJob,
    UnknownMiniCiToJob,
    UnknownMiniCiAfterJob,
    UnknownMiniCiBeforeJob,
    EmptyMiniCiSelection,
};

const ResolvedSelection = struct {
    first: usize,
    last: usize,

    fn includes(self: ResolvedSelection, index: usize) bool {
        return index >= self.first and index <= self.last;
    }
};

const ParsedArgs = struct {
    zig_exe: []const u8,
    build_args: []const []const u8,
    selection: Selection,
    skip_build: bool,
    shard: ?Shard = null,
    verify_workflow: ?[]const u8 = null,
};

/// The `run-test-zig-module-<name>` jobs, in `ModuleType` order. The module
/// inventory in `modules.zig` says which modules have such a step and whether
/// MiniCI runs it, so a new module's tests reach MiniCI without an edit here.
const module_test_jobs = module_test_jobs: {
    var list: []const Job = &.{};
    for (std.enums.values(modules.ModuleType)) |module_type| {
        const name = "run-test-zig-module-" ++ @tagName(module_type);
        switch (module_type.info().tests) {
            .unit => |unit| if (unit.minici) {
                list = list ++ [_]Job{.{ .name = name }};
            },
            .harness => list = list ++ [_]Job{.{ .name = name, .kind = .harness }},
            .no_tests => {},
        }
    }
    break :module_test_jobs list[0..list.len].*;
};

/// The Windows lanes split right after the module tests.
const last_module_test_job = module_test_jobs[module_test_jobs.len - 1].name;

const jobs = [_]Job{
    // `build.zig` keeps build work behind `build-ci`, and MiniCI checks it: a
    // job that runs a compile step it does not list in `expected_compiles`
    // fails the cache-reuse canary (see `cacheReuseFailure`). Keep this list to
    // leaf `run-*` steps. Do not add aliases or aggregate steps that hide
    // useful reporting boundaries.
    //
    // A new job defaults to `.every_host`. Mark it `.source` only when it reads
    // nothing but tracked sources, and `.primary_host` only when its result
    // cannot depend on the host (see `Placement`).
    .{ .name = "run-check-zig-format", .placement = .source },
    .{ .name = "run-check-zig-lints", .placement = .source },
    .{ .name = "run-check-tidy", .placement = .source },
    .{ .name = "run-check-source-bidi", .placement = .source },
    .{ .name = "run-check-git-lints", .placement = .source },
    .{ .name = "run-check-type-checker-patterns", .placement = .source },
    .{ .name = "run-check-enum-from-int-zero", .placement = .source },
    .{ .name = "run-check-unused-suppression", .placement = .source },
    .{ .name = "run-check-semantic-audit", .placement = .source },
    .{ .name = "run-check-postcheck-architecture", .placement = .source },
    .{ .name = "run-check-panic", .placement = .source },
    .{ .name = "run-check-cli-global-stdio", .placement = .source },
    .{ .name = "run-check-test-wiring" },
    .{ .name = "run-check-builtin-format", .placement = .primary_host },
    .{ .name = "run-check-glue-abi" },
    // These three scripts verify properties of emitted code with Linux-only
    // tools and exit at once on any other host, where running them cost a
    // `zig build` start-up each for nothing.
    .{ .name = "run-check-simd-codegen", .placement = .primary_host },
    .{ .name = "run-check-baseline-codegen", .placement = .primary_host },
    .{ .name = "run-check-match-extension-codegen" },
    .{ .name = "run-check-str-eq-same-allocation", .placement = .primary_host },
    .{ .name = "run-check-snapshots" },
    // Builds its own ReleaseSafe shim: the archive it checks is the one a
    // release ships, and everything `build-ci` builds is Debug.
    .{
        .name = "run-check-machine-code-shim-archive",
        .placement = .primary_host,
        .build_args = &.{"-Doptimize=ReleaseSafe"},
    },
    .{ .name = "run-check-test-asset-coverage", .placement = .source },
} ++ module_test_jobs ++ [_]Job{
    .{ .name = "run-test-zig-snapshot-tool" },
    .{ .name = "run-test-zig-builtin-doc" },
    .{ .name = "run-test-zig-cli-main" },
    .{ .name = "run-test-zig-machine-code-shim" },
    .{ .name = "run-test-zig-watch-cli" },
    .{ .name = "run-test-zig-minici" },
    .{ .name = "run-test-zig-fx-platform" },
    .{ .name = "run-test-zig-lir-inline" },
    .{ .name = "run-test-zig-trmc-lir" },
    .{ .name = "run-test-zig-build-helpers" },
    .{ .name = "run-test-zig-backend-llvm" },
    .{
        .name = "run-test-eval",
        .kind = .harness,
        // Each eval process also spawns compiler workers. Avoid overlapping
        // their committed thread stacks on the Windows CI runner (#12116).
        .args = if (builtin.os.tag == .windows)
            &.{ "--timeout", "120000", "--threads", "1" }
        else
            &.{ "--timeout", "120000" },
    },
    .{ .name = "run-test-simd-differential", .kind = .harness },
    // `run-test-eval` leaves LLVM out for speed. This focused run keeps the
    // exact-bit float contract on every host without it.
    .{ .name = "run-test-eval-llvm-float-bits", .kind = .harness },
    // The modules these load are WebAssembly, so one host is enough.
    .{ .name = "run-test-repl-wasm", .placement = .primary_host },
    .{ .name = "run-test-echo-wasm", .placement = .primary_host },
    // www.roc-lang.org refuses an echo.wasm over Cloudflare's 25 MiB asset
    // limit; this catches growth before the website's nightly bump does.
    .{ .name = "run-check-echo-wasm-size", .placement = .primary_host },
    .{ .name = "run-test-eval-host-effects", .kind = .harness },
    .{ .name = "run-test-playground", .kind = .harness },
    .{ .name = "run-test-cli", .kind = .harness },
    .{ .name = "run-test-serialization-sizes" },
    .{ .name = "run-test-builtin-bake-reproducible" },
    .{ .name = "run-test-downstream-package" },
    .{ .name = "run-test-wasm-static-lib" },
    .{ .name = "run-test-dylib" },
    .{ .name = "run-test-archive" },
    .{ .name = "run-coverage-parser" },
};

fn printUsageError(comptime message: []const u8, arg: []const u8) void {
    std.debug.print("MiniCI argument error: " ++ message ++ "\n", .{arg});
    printSelectionUsage();
}

fn printSelectionUsage() void {
    std.debug.print(
        \\MiniCI selection options:
        \\  --minici-from <job>   first MiniCI run job to execute
        \\  --minici-to <job>     last MiniCI run job to execute
        \\  --minici-after <job>  execute MiniCI run jobs after this job
        \\  --minici-before <job> execute MiniCI run jobs before this job
        \\  --minici-only <job>   execute exactly one MiniCI run job
        \\  --minici-shard <name> execute one CI shard (see `shards` in src/build/minici.zig)
        \\  --minici-skip-build   assume `build-ci` already ran and run selected jobs only
        \\  --minici-verify-workflow <path>
        \\                        check that <path> names every CI shard exactly once, then exit
        \\
        \\Other arguments are forwarded to child `zig build` commands.
        \\
    , .{});
}

fn printSelectionConflict(comptime message: []const u8, arg: []const u8) void {
    std.debug.print("MiniCI argument error: " ++ message ++ "\n", .{arg});
    printSelectionUsage();
}

fn setSelectionFrom(selection: *Selection, value: []const u8, arg: []const u8) !void {
    if (selection.from != null or selection.after != null) {
        printSelectionConflict("conflicting lower-bound option `{s}`; use only one of --minici-from or --minici-after", arg);
        return error.InvalidMiniCiArgument;
    }
    selection.from = value;
}

fn setSelectionTo(selection: *Selection, value: []const u8, arg: []const u8) !void {
    if (selection.to != null or selection.before != null) {
        printSelectionConflict("conflicting upper-bound option `{s}`; use only one of --minici-to or --minici-before", arg);
        return error.InvalidMiniCiArgument;
    }
    selection.to = value;
}

fn setSelectionAfter(selection: *Selection, value: []const u8, arg: []const u8) !void {
    if (selection.from != null or selection.after != null) {
        printSelectionConflict("conflicting lower-bound option `{s}`; use only one of --minici-from or --minici-after", arg);
        return error.InvalidMiniCiArgument;
    }
    selection.after = value;
}

fn setSelectionBefore(selection: *Selection, value: []const u8, arg: []const u8) !void {
    if (selection.to != null or selection.before != null) {
        printSelectionConflict("conflicting upper-bound option `{s}`; use only one of --minici-to or --minici-before", arg);
        return error.InvalidMiniCiArgument;
    }
    selection.before = value;
}

fn setShard(parsed_shard: *?Shard, selection: Selection, value: []const u8, arg: []const u8) !void {
    if (parsed_shard.* != null) {
        printSelectionConflict("conflicting shard option `{s}`; use --minici-shard once", arg);
        return error.InvalidMiniCiArgument;
    }
    if (selection.from != null or selection.to != null or selection.after != null or selection.before != null) {
        printSelectionConflict("conflicting selection option `{s}`; --minici-shard cannot be combined with range options", arg);
        return error.InvalidMiniCiArgument;
    }
    parsed_shard.* = shardByName(value) orelse {
        printUsageError("unknown MiniCI shard `{s}`", value);
        return error.InvalidMiniCiArgument;
    };
}

fn rejectRangeWithShard(parsed_shard: ?Shard, arg: []const u8) !void {
    if (parsed_shard != null) {
        printSelectionConflict("conflicting selection option `{s}`; --minici-shard cannot be combined with range options", arg);
        return error.InvalidMiniCiArgument;
    }
}

fn setSelectionOnly(selection: *Selection, value: []const u8, arg: []const u8) !void {
    if (selection.from != null or selection.to != null or selection.after != null or selection.before != null) {
        printSelectionConflict("conflicting selection option `{s}`; --minici-only cannot be combined with range options", arg);
        return error.InvalidMiniCiArgument;
    }
    selection.from = value;
    selection.to = value;
}

/// Fail before doing anything else when this aarch64 machine lacks SHA-256
/// instructions. aarch64 targets have them in their CPU baseline and
/// `src/base/sha256.zig` has no software rounds for them, so on such
/// a machine every artifact minici builds would die of SIGILL the first time it
/// digests a type. The message says so instead. x86_64 machines choose rounds
/// at runtime (`dispatches_at_runtime` in src/base/sha256.zig) or build
/// with the portable rounds (`uses_software_rounds` there, i.e. x86_64 macOS),
/// so they need no instructions and are let through.
fn requireSha256Hardware() void {
    const supported = switch (target.classifyCpuArch(builtin.cpu.arch)) {
        .aarch64 => aarch64HasSha2(),
        .x86_64, .aarch64_be, .arm, .wasm32, .other => true,
    };
    if (supported) return;
    std.debug.print(
        \\MiniCI: this CPU has no SHA-256 instructions (the ARMv8 `sha2` crypto extension).
        \\roc requires them on aarch64: type digests are computed with the CPU's SHA-256
        \\instructions and there are no software rounds for it (see
        \\src/base/sha256.zig and addSha256Floor in build.zig).
        \\An aarch64 CPU without them is not a supported machine for building or running
        \\the roc compiler, so this run stops here rather than failing later with SIGILL.
        \\
    , .{});
    std.process.exit(1);
}

fn aarch64HasSha2() bool {
    if (builtin.cpu.arch != .aarch64) return false;
    // Zig 0.17 declares `std.elf.AT` per OS, so only Linux may name HWCAP.
    if (builtin.os.tag == .linux) {
        // HWCAP_SHA2 is bit 6 of AT_HWCAP on aarch64 Linux.
        return (std.os.linux.getauxval(std.elf.AT.HWCAP) & (1 << 6)) != 0;
    }
    // Every Apple Silicon CPU has the crypto extension, and Zig's macOS
    // aarch64 baseline (apple_m1) already assumes it. Other aarch64 hosts
    // trust the build target, which also requires `sha2`.
    return true;
}

fn parseMiniArgs(allocator: std.mem.Allocator, args: []const []const u8) !ParsedArgs {
    const zig_exe = if (args.len >= 2) args[1] else "zig";
    var build_args = std.ArrayList([]const u8).empty;
    errdefer build_args.deinit(allocator);
    var selection = Selection{};
    var skip_build = false;
    var shard: ?Shard = null;
    var verify_workflow: ?[]const u8 = null;

    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--minici-skip-build")) {
            skip_build = true;
        } else if (std.mem.eql(u8, arg, "--minici-from")) {
            if (i + 1 >= args.len) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            i += 1;
            try rejectRangeWithShard(shard, arg);
            try setSelectionFrom(&selection, args[i], arg);
        } else if (std.mem.startsWith(u8, arg, "--minici-from=")) {
            const value = arg["--minici-from=".len..];
            if (value.len == 0) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            try rejectRangeWithShard(shard, arg);
            try setSelectionFrom(&selection, value, arg);
        } else if (std.mem.eql(u8, arg, "--minici-to")) {
            if (i + 1 >= args.len) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            i += 1;
            try rejectRangeWithShard(shard, arg);
            try setSelectionTo(&selection, args[i], arg);
        } else if (std.mem.startsWith(u8, arg, "--minici-to=")) {
            const value = arg["--minici-to=".len..];
            if (value.len == 0) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            try rejectRangeWithShard(shard, arg);
            try setSelectionTo(&selection, value, arg);
        } else if (std.mem.eql(u8, arg, "--minici-after")) {
            if (i + 1 >= args.len) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            i += 1;
            try rejectRangeWithShard(shard, arg);
            try setSelectionAfter(&selection, args[i], arg);
        } else if (std.mem.startsWith(u8, arg, "--minici-after=")) {
            const value = arg["--minici-after=".len..];
            if (value.len == 0) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            try rejectRangeWithShard(shard, arg);
            try setSelectionAfter(&selection, value, arg);
        } else if (std.mem.eql(u8, arg, "--minici-before")) {
            if (i + 1 >= args.len) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            i += 1;
            try rejectRangeWithShard(shard, arg);
            try setSelectionBefore(&selection, args[i], arg);
        } else if (std.mem.startsWith(u8, arg, "--minici-before=")) {
            const value = arg["--minici-before=".len..];
            if (value.len == 0) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            try rejectRangeWithShard(shard, arg);
            try setSelectionBefore(&selection, value, arg);
        } else if (std.mem.eql(u8, arg, "--minici-only")) {
            if (i + 1 >= args.len) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            i += 1;
            try rejectRangeWithShard(shard, arg);
            try setSelectionOnly(&selection, args[i], arg);
        } else if (std.mem.startsWith(u8, arg, "--minici-only=")) {
            const value = arg["--minici-only=".len..];
            if (value.len == 0) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            try rejectRangeWithShard(shard, arg);
            try setSelectionOnly(&selection, value, arg);
        } else if (std.mem.eql(u8, arg, "--minici-shard")) {
            if (i + 1 >= args.len) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            i += 1;
            try setShard(&shard, selection, args[i], arg);
        } else if (std.mem.startsWith(u8, arg, "--minici-shard=")) {
            const value = arg["--minici-shard=".len..];
            if (value.len == 0) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            try setShard(&shard, selection, value, arg);
        } else if (std.mem.eql(u8, arg, "--minici-verify-workflow")) {
            if (i + 1 >= args.len) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            i += 1;
            verify_workflow = args[i];
        } else if (std.mem.startsWith(u8, arg, "--minici-verify-workflow=")) {
            const value = arg["--minici-verify-workflow=".len..];
            if (value.len == 0) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            verify_workflow = value;
        } else {
            try build_args.append(allocator, arg);
        }
    }

    return .{
        .zig_exe = zig_exe,
        .build_args = try build_args.toOwnedSlice(allocator),
        .selection = if (shard) |chosen| chosen.selection else selection,
        .skip_build = skip_build,
        .shard = shard,
        .verify_workflow = verify_workflow,
    };
}

fn jobIndexByName(name: []const u8) ?usize {
    for (jobs, 0..) |job, i| {
        if (std.mem.eql(u8, job.name, name)) return i;
    }
    return null;
}

fn resolveSelection(selection: Selection) SelectionError!ResolvedSelection {
    const first = if (selection.from) |name|
        jobIndexByName(name) orelse return error.UnknownMiniCiFromJob
    else if (selection.after) |name|
        (jobIndexByName(name) orelse return error.UnknownMiniCiAfterJob) + 1
    else
        0;

    const last = if (selection.to) |name|
        jobIndexByName(name) orelse return error.UnknownMiniCiToJob
    else if (selection.before) |name| blk: {
        const before = jobIndexByName(name) orelse return error.UnknownMiniCiBeforeJob;
        if (before == 0) return error.EmptyMiniCiSelection;
        break :blk before - 1;
    } else jobs.len - 1;

    if (first > last) {
        return error.EmptyMiniCiSelection;
    }

    return .{ .first = first, .last = last };
}

fn printSelectionError(selection: Selection, err: SelectionError) void {
    switch (err) {
        error.UnknownMiniCiFromJob => {
            std.debug.print("MiniCI selection error: unknown --minici-from job `{s}`\n", .{selection.from orelse ""});
        },
        error.UnknownMiniCiToJob => {
            std.debug.print("MiniCI selection error: unknown --minici-to job `{s}`\n", .{selection.to orelse ""});
        },
        error.UnknownMiniCiAfterJob => {
            std.debug.print("MiniCI selection error: unknown --minici-after job `{s}`\n", .{selection.after orelse ""});
        },
        error.UnknownMiniCiBeforeJob => {
            std.debug.print("MiniCI selection error: unknown --minici-before job `{s}`\n", .{selection.before orelse ""});
        },
        error.EmptyMiniCiSelection => {
            const from_name = selection.from orelse "";
            const to_name = selection.to orelse "";
            const after_name = selection.after orelse "";
            const before_name = selection.before orelse "";
            std.debug.print(
                "MiniCI selection error: empty range from `{s}` to `{s}` after `{s}` before `{s}`\n",
                .{ from_name, to_name, after_name, before_name },
            );
        },
    }
}

/// What `zig build --summary all` prints in front of its step counts.
const build_summary_marker = "Build Summary: ";

/// Units Zig prints after a step's duration.
const duration_units = [_][]const u8{ "ns", "us", "ms", "s", "m" };

/// Units Zig prints after a step's peak RSS.
const max_rss_units = [_][]const u8{ "B", "K", "M", "G" };

/// What a step line of a `zig build --summary all` tree says happened to the
/// step. The spellings are the ones Zig's build runner writes (`printTreeStep`,
/// `printStepStatus` and `printStepFailure` in `lib/compiler/Maker.zig`; Zig
/// 0.16 wrote the same ones from `build_runner.zig`, without `transitive skip`).
const StepState = enum {
    /// ` cached`: the step's result came from the cache.
    cached,
    /// ` success`, or a test count with no failure in it: the step ran.
    ran,
    /// ` N errors`, ` failure`, ` w`, or a test count with a failure in it: the
    /// step ran and failed.
    failed,
    /// ` transitive failure`, ` transitive skip`, ` skipped`, or ` skipped (not
    /// enough memory) ...`: the step did not run.
    not_run,
    /// ` (reused)` or ` (+N more reused dependencies)`: the tree reaches a step
    /// it has already printed. The step's state is on its first line.
    repeated,
};

/// `text` without the ASCII digits it ends with, or null when it ends with none.
fn cutTrailingDigits(text: []const u8) ?[]const u8 {
    var end = text.len;
    while (end > 0 and std.ascii.isDigit(text[end - 1])) end -= 1;
    return if (end == text.len) null else text[0..end];
}

/// `text` without a trailing `<before><digits><after>`, or null when it does
/// not end with one. Cutting `" MaxRSS:"` and `"M"` from `x cached 2s
/// MaxRSS:19M` leaves `x cached 2s`.
fn cutNumber(text: []const u8, before: []const u8, after: []const u8) ?[]const u8 {
    const without_after = std.mem.cutSuffix(u8, text, after) orelse return null;
    const without_digits = cutTrailingDigits(without_after) orelse return null;
    return std.mem.cutSuffix(u8, without_digits, before);
}

/// `text` without a trailing `<before><digits><unit>` for one of `units`, or
/// `text` itself when it ends with none: Zig prints a step's duration and peak
/// RSS only when it has them.
fn cutOptionalMeasure(text: []const u8, before: []const u8, units: []const []const u8) []const u8 {
    for (units) |unit| {
        if (cutNumber(text, before, unit)) |rest| return rest;
    }
    return text;
}

/// A test count split off the end of a step line.
const TestCount = struct {
    /// The step line without the count.
    name: []const u8,
    /// Whether the count reports a failing, crashing, timed-out or leaking
    /// test, or logged errors. Zig prints those parts only when non-zero.
    failed: bool,
};

/// Splits a trailing test count off `text`, for example ` 158 pass, 1 skip (159
/// total)` or ` 793 pass, 1 fail (794 total); 2 leaks`. Null when `text` does
/// not end with one.
fn cutTestCount(text: []const u8) ?TestCount {
    var rest = text;
    var failed = false;
    for ([_][]const u8{ " error logs", " leaks" }) |what| {
        if (cutNumber(rest, "; ", what)) |cut| {
            rest = cut;
            failed = true;
        }
    }
    rest = cutNumber(rest, " (", " total)") orelse return null;
    for ([_][]const u8{ " timeout", " crash", " fail" }) |what| {
        if (cutNumber(rest, ", ", what)) |cut| {
            rest = cut;
            failed = true;
        }
    }
    if (cutNumber(rest, ", ", " skip")) |cut| rest = cut;
    const name = cutNumber(rest, " ", " pass") orelse return null;
    return .{ .name = name, .failed = failed };
}

/// One line of a `zig build --summary all` tree: a step and what happened to it.
const SummaryStep = struct {
    name: []const u8,
    state: StepState,

    /// Reads one tree line. Zig prints the tree drawing, the step's name and
    /// then its state, and a step name is free text, so the state is matched
    /// from the end of the line and whatever precedes it is the name. Null
    /// when the line is not one Zig prints; the caller reports that rather
    /// than guessing a state.
    fn parse(line: []const u8) ?SummaryStep {
        // With `--color off` Zig draws one `|  ` or three spaces per ancestor
        // below the root, then `+- `. A root step has no drawing.
        var text = line;
        while (std.mem.cutPrefix(u8, text, "|  ") orelse std.mem.cutPrefix(u8, text, "   ")) |cut| text = cut;
        if (std.mem.cutPrefix(u8, text, "+- ")) |cut| {
            text = cut;
        } else if (text.len != line.len) {
            return null;
        }

        // The tree reached a step it has already printed.
        if (std.mem.cutSuffix(u8, text, " (reused)")) |name| return .{ .name = name, .state = .repeated };
        if (cutNumber(text, " (+", " more reused dependencies)")) |name| return .{ .name = name, .state = .repeated };

        // A step that did not run.
        for ([_][]const u8{ " transitive failure", " transitive skip", " skipped" }) |spelling| {
            if (std.mem.cutSuffix(u8, text, spelling)) |name| return .{ .name = name, .state = .not_run };
        }
        if (cutNumber(text, " exceeded runner limit (", ")")) |rest| {
            if (cutNumber(rest, " skipped (not enough memory) upper bound of ", "")) |name| {
                return .{ .name = name, .state = .not_run };
            }
        }

        // A step that ran and failed. ` transitive failure` is matched above,
        // and ` w` is how Zig 0.16 and 0.17 spell "failed with only stderr".
        if (cutNumber(text, " ", " errors")) |name| return .{ .name = name, .state = .failed };
        for ([_][]const u8{ " failure", " w" }) |spelling| {
            if (std.mem.cutSuffix(u8, text, spelling)) |name| return .{ .name = name, .state = .failed };
        }

        // A step that succeeded, which Zig may follow with the step's duration
        // and then its peak RSS. A failing test count has nothing after it.
        const untimed = cutOptionalMeasure(cutOptionalMeasure(text, " MaxRSS:", &max_rss_units), " ", &duration_units);
        if (std.mem.cutSuffix(u8, untimed, " cached")) |name| return .{ .name = name, .state = .cached };
        if (std.mem.cutSuffix(u8, untimed, " success")) |name| return .{ .name = name, .state = .ran };
        const count = cutTestCount(untimed) orelse return null;
        if (!count.failed) return .{ .name = count.name, .state = .ran };
        return if (untimed.len == text.len) .{ .name = count.name, .state = .failed } else null;
    }
};

/// Whether a step named `name` is one in which Zig's build system runs the
/// compiler itself. `std.Build` names exactly those steps this way:
/// `Step.Compile` (`zig build-exe`, `build-lib`, `build-obj` and `test`) is
/// `compile <kind> <artifact> <optimize> <target>`, and `Step.TranslateC`
/// (`zig translate-c`) is `translate-c`. Both turn source into an artifact the
/// cache keys by its inputs, so either one running after `build-ci` means the
/// cache was not reused.
///
/// A `run ...` step is not compile work even when the command it runs is a
/// compiler (`run zig (roc_builtins.o)`, `run exe builtin_compiler
/// (Builtin.bin)`). The summary says nothing about what a Run step's command
/// does, so such a generator cannot be told apart from the phase's own leaf
/// Run step, which runs every time, without guessing from the command's name.
fn isCompileStep(name: []const u8) bool {
    return std.mem.startsWith(u8, name, "compile ") or std.mem.eql(u8, name, "translate-c");
}

/// The compile work one `zig build <step> --summary all` invocation did, read
/// from its summary tree by `readCompileWork`.
const CompileWork = struct {
    /// How many compile steps came from the cache.
    reused: usize,
    /// The compile steps that ran instead, in tree order, named as Zig prints
    /// them. Distinct steps can share a name, so a name can repeat.
    compiled: []const []const u8,
};

/// What is known about the compile work a command did.
const CompileWorkReading = union(enum) {
    /// The command was skipped, so it has no output to read.
    not_run,
    /// The whole `--summary all` tree was read.
    measured: CompileWork,
    /// The output has no `Build Summary:`. `zig build` prints one whenever it
    /// gets as far as running steps, so only a command that failed before
    /// then is expected to lack it.
    missing_summary,
    /// The summary is not in the shape Zig prints, so no count read from it
    /// can be trusted. Holds what was wrong.
    unreadable_summary: []const u8,
};

/// The step counts of a `Build Summary: 25/27 steps succeeded (1 failed); ...`
/// line. They cover every step of the build, which is also every step the
/// `--summary all` tree prints, so they say whether the whole tree was read.
const StepCounts = struct {
    succeeded: usize,
    total: usize,

    /// Reads the counts from the text after `build_summary_marker`.
    fn parse(text: []const u8) ?StepCounts {
        const counts = std.mem.cut(u8, text, " steps succeeded") orelse return null;
        const numbers = std.mem.cutScalar(u8, counts[0], '/') orelse return null;
        return .{
            .succeeded = std.fmt.parseInt(usize, numbers[0], 10) catch return null,
            .total = std.fmt.parseInt(usize, numbers[1], 10) catch return null,
        };
    }
};

/// Reads the compile work a `zig build <step> --summary all` invocation did
/// from its output: the tree under the last `Build Summary:`. Every tree line
/// has to be one Zig prints, and the tree has to list exactly the steps the
/// summary counts, so an output that was cut short or interleaved is reported
/// as unreadable instead of being counted as far as it goes.
fn readCompileWork(allocator: std.mem.Allocator, output: []const u8) std.mem.Allocator.Error!CompileWorkReading {
    const marker = std.mem.findLast(u8, output, build_summary_marker) orelse return .missing_summary;
    var lines = std.mem.splitScalar(u8, output[marker + build_summary_marker.len ..], '\n');

    const counts_text = std.mem.trimEnd(u8, lines.first(), "\r");
    const counted = StepCounts.parse(counts_text) orelse return .{
        .unreadable_summary = try std.fmt.allocPrint(allocator, "unrecognized step counts in `{s}{s}`", .{ build_summary_marker, counts_text }),
    };

    var listed = StepCounts{ .succeeded = 0, .total = 0 };
    var reused: usize = 0;
    var compiled = std.ArrayList([]const u8).empty;
    while (lines.next()) |raw_line| {
        const line = std.mem.trimEnd(u8, raw_line, "\r");
        // Zig ends the tree with an empty line.
        if (line.len == 0) break;
        const step = SummaryStep.parse(line) orelse return .{
            .unreadable_summary = try std.fmt.allocPrint(allocator, "unrecognized step line `{s}`", .{line}),
        };
        const is_compile = isCompileStep(step.name);
        switch (step.state) {
            // Counted on the line where the step first appears.
            .repeated => continue,
            .cached => {
                listed.succeeded += 1;
                if (is_compile) reused += 1;
            },
            .ran => {
                listed.succeeded += 1;
                if (is_compile) try compiled.append(allocator, try allocator.dupe(u8, step.name));
            },
            // A compile step that failed still ran the compiler.
            .failed => if (is_compile) try compiled.append(allocator, try allocator.dupe(u8, step.name)),
            .not_run => {},
        }
        listed.total += 1;
    }

    if (listed.total != counted.total or listed.succeeded != counted.succeeded) return .{
        .unreadable_summary = try std.fmt.allocPrint(
            allocator,
            "the summary counts {d}/{d} steps succeeded but its tree lists {d}/{d}",
            .{ counted.succeeded, counted.total, listed.succeeded, listed.total },
        ),
    };
    return .{ .measured = .{ .reused = reused, .compiled = try compiled.toOwnedSlice(allocator) } };
}

/// What the cache-reuse canary allows a command to compile.
const CompileExpectation = union(enum) {
    /// Anything: `build-ci` itself, and the phases of a lane that runs no
    /// `build-ci` before them.
    unrestricted,
    /// `build-ci` came first, so only these steps (the job's
    /// `expected_compiles`) may compile.
    only: []const []const u8,
};

/// How a command fails the cache-reuse canary.
const CacheReuseFailure = union(enum) {
    /// `build-ci` came before the phase, yet the phase ran these compile steps
    /// and its job does not declare them in `expected_compiles`.
    undeclared_compiles: []const []const u8,
    /// The command passed without printing a `Build Summary:`, so the compile
    /// work it did is unknown.
    missing_summary,
    /// The command printed a summary that is not in the shape Zig prints.
    /// Holds what was wrong.
    unreadable_summary: []const u8,
};

/// Whether `build-ci` comes before the phases of this invocation, which is
/// when a phase that compiles fails the canary. `--minici-skip-build` does not
/// change the answer: it states that `build-ci` already ran (in CI, in the
/// build job whose `.zig-cache` the shard restored), so its outputs are
/// expected to be there just the same. Only a lane that needs no `build-ci`
/// has phases that build their own tools.
fn buildCiPrecedesPhases(parsed_args: ParsedArgs) bool {
    const shard = parsed_args.shard orelse return true;
    return shard.lane.needsBuildCi();
}

/// Whether `name` is one of `names`, compared exactly.
fn containsName(names: []const []const u8, name: []const u8) bool {
    for (names) |candidate| {
        if (std.mem.eql(u8, candidate, name)) return true;
    }
    return false;
}

/// Decides whether a command fails the cache-reuse canary. An unreadable
/// summary always does, and so does a missing one on a command that passed:
/// the compile work is then unknown, which is never treated as none. A
/// command that failed without a summary is already reported as that failure.
fn cacheReuseFailure(
    allocator: std.mem.Allocator,
    result: CommandResult,
    expectation: CompileExpectation,
) std.mem.Allocator.Error!?CacheReuseFailure {
    switch (result.compile_work) {
        .not_run => return null,
        .missing_summary => return if (isPass(result)) .missing_summary else null,
        .unreadable_summary => |problem| return .{ .unreadable_summary = problem },
        .measured => |work| {
            const expected = switch (expectation) {
                .unrestricted => return null,
                .only => |names| names,
            };
            var undeclared = std.ArrayList([]const u8).empty;
            for (work.compiled) |name| {
                if (!containsName(expected, name)) try undeclared.append(allocator, name);
            }
            if (undeclared.items.len == 0) return null;
            return .{ .undeclared_compiles = try undeclared.toOwnedSlice(allocator) };
        },
    }
}

const CommandResult = struct {
    status: []const u8,
    start_ns: u64,
    end_ns: u64,
    duration_ns: u64,
    log_path: []const u8,
    command: []const []const u8,
    /// The compile steps this command reused and the ones it ran, read from
    /// its build summary.
    compile_work: CompileWorkReading,
    /// Why this command fails the cache-reuse canary, when it does. Set from
    /// `cacheReuseFailure` once the command has run.
    cache_reuse_failure: ?CacheReuseFailure = null,
    /// Whether the command ran with build options of its own (see
    /// `Job.build_args`). Its compile steps then belong to a configuration
    /// `build-ci` did not build, so they are reported apart from the steps the
    /// run was expected to reuse.
    own_configuration: bool = false,
    stats_path: ?[]const u8 = null,
    heartbeat_printed: bool = false,
};

const Progress = struct {
    current: usize,
    total: usize,
};

const SummaryCounts = struct {
    passed: usize = 0,
    failed: usize = 0,
    crashed: usize = 0,
    skipped: usize = 0,
    not_run: usize = 0,
};

fn nowNs(io: std.Io) u64 {
    return @intCast(@max(0, std.Io.Timestamp.now(io, .awake).nanoseconds));
}

fn durationSince(io: std.Io, started: u64) u64 {
    return nowNs(io) -| started;
}

fn unixMs(io: std.Io) u64 {
    return @intCast(@divTrunc(@max(0, std.Io.Timestamp.now(io, .real).nanoseconds), std.time.ns_per_ms));
}

fn seconds(ns: u64) f64 {
    return @as(f64, @floatFromInt(ns)) / 1_000_000_000.0;
}

fn decimalDigits(value: usize) usize {
    var digits: usize = 1;
    var remaining = value;
    while (remaining >= 10) : (remaining /= 10) {
        digits += 1;
    }
    return digits;
}

fn appendProgressPrefix(out: *std.ArrayList(u8), allocator: std.mem.Allocator, progress: Progress) !void {
    try out.appendSlice(allocator, "MiniCI ");
    const width = decimalDigits(progress.total);
    const current_width = decimalDigits(progress.current);
    var padding = width -| current_width;
    while (padding > 0) : (padding -= 1) {
        try out.append(allocator, ' ');
    }
    const text = try std.fmt.allocPrint(allocator, "{d}/{d}: ", .{ progress.current, progress.total });
    defer allocator.free(text);
    try out.appendSlice(allocator, text);
}

fn printProgressPrefix(progress: Progress) void {
    std.debug.print("MiniCI ", .{});
    const width = decimalDigits(progress.total);
    const current_width = decimalDigits(progress.current);
    var padding = width -| current_width;
    while (padding > 0) : (padding -= 1) {
        std.debug.print(" ", .{});
    }
    std.debug.print("{d}/{d}: ", .{ progress.current, progress.total });
}

fn printBuildStart(progress: Progress) void {
    printProgressPrefix(progress);
    std.debug.print("Building CI steps ... ", .{});
}

fn printRunStart(progress: Progress, name: []const u8) void {
    printProgressPrefix(progress);
    std.debug.print("Running `{s}` ... ", .{name});
}

fn isPass(result: CommandResult) bool {
    return std.mem.eql(u8, result.status, "pass");
}

fn isSuccessful(result: CommandResult) bool {
    return isPass(result) or std.mem.eql(u8, result.status, "skip");
}

fn isCheckJob(name: []const u8) bool {
    return std.mem.startsWith(u8, name, "run-check-");
}

fn buildStatusText(result: CommandResult) []const u8 {
    if (isPass(result)) return "completed";
    if (std.mem.eql(u8, result.status, "skip")) return "skipped";
    if (std.mem.eql(u8, result.status, "crash")) return "crashed";
    return "failed";
}

fn runStatusText(result: CommandResult) []const u8 {
    if (isPass(result)) return "passed";
    if (std.mem.eql(u8, result.status, "skip")) return "skipped";
    if (std.mem.eql(u8, result.status, "crash")) return "crashed";
    return "failed";
}

fn printRerunHint(result: CommandResult) void {
    const step_name = if (result.command.len > 2) result.command[2] else "build-ci";
    std.debug.print("  Re-run failed step: `zig build {s} --summary all --color off", .{step_name});
    var i: usize = 0;
    while (i < result.command.len) : (i += 1) {
        if (!std.mem.eql(u8, result.command[i], "--")) continue;

        i += 1;
        var printed_separator = false;
        while (i < result.command.len) : (i += 1) {
            if (std.mem.eql(u8, result.command[i], "--stats-json") and i + 1 < result.command.len) {
                i += 1;
                continue;
            }
            if (!printed_separator) {
                std.debug.print(" --", .{});
                printed_separator = true;
            }
            std.debug.print(" {s}", .{result.command[i]});
        }
        break;
    }
    std.debug.print("`\n", .{});
    std.debug.print("  Log: `{s}`\n", .{result.log_path});
}

/// Prints each line of `bytes` indented so the echoed output is visually set
/// apart from the orchestrator's own progress lines.
fn printIndentedLines(bytes: []const u8) void {
    var lines = std.mem.splitScalar(u8, bytes, '\n');
    while (lines.next()) |line| {
        std.debug.print("  | {s}\n", .{line});
    }
}

/// Byte offset of `line` (a subslice of `log`) within `log`. Relies on the
/// split iterators yielding subslices that point back into `log`.
fn lineOffset(log: []const u8, line: []const u8) usize {
    return @intFromPtr(line.ptr) - @intFromPtr(log.ptr);
}

/// A test harness summary line, e.g. `519 passed, 1 run failed, 32 skipped
/// (552 total) in 353090ms using 12 worker(s)`. We only treat it as the core
/// marker when it reports at least one failure, so a clean "all passed" summary
/// from a job that still failed for infrastructure reasons falls through to the
/// full-log fallback where the real error lives. A trailing harness token
/// (`total)`, `worker`, `process`, `wall`) is required so an incidental "N
/// passed, M failed" line in a test's own captured output does not match.
fn isTestSummaryLine(line: []const u8) bool {
    const t = std.mem.trim(u8, line, " \t\r");
    if (t.len == 0 or !std.ascii.isDigit(t[0])) return false;
    if (std.mem.find(u8, t, " passed") == null) return false;
    const reports_failure = std.mem.find(u8, t, "failed") != null or
        std.mem.find(u8, t, "crashed") != null or
        std.mem.find(u8, t, "timed out") != null;
    if (!reports_failure) return false;
    return std.mem.find(u8, t, "total)") != null or
        std.mem.find(u8, t, "worker") != null or
        std.mem.find(u8, t, "process") != null or
        std.mem.find(u8, t, "wall") != null;
}

/// Byte offset of the first line whose text (after leading spaces) begins with
/// `prefix`, or null if no such line exists.
fn findFirstLineStartingWith(log: []const u8, prefix: []const u8) ?usize {
    var it = std.mem.splitScalar(u8, log, '\n');
    while (it.next()) |line| {
        if (std.mem.startsWith(u8, std.mem.trimStart(u8, line, " "), prefix)) return lineOffset(log, line);
    }
    return null;
}

/// A `--summary` tree child line, e.g. `+- compile test ...` (optionally
/// indented under its parent).
fn isTreeChild(line: []const u8) bool {
    return std.mem.startsWith(u8, std.mem.trimStart(u8, line, " "), "+-");
}

/// The root line of a failing-step tree fragment: a non-empty, non-indented line
/// that is not itself a tree node (e.g. `build-ci`, `run-test-zig-module-...`).
fn isMiniTreeRoot(line: []const u8) bool {
    return line.len != 0 and line[0] != ' ' and line[0] != '\t' and line[0] != '+';
}

/// Region from the start of the log through the first failure summary line.
/// Everything after (suite/timing tables and the `--summary all` build tree) is
/// noise for a harness failure.
fn findTestSummaryRegion(log: []const u8) ?[]const u8 {
    var it = std.mem.splitScalar(u8, log, '\n');
    while (it.next()) |line| {
        if (isTestSummaryLine(line)) return log[0 .. lineOffset(log, line) + line.len];
    }
    return null;
}

/// Region spanning a failed `zig build` step: from the failing step's tree
/// fragment (`<step>` followed by `+- ...`) through the last line before the
/// `failed command:` marker. This drops the leading success spam from parallel
/// steps and the trailing `--summary all` dependency tree, and covers compiler
/// errors, unit-test failures, panics, and check-tool failures alike.
fn findZigBuildFailureRegion(log: []const u8) ?[]const u8 {
    // Output for the failing step ends at its `failed command:` line; without one
    // (rare), fall back to the build summary that precedes the dependency tree.
    const end = findFirstLineStartingWith(log, "failed command:") orelse
        findFirstLineStartingWith(log, "Build Summary:") orelse
        return null;

    // The failing step's tree fragment is the last `<root>` + `+- ...` pair before
    // that marker; taking the last keeps us closest to the actual error.
    var root: ?usize = null;
    var prev_off: usize = 0;
    var prev_line: []const u8 = "";
    var have_prev = false;
    var it = std.mem.splitScalar(u8, log, '\n');
    while (it.next()) |line| {
        const off = lineOffset(log, line);
        if (off >= end) break;
        if (isTreeChild(line) and have_prev and isMiniTreeRoot(prev_line)) root = prev_off;
        prev_off = off;
        prev_line = line;
        have_prev = true;
    }
    const start = root orelse return null;
    return std.mem.trimEnd(u8, log[start..end], "\n \t\r");
}

/// Extracts the core error region from a failing step's log, or null when no
/// known failure shape matches (the caller then shows the head/tail fallback).
/// The harness-summary shape is tried first because harness jobs also end with a
/// `failed command:` marker whose tree fragment sits below the useful summary.
fn findCoreError(log: []const u8) ?[]const u8 {
    if (findTestSummaryRegion(log)) |region| return region;
    if (findZigBuildFailureRegion(log)) |region| return region;
    return null;
}

/// Echoes a failing step's captured output to the console so the failure is
/// actionable without re-running the step. CI runners discard the workspace, so
/// a failed step's log file is unreachable there; echoing puts the actual error
/// into the GitHub log (and in front of any retry wrapper matching on output).
/// It first tries to extract just the core error (a harness summary, or a failed
/// `zig build` step's error output); when no known shape matches it falls back to
/// showing the head and tail of the whole log with the noisy middle elided. The
/// full output always remains in `result.log_path`, which `printRerunHint`
/// points at.
fn printFailureLog(allocator: std.mem.Allocator, io: std.Io, result: CommandResult) void {
    const contents = std.Io.Dir.cwd().readFileAlloc(io, result.log_path, allocator, .limited(256 * 1024 * 1024)) catch |err| {
        std.debug.print("  (could not read log `{s}`: {s})\n", .{ result.log_path, @errorName(err) });
        return;
    };
    defer allocator.free(contents);

    const trimmed = std.mem.trimEnd(u8, contents, "\n");
    if (trimmed.len == 0) {
        std.debug.print("  (`{s}` produced no output)\n", .{commandStepName(result.command)});
        return;
    }

    const region = findCoreError(trimmed) orelse trimmed;
    const extracted = region.len != trimmed.len;

    std.debug.print("  --- output from `{s}` ---\n", .{commandStepName(result.command)});
    if (region.len <= failure_log_head_bytes + failure_log_tail_bytes) {
        printIndentedLines(region);
        if (extracted) std.debug.print("  ... (extracted error; full log: `{s}`) ...\n", .{result.log_path});
    } else {
        // Trim the head back to a line boundary so it does not end mid-line.
        var head: []const u8 = region[0..failure_log_head_bytes];
        if (std.mem.findScalarLast(u8, head, '\n')) |nl| head = head[0..nl];
        // Advance the tail to the next line boundary so it does not start mid-line.
        var tail: []const u8 = region[region.len - failure_log_tail_bytes ..];
        if (std.mem.findScalar(u8, tail, '\n')) |nl| tail = tail[nl + 1 ..];

        const omitted = region.len - head.len - tail.len;
        printIndentedLines(head);
        std.debug.print("  ... {d} KiB omitted (full log: `{s}`) ...\n", .{ omitted / 1024, result.log_path });
        printIndentedLines(tail);
    }
    std.debug.print("  --- end output ---\n", .{});
}

fn heartbeatIntervalMs(env: *const std.process.Environ.Map) u64 {
    const raw = env.get(heartbeat_env) orelse return default_heartbeat_interval_ms;
    if (raw.len == 0) return default_heartbeat_interval_ms;
    return std.fmt.parseInt(u64, raw, 10) catch |err| {
        std.debug.print("invalid {s}='{s}': {s}; using default {d}ms\n", .{ heartbeat_env, raw, @errorName(err), default_heartbeat_interval_ms });
        return default_heartbeat_interval_ms;
    };
}

fn commandStepName(argv: []const []const u8) []const u8 {
    return if (argv.len > 2) argv[2] else argv[0];
}

fn addResultToSummary(counts: *SummaryCounts, result: CommandResult) void {
    if (isPass(result)) {
        counts.passed += 1;
    } else if (std.mem.eql(u8, result.status, "skip")) {
        counts.skipped += 1;
    } else if (std.mem.eql(u8, result.status, "crash")) {
        counts.crashed += 1;
    } else {
        counts.failed += 1;
    }
}

fn summaryCounts(total_phases: usize, build_result: CommandResult, results: []const CommandResult) SummaryCounts {
    var counts = SummaryCounts{};
    addResultToSummary(&counts, build_result);
    for (results) |result| {
        addResultToSummary(&counts, result);
    }
    const ran = 1 + results.len;
    counts.not_run = total_phases -| ran;
    return counts;
}

fn appendSummaryLine(
    out: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    total_phases: usize,
    build_result: CommandResult,
    results: []const CommandResult,
    wall_ns: u64,
) !void {
    const counts = summaryCounts(total_phases, build_result, results);
    const ran = total_phases - counts.not_run;
    const base = try std.fmt.allocPrint(
        allocator,
        "MiniCI summary: {d}/{d} phases ran; {d} passed, {d} failed, {d} crashed, {d} skipped",
        .{ ran, total_phases, counts.passed, counts.failed, counts.crashed, counts.skipped },
    );
    defer allocator.free(base);
    try out.appendSlice(allocator, base);
    if (counts.not_run != 0) {
        const not_run = try std.fmt.allocPrint(allocator, ", {d} not run", .{counts.not_run});
        defer allocator.free(not_run);
        try out.appendSlice(allocator, not_run);
    }
    const suffix = try std.fmt.allocPrint(allocator, "; wall {d:.3}s\n", .{seconds(wall_ns)});
    defer allocator.free(suffix);
    try out.appendSlice(allocator, suffix);
}

/// The plural ending for a count of things: `1 phase`, `2 phases`.
fn pluralS(count: usize) []const u8 {
    return if (count == 1) "" else "s";
}

/// How much of a command's compile work to show under its result line.
const CompileWorkDetail = enum {
    /// The two counts only. For `build-ci`, whose job is to compile: the names
    /// of its steps stay in `report.json` rather than filling the console.
    counts,
    /// The counts and the name of every compile step that ran, shown only
    /// when at least one did. For the run phases, which are meant to reuse
    /// what `build-ci` built.
    steps,
};

/// Appends what belongs right under a command's result line: the compile
/// steps it ran, and why it fails the cache-reuse canary if it does. A reader
/// of the job log then sees a cache that was not reused without opening
/// `report.json`. `expected_compiles` is the job's list, used to mark the
/// steps the job declares.
fn appendCompileWorkReport(
    out: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    result: CommandResult,
    detail: CompileWorkDetail,
    expected_compiles: []const []const u8,
) !void {
    switch (result.compile_work) {
        .not_run, .missing_summary, .unreadable_summary => {},
        .measured => |work| switch (detail) {
            .counts => try out.print(allocator, "  Compile steps: {d} reused, {d} compiled\n", .{ work.reused, work.compiled.len }),
            .steps => if (work.compiled.len != 0) {
                try out.print(allocator, "  Compile steps: {d} reused, {d} compiled:\n", .{ work.reused, work.compiled.len });
                for (work.compiled) |name| {
                    const note = if (containsName(expected_compiles, name)) " (declared in `expected_compiles`)" else "";
                    try out.print(allocator, "    {s}{s}\n", .{ name, note });
                }
            },
        },
    }
    const failure = result.cache_reuse_failure orelse return;
    switch (failure) {
        // The steps are the unmarked ones listed just above.
        .undeclared_compiles => |names| try out.print(
            allocator,
            "  Cache not reused: `build-ci` should have built {d} of these.\n",
            .{names.len},
        ),
        .missing_summary, .unreadable_summary => try appendCacheReuseFailure(out, allocator, result),
    }
}

/// Prints `appendCompileWorkReport`'s lines for a command that just finished.
fn printCompileWorkReport(
    allocator: std.mem.Allocator,
    result: CommandResult,
    detail: CompileWorkDetail,
    expected_compiles: []const []const u8,
) !void {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(allocator);
    try appendCompileWorkReport(&out, allocator, result, detail, expected_compiles);
    std.debug.print("{s}", .{out.items});
}

/// Appends the final summary's entry for a command that fails the cache-reuse
/// canary: its name, why it fails, and the steps it should not have compiled.
/// Appends nothing for a command that does not fail it.
fn appendCacheReuseFailure(out: *std.ArrayList(u8), allocator: std.mem.Allocator, result: CommandResult) !void {
    const failure = result.cache_reuse_failure orelse return;
    const name = commandStepName(result.command);
    switch (failure) {
        .undeclared_compiles => |names| {
            try out.print(
                allocator,
                "  `{s}` compiled {d} step{s} that `build-ci` should have built:\n",
                .{ name, names.len, pluralS(names.len) },
            );
            for (names) |step| try out.print(allocator, "    {s}\n", .{step});
        },
        .missing_summary => try out.print(
            allocator,
            "  `{s}` passed without printing a `Build Summary:`, so its compile work is unknown\n",
            .{name},
        ),
        .unreadable_summary => |problem| try out.print(
            allocator,
            "  `{s}` printed a build summary MiniCI cannot read: {s}\n",
            .{ name, problem },
        ),
    }
}

/// Appends the cache-reuse part of the final summary. Its first line reads
/// differently from the phase tally in `appendSummaryLine`, so a run that
/// passed every phase but did not reuse its cache is not mistaken for a test
/// failure; each offending command and its steps follow. The totals cover the
/// run phases only: `build-ci` is where compile steps are meant to run.
fn appendCacheReuseSummary(
    out: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    enforced: bool,
    build_result: CommandResult,
    results: []const CommandResult,
) !void {
    var measured: usize = 0;
    var reused: usize = 0;
    var compiled: usize = 0;
    var own_configuration_compiled: usize = 0;
    var failures: usize = @intFromBool(build_result.cache_reuse_failure != null);
    var undeclared = false;
    for (results) |result| {
        switch (result.compile_work) {
            .measured => |work| if (result.own_configuration) {
                own_configuration_compiled += work.compiled.len;
            } else {
                measured += 1;
                reused += work.reused;
                compiled += work.compiled.len;
            },
            .not_run, .missing_summary, .unreadable_summary => {},
        }
        const failure = result.cache_reuse_failure orelse continue;
        failures += 1;
        switch (failure) {
            .undeclared_compiles => undeclared = true,
            .missing_summary, .unreadable_summary => {},
        }
    }

    try out.appendSlice(allocator, "MiniCI cache reuse: ");
    if (failures != 0) {
        try out.print(allocator, "FAILED in {d} phase{s}", .{ failures, pluralS(failures) });
    } else if (measured == 0) {
        try out.appendSlice(allocator, "no run phase was measured\n");
        return;
    } else if (enforced) {
        try out.appendSlice(allocator, "ok");
    } else {
        try out.appendSlice(allocator, "not enforced (no `build-ci` before these phases)");
    }
    try out.print(
        allocator,
        "; {d} run phase{s} measured, {d} compile step{s} reused, {d} compiled{s}\n",
        .{
            measured,
            pluralS(measured),
            reused,
            pluralS(reused),
            compiled,
            if (enforced and failures == 0 and compiled != 0) " (all declared in `expected_compiles`)" else "",
        },
    );
    if (own_configuration_compiled != 0) try out.print(
        allocator,
        "  Not counted above: {d} compile step{s} in jobs that build a configuration of their own.\n",
        .{ own_configuration_compiled, pluralS(own_configuration_compiled) },
    );

    try appendCacheReuseFailure(out, allocator, build_result);
    for (results) |result| try appendCacheReuseFailure(out, allocator, result);
    if (undeclared) try out.appendSlice(allocator,
        \\  Either `build-ci` does not build these steps (see build.zig), or what it built was not reusable here.
        \\  A job that is meant to compile a step says so in `expected_compiles` (src/build/minici.zig).
        \\
    );
}

fn printSummary(
    allocator: std.mem.Allocator,
    total_phases: usize,
    cache_reuse_enforced: bool,
    build_result: CommandResult,
    results: []const CommandResult,
    wall_ns: u64,
) !void {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(allocator);
    try appendSummaryLine(&out, allocator, total_phases, build_result, results, wall_ns);
    try appendCacheReuseSummary(&out, allocator, cache_reuse_enforced, build_result, results);
    std.debug.print("{s}", .{out.items});
}

const Heartbeat = struct {
    io: std.Io,
    argv: []const []const u8,
    started: u64,
    interval_ms: u64,
    progress: Progress,
    done: std.atomic.Value(bool),
    printed: std.atomic.Value(bool),

    fn run(self: *@This()) void {
        if (self.interval_ms == 0) return;

        var next_ms = self.interval_ms;
        while (!self.done.load(.acquire)) {
            std.Io.sleep(self.io, std.Io.Duration.fromMilliseconds(500), .awake) catch {};
            if (self.done.load(.acquire)) return;

            const elapsed_ms = durationSince(self.io, self.started) / std.time.ns_per_ms;
            if (elapsed_ms < next_ms) continue;

            const already_printed = self.printed.swap(true, .acq_rel);
            if (!already_printed) std.debug.print("\n", .{});
            printProgressPrefix(self.progress);
            std.debug.print("still running `{s}` after {d:.1}s\n", .{
                commandStepName(self.argv),
                seconds(elapsed_ms * std.time.ns_per_ms),
            });
            next_ms += self.interval_ms;
        }
    }
};

fn writeFile(io: std.Io, path: []const u8, bytes: []const u8) !void {
    if (std.fs.path.dirname(path)) |dir| {
        try std.Io.Dir.cwd().createDirPath(io, dir);
    }
    var file = try std.Io.Dir.cwd().createFile(io, path, .{});
    defer file.close(io);
    try file.writeStreamingAll(io, bytes);
}

fn appendJsonString(out: *std.ArrayList(u8), allocator: std.mem.Allocator, value: []const u8) !void {
    try out.append(allocator, '"');
    for (value) |byte| {
        switch (byte) {
            '"' => try out.appendSlice(allocator, "\\\""),
            '\\' => try out.appendSlice(allocator, "\\\\"),
            '\n' => try out.appendSlice(allocator, "\\n"),
            '\r' => try out.appendSlice(allocator, "\\r"),
            '\t' => try out.appendSlice(allocator, "\\t"),
            else => {
                if (byte < 0x20) {
                    const escaped = try std.fmt.allocPrint(allocator, "\\u{x:0>4}", .{byte});
                    defer allocator.free(escaped);
                    try out.appendSlice(allocator, escaped);
                } else {
                    try out.append(allocator, byte);
                }
            },
        }
    }
    try out.append(allocator, '"');
}

fn appendU64(out: *std.ArrayList(u8), allocator: std.mem.Allocator, value: u64) !void {
    const text = try std.fmt.allocPrint(allocator, "{d}", .{value});
    defer allocator.free(text);
    try out.appendSlice(allocator, text);
}

fn runCommand(
    allocator: std.mem.Allocator,
    io: std.Io,
    argv: []const []const u8,
    log_path: []const u8,
    heartbeat_interval_ms: u64,
    run_started_ns: u64,
    progress: Progress,
) !CommandResult {
    const started = nowNs(io);
    var heartbeat = Heartbeat{
        .io = io,
        .argv = argv,
        .started = started,
        .interval_ms = heartbeat_interval_ms,
        .progress = progress,
        .done = std.atomic.Value(bool).init(false),
        .printed = std.atomic.Value(bool).init(false),
    };
    const heartbeat_thread = if (heartbeat.interval_ms == 0)
        null
    else
        std.Thread.spawn(.{}, Heartbeat.run, .{&heartbeat}) catch null;
    defer {
        heartbeat.done.store(true, .release);
        if (heartbeat_thread) |thread| thread.join();
    }

    const result = std.process.run(allocator, io, .{ .argv = argv }) catch |err| {
        const ended = nowNs(io);
        const message = try std.fmt.allocPrint(allocator, "spawn failed: {s}\n", .{@errorName(err)});
        try writeFile(io, log_path, message);
        return .{
            .status = "crash",
            .start_ns = started -| run_started_ns,
            .end_ns = ended -| run_started_ns,
            .duration_ns = ended -| started,
            .log_path = log_path,
            .command = argv,
            // Nothing ran, so nothing printed a summary.
            .compile_work = .missing_summary,
            .heartbeat_printed = heartbeat.printed.load(.acquire),
        };
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    var log = std.ArrayList(u8).empty;
    defer log.deinit(allocator);
    try log.appendSlice(allocator, result.stdout);
    try log.appendSlice(allocator, result.stderr);
    try writeFile(io, log_path, log.items);

    const status: []const u8 = switch (result.term) {
        .exited => |code| if (code == 0) "pass" else "fail",
        .signal, .stopped, .unknown => "crash",
    };
    const ended = nowNs(io);

    return .{
        .status = status,
        .start_ns = started -| run_started_ns,
        .end_ns = ended -| run_started_ns,
        .duration_ns = ended -| started,
        .log_path = log_path,
        .command = argv,
        .compile_work = try readCompileWork(allocator, log.items),
        .heartbeat_printed = heartbeat.printed.load(.acquire),
    };
}

/// Gives the cache entries `build-ci` just wrote the stat of the files they
/// name, so that the run jobs reuse them.
///
/// Zig 0.17.0 misses the cache for a declared input whose recorded stat does
/// not match the file, whatever the file contains (see
/// ci/restamp_zig_cache.zig), and `build-ci` leaves such records behind: an
/// input recorded in the clock tick it was written in gets a zeroed stat. A
/// generated host library copied into a fixture tree the moment it exists is
/// often that new, and the first run job to check it then rebuilds the tree
/// and everything built from it.
///
/// Only this build's own cache is re-stamped. Other builds on a developer's
/// machine may be reading the global one.
fn restampBuildCache(allocator: std.mem.Allocator, io: std.Io, zig_exe: []const u8) !void {
    const started = nowNs(io);
    const argv: []const []const u8 = &.{ restamp_exe, "--zig", zig_exe, "--local-cache-only" };
    std.debug.print("MiniCI: re-stamping the Zig cache `build-ci` wrote ... ", .{});

    const result = std.process.run(allocator, io, .{ .argv = argv }) catch |err| {
        std.debug.print("crashed\n  could not run `{s}`: {s}\n", .{ restamp_exe, @errorName(err) });
        std.process.exit(1);
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    const log = try std.mem.concat(allocator, u8, &.{ result.stdout, result.stderr });
    defer allocator.free(log);
    try writeFile(io, restamp_log, log);

    const passed = switch (result.term) {
        .exited => |code| code == 0,
        .signal, .stopped, .unknown => false,
    };
    if (!passed) {
        std.debug.print("failed\n  --- output from `{s}` ---\n", .{restamp_exe});
        printIndentedLines(std.mem.trimEnd(u8, log, "\n"));
        std.process.exit(1);
    }
    std.debug.print("done in {d:.3}s\n", .{seconds(durationSince(io, started))});
}

fn skipCommand(
    io: std.Io,
    argv: []const []const u8,
    log_path: []const u8,
    reason: []const u8,
    run_started_ns: u64,
) !CommandResult {
    const started = nowNs(io);
    try writeFile(io, log_path, reason);
    const ended = nowNs(io);
    return .{
        .status = "skip",
        .start_ns = started -| run_started_ns,
        .end_ns = ended -| run_started_ns,
        .duration_ns = ended -| started,
        .log_path = log_path,
        .command = argv,
        .compile_work = .not_run,
    };
}

fn buildCommand(
    allocator: std.mem.Allocator,
    zig_exe: []const u8,
    build_args: []const []const u8,
    step: []const u8,
    max_jobs: ?usize,
    stats_path: ?[]const u8,
    run_args: []const []const u8,
) ![]const []const u8 {
    var argv = std.ArrayList([]const u8).empty;
    try argv.append(allocator, zig_exe);
    try argv.append(allocator, "build");
    try argv.append(allocator, step);
    try argv.append(allocator, "--summary");
    try argv.append(allocator, "all");
    try argv.append(allocator, "--color");
    try argv.append(allocator, "off");
    if (max_jobs) |jobs_count| {
        try argv.append(allocator, try std.fmt.allocPrint(allocator, "-j{d}", .{jobs_count}));
    }
    for (build_args) |arg| {
        try argv.append(allocator, arg);
    }
    if (stats_path != null or run_args.len != 0) {
        try argv.append(allocator, "--");
    }
    if (stats_path) |path| {
        try argv.append(allocator, "--stats-json");
        try argv.append(allocator, path);
    }
    for (run_args) |arg| {
        try argv.append(allocator, arg);
    }
    return try argv.toOwnedSlice(allocator);
}

fn appendJsonStrings(out: *std.ArrayList(u8), allocator: std.mem.Allocator, strings: []const []const u8) !void {
    try out.appendSlice(allocator, "[");
    for (strings, 0..) |string, i| {
        if (i > 0) try out.appendSlice(allocator, ", ");
        try appendJsonString(out, allocator, string);
    }
    try out.appendSlice(allocator, "]");
}

fn writeReportJson(
    allocator: std.mem.Allocator,
    io: std.Io,
    run_started_unix_ms: u64,
    cache_reuse_enforced: bool,
    build_result: CommandResult,
    results: []const CommandResult,
) !void {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(allocator);

    try appendReportJsonObject(&out, allocator, run_started_unix_ms, cache_reuse_enforced, build_result, results);
    try out.appendSlice(allocator, "\n");
    try writeFile(io, out_dir ++ "/report.json", out.items);
}

/// Appends a command's `compile_work`: `summary` says how its build summary
/// read, and the counts are present only when it was measured, so an unknown
/// count is never written as zero.
fn appendCompileWorkJson(out: *std.ArrayList(u8), allocator: std.mem.Allocator, reading: CompileWorkReading) !void {
    try out.appendSlice(allocator, "{\"summary\": ");
    try appendJsonString(out, allocator, @tagName(reading));
    switch (reading) {
        .not_run, .missing_summary => {},
        .measured => |work| {
            try out.print(allocator, ", \"reused\": {d}, \"compiled\": {d}, \"compiled_steps\": ", .{ work.reused, work.compiled.len });
            try appendJsonStrings(out, allocator, work.compiled);
        },
        .unreadable_summary => |problem| {
            try out.appendSlice(allocator, ", \"problem\": ");
            try appendJsonString(out, allocator, problem);
        },
    }
    try out.appendSlice(allocator, "}");
}

/// Appends a command's `cache_reuse_failure`: null when it does not fail the
/// canary, otherwise the kind of failure and what it is about.
fn appendCacheReuseFailureJson(out: *std.ArrayList(u8), allocator: std.mem.Allocator, maybe_failure: ?CacheReuseFailure) !void {
    const failure = maybe_failure orelse return out.appendSlice(allocator, "null");
    try out.appendSlice(allocator, "{\"kind\": ");
    try appendJsonString(out, allocator, @tagName(failure));
    switch (failure) {
        .undeclared_compiles => |names| {
            try out.appendSlice(allocator, ", \"steps\": ");
            try appendJsonStrings(out, allocator, names);
        },
        .missing_summary => {},
        .unreadable_summary => |problem| {
            try out.appendSlice(allocator, ", \"problem\": ");
            try appendJsonString(out, allocator, problem);
        },
    }
    try out.appendSlice(allocator, "}");
}

fn appendResultJson(out: *std.ArrayList(u8), allocator: std.mem.Allocator, result: CommandResult) !void {
    try out.appendSlice(allocator, "{\n    \"status\": ");
    try appendJsonString(out, allocator, result.status);
    try out.appendSlice(allocator, ",\n    \"start_ns\": ");
    try appendU64(out, allocator, result.start_ns);
    try out.appendSlice(allocator, ",\n    \"end_ns\": ");
    try appendU64(out, allocator, result.end_ns);
    try out.appendSlice(allocator, ",\n    \"duration_ns\": ");
    try appendU64(out, allocator, result.duration_ns);
    try out.appendSlice(allocator, ",\n    \"log_path\": ");
    try appendJsonString(out, allocator, result.log_path);
    try out.appendSlice(allocator, ",\n    \"command\": ");
    try appendJsonStrings(out, allocator, result.command);
    try out.appendSlice(allocator, ",\n    \"stats_path\": ");
    if (result.stats_path) |path| {
        try appendJsonString(out, allocator, path);
    } else {
        try out.appendSlice(allocator, "null");
    }
    try out.appendSlice(allocator, ",\n    \"compile_work\": ");
    try appendCompileWorkJson(out, allocator, result.compile_work);
    try out.appendSlice(allocator, ",\n    \"cache_reuse_failure\": ");
    try appendCacheReuseFailureJson(out, allocator, result.cache_reuse_failure);
    try out.appendSlice(allocator, "\n  }");
}

fn appendScriptJsonBytes(out: *std.ArrayList(u8), allocator: std.mem.Allocator, bytes: []const u8) !void {
    for (bytes) |byte| {
        switch (byte) {
            '<' => try out.appendSlice(allocator, "\\u003c"),
            '>' => try out.appendSlice(allocator, "\\u003e"),
            '&' => try out.appendSlice(allocator, "\\u0026"),
            else => try out.append(allocator, byte),
        }
    }
}

/// Appends the report object that `report.json` holds and `index.html` embeds.
/// Schema version 2 added `cache_reuse_enforced`, and `compile_work` and
/// `cache_reuse_failure` on `build_ci` and on every job.
fn appendReportJsonObject(
    out: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    run_started_unix_ms: u64,
    cache_reuse_enforced: bool,
    build_result: CommandResult,
    results: []const CommandResult,
) !void {
    try out.appendSlice(allocator, "{\n  \"schema_version\": 2,\n  \"run_started_unix_ms\": ");
    try appendU64(out, allocator, run_started_unix_ms);
    try out.appendSlice(allocator, ",\n  \"cache_reuse_enforced\": ");
    try out.appendSlice(allocator, if (cache_reuse_enforced) "true" else "false");
    try out.appendSlice(allocator, ",\n  \"build_ci\": ");
    try appendResultJson(out, allocator, build_result);
    try out.appendSlice(allocator, ",\n  \"jobs\": [\n");
    for (results, 0..) |result, i| {
        if (i > 0) try out.appendSlice(allocator, ",\n");
        try appendResultJson(out, allocator, result);
    }
    try out.appendSlice(allocator, "\n  ]\n}");
}

fn appendStatsJsonObject(
    out: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    io: std.Io,
    results: []const CommandResult,
) !void {
    try out.appendSlice(allocator, "{\n");
    var first = true;
    for (results) |result| {
        const stats_path = result.stats_path orelse continue;
        if (!first) try out.appendSlice(allocator, ",\n");
        first = false;
        try out.appendSlice(allocator, "  ");
        try appendJsonString(out, allocator, result.command[2]);
        try out.appendSlice(allocator, ": ");
        const stats = std.Io.Dir.cwd().readFileAlloc(io, stats_path, allocator, .limited(256 * 1024 * 1024)) catch {
            try out.appendSlice(allocator, "null");
            continue;
        };
        defer allocator.free(stats);
        try appendScriptJsonBytes(out, allocator, stats);
    }
    try out.appendSlice(allocator, "\n}");
}

fn writeHtml(
    allocator: std.mem.Allocator,
    io: std.Io,
    run_started_unix_ms: u64,
    cache_reuse_enforced: bool,
    build_result: CommandResult,
    results: []const CommandResult,
) !void {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(allocator);

    try out.appendSlice(allocator,
        \\<!doctype html>
        \\<html lang="en">
        \\<head>
        \\  <meta charset="utf-8">
        \\  <meta name="viewport" content="width=device-width, initial-scale=1">
        \\  <title>MiniCI</title>
        \\  <style>
        \\    :root{color-scheme:light;--bg:#f6f7f9;--panel:#fff;--text:#15181d;--muted:#68707d;--line:#d9dee6;--line-soft:#edf0f4;--pass:#16834a;--fail:#b42318;--skip:#737b87;--bar:#356fb8;--select:#101828;--track:#eef2f6}
        \\    *{box-sizing:border-box}
        \\    body{margin:0;background:var(--bg);color:var(--text);font-family:system-ui,-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif;font-size:13px;line-height:1.4}
        \\    header{position:sticky;top:0;z-index:10;background:#fff;border-bottom:1px solid var(--line);padding:14px 20px}
        \\    h1{margin:0;font-size:20px;font-weight:700}
        \\    h2{margin:0 0 10px;font-size:15px;font-weight:700}
        \\    h3{margin:0 0 8px;font-size:13px;font-weight:700}
        \\    main{padding:16px 20px 28px;display:grid;grid-template-columns:minmax(0,1fr)360px;gap:16px;max-width:1800px;margin:0 auto}
        \\    .summary{display:flex;flex-wrap:wrap;gap:12px;margin-top:10px}
        \\    .metric{display:flex;gap:6px;align-items:baseline}
        \\    .metric b{font-size:16px}.metric span{color:var(--muted);font-size:12px;text-transform:uppercase;letter-spacing:.04em}
        \\    .panel{border:1px solid var(--line);background:var(--panel);border-radius:6px;overflow:hidden}
        \\    .section{margin-bottom:16px}
        \\    .section-head{display:flex;align-items:center;justify-content:space-between;gap:12px;margin-bottom:8px}
        \\    .controls{display:flex;gap:8px;align-items:center;flex-wrap:wrap}
        \\    input[type=search]{height:30px;border:1px solid var(--line);border-radius:4px;padding:0 9px;background:#fff;color:var(--text);min-width:220px}
        \\    button{height:30px;border:1px solid var(--line);background:#fff;color:var(--text);border-radius:4px;padding:0 10px;cursor:pointer}
        \\    button.active{border-color:var(--select);box-shadow:inset 0 0 0 1px var(--select)}
        \\    code{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;font-size:12px}
        \\    .status{font-weight:700}.pass{color:var(--pass)}.fail,.crash,.timeout{color:var(--fail)}.skip{color:var(--skip)}
        \\    .muted{color:var(--muted)}.small{font-size:12px}
        \\    .empty{padding:12px;color:var(--muted)}
        \\    .grid{display:grid;grid-template-columns:320px minmax(0,1fr);gap:12px;align-items:start}
        \\    .list{max-height:520px;overflow:auto;border:1px solid var(--line);background:#fff;border-radius:6px}
        \\    .row{display:grid;grid-template-columns:minmax(0,1fr)70px 80px;gap:8px;align-items:center;padding:7px 9px;border-top:1px solid var(--line-soft);cursor:pointer}
        \\    .row:first-child{border-top:0}.row:hover,.row.selected{background:#f2f5f9}.row .name{overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
        \\    .timeline{border:1px solid var(--line);background:#fff;border-radius:6px;overflow:hidden}
        \\    .axis{height:26px;position:relative;border-bottom:1px solid var(--line-soft);background:#fafbfc}
        \\    .tick{position:absolute;top:0;height:100%;border-left:1px solid var(--line-soft);font-size:11px;color:var(--muted);padding-left:4px;white-space:nowrap}
        \\    .chart-row{display:grid;grid-template-columns:260px minmax(0,1fr);gap:10px;min-height:34px;border-top:1px solid var(--line-soft);padding:6px 8px;align-items:center}
        \\    .chart-row:first-child{border-top:0}.label{overflow:hidden;text-overflow:ellipsis;white-space:nowrap}.label-meta{font-size:11px;color:var(--muted)}
        \\    .track{height:24px;position:relative;background:var(--track);border-radius:3px;overflow:hidden}
        \\    .lane-track{height:28px;position:relative;background:var(--track);border-radius:3px;overflow:hidden}
        \\    .bar{position:absolute;top:4px;height:16px;min-width:2px;border-radius:3px;background:var(--bar);cursor:pointer}
        \\    .lane-track .bar{top:5px;height:18px}.bar.pass{background:var(--pass)}.bar.fail,.bar.crash,.bar.timeout{background:var(--fail)}.bar.skip{background:var(--skip)}.bar.selected{outline:2px solid var(--select);outline-offset:1px}
        \\    .detail{padding:12px}.kv{display:grid;grid-template-columns:110px minmax(0,1fr);gap:6px 10px}.kv div{overflow-wrap:anywhere}
        \\    .failure-list{display:grid;gap:8px;max-height:440px;overflow:auto}.failure{border:1px solid var(--line);border-left:4px solid var(--fail);background:#fff;border-radius:4px;padding:9px;cursor:pointer}.failure:hover{background:#f7f8fa}
        \\    .event-data{margin-top:10px}.event-data pre{white-space:pre-wrap;overflow:auto;max-height:260px;margin:6px 0 0;padding:8px;background:#111827;color:#f8fafc;border-radius:4px;font-size:12px}
        \\    .split{display:grid;grid-template-columns:minmax(0,1fr)320px;gap:12px}
        \\    @media(max-width:1100px){main{grid-template-columns:1fr}.grid,.split{grid-template-columns:1fr}.chart-row{grid-template-columns:1fr}.label-meta{display:inline;margin-left:6px}}
        \\  </style>
        \\</head>
        \\<body>
        \\  <header>
        \\    <h1>MiniCI</h1>
        \\    <div id="summary" class="summary"><div class="metric"><b>Loading</b><span>Report</span></div></div>
        \\  </header>
        \\  <main>
        \\    <div>
        \\      <section class="section">
        \\        <div class="section-head"><h2>Run Timeline</h2><div class="controls"><input id="search" type="search" placeholder="Filter jobs and tests"><button id="failOnly">Failures</button></div></div>
        \\        <div id="runTimeline" class="timeline"><div class="empty">Loading run timeline...</div></div>
        \\      </section>
        \\      <section class="section grid">
        \\        <div><h2>Jobs</h2><div id="jobList" class="list"><div class="empty">Loading jobs...</div></div></div>
        \\        <div><h2 id="jobTitle">Job</h2><div id="jobDetail" class="panel"><div class="empty">Select a job.</div></div></div>
        \\      </section>
        \\      <section class="section split">
        \\        <div><h2 id="caseTitle">Case Detail</h2><div id="caseDetail" class="panel"><div class="empty">Select a harness case.</div></div></div>
        \\        <div><h2>Slowest Cases</h2><div id="caseList" class="list"><div class="empty">Select a harness job.</div></div></div>
        \\      </section>
        \\    </div>
        \\    <aside>
        \\      <section class="section"><h2>Failures</h2><div id="failures" class="failure-list"><div class="panel empty">Loading failures...</div></div></section>
        \\      <section class="section"><h2>Selection</h2><div id="selection" class="panel detail">Loading selection...</div></section>
        \\    </aside>
        \\  </main>
        \\  <script>
        \\  const REPORT =
    );
    var report_json = std.ArrayList(u8).empty;
    defer report_json.deinit(allocator);
    try appendReportJsonObject(&report_json, allocator, run_started_unix_ms, cache_reuse_enforced, build_result, results);
    try appendScriptJsonBytes(&out, allocator, report_json.items);
    try out.appendSlice(allocator,
        \\;
        \\  const STATS =
    );
    try appendStatsJsonObject(&out, allocator, io, results);
    try out.appendSlice(allocator,
        \\;
        \\  const state = { selectedJob: null, selectedCase: null, query: "", failOnly: false };
        \\  const statusClass = value => value === "passed" ? "pass" : value === "skipped" ? "skip" : String(value || "");
        \\  const isFailure = status => { const s = statusClass(status); return s !== "pass" && s !== "skip"; };
        \\  const esc = value => String(value ?? "").replace(/[&<>"']/g, ch => ({ "&":"&amp;", "<":"&lt;", ">":"&gt;", "\"":"&quot;", "'":"&#39;" }[ch]));
        \\  const jobName = job => job.name || (job.command && job.command.length > 2 ? job.command[2] : "unknown");
        \\  const commandText = command => (command || []).map(part => /\s/.test(part) ? JSON.stringify(part) : part).join(" ");
        \\  function formatNs(ns) {
        \\    if (!Number.isFinite(ns)) return "";
        \\    if (ns >= 1e9) return `${(ns / 1e9).toFixed(1)}s`;
        \\    if (ns >= 1e6) return `${(ns / 1e6).toFixed(1)}ms`;
        \\    if (ns >= 1e3) return `${(ns / 1e3).toFixed(1)}us`;
        \\    return `${Math.round(ns)}ns`;
        \\  }
        \\  function normJob(name, job) {
        \\    const start = Number(job.start_ns ?? 0);
        \\    const duration = Number(job.duration_ns ?? 0);
        \\    const end = Number(job.end_ns ?? (start + duration));
        \\    return { ...job, name, start_ns: start, end_ns: end, duration_ns: duration || Math.max(0, end - start) };
        \\  }
        \\  const jobs = [normJob("build-ci", REPORT.build_ci), ...REPORT.jobs.map(job => normJob(jobName(job), job))];
        \\  const jobsByName = new Map(jobs.map(job => [job.name, job]));
        \\  const maxRunEnd = Math.max(1, ...jobs.map(job => job.end_ns || job.duration_ns || 0));
        \\  function statsFor(job) { return STATS[job.name] && Array.isArray(STATS[job.name].events) ? STATS[job.name] : null; }
        \\  function childrenByParent(stats) {
        \\    const map = new Map();
        \\    for (const event of stats?.events || []) {
        \\      const key = event.parent_id || "";
        \\      if (!map.has(key)) map.set(key, []);
        \\      map.get(key).push(event);
        \\    }
        \\    for (const list of map.values()) list.sort((a,b) => (a.start_ns || 0) - (b.start_ns || 0));
        \\    return map;
        \\  }
        \\  function rootCases(stats) { return (stats?.events || []).filter(event => event.parent_id == null); }
        \\  function sortedCases(stats) {
        \\    return rootCases(stats).sort((a,b) => (isFailure(a.status) ? 0 : 1) - (isFailure(b.status) ? 0 : 1) || (b.duration_ns || 0) - (a.duration_ns || 0));
        \\  }
        \\  function scaleStyle(start, end, max) {
        \\    const left = Math.max(0, (Number(start || 0) / max) * 100);
        \\    const width = Math.max(0.25, ((Number(end || 0) - Number(start || 0)) / max) * 100);
        \\    return `left:${left.toFixed(3)}%;width:${width.toFixed(3)}%`;
        \\  }
        \\  function axis(max) {
        \\    const ticks = [];
        \\    for (let i = 0; i <= 4; i++) ticks.push(`<div class="tick" style="left:${i * 25}%">${formatNs(max * i / 4)}</div>`);
        \\    return `<div class="axis">${ticks.join("")}</div>`;
        \\  }
        \\  function rowMatches(text, status) {
        \\    const q = state.query.trim().toLowerCase();
        \\    if (state.failOnly && !isFailure(status)) return false;
        \\    return q === "" || String(text || "").toLowerCase().includes(q);
        \\  }
        \\  function renderSummary() {
        \\    const counts = { pass:0, fail:0, crash:0, timeout:0, skip:0 };
        \\    for (const job of jobs) counts[statusClass(job.status)] = (counts[statusClass(job.status)] || 0) + 1;
        \\    const failed = (counts.fail || 0) + (counts.crash || 0) + (counts.timeout || 0);
        \\    const started = REPORT.run_started_unix_ms ? new Date(REPORT.run_started_unix_ms).toLocaleString() : "";
        \\    document.getElementById("summary").innerHTML = [
        \\      ["Jobs", jobs.length], ["Passed", counts.pass || 0], ["Failed", failed], ["Wall", formatNs(maxRunEnd)], ["Started", started]
        \\    ].filter(item => item[1] !== "").map(([label,value]) => `<div class="metric"><b>${esc(value)}</b><span>${esc(label)}</span></div>`).join("");
        \\  }
        \\  function renderRunTimeline() {
        \\    const rows = jobs.map(job => `<div class="chart-row"><div><div class="label"><code>${esc(job.name)}</code></div><div class="label-meta"><span class="${statusClass(job.status)}">${esc(job.status)}</span> ${formatNs(job.duration_ns)}</div></div><div class="track"><div class="bar ${statusClass(job.status)} ${state.selectedJob === job.name ? "selected" : ""}" data-job="${esc(job.name)}" title="${esc(job.name)} ${formatNs(job.duration_ns)}" style="${scaleStyle(job.start_ns, job.end_ns, maxRunEnd)}"></div></div></div>`).join("");
        \\    document.getElementById("runTimeline").innerHTML = axis(maxRunEnd) + rows;
        \\  }
        \\  function renderJobList() {
        \\    const visible = jobs.filter(job => rowMatches(job.name, job.status)).sort((a,b) => (isFailure(a.status) ? 0 : 1) - (isFailure(b.status) ? 0 : 1) || (b.duration_ns || 0) - (a.duration_ns || 0));
        \\    document.getElementById("jobList").innerHTML = visible.length ? visible.map(job => `<div class="row ${state.selectedJob === job.name ? "selected" : ""}" data-job="${esc(job.name)}"><div class="name"><code>${esc(job.name)}</code></div><span class="status ${statusClass(job.status)}">${esc(job.status)}</span><span>${formatNs(job.duration_ns)}</span></div>`).join("") : `<div class="empty">No jobs match.</div>`;
        \\  }
        \\  function renderJobDetail() {
        \\    const job = jobsByName.get(state.selectedJob) || jobs[0];
        \\    state.selectedJob = job.name;
        \\    document.getElementById("jobTitle").textContent = job.name;
        \\    const stats = statsFor(job);
        \\    document.getElementById("selection").innerHTML = renderJobMeta(job);
        \\    if (!stats) {
        \\      document.getElementById("jobDetail").innerHTML = `<div class="detail">${renderJobMeta(job)}</div>`;
        \\      renderCaseList(null);
        \\      renderCaseDetail(null, null);
        \\      return;
        \\    }
        \\    const cases = rootCases(stats);
        \\    const byLane = new Map();
        \\    for (const c of cases) {
        \\      const lane = c.worker_index ?? 0;
        \\      if (!byLane.has(lane)) byLane.set(lane, []);
        \\      byLane.get(lane).push(c);
        \\    }
        \\    const maxEnd = Math.max(1, ...cases.map(c => c.end_ns || c.duration_ns || 0));
        \\    const lanes = [...byLane.entries()].sort((a,b) => a[0] - b[0]).map(([lane, list]) => {
        \\      const bars = list.map(c => `<div class="bar ${statusClass(c.status)} ${state.selectedCase === c.id ? "selected" : ""}" data-case="${esc(c.id)}" title="${esc(c.name)} ${formatNs(c.duration_ns)}" style="${scaleStyle(c.start_ns || 0, c.end_ns || c.duration_ns || 0, maxEnd)}"></div>`).join("");
        \\      return `<div class="chart-row"><div><div class="label">worker ${esc(lane)}</div><div class="label-meta">${list.length} cases</div></div><div class="lane-track">${bars}</div></div>`;
        \\    }).join("");
        \\    const summary = stats.summary || {};
        \\    document.getElementById("jobDetail").innerHTML = `<div class="detail small muted">Runner <b>${esc(stats.runner || job.name)}</b>: ${esc(summary.passed || 0)} passed, ${esc(summary.failed || 0)} failed, ${esc(summary.crashed || 0)} crashed, ${esc(summary.timed_out || 0)} timed out, ${esc(summary.skipped || 0)} skipped</div><div class="timeline">${axis(maxEnd)}${lanes || `<div class="empty">No case events.</div>`}</div>`;
        \\    if (!state.selectedCase || !cases.some(c => c.id === state.selectedCase)) state.selectedCase = sortedCases(stats)[0]?.id || null;
        \\    renderCaseList(stats);
        \\    renderCaseDetail(stats, state.selectedCase);
        \\  }
        \\  function renderJobMeta(job) {
        \\    return `<div class="kv"><div>Status</div><div class="status ${statusClass(job.status)}">${esc(job.status)}</div><div>Duration</div><div>${formatNs(job.duration_ns)}</div><div>Command</div><div><code>${esc(commandText(job.command))}</code></div><div>Log</div><div><code>${esc(job.log_path || "")}</code></div>${job.stats_path ? `<div>Stats</div><div><code>${esc(job.stats_path)}</code></div>` : ""}</div>`;
        \\  }
        \\  function renderCaseList(stats) {
        \\    if (!stats) { document.getElementById("caseList").innerHTML = `<div class="empty">No harness cases.</div>`; return; }
        \\    const cases = sortedCases(stats).filter(c => rowMatches(c.name, c.status)).slice(0, 300);
        \\    document.getElementById("caseList").innerHTML = cases.length ? cases.map(c => `<div class="row ${state.selectedCase === c.id ? "selected" : ""}" data-case="${esc(c.id)}"><div class="name">${esc(c.name)}</div><span class="status ${statusClass(c.status)}">${esc(c.status)}</span><span>${formatNs(c.duration_ns)}</span></div>`).join("") : `<div class="empty">No cases match.</div>`;
        \\  }
        \\  function renderCaseDetail(stats, caseId) {
        \\    const root = document.getElementById("caseDetail");
        \\    if (!stats || !caseId) { document.getElementById("caseTitle").textContent = "Case Detail"; root.innerHTML = `<div class="empty">Select a harness case.</div>`; return; }
        \\    const byParent = childrenByParent(stats);
        \\    const event = (stats.events || []).find(e => e.id === caseId);
        \\    if (!event) { root.innerHTML = `<div class="empty">Selected case is missing.</div>`; return; }
        \\    document.getElementById("caseTitle").textContent = event.name;
        \\    const children = byParent.get(event.id) || [];
        \\    const spans = children.length ? children : [event];
        \\    const maxEnd = Math.max(1, ...spans.map(s => s.end_ns || s.duration_ns || 0));
        \\    const rows = spans.map(s => `<div class="chart-row"><div><div class="label">${esc(s.kind)} ${esc(s.name)}</div><div class="label-meta"><span class="${statusClass(s.status)}">${esc(s.status)}</span> ${formatNs(s.duration_ns)}</div></div><div class="track"><div class="bar ${statusClass(s.status)}" title="${esc(s.name)} ${formatNs(s.duration_ns)}" style="${scaleStyle(s.start_ns || 0, s.end_ns || s.duration_ns || 0, maxEnd)}"></div></div></div>`).join("");
        \\    root.innerHTML = `<div class="detail">${renderCaseMeta(event)}${renderData(event.data)}</div><div class="timeline">${axis(maxEnd)}${rows}</div>`;
        \\  }
        \\  function renderCaseMeta(event) {
        \\    const job = jobsByName.get(state.selectedJob);
        \\    const repro = job ? `zig build ${job.name} -- --test-filter ${JSON.stringify(event.name)}` : "";
        \\    return `<div class="kv"><div>Status</div><div class="status ${statusClass(event.status)}">${esc(event.status)}</div><div>Duration</div><div>${formatNs(event.duration_ns)}</div><div>Worker</div><div>${esc(event.worker_index ?? "")}</div><div>Rerun</div><div><code>${esc(repro)}</code></div></div>`;
        \\  }
        \\  function renderData(data) {
        \\    if (!data || Object.keys(data).length === 0) return "";
        \\    return `<div class="event-data">${Object.entries(data).slice(0,8).map(([key,value]) => `<b>${esc(key)}</b><pre>${esc(String(value).slice(0, 4000))}</pre>`).join("")}</div>`;
        \\  }
        \\  function collectFailures() {
        \\    const failures = [];
        \\    for (const job of jobs) {
        \\      if (job.name !== "build-ci" && isFailure(job.status)) failures.push({ job, event: null });
        \\      const stats = statsFor(job);
        \\      for (const event of rootCases(stats)) if (isFailure(event.status)) failures.push({ job, event });
        \\    }
        \\    return failures;
        \\  }
        \\  function renderFailures() {
        \\    const failures = collectFailures();
        \\    document.getElementById("failures").innerHTML = failures.length ? failures.map((item, i) => {
        \\      const title = item.event ? item.event.name : item.job.name;
        \\      const status = item.event ? item.event.status : item.job.status;
        \\      return `<article class="failure" data-failure="${i}"><div><b>${esc(item.job.name)}</b> <span class="${statusClass(status)}">${esc(status)}</span></div><div>${esc(title)}</div><div class="small muted"><code>${esc(item.event ? `zig build ${item.job.name} -- --test-filter ${JSON.stringify(item.event.name)}` : `zig build ${item.job.name}`)}</code></div></article>`;
        \\    }).join("") : `<div class="panel empty">No failing jobs or harness cases.</div>`;
        \\  }
        \\  function selectJob(name) { state.selectedJob = name; state.selectedCase = null; renderAll(); }
        \\  function selectCase(id) { state.selectedCase = id; renderAll(); }
        \\  function wireEvents() {
        \\    document.querySelectorAll("[data-job]").forEach(el => el.onclick = () => selectJob(el.getAttribute("data-job")));
        \\    document.querySelectorAll("[data-case]").forEach(el => el.onclick = () => selectCase(el.getAttribute("data-case")));
        \\    const failures = collectFailures();
        \\    document.querySelectorAll("[data-failure]").forEach(el => el.onclick = () => {
        \\      const item = failures[Number(el.getAttribute("data-failure"))];
        \\      if (!item) return;
        \\      state.selectedJob = item.job.name;
        \\      state.selectedCase = item.event ? item.event.id : null;
        \\      renderAll();
        \\    });
        \\  }
        \\  function chooseInitialSelection() {
        \\    const failure = collectFailures()[0];
        \\    state.selectedJob = failure ? failure.job.name : jobs[0].name;
        \\    state.selectedCase = failure && failure.event ? failure.event.id : null;
        \\  }
        \\  function renderAll() {
        \\    renderSummary();
        \\    renderRunTimeline();
        \\    renderJobList();
        \\    renderJobDetail();
        \\    renderFailures();
        \\    wireEvents();
        \\  }
        \\  function showRenderError(error) {
        \\    const message = error && error.stack ? error.stack : String(error);
        \\    const html = `<div class="detail"><b>Report render failed</b><div class="event-data"><pre>${esc(message)}</pre></div></div>`;
        \\    document.getElementById("selection").innerHTML = html;
        \\    document.getElementById("runTimeline").innerHTML = html;
        \\  }
        \\  function boot() {
        \\    try {
        \\      document.getElementById("search").addEventListener("input", event => { state.query = event.target.value; renderAll(); });
        \\      document.getElementById("failOnly").addEventListener("click", event => { state.failOnly = !state.failOnly; event.target.classList.toggle("active", state.failOnly); renderAll(); });
        \\      chooseInitialSelection();
        \\      renderAll();
        \\    } catch (error) {
        \\      showRenderError(error);
        \\    }
        \\  }
        \\  setTimeout(boot, 0);
        \\  </script>
        \\</body>
        \\</html>
        \\
    );
    try writeFile(io, out_dir ++ "/index.html", out.items);
}

/// Runs build-ci followed by each named MiniCI run job and writes reports.
/// CPUs this process may run on, honoring any inherited affinity (e.g. an outer
/// `taskset`). Null if it cannot be determined.
fn onlineCpuCount() ?usize {
    const set = std.posix.sched_getaffinity(0) catch return null;
    return std.posix.CPU_COUNT(set);
}

/// Parses Linux's estimate of RAM that can be allocated without swapping.
fn parseAvailableRamBytes(meminfo: []const u8) ?u64 {
    var lines = std.mem.splitScalar(u8, meminfo, '\n');
    while (lines.next()) |line| {
        const prefix = "MemAvailable:";
        if (!std.mem.startsWith(u8, line, prefix)) continue;
        var fields = std.mem.tokenizeAny(u8, line[prefix.len..], " \t");
        const kib = std.fmt.parseInt(u64, fields.next() orelse return null, 10) catch return null;
        const unit = fields.next() orelse return null;
        if (!std.mem.eql(u8, unit, "kB")) return null;
        return std.math.mul(u64, kib, 1024) catch null;
    }
    return null;
}

/// Linux's current estimate of RAM available without swapping.
fn availableRamBytes() ?u64 {
    if (builtin.os.tag != .linux) return null;
    const linux = std.os.linux;
    const open_rc = linux.openat(linux.AT.FDCWD, "/proc/meminfo", .{}, 0);
    if (linux.errno(open_rc) != .SUCCESS) return null;
    const fd: i32 = @intCast(open_rc);
    defer _ = linux.close(fd);
    var meminfo: [64 * 1024]u8 = undefined;
    var len: usize = 0;
    while (len < meminfo.len) {
        const read_rc = linux.read(fd, meminfo[len..].ptr, meminfo.len - len);
        if (linux.errno(read_rc) != .SUCCESS) return null;
        if (read_rc == 0) break;
        len += read_rc;
    }
    return parseAvailableRamBytes(meminfo[0..len]);
}

/// How many concurrent heavy Roc/LLVM compilations fit in currently available
/// RAM. A large compiler root has been observed at 6.8 GiB RSS, so reserve OS
/// headroom and budget 6.5 GiB per slot. Never returns fewer than 1.
fn cpuBudgetForAvailableRam(available_ram_bytes: u64, online_cpus: usize) usize {
    const available_mib = available_ram_bytes / (1024 * 1024);
    const reserve_mib: u64 = 2048; // ~2 GiB for the OS/desktop + MiniCI itself
    const per_compile_mib: u64 = 6656; // 6.5 GiB per concurrent compiler root
    if (available_mib <= reserve_mib + per_compile_mib) return 1;
    const fits: u64 = (available_mib - reserve_mib) / per_compile_mib;
    return @intCast(@min(@max(fits, 1), @as(u64, online_cpus)));
}

/// Parses `MINICI_MAX_CPUS`. Null when unset/empty/invalid or `<1` (auto). An
/// explicit value applies on any host, overriding automatic RAM budgeting.
fn envCpuOverride(env: *const std.process.Environ.Map) ?usize {
    const raw = env.get(cpu_limit_env) orelse return null;
    const trimmed = std.mem.trim(u8, raw, " \t");
    if (trimmed.len == 0) return null;
    const n = std.fmt.parseInt(usize, trimmed, 10) catch |err| {
        std.debug.print("invalid {s}='{s}': {s}; auto-detecting instead\n", .{ cpu_limit_env, raw, @errorName(err) });
        return null;
    };
    return if (n >= 1) n else null;
}

/// Selects the `zig build build-ci` job count whose concurrent multi-GiB
/// compiler roots fit in currently available RAM. Later MiniCI phases retain
/// all CPUs because their already-built test runners do not create this build
/// graph. `MINICI_MAX_CPUS=N` overrides automatic budgeting.
fn memoryAwareBuildJobs(_: std.Io, _: std.mem.Allocator, env: *const std.process.Environ.Map) ?usize {
    if (builtin.os.tag != .linux) return null;

    const online = onlineCpuCount() orelse return null;
    if (online <= 1) return null;

    const override = envCpuOverride(env);
    const available_ram = availableRamBytes();
    const budget = if (override) |n|
        @min(n, online)
    else blk: {
        break :blk cpuBudgetForAvailableRam(available_ram orelse return null, online);
    };
    if (budget >= online) return null;

    if (override != null) {
        std.debug.print(
            "Limiting build-ci to {d} parallel jobs across {d} CPUs ({s}); run phases retain all CPUs.\n",
            .{ budget, online, cpu_limit_env },
        );
    } else {
        const available_mib = (available_ram orelse 0) / (1024 * 1024);
        std.debug.print(
            "{d} MiB available RAM: limiting build-ci to {d} parallel jobs across {d} CPUs to avoid OOM; run phases retain all CPUs (set {s}=N to override).\n",
            .{ available_mib, budget, online, cpu_limit_env },
        );
    }
    return budget;
}

/// Parallel job limit for run phases that may still compile. Run phases
/// normally execute prebuilt runners, but some (for example the test-wiring
/// check) build a debug compiler of their own. On Windows, running those
/// builds at full width has exhausted the hosted runner's memory
/// (`std::bad_alloc` inside zig.exe), so cap them there. `MINICI_MAX_CPUS=N`
/// overrides the cap.
fn runPhaseJobs(env: *const std.process.Environ.Map) ?usize {
    if (envCpuOverride(env)) |n| return if (builtin.os.tag == .windows) n else null;
    return if (builtin.os.tag == .windows) 2 else null;
}

const workflow_shard_key = "minici_shard:";

/// Returns one message per problem with the `minici_shard:` keys in `text`: a
/// name that is not in `shards`, a shard named more than once, or a shard not
/// named at all. An empty result means the workflow runs every shard exactly
/// once, which together with the "MiniCI shards cover" tests means CI runs
/// every MiniCI job on every host it belongs on.
fn workflowShardProblems(allocator: std.mem.Allocator, text: []const u8) ![]const []const u8 {
    var problems = std.ArrayList([]const u8).empty;
    errdefer problems.deinit(allocator);
    var counts = @as([shards.len]usize, @splat(0));

    var lines = std.mem.splitScalar(u8, text, '\n');
    while (lines.next()) |raw_line| {
        var line = std.mem.trim(u8, raw_line, " \t\r");
        if (std.mem.startsWith(u8, line, "- ")) line = std.mem.trimStart(u8, line[2..], " ");
        if (!std.mem.startsWith(u8, line, workflow_shard_key)) continue;
        var value = line[workflow_shard_key.len..];
        if (std.mem.findScalar(u8, value, '#')) |comment_start| value = value[0..comment_start];
        value = std.mem.trim(u8, value, " \t\"'");

        const index = for (shards, 0..) |shard, i| {
            if (std.mem.eql(u8, shard.name, value)) break i;
        } else {
            try problems.append(allocator, try std.fmt.allocPrint(allocator, "unknown MiniCI shard `{s}`", .{value}));
            continue;
        };
        counts[index] += 1;
    }

    for (shards, counts) |shard, count| {
        if (count == 1) continue;
        try problems.append(allocator, try std.fmt.allocPrint(
            allocator,
            "MiniCI shard `{s}` is named {d} times (expected exactly once)",
            .{ shard.name, count },
        ));
    }
    return problems.toOwnedSlice(allocator);
}

fn verifyWorkflow(allocator: std.mem.Allocator, io: std.Io, path: []const u8) !void {
    const text = std.Io.Dir.cwd().readFileAlloc(io, path, allocator, .limited(4 * 1024 * 1024)) catch |err| {
        std.debug.print("MiniCI: cannot read workflow `{s}`: {s}\n", .{ path, @errorName(err) });
        std.process.exit(1);
    };
    const problems = try workflowShardProblems(allocator, text);
    if (problems.len == 0) {
        std.debug.print("MiniCI: `{s}` runs each of the {d} MiniCI shards exactly once\n", .{ path, shards.len });
        return;
    }
    for (problems) |problem| std.debug.print("MiniCI workflow error in `{s}`: {s}\n", .{ path, problem });
    std.process.exit(1);
}

/// Entry point: build the CI artifacts, then run each selected `run-*` job in order,
/// streaming heartbeats and a machine-readable report. Limits only build graph
/// parallelism on memory-constrained hosts (see `memoryAwareBuildJobs`). A job
/// that compiles what `build-ci` should have built fails the run with
/// `cache_reuse_exit_code` once every job has run (see `cacheReuseFailure`).
pub fn main(init: std.process.Init) !void {
    requireSha256Hardware();
    const io = init.io;
    var gpa_impl = std.heap.DebugAllocator(.{ .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }){};
    defer _ = build_options.debugGpaOk(gpa_impl.deinit());
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const allocator = arena_impl.allocator();

    const raw_args = try init.minimal.args.toSlice(allocator);
    const args: []const []const u8 = @ptrCast(raw_args);
    const parsed_args = parseMiniArgs(allocator, args) catch |err| switch (err) {
        error.OutOfMemory => return err,
        error.InvalidMiniCiArgument => std.process.exit(2),
    };
    if (parsed_args.verify_workflow) |workflow_path| {
        try verifyWorkflow(allocator, io, workflow_path);
        return;
    }
    const lane: ?Lane = if (parsed_args.shard) |shard| shard.lane else null;
    const cache_reuse_enforced = buildCiPrecedesPhases(parsed_args);
    const selected_jobs = resolveSelection(parsed_args.selection) catch |err| {
        printSelectionError(parsed_args.selection, err);
        printSelectionUsage();
        std.process.exit(2);
    };
    const zig_exe = parsed_args.zig_exe;
    const build_args = parsed_args.build_args;
    const heartbeat_interval_ms = heartbeatIntervalMs(init.environ_map);

    std.Io.Dir.cwd().deleteTree(io, out_dir) catch {};
    try std.Io.Dir.cwd().createDirPath(io, raw_dir);
    try std.Io.Dir.cwd().createDirPath(io, logs_dir);

    std.debug.print("=== MINICI ORCHESTRATOR ===\n", .{});
    const build_jobs = memoryAwareBuildJobs(io, allocator, init.environ_map);
    const run_started_ns = nowNs(io);
    const run_started_unix_ms = unixMs(io);
    const total_phases = jobs.len + 1;
    if (parsed_args.shard) |shard| {
        std.debug.print("MiniCI shard: `{s}` ({s} lane)\n", .{ shard.name, @tagName(shard.lane) });
    }
    if (selected_jobs.first != 0 or selected_jobs.last != jobs.len - 1) {
        std.debug.print("MiniCI selection: `{s}` through `{s}`\n", .{
            jobs[selected_jobs.first].name,
            jobs[selected_jobs.last].name,
        });
    }

    const build_argv = try buildCommand(allocator, zig_exe, build_args, "build-ci", build_jobs, null, &.{});
    const build_log = logs_dir ++ "/build-ci.txt";
    const build_progress = Progress{ .current = 1, .total = total_phases };
    printBuildStart(build_progress);
    var build_result = if (parsed_args.skip_build)
        try skipCommand(io, build_argv, build_log, "skipped by --minici-skip-build\n", run_started_ns)
    else if (lane != null and !lane.?.needsBuildCi())
        try skipCommand(io, build_argv, build_log, "skipped: the source lane runs only source checks, which build their own tools\n", run_started_ns)
    else
        try runCommand(allocator, io, build_argv, build_log, heartbeat_interval_ms, run_started_ns, build_progress);
    // `build-ci` is where compile steps belong, so it may run any of them; its
    // summary still has to be readable.
    build_result.cache_reuse_failure = try cacheReuseFailure(allocator, build_result, .unrestricted);
    if (build_result.heartbeat_printed) printBuildStart(build_progress);
    std.debug.print("{s} in {d:.3}s\n", .{ buildStatusText(build_result), seconds(build_result.duration_ns) });
    try printCompileWorkReport(allocator, build_result, .counts, &.{});

    var results = std.ArrayList(CommandResult).empty;
    defer results.deinit(allocator);

    if (!isSuccessful(build_result)) {
        printFailureLog(allocator, io, build_result);
        printRerunHint(build_result);
        try writeReportJson(allocator, io, run_started_unix_ms, cache_reuse_enforced, build_result, results.items);
        try writeHtml(allocator, io, run_started_unix_ms, cache_reuse_enforced, build_result, results.items);
        try printSummary(allocator, total_phases, cache_reuse_enforced, build_result, results.items, durationSince(io, run_started_ns));
        std.process.exit(1);
    }
    if (isPass(build_result)) try restampBuildCache(allocator, io, zig_exe);

    for (jobs, 0..) |job, job_index| {
        const log_path = try std.fmt.allocPrint(allocator, "{s}/{s}.txt", .{ logs_dir, job.name });
        const stats_path: ?[]const u8 = if (job.kind == .harness)
            try std.fmt.allocPrint(allocator, "{s}/{s}.json", .{ raw_dir, job.name })
        else
            null;
        const job_build_args = try std.mem.concat(allocator, []const u8, &.{ build_args, job.build_args });
        const argv = try buildCommand(allocator, zig_exe, job_build_args, job.name, runPhaseJobs(init.environ_map), stats_path, job.args);
        const progress = Progress{ .current = job_index + 2, .total = total_phases };
        printRunStart(progress, job.name);
        const skip_reason: ?[]const u8 = if (!selected_jobs.includes(job_index))
            "excluded by MiniCI selection\n"
        else if (lane != null and !lane.?.runs(job.placement))
            try std.fmt.allocPrint(allocator, "not run in the {s} lane ({s} job)\n", .{ @tagName(lane.?), @tagName(job.placement) })
        else
            job.skip_reason;
        var result = if (skip_reason) |reason|
            try skipCommand(io, argv, log_path, reason, run_started_ns)
        else
            try runCommand(allocator, io, argv, log_path, heartbeat_interval_ms, run_started_ns, progress);
        result.stats_path = if (skip_reason == null) stats_path else null;
        result.own_configuration = job.build_args.len != 0;
        // A phase that compiles when it should have reused `build-ci` outputs
        // fails the run at the end, with its own exit status; the remaining
        // phases still run, so one run reports every such phase. A job with
        // build options of its own has no `build-ci` outputs to reuse.
        const compile_expectation: CompileExpectation = if (cache_reuse_enforced and !result.own_configuration)
            .{ .only = job.expected_compiles }
        else
            .unrestricted;
        result.cache_reuse_failure = try cacheReuseFailure(allocator, result, compile_expectation);
        try results.append(allocator, result);
        if (result.heartbeat_printed) printRunStart(progress, job.name);
        std.debug.print("{s} in {d:.3}s\n", .{ runStatusText(result), seconds(result.duration_ns) });
        try printCompileWorkReport(allocator, result, if (result.own_configuration) .counts else .steps, job.expected_compiles);

        if (!isSuccessful(result)) {
            printFailureLog(allocator, io, result);
            printRerunHint(result);
        }

        const stops_at_failing_check = if (lane) |l| l.stopsAtFailingCheck() else true;
        if (stops_at_failing_check and isCheckJob(job.name) and !isSuccessful(result)) {
            try writeReportJson(allocator, io, run_started_unix_ms, cache_reuse_enforced, build_result, results.items);
            try writeHtml(allocator, io, run_started_unix_ms, cache_reuse_enforced, build_result, results.items);
            try printSummary(allocator, total_phases, cache_reuse_enforced, build_result, results.items, durationSince(io, run_started_ns));
            std.process.exit(1);
        }
    }

    try writeReportJson(allocator, io, run_started_unix_ms, cache_reuse_enforced, build_result, results.items);
    try writeHtml(allocator, io, run_started_unix_ms, cache_reuse_enforced, build_result, results.items);
    try printSummary(allocator, total_phases, cache_reuse_enforced, build_result, results.items, durationSince(io, run_started_ns));

    for (results.items) |result| {
        if (!isSuccessful(result)) std.process.exit(1);
    }
    if (build_result.cache_reuse_failure != null) std.process.exit(cache_reuse_exit_code);
    for (results.items) |result| {
        if (result.cache_reuse_failure != null) std.process.exit(cache_reuse_exit_code);
    }
}

test "appendProgressPrefix aligns current phase to total width" {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(std.testing.allocator);

    try appendProgressPrefix(&out, std.testing.allocator, .{ .current = 1, .total = 61 });
    try std.testing.expectEqualStrings("MiniCI  1/61: ", out.items);

    out.clearRetainingCapacity();
    try appendProgressPrefix(&out, std.testing.allocator, .{ .current = 20, .total = 61 });
    try std.testing.expectEqualStrings("MiniCI 20/61: ", out.items);

    out.clearRetainingCapacity();
    try appendProgressPrefix(&out, std.testing.allocator, .{ .current = 7, .total = 123 });
    try std.testing.expectEqualStrings("MiniCI   7/123: ", out.items);
}

test "parseAvailableRamBytes reads Linux MemAvailable" {
    const meminfo =
        \\MemTotal:       32768000 kB
        \\MemFree:         102400 kB
        \\MemAvailable:  16384000 kB
        \\Buffers:         204800 kB
    ;
    try std.testing.expectEqual(@as(?u64, 16_384_000 * 1024), parseAvailableRamBytes(meminfo));
    try std.testing.expect(parseAvailableRamBytes("MemTotal: 1024 kB\n") == null);
    try std.testing.expect(parseAvailableRamBytes("MemAvailable: unknown kB\n") == null);
}

test "cpuBudgetForAvailableRam preserves safe parallelism" {
    const mib: u64 = 1024 * 1024;
    try std.testing.expectEqual(@as(usize, 1), cpuBudgetForAvailableRam(8 * 1024 * mib, 16));
    try std.testing.expectEqual(@as(usize, 2), cpuBudgetForAvailableRam(16 * 1024 * mib, 16));
    try std.testing.expectEqual(@as(usize, 4), cpuBudgetForAvailableRam(32 * 1024 * mib, 16));
    try std.testing.expectEqual(@as(usize, 2), cpuBudgetForAvailableRam(64 * 1024 * mib, 2));
}

test "parseMiniArgs keeps MiniCI selection out of forwarded build args" {
    const args = &.{
        "minici",
        "zig",
        "--search-prefix",
        "/opt/zig",
        "--minici-after",
        "run-test-eval",
        "--minici-before=run-test-cli",
        "--minici-skip-build",
        "-Ddebug-gpa-traces",
    };
    const parsed = try parseMiniArgs(std.testing.allocator, args);
    defer std.testing.allocator.free(parsed.build_args);

    try std.testing.expectEqualStrings("zig", parsed.zig_exe);
    try std.testing.expectEqualStrings("run-test-eval", parsed.selection.after orelse return error.MissingAfter);
    try std.testing.expectEqualStrings("run-test-cli", parsed.selection.before orelse return error.MissingBefore);
    try std.testing.expect(parsed.skip_build);
    try std.testing.expectEqual(@as(usize, 3), parsed.build_args.len);
    try std.testing.expectEqualStrings("--search-prefix", parsed.build_args[0]);
    try std.testing.expectEqualStrings("/opt/zig", parsed.build_args[1]);
    try std.testing.expectEqualStrings("-Ddebug-gpa-traces", parsed.build_args[2]);
}

test "parseMiniArgs supports selecting one MiniCI job" {
    const args = &.{ "minici", "zig", "--minici-only", "run-test-zig-minici" };
    const parsed = try parseMiniArgs(std.testing.allocator, args);
    defer std.testing.allocator.free(parsed.build_args);

    try std.testing.expectEqualStrings("run-test-zig-minici", parsed.selection.from orelse return error.MissingFrom);
    try std.testing.expectEqualStrings("run-test-zig-minici", parsed.selection.to orelse return error.MissingTo);
    try std.testing.expect(!parsed.skip_build);
    try std.testing.expectEqual(@as(usize, 0), parsed.build_args.len);
}

test "parseMiniArgs rejects ambiguous MiniCI bounds" {
    try std.testing.expectError(
        error.InvalidMiniCiArgument,
        parseMiniArgs(std.testing.allocator, &.{ "minici", "zig", "--minici-from", "run-check-zig-format", "--minici-after", "run-check-zig-lints" }),
    );
    try std.testing.expectError(
        error.InvalidMiniCiArgument,
        parseMiniArgs(std.testing.allocator, &.{ "minici", "zig", "--minici-to", "run-check-tidy", "--minici-before", "run-check-git-lints" }),
    );
    try std.testing.expectError(
        error.InvalidMiniCiArgument,
        parseMiniArgs(std.testing.allocator, &.{ "minici", "zig", "--minici-only", "run-check-tidy", "--minici-to", "run-check-git-lints" }),
    );
}

test "resolveSelection defaults to every MiniCI run job" {
    const selected = try resolveSelection(.{});
    try std.testing.expect(selected.includes(0));
    try std.testing.expect(selected.includes(jobs.len - 1));
}

test "resolveSelection includes the requested MiniCI range" {
    const selected = try resolveSelection(.{
        .from = "run-test-zig-snapshot-tool",
        .to = "run-test-zig-minici",
    });
    const first = jobIndexByName("run-test-zig-snapshot-tool") orelse return error.MissingFirst;
    const last = jobIndexByName("run-test-zig-minici") orelse return error.MissingLast;

    try std.testing.expect(!selected.includes(first - 1));
    try std.testing.expect(selected.includes(first));
    try std.testing.expect(selected.includes(last));
    try std.testing.expect(!selected.includes(last + 1));
}

test "MiniCI's module test jobs follow the module inventory" {
    // Unit-tested modules run unless the inventory keeps them out of MiniCI.
    try std.testing.expect(jobIndexByName("run-test-zig-module-static_data") != null);
    try std.testing.expect(jobIndexByName("run-test-zig-module-roc_args") != null);
    try std.testing.expect(jobIndexByName("run-test-zig-module-glue") == null);
    // A harness-run module is a harness job; an untested module is no job.
    const harness = jobIndexByName("run-test-zig-module-lsp_integration") orelse return error.MissingHarnessJob;
    try std.testing.expectEqual(JobKind.harness, jobs[harness].kind);
    try std.testing.expect(jobIndexByName("run-test-zig-module-tracy") == null);

    var expected: usize = 0;
    for (std.enums.values(modules.ModuleType)) |module_type| {
        expected += switch (module_type.info().tests) {
            .unit => |unit| @intFromBool(unit.minici),
            .harness => 1,
            .no_tests => 0,
        };
    }
    var listed: usize = 0;
    for (jobs) |job| listed += @intFromBool(std.mem.startsWith(u8, job.name, "run-test-zig-module-"));
    try std.testing.expectEqual(expected, listed);
}

test "resolveSelection supports exclusive MiniCI range boundaries" {
    const selected = try resolveSelection(.{
        .after = last_module_test_job,
        .before = "run-test-eval",
    });
    const after = jobIndexByName(last_module_test_job) orelse return error.MissingAfter;
    const before = jobIndexByName("run-test-eval") orelse return error.MissingBefore;

    try std.testing.expect(!selected.includes(after));
    try std.testing.expect(selected.includes(after + 1));
    try std.testing.expect(selected.includes(before - 1));
    try std.testing.expect(!selected.includes(before));
}

test "resolveSelection supports exhaustive adjacent MiniCI shards" {
    const first = try resolveSelection(.{ .to = last_module_test_job });
    const middle = try resolveSelection(.{
        .after = last_module_test_job,
        .before = "run-test-eval",
    });
    const last = try resolveSelection(.{ .from = "run-test-eval" });

    const core_boundary = jobIndexByName(last_module_test_job) orelse return error.MissingCoreBoundary;
    const harness_boundary = jobIndexByName("run-test-eval") orelse return error.MissingHarnessBoundary;

    for (jobs, 0..) |_, i| {
        const selected_count: usize =
            @intFromBool(first.includes(i)) +
            @intFromBool(middle.includes(i)) +
            @intFromBool(last.includes(i));
        try std.testing.expectEqual(@as(usize, 1), selected_count);
    }

    try std.testing.expect(first.includes(core_boundary));
    try std.testing.expect(!middle.includes(core_boundary));
    try std.testing.expect(!middle.includes(harness_boundary));
    try std.testing.expect(last.includes(harness_boundary));
}

/// How many of `host`'s shards run job `job_index`. The `source` shard runs on
/// Linux, but its jobs are host-independent, so it counts for every host.
fn shardRunsForHost(host: Host, job_index: usize) !usize {
    var count: usize = 0;
    for (shards) |shard| {
        if (shard.host != host and shard.lane != .source) continue;
        const selected = try resolveSelection(shard.selection);
        if (selected.includes(job_index) and shard.lane.runs(jobs[job_index].placement)) count += 1;
    }
    return count;
}

test "MiniCI shards cover every job exactly once on Linux" {
    for (jobs, 0..) |job, i| {
        const count = try shardRunsForHost(.linux, i);
        if (count != 1) std.debug.print("job `{s}` runs {d} times on Linux\n", .{ job.name, count });
        try std.testing.expectEqual(@as(usize, 1), count);
    }
}

test "MiniCI shards cover every host-specific job exactly once on macOS and Windows" {
    for ([_]Host{ .macos, .windows }) |host| {
        for (jobs, 0..) |job, i| {
            const expected: usize = switch (job.placement) {
                // Source jobs run once, in the source lane, for every host.
                .source => 1,
                // Primary-host jobs run only on Linux.
                .primary_host => 0,
                .every_host => 1,
            };
            const count = try shardRunsForHost(host, i);
            if (count != expected) std.debug.print("job `{s}` runs {d} times on {s}\n", .{ job.name, count, @tagName(host) });
            try std.testing.expectEqual(expected, count);
        }
    }
}

test "MiniCI shard names are unique and their selections resolve" {
    for (shards, 0..) |shard, i| {
        _ = try resolveSelection(shard.selection);
        for (shards[i + 1 ..]) |other| {
            try std.testing.expect(!std.mem.eql(u8, shard.name, other.name));
        }
    }
}

test "MiniCI source lane holds only checks and needs no build" {
    try std.testing.expect(!Lane.source.needsBuildCi());
    try std.testing.expect(!Lane.source.stopsAtFailingCheck());
    try std.testing.expect(Lane.primary.stopsAtFailingCheck());
    for (jobs) |job| {
        if (job.placement == .source) try std.testing.expect(isCheckJob(job.name));
    }
}

test "parseMiniArgs selects a CI shard by name" {
    const parsed = try parseMiniArgs(std.testing.allocator, &.{ "minici", "zig", "--minici-skip-build", "--minici-shard", "macos-harness" });
    defer std.testing.allocator.free(parsed.build_args);

    const shard = parsed.shard orelse return error.MissingShard;
    try std.testing.expectEqualStrings("macos-harness", shard.name);
    try std.testing.expectEqual(Lane.secondary, shard.lane);
    try std.testing.expectEqualStrings("run-test-eval", parsed.selection.after orelse return error.MissingAfter);
    try std.testing.expect(parsed.skip_build);
}

test "parseMiniArgs rejects unknown shards and shards combined with ranges" {
    try std.testing.expectError(
        error.InvalidMiniCiArgument,
        parseMiniArgs(std.testing.allocator, &.{ "minici", "zig", "--minici-shard", "missing-shard" }),
    );
    try std.testing.expectError(
        error.InvalidMiniCiArgument,
        parseMiniArgs(std.testing.allocator, &.{ "minici", "zig", "--minici-shard", "source", "--minici-to", "run-check-tidy" }),
    );
    try std.testing.expectError(
        error.InvalidMiniCiArgument,
        parseMiniArgs(std.testing.allocator, &.{ "minici", "zig", "--minici-from", "run-check-tidy", "--minici-shard=source" }),
    );
    try std.testing.expectError(
        error.InvalidMiniCiArgument,
        parseMiniArgs(std.testing.allocator, &.{ "minici", "zig", "--minici-shard", "source", "--minici-shard", "linux-core" }),
    );
}

fn expectWorkflowProblems(text: []const u8, expected: usize) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const problems = try workflowShardProblems(arena.allocator(), text);
    try std.testing.expectEqual(expected, problems.len);
}

test "workflowShardProblems accepts a workflow naming every shard once" {
    var text = std.ArrayList(u8).empty;
    defer text.deinit(std.testing.allocator);
    for (shards, 0..) |shard, i| {
        // Exercise the list-item, quoted and trailing-comment spellings.
        const line = switch (i % 3) {
            0 => try std.fmt.allocPrint(std.testing.allocator, "          - minici_shard: {s}\n", .{shard.name}),
            1 => try std.fmt.allocPrint(std.testing.allocator, "            minici_shard: '{s}'\n", .{shard.name}),
            else => try std.fmt.allocPrint(std.testing.allocator, "            minici_shard: \"{s}\" # note\n", .{shard.name}),
        };
        defer std.testing.allocator.free(line);
        try text.appendSlice(std.testing.allocator, line);
    }
    try expectWorkflowProblems(text.items, 0);
}

test "workflowShardProblems reports missing, duplicate and unknown shards" {
    // Every shard missing.
    try expectWorkflowProblems("jobs: {}\n", shards.len);

    var text = std.ArrayList(u8).empty;
    defer text.deinit(std.testing.allocator);
    for (shards) |shard| {
        const line = try std.fmt.allocPrint(std.testing.allocator, "minici_shard: {s}\n", .{shard.name});
        defer std.testing.allocator.free(line);
        try text.appendSlice(std.testing.allocator, line);
    }
    try text.appendSlice(std.testing.allocator, "minici_shard: linux-core\nminici_shard: ubuntu-full\n");
    // One duplicate plus one unknown name.
    try expectWorkflowProblems(text.items, 2);
}

test "MiniCI runs the exhaustive SIMD differential after ordinary eval" {
    const eval_index = jobIndexByName("run-test-eval") orelse return error.MissingEval;
    const simd_index = jobIndexByName("run-test-simd-differential") orelse return error.MissingSimdDifferential;

    try std.testing.expectEqual(eval_index + 1, simd_index);
    try std.testing.expectEqual(JobKind.harness, jobs[simd_index].kind);
}

test "resolveSelection rejects unknown and empty MiniCI ranges" {
    try std.testing.expectError(
        error.UnknownMiniCiFromJob,
        resolveSelection(.{ .from = "missing-job" }),
    );
    try std.testing.expectError(
        error.UnknownMiniCiAfterJob,
        resolveSelection(.{ .after = "missing-job" }),
    );
    try std.testing.expectError(
        error.EmptyMiniCiSelection,
        resolveSelection(.{ .from = "run-test-cli", .to = "run-test-eval" }),
    );
    try std.testing.expectError(
        error.EmptyMiniCiSelection,
        resolveSelection(.{ .after = "run-check-zig-format", .before = "run-check-zig-lints" }),
    );
}

fn testResult(status: []const u8, duration_ns: u64) CommandResult {
    return .{
        .status = status,
        .start_ns = 0,
        .end_ns = duration_ns,
        .duration_ns = duration_ns,
        .log_path = "log.txt",
        .command = &.{ "zig", "build", "step" },
        .compile_work = .not_run,
    };
}

test "appendSummaryLine reports all phases passed" {
    const build_result = testResult("pass", 1);
    const results = [_]CommandResult{
        testResult("pass", 2),
        testResult("pass", 3),
    };

    var out = std.ArrayList(u8).empty;
    defer out.deinit(std.testing.allocator);
    try appendSummaryLine(&out, std.testing.allocator, 3, build_result, &results, 1_500_000_000);

    try std.testing.expectEqualStrings(
        "MiniCI summary: 3/3 phases ran; 3 passed, 0 failed, 0 crashed, 0 skipped; wall 1.500s\n",
        out.items,
    );
}

test "appendSummaryLine reports skipped phases" {
    const build_result = testResult("pass", 1);
    const results = [_]CommandResult{
        testResult("skip", 2),
        testResult("pass", 3),
    };

    var out = std.ArrayList(u8).empty;
    defer out.deinit(std.testing.allocator);
    try appendSummaryLine(&out, std.testing.allocator, 3, build_result, &results, 2_000_000_000);

    try std.testing.expectEqualStrings(
        "MiniCI summary: 3/3 phases ran; 2 passed, 0 failed, 0 crashed, 1 skipped; wall 2.000s\n",
        out.items,
    );
}

test "appendSummaryLine reports early failure with not-run phases" {
    const build_result = testResult("pass", 1);
    const results = [_]CommandResult{
        testResult("fail", 2),
    };

    var out = std.ArrayList(u8).empty;
    defer out.deinit(std.testing.allocator);
    try appendSummaryLine(&out, std.testing.allocator, 4, build_result, &results, 3_250_000_000);

    try std.testing.expectEqualStrings(
        "MiniCI summary: 2/4 phases ran; 1 passed, 1 failed, 0 crashed, 0 skipped, 2 not run; wall 3.250s\n",
        out.items,
    );
}

test "findCoreError extracts a Zig compile error region" {
    const log =
        \\0 errors and 0 warnings found in 373ms while successfully building:
        \\
        \\    test/wasm/app.wasm
        \\Build succeeded!
        \\build-ci
        \\+- build-test-zig
        \\   +- compile test check Debug native 1 errors
        \\src/check/checked_artifact.zig:28167:9: error: expected type 'A', found 'B'
        \\        module_name,
        \\        ^~~~~~~~~~~
        \\src/check/canonical_names.zig:28:26: note: enum declared here
        \\error: 1 compilation errors
        \\failed command: /Users/x/zig test -ODebug --dep tracy --dep builtins ...
        \\
        \\Build succeeded!
        \\Build Summary: 321/324 steps succeeded (1 failed)
        \\build-ci transitive failure
        \\+- roc success
    ;
    const region = findCoreError(log) orelse return error.NoMatch;
    // Starts at the failing-step tree above the source error.
    try std.testing.expect(std.mem.startsWith(u8, region, "build-ci\n+- build-test-zig"));
    // Ends at the compiler terminator, dropping the giant `failed command:` line
    // and the trailing summary tree.
    try std.testing.expect(std.mem.endsWith(u8, region, "error: 1 compilation errors"));
    try std.testing.expect(std.mem.find(u8, region, "failed command:") == null);
    try std.testing.expect(std.mem.find(u8, region, "Build Summary:") == null);
    // Leading success spam is not pulled into the context above the error.
    try std.testing.expect(std.mem.find(u8, region, "successfully building") == null);
}

test "findCoreError extracts a Zig unit-test failure region" {
    const log =
        \\run-test-zig-module-collections
        \\+- run test collections 45 pass, 1 fail (46 total)
        \\error: 'mod.test.some assertion' failed:
        \\       expected 1, found 2
        \\       /path/src/collections/mod.zig:143:5: 0x0 in test.some assertion (collections)
        \\           try std.testing.expectEqual(@as(u32, 1), @as(u32, 2));
        \\           ^
        \\failed command: ./.zig-cache/o/abc/collections --cache-dir=./.zig-cache --listen=-
        \\
        \\Build Summary: 1/3 steps succeeded (1 failed); 45/46 tests passed (1 failed)
        \\run-test-zig-module-collections transitive failure
    ;
    const region = findCoreError(log) orelse return error.NoMatch;
    try std.testing.expect(std.mem.startsWith(u8, region, "run-test-zig-module-collections\n+- run test collections"));
    try std.testing.expect(std.mem.find(u8, region, "error: 'mod.test.some assertion' failed:") != null);
    try std.testing.expect(std.mem.endsWith(u8, region, "^"));
    try std.testing.expect(std.mem.find(u8, region, "failed command:") == null);
    try std.testing.expect(std.mem.find(u8, region, "Build Summary:") == null);
}

test "findCoreError extracts a check-tool failure region" {
    const log =
        \\run-check-zig-format
        \\+- zig fmt --check failure
        \\error: /path/src/collections/BADFORMAT.zig: non-conforming formatting
        \\error: process exited with error code 1
        \\failed command: /path/zig fmt --check /path/src /path/build.zig
        \\
        \\Build Summary: 0/2 steps succeeded (1 failed)
        \\run-check-zig-format transitive failure
    ;
    const region = findCoreError(log) orelse return error.NoMatch;
    try std.testing.expect(std.mem.startsWith(u8, region, "run-check-zig-format\n+- zig fmt --check failure"));
    try std.testing.expect(std.mem.endsWith(u8, region, "error: process exited with error code 1"));
    try std.testing.expect(std.mem.find(u8, region, "failed command:") == null);
    try std.testing.expect(std.mem.find(u8, region, "Build Summary:") == null);
}

test "findCoreError extracts a test harness summary region" {
    const log =
        \\Roc cache not found (nothing to clear)
        \\=== CLI Test Runner ===
        \\552 tests, 12 workers, 240s timeout, backends: interpreter, dev
        \\
        \\  run failed   echo platform: hello (interpreter)  (221.5ms, phase=run)
        \\        stdout mismatch: expected 14 bytes, got 16
        \\        stdout: Hellooo, World!
        \\
        \\519 passed, 1 run failed, 32 skipped (552 total) in 353090ms using 12 worker(s)
        \\
        \\=== Suite Summary ===
        \\  echo           20 run,    1 failed,    9 skipped
        \\run-test-cli
        \\+- run exe parallel_cli_runner failure
        \\error: process exited with error code 1
        \\Build Summary: 229/231 steps succeeded (1 failed)
    ;
    const region = findCoreError(log) orelse return error.NoMatch;
    // Starts at the top of the log and includes the failing case detail.
    try std.testing.expect(std.mem.startsWith(u8, region, "Roc cache not found"));
    try std.testing.expect(std.mem.find(u8, region, "run failed   echo platform") != null);
    // Ends at the summary line, dropping the suite/timing tables and build tree.
    try std.testing.expect(std.mem.endsWith(u8, region, "using 12 worker(s)"));
    try std.testing.expect(std.mem.find(u8, region, "=== Suite Summary ===") == null);
    try std.testing.expect(std.mem.find(u8, region, "Build Summary:") == null);
}

test "findCoreError ignores a clean all-passed summary" {
    // A summary with no failures should not match: a job that failed despite an
    // all-passed summary needs the full log, not a misleading green summary.
    const log =
        \\=== CLI Test Runner ===
        \\519 passed, 32 skipped (552 total) in 353090ms using 12 worker(s)
        \\error: process exited with error code 1
    ;
    try std.testing.expect(findCoreError(log) == null);
}

test "findCoreError ignores an incidental passed/failed line without a harness token" {
    // A test's own captured output might print "3 passed, 1 failed" with no
    // harness token; that must not be mistaken for the runner summary.
    const log =
        \\some captured program output
        \\3 passed, 1 failed
        \\more output
    ;
    try std.testing.expect(findCoreError(log) == null);
}

test "findCoreError returns null when no known shape matches" {
    const log =
        \\some lint tool output
        \\a warning here
        \\nothing actionable in a recognizable shape
    ;
    try std.testing.expect(findCoreError(log) == null);
}

/// A whole phase log from a Windows shard on Zig 0.16 whose `.zig-cache` was
/// reused: every compile step is `cached`.
const summary_reused_zig_0_16 =
    \\Build Summary: 7/7 steps succeeded; 158/159 tests passed (1 skipped)
    \\run-test-zig-module-base success
    \\+- run test base 158 pass, 1 skip (159 total) 294ms MaxRSS:32M
    \\   +- compile test base Debug native-native cached 98ms MaxRSS:19M
    \\   |  +- options cached
    \\   |  +- options cached
    \\   +- install stack_overflow_test_helper cached
    \\      +- compile exe stack_overflow_test_helper Debug native-native cached 98ms MaxRSS:19M
    \\         +- options (reused)
    \\
;

/// The same phase, whole and fully reused, from a Linux shard on Zig 0.17,
/// which spells optimize modes in lower case (`debug`, `fast`). This tree also
/// repeats whole subtrees as `(+N more reused dependencies)`.
const summary_reused_zig_0_17 =
    \\Build Summary: 12/12 steps succeeded; 151/151 tests passed
    \\run-test-zig-module-base success
    \\+- run test base 151 pass (151 total) 236ms MaxRSS:53M
    \\   +- compile test base debug native-native-musl cached 6ms MaxRSS:40M
    \\   |  +- options cached
    \\   |  |  +- compile exe stack_overflow_test_helper debug native-native-musl cached 8ms MaxRSS:40M
    \\   |  |     +- options cached
    \\   |  |     +- run exe compiler_identity (compiler_identity.zig) cached
    \\   |  |        +- compile exe compiler_identity fast native cached 8ms MaxRSS:41M
    \\   |  |        +- WriteFile src/backend/dev/CallingConvention.zig cached
    \\   |  |        +- run exe compiler_identity (toolchain_identity.zig) cached
    \\   |  |           +- compile exe compiler_identity fast native (reused)
    \\   |  |           +- WriteFile lib cached
    \\   |  +- options (reused)
    \\   |  +- run exe compiler_identity (compiler_identity.zig) (+3 more reused dependencies)
    \\   +- install stack_overflow_test_helper success
    \\      +- compile exe stack_overflow_test_helper debug native-native-musl (+2 more reused dependencies)
    \\
;

/// The start of `run-check-test-wiring`'s tree from a Linux shard on Zig 0.16
/// that rebuilt 25 compile steps. The step counts in the first line are those
/// of this excerpt.
const summary_rebuilt_zig_0_16 =
    \\Build Summary: 12/12 steps succeeded
    \\run-check-test-wiring success
    \\+- run exe check_test_wiring success 8s
    \\   +- compile exe check_test_wiring Debug native success 927ms MaxRSS:144M
    \\   +- install check_test_wiring success
    \\   |  +- compile exe check_test_wiring Debug native (reused)
    \\   +- compile test machine_code_shim Debug native-native-musl success 4s MaxRSS:376M
    \\   |  +- WriteFile Builtin.bin success
    \\   |  |  +- run exe builtin_compiler (Builtin.bin) success 26s
    \\   |  |  |  +- compile exe builtin_compiler Debug native success 24s MaxRSS:1G
    \\   |  |  |     +- options cached
    \\   |  |  +- run exe builtin_compiler (Builtin.bin) (+1 more reused dependencies)
    \\   |  |  +- run exe builtin_compiler (Builtin.bin) (+1 more reused dependencies)
    \\   |  +- WriteFile cached
    \\   |  +- compile obj machine_code_shim_test_host Debug native-native-musl cached 57ms MaxRSS:40M
    \\   |  |  +- options (reused)
    \\   |  +- compile obj roc_builtins Debug native-native-musl cached 40ms MaxRSS:40M
    \\
;

/// The start of the same tree from a Linux shard on Zig 0.17 that rebuilt
/// nearly everything, followed by the `translate-c` it also reran. The step
/// counts in the first line are those of this excerpt.
const summary_rebuilt_zig_0_17 =
    \\Build Summary: 19/19 steps succeeded
    \\run-check-test-wiring success
    \\+- run exe check_test_wiring success 6s
    \\   +- compile exe check_test_wiring debug native success 679ms MaxRSS:154M
    \\   +- install check_test_wiring success
    \\   |  +- compile exe check_test_wiring debug native (reused)
    \\   +- compile test machine_code_shim debug native-native-musl success 3s MaxRSS:375M
    \\   |  +- WriteFile Builtin.bin success
    \\   |  |  +- run exe builtin_compiler (Builtin.bin) success 18s
    \\   |  |  |  +- compile exe builtin_compiler debug native success 22s MaxRSS:1G
    \\   |  |  |     +- options cached
    \\   |  |  |     +- run exe compiler_identity (compiler_identity.zig) cached
    \\   |  |  |        +- compile exe compiler_identity fast native cached 7ms MaxRSS:38M
    \\   |  |  |        +- WriteFile src/backend/dev/CallingConvention.zig cached
    \\   |  |  |        +- run exe compiler_identity (toolchain_identity.zig) cached
    \\   |  |  |           +- compile exe compiler_identity fast native (reused)
    \\   |  |  |           +- WriteFile lib cached
    \\   |  |  +- run exe builtin_compiler (Builtin.bin) (+1 more reused dependencies)
    \\   |  |  +- run exe builtin_compiler (Builtin.bin) (+1 more reused dependencies)
    \\   |  +- WriteFile cached
    \\   |  +- compile obj machine_code_shim_test_host debug native-native-musl success 599ms MaxRSS:132M
    \\   |  |  +- options (reused)
    \\   |  |  +- run exe compiler_identity (compiler_identity.zig) (+3 more reused dependencies)
    \\   |  +- compile obj roc_builtins debug native-native-musl cached 20ms MaxRSS:40M
    \\   |  +- compile obj machine_code_shim_test_host debug native-native-musl (+2 more reused dependencies)
    \\   |  +- translate-c success 36s MaxRSS:699M
    \\   |  |  +- WriteFile roc_zstd.h cached
    \\
;

/// A phase log from a Linux shard on Zig 0.16 whose test binary crashed, up to
/// Zig's closing error: the failing step's own output comes before the
/// summary, and more text follows the tree.
const summary_failed_tests_zig_0_16 =
    \\run-test-zig-module-postcheck
    \\+- run test postcheck 622 pass, 1 crash (623 total)
    \\error: 'structural_test.test.hosted Try adaptation consumes checker-recorded nominal provenance' terminated with signal ABRT with stderr:
    \\       thread 11278 panic: missing source slice start marker
    \\       Cannot print stack trace: stack tracing is disabled
    \\failed command: ./.zig-cache/o/6ca79fb3c8c049151c41fed337c0a5e8/postcheck --cache-dir=./.zig-cache --seed=0x121a8a81 --listen=-
    \\
    \\Build Summary: 2/4 steps succeeded (1 failed); 622/623 tests passed (1 crashed)
    \\run-test-zig-module-postcheck transitive failure
    \\+- run test postcheck 622 pass, 1 crash (623 total)
    \\   +- compile test postcheck Debug native-native-musl cached 46ms MaxRSS:41M
    \\      +- options cached
    \\
    \\error: the following build command failed with exit code 1:
;

/// The summary of a local Zig 0.17 build that stopped on compile errors.
const summary_compile_errors_zig_0_17 =
    \\Build Summary: 3/10 steps succeeded (2 failed)
    \\test-wasm transitive failure
    \\+- install test-wasm transitive failure
    \\|  +- compile exe test-wasm debug native 1 errors
    \\|     +- options cached
    \\|     +- options (reused)
    \\+- run exe test-wasm transitive failure
    \\   +- compile exe test-wasm debug native (+2 more reused dependencies)
    \\   +- install transitive failure
    \\      +- install bytebox transitive failure
    \\      |  +- compile exe bytebox debug native 24 errors
    \\      |     +- options (reused)
    \\      |     +- options (reused)
    \\      +- install bytebox success
    \\         +- compile lib bytebox debug native success 2s MaxRSS:339M
    \\            +- options (reused)
    \\
    \\error: the following build command exited with code 1:
;

/// Expects `output` to read as `reused` cached compile steps and the
/// `compiled` ones having run.
fn expectCompileWork(output: []const u8, reused: usize, compiled: []const []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const work = switch (try readCompileWork(arena.allocator(), output)) {
        .measured => |measured| measured,
        .unreadable_summary => |problem| {
            std.debug.print("unreadable summary: {s}\n", .{problem});
            return error.UnreadableSummary;
        },
        .not_run, .missing_summary => return error.NotMeasured,
    };
    try std.testing.expectEqual(reused, work.reused);
    try std.testing.expectEqual(compiled.len, work.compiled.len);
    for (compiled, work.compiled) |expected, actual| try std.testing.expectEqualStrings(expected, actual);
}

/// Expects `output` to be rejected as an unreadable summary whose problem
/// mentions `needle`.
fn expectUnreadableSummary(output: []const u8, needle: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    switch (try readCompileWork(arena.allocator(), output)) {
        .unreadable_summary => |problem| {
            if (std.mem.find(u8, problem, needle) == null) std.debug.print("unexpected problem: {s}\n", .{problem});
            try std.testing.expect(std.mem.find(u8, problem, needle) != null);
        },
        .not_run, .measured, .missing_summary => return error.SummaryWasNotRejected,
    }
}

test "SummaryStep.parse reads each state Zig prints" {
    const cases = [_]struct { line: []const u8, name: []const u8, state: StepState }{
        .{ .line = "run-check-test-wiring success", .name = "run-check-test-wiring", .state = .ran },
        .{ .line = "+- run exe check_test_wiring success 10s", .name = "run exe check_test_wiring", .state = .ran },
        .{
            .line = "   +- compile exe check_test_wiring Debug native cached 116ms MaxRSS:19M",
            .name = "compile exe check_test_wiring Debug native",
            .state = .cached,
        },
        .{ .line = "   |  +- WriteFile Builtin.bin cached", .name = "WriteFile Builtin.bin", .state = .cached },
        .{
            .line = "   |  |  |  +- compile exe builtin_compiler debug native success 22s MaxRSS:1G",
            .name = "compile exe builtin_compiler debug native",
            .state = .ran,
        },
        .{ .line = "   |  +- translate-c success 36s MaxRSS:699M", .name = "translate-c", .state = .ran },
        .{ .line = "   +- install check_test_wiring success", .name = "install check_test_wiring", .state = .ran },
        // A step name is free text: spaces, parentheses and path separators.
        .{
            .line = "   |  |  +- run C:\\hostedtoolcache\\windows\\zig\\0.16.0\\x64\\zig.exe (roc_builtins.o) cached",
            .name = "run C:\\hostedtoolcache\\windows\\zig\\0.16.0\\x64\\zig.exe (roc_builtins.o)",
            .state = .cached,
        },
        // Test counts, passing and failing.
        .{ .line = "+- run test base 158 pass, 1 skip (159 total) 294ms MaxRSS:32M", .name = "run test base", .state = .ran },
        .{ .line = "+- run test minici_test 31 pass (31 total) 5ms MaxRSS:11M", .name = "run test minici_test", .state = .ran },
        .{ .line = "+- run test compile 793 pass, 1 fail (794 total)", .name = "run test compile", .state = .failed },
        .{ .line = "+- run test cli_test 374 pass, 2 skip, 1 fail (377 total)", .name = "run test cli_test", .state = .failed },
        .{ .line = "+- run test postcheck 622 pass, 1 crash (623 total)", .name = "run test postcheck", .state = .failed },
        .{ .line = "+- run test eval 9 pass, 1 timeout (10 total); 2 leaks", .name = "run test eval", .state = .failed },
        .{ .line = "+- run test eval 10 pass (10 total); 1 error logs", .name = "run test eval", .state = .failed },
        // Other failures.
        .{ .line = "|  +- compile exe test-wasm debug native 1 errors", .name = "compile exe test-wasm debug native", .state = .failed },
        .{ .line = "+- run exe minici failure", .name = "run exe minici", .state = .failed },
        .{ .line = "+- run exe minici w", .name = "run exe minici", .state = .failed },
        // Steps that did not run.
        .{ .line = "run-test-zig-module-postcheck transitive failure", .name = "run-test-zig-module-postcheck", .state = .not_run },
        .{ .line = "+- install roc transitive skip", .name = "install roc", .state = .not_run },
        .{ .line = "+- run exe roc skipped", .name = "run exe roc", .state = .not_run },
        .{
            .line = "+- compile exe roc debug native skipped (not enough memory) upper bound of 8000000000 exceeded runner limit (4000000000)",
            .name = "compile exe roc debug native",
            .state = .not_run,
        },
        // Repeats of a step printed elsewhere in the tree.
        .{
            .line = "   |  +- compile exe check_test_wiring Debug native (reused)",
            .name = "compile exe check_test_wiring Debug native",
            .state = .repeated,
        },
        .{
            .line = "   |  |  +- run exe builtin_compiler (Builtin.bin) (+1 more reused dependencies)",
            .name = "run exe builtin_compiler (Builtin.bin)",
            .state = .repeated,
        },
    };
    for (cases) |case| {
        const step = SummaryStep.parse(case.line) orelse {
            std.debug.print("unrecognized: {s}\n", .{case.line});
            return error.UnrecognizedStepLine;
        };
        try std.testing.expectEqualStrings(case.name, step.name);
        try std.testing.expectEqual(case.state, step.state);
    }
}

test "SummaryStep.parse rejects a line Zig does not print" {
    const lines = [_][]const u8{
        // A state that is not one of Zig's.
        "+- compile exe roc debug native rebuilt 3s MaxRSS:1G",
        // No state at all.
        "+- compile exe roc debug native",
        // Indented without the `+- ` that ends the tree drawing.
        "   compile exe roc debug native cached",
        // Zig never times a failing test count.
        "+- run test eval 9 pass, 1 fail (10 total) 12ms",
        // A unit Zig does not use.
        "+- compile exe roc debug native success 3h",
    };
    for (lines) |line| {
        if (SummaryStep.parse(line)) |step| {
            std.debug.print("read `{s}` as {s}\n", .{ line, @tagName(step.state) });
            return error.LineWasNotRejected;
        }
    }
}

test "isCompileStep covers the steps in which Zig runs the compiler" {
    try std.testing.expect(isCompileStep("compile exe roc debug native-native-musl"));
    try std.testing.expect(isCompileStep("compile test check Debug native-native"));
    try std.testing.expect(isCompileStep("compile obj roc_builtins32_bc fast wasm32-freestanding-none"));
    try std.testing.expect(isCompileStep("compile lib zstd Debug native-native"));
    try std.testing.expect(isCompileStep("translate-c"));
    // The phase's own leaf step, and steps that copy or generate files.
    try std.testing.expect(!isCompileStep("run exe check_test_wiring"));
    try std.testing.expect(!isCompileStep("run test base"));
    try std.testing.expect(!isCompileStep("run exe builtin_compiler (Builtin.bin)"));
    try std.testing.expect(!isCompileStep("install check_test_wiring"));
    try std.testing.expect(!isCompileStep("WriteFile Builtin.bin"));
    try std.testing.expect(!isCompileStep("options"));
}

test "readCompileWork counts a fully reused tree" {
    // `(reused)` and `(+N more reused dependencies)` lines repeat a step that
    // is already counted.
    try expectCompileWork(summary_reused_zig_0_16, 2, &.{});
    try expectCompileWork(summary_reused_zig_0_17, 3, &.{});
}

test "readCompileWork names the compile steps that ran" {
    // Durations and peak RSS are not part of a step's name, and the `(reused)`
    // repeat of `check_test_wiring` is not a second compile.
    try expectCompileWork(summary_rebuilt_zig_0_16, 2, &.{
        "compile exe check_test_wiring Debug native",
        "compile test machine_code_shim Debug native-native-musl",
        "compile exe builtin_compiler Debug native",
    });
    try expectCompileWork(summary_rebuilt_zig_0_17, 2, &.{
        "compile exe check_test_wiring debug native",
        "compile test machine_code_shim debug native-native-musl",
        "compile exe builtin_compiler debug native",
        "compile obj machine_code_shim_test_host debug native-native-musl",
        "translate-c",
    });
}

test "readCompileWork reads the tree of a phase that failed" {
    // The test binary crashed; its compile step was still reused.
    try expectCompileWork(summary_failed_tests_zig_0_16, 1, &.{});
    // A compile step with errors ran the compiler; one behind it did not run.
    try expectCompileWork(summary_compile_errors_zig_0_17, 0, &.{
        "compile exe test-wasm debug native",
        "compile exe bytebox debug native",
        "compile lib bytebox debug native",
    });
}

test "readCompileWork reads the last summary in the output" {
    // A job's own output comes first in the log and can hold the summary of a
    // `zig build` the job ran itself.
    try expectCompileWork(summary_compile_errors_zig_0_17 ++ "\n" ++ summary_reused_zig_0_16, 2, &.{});
}

test "readCompileWork reports a missing summary instead of counting nothing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const output =
        \\Checking for separator comments...
        \\All lints passed!
        \\
    ;
    try std.testing.expect(try readCompileWork(arena.allocator(), output) == .missing_summary);
    try std.testing.expect(try readCompileWork(arena.allocator(), "") == .missing_summary);
}

test "readCompileWork rejects a state it does not recognize" {
    const output =
        \\Build Summary: 3/3 steps succeeded; 158/159 tests passed (1 skipped)
        \\run-test-zig-module-base success
        \\+- run test base 158 pass, 1 skip (159 total) 294ms MaxRSS:32M
        \\   +- compile test base Debug native-native rebuilt 98ms MaxRSS:19M
        \\
    ;
    try expectUnreadableSummary(output, "unrecognized step line `   +- compile test base Debug native-native rebuilt 98ms MaxRSS:19M`");
    try expectUnreadableSummary("Build Summary: all steps succeeded\nroot success\n", "unrecognized step counts");
}

test "readCompileWork rejects a tree that does not list every step the summary counts" {
    // The reused tree cut off two lines early: one compile step is missing.
    const cut = std.mem.find(u8, summary_reused_zig_0_16, "      +- compile exe stack_overflow_test_helper") orelse return error.MissingLine;
    try expectUnreadableSummary(summary_reused_zig_0_16[0..cut], "the summary counts 7/7 steps succeeded but its tree lists 6/6");
    // A summary line with no tree under it.
    try expectUnreadableSummary("Build Summary: 7/7 steps succeeded\n", "its tree lists 0/0");
}

/// A result for `zig build <step>` whose summary read as `reused` cached
/// compile steps and the `compiled` ones having run.
fn measuredResult(comptime step: []const u8, status: []const u8, reused: usize, compiled: []const []const u8) CommandResult {
    var result = testResult(status, 1);
    result.command = &.{ "zig", "build", step };
    result.compile_work = .{ .measured = .{ .reused = reused, .compiled = compiled } };
    return result;
}

/// Expects the canary to flag exactly `undeclared` for `result`, or to pass it
/// when `undeclared` is empty.
fn expectUndeclaredCompiles(result: CommandResult, expectation: CompileExpectation, undeclared: []const []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const failure = try cacheReuseFailure(arena.allocator(), result, expectation) orelse {
        try std.testing.expectEqual(@as(usize, 0), undeclared.len);
        return;
    };
    switch (failure) {
        .undeclared_compiles => |names| {
            try std.testing.expectEqual(undeclared.len, names.len);
            for (undeclared, names) |expected, actual| try std.testing.expectEqualStrings(expected, actual);
        },
        .missing_summary, .unreadable_summary => return error.WrongFailure,
    }
}

test "cacheReuseFailure flags the compile steps a job does not declare" {
    const lock = "compile obj glue_zig_abi_lock_x64_linux Debug x86_64-linux-musl";
    const generator = "compile exe generate_foreign_abi_lock ReleaseSafe native";
    const compiled = [_][]const u8{ lock, generator };
    const result = measuredResult("run-check-glue-abi", "pass", 97, &compiled);

    // Nothing declared: `build-ci` should have built both.
    try expectUndeclaredCompiles(result, .{ .only = &.{} }, &compiled);
    // A declared step is the job's own work; only the other one is flagged.
    try expectUndeclaredCompiles(result, .{ .only = &.{lock} }, &.{generator});
    try expectUndeclaredCompiles(result, .{ .only = &compiled }, &.{});
    // A declaration names a step exactly; Zig 0.17's spelling is another name.
    try expectUndeclaredCompiles(result, .{ .only = &.{"compile obj glue_zig_abi_lock_x64_linux debug x86_64-linux-musl"} }, &compiled);
    // Without a `build-ci` before it, a phase builds what it needs.
    try expectUndeclaredCompiles(result, .unrestricted, &.{});
    // A phase that reused everything passes.
    try expectUndeclaredCompiles(measuredResult("run-check-test-wiring", "pass", 83, &.{}), .{ .only = &.{} }, &.{});
    // A phase that failed is still held to it.
    try expectUndeclaredCompiles(measuredResult("run-check-glue-abi", "fail", 97, &compiled), .{ .only = &.{} }, &compiled);
}

test "cacheReuseFailure never takes unknown compile work for none" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // A phase that passed always prints a summary, so one without it fails,
    // whether or not the lane restricts what it may compile.
    var passed = testResult("pass", 1);
    passed.compile_work = .missing_summary;
    for ([_]CompileExpectation{ .unrestricted, .{ .only = &.{} } }) |expectation| {
        const failure = try cacheReuseFailure(allocator, passed, expectation) orelse return error.NotFlagged;
        try std.testing.expect(failure == .missing_summary);
    }

    // A phase that failed or crashed before the summary is already reported.
    for ([_][]const u8{ "fail", "crash" }) |status| {
        var stopped = testResult(status, 1);
        stopped.compile_work = .missing_summary;
        try std.testing.expect(try cacheReuseFailure(allocator, stopped, .{ .only = &.{} }) == null);
    }

    // A summary that cannot be read fails whatever the phase's own status.
    for ([_][]const u8{ "pass", "fail" }) |status| {
        var unreadable = testResult(status, 1);
        unreadable.compile_work = .{ .unreadable_summary = "unrecognized step line `x`" };
        const failure = try cacheReuseFailure(allocator, unreadable, .unrestricted) orelse return error.NotFlagged;
        try std.testing.expect(failure == .unreadable_summary);
    }

    // A skipped phase ran nothing.
    try std.testing.expect(try cacheReuseFailure(allocator, testResult("skip", 1), .{ .only = &.{} }) == null);
}

test "cache reuse is enforced whenever build-ci precedes the phases" {
    const cases = [_]struct { args: []const []const u8, enforced: bool }{
        // A plain run builds `build-ci` itself.
        .{ .args = &.{ "minici", "zig" }, .enforced = true },
        .{ .args = &.{ "minici", "zig", "--minici-shard", "linux-core" }, .enforced = true },
        // A CI shard skips the build and restores the build job's `.zig-cache`.
        .{ .args = &.{ "minici", "zig", "--minici-skip-build", "--minici-shard", "windows-core" }, .enforced = true },
        .{ .args = &.{ "minici", "zig", "--minici-skip-build", "--minici-only", "run-check-test-wiring" }, .enforced = true },
        // The source lane has no `build-ci`: its checks build their own tools.
        .{ .args = &.{ "minici", "zig", "--minici-shard", "source" }, .enforced = false },
        .{ .args = &.{ "minici", "zig", "--minici-skip-build", "--minici-shard", "source" }, .enforced = false },
    };
    for (cases) |case| {
        const parsed = try parseMiniArgs(std.testing.allocator, case.args);
        defer std.testing.allocator.free(parsed.build_args);
        try std.testing.expectEqual(case.enforced, buildCiPrecedesPhases(parsed));
    }
}

test "appendCompileWorkReport lists the compiled steps under the result line" {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(std.testing.allocator);

    const wiring = "compile exe check_test_wiring Debug native";
    const check = "compile test check Debug native-native-musl";
    var result = measuredResult("run-check-test-wiring", "pass", 54, &.{ wiring, check });
    result.cache_reuse_failure = .{ .undeclared_compiles = &.{check} };
    try appendCompileWorkReport(&out, std.testing.allocator, result, .steps, &.{wiring});
    try std.testing.expectEqualStrings(
        \\  Compile steps: 54 reused, 2 compiled:
        \\    compile exe check_test_wiring Debug native (declared in `expected_compiles`)
        \\    compile test check Debug native-native-musl
        \\  Cache not reused: `build-ci` should have built 1 of these.
        \\
    , out.items);

    // A phase that reused everything adds nothing to the log.
    out.clearRetainingCapacity();
    try appendCompileWorkReport(&out, std.testing.allocator, measuredResult("run-check-test-wiring", "pass", 83, &.{}), .steps, &.{});
    try std.testing.expectEqualStrings("", out.items);

    // `build-ci` shows its counts, not its steps.
    out.clearRetainingCapacity();
    try appendCompileWorkReport(&out, std.testing.allocator, measuredResult("build-ci", "pass", 12, &.{ wiring, check }), .counts, &.{});
    try std.testing.expectEqualStrings("  Compile steps: 12 reused, 2 compiled\n", out.items);

    // Unknown compile work is said to be unknown.
    out.clearRetainingCapacity();
    var silent = testResult("pass", 1);
    silent.compile_work = .missing_summary;
    silent.cache_reuse_failure = .missing_summary;
    try appendCompileWorkReport(&out, std.testing.allocator, silent, .steps, &.{});
    try std.testing.expectEqualStrings(
        "  `step` passed without printing a `Build Summary:`, so its compile work is unknown\n",
        out.items,
    );
}

test "appendCacheReuseSummary reports a run that reused everything" {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(std.testing.allocator);

    const results = [_]CommandResult{
        measuredResult("run-check-test-wiring", "pass", 83, &.{}),
        testResult("skip", 1),
        measuredResult("run-check-simd-codegen", "pass", 210, &.{}),
    };
    try appendCacheReuseSummary(&out, std.testing.allocator, true, testResult("skip", 1), &results);
    try std.testing.expectEqualStrings(
        "MiniCI cache reuse: ok; 2 run phases measured, 293 compile steps reused, 0 compiled\n",
        out.items,
    );

    out.clearRetainingCapacity();
    try appendCacheReuseSummary(&out, std.testing.allocator, true, testResult("fail", 1), &.{});
    try std.testing.expectEqualStrings("MiniCI cache reuse: no run phase was measured\n", out.items);
}

test "appendCacheReuseSummary keeps a job's own configuration out of the reuse totals" {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(std.testing.allocator);

    // The ReleaseSafe shim check compiles its own outputs. They are not steps
    // the run failed to reuse, so they neither fail it nor count as compiled.
    var own = measuredResult("run-check-machine-code-shim-archive", "pass", 4, &.{
        "compile lib roc_machine_code_shim safe native-native-musl",
        "compile exe machine_code_shim_archive_check debug native",
    });
    own.own_configuration = true;
    const results = [_]CommandResult{
        measuredResult("run-check-test-wiring", "pass", 83, &.{}),
        own,
    };
    try appendCacheReuseSummary(&out, std.testing.allocator, true, testResult("skip", 1), &results);
    try std.testing.expectEqualStrings(
        \\MiniCI cache reuse: ok; 1 run phase measured, 83 compile steps reused, 0 compiled
        \\  Not counted above: 2 compile steps in jobs that build a configuration of their own.
        \\
    , out.items);
}

test "only a job with build options of its own is exempt from the cache-reuse canary" {
    // Every job listed here compiles a configuration `build-ci` does not build.
    // Adding one is a decision to compile in a shard, so it is made here.
    var with_build_args: usize = 0;
    for (jobs) |job| {
        if (job.build_args.len == 0) continue;
        with_build_args += 1;
        try std.testing.expectEqualStrings("run-check-machine-code-shim-archive", job.name);
        try std.testing.expectEqual(@as(usize, 0), job.expected_compiles.len);
    }
    try std.testing.expectEqual(@as(usize, 1), with_build_args);
}

test "appendCacheReuseSummary names each phase that did not reuse its cache" {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(std.testing.allocator);

    const compiled = [_][]const u8{
        "compile exe check_test_wiring Debug native",
        "compile test check Debug native-native-musl",
    };
    var wiring = measuredResult("run-check-test-wiring", "pass", 54, &compiled);
    wiring.cache_reuse_failure = .{ .undeclared_compiles = &compiled };
    var silent = testResult("pass", 1);
    silent.command = &.{ "zig", "build", "run-check-snapshots" };
    silent.compile_work = .missing_summary;
    silent.cache_reuse_failure = .missing_summary;
    const results = [_]CommandResult{ wiring, measuredResult("run-check-simd-codegen", "pass", 210, &.{}), silent };

    try appendCacheReuseSummary(&out, std.testing.allocator, true, testResult("skip", 1), &results);
    try std.testing.expectEqualStrings(
        \\MiniCI cache reuse: FAILED in 2 phases; 2 run phases measured, 264 compile steps reused, 2 compiled
        \\  `run-check-test-wiring` compiled 2 steps that `build-ci` should have built:
        \\    compile exe check_test_wiring Debug native
        \\    compile test check Debug native-native-musl
        \\  `run-check-snapshots` passed without printing a `Build Summary:`, so its compile work is unknown
        \\  Either `build-ci` does not build these steps (see build.zig), or what it built was not reusable here.
        \\  A job that is meant to compile a step says so in `expected_compiles` (src/build/minici.zig).
        \\
    , out.items);

    // The phase tally stays that of a run whose phases all passed.
    out.clearRetainingCapacity();
    try appendSummaryLine(&out, std.testing.allocator, 4, testResult("skip", 1), &results, 1_000_000_000);
    try std.testing.expectEqualStrings(
        "MiniCI summary: 4/4 phases ran; 3 passed, 0 failed, 0 crashed, 1 skipped; wall 1.000s\n",
        out.items,
    );
}

test "appendCacheReuseSummary does not flag compiles in the lane without build-ci" {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(std.testing.allocator);

    const results = [_]CommandResult{
        measuredResult("run-check-tidy", "pass", 0, &.{"compile exe tidy debug native"}),
    };
    try appendCacheReuseSummary(&out, std.testing.allocator, false, testResult("skip", 1), &results);
    try std.testing.expectEqualStrings(
        "MiniCI cache reuse: not enforced (no `build-ci` before these phases); 1 run phase measured, 0 compile steps reused, 1 compiled\n",
        out.items,
    );
}

test "report.json carries compile work and the canary's verdict" {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(std.testing.allocator);

    const compiled = [_][]const u8{"compile exe check_test_wiring Debug native"};
    var wiring = measuredResult("run-check-test-wiring", "pass", 54, &compiled);
    wiring.cache_reuse_failure = .{ .undeclared_compiles = &compiled };
    var unreadable = testResult("pass", 1);
    unreadable.compile_work = .{ .unreadable_summary = "unrecognized step line `x`" };
    unreadable.cache_reuse_failure = .{ .unreadable_summary = "unrecognized step line `x`" };
    const results = [_]CommandResult{ wiring, unreadable };

    try appendReportJsonObject(&out, std.testing.allocator, 1, true, testResult("skip", 1), &results);
    const expected = [_][]const u8{
        "\"schema_version\": 2,",
        "\"cache_reuse_enforced\": true,",
        // A command that did not run has no counts, rather than counts of zero.
        "\"compile_work\": {\"summary\": \"not_run\"},\n    \"cache_reuse_failure\": null",
        "\"compile_work\": {\"summary\": \"measured\", \"reused\": 54, \"compiled\": 1, " ++
            "\"compiled_steps\": [\"compile exe check_test_wiring Debug native\"]},",
        "\"cache_reuse_failure\": {\"kind\": \"undeclared_compiles\", \"steps\": [\"compile exe check_test_wiring Debug native\"]}",
        "\"compile_work\": {\"summary\": \"unreadable_summary\", \"problem\": \"unrecognized step line `x`\"},",
        "\"cache_reuse_failure\": {\"kind\": \"unreadable_summary\", \"problem\": \"unrecognized step line `x`\"}",
    };
    for (expected) |needle| {
        if (std.mem.find(u8, out.items, needle) == null) std.debug.print("missing from report: {s}\n{s}\n", .{ needle, out.items });
        try std.testing.expect(std.mem.find(u8, out.items, needle) != null);
    }
}
