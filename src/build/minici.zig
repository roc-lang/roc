//! MiniCI runner for split build/run jobs.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");

const out_dir = "zig-out/minici";
const raw_dir = out_dir ++ "/raw";
const logs_dir = out_dir ++ "/logs";
const heartbeat_env = "MINICI_HEARTBEAT_INTERVAL_MS";
const default_heartbeat_interval_ms: u64 = 30_000;
/// Override for the auto-detected CPU budget on memory-constrained hosts.
/// See `applyMemoryAwareCpuLimit`. `MINICI_MAX_CPUS=0` or unset means auto.
const cpu_limit_env = "MINICI_MAX_CPUS";

/// How many bytes from the start and end of a failing step's log to echo to
/// the console. Compiler and test errors land near the top of the output, while
/// `--summary all` prints a large build tree that pushes the terminating error
/// line to the very bottom, so surfacing both ends (and eliding the noisy
/// middle) keeps the failure actionable without a re-run.
const failure_log_head_bytes: usize = 12 * 1024;
const failure_log_tail_bytes: usize = 4 * 1024;

const JobKind = enum {
    single,
    harness,
};

const Job = struct {
    name: []const u8,
    kind: JobKind = .single,
    args: []const []const u8 = &.{},
    skip_reason: ?[]const u8 = null,
};

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
};

const jobs = [_]Job{
    // MiniCI trusts `build.zig` to keep build work behind `build-ci`. Keep this
    // list to leaf `run-*` steps. Do not add aliases or aggregate steps that
    // hide useful reporting boundaries.
    .{ .name = "run-check-zig-format" },
    .{ .name = "run-check-zig-lints" },
    .{ .name = "run-check-tidy" },
    .{ .name = "run-check-git-lints" },
    .{ .name = "run-check-type-checker-patterns" },
    .{ .name = "run-check-enum-from-int-zero" },
    .{ .name = "run-check-unused-suppression" },
    .{ .name = "run-check-semantic-audit" },
    .{ .name = "run-check-postcheck-architecture" },
    .{ .name = "run-check-wasm-builtin-routing" },
    .{ .name = "run-check-panic" },
    .{ .name = "run-check-cli-global-stdio" },
    .{ .name = "run-check-test-wiring" },
    .{ .name = "run-check-builtin-format" },
    .{ .name = "run-check-glue-abi" },
    .{ .name = "run-check-snapshots" },
    .{ .name = "run-check-test-asset-coverage" },
    .{ .name = "run-test-zig-module-collections" },
    .{ .name = "run-test-zig-module-base" },
    .{ .name = "run-test-zig-module-types" },
    .{ .name = "run-test-zig-module-builtins" },
    .{ .name = "run-test-zig-module-compile" },
    .{ .name = "run-test-zig-module-reporting" },
    .{ .name = "run-test-zig-module-parse" },
    .{ .name = "run-test-zig-module-can" },
    .{ .name = "run-test-zig-module-check" },
    .{ .name = "run-test-zig-module-ctx" },
    .{ .name = "run-test-zig-module-eval" },
    .{ .name = "run-test-zig-module-layout" },
    .{ .name = "run-test-zig-module-values" },
    .{ .name = "run-test-zig-module-ipc" },
    .{ .name = "run-test-zig-module-fmt" },
    .{ .name = "run-test-zig-module-watch" },
    .{ .name = "run-test-zig-module-bundle" },
    .{ .name = "run-test-zig-module-unbundle" },
    .{ .name = "run-test-zig-module-base58" },
    .{ .name = "run-test-zig-module-lsp" },
    .{ .name = "run-test-zig-module-lsp_unit" },
    .{ .name = "run-test-zig-module-lsp_integration", .kind = .harness },
    .{ .name = "run-test-zig-module-backend" },
    .{ .name = "run-test-zig-module-lir_core" },
    .{ .name = "run-test-zig-module-postcheck" },
    .{ .name = "run-test-zig-module-lir" },
    .{ .name = "run-test-zig-module-symbol" },
    .{ .name = "run-test-zig-module-sljmp" },
    .{ .name = "run-test-zig-module-echo_platform" },
    .{ .name = "run-test-zig-module-docs" },
    .{ .name = "run-test-zig-module-host_alloc" },
    .{ .name = "run-test-zig-module-bump" },
    .{ .name = "run-test-zig-module-roc_target" },
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
    .{ .name = "run-test-zig-cli-runner-unit" },
    .{ .name = "run-test-zig-backend-llvm" },
    .{ .name = "run-test-eval", .kind = .harness, .args = &.{ "--timeout", "120000" } },
    .{ .name = "run-test-eval-host-effects", .kind = .harness },
    .{ .name = "run-test-lambda-mono-differential", .kind = .harness },
    .{ .name = "run-test-playground", .kind = .harness },
    .{ .name = "run-test-cli", .kind = .harness },
    .{ .name = "run-test-serialization-sizes" },
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
        \\  --minici-skip-build   assume `build-ci` already ran and run selected jobs only
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

fn setSelectionOnly(selection: *Selection, value: []const u8, arg: []const u8) !void {
    if (selection.from != null or selection.to != null or selection.after != null or selection.before != null) {
        printSelectionConflict("conflicting selection option `{s}`; --minici-only cannot be combined with range options", arg);
        return error.InvalidMiniCiArgument;
    }
    selection.from = value;
    selection.to = value;
}

fn parseMiniArgs(allocator: std.mem.Allocator, args: []const []const u8) !ParsedArgs {
    const zig_exe = if (args.len >= 2) args[1] else "zig";
    var build_args = std.ArrayList([]const u8).empty;
    errdefer build_args.deinit(allocator);
    var selection = Selection{};
    var skip_build = false;

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
            try setSelectionFrom(&selection, args[i], arg);
        } else if (std.mem.startsWith(u8, arg, "--minici-from=")) {
            const value = arg["--minici-from=".len..];
            if (value.len == 0) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            try setSelectionFrom(&selection, value, arg);
        } else if (std.mem.eql(u8, arg, "--minici-to")) {
            if (i + 1 >= args.len) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            i += 1;
            try setSelectionTo(&selection, args[i], arg);
        } else if (std.mem.startsWith(u8, arg, "--minici-to=")) {
            const value = arg["--minici-to=".len..];
            if (value.len == 0) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            try setSelectionTo(&selection, value, arg);
        } else if (std.mem.eql(u8, arg, "--minici-after")) {
            if (i + 1 >= args.len) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            i += 1;
            try setSelectionAfter(&selection, args[i], arg);
        } else if (std.mem.startsWith(u8, arg, "--minici-after=")) {
            const value = arg["--minici-after=".len..];
            if (value.len == 0) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            try setSelectionAfter(&selection, value, arg);
        } else if (std.mem.eql(u8, arg, "--minici-before")) {
            if (i + 1 >= args.len) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            i += 1;
            try setSelectionBefore(&selection, args[i], arg);
        } else if (std.mem.startsWith(u8, arg, "--minici-before=")) {
            const value = arg["--minici-before=".len..];
            if (value.len == 0) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            try setSelectionBefore(&selection, value, arg);
        } else if (std.mem.eql(u8, arg, "--minici-only")) {
            if (i + 1 >= args.len) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            i += 1;
            try setSelectionOnly(&selection, args[i], arg);
        } else if (std.mem.startsWith(u8, arg, "--minici-only=")) {
            const value = arg["--minici-only=".len..];
            if (value.len == 0) {
                printUsageError("missing value after `{s}`", arg);
                return error.InvalidMiniCiArgument;
            }
            try setSelectionOnly(&selection, value, arg);
        } else {
            try build_args.append(allocator, arg);
        }
    }

    return .{
        .zig_exe = zig_exe,
        .build_args = try build_args.toOwnedSlice(allocator),
        .selection = selection,
        .skip_build = skip_build,
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

const CommandResult = struct {
    status: []const u8,
    start_ns: u64,
    end_ns: u64,
    duration_ns: u64,
    log_path: []const u8,
    command: []const []const u8,
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

fn printSummary(
    allocator: std.mem.Allocator,
    total_phases: usize,
    build_result: CommandResult,
    results: []const CommandResult,
    wall_ns: u64,
) !void {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(allocator);
    try appendSummaryLine(&out, allocator, total_phases, build_result, results, wall_ns);
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
        else => "crash",
    };
    const ended = nowNs(io);

    return .{
        .status = status,
        .start_ns = started -| run_started_ns,
        .end_ns = ended -| run_started_ns,
        .duration_ns = ended -| started,
        .log_path = log_path,
        .command = argv,
        .heartbeat_printed = heartbeat.printed.load(.acquire),
    };
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
    };
}

fn buildCommand(
    allocator: std.mem.Allocator,
    zig_exe: []const u8,
    build_args: []const []const u8,
    step: []const u8,
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

fn appendCommandJson(out: *std.ArrayList(u8), allocator: std.mem.Allocator, command: []const []const u8) !void {
    try out.appendSlice(allocator, "[");
    for (command, 0..) |arg, i| {
        if (i > 0) try out.appendSlice(allocator, ", ");
        try appendJsonString(out, allocator, arg);
    }
    try out.appendSlice(allocator, "]");
}

fn writeReportJson(
    allocator: std.mem.Allocator,
    io: std.Io,
    run_started_unix_ms: u64,
    build_result: CommandResult,
    results: []const CommandResult,
) !void {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(allocator);

    try out.appendSlice(allocator, "{\n  \"schema_version\": 1,\n  \"run_started_unix_ms\": ");
    try appendU64(&out, allocator, run_started_unix_ms);
    try out.appendSlice(allocator, ",\n  \"build_ci\": ");
    try appendResultJson(&out, allocator, build_result);
    try out.appendSlice(allocator, ",\n  \"jobs\": [\n");
    for (results, 0..) |result, i| {
        if (i > 0) try out.appendSlice(allocator, ",\n");
        try appendResultJson(&out, allocator, result);
    }
    try out.appendSlice(allocator, "\n  ]\n}\n");
    try writeFile(io, out_dir ++ "/report.json", out.items);
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
    try appendCommandJson(out, allocator, result.command);
    try out.appendSlice(allocator, ",\n    \"stats_path\": ");
    if (result.stats_path) |path| {
        try appendJsonString(out, allocator, path);
    } else {
        try out.appendSlice(allocator, "null");
    }
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

fn appendReportJsonObject(
    out: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    run_started_unix_ms: u64,
    build_result: CommandResult,
    results: []const CommandResult,
) !void {
    try out.appendSlice(allocator, "{\n  \"schema_version\": 1,\n  \"run_started_unix_ms\": ");
    try appendU64(out, allocator, run_started_unix_ms);
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
    try appendReportJsonObject(&report_json, allocator, run_started_unix_ms, build_result, results);
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

/// Total physical RAM in bytes, or null if it cannot be determined.
fn totalRamBytes() ?u64 {
    var info: std.os.linux.Sysinfo = undefined;
    if (@as(isize, @bitCast(std.os.linux.sysinfo(&info))) != 0) return null;
    return @as(u64, info.totalram) * @as(u64, @max(1, info.mem_unit));
}

/// How many heavy `zig build` compilations of the roc/LLVM sources fit in RAM at
/// once. Each needs several GiB (observed peak RSS 3.5-4.9 GiB), so we reserve
/// headroom for the OS/desktop and divide the rest. Never returns fewer than 1.
fn cpuBudgetForRam(total_ram_bytes: u64, online_cpus: usize) usize {
    const total_mib = total_ram_bytes / (1024 * 1024);
    const reserve_mib: u64 = 2048; // ~2 GiB for the OS/desktop + MiniCI itself
    const per_compile_mib: u64 = 4096; // ~4 GiB budget per concurrent compile
    if (total_mib <= reserve_mib + per_compile_mib) return 1;
    const fits: u64 = (total_mib - reserve_mib) / per_compile_mib;
    return @intCast(@min(@max(fits, 1), @as(u64, online_cpus)));
}

/// Whether we're running on Raspberry Pi hardware, per the firmware board name
/// exposed in the device tree (e.g. "Raspberry Pi 5 Model B Rev 1.0"). The auto
/// CPU limit is scoped to these boards: their limited RAM and slow SD-card swap
/// turn an over-parallel build into an OOM that can hard-reboot the machine,
/// whereas other hosts (including CI runners) have headroom and are left alone.
fn isRaspberryPi(io: std.Io, allocator: std.mem.Allocator) bool {
    const model = std.Io.Dir.cwd().readFileAlloc(io, "/sys/firmware/devicetree/base/model", allocator, .limited(256)) catch return false;
    return std.mem.find(u8, model, "Raspberry Pi") != null;
}

/// Parses `MINICI_MAX_CPUS`. Null when unset/empty/invalid or `<1` (auto). An
/// explicit value applies on any host, overriding the Raspberry Pi heuristic.
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

/// Restricts this process (and thus the children it forks) to CPUs `[0, count)`.
fn setCpuAffinity(count: usize) !void {
    var set: std.os.linux.cpu_set_t = [_]usize{0} ** (std.os.linux.CPU_SETSIZE / @sizeOf(usize));
    const word_bits = @bitSizeOf(usize);
    var cpu: usize = 0;
    while (cpu < count) : (cpu += 1) {
        set[cpu / word_bits] |= @as(usize, 1) << @intCast(cpu % word_bits);
    }
    try std.os.linux.sched_setaffinity(0, &set);
}

/// On a Raspberry Pi with little RAM, pin this process to a CPU subset so the
/// `zig build` and test-worker children it spawns don't run one multi-GiB
/// compile per core and exhaust RAM — which OOM-kills build jobs and, once swap
/// starts thrashing, can hang the machine hard enough to reboot it.
///
/// Zig sizes compile parallelism (and the CLI test runner sizes its worker pool)
/// to the visible CPU count, and children inherit our affinity: MiniCI forks
/// every child from this (main) thread via an inline `std.process.run`, so
/// restricting this thread restricts them all. `MINICI_MAX_CPUS=N` overrides the
/// heuristic and applies on any host.
fn applyMemoryAwareCpuLimit(io: std.Io, allocator: std.mem.Allocator, env: *const std.process.Environ.Map) void {
    if (builtin.os.tag != .linux) return;

    const online = onlineCpuCount() orelse return;
    if (online <= 1) return;

    const override = envCpuOverride(env);
    const budget = if (override) |n|
        @min(n, online)
    else blk: {
        // Auto-limiting only targets Raspberry Pi hardware.
        if (!isRaspberryPi(io, allocator)) return;
        break :blk cpuBudgetForRam(totalRamBytes() orelse return, online);
    };
    if (budget >= online) return;

    setCpuAffinity(budget) catch |err| {
        std.debug.print("note: could not limit CPUs ({s}); continuing at full parallelism\n", .{@errorName(err)});
        return;
    };

    if (override != null) {
        std.debug.print(
            "Limiting build to {d} of {d} CPUs ({s}).\n",
            .{ budget, online, cpu_limit_env },
        );
    } else {
        const total_mib = (totalRamBytes() orelse 0) / (1024 * 1024);
        std.debug.print(
            "Raspberry Pi with {d} MiB RAM detected: limiting build to {d} of {d} CPUs to avoid OOM (set {s}=N to override).\n",
            .{ total_mib, budget, online, cpu_limit_env },
        );
    }
}

/// Entry point: build the CI artifacts, then run each `run-*` job in order,
/// streaming heartbeats and a machine-readable report. Restricts CPU usage first
/// on memory-constrained hosts (see `applyMemoryAwareCpuLimit`).
pub fn main(init: std.process.Init) !void {
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
        else => std.process.exit(2),
    };
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
    applyMemoryAwareCpuLimit(io, allocator, init.environ_map);
    const run_started_ns = nowNs(io);
    const run_started_unix_ms = unixMs(io);
    const total_phases = jobs.len + 1;
    if (selected_jobs.first != 0 or selected_jobs.last != jobs.len - 1) {
        std.debug.print("MiniCI selection: `{s}` through `{s}`\n", .{
            jobs[selected_jobs.first].name,
            jobs[selected_jobs.last].name,
        });
    }

    const build_argv = try buildCommand(allocator, zig_exe, build_args, "build-ci", null, &.{});
    const build_log = logs_dir ++ "/build-ci.txt";
    const build_progress = Progress{ .current = 1, .total = total_phases };
    printBuildStart(build_progress);
    const build_result = if (parsed_args.skip_build)
        try skipCommand(io, build_argv, build_log, "skipped by --minici-skip-build\n", run_started_ns)
    else
        try runCommand(allocator, io, build_argv, build_log, heartbeat_interval_ms, run_started_ns, build_progress);
    if (build_result.heartbeat_printed) printBuildStart(build_progress);
    std.debug.print("{s} in {d:.3}s\n", .{ buildStatusText(build_result), seconds(build_result.duration_ns) });

    var results = std.ArrayList(CommandResult).empty;
    defer results.deinit(allocator);

    if (!isSuccessful(build_result)) {
        printFailureLog(allocator, io, build_result);
        printRerunHint(build_result);
        try writeReportJson(allocator, io, run_started_unix_ms, build_result, results.items);
        try writeHtml(allocator, io, run_started_unix_ms, build_result, results.items);
        try printSummary(allocator, total_phases, build_result, results.items, durationSince(io, run_started_ns));
        std.process.exit(1);
    }

    for (jobs, 0..) |job, job_index| {
        const log_path = try std.fmt.allocPrint(allocator, "{s}/{s}.txt", .{ logs_dir, job.name });
        const stats_path: ?[]const u8 = if (job.kind == .harness)
            try std.fmt.allocPrint(allocator, "{s}/{s}.json", .{ raw_dir, job.name })
        else
            null;
        const argv = try buildCommand(allocator, zig_exe, build_args, job.name, stats_path, job.args);
        const progress = Progress{ .current = job_index + 2, .total = total_phases };
        printRunStart(progress, job.name);
        const skip_reason: ?[]const u8 = if (selected_jobs.includes(job_index))
            job.skip_reason
        else
            "excluded by MiniCI selection\n";
        var result = if (skip_reason) |reason|
            try skipCommand(io, argv, log_path, reason, run_started_ns)
        else
            try runCommand(allocator, io, argv, log_path, heartbeat_interval_ms, run_started_ns, progress);
        result.stats_path = if (skip_reason == null) stats_path else null;
        try results.append(allocator, result);
        if (result.heartbeat_printed) printRunStart(progress, job.name);
        std.debug.print("{s} in {d:.3}s\n", .{ runStatusText(result), seconds(result.duration_ns) });

        if (!isSuccessful(result)) {
            printFailureLog(allocator, io, result);
            printRerunHint(result);
        }

        if (isCheckJob(job.name) and !isSuccessful(result)) {
            try writeReportJson(allocator, io, run_started_unix_ms, build_result, results.items);
            try writeHtml(allocator, io, run_started_unix_ms, build_result, results.items);
            try printSummary(allocator, total_phases, build_result, results.items, durationSince(io, run_started_ns));
            std.process.exit(1);
        }
    }

    try writeReportJson(allocator, io, run_started_unix_ms, build_result, results.items);
    try writeHtml(allocator, io, run_started_unix_ms, build_result, results.items);
    try printSummary(allocator, total_phases, build_result, results.items, durationSince(io, run_started_ns));

    for (results.items) |result| {
        if (!isSuccessful(result)) std.process.exit(1);
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

test "resolveSelection supports exclusive MiniCI range boundaries" {
    const selected = try resolveSelection(.{
        .after = "run-test-zig-module-roc_target",
        .before = "run-test-eval",
    });
    const after = jobIndexByName("run-test-zig-module-roc_target") orelse return error.MissingAfter;
    const before = jobIndexByName("run-test-eval") orelse return error.MissingBefore;

    try std.testing.expect(!selected.includes(after));
    try std.testing.expect(selected.includes(after + 1));
    try std.testing.expect(selected.includes(before - 1));
    try std.testing.expect(!selected.includes(before));
}

test "resolveSelection supports exhaustive adjacent MiniCI shards" {
    const first = try resolveSelection(.{ .to = "run-test-zig-module-roc_target" });
    const middle = try resolveSelection(.{
        .after = "run-test-zig-module-roc_target",
        .before = "run-test-eval",
    });
    const last = try resolveSelection(.{ .from = "run-test-eval" });

    const core_boundary = jobIndexByName("run-test-zig-module-roc_target") orelse return error.MissingCoreBoundary;
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
