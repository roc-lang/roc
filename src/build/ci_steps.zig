//! Build-system helpers for MiniCI command phases and reporting.

const std = @import("std");
const builtin = @import("builtin");

const Step = std.Build.Step;

/// Callback type for MiniCI phases implemented as in-process build checks.
pub const CheckStepFn = *const fn (*Step) anyerror!void;

const non_tty_test_progress_env = "ROC_TEST_PROGRESS_INTERVAL_MS";
const default_non_tty_test_progress_ms = "30000";
const mini_ci_heartbeat_env = "ROC_MINICI_HEARTBEAT_INTERVAL_MS";
const default_non_tty_heartbeat_ms: u64 = 60_000;

const PhasePosition = struct {
    index: usize,
    total: usize,
};

const RunContext = struct {
    progress_node: std.Progress.Node = .none,
    heartbeat_interval_ms: u64 = 0,
    aggregate_label: []const u8 = "ci",
    phase_position: ?PhasePosition = null,
};

const PhaseAction = union(enum) {
    argv: []const []const u8,
    zig_run: []const u8,
    zig_build: []const []const u8,
    callback: CheckStepFn,
};

const Phase = struct {
    label: []const u8,
    action: PhaseAction,
    failure_message: ?[]const u8 = null,
    abnormal_message: ?[]const u8 = null,
    skip_windows_message: ?[]const u8 = null,
    requires_git_worktree: bool = false,
};

fn checkedDataAuditPhase() Phase {
    return .{
        .label = "semantic audit",
        .action = .{ .argv = &.{ "perl", "ci/semantic_audit.pl" } },
        .failure_message = "Semantic audit failed. Run 'perl ci/semantic_audit.pl' to see details.",
        .abnormal_message = "ci/semantic_audit.pl terminated abnormally",
        .skip_windows_message = "Skipping semantic audit on Windows (perl unavailable)",
    };
}

/// Build step that runs the checked-data audit gate.
/// Skipped on Windows because perl is not preinstalled there; Linux/macOS CI
/// still enforces this gate.
pub const SemanticAuditStep = struct {
    step: Step,

    pub fn create(b: *std.Build) *SemanticAuditStep {
        const self = b.allocator.create(SemanticAuditStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "semantic-audit",
                .owner = b,
                .makeFn = make,
            }),
        };
        return self;
    }

    fn make(step: *Step, options: Step.MakeOptions) !void {
        try runPhaseAction(
            step,
            checkedDataAuditPhase(),
            .{ .progress_node = options.progress_node },
            false,
        );
    }
};

/// Custom build step that runs the MiniCI phase sequence with progress output.
pub const MiniCiStep = struct {
    step: Step,
    check_fx_platform_test_coverage: CheckStepFn,

    pub fn create(b: *std.Build, check_fx_platform_test_coverage: CheckStepFn) *MiniCiStep {
        const self = b.allocator.create(MiniCiStep) catch @panic("OOM");
        self.* = .{
            .step = Step.init(.{
                .id = Step.Id.custom,
                .name = "minici-inner",
                .owner = b,
                .makeFn = make,
            }),
            .check_fx_platform_test_coverage = check_fx_platform_test_coverage,
        };
        return self;
    }

    const StepTiming = struct {
        name: []const u8,
        ns: u64,
    };

    fn make(step: *Step, options: Step.MakeOptions) !void {
        const self: *MiniCiStep = @fieldParentPtr("step", step);
        const b = step.owner;
        const io = b.graph.io;
        const run_phases = [_]Phase{
            .{ .label = "zig build fmt", .action = .{ .zig_build = &.{"fmt"} } },
            .{
                .label = "zig lints",
                .action = .{ .zig_run = "ci/zig_lints.zig" },
                .failure_message = "Zig lints failed. Run 'zig run ci/zig_lints.zig' to see details.",
                .abnormal_message = "zig run ci/zig_lints.zig terminated abnormally",
            },
            .{
                .label = "tidy checks",
                .action = .{ .zig_run = "ci/tidy.zig" },
                .failure_message = "Tidy checks failed. Run 'zig run ci/tidy.zig' to see details.",
                .abnormal_message = "zig run ci/tidy.zig terminated abnormally",
            },
            checkedDataAuditPhase(),
            .{
                .label = "post-check architecture",
                .action = .{ .argv = &.{ "perl", "ci/check_postcheck_architecture.pl" } },
                .failure_message = "Post-check architecture check failed. Run 'perl ci/check_postcheck_architecture.pl' to see details.",
                .abnormal_message = "ci/check_postcheck_architecture.pl terminated abnormally",
                .skip_windows_message = "Skipping post-check architecture check on Windows (perl not available)",
            },
            .{
                .label = "test wiring",
                .action = .{ .zig_run = "ci/check_test_wiring.zig" },
                .failure_message = "Test wiring check failed. Run 'zig run ci/check_test_wiring.zig' to see details.",
                .abnormal_message = "zig run ci/check_test_wiring.zig terminated abnormally",
            },
            .{ .label = "zig build", .action = .{ .zig_build = &.{} } },
            .{
                .label = "Builtin.roc formatting",
                .action = .{ .argv = &.{ "./zig-out/bin/roc", "fmt", "--check", "src/build/roc/Builtin.roc" } },
                .failure_message = "src/build/roc/Builtin.roc is not formatted. Run 'zig build run -- fmt src/build/roc/Builtin.roc' to format it.",
                .abnormal_message = "roc fmt --check terminated abnormally",
            },
            .{ .label = "zig build snapshot", .action = .{ .zig_build = &.{"snapshot"} } },
            .{
                .label = "snapshot changes",
                .action = .{ .argv = &.{ "git", "diff", "--exit-code", "test/snapshots" } },
                .failure_message = "Snapshots in 'test/snapshots' have changed. Run 'zig build snapshot' locally, review the updates, and commit the changes.",
                .abnormal_message = "git diff terminated abnormally",
                .requires_git_worktree = true,
            },
            .{ .label = "fx platform test coverage", .action = .{ .callback = self.check_fx_platform_test_coverage } },
            .{ .label = "zig build test", .action = .{ .zig_build = &.{"test"} } },
            .{ .label = "zig build -Doptimize=ReleaseFast test-playground", .action = .{ .zig_build = &.{ "-Doptimize=ReleaseFast", "test-playground" } } },
            .{ .label = "zig build test-serialization-sizes", .action = .{ .zig_build = &.{"test-serialization-sizes"} } },
            .{ .label = "zig build test-cli", .action = .{ .zig_build = &.{"test-cli"} } },
            .{ .label = "zig build coverage", .action = .{ .zig_build = &.{"coverage"} } },
        };
        var timings = std.ArrayList(StepTiming).empty;
        defer timings.deinit(b.allocator);
        var wall_timer = std.Io.Timestamp.now(io, .awake);

        options.progress_node.setEstimatedTotalItems(run_phases.len);
        const heartbeat_interval_ms = try miniCiHeartbeatIntervalMs(step);

        for (run_phases, 0..) |phase, phase_idx| {
            const phase_number = phase_idx + 1;
            const phase_node = options.progress_node.start(phase.label, 0);
            defer phase_node.end();

            std.debug.print("minici [{d}/{d}] start: {s}\n", .{ phase_number, run_phases.len, phase.label });

            const phase_started = std.Io.Timestamp.now(io, .awake);
            const ctx = RunContext{
                .progress_node = phase_node,
                .heartbeat_interval_ms = heartbeat_interval_ms,
                .aggregate_label = "minici",
                .phase_position = .{ .index = phase_number, .total = run_phases.len },
            };

            runPhaseAction(step, phase, ctx, true) catch |err| {
                const elapsed_ns = @as(u64, @intCast(phase_started.untilNow(io, .awake).nanoseconds));
                std.debug.print("minici [{d}/{d}] failed: {s} ({d:.1}s)\n", .{
                    phase_number,
                    run_phases.len,
                    phase.label,
                    seconds(elapsed_ns),
                });
                return err;
            };

            try appendTiming(b.allocator, io, &timings, phase.label, phase_started);
            const elapsed_ns = timings.items[timings.items.len - 1].ns;
            std.debug.print("minici [{d}/{d}] done: {s} ({d:.1}s)\n", .{
                phase_number,
                run_phases.len,
                phase.label,
                seconds(elapsed_ns),
            });
        }

        printTimingSummary(timings.items, @as(u64, @intCast(wall_timer.untilNow(io, .awake).nanoseconds)));
    }

    fn appendTiming(
        allocator: std.mem.Allocator,
        io: std.Io,
        timings: *std.ArrayList(StepTiming),
        name: []const u8,
        started: std.Io.Timestamp,
    ) !void {
        try timings.append(allocator, .{ .name = name, .ns = @as(u64, @intCast(started.untilNow(io, .awake).nanoseconds)) });
    }

    fn printTimingSummary(timings: []const StepTiming, wall_ns: u64) void {
        std.debug.print("\n==== minici timing summary ====\n", .{});
        for (timings) |t| {
            std.debug.print("  {s:<48} {d:7.2}s\n", .{ t.name, seconds(t.ns) });
        }
        std.debug.print("  {s:<48} {s:->8}\n", .{ "", "" });
        std.debug.print("  {s:<48} {d:7.2}s\n", .{ "TOTAL", seconds(wall_ns) });
        std.debug.print("===============================\n", .{});
    }
};

const Heartbeat = struct {
    io: std.Io,
    aggregate_label: []const u8,
    phase_position: ?PhasePosition,
    label: []const u8,
    interval_ms: u64,
    started: std.Io.Timestamp,
    done: std.atomic.Value(bool),
    thread: ?std.Thread = null,

    fn init(io: std.Io, ctx: RunContext, label: []const u8) Heartbeat {
        return .{
            .io = io,
            .aggregate_label = ctx.aggregate_label,
            .phase_position = ctx.phase_position,
            .label = label,
            .interval_ms = ctx.heartbeat_interval_ms,
            .started = std.Io.Timestamp.now(io, .awake),
            .done = std.atomic.Value(bool).init(false),
        };
    }

    fn start(self: *Heartbeat) void {
        if (self.interval_ms == 0) return;
        self.thread = std.Thread.spawn(.{}, Heartbeat.run, .{self}) catch null;
    }

    fn stop(self: *Heartbeat) void {
        self.done.store(true, .release);
        if (self.thread) |thread| thread.join();
    }

    fn run(self: *Heartbeat) void {
        var last_report_ms: u64 = 0;
        while (!self.done.load(.acquire)) {
            std.Io.sleep(self.io, std.Io.Duration.fromMilliseconds(1_000), .awake) catch {};
            if (self.done.load(.acquire)) return;

            const elapsed_ns = @as(u64, @intCast(self.started.untilNow(self.io, .awake).nanoseconds));
            const elapsed_ms = elapsed_ns / std.time.ns_per_ms;
            if (elapsed_ms - last_report_ms < self.interval_ms) continue;
            last_report_ms = elapsed_ms;

            if (self.phase_position) |pos| {
                std.debug.print("{s} [{d}/{d}] still running: {s} (elapsed {d:.1}s)\n", .{
                    self.aggregate_label,
                    pos.index,
                    pos.total,
                    self.label,
                    seconds(elapsed_ns),
                });
            } else {
                std.debug.print("{s} still running: {s} (elapsed {d:.1}s)\n", .{
                    self.aggregate_label,
                    self.label,
                    seconds(elapsed_ns),
                });
            }
        }
    }
};

fn runPhaseAction(step: *Step, phase: Phase, ctx: RunContext, inject_test_progress: bool) !void {
    if (try shouldSkipPhase(step, phase)) return;

    switch (phase.action) {
        .callback => |check| try check(step),
        else => {
            const b = step.owner;
            var child_argv = std.ArrayList([]const u8).empty;
            defer child_argv.deinit(b.allocator);

            try appendPhaseArgv(b, &child_argv, phase.action);

            const abnormal_message = phase.abnormal_message orelse b.fmt("`{s}` terminated abnormally", .{phase.label});
            try spawnAndWait(
                step,
                child_argv.items,
                ctx,
                phase.label,
                phase.failure_message,
                abnormal_message,
                inject_test_progress,
            );
        },
    }
}

fn shouldSkipPhase(step: *Step, phase: Phase) !bool {
    const b = step.owner;

    if (phase.skip_windows_message) |message| {
        if (builtin.os.tag == .windows) {
            std.debug.print("{s}\n", .{message});
            return true;
        }
    }

    if (phase.requires_git_worktree) {
        std.Io.Dir.cwd().access(b.graph.io, ".git", .{}) catch |err| switch (err) {
            error.FileNotFound => {
                std.debug.print("Skipping {s} outside a Git worktree.\n", .{phase.label});
                return true;
            },
            else => return err,
        };
    }

    return false;
}

fn appendPhaseArgv(
    b: *std.Build,
    child_argv: *std.ArrayList([]const u8),
    action: PhaseAction,
) !void {
    switch (action) {
        .argv => |args| try appendArgs(b, child_argv, args),
        .zig_run => |script| {
            try child_argv.append(b.allocator, b.graph.zig_exe);
            try child_argv.append(b.allocator, "run");
            try child_argv.append(b.allocator, script);
            try appendZigCacheArgs(b, child_argv);
        },
        .zig_build => |args| {
            try child_argv.append(b.allocator, b.graph.zig_exe);
            try child_argv.append(b.allocator, "build");
            try appendZigCacheArgs(b, child_argv);
            try appendArgs(b, child_argv, args);
        },
        .callback => unreachable,
    }
}

fn appendArgs(
    b: *std.Build,
    child_argv: *std.ArrayList([]const u8),
    args: []const []const u8,
) !void {
    for (args) |arg| {
        try child_argv.append(b.allocator, arg);
    }
}

fn spawnAndWait(
    step: *Step,
    argv: []const []const u8,
    ctx: RunContext,
    display: []const u8,
    failure_message: ?[]const u8,
    abnormal_message: []const u8,
    inject_test_progress: bool,
) !void {
    const b = step.owner;
    const io = b.graph.io;

    var child_env_storage: ?std.process.Environ.Map = null;
    defer if (child_env_storage) |*child_env| child_env.deinit();

    var env_map: *const std.process.Environ.Map = &b.graph.environ_map;
    if (inject_test_progress) {
        child_env_storage = try b.graph.environ_map.clone(b.allocator);
        if (child_env_storage) |*child_env| {
            try addCiChildEnv(child_env);
            env_map = child_env;
        }
    }

    var child = try std.process.spawn(io, .{
        .argv = argv,
        .environ_map = env_map,
        .progress_node = ctx.progress_node,
    });
    var heartbeat = Heartbeat.init(io, ctx, display);
    heartbeat.start();
    defer heartbeat.stop();

    const term = try child.wait(io);

    switch (term) {
        .exited => |code| {
            if (code != 0) {
                if (failure_message) |message| {
                    return step.fail("{s}", .{message});
                }
                return step.fail("`{s}` failed with exit code {d}", .{ display, code });
            }
        },
        else => return step.fail("{s}", .{abnormal_message}),
    }
}

fn addCiChildEnv(env_map: *std.process.Environ.Map) !void {
    if (!env_map.contains(non_tty_test_progress_env)) {
        try env_map.put(non_tty_test_progress_env, default_non_tty_test_progress_ms);
    }
}

fn miniCiHeartbeatIntervalMs(step: *Step) !u64 {
    const b = step.owner;
    if (b.graph.environ_map.get(mini_ci_heartbeat_env)) |raw| {
        return std.fmt.parseInt(u64, raw, 10) catch |err| {
            return step.fail("Invalid {s} value '{s}': {s}", .{ mini_ci_heartbeat_env, raw, @errorName(err) });
        };
    }

    const stderr = std.Io.File.stderr();
    const is_tty = stderr.isTty(b.graph.io) catch false;
    return if (is_tty) 0 else default_non_tty_heartbeat_ms;
}

fn appendZigCacheArgs(b: *std.Build, child_argv: *std.ArrayList([]const u8)) !void {
    try child_argv.append(b.allocator, "--cache-dir");
    try child_argv.append(b.allocator, b.cache_root.path orelse ".");
    try child_argv.append(b.allocator, "--global-cache-dir");
    try child_argv.append(b.allocator, b.graph.global_cache_root.path orelse ".");
}

fn seconds(ns: u64) f64 {
    return @as(f64, @floatFromInt(ns)) / 1_000_000_000.0;
}
