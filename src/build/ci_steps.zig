//! Build-system helpers for CI check steps.

const std = @import("std");
const builtin = @import("builtin");

const Step = std.Build.Step;

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
        );
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

fn runPhaseAction(step: *Step, phase: Phase, ctx: RunContext) !void {
    if (try shouldSkipPhase(step, phase)) return;

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
    );
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
) !void {
    const b = step.owner;
    const io = b.graph.io;

    var child = try std.process.spawn(io, .{
        .argv = argv,
        .environ_map = &b.graph.environ_map,
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

fn appendZigCacheArgs(b: *std.Build, child_argv: *std.ArrayList([]const u8)) !void {
    try child_argv.append(b.allocator, "--cache-dir");
    try child_argv.append(b.allocator, b.cache_root.path orelse ".");
    try child_argv.append(b.allocator, "--global-cache-dir");
    try child_argv.append(b.allocator, b.graph.global_cache_root.path orelse ".");
}

fn seconds(ns: u64) f64 {
    return @as(f64, @floatFromInt(ns)) / 1_000_000_000.0;
}
