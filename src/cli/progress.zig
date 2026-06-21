//! Live, phase-by-phase progress reporting for long-running CLI operations
//! (`roc build`, `roc check`, and the default `roc` command).
//!
//! The reporter records the wall-clock duration of each compilation phase as it
//! runs. When an operation takes longer than a threshold (or `--timings` was
//! passed) it draws an animated breakdown to the terminal: a spinner and a
//! live-updating elapsed counter next to the phase currently running, with the
//! final duration shown next to each phase that has already finished. Once the
//! operation completes the spinner is replaced by the last phase's final time,
//! leaving a static retrospective of where the time went.
//!
//! The animation is driven by a background thread so the counter keeps ticking
//! even while the main thread is blocked inside a synchronous phase (LLVM code
//! generation being the usual culprit). On non-terminal output, or on targets
//! without thread support, no animation is drawn; instead the full breakdown is
//! printed once at the end when it is requested or warranted.

const std = @import("std");
const builtin = @import("builtin");
const ansi = @import("ansi_term.zig");

/// Whether the current target can spawn the background animation thread.
const supports_threads = builtin.target.cpu.arch != .wasm32 and !builtin.single_threaded;

/// `std.Thread` on threaded targets; `void` where threads are unavailable so the
/// type never gets semantically analyzed (e.g. the wasm playground build).
const ThreadHandle = if (supports_threads) std.Thread else void;

/// How often the spinner/counter is redrawn while a phase is running.
const tick_ns: u64 = 125 * std.time.ns_per_ms;

/// Operations slower than this show their breakdown even without `--timings`.
const default_threshold_ns: u64 = std.time.ns_per_s;

/// Column the phase durations are aligned to (phase names are padded to this).
const name_width: usize = 24;

/// Maximum number of top-level phases a single operation reports.
const max_phases: usize = 16;

const spinner_frames = [_][]const u8{
    "\u{280B}", "\u{2819}", "\u{2839}", "\u{2838}", "\u{283C}",
    "\u{2834}", "\u{2826}", "\u{2827}", "\u{2807}", "\u{280F}",
};

/// A named sub-measurement shown beneath a phase (e.g. the front-end's
/// Parsing / Name Resolution / Type Inference split).
pub const SubTiming = struct {
    name: []const u8,
    ns: u64,
};

const Phase = struct {
    name: []const u8,
    start_ns: u64,
    end_ns: ?u64 = null,
    /// When set, the phase renders as these rows instead of a single line.
    sub: [4]SubTiming = undefined,
    sub_len: u8 = 0,
};

/// Configuration for a `Reporter`.
pub const Config = struct {
    std_io: std.Io,
    /// Stream the breakdown is drawn to (typically stderr).
    writer: *std.Io.Writer,
    /// Label shown above the phases, e.g. "roc build".
    op_label: []const u8,
    /// True when `--timings` was passed: always show the breakdown.
    timings_flag: bool,
    /// True when `writer` is connected to a terminal.
    is_tty: bool,
};

/// Tracks and displays the timing of an operation's compilation phases.
pub const Reporter = struct {
    std_io: std.Io,
    writer: *std.Io.Writer,
    op_label: []const u8,
    always: bool,
    animate: bool,
    is_tty: bool,
    threshold_ns: u64,
    start_ts: std.Io.Timestamp,

    mutex: std.Io.Mutex = .init,
    phases: [max_phases]Phase = undefined,
    phase_count: usize = 0,
    active: ?usize = null,
    displaying: bool = false,
    pending_partial: bool = false,
    spin: usize = 0,
    thread: ?ThreadHandle = null,
    stop: bool = false,
    finished: bool = false,

    /// Initialize a reporter and begin the wall clock. Call `deinit` when done.
    /// `start` must be called to begin the animation thread.
    pub fn init(config: Config) Reporter {
        const animate = supports_threads and config.is_tty;
        return .{
            .std_io = config.std_io,
            .writer = config.writer,
            .op_label = config.op_label,
            .always = config.timings_flag,
            .animate = animate,
            .is_tty = config.is_tty,
            .threshold_ns = default_threshold_ns,
            .start_ts = std.Io.Timestamp.now(config.std_io, .awake),
        };
    }

    /// Spawn the background animation thread (no-op when not animating).
    pub fn start(self: *Reporter) void {
        if (comptime supports_threads) {
            if (!self.animate) return;
            self.thread = std.Thread.spawn(.{}, bgLoop, .{self}) catch null;
        }
    }

    /// Mark the beginning of a named phase. The previously active phase, if any,
    /// must have been ended first.
    pub fn begin(self: *Reporter, name: []const u8) void {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);
        if (self.finished or self.phase_count >= max_phases) return;
        const idx = self.phase_count;
        self.phases[idx] = .{ .name = name, .start_ns = self.elapsedNs() };
        self.active = idx;
        self.phase_count += 1;
        if (self.displaying) self.drawActiveLine();
    }

    /// End the currently active phase, recording its duration.
    pub fn end(self: *Reporter) void {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);
        self.endActiveLocked(&.{});
    }

    /// End the active phase and record a sub-timing breakdown to display in its
    /// place (e.g. splitting "Type Checking" into its constituent phases).
    pub fn endWithBreakdown(self: *Reporter, subs: []const SubTiming) void {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);
        self.endActiveLocked(subs);
    }

    fn endActiveLocked(self: *Reporter, subs: []const SubTiming) void {
        const idx = self.active orelse return;
        self.phases[idx].end_ns = self.elapsedNs();
        const n = @min(subs.len, self.phases[idx].sub.len);
        for (subs[0..n], 0..) |s, i| self.phases[idx].sub[i] = s;
        self.phases[idx].sub_len = @intCast(n);
        if (self.displaying) self.commitPhaseInPlace(idx);
        self.active = null;
    }

    /// Stop the animation and print the final breakdown if one is warranted but
    /// was never shown live. With `--timings` the breakdown always prints, even to
    /// a non-terminal. Without it, the threshold-triggered breakdown is a terminal
    /// decoration only: a non-interactive caller (pipe, CI, test harness capturing
    /// stderr) must opt in with `--timings`, so captured output stays clean.
    pub fn finish(self: *Reporter) void {
        self.stopThread();
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);
        if (self.finished) return;
        self.finished = true;
        const threshold_reached = self.is_tty and self.elapsedNs() >= self.threshold_ns;
        if (!self.displaying and (self.always or threshold_reached)) {
            self.printStaticBreakdown();
        }
        self.writer.flush() catch {};
    }

    /// Abort the display without printing a final breakdown, clearing any
    /// in-progress line so subsequent output (e.g. diagnostics) starts clean.
    pub fn fail(self: *Reporter) void {
        self.stopThread();
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);
        if (self.finished) return;
        self.finished = true;
        if (self.displaying and self.pending_partial) {
            self.clearLine();
            self.pending_partial = false;
            self.writer.flush() catch {};
        }
    }

    /// Stop the background thread and release the reporter. Idempotent.
    pub fn deinit(self: *Reporter) void {
        self.stopThread();
    }

    fn stopThread(self: *Reporter) void {
        if (comptime !supports_threads) return;
        self.mutex.lockUncancelable(self.std_io);
        self.stop = true;
        self.mutex.unlock(self.std_io);
        if (self.thread) |t| {
            t.join();
            self.thread = null;
        }
    }

    fn bgLoop(self: *Reporter) void {
        while (true) {
            self.std_io.sleep(std.Io.Duration.fromNanoseconds(tick_ns), .awake) catch {};
            self.mutex.lockUncancelable(self.std_io);
            if (self.stop) {
                self.mutex.unlock(self.std_io);
                break;
            }
            self.tick();
            self.mutex.unlock(self.std_io);
        }
    }

    /// One animation frame. Caller holds the mutex.
    fn tick(self: *Reporter) void {
        if (!self.displaying) {
            if (self.always or self.elapsedNs() >= self.threshold_ns) {
                self.startDisplay();
            }
            return;
        }
        if (self.active != null) {
            self.spin += 1;
            self.drawActiveLine();
        }
    }

    /// Transition into live display: print the header and a catch-up snapshot of
    /// every phase recorded so far. Caller holds the mutex.
    fn startDisplay(self: *Reporter) void {
        self.displaying = true;
        self.writer.print("{s}\n", .{self.op_label}) catch {};
        var i: usize = 0;
        while (i < self.phase_count) : (i += 1) {
            if (self.active == i and self.phases[i].end_ns == null) {
                self.drawActiveLine();
            } else {
                self.writeCommittedPhase(i);
            }
        }
        self.writer.flush() catch {};
    }

    /// Print the whole breakdown at once (no animation). Caller holds the mutex.
    fn printStaticBreakdown(self: *Reporter) void {
        self.writer.print("{s}\n", .{self.op_label}) catch {};
        var i: usize = 0;
        while (i < self.phase_count) : (i += 1) self.writeCommittedPhase(i);
    }

    /// Redraw the active phase's line in place with the spinner and live counter.
    /// Caller holds the mutex.
    fn drawActiveLine(self: *Reporter) void {
        const idx = self.active orelse return;
        const p = self.phases[idx];
        self.clearLine();
        const frame = spinner_frames[self.spin % spinner_frames.len];
        var buf: [32]u8 = undefined;
        const elapsed = self.elapsedNs() - p.start_ns;
        const dur = formatDuration(&buf, elapsed, .live);
        self.writer.print("  {s} {f}{s}\n", .{ frame, padName(p.name), dur }) catch {};
        // Park the cursor back on the line so the next frame overwrites it.
        self.writer.print("\x1B[1A", .{}) catch {};
        self.pending_partial = true;
        self.writer.flush() catch {};
    }

    /// Replace the active line with the phase's final, committed rows.
    /// Caller holds the mutex.
    fn commitPhaseInPlace(self: *Reporter, idx: usize) void {
        self.clearLine();
        self.pending_partial = false;
        self.writeCommittedPhase(idx);
        self.writer.flush() catch {};
    }

    /// Write a finished phase as one or more committed rows. Caller holds mutex.
    fn writeCommittedPhase(self: *Reporter, idx: usize) void {
        const p = self.phases[idx];
        const check = if (self.is_tty) ansi.green ++ "\u{2713}" ++ ansi.reset else "\u{2713}";
        if (p.sub_len > 0) {
            for (p.sub[0..p.sub_len]) |s| {
                var buf: [32]u8 = undefined;
                const dur = formatDuration(&buf, s.ns, .final);
                self.writer.print("  {s} {f}{s}\n", .{ check, padName(s.name), dur }) catch {};
            }
            return;
        }
        const total = (p.end_ns orelse self.elapsedNs()) - p.start_ns;
        var buf: [32]u8 = undefined;
        const dur = formatDuration(&buf, total, .final);
        self.writer.print("  {s} {f}{s}\n", .{ check, padName(p.name), dur }) catch {};
    }

    /// Return to the start of the current line and clear it. Caller holds mutex.
    fn clearLine(self: *Reporter) void {
        self.writer.writeAll("\r") catch {};
        ansi.clearFromCursorToLineEnd(self.writer) catch {};
    }

    fn elapsedNs(self: *Reporter) u64 {
        const now = std.Io.Timestamp.now(self.std_io, .awake);
        const delta = now.nanoseconds - self.start_ts.nanoseconds;
        return if (delta > 0) @intCast(delta) else 0;
    }
};

/// Pad a phase name (ASCII) to the duration-alignment column.
fn padName(name: []const u8) PaddedName {
    return .{ .name = name };
}

const PaddedName = struct {
    name: []const u8,

    pub fn format(self: PaddedName, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(self.name);
        if (self.name.len < name_width) {
            try writer.splatByteAll(' ', name_width - self.name.len);
        }
    }
};

/// Write a human-friendly duration (e.g. "850ms", "1.2s", "2m 5s") to `writer`.
pub fn writeDuration(writer: *std.Io.Writer, ns: u64) std.Io.Writer.Error!void {
    var buf: [32]u8 = undefined;
    try writer.writeAll(formatDuration(&buf, ns, .final));
}

const DurationStyle = enum {
    /// Whole-second granularity, for the live ticking counter.
    live,
    /// Sub-second precision, for finished phases.
    final,
};

/// Format a nanosecond duration into `buf`, returning the written slice.
fn formatDuration(buf: []u8, ns: u64, style: DurationStyle) []const u8 {
    const total_secs = ns / std.time.ns_per_s;
    if (total_secs >= 3600) {
        const h = total_secs / 3600;
        const m = (total_secs % 3600) / 60;
        return if (m == 0)
            std.fmt.bufPrint(buf, "{d}h", .{h}) catch buf[0..0]
        else
            std.fmt.bufPrint(buf, "{d}h {d}m", .{ h, m }) catch buf[0..0];
    }
    if (total_secs >= 60) {
        const m = total_secs / 60;
        const s = total_secs % 60;
        return if (s == 0)
            std.fmt.bufPrint(buf, "{d}m", .{m}) catch buf[0..0]
        else
            std.fmt.bufPrint(buf, "{d}m {d}s", .{ m, s }) catch buf[0..0];
    }
    switch (style) {
        .live => return std.fmt.bufPrint(buf, "{d}s", .{total_secs}) catch buf[0..0],
        .final => {
            if (ns < std.time.ns_per_s) {
                const ms = (ns + 500_000) / std.time.ns_per_ms;
                return std.fmt.bufPrint(buf, "{d}ms", .{ms}) catch buf[0..0];
            }
            const secs_f = @as(f64, @floatFromInt(ns)) / @as(f64, std.time.ns_per_s);
            return std.fmt.bufPrint(buf, "{d:.1}s", .{secs_f}) catch buf[0..0];
        },
    }
}

const testing = std.testing;

test "formatDuration live: whole seconds" {
    var buf: [32]u8 = undefined;
    try testing.expectEqualStrings("0s", formatDuration(&buf, 400 * std.time.ns_per_ms, .live));
    try testing.expectEqualStrings("12s", formatDuration(&buf, 12 * std.time.ns_per_s, .live));
    try testing.expectEqualStrings("59s", formatDuration(&buf, 59 * std.time.ns_per_s, .live));
}

test "formatDuration live: minutes and hours" {
    var buf: [32]u8 = undefined;
    try testing.expectEqualStrings("1m", formatDuration(&buf, 60 * std.time.ns_per_s, .live));
    try testing.expectEqualStrings("1m 1s", formatDuration(&buf, 61 * std.time.ns_per_s, .live));
    try testing.expectEqualStrings("2m 30s", formatDuration(&buf, 150 * std.time.ns_per_s, .live));
    try testing.expectEqualStrings("1h", formatDuration(&buf, 3600 * std.time.ns_per_s, .live));
    try testing.expectEqualStrings("1h 2m", formatDuration(&buf, (3600 + 120) * std.time.ns_per_s, .live));
}

test "formatDuration final: ms and seconds" {
    var buf: [32]u8 = undefined;
    try testing.expectEqualStrings("0ms", formatDuration(&buf, 100_000, .final));
    try testing.expectEqualStrings("12ms", formatDuration(&buf, 12 * std.time.ns_per_ms, .final));
    try testing.expectEqualStrings("999ms", formatDuration(&buf, 999 * std.time.ns_per_ms, .final));
    try testing.expectEqualStrings("1.2s", formatDuration(&buf, 1200 * std.time.ns_per_ms, .final));
    try testing.expectEqualStrings("1m 5s", formatDuration(&buf, 65 * std.time.ns_per_s, .final));
}

test "padName pads short names and leaves long names" {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    try aw.writer.print("[{f}]", .{padName("Parsing")});
    try testing.expectEqualStrings("[Parsing                 ]", aw.written());
}

fn collectStatic(buf: *std.Io.Writer.Allocating, timings_flag: bool) void {
    var reporter = Reporter.init(.{
        .std_io = std.Io.Threaded.global_single_threaded.io(),
        .writer = &buf.writer,
        .op_label = "roc build",
        .timings_flag = timings_flag,
        .is_tty = false, // force the non-animated, print-at-end path
    });
    defer reporter.deinit();
    reporter.start();

    reporter.begin("Resolving Dependencies");
    reporter.end();
    reporter.begin("Type Checking");
    reporter.endWithBreakdown(&.{
        .{ .name = "Parsing", .ns = 10 * std.time.ns_per_ms },
        .{ .name = "Name Resolution", .ns = 20 * std.time.ns_per_ms },
        .{ .name = "Type Inference", .ns = 30 * std.time.ns_per_ms },
    });
    reporter.begin("Code Generation");
    reporter.end();
    reporter.finish();
}

test "static breakdown lists every phase with the timings flag" {
    var buf: std.Io.Writer.Allocating = .init(testing.allocator);
    defer buf.deinit();
    collectStatic(&buf, true);

    const out = buf.written();
    try testing.expect(std.mem.find(u8, out, "roc build\n") != null);
    try testing.expect(std.mem.find(u8, out, "Resolving Dependencies") != null);
    try testing.expect(std.mem.find(u8, out, "Parsing") != null);
    try testing.expect(std.mem.find(u8, out, "Name Resolution") != null);
    try testing.expect(std.mem.find(u8, out, "Type Inference") != null);
    try testing.expect(std.mem.find(u8, out, "Code Generation") != null);
    // Type Checking is replaced by its breakdown, not shown directly.
    try testing.expect(std.mem.find(u8, out, "Type Checking") == null);
}

test "fast run without the timings flag prints nothing" {
    var buf: std.Io.Writer.Allocating = .init(testing.allocator);
    defer buf.deinit();
    collectStatic(&buf, false);
    try testing.expectEqualStrings("", buf.written());
}

test "slow non-terminal run without the timings flag prints nothing" {
    // A non-interactive caller (pipe/CI/test harness) must not get the
    // threshold-triggered breakdown on stderr; only `--timings` opts in.
    var buf: std.Io.Writer.Allocating = .init(testing.allocator);
    defer buf.deinit();
    var reporter = Reporter.init(.{
        .std_io = std.Io.Threaded.global_single_threaded.io(),
        .writer = &buf.writer,
        .op_label = "roc build",
        .timings_flag = false,
        .is_tty = false,
    });
    defer reporter.deinit();
    reporter.threshold_ns = 0; // force the threshold to be considered reached
    reporter.start();
    reporter.begin("Type Checking");
    reporter.end();
    reporter.finish();
    try testing.expectEqualStrings("", buf.written());
}
