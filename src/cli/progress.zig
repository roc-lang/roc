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
const base = @import("base");
const ansi = @import("ansi_term.zig");

/// Whether the current target can spawn the background animation thread.
const supports_threads = builtin.target.cpu.arch != .wasm32 and !builtin.single_threaded;

/// `std.Thread` on threaded targets; `void` where threads are unavailable so the
/// type never gets semantically analyzed (e.g. the wasm playground build).
const ThreadHandle = if (supports_threads) std.Thread else void;

/// How often the spinner/counter is redrawn while a phase is running.
const tick_ns: u64 = 125 * std.time.ns_per_ms;

/// How often memory is sampled when `--timings` is active. Sampling is paid
/// only in that mode; the animation-only thread keeps the slower redraw tick.
const mem_tick_ns: u64 = 10 * std.time.ns_per_ms;

/// Operations slower than this show their breakdown even without `--timings`.
const default_threshold_ns: u64 = std.time.ns_per_s;

/// Minimum width of the phase-name column. A separator is always emitted after
/// the padded name so a future longer label cannot run into its duration.
const name_width: usize = 37;

/// Maximum number of top-level phases a single operation reports.
const max_phases: usize = 16;
const max_subphases: usize = 24;
// Test-cache diagnostics plus the post-check workload groups already require
// four entries; retain headroom so later explicit diagnostics are not silently
// dropped merely because their recording order changes.
const max_counter_groups: usize = 8;
const max_counters_per_group: usize = 24;

/// Wide enough for at least seven digits, their grouping underscores, and the ms suffix.
/// This accommodates durations up to tens of minutes (5_999_000ms is just under 100 minutes).
const min_completed_duration_width: usize = "5_999_000ms".len;

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

/// A deterministic operation count shown only for explicit diagnostic output.
pub const Counter = struct {
    name: []const u8,
    count: u64,
};

const CounterGroup = struct {
    name: []const u8,
    counters: [max_counters_per_group]Counter = undefined,
    len: u8 = 0,
};

const Phase = struct {
    name: []const u8,
    start_ns: u64,
    end_ns: ?u64 = null,
    /// When set, the phase renders as these rows instead of a single line.
    sub: [max_subphases]SubTiming = undefined,
    sub_len: u8 = 0,
    show_parent_with_subs: bool = false,
    /// Smallest and largest process footprint sampled while this phase was
    /// active. `mem_max == 0` means no sample landed (sampling off, or the
    /// phase was shorter than the sampling tick and the boundary reads
    /// failed), and the row prints without a memory column.
    mem_min: u64 = std.math.maxInt(u64),
    mem_max: u64 = 0,
    /// Per-sub memory ranges, filled only for sequential breakdowns by
    /// slicing the sample buffer over each sub's reconstructed window.
    sub_mem: [max_subphases]Reporter.MemRange = @splat(.{}),
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
    /// Timestamped footprint samples for slicing sequential breakdowns.
    /// When full, every other sample is dropped and the stride doubles, so
    /// resolution degrades gracefully on long operations.
    samples: [4096]MemSample = undefined,
    sample_len: u16 = 0,
    sample_stride_ns: u64 = mem_tick_ns,
    last_sample_ns: u64 = 0,
    /// Largest footprint sampled over the whole operation.
    peak_bytes: u64 = 0,
    phases: [max_phases]Phase = undefined,
    phase_count: usize = 0,
    counter_groups: [max_counter_groups]CounterGroup = undefined,
    counter_group_count: usize = 0,
    active: ?usize = null,
    displaying: bool = false,
    pending_partial: bool = false,
    spin: usize = 0,
    thread: ?ThreadHandle = null,
    stop: bool = false,
    /// Signaled when the reporter is done, so the background thread's timed
    /// wait returns at once instead of running its tick out.
    stop_event: std.Io.Event = .unset,
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

    /// Spawn the background thread. It animates when drawing to a terminal
    /// and samples memory when `--timings` is active; with neither there is
    /// nothing to do and no thread is spawned.
    pub fn start(self: *Reporter) void {
        if (comptime supports_threads) {
            if (!self.animate and !self.always) return;
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
        self.sampleMemoryLocked();
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
    /// The sub durations are per-category aggregates over interleaved work,
    /// so no per-sub memory range can be attributed.
    pub fn endWithBreakdown(self: *Reporter, subs: []const SubTiming) void {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);
        self.endActiveLocked(subs);
    }

    /// End the active phase with a breakdown whose subs ran once each, in
    /// order. Each sub's window is reconstructed from the cumulative
    /// durations and sliced from the sample buffer for a per-sub memory
    /// range.
    pub fn endWithBreakdownSequential(self: *Reporter, subs: []const SubTiming) void {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);
        const idx = self.active orelse return;
        self.sampleMemoryLocked();
        if (self.always) {
            var cursor = self.phases[idx].start_ns;
            const n = @min(subs.len, self.phases[idx].sub_mem.len);
            for (subs[0..n], 0..) |sub, i| {
                self.phases[idx].sub_mem[i] = self.sampleRangeInWindow(cursor, cursor + sub.ns);
                cursor += sub.ns;
            }
        }
        self.endActiveLocked(subs);
    }

    /// A memory range observed by the producer of an externally timed phase.
    /// The reporter cannot window-sample work that runs interleaved inside
    /// another phase, so the producer supplies its own boundary readings.
    pub const MemRange = struct {
        min: u64 = std.math.maxInt(u64),
        max: u64 = 0,
    };

    const MemSample = struct {
        at_ns: u64,
        bytes: u64,
    };

    /// Append a phase that completed inside another synchronous operation.
    /// The parent row shows `duration_ns`, followed by indented sub-timings.
    pub fn recordCompletedWithBreakdown(
        self: *Reporter,
        name: []const u8,
        duration_ns: u64,
        mem: MemRange,
        subs: []const SubTiming,
    ) void {
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);
        if (self.finished or self.phase_count >= max_phases) return;

        const idx = self.phase_count;
        self.phases[idx] = .{
            .name = name,
            .start_ns = 0,
            .end_ns = duration_ns,
            .show_parent_with_subs = true,
            .mem_min = mem.min,
            .mem_max = if (self.always) mem.max else 0,
        };
        const n = @min(subs.len, self.phases[idx].sub.len);
        for (subs[0..n], 0..) |sub, i| self.phases[idx].sub[i] = sub;
        self.phases[idx].sub_len = @intCast(n);
        self.phase_count += 1;
        if (self.displaying) self.writeCommittedPhase(idx);
    }

    /// Record deterministic counters to print after the timing phases. Counter
    /// diagnostics are explicit `--timings` output and never appear merely
    /// because an interactive operation crossed the slow-operation threshold.
    pub fn recordCounters(self: *Reporter, name: []const u8, counters: []const Counter) void {
        if (!self.always) return;
        self.mutex.lockUncancelable(self.std_io);
        defer self.mutex.unlock(self.std_io);
        if (self.finished or self.counter_group_count >= self.counter_groups.len) return;

        const group = &self.counter_groups[self.counter_group_count];
        group.* = .{ .name = name };
        const len = @min(counters.len, group.counters.len);
        @memcpy(group.counters[0..len], counters[0..len]);
        group.len = @intCast(len);
        self.counter_group_count += 1;
    }

    fn endActiveLocked(self: *Reporter, subs: []const SubTiming) void {
        const idx = self.active orelse return;
        self.sampleMemoryLocked();
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
        } else if (self.displaying) {
            var totals_buf: [64]u8 = undefined;
            self.writer.print("{f}{s}\n", .{ padName(self.op_label), self.formatTotals(&totals_buf) }) catch {};
        }
        if (self.always) self.writeCounterGroups();
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
        const thread = self.thread orelse return;
        self.mutex.lockUncancelable(self.std_io);
        self.stop = true;
        self.mutex.unlock(self.std_io);
        // Waking the thread is what bounds this join: the flag alone is
        // invisible to a thread that is partway through a wait interval.
        self.stop_event.set(self.std_io);
        thread.join();
        self.thread = null;
    }

    /// Wait up to `ns` for the reporter to finish. Returns true when the wait
    /// ended because the reporter is finishing rather than because the
    /// interval elapsed.
    fn waitForStop(self: *Reporter, ns: u64) bool {
        self.stop_event.waitTimeout(self.std_io, .{ .duration = .{
            .raw = .fromNanoseconds(ns),
            .clock = .awake,
        } }) catch |err| switch (err) {
            // A spurious wakeup lands here too; the loop below re-checks the
            // stop flag and the draw threshold, so an early tick is harmless.
            error.Timeout => return false,
            error.Canceled => return true,
        };
        return true;
    }

    fn bgLoop(self: *Reporter) void {
        const interval_ns = if (self.always) mem_tick_ns else tick_ns;
        const draws_every: u64 = if (self.always) tick_ns / mem_tick_ns else 1;
        // An animation-only reporter draws nothing until the operation crosses
        // the breakdown threshold, so the first wait runs all the way to it and
        // only then does the redraw cadence start. `--timings` samples memory
        // from the outset and keeps its cadence throughout.
        var wait_ns: u64 = if (self.always) interval_ns else @max(self.threshold_ns, interval_ns);
        var wakeups: u64 = 0;
        while (true) {
            if (self.waitForStop(wait_ns)) break;
            wait_ns = interval_ns;
            self.mutex.lockUncancelable(self.std_io);
            if (self.stop) {
                self.mutex.unlock(self.std_io);
                break;
            }
            if (self.always) self.sampleMemoryLocked();
            wakeups += 1;
            if (self.animate and wakeups % draws_every == 0) self.tick();
            self.mutex.unlock(self.std_io);
        }
    }

    /// Fold the current process footprint into the active phase's range and
    /// the sample buffer. Caller holds the mutex. Only `--timings` runs pay
    /// for the read.
    fn sampleMemoryLocked(self: *Reporter) void {
        if (!self.always) return;
        const idx = self.active orelse return;
        const bytes = base.process_memory.currentBytes() orelse return;
        const p = &self.phases[idx];
        if (bytes < p.mem_min) p.mem_min = bytes;
        if (bytes > p.mem_max) p.mem_max = bytes;
        if (bytes > self.peak_bytes) self.peak_bytes = bytes;

        const now = self.elapsedNs();
        if (self.sample_len > 0 and now - self.last_sample_ns < self.sample_stride_ns) return;
        if (self.sample_len == self.samples.len) {
            var write: u16 = 0;
            var read: u16 = 0;
            while (read < self.sample_len) : (read += 2) {
                self.samples[write] = self.samples[read];
                write += 1;
            }
            self.sample_len = write;
            self.sample_stride_ns *= 2;
        }
        self.samples[self.sample_len] = .{ .at_ns = now, .bytes = bytes };
        self.sample_len += 1;
        self.last_sample_ns = now;
    }

    /// Smallest and largest buffered sample in `[from_ns, to_ns]`.
    fn sampleRangeInWindow(self: *const Reporter, from_ns: u64, to_ns: u64) MemRange {
        var range = MemRange{};
        for (self.samples[0..self.sample_len]) |sample| {
            if (sample.at_ns < from_ns or sample.at_ns > to_ns) continue;
            if (sample.bytes < range.min) range.min = sample.bytes;
            if (sample.bytes > range.max) range.max = sample.bytes;
        }
        return range;
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
        var totals_buf: [64]u8 = undefined;
        self.writer.print("{f}{s}\n", .{ padName(self.op_label), self.formatTotals(&totals_buf) }) catch {};
        var i: usize = 0;
        while (i < self.phase_count) : (i += 1) self.writeCommittedPhase(i);
    }

    fn writeCounterGroups(self: *Reporter) void {
        for (self.counter_groups[0..self.counter_group_count]) |group| {
            self.writer.print("  {s}\n", .{group.name}) catch {};
            for (group.counters[0..group.len]) |counter| {
                self.writer.print("      {f} {d}\n", .{ padChildName(counter.name), counter.count }) catch {};
            }
        }
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
        self.writer.print("  {s} {f} {s}\n", .{ frame, padName(p.name), dur }) catch {};
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
        var mem_buf: [48]u8 = undefined;
        const mem = formatMemRange(&mem_buf, p.mem_min, p.mem_max);
        if (p.sub_len > 0) {
            // A parent with a sampled memory range always gets its own row:
            // sub-timings are per-category aggregates over interleaved work,
            // so the range is only truthful on the parent's contiguous window.
            const show_parent = p.show_parent_with_subs or mem.len > 0;
            if (show_parent) {
                var parent_buf: [32]u8 = undefined;
                const total = (p.end_ns orelse self.elapsedNs()) - p.start_ns;
                const parent_dur = formatCompletedRowDuration(&parent_buf, total);
                self.writer.print("  {s} {f} {s}{s}\n", .{ check, padName(p.name), parent_dur, mem }) catch {};
            }
            for (p.sub[0..p.sub_len], 0..) |s, sub_index| {
                var buf: [32]u8 = undefined;
                const dur = formatCompletedRowDuration(&buf, s.ns);
                var sub_mem_buf: [48]u8 = undefined;
                const sub_range = p.sub_mem[sub_index];
                const sub_mem = formatMemRange(&sub_mem_buf, sub_range.min, sub_range.max);
                if (show_parent) {
                    self.writer.print("      {f} {s}{s}\n", .{ padChildName(s.name), dur, sub_mem }) catch {};
                } else {
                    self.writer.print("  {s} {f} {s}{s}\n", .{ check, padName(s.name), dur, sub_mem }) catch {};
                }
            }
            return;
        }
        const total = (p.end_ns orelse self.elapsedNs()) - p.start_ns;
        var buf: [32]u8 = undefined;
        const dur = formatCompletedRowDuration(&buf, total);
        self.writer.print("  {s} {f} {s}{s}\n", .{ check, padName(p.name), dur, mem }) catch {};
    }

    /// Return to the start of the current line and clear it. Caller holds mutex.
    fn clearLine(self: *Reporter) void {
        self.writer.writeAll("\r") catch {};
        ansi.clearFromCursorToLineEnd(self.writer) catch {};
    }

    /// "2m 25s, peak RSS 6042MB" (the memory part only when sampled).
    fn formatTotals(self: *Reporter, buf: []u8) []const u8 {
        var dur_buf: [32]u8 = undefined;
        const dur = formatDuration(&dur_buf, self.elapsedNs(), .final);
        if (self.peak_bytes == 0) {
            return std.fmt.bufPrint(buf, "{s}", .{dur}) catch buf[0..0];
        }
        var bytes_buf: [32]u8 = undefined;
        const peak = formatBytes(&bytes_buf, self.peak_bytes);
        return std.fmt.bufPrint(buf, "{s}, peak RSS {s}", .{ dur, peak }) catch buf[0..0];
    }

    fn elapsedNs(self: *Reporter) u64 {
        const now = std.Io.Timestamp.now(self.std_io, .awake);
        const delta = now.nanoseconds - self.start_ts.nanoseconds;
        return if (delta > 0) @intCast(delta) else 0;
    }
};

/// Pad a phase name (ASCII) to the duration-alignment column.
fn padName(name: []const u8) PaddedName {
    return .{ .name = name, .width = name_width };
}

fn padChildName(name: []const u8) PaddedName {
    return .{ .name = name, .width = name_width - 2 };
}

const PaddedName = struct {
    name: []const u8,
    width: usize,

    pub fn format(self: PaddedName, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(self.name);
        if (self.name.len < self.width) {
            try writer.splatByteAll(' ', self.width - self.name.len);
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
    /// Human-friendly formatting with sub-second precision, for finished phases.
    final,
};

/// Format a duration in human-readable units.
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

/// Format a duration as rounded, right-aligned, grouped milliseconds.
/// Format a nanosecond duration into `buf`, returning the written slice.
fn formatCompletedRowDuration(buf: []u8, ns: u64) []const u8 {
    var total_ms = ns / std.time.ns_per_ms;
    if (ns % std.time.ns_per_ms >= std.time.ns_per_ms / 2) total_ms += 1;

    const digit_count = countDigits(total_ms);
    const number_length = digit_count + (digit_count - 1) / 3;
    const content_length = number_length + "ms".len;
    const output_length = @max(min_completed_duration_width, content_length);
    if (output_length > buf.len) return buf[0..0];

    const padding_length = output_length - content_length;
    @memset(buf[0..padding_length], ' ');

    const number_end = padding_length + number_length;
    var cursor = number_end;
    var remaining = total_ms;
    var digits_written: usize = 0;
    while (digits_written < digit_count) {
        cursor -= 1;
        buf[cursor] = '0' + @as(u8, @intCast(remaining % 10));
        remaining /= 10;
        digits_written += 1;
        if (digits_written % 3 == 0 and digits_written < digit_count) {
            cursor -= 1;
            buf[cursor] = '_';
        }
    }

    @memcpy(buf[number_end .. number_end + "ms".len], "ms");

    return buf[0..output_length];
}

fn countDigits(value: u64) usize {
    var remaining = value;
    var result: usize = 1;
    while (remaining >= 10) {
        remaining /= 10;
        result += 1;
    }
    return result;
}

/// Format `, RSS 123MB - 456MB` for a sampled range, or an empty string when
/// no sample landed.
fn formatMemRange(buf: []u8, mem_min: u64, mem_max: u64) []const u8 {
    if (mem_max == 0) return buf[0..0];
    var low_buf: [32]u8 = undefined;
    var high_buf: [32]u8 = undefined;
    const low = formatBytes(&low_buf, mem_min);
    const high = formatBytes(&high_buf, mem_max);
    if (std.mem.eql(u8, low, high)) {
        return std.fmt.bufPrint(buf, ", RSS {s}", .{high}) catch buf[0..0];
    }
    return std.fmt.bufPrint(buf, ", RSS {s} - {s}", .{ low, high }) catch buf[0..0];
}

/// Format a byte count in MB. Values at or above 1MB use whole MB; only values
/// below 1MB use a decimal.
fn formatBytes(buf: []u8, bytes: u64) []const u8 {
    const mb = 1024 * 1024;
    if (bytes < mb) {
        const mb_f = @as(f64, @floatFromInt(bytes)) / @as(f64, @floatFromInt(mb));
        return std.fmt.bufPrint(buf, "{d:.1}MB", .{mb_f}) catch buf[0..0];
    }
    return std.fmt.bufPrint(buf, "{d}MB", .{(bytes + mb / 2) / mb}) catch buf[0..0];
}

const testing = std.testing;

test "formatBytes ranges" {
    var buf: [32]u8 = undefined;
    try testing.expectEqualStrings("0.5MB", formatBytes(&buf, 512 * 1024));
    try testing.expectEqualStrings("123MB", formatBytes(&buf, 123 * 1024 * 1024));
    try testing.expectEqualStrings("5673MB", formatBytes(&buf, 5673 * 1024 * 1024));
}

test "formatMemRange collapses equal endpoints and skips missing samples" {
    var buf: [48]u8 = undefined;
    try testing.expectEqualStrings("", formatMemRange(&buf, std.math.maxInt(u64), 0));
    const mb = 1024 * 1024;
    try testing.expectEqualStrings(", RSS 123MB - 456MB", formatMemRange(&buf, 123 * mb, 456 * mb));
    try testing.expectEqualStrings(", RSS 200MB", formatMemRange(&buf, 200 * mb, 200 * mb));
}

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

test "formatCompletedRowDuration pads and groups milliseconds" {
    var buf: [32]u8 = undefined;
    try testing.expectEqualStrings("        0ms", formatCompletedRowDuration(&buf, 100_000));
    try testing.expectEqualStrings("       12ms", formatCompletedRowDuration(&buf, 12 * std.time.ns_per_ms));
    try testing.expectEqualStrings("      999ms", formatCompletedRowDuration(&buf, 999_499_999));
    try testing.expectEqualStrings("    1_000ms", formatCompletedRowDuration(&buf, 999_500_000));
    try testing.expectEqualStrings("    1_200ms", formatCompletedRowDuration(&buf, 1200 * std.time.ns_per_ms));
    try testing.expectEqualStrings("   65_000ms", formatCompletedRowDuration(&buf, 65 * std.time.ns_per_s));
    try testing.expectEqualStrings("  100_500ms", formatCompletedRowDuration(&buf, 100 * std.time.ns_per_s + 500 * std.time.ns_per_ms));
    try testing.expectEqualStrings("6_000_000ms", formatCompletedRowDuration(&buf, 100 * std.time.ns_per_min));
    try testing.expectEqualStrings("60_000_000ms", formatCompletedRowDuration(&buf, 1000 * std.time.ns_per_min));
}

test "padName pads short names and leaves long names" {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    try aw.writer.print("[{f}]", .{padName("Parsing")});
    try testing.expectEqualStrings("[Parsing                              ]", aw.written());
}

test "full-width phase name remains separated from its duration" {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    try aw.writer.print("{f} {s}", .{ padName("arm64 Instruction Generation"), "4ms" });
    try testing.expectEqualStrings("arm64 Instruction Generation          4ms", aw.written());
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
    reporter.recordCompletedWithBreakdown("Compile-Time Evaluation", 70 * std.time.ns_per_ms, .{}, &.{
        .{ .name = "Monotype Lowering", .ns = 40 * std.time.ns_per_ms },
        .{ .name = "LIR Generation", .ns = 10 * std.time.ns_per_ms },
        .{ .name = "LIR Passes", .ns = 3 * std.time.ns_per_ms },
        .{ .name = "ARC", .ns = 2 * std.time.ns_per_ms },
        .{ .name = "Static Data", .ns = 5 * std.time.ns_per_ms },
        .{ .name = "x64 Instruction Generation", .ns = 5 * std.time.ns_per_ms },
        .{ .name = "Execution", .ns = 3 * std.time.ns_per_ms },
        .{ .name = "Store Results", .ns = 2 * std.time.ns_per_ms },
    });
    reporter.begin("LLVM IR Generation");
    reporter.end();
    reporter.begin("LLVM Optimize + Emit");
    reporter.end();
    reporter.begin("Linking");
    reporter.end();
    reporter.recordCounters("Monotype workload", &.{
        .{ .name = "Graph nodes created", .count = 1234 },
        .{ .name = "Unification requests", .count = 5678 },
    });
    reporter.finish();
}

test "static breakdown lists every phase with the timings flag" {
    var buf: std.Io.Writer.Allocating = .init(testing.allocator);
    defer buf.deinit();
    collectStatic(&buf, true);

    const out = buf.written();
    try testing.expect(std.mem.startsWith(u8, out, "roc build"));
    try testing.expect(std.mem.find(u8, out, "Resolving Dependencies") != null);
    try testing.expect(std.mem.find(u8, out, "Parsing") != null);
    try testing.expect(std.mem.find(u8, out, "Name Resolution") != null);
    try testing.expect(std.mem.find(u8, out, "Type Inference") != null);
    try testing.expect(std.mem.find(u8, out, "Compile-Time Evaluation") != null);
    try testing.expect(std.mem.find(u8, out, "Monotype Lowering") != null);
    try testing.expect(std.mem.find(u8, out, "       40ms") != null);
    try testing.expect(std.mem.find(u8, out, "LIR Passes") != null);
    try testing.expect(std.mem.find(u8, out, "ARC") != null);
    try testing.expect(std.mem.find(u8, out, "Store Results") != null);
    try testing.expect(std.mem.find(u8, out, "x64 Instruction Generation") != null);
    try testing.expect(std.mem.find(u8, out, "LLVM IR Generation") != null);
    // The post-codegen backend phases each get their own aligned row.
    try testing.expect(std.mem.find(u8, out, "LLVM Optimize + Emit") != null);
    try testing.expect(std.mem.find(u8, out, "Linking") != null);
    try testing.expect(std.mem.find(u8, out, "Monotype workload") != null);
    try testing.expect(std.mem.find(u8, out, "Graph nodes created") != null);
    try testing.expect(std.mem.find(u8, out, "1234") != null);
    // With a sampled memory range the parent row shows above its breakdown
    // (the range is only truthful on the parent's contiguous window); without
    // sampling the breakdown replaces it entirely.
    if (base.process_memory.currentBytes() != null) {
        try testing.expect(std.mem.find(u8, out, "Type Checking") != null);
        try testing.expect(std.mem.find(u8, out, ", RSS ") != null);
    } else {
        try testing.expect(std.mem.find(u8, out, "Type Checking") == null);
    }
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

test "stopping the animation thread does not wait out its interval" {
    if (comptime !supports_threads) return;

    var io_impl: std.Io.Threaded = .init(testing.allocator, .{});
    defer io_impl.deinit();
    const io = io_impl.io();

    var buf: std.Io.Writer.Allocating = .init(testing.allocator);
    defer buf.deinit();

    var reporter = Reporter.init(.{
        .std_io = io,
        .writer = &buf.writer,
        .op_label = "roc build",
        .timings_flag = false,
        // A terminal is what spawns the animation thread.
        .is_tty = true,
    });
    reporter.start();
    try testing.expect(reporter.thread != null);

    const before = reporter.elapsedNs();
    reporter.deinit();
    const shutdown_ns = reporter.elapsedNs() - before;

    // Shutdown must not block on the thread's wait interval. The bound is
    // generous so a loaded machine cannot fail this, while still catching a
    // shutdown that waits out a whole tick.
    try testing.expect(shutdown_ns < tick_ns / 2);
}
