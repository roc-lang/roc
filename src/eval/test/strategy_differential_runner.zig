//! Cross-strategy differential harness.
//!
//! The two runtime lowering strategies (`.lss` and `.boxy`) share no lowering
//! code between checked modules and LIR, so each value-semantics concern that
//! both implement (rendering, equality, hashing, matching) is a place they can
//! disagree. This harness makes that disagreement test-detectable: every
//! corpus program is compiled under both strategies and executed on both the
//! LIR interpreter and the dev backend, and all four executions must agree on
//!
//!   - the final inspect string, or the crash message when the run crashes;
//!   - the ordered `dbg` transcript;
//!   - the ordered expect-failure transcript.
//!
//! The corpus is every runnable eval case, every runtime host-effects case,
//! and the harness's own cases (`strategy_differential_corpus.zig`), which pin
//! `to_inspect` override selection, shape coverage for inspect, equality, and
//! hashing, and match lowering. A case that a strategy or backend cannot run
//! is counted and reported per reason, never silently skipped. Eval cases
//! whose `.boxy` executions do not yet agree are listed with their failure
//! and reason in `strategy_differential_gaps.zig`; the run counts them per
//! reason and fails on any unlisted failure and on any stale entry.
//!
//! Cases run through `test_harness.ProcessPool`: forked children on POSIX,
//! persistent `--worker-stream` worker processes on Windows. Each case is
//! isolated, so a compiler panic, backend crash, or hang is attributed to that
//! case instead of killing the run. The `fail-fast` argument switches to
//! sequential in-process execution for debugging.

const std = @import("std");
const builtin = @import("builtin");
const eval = @import("eval");
const base = @import("base");
const collections = @import("collections");
const test_harness = @import("test_harness");
const build_options = @import("build_options");

const helpers = eval.test_helpers;
const InspectedRun = eval.InspectedRun;
const eval_tests = @import("eval_tests.zig");
const host_effects_tests = @import("host_effects_tests.zig");
const corpus = @import("strategy_differential_corpus.zig");
const known_gaps = @import("strategy_differential_gaps.zig");

const posix = std.posix;

const RunnerError = std.mem.Allocator.Error || std.fmt.ParseIntError || test_harness.Timer.Error || test_harness.WorkerArgvError || error{
    InvalidArgument,
    Diverged,
    NothingExecuted,
    StaleKnownGap,
    UnexpectedChildFailure,
    Unexpected,
};

const Strategy = base.SpecializationStrategy;
const strategies = [_]Strategy{ .lss, .boxy };

const Backend = enum { interpreter, dev };
const backends = [_]Backend{ .interpreter, .dev };

const dev_backend_available = eval.backendAvailable(.dev);

/// Where a case came from.
const Origin = enum { eval, host_effects, harness };

const Case = struct {
    name: []const u8,
    source: []const u8,
    source_kind: helpers.SourceKind = .expr,
    imports: []const helpers.ModuleSource = &.{},
    origin: Origin,
    skip_interpreter: bool = false,
    skip_dev: bool = false,
    /// The exact rendering every execution must produce, when the case
    /// states one.
    expected: ?[]const u8 = null,
};

const CaseStatus = enum {
    /// Every execution agreed.
    pass,
    /// Two executions disagreed.
    diverged,
    /// The executions agreed with each other but not with the case's
    /// stated rendering.
    wrong,
    /// Neither strategy compiled the program, so nothing executed.
    compile_skip,
    /// The isolated child died: a compiler panic, signal, or timeout.
    child_failed,
};

const CaseResult = struct {
    status: CaseStatus,
    /// Owned detail text.
    detail: ?[]u8 = null,
    /// Executions a case could not run, one reason line each.
    unsupported: ?[]u8 = null,
    timed_out: bool = false,
};

const Totals = struct {
    pass: usize = 0,
    diverged: usize = 0,
    wrong: usize = 0,
    compile_skip: usize = 0,
    child_failed: usize = 0,
    /// Failures a known strategy gap states, counted by reason.
    known_gap: usize = 0,
    /// Known gaps whose case now agrees, or that name no case.
    stale_gap: usize = 0,
};

const ReasonEntry = struct {
    count: usize,
    examples: [3][]const u8,
    example_count: usize,
};

var verbose_logging: bool = false;

fn logVerbose(comptime fmt: []const u8, args: anytype) void {
    if (verbose_logging) std.debug.print(fmt, args);
}

/// One execution's observable result.
const Execution = struct {
    /// The returned inspect string, or the crash message.
    output: []u8,
    crashed: bool,
    dbg_events: [][]u8,
    expect_failures: [][]u8,
};

fn executionFromResult(gpa: std.mem.Allocator, result: InspectedRun.Result) std.mem.Allocator.Error!Execution {
    var dbg_list = std.ArrayList([]u8).empty;
    var expect_list = std.ArrayList([]u8).empty;
    for (result.events) |event| switch (event) {
        .dbg => |bytes| try dbg_list.append(gpa, try gpa.dupe(u8, bytes)),
        .expect_failed => |bytes| try expect_list.append(gpa, try gpa.dupe(u8, bytes)),
        .crashed, .effect => {},
    };
    return .{
        .output = try gpa.dupe(u8, switch (result.outcome) {
            .returned, .crashed => |bytes| bytes,
        }),
        .crashed = result.outcome == .crashed,
        .dbg_events = try dbg_list.toOwnedSlice(gpa),
        .expect_failures = try expect_list.toOwnedSlice(gpa),
    };
}

/// A compile or execution failure outside the abort protocol, by error name.
const Attempt = union(enum) {
    ran: Execution,
    failed: []const u8,
};

fn execute(gpa: std.mem.Allocator, backend: Backend, lowered: *const helpers.LoweredProgram) std.mem.Allocator.Error!Attempt {
    const result = switch (backend) {
        .interpreter => helpers.inspectedRunResult(gpa, .interpreter, lowered),
        .dev => helpers.inspectedRunResult(gpa, .dev, lowered),
    } catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return .{ .failed = @errorName(err) },
    };
    defer result.deinit(gpa);
    return .{ .ran = try executionFromResult(gpa, result) };
}

const Label = struct {
    strategy: Strategy,
    backend: Backend,

    fn name(label: Label) []const u8 {
        return switch (label.strategy) {
            .lss => switch (label.backend) {
                .interpreter => "lss/interpreter",
                .dev => "lss/dev",
            },
            .boxy => switch (label.backend) {
                .interpreter => "boxy/interpreter",
                .dev => "boxy/dev",
            },
        };
    }
};

fn runCase(gpa: std.mem.Allocator, io: std.Io, case: Case) std.mem.Allocator.Error!CaseResult {
    var unsupported = std.ArrayList(u8).empty;
    defer unsupported.deinit(gpa);

    var attempts: [strategies.len * backends.len]?Attempt = @splat(null);
    var labels: [strategies.len * backends.len]Label = undefined;
    var compile_errors: [strategies.len]?[]const u8 = @splat(null);

    for (strategies, 0..) |strategy, strategy_index| {
        var compiled = helpers.compileInspectedProgramWithOptions(gpa, io, case.source_kind, case.source, case.imports, .{
            .specialization_strategy = strategy,
            .wasm = .skip,
        }) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {
                compile_errors[strategy_index] = @errorName(err);
                continue;
            },
        };
        defer compiled.deinit(gpa);

        // The harness's own programs exist to run; one the checker rejects
        // exercises nothing.
        if (case.origin == .harness and strategy_index == 0) {
            var problem_count = compiled.resources.checker.problems.problems.items.len;
            for (compiled.resources.extra_modules) |module| problem_count += module.checker.problems.problems.items.len;
            if (problem_count != 0) {
                return .{ .status = .wrong, .detail = try std.fmt.allocPrint(
                    gpa,
                    "  case:        {s}\n  the program has {d} checking problem(s)\n",
                    .{ case.name, problem_count },
                ) };
            }
        }

        for (backends, 0..) |backend, backend_index| {
            const slot = strategy_index * backends.len + backend_index;
            labels[slot] = .{ .strategy = strategy, .backend = backend };
            switch (backend) {
                .interpreter => if (case.skip_interpreter) {
                    if (strategy_index == 0) try unsupported.appendSlice(gpa, "case skips the interpreter backend\n");
                    continue;
                },
                .dev => {
                    if (!dev_backend_available) {
                        if (strategy_index == 0) try unsupported.appendSlice(gpa, "dev backend unavailable on this host\n");
                        continue;
                    }
                    if (case.skip_dev) {
                        if (strategy_index == 0) try unsupported.appendSlice(gpa, "case skips the dev backend\n");
                        continue;
                    }
                },
            }
            attempts[slot] = try execute(gpa, backend, &compiled.lowered);
        }
    }

    const owned_unsupported: ?[]u8 = if (unsupported.items.len == 0) null else try unsupported.toOwnedSlice(gpa);

    if (compile_errors[0] != null and compile_errors[1] != null) {
        if (!std.mem.eql(u8, compile_errors[0].?, compile_errors[1].?)) {
            return .{ .status = .diverged, .unsupported = owned_unsupported, .detail = try std.fmt.allocPrint(
                gpa,
                "  case:        {s}\n  differs on:  compile failure\n  lss:         {s}\n  boxy:        {s}\n",
                .{ case.name, compile_errors[0].?, compile_errors[1].? },
            ) };
        }
        return .{ .status = .compile_skip, .unsupported = owned_unsupported };
    }
    for (compile_errors, 0..) |maybe_err, strategy_index| {
        const err_name = maybe_err orelse continue;
        return .{ .status = .diverged, .unsupported = owned_unsupported, .detail = try std.fmt.allocPrint(
            gpa,
            "  case:        {s}\n  differs on:  compile\n  {s} failed to compile: {s}\n  source:\n{s}\n",
            .{ case.name, @tagName(strategies[strategy_index]), err_name, case.source },
        ) };
    }

    var reference: ?struct { attempt: Attempt, label: Label } = null;
    for (attempts, labels) |maybe_attempt, label| {
        const attempt = maybe_attempt orelse continue;
        const ref = reference orelse {
            reference = .{ .attempt = attempt, .label = label };
            continue;
        };
        if (try compareAttempts(gpa, case, ref.label, ref.attempt, label, attempt)) |detail| {
            return .{ .status = .diverged, .detail = detail, .unsupported = owned_unsupported };
        }
    }

    const ref = reference orelse return .{ .status = .compile_skip, .unsupported = owned_unsupported };
    // The harness's own programs read a seed after a `dbg` no compile-time
    // evaluation performs; without it at runtime, the values they test were
    // computed while checking and neither strategy's runtime ran.
    if (case.origin == .harness) switch (ref.attempt) {
        .ran => |execution| if (execution.dbg_events.len == 0) {
            return .{ .status = .wrong, .unsupported = owned_unsupported, .detail = try std.fmt.allocPrint(
                gpa,
                "  case:        {s}\n  reported no dbg at runtime, so its values were computed while checking\n",
                .{case.name},
            ) };
        },
        .failed => {},
    };
    if (case.expected) |expected| switch (ref.attempt) {
        .ran => |execution| if (execution.crashed or !std.mem.eql(u8, execution.output, expected)) {
            return .{ .status = .wrong, .unsupported = owned_unsupported, .detail = try std.fmt.allocPrint(
                gpa,
                "  case:        {s}\n  expected:    {s}\n  every run:   {s}{s}\n  source:\n{s}\n",
                .{ case.name, expected, if (execution.crashed) "crash: " else "", execution.output, case.source },
            ) };
        },
        .failed => |err_name| return .{ .status = .wrong, .unsupported = owned_unsupported, .detail = try std.fmt.allocPrint(
            gpa,
            "  case:        {s}\n  expected:    {s}\n  every run failed: {s}\n",
            .{ case.name, expected, err_name },
        ) },
    };
    return .{ .status = .pass, .unsupported = owned_unsupported };
}

fn compareAttempts(
    gpa: std.mem.Allocator,
    case: Case,
    ref_label: Label,
    ref: Attempt,
    label: Label,
    attempt: Attempt,
) std.mem.Allocator.Error!?[]u8 {
    switch (ref) {
        .failed => |ref_err| switch (attempt) {
            .failed => |err_name| if (!std.mem.eql(u8, ref_err, err_name)) {
                return try divergence(gpa, case, "execution failure", ref_label, ref_err, label, err_name);
            },
            .ran => |execution| return try divergence(gpa, case, "termination", ref_label, ref_err, label, execution.output),
        },
        .ran => |ref_run| switch (attempt) {
            .failed => |err_name| return try divergence(gpa, case, "termination", ref_label, ref_run.output, label, err_name),
            .ran => |run| {
                if (ref_run.crashed != run.crashed) {
                    return try divergence(gpa, case, "termination", ref_label, terminationText(ref_run), label, terminationText(run));
                }
                if (!std.mem.eql(u8, ref_run.output, run.output)) {
                    return try divergence(gpa, case, if (run.crashed) "crash message" else "inspect output", ref_label, ref_run.output, label, run.output);
                }
                if (try compareTranscript(gpa, case, "dbg", ref_label, ref_run.dbg_events, label, run.dbg_events)) |detail| return detail;
                if (try compareTranscript(gpa, case, "expect failure", ref_label, ref_run.expect_failures, label, run.expect_failures)) |detail| return detail;
            },
        },
    }
    return null;
}

fn terminationText(execution: Execution) []const u8 {
    return if (execution.crashed) "crashed" else "returned";
}

fn compareTranscript(
    gpa: std.mem.Allocator,
    case: Case,
    comptime what: []const u8,
    ref_label: Label,
    ref: []const []u8,
    label: Label,
    actual: []const []u8,
) std.mem.Allocator.Error!?[]u8 {
    if (ref.len != actual.len) {
        const ref_text = try std.fmt.allocPrint(gpa, "{d}", .{ref.len});
        defer gpa.free(ref_text);
        const actual_text = try std.fmt.allocPrint(gpa, "{d}", .{actual.len});
        defer gpa.free(actual_text);
        return try divergence(gpa, case, what ++ " event count", ref_label, ref_text, label, actual_text);
    }
    for (ref, actual) |ref_event, event| {
        if (!std.mem.eql(u8, ref_event, event)) {
            return try divergence(gpa, case, what ++ " event", ref_label, ref_event, label, event);
        }
    }
    return null;
}

fn divergence(
    gpa: std.mem.Allocator,
    case: Case,
    what: []const u8,
    ref_label: Label,
    ref: []const u8,
    label: Label,
    actual: []const u8,
) std.mem.Allocator.Error![]u8 {
    return std.fmt.allocPrint(
        gpa,
        "  case:        {s}\n" ++
            "  differs on:  {s}\n" ++
            "  {s:<17}{s}\n" ++
            "  {s:<17}{s}\n" ++
            "  source:\n{s}\n",
        .{ case.name, what, ref_label.name(), ref, label.name(), actual, case.source },
    );
}

/// Aggregates per-case results into totals and printed reporting.
const Report = struct {
    gpa: std.mem.Allocator,
    totals: Totals = .{},
    reasons: std.StringHashMap(ReasonEntry),
    /// Each known gap by case name, with whether a case it names ran.
    gaps: std.StringHashMap(GapState),
    gap_counts: [std.enums.values(known_gaps.Reason).len]usize = @splat(0),
    saw_failure: bool = false,

    const GapState = struct {
        gap: known_gaps.Gap,
        seen: bool = false,
    };

    fn init(gpa: std.mem.Allocator) std.mem.Allocator.Error!Report {
        var report = Report{ .gpa = gpa, .reasons = .init(gpa), .gaps = .init(gpa) };
        errdefer report.deinit();
        for (known_gaps.gaps) |gap| {
            const entry = try report.gaps.getOrPut(gap.case);
            // Each case is listed once.
            std.debug.assert(!entry.found_existing);
            entry.value_ptr.* = .{ .gap = gap };
        }
        return report;
    }

    fn deinit(self: *Report) void {
        var it = self.reasons.iterator();
        while (it.next()) |entry| self.gpa.free(entry.key_ptr.*);
        self.reasons.deinit();
        self.gaps.deinit();
    }

    fn record(self: *Report, case: Case, result: CaseResult) std.mem.Allocator.Error!void {
        const gpa = self.gpa;
        defer if (result.detail) |detail| gpa.free(detail);
        defer if (result.unsupported) |unsupported| gpa.free(unsupported);

        if (result.unsupported) |unsupported| {
            var lines = std.mem.splitScalar(u8, unsupported, '\n');
            while (lines.next()) |line| {
                if (line.len == 0) continue;
                const entry = try self.reasons.getOrPut(line);
                if (entry.found_existing) {
                    entry.value_ptr.count += 1;
                    if (entry.value_ptr.example_count < entry.value_ptr.examples.len) {
                        entry.value_ptr.examples[entry.value_ptr.example_count] = case.name;
                        entry.value_ptr.example_count += 1;
                    }
                } else {
                    entry.key_ptr.* = try gpa.dupe(u8, line);
                    entry.value_ptr.* = .{ .count = 1, .examples = .{ case.name, undefined, undefined }, .example_count = 1 };
                }
            }
        }

        if (self.gaps.getPtr(case.name)) |state| {
            state.seen = true;
            if (gapStatesStatus(state.gap.status, result.status)) {
                self.totals.known_gap += 1;
                self.gap_counts[@intFromEnum(state.gap.reason)] += 1;
                logVerbose("KNOWN-GAP  {s}: {s}\n", .{ case.name, state.gap.reason.text() });
                return;
            }
            if (result.status == .pass) {
                self.totals.stale_gap += 1;
                self.saw_failure = true;
                std.debug.print("STALE-GAP  {s}: now agrees under both strategies; delete its entry in strategy_differential_gaps.zig\n", .{case.name});
                return;
            }
            // A failure the gap does not state is reported like any other.
        }

        switch (result.status) {
            .pass => {
                self.totals.pass += 1;
                logVerbose("PASS       {s}\n", .{case.name});
            },
            .diverged => {
                self.totals.diverged += 1;
                self.saw_failure = true;
                std.debug.print("DIVERGED   {s}\n{s}\n", .{ case.name, result.detail orelse "(no detail)" });
            },
            .wrong => {
                self.totals.wrong += 1;
                self.saw_failure = true;
                std.debug.print("WRONG      {s}\n{s}\n", .{ case.name, result.detail orelse "(no detail)" });
            },
            .compile_skip => {
                self.totals.compile_skip += 1;
                logVerbose("NO-COMPILE {s}\n", .{case.name});
            },
            .child_failed => {
                self.totals.child_failed += 1;
                self.saw_failure = true;
                const fallback: []const u8 = if (result.timed_out)
                    "timed out"
                else
                    "child died without a result (compiler panic or signal)";
                std.debug.print("CHILD-FAIL {s}: {s}\n", .{ case.name, result.detail orelse fallback });
            },
        }
    }
};

fn gapStatesStatus(gap_status: known_gaps.Status, status: CaseStatus) bool {
    return switch (gap_status) {
        .diverged => status == .diverged,
        .child_failed => status == .child_failed,
    };
}

fn statusByte(status: CaseStatus) u8 {
    return switch (status) {
        .pass => 'P',
        .diverged => 'D',
        .wrong => 'W',
        .compile_skip => 'C',
        .child_failed => 'F',
    };
}

fn statusFromByte(byte: u8) ?CaseStatus {
    return switch (byte) {
        'P' => .pass,
        'D' => .diverged,
        'W' => .wrong,
        'C' => .compile_skip,
        'F' => .child_failed,
        else => null,
    };
}

fn runCaseForPool(io: std.Io, allocator: std.mem.Allocator, case: Case, timeout_ms: u64) CaseResult {
    _ = timeout_ms; // the pool's parent-side watchdog enforces the budget
    return runCase(allocator, io, case) catch |err| .{
        .status = .child_failed,
        .detail = std.fmt.allocPrint(allocator, "child error: {s}", .{@errorName(err)}) catch null,
    };
}

/// Wire format: status byte, u32 detail length, detail, then unsupported
/// reason lines.
fn appendResultBytes(out: *std.ArrayList(u8), gpa: std.mem.Allocator, result: CaseResult) std.mem.Allocator.Error!void {
    const detail = result.detail orelse "";
    try out.append(gpa, statusByte(result.status));
    try out.appendSlice(gpa, std.mem.asBytes(&@as(u32, @intCast(detail.len))));
    try out.appendSlice(gpa, detail);
    try out.appendSlice(gpa, result.unsupported orelse "");
}

fn serializeCaseResult(fd: posix.fd_t, result: CaseResult) void {
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(base.defaultGpa());
    appendResultBytes(&buf, base.defaultGpa(), result) catch return;
    test_harness.writeAll(fd, buf.items);
}

fn serializeCaseResultStreamed(fd: posix.fd_t, result: CaseResult) void {
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(base.defaultGpa());
    appendResultBytes(&buf, base.defaultGpa(), result) catch return;
    test_harness.writeFrameHeader(fd, buf.items.len);
    test_harness.writeAll(fd, buf.items);
}

fn deserializeCaseResult(buf: []const u8, gpa: std.mem.Allocator) ?CaseResult {
    if (buf.len < 5) return null;
    const status = statusFromByte(buf[0]) orelse return null;
    var detail_len: u32 = undefined;
    @memcpy(std.mem.asBytes(&detail_len), buf[1..5]);
    if (buf.len < 5 + detail_len) return null;
    const detail_bytes = buf[5 .. 5 + detail_len];
    const unsupported_bytes = buf[5 + detail_len ..];
    return .{
        .status = status,
        .detail = if (detail_bytes.len == 0) null else gpa.dupe(u8, detail_bytes) catch null,
        .unsupported = if (unsupported_bytes.len == 0) null else gpa.dupe(u8, unsupported_bytes) catch null,
    };
}

fn stabilizeCaseResult(gpa: std.mem.Allocator, result: CaseResult) CaseResult {
    return .{
        .status = result.status,
        .detail = if (result.detail) |detail| gpa.dupe(u8, detail) catch null else null,
        .unsupported = if (result.unsupported) |unsupported| gpa.dupe(u8, unsupported) catch null else null,
        .timed_out = result.timed_out,
    };
}

fn getCaseName(case: Case) []const u8 {
    return case.name;
}

const Pool = test_harness.ProcessPool(Case, CaseResult, .{
    .runTest = &runCaseForPool,
    .serialize = &serializeCaseResult,
    .serializeStreamed = &serializeCaseResultStreamed,
    .deserialize = &deserializeCaseResult,
    .default_result = .{ .status = .child_failed },
    .timeout_result = .{ .status = .child_failed, .timed_out = true },
    .stabilizeResult = &stabilizeCaseResult,
    .getName = &getCaseName,
    .use_process_groups = true,
});

/// The inspected rendering of a `Str` whose contents need no escaping.
fn inspectedStrLiteral(gpa: std.mem.Allocator, contents: []const u8) RunnerError![]const u8 {
    for (contents) |byte| {
        if (byte == '"' or byte == '\\' or byte < 0x20) return error.Unexpected;
    }
    return std.fmt.allocPrint(gpa, "\"{s}\"", .{contents});
}

/// `arena` owns the case text built here, which lives as long as the run.
fn collectCases(gpa: std.mem.Allocator, arena: std.mem.Allocator, cases: *std.ArrayList(Case), include_opt_in: bool) RunnerError!void {
    for (corpus.cases) |case| {
        try cases.append(gpa, .{
            .name = case.name,
            .source = case.source,
            .source_kind = case.source_kind,
            .imports = case.imports,
            .origin = .harness,
            .expected = case.expected,
        });
    }
    for (eval_tests.tests) |tc| {
        if (tc.opt_in and !include_opt_in) continue;
        const expected: ?[]const u8 = switch (tc.expected) {
            // Frontend-problem and compile-time-only cases execute nothing
            // at runtime, so neither strategy is exercised.
            .problem, .problem_and_crash, .comptime_f32_bits, .comptime_f64_bits, .comptime_f32_list_bits, .comptime_f64_list_bits => continue,
            .inspect_str => |text| text,
            // Allocation cases state the contents of the `Str` they
            // produce; every execution here renders that `Str` inspected.
            .allocations_at_most => |allocation| try inspectedStrLiteral(arena, allocation.output),
            .crash => null,
        };
        try cases.append(gpa, .{
            .name = tc.name,
            .source = tc.source,
            .source_kind = tc.source_kind,
            .imports = tc.imports,
            .origin = .eval,
            .skip_interpreter = tc.skip.interpreter,
            .skip_dev = tc.skip.dev,
            .expected = expected,
        });
    }
    for (host_effects_tests.tests) |tc| {
        try cases.append(gpa, .{
            .name = tc.name,
            .source = tc.source,
            .source_kind = tc.source_kind,
            .imports = tc.imports,
            .origin = .host_effects,
            .skip_interpreter = tc.skip.interpreter,
            .skip_dev = tc.skip.dev,
        });
    }
}

/// Entry point for the cross-strategy differential harness.
pub fn main(init: std.process.Init) RunnerError!void {
    const io = init.io;

    var gpa_impl: std.heap.DebugAllocator(.{ .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }) = .init;
    defer _ = build_options.debugGpaOk(gpa_impl.deinit());
    const gpa = gpa_impl.allocator();

    var args_arena = collections.SingleThreadArena.init(gpa);
    defer args_arena.deinit();
    const cli = try test_harness.parseStandardArgs(args_arena.allocator(), init.minimal.args);

    if (cli.help_requested) {
        printHelp();
        return;
    }
    verbose_logging = cli.verbose;

    var fail_fast = false;
    var harness_only = false;
    for (cli.positional) |arg| {
        if (std.mem.eql(u8, arg, "fail-fast")) {
            fail_fast = true;
        } else if (std.mem.eql(u8, arg, "harness-only")) {
            harness_only = true;
        } else {
            std.debug.print("unknown positional argument: {s}\n", .{arg});
            printHelp();
            return error.InvalidArgument;
        }
    }

    var cases: std.ArrayList(Case) = .empty;
    defer cases.deinit(gpa);
    try collectCases(gpa, args_arena.allocator(), &cases, cli.filters.len != 0);

    var run_list: std.ArrayList(Case) = .empty;
    defer run_list.deinit(gpa);
    for (cases.items) |case| {
        if (harness_only and case.origin != .harness) continue;
        if (cli.filters.len > 0) {
            var matched = false;
            for (cli.filters) |pattern| {
                if (std.mem.find(u8, case.name, pattern) != null or
                    std.mem.find(u8, case.source, pattern) != null)
                {
                    matched = true;
                    break;
                }
            }
            if (!matched) continue;
        }
        try run_list.append(gpa, case);
    }

    // Worker modes: the Windows pool spawned this process to execute specific
    // cases. The worker argv template preserves every selection flag, so the
    // run list built above matches the parent's and indices stay aligned.
    if (Pool.runWorkerMode(io, cli, run_list.items, cli.timeout_ms)) return;

    var report = try Report.init(gpa);
    defer report.deinit();
    var timer = try test_harness.Timer.start();

    if (fail_fast) {
        for (run_list.items) |case| {
            const result = try runCase(gpa, io, case);
            try report.record(case, result);
            if (report.saw_failure) break;
        }
    } else {
        const results = try gpa.alloc(CaseResult, run_list.items.len);
        defer gpa.free(results);
        @memset(results, .{ .status = .child_failed });

        const worker_argv_template = try test_harness.buildWorkerArgvTemplate(io, args_arena.allocator(), init.minimal.args);
        const cpu_count = std.Thread.getCpuCount() catch 4;
        const job_limit = @max(cli.max_threads orelse @min(cpu_count, run_list.items.len), 1);
        Pool.runWithSpans(io, run_list.items, results, null, job_limit, cli.timeout_ms, gpa, worker_argv_template, cli.child_debug);

        for (run_list.items, results) |case, result| try report.record(case, result);
    }

    const totals = report.totals;
    std.debug.print(
        "\nstrategy differential: {d} cases in {d} ms\n" ++
            "  agreed:              {d}\n" ++
            "  diverged:            {d}\n" ++
            "  agreed but wrong:    {d}\n" ++
            "  did not compile:     {d}\n" ++
            "  child failures:      {d}\n" ++
            "  known strategy gaps: {d}\n",
        .{
            run_list.items.len,
            timer.read() / std.time.ns_per_ms,
            totals.pass,
            totals.diverged,
            totals.wrong,
            totals.compile_skip,
            totals.child_failed,
            totals.known_gap,
        },
    );

    // Every gap names a case of the full run.
    if (cli.filters.len == 0 and !harness_only) {
        var it = report.gaps.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.seen) continue;
            report.totals.stale_gap += 1;
            std.debug.print("STALE-GAP  {s}: names no case; delete its entry in strategy_differential_gaps.zig\n", .{entry.key_ptr.*});
        }
    }

    if (report.totals.known_gap > 0) {
        std.debug.print("\nknown strategy gaps (counted, not skipped):\n", .{});
        for (report.gap_counts, 0..) |count, index| {
            if (count == 0) continue;
            const reason: known_gaps.Reason = @enumFromInt(index);
            std.debug.print("  {d:>5}  {s}\n", .{ count, reason.text() });
        }
    }

    std.debug.print("\nshape coverage exemptions:\n", .{});
    for (corpus.equality_exemptions) |exemption| std.debug.print("  equality {s}: {s}\n", .{ @tagName(exemption.kind), exemption.reason });
    for (corpus.hash_exemptions) |exemption| std.debug.print("  hashing  {s}: {s}\n", .{ @tagName(exemption.kind), exemption.reason });

    if (report.reasons.count() > 0) {
        std.debug.print("\nexecutions not run (stated coverage gaps):\n", .{});
        var it = report.reasons.iterator();
        while (it.next()) |entry| {
            std.debug.print("  {d:>5}  {s}  (e.g.", .{ entry.value_ptr.count, entry.key_ptr.* });
            for (entry.value_ptr.examples[0..entry.value_ptr.example_count]) |example| {
                std.debug.print(" \"{s}\"", .{example});
            }
            std.debug.print(")\n", .{});
        }
    }

    if (totals.diverged > 0 or totals.wrong > 0) {
        std.debug.print("\nFAILED: {d} divergence(s), {d} wrong agreement(s)\n", .{ totals.diverged, totals.wrong });
        return error.Diverged;
    }
    if (totals.child_failed > 0) {
        std.debug.print("\nFAILED: {d} case(s) died (compiler panic, backend crash, or timeout)\n", .{totals.child_failed});
        return error.UnexpectedChildFailure;
    }
    if (report.totals.stale_gap > 0) {
        std.debug.print("\nFAILED: {d} stale known gap(s)\n", .{report.totals.stale_gap});
        return error.StaleKnownGap;
    }
    if (totals.pass == 0) {
        std.debug.print("\nFAILED: no case executed under both strategies\n", .{});
        return error.NothingExecuted;
    }
    std.debug.print("\nOK\n", .{});
}

fn printHelp() void {
    std.debug.print(
        "strategy differential runner\n\n" ++
            "Compiles each corpus program under .lss and .boxy, runs it on the LIR\n" ++
            "interpreter and the dev backend, and requires every execution to agree\n" ++
            "on its inspect output or crash message, dbg transcript, and\n" ++
            "expect-failure transcript.\n\n" ++
            "Options:\n" ++
            "  --filter <substr>   only run cases whose name/source matches (repeatable)\n" ++
            "  --threads <N>       max concurrent case processes (default: min(cpu_count, cases))\n" ++
            "  --timeout <ms>      per-case watchdog budget (default: 120000)\n" ++
            "  --verbose           per-case logging\n" ++
            "  harness-only        only the harness's own corpus\n" ++
            "  fail-fast           stop at the first failure; runs cases sequentially\n" ++
            "                      in-process (no crash isolation) for debugging\n",
        .{},
    );
}
