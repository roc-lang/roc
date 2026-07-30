//! Parallel LSP integration harness.
//!
//! The root module exports compiler-backed LSP specs. This test runner owns
//! filtering, process-level parallelism, timeout reporting, and MiniCI stats.

const std = @import("std");
const build_options = @import("build_options");
const posix = std.posix;
const Allocator = std.mem.Allocator;

const harness = @import("test_harness");
const integration = @import("integration_specs");
const test_env = integration.integration_env;

/// Standard library options for the LSP integration runner.
pub const std_options: std.Options = .{
    .logFn = log,
};

const timeout_result_grace_ms: u64 = 5_000;
const wrapper_name = "lsp integration tests";

const BuildSpecsError = Allocator.Error;
const StatsJsonError = Allocator.Error || std.Io.Dir.AccessError || std.Io.Dir.CreateDirPathError || std.Io.File.OpenError || std.Io.File.Writer.Error;
const RunnerMainError = BuildSpecsError || StatsJsonError || harness.WorkerArgvError || std.process.Args.ToSliceError;

const TestStatus = enum(u8) {
    pass,
    fail,
    skip,
    crash,
    timeout,
};

const WireHeader = extern struct {
    status: u8,
    duration_ns: u64,
    message_len: u32,
};

const TestResult = struct {
    status: TestStatus = .crash,
    duration_ns: u64 = 0,
    message: ?[]const u8 = null,
};

var log_err_count: usize = 0;
const log_level = std.log.Level.warn;

fn matchesFilters(spec_name: []const u8, filters: []const []const u8) bool {
    if (filters.len == 0) return true;
    for (filters) |filter| {
        if (std.mem.find(u8, wrapper_name, filter) != null) return true;
        if (std.mem.find(u8, spec_name, filter) != null) return true;
    }
    return false;
}

fn buildSpecs(allocator: Allocator, filters: []const []const u8) BuildSpecsError![]const integration.Spec {
    var selected: std.ArrayListUnmanaged(integration.Spec) = .empty;
    for (&integration.specs) |spec| {
        if (matchesFilters(spec.name, filters)) {
            try selected.append(allocator, spec);
        }
    }
    return try selected.toOwnedSlice(allocator);
}

fn appendMessagePart(
    message: *std.ArrayListUnmanaged(u8),
    allocator: Allocator,
    wrote_any: *bool,
    comptime fmt: []const u8,
    args: anytype,
) void {
    if (wrote_any.*) {
        message.appendSlice(allocator, "; ") catch return;
    }
    const text = std.fmt.allocPrint(allocator, fmt, args) catch return;
    defer allocator.free(text);
    message.appendSlice(allocator, text) catch return;
    wrote_any.* = true;
}

fn buildMessage(
    allocator: Allocator,
    maybe_error: ?integration.SpecError,
    logged_errors: usize,
    leaked_allocations: usize,
) ?[]const u8 {
    var message: std.ArrayListUnmanaged(u8) = .empty;
    var wrote_any = false;

    if (maybe_error) |err| {
        appendMessagePart(&message, allocator, &wrote_any, "failed with error.{s}", .{@errorName(err)});
    }
    if (logged_errors != 0) {
        appendMessagePart(&message, allocator, &wrote_any, "{d} errors were logged", .{logged_errors});
    }
    if (leaked_allocations != 0) {
        appendMessagePart(&message, allocator, &wrote_any, "{d} allocations leaked", .{leaked_allocations});
    }

    if (!wrote_any) return null;
    return message.toOwnedSlice(allocator) catch null;
}

fn runSingleTest(io: std.Io, allocator: Allocator, spec: integration.Spec, _: u64) TestResult {
    var timer = harness.Timer.start() catch @panic("no clock");

    var spec_allocator_impl: std.heap.DebugAllocator(.{ .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }) = .init;
    const spec_allocator = spec_allocator_impl.allocator();
    test_env.init(spec_allocator, io);
    log_err_count = 0;

    var status: TestStatus = .pass;
    var maybe_error: ?integration.SpecError = null;
    if (spec.run()) |_| {
        status = .pass;
    } else |err| switch (err) {
        error.SkipZigTest => status = .skip,
        else => {
            status = .fail;
            maybe_error = err;
            if (@errorReturnTrace()) |trace| {
                std.debug.dumpErrorReturnTrace(trace);
            }
        },
    }

    const leaks: usize = if (build_options.debugGpaOk(spec_allocator_impl.deinit())) 0 else 1;

    if (log_err_count != 0 or leaks != 0) {
        if (status == .pass or status == .skip) status = .fail;
    }

    return .{
        .status = status,
        .duration_ns = timer.read(),
        .message = buildMessage(allocator, maybe_error, log_err_count, leaks),
    };
}

const max_message = 8192;

fn truncatedMessage(result: TestResult) []const u8 {
    const message_data = result.message orelse "";
    return message_data[0..@min(message_data.len, max_message)];
}

fn serializeResult(fd: posix.fd_t, result: TestResult) void {
    const message_out = truncatedMessage(result);
    const header = WireHeader{
        .status = @intFromEnum(result.status),
        .duration_ns = result.duration_ns,
        .message_len = @intCast(message_out.len),
    };

    harness.writeAll(fd, std.mem.asBytes(&header));
    harness.writeAll(fd, message_out);
}

/// Streamed variant for persistent worker mode: the same wire bytes behind a
/// u32 frame-length prefix.
fn serializeResultStreamed(fd: posix.fd_t, result: TestResult) void {
    harness.writeFrameHeader(fd, @sizeOf(WireHeader) + truncatedMessage(result).len);
    serializeResult(fd, result);
}

fn deserializeResult(buf: []const u8, allocator: Allocator) ?TestResult {
    if (buf.len < @sizeOf(WireHeader)) return null;

    const header: *const WireHeader = @ptrCast(@alignCast(buf.ptr));
    var offset: usize = @sizeOf(WireHeader);
    const message = harness.readStr(buf, &offset, header.message_len, allocator);

    return .{
        .status = @enumFromInt(header.status),
        .duration_ns = header.duration_ns,
        .message = message,
    };
}

fn dupeOptional(allocator: Allocator, value: ?[]const u8) ?[]const u8 {
    return if (value) |slice| allocator.dupe(u8, slice) catch null else null;
}

fn stabilizeResult(allocator: Allocator, result: TestResult) TestResult {
    return .{
        .status = result.status,
        .duration_ns = result.duration_ns,
        .message = dupeOptional(allocator, result.message),
    };
}

fn getTestName(spec: integration.Spec) []const u8 {
    return spec.name;
}

const Pool = harness.ProcessPool(integration.Spec, TestResult, .{
    .runTest = &runSingleTest,
    .serialize = &serializeResult,
    .serializeStreamed = &serializeResultStreamed,
    .deserialize = &deserializeResult,
    .default_result = .{ .status = .crash },
    .timeout_result = .{ .status = .timeout },
    .stabilizeResult = &stabilizeResult,
    .getName = &getTestName,
    .use_process_groups = true,
    .timeout_report_grace_ms = timeout_result_grace_ms,
    .windows_persistent_workers = false,
});

fn statusLabel(status: TestStatus) []const u8 {
    return switch (status) {
        .pass => "PASS",
        .fail => "FAIL",
        .skip => "SKIP",
        .crash => "CRASH",
        .timeout => "TIMEOUT",
    };
}

fn statsStatus(status: TestStatus) []const u8 {
    return switch (status) {
        .pass => "pass",
        .fail => "fail",
        .skip => "skip",
        .crash => "crash",
        .timeout => "timeout",
    };
}

fn nsToMs(ns: u64) f64 {
    return @as(f64, @floatFromInt(ns)) / 1_000_000.0;
}

fn printResults(
    specs: []const integration.Spec,
    results: []const TestResult,
    verbose: bool,
    allocator: Allocator,
    wall_ns: u64,
    workers: usize,
) void {
    var passed: usize = 0;
    var failed: usize = 0;
    var skipped: usize = 0;
    var crashed: usize = 0;
    var timed_out: usize = 0;

    for (specs, results) |spec, result| {
        switch (result.status) {
            .pass => passed += 1,
            .fail => failed += 1,
            .skip => skipped += 1,
            .crash => crashed += 1,
            .timeout => timed_out += 1,
        }

        if (result.status != .pass or verbose) {
            std.debug.print("  {s:<8} {s}  ({d:.1}ms)\n", .{
                statusLabel(result.status),
                spec.name,
                nsToMs(result.duration_ns),
            });
            if (result.message) |message| std.debug.print("        {s}\n", .{message});
            if (result.status != .pass and result.status != .skip) {
                std.debug.print("        Repro: zig build run-test-zig-module-lsp_integration -- --test-filter \"{s}\"\n\n", .{spec.name});
            }
        }
    }

    std.debug.print("\n=== LSP Integration Summary ===\n", .{});
    std.debug.print(
        "{d}/{d} passed, {d} failed, {d} skipped, {d} crashed, {d} timed out, {d:.1}s wall, {d} workers\n",
        .{ passed, specs.len, failed, skipped, crashed, timed_out, @as(f64, @floatFromInt(wall_ns)) / 1_000_000_000.0, workers },
    );

    var durations = allocator.alloc(u64, results.len) catch return;
    defer allocator.free(durations);
    for (results, 0..) |result, i| durations[i] = result.duration_ns;

    if (harness.computeTimingStats(durations)) |_| {
        std.debug.print("\n=== Timing Summary (ms) ===\n", .{});
        harness.printStatsHeader();
        harness.printStatsRow("integration", harness.computeTimingStats(durations));
    }
    harness.printSlowestN(integration.Spec, specs, durations, 5, allocator, getTestName);
}

fn statsSummary(results: []const TestResult) harness.StatsSummary {
    var summary: harness.StatsSummary = .{ .total = results.len };
    for (results) |result| {
        switch (result.status) {
            .pass => summary.passed += 1,
            .fail => summary.failed += 1,
            .skip => summary.skipped += 1,
            .crash => summary.crashed += 1,
            .timeout => summary.timed_out += 1,
        }
    }
    return summary;
}

fn caseStatsData(allocator: Allocator, result: TestResult) []const harness.StatsData {
    if (result.message == null) return &.{};
    const data = allocator.alloc(harness.StatsData, 1) catch return &.{};
    data[0] = .{ .key = "message", .value = result.message.? };
    return data;
}

fn writeStatsJson(
    allocator: Allocator,
    io: std.Io,
    path: []const u8,
    specs: []const integration.Spec,
    results: []const TestResult,
    spans: []const ?harness.PoolSpan,
) StatsJsonError!void {
    var stats_arena = std.heap.ArenaAllocator.init(allocator);
    defer stats_arena.deinit();
    const stats_allocator = stats_arena.allocator();

    var events: std.ArrayListUnmanaged(harness.StatsEvent) = .empty;
    for (specs, results, 0..) |spec, result, i| {
        const span = if (i < spans.len) spans[i] else null;
        const start_ns = if (span) |s| s.start_ns else 0;
        const end_ns = if (span) |s| s.end_ns else result.duration_ns;
        const id = try std.fmt.allocPrint(stats_allocator, "case-{d}", .{i});
        events.append(stats_allocator, .{
            .id = id,
            .parent_id = null,
            .kind = "case",
            .name = spec.name,
            .status = statsStatus(result.status),
            .start_ns = start_ns,
            .end_ns = end_ns,
            .worker_index = if (span) |s| s.worker_index else null,
            .data = caseStatsData(stats_allocator, result),
        }) catch {};
    }

    try harness.writeRunnerStatsJson(stats_allocator, io, path, .{
        .runner = "lsp_integration",
        .summary = statsSummary(results),
        .events = events.items,
    });
}

fn printUsage() void {
    std.debug.print(
        \\Usage: lsp_integration [options]
        \\
        \\Options:
        \\  --filter <pattern>   Run specs matching pattern (repeatable)
        \\  --threads <N>        Max concurrent workers (default: CPU count)
        \\  --timeout <ms>       Per-spec timeout in ms (default: 120000)
        \\  --verbose            Show PASS results with timing
        \\  --stats-json <path>  Write MiniCI harness stats JSON
        \\
    , .{});
}

/// Runs the parallel LSP integration harness or one worker process.
pub fn main(init: std.process.Init) RunnerMainError!void {
    var gpa_impl: std.heap.DebugAllocator(.{ .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }) = .init;
    defer _ = build_options.debugGpaOk(gpa_impl.deinit());
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const args = harness.parseStandardArgs(arena, init.minimal.args) catch |err| {
        printUsage();
        return err;
    };

    if (args.help_requested) {
        printUsage();
        return;
    }

    const specs = try buildSpecs(arena, args.filters);
    if (specs.len == 0) {
        std.debug.print("No LSP integration specs matched filters.\n", .{});
        return;
    }

    // Worker modes: on Windows the harness pool spawned this process with
    // `--worker <idx>` or `--worker-stream`. The worker re-applied the same
    // filters above, so indices stay aligned with the parent's spec list.
    if (Pool.runWorkerMode(init.io, args, specs, args.timeout_ms)) return;

    const cpu_count = std.Thread.getCpuCount() catch 4;
    const max_children = args.max_threads orelse @min(cpu_count, specs.len);

    std.debug.print("=== LSP Integration Harness ===\n", .{});
    std.debug.print("{d} specs, {d} workers, {d}s timeout\n\n", .{ specs.len, max_children, args.timeout_ms / 1000 });

    const results = try gpa.alloc(TestResult, specs.len);
    defer gpa.free(results);
    @memset(results, .{ .status = .crash });
    const spans = try gpa.alloc(?harness.PoolSpan, specs.len);
    defer gpa.free(spans);
    @memset(spans, null);

    const worker_argv_template = try harness.buildWorkerArgvTemplate(init.io, arena, init.minimal.args);

    var wall_timer = harness.Timer.start() catch @panic("no clock");
    Pool.runWithSpans(init.io, specs, results, spans, max_children, args.timeout_ms, gpa, worker_argv_template);
    const wall_ns = wall_timer.read();

    printResults(specs, results, args.verbose, gpa, wall_ns, max_children);

    if (args.stats_json_path) |path| {
        try writeStatsJson(gpa, init.io, path, specs, results, spans);
    }

    var has_failure = false;
    for (results) |result| {
        if (result.message) |message| gpa.free(message);
        switch (result.status) {
            .pass, .skip => {},
            .fail, .crash, .timeout => has_failure = true,
        }
    }
    if (has_failure) std.process.exit(1);
}

/// Logs messages emitted while integration specs execute.
pub fn log(
    comptime message_level: std.log.Level,
    comptime scope: @EnumLiteral(),
    comptime format: []const u8,
    args: anytype,
) void {
    @disableInstrumentation();
    if (@intFromEnum(message_level) <= @intFromEnum(std.log.Level.err)) {
        log_err_count +|= 1;
    }
    if (@intFromEnum(message_level) <= @intFromEnum(log_level)) {
        std.debug.print(
            "[" ++ @tagName(scope) ++ "] (" ++ @tagName(message_level) ++ "): " ++ format ++ "\n",
            args,
        );
    }
}
