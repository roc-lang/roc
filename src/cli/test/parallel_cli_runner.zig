//! Parallel CLI test runner for Roc platform integration tests.
//!
//! Replaces the 5 sequential test_runner invocations in `zig build test-cli`
//! with a single binary that runs all platform tests in parallel using a
//! fork-based process pool (via src/build/test_harness.zig).
//!
//! Usage:
//!   parallel_cli_runner <roc_binary> [options]
//!
//! Options:
//!   --filter <pattern>   Run only tests whose name contains <pattern> (repeatable)
//!   --threads <N>        Max concurrent child processes (default: CPU count)
//!   --timeout <ms>       Per-test timeout in ms (default: 60000)
//!   --verbose            Print PASS results and timing details

const std = @import("std");
const posix = std.posix;
const Allocator = std.mem.Allocator;

const harness = @import("test_harness");
const platform_config = @import("platform_config.zig");

// Test spec types

/// A single CLI test operation — one atomic unit of work.
const CliTestSpec = struct {
    /// Human-readable name, e.g. "test/fx/hello_world.roc [dev]"
    name: []const u8,
    /// Path to .roc file (relative to project root)
    roc_file: []const u8,
    /// Platform name (for display grouping)
    platform: []const u8,
    /// Backend: null = interpreter, "dev" = dev backend
    backend: ?[]const u8,
    /// What kind of test to run
    test_kind: TestKind,

    const TestKind = union(enum) {
        /// Build natively and run; check exit code 0
        native_run,
        /// Build natively, run with --test <spec>; check exit code 0
        io_spec: []const u8,
    };
};

/// Which platform/backend combos to test (mirrors build.zig's 5 invocations).
const RunConfig = struct {
    platform_name: []const u8,
    backend: ?[]const u8,
};

const run_configs = [_]RunConfig{
    .{ .platform_name = "int", .backend = null },
    .{ .platform_name = "str", .backend = null },
    .{ .platform_name = "int", .backend = "dev" },
    .{ .platform_name = "str", .backend = "dev" },
    .{ .platform_name = "fx", .backend = "dev" },
};

// Spec generation

fn buildTestSpecs(allocator: Allocator, filters: []const []const u8) ![]const CliTestSpec {
    var specs: std.ArrayListUnmanaged(CliTestSpec) = .empty;

    for (&run_configs) |cfg| {
        const platform = platform_config.findPlatform(cfg.platform_name) orelse continue;

        switch (platform.test_apps) {
            .single => |app_name| {
                const roc_file = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ platform.base_dir, app_name });
                const name = try fmtTestName(allocator, roc_file, cfg.backend);
                if (matchesFilters(name, roc_file, filters)) {
                    try specs.append(allocator, .{
                        .name = name,
                        .roc_file = roc_file,
                        .platform = platform.name,
                        .backend = cfg.backend,
                        .test_kind = .native_run,
                    });
                }
            },
            .spec_list => |io_specs| {
                for (io_specs) |spec| {
                    const name = try fmtTestName(allocator, spec.roc_file, cfg.backend);
                    if (matchesFilters(name, spec.roc_file, filters)) {
                        try specs.append(allocator, .{
                            .name = name,
                            .roc_file = spec.roc_file,
                            .platform = platform.name,
                            .backend = cfg.backend,
                            .test_kind = .{ .io_spec = spec.io_spec },
                        });
                    }
                }
            },
            .simple_list => |simple_specs| {
                for (simple_specs) |spec| {
                    const name = try fmtTestName(allocator, spec.roc_file, cfg.backend);
                    if (matchesFilters(name, spec.roc_file, filters)) {
                        try specs.append(allocator, .{
                            .name = name,
                            .roc_file = spec.roc_file,
                            .platform = platform.name,
                            .backend = cfg.backend,
                            .test_kind = .native_run,
                        });
                    }
                }
            },
        }
    }

    return specs.toOwnedSlice(allocator);
}

fn fmtTestName(allocator: Allocator, roc_file: []const u8, backend: ?[]const u8) ![]const u8 {
    if (backend) |b| {
        return std.fmt.allocPrint(allocator, "{s} [{s}]", .{ roc_file, b });
    }
    return std.fmt.allocPrint(allocator, "{s}", .{roc_file});
}

fn matchesFilters(name: []const u8, roc_file: []const u8, filters: []const []const u8) bool {
    if (filters.len == 0) return true;
    for (filters) |f| {
        if (std.mem.indexOf(u8, name, f) != null) return true;
        if (std.mem.indexOf(u8, roc_file, f) != null) return true;
    }
    return false;
}

// Wire protocol (child -> parent via pipe)

const TestStatus = enum(u8) {
    pass = 0,
    fail = 1,
    skip = 2,
    timeout = 3,
    crash = 4,
};

const WireHeader = extern struct {
    status: u8,
    duration_ns: u64,
    exit_code: u32,
    stderr_len: u32,
    stdout_len: u32,
    message_len: u32,
};

const TestResult = struct {
    status: TestStatus = .crash,
    duration_ns: u64 = 0,
    exit_code: u32 = 0,
    stderr_capture: ?[]const u8 = null,
    stdout_capture: ?[]const u8 = null,
    message: ?[]const u8 = null,
};

fn serializeResult(fd: posix.fd_t, result: TestResult) void {
    const stderr_data = result.stderr_capture orelse "";
    const stdout_data = result.stdout_capture orelse "";
    const message_data = result.message orelse "";

    const max_capture = 8192;
    const stderr_out = stderr_data[0..@min(stderr_data.len, max_capture)];
    const stdout_out = stdout_data[0..@min(stdout_data.len, max_capture)];
    const message_out = message_data[0..@min(message_data.len, max_capture)];

    const header = WireHeader{
        .status = @intFromEnum(result.status),
        .duration_ns = result.duration_ns,
        .exit_code = result.exit_code,
        .stderr_len = @intCast(stderr_out.len),
        .stdout_len = @intCast(stdout_out.len),
        .message_len = @intCast(message_out.len),
    };

    harness.writeAll(fd, std.mem.asBytes(&header));
    harness.writeAll(fd, stderr_out);
    harness.writeAll(fd, stdout_out);
    harness.writeAll(fd, message_out);
}

fn deserializeResult(buf: []const u8, gpa: Allocator) ?TestResult {
    if (buf.len < @sizeOf(WireHeader)) return null;

    const header: *const WireHeader = @ptrCast(@alignCast(buf.ptr));
    var offset: usize = @sizeOf(WireHeader);

    const stderr_capture = harness.readStr(buf, &offset, header.stderr_len, gpa);
    const stdout_capture = harness.readStr(buf, &offset, header.stdout_len, gpa);
    const message = harness.readStr(buf, &offset, header.message_len, gpa);

    return .{
        .status = @enumFromInt(header.status),
        .duration_ns = header.duration_ns,
        .exit_code = header.exit_code,
        .stderr_capture = stderr_capture,
        .stdout_capture = stdout_capture,
        .message = message,
    };
}

// Child test execution

var roc_binary_path: []const u8 = "";

var next_cache_id: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);

fn createIsolatedCacheDir(allocator: Allocator) ![]u8 {
    const cache_id = next_cache_id.fetchAdd(1, .monotonic);
    const cache_leaf = try std.fmt.allocPrint(allocator, "{d}-{d}", .{
        @as(u64, @intCast(std.time.nanoTimestamp())),
        cache_id,
    });
    defer allocator.free(cache_leaf);

    const cwd_path = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd_path);

    const cache_rel = try std.fs.path.join(allocator, &.{ ".zig-cache", "roc-test-cache", cache_leaf });
    defer allocator.free(cache_rel);

    std.fs.cwd().makePath(cache_rel) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    return std.fs.path.join(allocator, &.{ cwd_path, cache_rel });
}

fn runSingleTest(allocator: Allocator, spec: CliTestSpec) TestResult {
    var timer = harness.Timer.start() catch return .{ .status = .crash, .message = "no clock" };

    const cache_dir = createIsolatedCacheDir(allocator) catch
        return .{ .status = .crash, .message = "failed to create cache dir" };
    defer std.fs.cwd().deleteTree(cache_dir) catch {};

    const pid = std.c.getpid();
    const output_name = std.fmt.allocPrint(allocator, "./.test_output_{d}", .{pid}) catch
        return .{ .status = .crash, .message = "OOM" };
    defer std.fs.cwd().deleteFile(output_name) catch {};

    var env_map = std.process.getEnvMap(allocator) catch
        return .{ .status = .crash, .message = "failed to get env" };
    defer env_map.deinit();
    env_map.put("ROC_CACHE_DIR", cache_dir) catch
        return .{ .status = .crash, .message = "failed to set env" };

    // Step 1: Build
    const output_arg = std.fmt.allocPrint(allocator, "--output={s}", .{output_name}) catch
        return .{ .status = .crash, .message = "OOM" };

    var build_argv_buf: [5][]const u8 = undefined;
    var argc: usize = 0;
    build_argv_buf[argc] = roc_binary_path;
    argc += 1;
    build_argv_buf[argc] = "build";
    argc += 1;
    build_argv_buf[argc] = output_arg;
    argc += 1;
    if (spec.backend) |b| {
        const backend_arg = std.fmt.allocPrint(allocator, "--opt={s}", .{b}) catch
            return .{ .status = .crash, .message = "OOM" };
        build_argv_buf[argc] = backend_arg;
        argc += 1;
    }
    build_argv_buf[argc] = spec.roc_file;
    argc += 1;

    const build_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = build_argv_buf[0..argc],
        .env_map = &env_map,
    }) catch |err| {
        const msg = std.fmt.allocPrint(allocator, "build spawn error: {}", .{err}) catch "build spawn error";
        return .{ .status = .fail, .duration_ns = timer.read(), .message = msg };
    };

    if (!isSuccess(build_result.term)) {
        return .{
            .status = .fail,
            .duration_ns = timer.read(),
            .exit_code = exitCode(build_result.term),
            .stderr_capture = build_result.stderr,
            .stdout_capture = build_result.stdout,
            .message = "build failed",
        };
    }
    allocator.free(build_result.stdout);
    allocator.free(build_result.stderr);

    std.fs.cwd().access(output_name, .{}) catch {
        return .{ .status = .fail, .duration_ns = timer.read(), .message = "build succeeded but binary not created" };
    };

    // Step 2: Run
    switch (spec.test_kind) {
        .native_run => {
            const run_result = std.process.Child.run(.{
                .allocator = allocator,
                .argv = &[_][]const u8{output_name},
            }) catch |err| {
                const msg = std.fmt.allocPrint(allocator, "run spawn error: {}", .{err}) catch "run spawn error";
                return .{ .status = .fail, .duration_ns = timer.read(), .message = msg };
            };
            return checkRunResult(run_result, &timer, "run failed");
        },
        .io_spec => |io_spec| {
            const run_result = std.process.Child.run(.{
                .allocator = allocator,
                .argv = &[_][]const u8{ output_name, "--test", io_spec },
            }) catch |err| {
                const msg = std.fmt.allocPrint(allocator, "io_spec run spawn error: {}", .{err}) catch "run spawn error";
                return .{ .status = .fail, .duration_ns = timer.read(), .message = msg };
            };
            return checkRunResult(run_result, &timer, "io_spec test failed");
        },
    }
}

fn checkRunResult(result: std.process.Child.RunResult, timer: *harness.Timer, fail_msg: []const u8) TestResult {
    if (hasMemoryErrors(result.stderr)) |mem_msg| {
        return .{
            .status = .fail,
            .duration_ns = timer.read(),
            .exit_code = exitCode(result.term),
            .stderr_capture = result.stderr,
            .stdout_capture = result.stdout,
            .message = mem_msg,
        };
    }
    if (!isSuccess(result.term)) {
        return .{
            .status = .fail,
            .duration_ns = timer.read(),
            .exit_code = exitCode(result.term),
            .stderr_capture = result.stderr,
            .stdout_capture = result.stdout,
            .message = fail_msg,
        };
    }
    return .{ .status = .pass, .duration_ns = timer.read() };
}

fn isSuccess(term: std.process.Child.Term) bool {
    return switch (term) {
        .Exited => |code| code == 0,
        else => false,
    };
}

fn exitCode(term: std.process.Child.Term) u32 {
    return switch (term) {
        .Exited => |code| @intCast(code),
        .Signal => |sig| @as(u32, sig) | 0x80000000,
        else => 0xFFFFFFFF,
    };
}

fn hasMemoryErrors(stderr: []const u8) ?[]const u8 {
    if (std.mem.indexOf(u8, stderr, "error(gpa):") != null) return "memory error detected";
    if (std.mem.indexOf(u8, stderr, "allocation(s) not freed") != null) return "memory leak detected";
    return null;
}

fn getTestName(spec: CliTestSpec) []const u8 {
    return spec.name;
}

fn dupeOptional(gpa: Allocator, value: ?[]const u8) ?[]const u8 {
    return if (value) |slice| gpa.dupe(u8, slice) catch null else null;
}

fn stabilizeResult(gpa: Allocator, result: TestResult) TestResult {
    return .{
        .status = result.status,
        .duration_ns = result.duration_ns,
        .exit_code = result.exit_code,
        .stderr_capture = dupeOptional(gpa, result.stderr_capture),
        .stdout_capture = dupeOptional(gpa, result.stdout_capture),
        .message = dupeOptional(gpa, result.message),
    };
}

// Process pool (via harness)

const Pool = harness.ProcessPool(CliTestSpec, TestResult, .{
    .runTest = &runSingleTest,
    .serialize = &serializeResult,
    .deserialize = &deserializeResult,
    .default_result = .{ .status = .crash },
    .timeout_result = .{ .status = .timeout },
    .stabilizeResult = &stabilizeResult,
    .getName = &getTestName,
    .use_process_groups = true,
});

// Output

fn printResults(
    tests: []const CliTestSpec,
    results: []const TestResult,
    verbose: bool,
    gpa: Allocator,
    wall_ns: u64,
    max_children: usize,
) void {
    var passed: usize = 0;
    var failed: usize = 0;
    var crashed: usize = 0;
    var skipped: usize = 0;
    var timed_out: usize = 0;

    for (tests, 0..) |tc, i| {
        const r = results[i];
        const ms = harness.nsToMs(r.duration_ns);

        switch (r.status) {
            .pass => {
                passed += 1;
                if (verbose) std.debug.print("  PASS  {s}  ({d:.1}ms)\n", .{ tc.name, ms });
            },
            .fail => {
                failed += 1;
                std.debug.print("  FAIL  {s}  ({d:.1}ms)\n", .{ tc.name, ms });
                if (r.message) |msg| std.debug.print("        {s}\n", .{msg});
                if (r.exit_code != 0) {
                    if (r.exit_code & 0x80000000 != 0) {
                        std.debug.print("        signal {d}\n", .{r.exit_code & 0x7FFFFFFF});
                    } else {
                        std.debug.print("        exit code {d}\n", .{r.exit_code});
                    }
                }
                printCapturedOutput("stderr", r.stderr_capture);
                printCapturedOutput("stdout", r.stdout_capture);
                printRepro(tc.name);
            },
            .crash => {
                crashed += 1;
                std.debug.print("  CRASH {s}  ({d:.1}ms)\n", .{ tc.name, ms });
                if (r.message) |msg| std.debug.print("        {s}\n", .{msg});
                printCapturedOutput("stderr", r.stderr_capture);
                printRepro(tc.name);
            },
            .timeout => {
                timed_out += 1;
                std.debug.print("  HANG  {s}\n", .{tc.name});
                printRepro(tc.name);
            },
            .skip => {
                skipped += 1;
                if (verbose) std.debug.print("  SKIP  {s}\n", .{tc.name});
            },
        }
    }

    const wall_ms = harness.nsToMs(wall_ns);
    std.debug.print("\n{d} passed, {d} failed", .{ passed, failed });
    if (crashed > 0) std.debug.print(", {d} crashed", .{crashed});
    if (timed_out > 0) std.debug.print(", {d} hung", .{timed_out});
    if (skipped > 0) std.debug.print(", {d} skipped", .{skipped});
    std.debug.print(" ({d} total) in {d:.0}ms using {d} worker(s)\n", .{ tests.len, wall_ms, max_children });

    // Timing summary
    var durations: std.ArrayListUnmanaged(u64) = .empty;
    defer durations.deinit(gpa);
    for (results) |r| {
        if (r.duration_ns > 0) durations.append(gpa, r.duration_ns) catch continue;
    }
    if (harness.computeTimingStats(durations.items)) |_| {
        std.debug.print("\n=== Timing Summary (ms) ===\n", .{});
        harness.printStatsHeader();
        harness.printStatsRow("total", harness.computeTimingStats(durations.items));
    }

    var duration_arr = gpa.alloc(u64, results.len) catch return;
    defer gpa.free(duration_arr);
    for (results, 0..) |r, i| duration_arr[i] = r.duration_ns;
    harness.printSlowestN(CliTestSpec, tests, duration_arr, 5, gpa, getTestName);
}

fn printCapturedOutput(label: []const u8, capture: ?[]const u8) void {
    const data = capture orelse return;
    if (data.len == 0) return;
    var lines = std.mem.splitScalar(u8, data, '\n');
    var count: usize = 0;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (count == 0) {
            std.debug.print("        {s}: {s}\n", .{ label, line });
        } else if (count < 5) {
            std.debug.print("        {s}\n", .{line});
        } else {
            std.debug.print("        ... ({s} truncated)\n", .{label});
            break;
        }
        count += 1;
    }
}

fn printRepro(test_name: []const u8) void {
    std.debug.print("        Repro: zig build test-cli -- --test-filter \"{s}\"\n\n", .{test_name});
}

// Main

fn printUsage() void {
    std.debug.print(
        \\Usage: parallel_cli_runner <roc_binary> [options]
        \\
        \\Options:
        \\  --filter <pattern>   Run tests matching pattern (repeatable)
        \\  --threads <N>        Max concurrent workers (default: CPU count)
        \\  --timeout <ms>       Per-test timeout in ms (default: 60000)
        \\  --verbose            Show PASS results with timing
        \\
    , .{});
}

/// Entry point for the parallel CLI test runner.
pub fn main() !void {
    var gpa_impl: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var spec_arena = std.heap.ArenaAllocator.init(gpa);
    defer spec_arena.deinit();

    const args = try harness.parseStandardArgs(spec_arena.allocator());

    if (args.help_requested) {
        printUsage();
        return;
    }

    if (args.positional.len < 1) {
        printUsage();
        std.process.exit(1);
    }

    roc_binary_path = args.positional[0];

    const tests = try buildTestSpecs(spec_arena.allocator(), args.filters);
    if (tests.len == 0) return;

    const cpu_count = std.Thread.getCpuCount() catch 4;
    const max_children = args.max_threads orelse @min(cpu_count, tests.len);

    std.debug.print("=== CLI Test Runner ===\n", .{});
    std.debug.print("{d} tests, {d} workers, {d}s timeout\n\n", .{ tests.len, max_children, args.timeout_ms / 1000 });

    const results = try gpa.alloc(TestResult, tests.len);
    defer gpa.free(results);
    @memset(results, .{ .status = .crash });

    var wall_timer = harness.Timer.start() catch @panic("no clock");
    Pool.run(tests, results, max_children, args.timeout_ms, gpa);
    const wall_ns = wall_timer.read();

    printResults(tests, results, args.verbose, gpa, wall_ns, max_children);

    for (results) |r| {
        if (r.stderr_capture) |s| gpa.free(s);
        if (r.stdout_capture) |s| gpa.free(s);
        if (r.message) |m| gpa.free(m);
    }

    for (results) |r| {
        switch (r.status) {
            .fail, .crash, .timeout => std.process.exit(1),
            else => {},
        }
    }
}
