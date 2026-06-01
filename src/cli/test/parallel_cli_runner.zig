//! Parallel CLI test runner for Roc platform integration tests.
//!
//! Runs platform fixtures as an explicit `--opt` matrix using a fork-based
//! process pool (via src/build/test_harness.zig).
//!
//! Usage:
//!   parallel_cli_runner <roc_binary> [options]
//!
//! Options:
//!   --filter <pattern>   Run only tests whose name contains <pattern> (repeatable)
//!   --threads <N>        Max concurrent child processes (default: CPU count)
//!   --timeout <ms>       Per-test timeout in ms (default: 120000)
//!   --include-llvm       Include size and speed LLVM backend jobs
//!   --verbose            Print PASS results and timing details

const std = @import("std");
const builtin = @import("builtin");
const posix = std.posix;
const Allocator = std.mem.Allocator;

const harness = @import("test_harness");
const platform_config = @import("platform_config.zig");
const util = @import("util.zig");

var debug_threaded_io_instance: std.Io.Threaded = .init_single_threaded;
/// Override the default debug IO so that `std.Options.debug_io` uses a properly
/// initialized Threaded instance with a real allocator. Without this, the default
/// `global_single_threaded` has `.allocator = .failing` and IO operations fail.
pub const std_options_debug_threaded_io: *std.Io.Threaded = &debug_threaded_io_instance;

// Test spec types

const OptMode = enum(u8) {
    interpreter,
    dev,
    size,
    speed,

    fn cliName(self: OptMode) []const u8 {
        return switch (self) {
            .interpreter => "interpreter",
            .dev => "dev",
            .size => "size",
            .speed => "speed",
        };
    }
};

const base_test_opts = [_]OptMode{ .interpreter, .dev };
const llvm_test_opts = [_]OptMode{ .size, .speed };

/// A single CLI test operation — one matrix cell of work.
const CliTestSpec = struct {
    /// Unique id within this runner invocation. This keeps generated binary
    /// names distinct even on hosts that run all specs in the same process.
    id: usize,
    /// Human-readable name, e.g. "test/fx/hello_world.roc [dev]"
    name: []const u8,
    /// Path to .roc file (relative to project root)
    roc_file: []const u8,
    /// Platform name (for display grouping)
    platform: []const u8,
    /// Execution mode passed through `--opt`.
    opt: OptMode,
    /// What kind of test to run
    test_kind: TestKind,

    const TestKind = union(enum) {
        /// Build natively and run; check exit code 0
        native_run,
        /// Build natively, run with --test <spec>; check exit code 0
        io_spec: []const u8,
    };
};

// Spec generation

fn buildTestSpecs(allocator: Allocator, filters: []const []const u8, include_llvm: bool) ![]const CliTestSpec {
    var specs: std.ArrayListUnmanaged(CliTestSpec) = .empty;

    for (&platform_config.platforms) |platform| {
        for (&base_test_opts) |opt| {
            try appendPlatformSpecs(allocator, &specs, platform, opt, filters);
        }
        if (include_llvm) {
            for (&llvm_test_opts) |opt| {
                try appendPlatformSpecs(allocator, &specs, platform, opt, filters);
            }
        }
    }

    return specs.toOwnedSlice(allocator);
}

fn appendPlatformSpecs(
    allocator: Allocator,
    specs: *std.ArrayListUnmanaged(CliTestSpec),
    platform: platform_config.PlatformConfig,
    opt: OptMode,
    filters: []const []const u8,
) !void {
    switch (platform.test_apps) {
        .single => |app_name| {
            const roc_file = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ platform.base_dir, app_name });
            const name = try fmtTestName(allocator, roc_file, opt);
            if (matchesFilters(name, roc_file, filters)) {
                try specs.append(allocator, .{
                    .id = specs.items.len,
                    .name = name,
                    .roc_file = roc_file,
                    .platform = platform.name,
                    .opt = opt,
                    .test_kind = .native_run,
                });
            }
        },
        .spec_list => |io_specs| {
            for (io_specs) |spec| {
                if (skipIoSpecOnHost(spec)) continue;

                const name = try fmtTestName(allocator, spec.roc_file, opt);
                if (matchesFilters(name, spec.roc_file, filters)) {
                    try specs.append(allocator, .{
                        .id = specs.items.len,
                        .name = name,
                        .roc_file = spec.roc_file,
                        .platform = platform.name,
                        .opt = opt,
                        .test_kind = .{ .io_spec = spec.io_spec },
                    });
                }
            }
        },
        .simple_list => |simple_specs| {
            for (simple_specs) |spec| {
                const name = try fmtTestName(allocator, spec.roc_file, opt);
                if (matchesFilters(name, spec.roc_file, filters)) {
                    try specs.append(allocator, .{
                        .id = specs.items.len,
                        .name = name,
                        .roc_file = spec.roc_file,
                        .platform = platform.name,
                        .opt = opt,
                        .test_kind = .native_run,
                    });
                }
            }
        },
    }
}

fn skipIoSpecOnHost(spec: @import("fx_test_specs.zig").TestSpec) bool {
    if (spec.skip) return true;
    return spec.skip_on_windows and builtin.os.tag == .windows;
}

fn fmtTestName(allocator: Allocator, roc_file: []const u8, opt: OptMode) ![]const u8 {
    return std.fmt.allocPrint(allocator, "{s} [{s}]", .{ roc_file, opt.cliName() });
}

fn matchesFilters(name: []const u8, roc_file: []const u8, filters: []const []const u8) bool {
    if (filters.len == 0) return true;
    for (filters) |f| {
        if (std.mem.find(u8, name, f) != null) return true;
        if (std.mem.find(u8, roc_file, f) != null) return true;
    }
    return false;
}

// Wire protocol (child -> parent via pipe)

const TestStatus = enum(u8) {
    pass = 0,
    build_failed = 1,
    run_failed = 2,
    timeout = 3,
    crash = 4,
    infra_error = 5,
    skip = 6,
};

const TestPhase = enum(u8) {
    setup = 0,
    build = 1,
    run = 2,
    cleanup = 3,
    harness = 4,
};

const WireHeader = extern struct {
    status: u8,
    phase: u8,
    duration_ns: u64,
    build_ns: u64,
    run_ns: u64,
    exit_code: u32,
    stderr_len: u32,
    stdout_len: u32,
    message_len: u32,
};

const TestResult = struct {
    status: TestStatus = .crash,
    phase: TestPhase = .harness,
    duration_ns: u64 = 0,
    build_ns: u64 = 0,
    run_ns: u64 = 0,
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
        .phase = @intFromEnum(result.phase),
        .duration_ns = result.duration_ns,
        .build_ns = result.build_ns,
        .run_ns = result.run_ns,
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

/// Streamed variant for persistent worker mode: writes a `u32` length prefix
/// before the wire bytes so the parent can frame multiple results sharing
/// the same stdout pipe.
fn serializeResultStreamed(fd: posix.fd_t, result: TestResult) void {
    const stderr_data = result.stderr_capture orelse "";
    const stdout_data = result.stdout_capture orelse "";
    const message_data = result.message orelse "";

    const max_capture = 8192;
    const stderr_out = stderr_data[0..@min(stderr_data.len, max_capture)];
    const stdout_out = stdout_data[0..@min(stdout_data.len, max_capture)];
    const message_out = message_data[0..@min(message_data.len, max_capture)];

    const header = WireHeader{
        .status = @intFromEnum(result.status),
        .phase = @intFromEnum(result.phase),
        .duration_ns = result.duration_ns,
        .build_ns = result.build_ns,
        .run_ns = result.run_ns,
        .exit_code = result.exit_code,
        .stderr_len = @intCast(stderr_out.len),
        .stdout_len = @intCast(stdout_out.len),
        .message_len = @intCast(message_out.len),
    };

    const length: u32 = @intCast(@sizeOf(WireHeader) + stderr_out.len + stdout_out.len + message_out.len);
    harness.writeAll(fd, std.mem.asBytes(&length));
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
        .phase = @enumFromInt(header.phase),
        .duration_ns = header.duration_ns,
        .build_ns = header.build_ns,
        .run_ns = header.run_ns,
        .exit_code = header.exit_code,
        .stderr_capture = stderr_capture,
        .stdout_capture = stdout_capture,
        .message = message,
    };
}

// Child test execution

var roc_binary_path: []const u8 = "";
var project_root_path: []const u8 = "";

fn deleteIfExists(path: []const u8) !void {
    std.Io.Dir.cwd().deleteFile(std.Options.debug_io, path) catch |err| switch (err) {
        error.FileNotFound => {},
        else => return err,
    };
}

fn deleteOutputArtifacts(allocator: Allocator, output_name: []const u8) !void {
    try deleteIfExists(output_name);

    if (comptime builtin.os.tag == .windows) {
        const exe_name = try std.fmt.allocPrint(allocator, "{s}.exe", .{output_name});
        defer allocator.free(exe_name);
        try deleteIfExists(exe_name);

        const pdb_name = try std.fmt.allocPrint(allocator, "{s}.pdb", .{output_name});
        defer allocator.free(pdb_name);
        try deleteIfExists(pdb_name);
    }
}

fn absoluteFromProjectRoot(allocator: Allocator, path: []const u8) ![]u8 {
    if (std.fs.path.isAbsolute(path)) {
        return allocator.dupe(u8, path);
    }
    return std.fs.path.join(allocator, &.{ project_root_path, path });
}

fn runSingleTest(allocator: Allocator, spec: CliTestSpec, timeout_ms: u64) TestResult {
    var timer = harness.Timer.start() catch return .{ .status = .infra_error, .phase = .setup, .message = "no clock" };

    const dirs = util.createIsolatedTestDirs(allocator) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to create test directories" };
    defer dirs.deinit(allocator);

    const roc_file = absoluteFromProjectRoot(allocator, spec.roc_file) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to resolve Roc file path" };

    const output_name = std.fs.path.join(allocator, &.{ dirs.work_dir, "app" }) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to allocate output path" };

    deleteOutputArtifacts(allocator, output_name) catch |err| {
        const msg = std.fmt.allocPrint(allocator, "failed to remove stale output file: {}", .{err}) catch "failed to remove stale output file";
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = msg };
    };

    const environ: std.process.Environ = if (@import("builtin").os.tag == .windows) .{
        .block = .global,
    } else blk: {
        const env_ptr: [*:null]const ?[*:0]const u8 = @ptrCast(std.c.environ);
        break :blk .{ .block = .{ .slice = std.mem.sliceTo(env_ptr, null) } };
    };
    var env_map = environ.createMap(allocator) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to copy environment" };
    defer env_map.deinit();
    env_map.put("ROC_CACHE_DIR", dirs.roc_cache_dir) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to set ROC_CACHE_DIR" };
    env_map.put("XDG_CACHE_HOME", dirs.roc_cache_dir) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to set XDG_CACHE_HOME" };
    env_map.put("ZIG_LOCAL_CACHE_DIR", dirs.zig_local_cache_dir) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to set ZIG_LOCAL_CACHE_DIR" };

    const result = switch (spec.opt) {
        .interpreter => runInterpreterTest(allocator, spec, roc_file, &env_map, dirs.work_dir, &timer, timeout_ms),
        .dev, .size, .speed => runCompiledTest(allocator, spec, roc_file, output_name, &env_map, dirs.work_dir, &timer, timeout_ms),
    };

    if (result.status == .pass) {
        util.cleanupTestWorkDir(dirs.work_dir);
        return result;
    }
    return addPreservedWorkDirMessage(allocator, result, dirs.work_dir);
}

fn runInterpreterTest(
    allocator: Allocator,
    spec: CliTestSpec,
    roc_file: []const u8,
    env_map: *const std.process.Environ.Map,
    work_dir: []const u8,
    timer: *harness.Timer,
    timeout_ms: u64,
) TestResult {
    const opt_arg = std.fmt.allocPrint(allocator, "--opt={s}", .{spec.opt.cliName()}) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to allocate opt arg" };

    var argv_buf: [5][]const u8 = undefined;
    var argc: usize = 0;
    argv_buf[argc] = roc_binary_path;
    argc += 1;
    argv_buf[argc] = "run";
    argc += 1;
    argv_buf[argc] = opt_arg;
    argc += 1;
    switch (spec.test_kind) {
        .native_run => {},
        .io_spec => |io_spec| {
            const test_arg = std.fmt.allocPrint(allocator, "--test={s}", .{io_spec}) catch
                return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to allocate IO spec arg" };
            argv_buf[argc] = test_arg;
            argc += 1;
        },
    }
    argv_buf[argc] = roc_file;
    argc += 1;

    var run_timer = harness.Timer.start() catch return .{ .status = .infra_error, .phase = .run, .duration_ns = timer.read(), .message = "no clock" };
    const run_result = util.runChildWithTimeout(allocator, argv_buf[0..argc], .{
        .cwd = work_dir,
        .env_map = env_map,
        .max_output_bytes = 10 * 1024 * 1024,
        .timeout_ms = timeout_ms,
    }) catch |err| {
        const msg = std.fmt.allocPrint(allocator, "run spawn error: {}", .{err}) catch "run spawn error";
        return .{ .status = .infra_error, .phase = .run, .duration_ns = timer.read(), .run_ns = run_timer.read(), .message = msg };
    };
    const run_ns = run_timer.read();
    return resultFromProcess(run_result, timer, .run, 0, run_ns, "run failed");
}

fn runCompiledTest(
    allocator: Allocator,
    spec: CliTestSpec,
    roc_file: []const u8,
    output_name: []const u8,
    env_map: *const std.process.Environ.Map,
    work_dir: []const u8,
    timer: *harness.Timer,
    timeout_ms: u64,
) TestResult {
    const output_arg = std.fmt.allocPrint(allocator, "--output={s}", .{output_name}) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to allocate output arg" };
    const opt_arg = std.fmt.allocPrint(allocator, "--opt={s}", .{spec.opt.cliName()}) catch
        return .{ .status = .infra_error, .phase = .setup, .duration_ns = timer.read(), .message = "failed to allocate opt arg" };

    const build_argv = &[_][]const u8{ roc_binary_path, "build", output_arg, opt_arg, roc_file };

    var build_timer = harness.Timer.start() catch return .{ .status = .infra_error, .phase = .build, .duration_ns = timer.read(), .message = "no clock" };
    const build_result = util.runChildWithTimeout(allocator, build_argv, .{
        .cwd = work_dir,
        .env_map = env_map,
        .max_output_bytes = 10 * 1024 * 1024,
        .timeout_ms = timeout_ms,
    }) catch |err| {
        const msg = std.fmt.allocPrint(allocator, "build spawn error: {}", .{err}) catch "build spawn error";
        return .{ .status = .infra_error, .phase = .build, .duration_ns = timer.read(), .build_ns = build_timer.read(), .message = msg };
    };
    const build_ns = build_timer.read();
    if (!processSucceeded(build_result.term)) {
        return resultFromProcess(build_result, timer, .build, build_ns, 0, "build failed");
    }

    if (!builtOutputExists(allocator, output_name)) {
        return .{ .status = .build_failed, .phase = .build, .duration_ns = timer.read(), .build_ns = build_ns, .message = "build succeeded but output file was not created" };
    }

    var run_argv_buf: [3][]const u8 = undefined;
    var argc: usize = 0;
    run_argv_buf[argc] = output_name;
    argc += 1;
    switch (spec.test_kind) {
        .native_run => {},
        .io_spec => |io_spec| {
            run_argv_buf[argc] = "--test";
            argc += 1;
            run_argv_buf[argc] = io_spec;
            argc += 1;
        },
    }

    var run_timer = harness.Timer.start() catch return .{ .status = .infra_error, .phase = .run, .duration_ns = timer.read(), .build_ns = build_ns, .message = "no clock" };
    const run_result = util.runChildWithTimeout(allocator, run_argv_buf[0..argc], .{
        .cwd = work_dir,
        .max_output_bytes = 10 * 1024 * 1024,
        .timeout_ms = timeout_ms,
    }) catch |err| {
        const msg = std.fmt.allocPrint(allocator, "run spawn error: {}", .{err}) catch "run spawn error";
        return .{ .status = .infra_error, .phase = .run, .duration_ns = timer.read(), .build_ns = build_ns, .run_ns = run_timer.read(), .message = msg };
    };
    const run_ns = run_timer.read();
    return resultFromProcess(run_result, timer, .run, build_ns, run_ns, "run failed");
}

fn builtOutputExists(allocator: Allocator, output_name: []const u8) bool {
    std.Io.Dir.cwd().access(std.Options.debug_io, output_name, .{}) catch {
        if (builtin.os.tag == .windows) {
            const exe_name = std.fmt.allocPrint(allocator, "{s}.exe", .{output_name}) catch return false;
            defer allocator.free(exe_name);
            std.Io.Dir.cwd().access(std.Options.debug_io, exe_name, .{}) catch return false;
        } else {
            return false;
        }
    };
    return true;
}

fn resultFromProcess(
    result: std.process.RunResult,
    timer: *harness.Timer,
    phase: TestPhase,
    build_ns: u64,
    run_ns: u64,
    fail_msg: []const u8,
) TestResult {
    if (processTimedOut(result.stderr)) {
        return .{
            .status = .timeout,
            .phase = phase,
            .duration_ns = timer.read(),
            .build_ns = build_ns,
            .run_ns = run_ns,
            .exit_code = exitCode(result.term),
            .stderr_capture = result.stderr,
            .stdout_capture = result.stdout,
            .message = "child command timed out",
        };
    }
    if (hasMemoryErrors(result.stderr)) |mem_msg| {
        return .{
            .status = if (phase == .build) .build_failed else .run_failed,
            .phase = phase,
            .duration_ns = timer.read(),
            .build_ns = build_ns,
            .run_ns = run_ns,
            .exit_code = exitCode(result.term),
            .stderr_capture = result.stderr,
            .stdout_capture = result.stdout,
            .message = mem_msg,
        };
    }
    switch (result.term) {
        .exited => |code| {
            if (code == 0) {
                return .{ .status = .pass, .phase = phase, .duration_ns = timer.read(), .build_ns = build_ns, .run_ns = run_ns };
            }
            return .{
                .status = if (phase == .build) .build_failed else .run_failed,
                .phase = phase,
                .duration_ns = timer.read(),
                .build_ns = build_ns,
                .run_ns = run_ns,
                .exit_code = @intCast(code),
                .stderr_capture = result.stderr,
                .stdout_capture = result.stdout,
                .message = fail_msg,
            };
        },
        .signal => {
            return .{
                .status = .crash,
                .phase = phase,
                .duration_ns = timer.read(),
                .build_ns = build_ns,
                .run_ns = run_ns,
                .exit_code = exitCode(result.term),
                .stderr_capture = result.stderr,
                .stdout_capture = result.stdout,
                .message = fail_msg,
            };
        },
        else => {
            return .{
                .status = .crash,
                .phase = phase,
                .duration_ns = timer.read(),
                .build_ns = build_ns,
                .run_ns = run_ns,
                .exit_code = exitCode(result.term),
                .stderr_capture = result.stderr,
                .stdout_capture = result.stdout,
                .message = fail_msg,
            };
        },
    }
}

fn processSucceeded(term: std.process.Child.Term) bool {
    return switch (term) {
        .exited => |code| code == 0,
        else => false,
    };
}

fn processTimedOut(stderr: []const u8) bool {
    return std.mem.find(u8, stderr, "child command timed out") != null;
}

fn statusLabel(status: TestStatus) []const u8 {
    return switch (status) {
        .pass => "passed",
        .build_failed => "build failed",
        .run_failed => "run failed",
        .timeout => "timed out",
        .crash => "crashed",
        .infra_error => "infrastructure error",
        .skip => "skipped",
    };
}

fn phaseLabel(phase: TestPhase) []const u8 {
    return switch (phase) {
        .setup => "setup",
        .build => "build",
        .run => "run",
        .cleanup => "cleanup",
        .harness => "harness",
    };
}

fn addPreservedWorkDirMessage(allocator: Allocator, result: TestResult, work_dir: []const u8) TestResult {
    var updated = result;
    const prefix = result.message orelse statusLabel(result.status);
    updated.message = std.fmt.allocPrint(allocator, "{s}; preserved work dir: {s}", .{ prefix, work_dir }) catch result.message;
    return updated;
}

fn exitCode(term: std.process.Child.Term) u32 {
    return switch (term) {
        .exited => |code| @intCast(code),
        .signal => |sig| @as(u32, @intFromEnum(sig)) | 0x80000000,
        else => 0xFFFFFFFF,
    };
}

fn hasMemoryErrors(stderr: []const u8) ?[]const u8 {
    if (std.mem.find(u8, stderr, "error(gpa):") != null) return "memory error detected";
    if (std.mem.find(u8, stderr, "allocation(s) not freed") != null) return "memory leak detected";
    return null;
}

/// Build argv used by the Windows ChildProcessPool to spawn worker copies of
/// this runner. Starts with `selfExePath`, then preserves every original arg
/// *except* `--worker N` / `--worker-backend NAME` (stripped to avoid
/// duplication when the harness appends `--worker <idx>` per spawn).
fn buildCliWorkerArgvTemplate(arena: Allocator, process_args: std.process.Args) ![]const []const u8 {
    var self_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const self_path_len = try std.process.executablePath(std.Options.debug_io, &self_path_buf);
    const self_path = try arena.dupe(u8, self_path_buf[0..self_path_len]);

    const raw = try process_args.toSlice(arena);
    const original_args: []const []const u8 = @ptrCast(raw);

    var argv: std.ArrayListUnmanaged([]const u8) = .empty;
    try argv.append(arena, self_path);

    var i: usize = 1;
    while (i < original_args.len) : (i += 1) {
        const arg = original_args[i];
        if (std.mem.eql(u8, arg, "--worker") or std.mem.eql(u8, arg, "--worker-backend")) {
            i += 1;
            continue;
        }
        try argv.append(arena, arg);
    }

    return try argv.toOwnedSlice(arena);
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
        .phase = result.phase,
        .duration_ns = result.duration_ns,
        .build_ns = result.build_ns,
        .run_ns = result.run_ns,
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
    .windows_persistent_workers = false,
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
    const status_count = 7;
    const opt_count = 4;
    const all_opts = [_]OptMode{ .interpreter, .dev, .size, .speed };
    var status_counts = [_]usize{0} ** status_count;
    var opt_counts = [_]usize{0} ** opt_count;
    var opt_failures = [_]usize{0} ** opt_count;

    for (tests, 0..) |tc, i| {
        const r = results[i];
        const ms = harness.nsToMs(r.duration_ns);
        status_counts[@intFromEnum(r.status)] += 1;
        opt_counts[@intFromEnum(tc.opt)] += 1;
        if (r.status != .pass and r.status != .skip) {
            opt_failures[@intFromEnum(tc.opt)] += 1;
        }

        switch (r.status) {
            .pass => {
                if (verbose) std.debug.print("  PASS  {s}  ({d:.1}ms)\n", .{ tc.name, ms });
            },
            .build_failed, .run_failed, .timeout, .crash, .infra_error => printProblemResult(tc, r, ms),
            .skip => {
                if (verbose) std.debug.print("  SKIP  {s}\n", .{tc.name});
            },
        }
    }

    const wall_ms = harness.nsToMs(wall_ns);
    std.debug.print("\n{d} passed", .{status_counts[@intFromEnum(TestStatus.pass)]});
    if (status_counts[@intFromEnum(TestStatus.build_failed)] > 0) std.debug.print(", {d} build failed", .{status_counts[@intFromEnum(TestStatus.build_failed)]});
    if (status_counts[@intFromEnum(TestStatus.run_failed)] > 0) std.debug.print(", {d} run failed", .{status_counts[@intFromEnum(TestStatus.run_failed)]});
    if (status_counts[@intFromEnum(TestStatus.crash)] > 0) std.debug.print(", {d} crashed", .{status_counts[@intFromEnum(TestStatus.crash)]});
    if (status_counts[@intFromEnum(TestStatus.timeout)] > 0) std.debug.print(", {d} timed out", .{status_counts[@intFromEnum(TestStatus.timeout)]});
    if (status_counts[@intFromEnum(TestStatus.infra_error)] > 0) std.debug.print(", {d} infra errors", .{status_counts[@intFromEnum(TestStatus.infra_error)]});
    if (status_counts[@intFromEnum(TestStatus.skip)] > 0) std.debug.print(", {d} skipped", .{status_counts[@intFromEnum(TestStatus.skip)]});
    std.debug.print(" ({d} total) in {d:.0}ms using {d} worker(s)\n", .{ tests.len, wall_ms, max_children });

    std.debug.print("\n=== Backend Matrix ===\n", .{});
    for (all_opts) |opt| {
        const opt_idx = @intFromEnum(opt);
        if (opt_counts[opt_idx] == 0) continue;
        std.debug.print("  {s:<11} {d:>4} run, {d:>4} failed\n", .{ opt.cliName(), opt_counts[opt_idx], opt_failures[opt_idx] });
    }

    // Timing summary
    var durations: std.ArrayListUnmanaged(u64) = .empty;
    var build_durations: std.ArrayListUnmanaged(u64) = .empty;
    var run_durations: std.ArrayListUnmanaged(u64) = .empty;
    var opt_durations = [_]std.ArrayListUnmanaged(u64){ .empty, .empty, .empty, .empty };
    defer durations.deinit(gpa);
    defer build_durations.deinit(gpa);
    defer run_durations.deinit(gpa);
    defer {
        for (&opt_durations) |*list| list.deinit(gpa);
    }
    for (results) |r| {
        if (r.duration_ns > 0) durations.append(gpa, r.duration_ns) catch continue;
        if (r.build_ns > 0) build_durations.append(gpa, r.build_ns) catch {};
        if (r.run_ns > 0) run_durations.append(gpa, r.run_ns) catch {};
    }
    for (tests, results) |tc, r| {
        if (r.duration_ns > 0) opt_durations[@intFromEnum(tc.opt)].append(gpa, r.duration_ns) catch {};
    }
    if (harness.computeTimingStats(durations.items)) |_| {
        std.debug.print("\n=== Timing Summary (ms) ===\n", .{});
        harness.printStatsHeader();
        harness.printStatsRow("total", harness.computeTimingStats(durations.items));
        harness.printStatsRow("build", harness.computeTimingStats(build_durations.items));
        harness.printStatsRow("run", harness.computeTimingStats(run_durations.items));
        for (all_opts) |opt| {
            harness.printStatsRow(opt.cliName(), harness.computeTimingStats(opt_durations[@intFromEnum(opt)].items));
        }
    }

    var duration_arr = gpa.alloc(u64, results.len) catch return;
    defer gpa.free(duration_arr);
    for (results, 0..) |r, i| duration_arr[i] = r.duration_ns;
    harness.printSlowestN(CliTestSpec, tests, duration_arr, 5, gpa, getTestName);
}

fn printProblemResult(tc: CliTestSpec, r: TestResult, ms: f64) void {
    std.debug.print("  {s:<12} {s}  ({d:.1}ms, phase={s})\n", .{ statusLabel(r.status), tc.name, ms, phaseLabel(r.phase) });
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
    std.debug.print("        Repro: zig build test-platforms -- --test-filter \"{s}\"\n\n", .{test_name});
}

// Main

fn printUsage() void {
    std.debug.print(
        \\Usage: parallel_cli_runner <roc_binary> [options]
        \\
        \\Options:
        \\  --filter <pattern>   Run tests matching pattern (repeatable)
        \\  --threads <N>        Max concurrent workers (default: CPU count)
        \\  --timeout <ms>       Per-test timeout in ms (default: 120000)
        \\  --include-llvm       Include size and speed LLVM backend jobs
        \\  --verbose            Show PASS results with timing
        \\
    , .{});
}

/// Entry point for the parallel CLI test runner.
pub fn main(init: std.process.Init) !void {
    // Initialize the debug IO with a real allocator so std.Options.debug_io
    // can spawn processes, create directories, delete files, etc.
    debug_threaded_io_instance = .init(init.gpa, .{
        .argv0 = .init(init.minimal.args),
        .environ = init.minimal.environ,
    });
    defer debug_threaded_io_instance.deinit();

    var gpa_impl: std.heap.DebugAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var spec_arena = std.heap.ArenaAllocator.init(gpa);
    defer spec_arena.deinit();

    const args = try harness.parseStandardArgs(spec_arena.allocator(), init.minimal.args);

    if (args.help_requested) {
        printUsage();
        return;
    }

    if (args.positional.len < 1) {
        printUsage();
        std.process.exit(1);
    }

    project_root_path = try std.Io.Dir.cwd().realPathFileAlloc(std.Options.debug_io, ".", spec_arena.allocator());
    roc_binary_path = if (std.fs.path.isAbsolute(args.positional[0]))
        args.positional[0]
    else
        try std.fs.path.join(spec_arena.allocator(), &.{ project_root_path, args.positional[0] });

    const tests = try buildTestSpecs(spec_arena.allocator(), args.filters, args.include_llvm);
    if (tests.len == 0) return;

    // Worker mode: parent spawned us with `--worker <idx>` to run a single
    // test and serialize the result to stdout. Used on Windows where the
    // harness runs N worker processes in parallel instead of forking.
    if (args.worker_index) |idx| {
        if (idx >= tests.len) std.process.exit(2);
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const result = runSingleTest(arena.allocator(), tests[idx], args.timeout_ms);
        serializeResult(std.Io.File.stdout().handle, result);
        return;
    }

    // Persistent worker mode: read test indices from stdin (one decimal per
    // line), run each, write a u32-length-prefixed result to stdout, loop
    // until stdin EOFs. Amortizes the per-Child process-boot cost across
    // many tests on the same worker. Without this branch, a worker spawned
    // with `--worker-stream` would fall through to the parent path below
    // and reentrantly spawn its own pool of workers — fork-bombing the box.
    if (args.worker_stream) {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const stdin_handle = std.Io.File.stdin().handle;
        const stdout_handle = std.Io.File.stdout().handle;

        var line_buf: [32]u8 = undefined;
        outer: while (true) {
            var line_len: usize = 0;
            while (true) {
                if (line_len >= line_buf.len) break :outer;
                const n = harness.posixRead(stdin_handle, line_buf[line_len .. line_len + 1]) catch break :outer;
                if (n == 0) break :outer;
                if (line_buf[line_len] == '\n') break;
                line_len += 1;
            }
            const idx = std.fmt.parseInt(usize, line_buf[0..line_len], 10) catch continue;
            if (idx >= tests.len) continue;

            _ = arena.reset(.retain_capacity);
            const result = runSingleTest(arena.allocator(), tests[idx], args.timeout_ms);
            serializeResultStreamed(stdout_handle, result);
        }
        return;
    }

    const cpu_count = std.Thread.getCpuCount() catch 4;
    const max_children = args.max_threads orelse @min(cpu_count, tests.len);

    std.debug.print("=== CLI Test Runner ===\n", .{});
    std.debug.print("{d} tests, {d} workers, {d}s timeout", .{ tests.len, max_children, args.timeout_ms / 1000 });
    if (args.include_llvm) {
        std.debug.print(", backends: interpreter, dev, size, speed\n\n", .{});
    } else {
        std.debug.print(", backends: interpreter, dev\n\n", .{});
    }

    const results = try gpa.alloc(TestResult, tests.len);
    defer gpa.free(results);
    @memset(results, .{ .status = .crash });

    // Build a worker_argv_template so Windows can re-invoke this binary as a
    // single-test Child worker. On POSIX it's unused (fork path doesn't
    // re-exec). Always pass the positional `roc_binary` path through so the
    // child uses the same binary.
    const worker_argv_template = try buildCliWorkerArgvTemplate(spec_arena.allocator(), init.minimal.args);

    var wall_timer = harness.Timer.start() catch @panic("no clock");
    Pool.run(init.io, tests, results, max_children, args.timeout_ms, gpa, worker_argv_template);
    const wall_ns = wall_timer.read();

    printResults(tests, results, args.verbose, gpa, wall_ns, max_children);

    for (results) |r| {
        if (r.stderr_capture) |s| gpa.free(s);
        if (r.stdout_capture) |s| gpa.free(s);
        if (r.message) |m| gpa.free(m);
    }

    for (results) |r| {
        switch (r.status) {
            .build_failed, .run_failed, .crash, .timeout, .infra_error => std.process.exit(1),
            else => {},
        }
    }
}
