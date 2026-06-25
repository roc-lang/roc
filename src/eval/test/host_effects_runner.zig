//! Parallel runtime host-effects test runner.
//!
//! Runs raw Roc source through the normal compiler pipeline and compares the
//! exact host callback traffic produced by the interpreter and dev backend:
//!
//! - `dbg`
//! - `expect_failed`
//! - `crashed`
//!
//! The observable contract is intentionally limited to what a production host
//! sees through `host_abi`: callback kind, raw UTF-8 payload bytes, event
//! order, and whether execution returned or terminated via crash.

const std = @import("std");
const Allocator = std.mem.Allocator;
const posix = std.posix;
const eval = @import("eval");
const base = @import("base");
const harness = @import("test_harness");

const helpers = eval.test_helpers;
const RuntimeHostEnv = eval.RuntimeHostEnv;
const LoweredProgram = helpers.LoweredProgram;
const Interpreter = eval.Interpreter;
const backend = @import("backend");
const HostLirCodeGen = backend.HostLirCodeGen;
const ExecutableMemory = backend.ExecutableMemory;
const collections = @import("collections");

/// Public struct `TestCase`.
pub const TestCase = struct {
    name: []const u8,
    source: []const u8,
    source_kind: helpers.SourceKind = .expr,
    imports: []const helpers.ModuleSource = &.{},
    expected_events: []const ExpectedEvent,
    expected_termination: RuntimeHostEnv.Termination,
    expected_live_allocations: ?u32 = null,
    skip: Skip = .{},

    pub const ExpectedEvent = union(enum) {
        dbg: []const u8,
        dbg_contains: []const u8,
        dbg_any: void,
        expect_failed: []const u8,
        crashed: []const u8,
    };

    pub const Skip = packed struct {
        interpreter: bool = false,
        dev: bool = false,
    };
};

const host_effects_tests = @import("host_effects_tests.zig");

const Timer = harness.Timer;
const has_fork = harness.has_fork;

const NUM_BACKENDS = 2;
const BACKEND_NAMES = [NUM_BACKENDS][]const u8{ "interpreter", "dev" };
const DEV_BACKEND_IMPLEMENTED = eval.backendAvailable(.dev);

const BackendEvalError = helpers.TestHelperError || Interpreter.Error || Allocator.Error || error{DevBackendUnavailable};
const StatsJsonError = Allocator.Error || std.Io.Dir.AccessError || std.Io.Dir.CreateDirPathError || std.Io.File.OpenError || std.Io.File.Writer.Error;
const RunnerMainError = StatsJsonError || std.process.Args.ToSliceError || Allocator.Error;

const BackendStatus = enum(u8) {
    pass,
    wrong,
    fail,
    crash,
    skip,
    not_implemented,
};

const BackendDetail = struct {
    status: BackendStatus,
    run: ?RuntimeHostEnv.RecordedRun = null,
    message: ?[]const u8 = null,
    duration_ns: u64 = 0,

    fn deinit(self: *BackendDetail, allocator: std.mem.Allocator) void {
        if (self.run) |*run| run.deinit(allocator);
        if (self.message) |msg| allocator.free(msg);
    }
};

const TestOutcome = struct {
    status: Status,
    message: ?[]const u8 = null,
    backends: [NUM_BACKENDS]BackendDetail = [_]BackendDetail{
        .{ .status = .skip },
        .{ .status = .skip },
    },

    const Status = enum(u8) {
        pass,
        fail,
        crash,
        skip,
        timeout,
    };
};

const TestResult = struct {
    status: TestOutcome.Status,
    message: ?[]const u8,
    duration_ns: u64,
    backends: [NUM_BACKENDS]BackendDetail,

    fn deinit(self: *TestResult, allocator: std.mem.Allocator) void {
        if (self.message) |msg| allocator.free(msg);
        for (&self.backends) |*backend_detail| backend_detail.deinit(allocator);
    }
};

const BackendRunHeader = extern struct {
    termination: u8,
    event_count: u32,
    allocation_count: u32,
};

const EventHeader = extern struct {
    kind: u8,
    len: u32,
};

const WireHeader = extern struct {
    status: u8,
    duration_ns: u64,
    message_len: u32,
    backend_statuses: [NUM_BACKENDS]u8,
    backend_durations: [NUM_BACKENDS]u64,
    backend_message_lens: [NUM_BACKENDS]u32,
    backend_run_lens: [NUM_BACKENDS]u32,
};

const BackendEvalFn = *const fn (std.mem.Allocator, *const LoweredProgram) BackendEvalError!RuntimeHostEnv.RecordedRun;

const ForkResult = union(enum) {
    success: RuntimeHostEnv.RecordedRun,
    child_error: []const u8,
    signal_death: u8,
    fork_failed: void,
};

fn readWireValue(comptime T: type, buf: []const u8, offset: *usize) ?T {
    const end = offset.* + @sizeOf(T);
    if (end > buf.len) return null;

    var value: T = undefined;
    @memcpy(std.mem.asBytes(&value), buf[offset.*..end]);
    offset.* = end;
    return value;
}

fn appendEncodedRun(
    allocator: std.mem.Allocator,
    out: *std.ArrayListUnmanaged(u8),
    run: RuntimeHostEnv.RecordedRun,
) Allocator.Error!void {
    const header: BackendRunHeader = .{
        .termination = @intFromEnum(run.termination),
        .event_count = @intCast(run.events.len),
        .allocation_count = run.allocation_count,
    };
    try out.appendSlice(allocator, std.mem.asBytes(&header));
    for (run.events) |event| {
        const kind: u8 = switch (event) {
            .dbg => 0,
            .expect_failed => 1,
            .crashed => 2,
        };
        const payload = event.bytes();
        const event_header: EventHeader = .{
            .kind = kind,
            .len = @intCast(payload.len),
        };
        try out.appendSlice(allocator, std.mem.asBytes(&event_header));
        try out.appendSlice(allocator, payload);
    }
}

fn decodeRun(buf: []const u8, gpa: std.mem.Allocator) ?RuntimeHostEnv.RecordedRun {
    var offset: usize = 0;
    const header = readWireValue(BackendRunHeader, buf, &offset) orelse return null;

    var events = gpa.alloc(RuntimeHostEnv.HostEvent, header.event_count) catch return null;
    errdefer gpa.free(events);

    for (events, 0..) |*event, i| {
        const event_header = readWireValue(EventHeader, buf, &offset) orelse {
            for (events[0..i]) |*prev| prev.deinit(gpa);
            gpa.free(events);
            return null;
        };
        const end = offset + event_header.len;
        if (end > buf.len) {
            for (events[0..i]) |*prev| prev.deinit(gpa);
            gpa.free(events);
            return null;
        }
        const payload = gpa.dupe(u8, buf[offset..end]) catch {
            for (events[0..i]) |*prev| prev.deinit(gpa);
            gpa.free(events);
            return null;
        };
        offset = end;
        event.* = switch (event_header.kind) {
            0 => .{ .dbg = payload },
            1 => .{ .expect_failed = payload },
            2 => .{ .crashed = payload },
            else => {
                gpa.free(payload);
                for (events[0..i]) |*prev| prev.deinit(gpa);
                gpa.free(events);
                return null;
            },
        };
    }

    return .{
        .termination = @enumFromInt(header.termination),
        .events = events,
        .allocation_count = header.allocation_count,
    };
}

fn serializeRun(fd: posix.fd_t, run: RuntimeHostEnv.RecordedRun) void {
    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(base.defaultGpa());
    appendEncodedRun(base.defaultGpa(), &buf, run) catch return;
    harness.writeAll(fd, buf.items);
}

fn forkAndEval(eval_fn: BackendEvalFn, lowered: *const LoweredProgram) ForkResult {
    if (comptime !has_fork) {
        const result = eval_fn(base.defaultGpa(), lowered) catch |err| {
            return .{ .child_error = @errorName(err) };
        };
        return .{ .success = result };
    }

    const pipe_fds = harness.pipe() catch return .{ .fork_failed = {} };
    const pipe_read = pipe_fds[0];
    const pipe_write = pipe_fds[1];

    const fork_result = harness.fork() catch {
        harness.closeFd(pipe_read);
        harness.closeFd(pipe_write);
        return .{ .fork_failed = {} };
    };

    if (fork_result == 0) {
        harness.closeFd(pipe_read);
        var child_arena = collections.SingleThreadArena.init(base.defaultGpa());
        const child_alloc = child_arena.allocator();

        const run = eval_fn(child_alloc, lowered) catch |err| {
            const name = @errorName(err);
            harness.writeAll(pipe_write, name);
            harness.closeFd(pipe_write);
            std.c._exit(2);
        };
        serializeRun(pipe_write, run);
        harness.closeFd(pipe_write);
        std.c._exit(0);
    }

    harness.closeFd(pipe_write);

    var result_buf: std.ArrayListUnmanaged(u8) = .empty;
    var read_buf: [4096]u8 = undefined;
    var read_error = false;
    while (true) {
        const bytes_read = harness.posixRead(pipe_read, &read_buf) catch {
            read_error = true;
            break;
        };
        if (bytes_read == 0) break;
        result_buf.appendSlice(base.defaultGpa(), read_buf[0..bytes_read]) catch {
            read_error = true;
            break;
        };
    }
    harness.closeFd(pipe_read);

    const wait_result = harness.waitpid(fork_result, 0);
    const status = wait_result.status;
    const termination_signal: u8 = @truncate(status & 0x7f);
    if (termination_signal != 0) {
        result_buf.deinit(base.defaultGpa());
        return .{ .signal_death = termination_signal };
    }

    const exit_code: u8 = @truncate((status >> 8) & 0xff);
    if (exit_code == 2) {
        const owned = result_buf.toOwnedSlice(base.defaultGpa()) catch {
            result_buf.deinit(base.defaultGpa());
            return .{ .child_error = "ChildExecFailed" };
        };
        return .{ .child_error = owned };
    }
    if (exit_code != 0 or read_error) {
        result_buf.deinit(base.defaultGpa());
        return .{ .child_error = "ChildExecFailed" };
    }

    const owned = result_buf.toOwnedSlice(base.defaultGpa()) catch {
        result_buf.deinit(base.defaultGpa());
        return .{ .child_error = "ChildExecFailed" };
    };
    defer base.defaultGpa().free(owned);

    const decoded = decodeRun(owned, base.defaultGpa()) orelse return .{ .child_error = "DecodeFailed" };
    return .{ .success = decoded };
}

fn runInterpreter(allocator: std.mem.Allocator, lowered: *const LoweredProgram) BackendEvalError!RuntimeHostEnv.RecordedRun {
    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interp = try Interpreter.init(
        allocator,
        &lowered.view.store,
        &lowered.view.layouts,
        runtime_env.get_ops(),
    );
    defer interp.deinit();

    const arg_layouts = try helpers.mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);

    const eval_result = interp.eval(.{
        .proc_id = lowered.mainProc(),
        .arg_layouts = arg_layouts,
    }) catch |err| switch (err) {
        error.Crash => return runtime_env.snapshot(allocator),
        else => return err,
    };
    switch (eval_result) {
        .value => {},
    }

    return runtime_env.snapshot(allocator);
}

fn runDev(allocator: std.mem.Allocator, lowered: *const LoweredProgram) BackendEvalError!RuntimeHostEnv.RecordedRun {
    if (comptime !DEV_BACKEND_IMPLEMENTED) {
        return error.DevBackendUnavailable;
    } else {
        var codegen = try HostLirCodeGen.init(
            allocator,
            &lowered.view.store,
            &lowered.view.layouts,
            &.{},
        );
        defer codegen.deinit();
        try codegen.compileAllProcSpecs(lowered.view.store.getProcSpecs());

        const proc = lowered.view.store.getProcSpec(lowered.mainProc());
        const arg_layouts = try helpers.mainProcArgLayouts(allocator, lowered);
        defer allocator.free(arg_layouts);
        const entrypoint = try codegen.generateEntrypointWrapper(
            "roc_eval_host_effects_main",
            lowered.mainProc(),
            arg_layouts,
            proc.ret_layout,
        );
        var exec_mem = try ExecutableMemory.initWithEntryOffset(
            codegen.getGeneratedCode(),
            entrypoint.offset,
        );
        defer exec_mem.deinit();

        var runtime_env = RuntimeHostEnv.init(allocator);
        defer runtime_env.deinit();

        const arg_buffer = try helpers.zeroedEntrypointArgBuffer(allocator, lowered, arg_layouts);
        defer if (arg_buffer) |buf| allocator.free(buf);

        const ret_layout = proc.ret_layout;
        const size_align = lowered.view.layouts.layoutSizeAlign(lowered.view.layouts.getLayout(ret_layout));
        const alloc_len = @max(size_align.size, 1);
        const ret_buf = try allocator.alignedAlloc(u8, collections.max_roc_alignment, alloc_len);
        defer allocator.free(ret_buf);
        @memset(ret_buf, 0);

        var crash_boundary = runtime_env.enterCrashBoundary();
        defer crash_boundary.deinit();
        const sj = crash_boundary.set();
        if (sj == 0) {
            exec_mem.callRocABI(
                @ptrCast(runtime_env.get_ops()),
                @ptrCast(ret_buf.ptr),
                if (arg_buffer) |buf| @ptrCast(buf.ptr) else null,
            );
        }

        return runtime_env.snapshot(allocator);
    }
}

fn matchesExpectation(run: RuntimeHostEnv.RecordedRun, tc: TestCase) bool {
    if (run.termination != tc.expected_termination) return false;
    if (tc.expected_live_allocations) |expected| {
        if (run.allocation_count != expected) return false;
    }
    if (run.events.len != tc.expected_events.len) return false;
    for (run.events, tc.expected_events) |actual, expected| {
        switch (expected) {
            .dbg => |msg| switch (actual) {
                .dbg => |actual_msg| if (!std.mem.eql(u8, msg, actual_msg)) return false,
                else => return false,
            },
            .dbg_contains => |fragment| switch (actual) {
                .dbg => |actual_msg| if (std.mem.find(u8, actual_msg, fragment) == null) return false,
                else => return false,
            },
            .dbg_any => switch (actual) {
                .dbg => {},
                else => return false,
            },
            .expect_failed => |msg| switch (actual) {
                .expect_failed => |actual_msg| if (!std.mem.eql(u8, msg, actual_msg)) return false,
                else => return false,
            },
            .crashed => |msg| switch (actual) {
                .crashed => |actual_msg| if (!std.mem.eql(u8, msg, actual_msg)) return false,
                else => return false,
            },
        }
    }
    return true;
}

fn runSingleTest(io: std.Io, allocator: std.mem.Allocator, tc: TestCase) TestOutcome {
    var compiled = helpers.compileProgram(allocator, io, tc.source_kind, tc.source, tc.imports) catch |err| {
        return .{
            .status = .fail,
            .message = allocator.dupe(u8, @errorName(err)) catch null,
        };
    };
    defer compiled.deinit(allocator);

    var backends: [NUM_BACKENDS]BackendDetail = [_]BackendDetail{
        .{ .status = .skip },
        .{ .status = .skip },
    };
    var any_failure = false;
    var any_crash = false;
    var any_skip = false;

    const backend_specs = [_]struct {
        skip: bool,
        eval_fn: BackendEvalFn,
    }{
        .{ .skip = tc.skip.interpreter, .eval_fn = runInterpreter },
        .{ .skip = tc.skip.dev, .eval_fn = runDev },
    };

    for (backend_specs, 0..) |backend_spec, i| {
        if (i == 1 and !DEV_BACKEND_IMPLEMENTED) {
            backends[i] = .{ .status = .not_implemented };
            continue;
        }
        if (backend_spec.skip) {
            any_skip = true;
            backends[i] = .{ .status = .skip };
            continue;
        }

        var timer = Timer.start() catch unreachable;
        const lowered = &compiled.lowered;
        const fork_result = forkAndEval(backend_spec.eval_fn, lowered);
        const duration_ns = timer.read();

        switch (fork_result) {
            .success => |run| {
                if (matchesExpectation(run, tc)) {
                    backends[i] = .{
                        .status = .pass,
                        .run = run,
                        .duration_ns = duration_ns,
                    };
                } else {
                    any_failure = true;
                    backends[i] = .{
                        .status = .wrong,
                        .run = run,
                        .duration_ns = duration_ns,
                    };
                }
            },
            .child_error => |msg| {
                any_failure = true;
                backends[i] = .{
                    .status = .fail,
                    .message = allocator.dupe(u8, msg) catch null,
                    .duration_ns = duration_ns,
                };
            },
            .signal_death => |signal| {
                any_crash = true;
                const sig_str = std.fmt.allocPrint(allocator, "signal {d}", .{signal}) catch null;
                backends[i] = .{
                    .status = .crash,
                    .message = sig_str,
                    .duration_ns = duration_ns,
                };
            },
            .fork_failed => {
                any_failure = true;
                backends[i] = .{
                    .status = .fail,
                    .message = allocator.dupe(u8, "ForkFailed") catch null,
                    .duration_ns = duration_ns,
                };
            },
        }
    }

    if (any_crash) {
        return .{ .status = .crash, .backends = backends };
    }
    if (any_failure) {
        return .{ .status = .fail, .backends = backends };
    }
    if (any_skip) {
        return .{ .status = .skip, .backends = backends };
    }
    return .{ .status = .pass, .backends = backends };
}

fn serializeOutcome(fd: posix.fd_t, outcome: TestOutcome, duration_ns: u64) void {
    var run_bufs: [NUM_BACKENDS]?[]u8 = .{ null, null };
    defer {
        for (run_bufs) |maybe_buf| {
            if (maybe_buf) |buf| base.defaultGpa().free(buf);
        }
    }

    var header: WireHeader = .{
        .status = @intFromEnum(outcome.status),
        .duration_ns = duration_ns,
        .message_len = if (outcome.message) |msg| @intCast(msg.len) else 0,
        .backend_statuses = undefined,
        .backend_durations = undefined,
        .backend_message_lens = undefined,
        .backend_run_lens = undefined,
    };

    for (outcome.backends, 0..) |backend_detail, i| {
        header.backend_statuses[i] = @intFromEnum(backend_detail.status);
        header.backend_durations[i] = backend_detail.duration_ns;
        header.backend_message_lens[i] = if (backend_detail.message) |msg| @intCast(msg.len) else 0;
        if (backend_detail.run) |run| {
            var buf: std.ArrayListUnmanaged(u8) = .empty;
            appendEncodedRun(base.defaultGpa(), &buf, run) catch {
                header.backend_run_lens[i] = 0;
                continue;
            };
            const owned = buf.toOwnedSlice(base.defaultGpa()) catch {
                buf.deinit(base.defaultGpa());
                header.backend_run_lens[i] = 0;
                continue;
            };
            run_bufs[i] = owned;
            header.backend_run_lens[i] = @intCast(owned.len);
        } else {
            header.backend_run_lens[i] = 0;
        }
    }

    harness.writeAll(fd, std.mem.asBytes(&header));
    if (outcome.message) |msg| harness.writeAll(fd, msg);
    for (outcome.backends) |backend_detail| {
        if (backend_detail.message) |msg| harness.writeAll(fd, msg);
    }
    for (run_bufs) |maybe_buf| {
        if (maybe_buf) |buf| harness.writeAll(fd, buf);
    }
}

fn deserializeOutcome(buf: []const u8, gpa: std.mem.Allocator) ?TestResult {
    var offset: usize = 0;
    const header = readWireValue(WireHeader, buf, &offset) orelse return null;

    const message = harness.readStr(buf, &offset, header.message_len, gpa);
    var backends: [NUM_BACKENDS]BackendDetail = undefined;

    for (0..NUM_BACKENDS) |i| {
        const backend_message = harness.readStr(buf, &offset, header.backend_message_lens[i], gpa);
        backends[i] = .{
            .status = @enumFromInt(header.backend_statuses[i]),
            .run = null,
            .message = backend_message,
            .duration_ns = header.backend_durations[i],
        };
    }

    for (0..NUM_BACKENDS) |i| {
        const run_len = header.backend_run_lens[i];
        if (run_len == 0) continue;
        const end = offset + run_len;
        if (end > buf.len) return null;
        backends[i].run = decodeRun(buf[offset..end], gpa);
        if (backends[i].run == null) return null;
        offset = end;
    }

    return .{
        .status = @enumFromInt(header.status),
        .message = message,
        .duration_ns = header.duration_ns,
        .backends = backends,
    };
}

fn runTestForPool(io: std.Io, allocator: std.mem.Allocator, tc: TestCase, _: u64) TestResult {
    var timer = Timer.start() catch unreachable;
    const outcome = runSingleTest(io, allocator, tc);
    const duration_ns = timer.read();
    return .{
        .status = outcome.status,
        .message = outcome.message,
        .duration_ns = duration_ns,
        .backends = outcome.backends,
    };
}

fn serializeResultForPool(fd: posix.fd_t, result: TestResult) void {
    const outcome: TestOutcome = .{
        .status = result.status,
        .message = result.message,
        .backends = result.backends,
    };
    serializeOutcome(fd, outcome, result.duration_ns);
}

fn dupeOptional(gpa: std.mem.Allocator, value: ?[]const u8) ?[]const u8 {
    return if (value) |slice| gpa.dupe(u8, slice) catch null else null;
}

fn dupeRun(gpa: std.mem.Allocator, run: ?RuntimeHostEnv.RecordedRun) ?RuntimeHostEnv.RecordedRun {
    if (run) |recorded| {
        return recorded.dupe(gpa) catch null;
    }
    return null;
}

fn stabilizeResult(gpa: std.mem.Allocator, result: TestResult) TestResult {
    var stable_backends = result.backends;
    for (&stable_backends) |*backend_detail| {
        backend_detail.message = dupeOptional(gpa, backend_detail.message);
        backend_detail.run = dupeRun(gpa, backend_detail.run);
    }
    return .{
        .status = result.status,
        .message = dupeOptional(gpa, result.message),
        .duration_ns = result.duration_ns,
        .backends = stable_backends,
    };
}

fn getTestName(tc: TestCase) []const u8 {
    return tc.name;
}

const default_result: TestResult = .{
    .status = .crash,
    .message = null,
    .duration_ns = 0,
    .backends = [_]BackendDetail{ .{ .status = .crash }, .{ .status = .crash } },
};

const timeout_result: TestResult = .{
    .status = .timeout,
    .message = null,
    .duration_ns = 0,
    .backends = [_]BackendDetail{ .{ .status = .crash }, .{ .status = .crash } },
};

const Pool = harness.ProcessPool(TestCase, TestResult, .{
    .runTest = &runTestForPool,
    .serialize = &serializeResultForPool,
    .deserialize = &deserializeOutcome,
    .default_result = default_result,
    .timeout_result = timeout_result,
    .stabilizeResult = &stabilizeResult,
    .getName = getTestName,
});

fn collectTests() []const TestCase {
    return &host_effects_tests.tests;
}

fn printHelp() void {
    const help =
        \\Roc Runtime Host-Effects Test Runner
        \\
        \\Runs eval tests across the interpreter and dev backend and compares the
        \\exact host callback traffic emitted through RocOps:
        \\  - dbg
        \\  - expect_failed
        \\  - crashed
        \\
        \\USAGE:
        \\  zig build run-test-eval-host-effects
        \\  zig build run-test-eval-host-effects -- <OPTIONS>
        \\  ./zig-out/bin/eval-host-effects-runner [<OPTIONS>]
        \\
        \\OPTIONS:
        \\  -h, --help            Show this help message and exit.
        \\  --filter <PATTERN>    Run only tests whose name or source contains PATTERN.
        \\  --threads <N>         Max concurrent child processes.
        \\  --verbose             Print PASS and SKIP results.
        \\  --timeout <MS>        Per-test timeout in milliseconds.
        \\
    ;
    std.debug.print("{s}", .{help});
}

fn printEscapedBytes(bytes: []const u8) void {
    std.debug.print("\"", .{});
    for (bytes) |byte| {
        switch (byte) {
            '\n' => std.debug.print("\\n", .{}),
            '\r' => std.debug.print("\\r", .{}),
            '\t' => std.debug.print("\\t", .{}),
            '\\' => std.debug.print("\\\\", .{}),
            '"' => std.debug.print("\\\"", .{}),
            else => {
                if (std.ascii.isPrint(byte)) {
                    std.debug.print("{c}", .{byte});
                } else {
                    std.debug.print("\\x{x:0>2}", .{byte});
                }
            },
        }
    }
    std.debug.print("\"", .{});
}

fn printExpected(tc: TestCase) void {
    std.debug.print("        expected: {s} ", .{@tagName(tc.expected_termination)});
    if (tc.expected_live_allocations) |expected_live_allocations| {
        std.debug.print("live_allocations={d} ", .{expected_live_allocations});
    }
    if (tc.expected_events.len == 0) {
        std.debug.print("[]\n", .{});
        return;
    }
    std.debug.print("[", .{});
    for (tc.expected_events, 0..) |event, i| {
        if (i > 0) std.debug.print(", ", .{});
        switch (event) {
            .dbg => |msg| {
                std.debug.print("dbg=", .{});
                printEscapedBytes(msg);
            },
            .dbg_contains => |msg| {
                std.debug.print("dbg_contains=", .{});
                printEscapedBytes(msg);
            },
            .dbg_any => {
                std.debug.print("dbg=<any>", .{});
            },
            .expect_failed => |msg| {
                std.debug.print("expect_failed=", .{});
                printEscapedBytes(msg);
            },
            .crashed => |msg| {
                std.debug.print("crashed=", .{});
                printEscapedBytes(msg);
            },
        }
    }
    std.debug.print("]\n", .{});
}

fn printRecordedRun(run: RuntimeHostEnv.RecordedRun) void {
    std.debug.print("{s} live_allocations={d} ", .{ @tagName(run.termination), run.allocation_count });
    if (run.events.len == 0) {
        std.debug.print("[]", .{});
        return;
    }
    std.debug.print("[", .{});
    for (run.events, 0..) |event, i| {
        if (i > 0) std.debug.print(", ", .{});
        switch (event) {
            .dbg => |msg| {
                std.debug.print("dbg=", .{});
                printEscapedBytes(msg);
            },
            .expect_failed => |msg| {
                std.debug.print("expect_failed=", .{});
                printEscapedBytes(msg);
            },
            .crashed => |msg| {
                std.debug.print("crashed=", .{});
                printEscapedBytes(msg);
            },
        }
    }
    std.debug.print("]", .{});
}

fn writeFailureDetail(tc: TestCase, result: TestResult) void {
    printExpected(tc);
    for (result.backends, 0..) |backend_detail, i| {
        std.debug.print("        {s}: ", .{BACKEND_NAMES[i]});
        switch (backend_detail.status) {
            .pass => {
                std.debug.print("PASS ", .{});
                if (backend_detail.run) |run| printRecordedRun(run);
                std.debug.print("\n", .{});
            },
            .wrong => {
                std.debug.print("WRONG ", .{});
                if (backend_detail.run) |run| printRecordedRun(run);
                std.debug.print("\n", .{});
            },
            .fail => {
                std.debug.print("FAIL", .{});
                if (backend_detail.message) |msg| {
                    std.debug.print(" ", .{});
                    printEscapedBytes(msg);
                }
                std.debug.print("\n", .{});
            },
            .crash => {
                std.debug.print("CRASH", .{});
                if (backend_detail.message) |msg| {
                    std.debug.print(" ", .{});
                    printEscapedBytes(msg);
                }
                std.debug.print("\n", .{});
            },
            .skip => std.debug.print("SKIP\n", .{}),
            .not_implemented => std.debug.print("NOT_IMPLEMENTED\n", .{}),
        }
    }
}

fn statsStatus(status: TestOutcome.Status) []const u8 {
    return switch (status) {
        .pass => "pass",
        .fail => "fail",
        .crash => "crash",
        .skip => "skip",
        .timeout => "timeout",
    };
}

fn backendStatsStatus(status: BackendStatus) []const u8 {
    return switch (status) {
        .pass => "pass",
        .wrong, .fail => "fail",
        .crash => "crash",
        .skip, .not_implemented => "skip",
    };
}

fn statsSummary(results: []const TestResult) harness.StatsSummary {
    var summary: harness.StatsSummary = .{ .total = results.len };
    for (results) |result| {
        switch (result.status) {
            .pass => summary.passed += 1,
            .fail => summary.failed += 1,
            .crash => summary.crashed += 1,
            .skip => summary.skipped += 1,
            .timeout => summary.timed_out += 1,
        }
    }
    return summary;
}

fn recordedRunSummary(allocator: std.mem.Allocator, run: RuntimeHostEnv.RecordedRun) []const u8 {
    return std.fmt.allocPrint(
        allocator,
        "{s} live_allocations={d} events={d}",
        .{ @tagName(run.termination), run.allocation_count, run.events.len },
    ) catch "recorded run unavailable";
}

fn maybeStatsData(
    allocator: std.mem.Allocator,
    result: TestResult,
) []const harness.StatsData {
    if (result.status == .pass) return &.{};

    var count: usize = 0;
    if (result.message != null) count += 1;
    for (result.backends) |backend_detail| {
        if (backend_detail.message != null) count += 1;
        if (backend_detail.run != null and backend_detail.status != .pass) count += 1;
    }
    if (count == 0) return &.{};

    const data = allocator.alloc(harness.StatsData, count) catch return &.{};
    var next: usize = 0;
    if (result.message) |message| {
        data[next] = .{ .key = "message", .value = message };
        next += 1;
    }
    for (result.backends, 0..) |backend_detail, i| {
        if (backend_detail.message) |message| {
            data[next] = .{ .key = BACKEND_NAMES[i], .value = message };
            next += 1;
        }
        if (backend_detail.run) |run| {
            if (backend_detail.status != .pass) {
                data[next] = .{ .key = BACKEND_NAMES[i], .value = recordedRunSummary(allocator, run) };
                next += 1;
            }
        }
    }
    return data;
}

fn appendStatsEvent(
    allocator: std.mem.Allocator,
    events: *std.ArrayListUnmanaged(harness.StatsEvent),
    id: []const u8,
    parent_id: ?[]const u8,
    kind: []const u8,
    name: []const u8,
    status: []const u8,
    start_ns: u64,
    end_ns: u64,
    data: []const harness.StatsData,
) void {
    events.append(allocator, .{
        .id = id,
        .parent_id = parent_id,
        .kind = kind,
        .name = name,
        .status = status,
        .start_ns = start_ns,
        .end_ns = end_ns,
        .data = data,
    }) catch {};
}

fn appendCaseStatsEvent(
    allocator: std.mem.Allocator,
    events: *std.ArrayListUnmanaged(harness.StatsEvent),
    id: []const u8,
    name: []const u8,
    status: []const u8,
    duration_ns: u64,
    maybe_span: ?harness.PoolSpan,
    data: []const harness.StatsData,
) void {
    const start_ns = if (maybe_span) |span| span.start_ns else 0;
    const end_ns = if (maybe_span) |span| span.end_ns else duration_ns;
    const worker_index = if (maybe_span) |span| span.worker_index else null;
    events.append(allocator, .{
        .id = id,
        .parent_id = null,
        .kind = "case",
        .name = name,
        .status = status,
        .start_ns = start_ns,
        .end_ns = end_ns,
        .worker_index = worker_index,
        .data = data,
    }) catch {};
}

fn writeStatsJson(
    allocator: std.mem.Allocator,
    io: std.Io,
    path: []const u8,
    tests: []const TestCase,
    results: []const TestResult,
    spans: []const ?harness.PoolSpan,
) StatsJsonError!void {
    var stats_arena = std.heap.ArenaAllocator.init(allocator);
    defer stats_arena.deinit();
    const stats_allocator = stats_arena.allocator();

    var events: std.ArrayListUnmanaged(harness.StatsEvent) = .empty;

    for (tests, results, 0..) |tc, result, i| {
        const case_id = try std.fmt.allocPrint(stats_allocator, "case-{d}", .{i});
        const maybe_span = if (i < spans.len) spans[i] else null;
        appendCaseStatsEvent(stats_allocator, &events, case_id, tc.name, statsStatus(result.status), result.duration_ns, maybe_span, maybeStatsData(stats_allocator, result));

        var cursor: u64 = 0;
        for (result.backends, 0..) |backend_detail, backend_i| {
            if (backend_detail.duration_ns == 0 and backend_detail.status == .skip) continue;
            const id = try std.fmt.allocPrint(stats_allocator, "case-{d}-backend-{s}", .{ i, BACKEND_NAMES[backend_i] });
            const status = backendStatsStatus(backend_detail.status);
            appendStatsEvent(stats_allocator, &events, id, case_id, "backend", BACKEND_NAMES[backend_i], status, cursor, cursor + backend_detail.duration_ns, &.{});
            cursor += backend_detail.duration_ns;
        }
    }

    try harness.writeRunnerStatsJson(stats_allocator, io, path, .{
        .runner = "eval-host-effects",
        .summary = statsSummary(results),
        .events = events.items,
    });
}

/// Public function `main`.
pub fn main(init: std.process.Init) RunnerMainError!void {
    var gpa_impl: std.heap.DebugAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    const io = init.io;

    var args_arena = collections.SingleThreadArena.init(gpa);
    defer args_arena.deinit();
    const cli = try harness.parseStandardArgs(args_arena.allocator(), init.minimal.args);

    if (cli.help_requested) {
        printHelp();
        return;
    }

    const all_tests = collectTests();
    var filtered_buf: std.ArrayListUnmanaged(TestCase) = .empty;
    defer filtered_buf.deinit(gpa);

    if (cli.filters.len > 0) {
        for (all_tests) |tc| {
            for (cli.filters) |pattern| {
                if (std.mem.find(u8, tc.name, pattern) != null or
                    std.mem.find(u8, tc.source, pattern) != null)
                {
                    try filtered_buf.append(gpa, tc);
                    break;
                }
            }
        }
    } else {
        try filtered_buf.appendSlice(gpa, all_tests);
    }

    const tests = filtered_buf.items;
    if (tests.len == 0) return;

    const cpu_count = std.Thread.getCpuCount() catch 1;
    const max_children: usize = cli.max_threads orelse @min(cpu_count, tests.len);

    const results = try gpa.alloc(TestResult, tests.len);
    defer {
        for (results) |*result| result.deinit(gpa);
        gpa.free(results);
    }
    @memset(results, default_result);
    const spans = try gpa.alloc(?harness.PoolSpan, tests.len);
    defer gpa.free(spans);
    @memset(spans, null);

    var wall_timer = Timer.start() catch unreachable;
    const hang_timeout_ms: u64 = if (cli.timeout_provided and cli.timeout_ms > 0)
        cli.timeout_ms
    else if (max_children <= 1)
        10_000
    else
        30_000;

    // worker_argv_template is null — this runner doesn't (yet) support
    // Windows Child-based parallelism; on Windows it falls through to
    // runSequential as before.
    Pool.runWithSpans(io, tests, results, spans, max_children, hang_timeout_ms, gpa, null);

    const wall_elapsed = wall_timer.read();
    var passed: usize = 0;
    var failed: usize = 0;
    var crashed: usize = 0;
    var skipped: usize = 0;
    var timed_out: usize = 0;

    std.debug.print("\n=== Runtime Host-Effects Test Results ===\n", .{});

    for (tests, 0..) |tc, i| {
        const result = results[i];
        const ms = @as(f64, @floatFromInt(result.duration_ns)) / 1_000_000.0;
        switch (result.status) {
            .pass => {
                passed += 1;
                if (cli.verbose) std.debug.print("  PASS  {s}  ({d:.1}ms)\n", .{ tc.name, ms });
            },
            .fail => {
                failed += 1;
                std.debug.print("  FAIL  {s}  ({d:.1}ms)\n", .{ tc.name, ms });
                if (result.message) |msg| std.debug.print("        {s}\n", .{msg});
                writeFailureDetail(tc, result);
            },
            .crash => {
                crashed += 1;
                std.debug.print("  CRASH {s}  ({d:.1}ms)\n", .{ tc.name, ms });
                if (result.message) |msg| std.debug.print("        {s}\n", .{msg});
                writeFailureDetail(tc, result);
            },
            .skip => {
                skipped += 1;
                if (cli.verbose) std.debug.print("  SKIP  {s}\n", .{tc.name});
            },
            .timeout => {
                timed_out += 1;
                std.debug.print("  HANG  {s}  ({d:.1}ms)\n", .{ tc.name, ms });
            },
        }
    }

    const wall_ms = @as(f64, @floatFromInt(wall_elapsed)) / 1_000_000.0;
    std.debug.print(
        "\n{d} passed, {d} failed, {d} crashed, {d} hung, {d} skipped ({d} total) in {d:.0}ms using {d} process(es)\n",
        .{ passed, failed, crashed, timed_out, skipped, tests.len, wall_ms, max_children },
    );

    if (cli.stats_json_path) |path| {
        try writeStatsJson(gpa, io, path, tests, results, spans);
    }

    if (failed > 0 or crashed > 0 or timed_out > 0) std.process.exit(1);
}
