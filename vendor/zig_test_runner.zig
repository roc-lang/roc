//! Vendored Zig 0.17.0 test runner (MIT), with an explicit Roc summary output.
//! Default test runner for unit tests.
const builtin = @import("builtin");

const std = @import("std");
const Io = std.Io;
const fatal = std.process.fatal;
const testing = std.testing;
const assert = std.debug.assert;
const panic = std.debug.panic;
const fuzz_abi = std.Build.abi.fuzz;

pub const std_options: std.Options = .{
    .logFn = log,
};

var log_err_count: usize = 0;
var report_path: ?[]const u8 = null;
var report_directory: ?[]const u8 = null;
var report_name: ?[]const u8 = null;
var report_status: []u8 = &.{};
var selected_tests: []usize = &.{};
var selected_count: usize = 0;
var filters: std.ArrayList([]const u8) = .empty;

fn matchesFilter(name: []const u8) bool {
    if (filters.items.len == 0) return true;
    for (filters.items) |filter| {
        if (std.mem.find(u8, name, filter) != null) return true;
    }
    return false;
}

fn writeReport() void {
    const path = report_name orelse report_path orelse return;
    const directory = if (report_directory) |dir|
        Io.Dir.cwd().openDir(runner_threaded_io, dir, .{}) catch |err|
            panic("cannot open test report directory: {t}", .{err})
    else
        Io.Dir.cwd();
    defer if (report_directory != null) directory.close(runner_threaded_io);
    const file = directory.createFile(runner_threaded_io, path, .{}) catch |err|
        panic("cannot create test report: {t}", .{err});
    defer file.close(runner_threaded_io);
    var buffer: [4096]u8 = undefined;
    var output = file.writer(runner_threaded_io, &buffer);
    for (builtin.test_functions, report_status) |test_fn, status| {
        if (status == 0) continue;
        output.interface.print("{d}\t{s}\n", .{ status, test_fn.name }) catch |err|
            panic("cannot write test report: {t}", .{err});
    }
    output.interface.flush() catch |err| panic("cannot flush test report: {t}", .{err});
}

var fba: std.heap.FixedBufferAllocator = .init(&fba_buffer);
var fba_buffer: [8192]u8 = undefined;
var stdin_buffer: [4096]u8 = undefined;
var stdout_buffer: [4096]u8 = undefined;
var stdin_reader: Io.File.Reader = undefined;
var stdout_writer: Io.File.Writer = undefined;
const runner_threaded_io: Io = Io.Threaded.global_single_threaded.io();

/// Keep in sync with logic in `std.Build.addRunArtifact` which decides whether
/// the test runner will communicate with the build runner via `std.zig.Server`.
const need_simple = switch (builtin.zig_backend) {
    .stage2_aarch64,
    .stage2_loongarch,
    .stage2_powerpc,
    .stage2_riscv64,
    => true,
    else => false,
};

pub fn main(init: std.process.Init.Minimal) void {
    @disableInstrumentation();

    if (builtin.cpu.arch.isSpirV()) {
        // SPIR-V needs an special test-runner
        return;
    }

    const args = init.args.toSlice(fba.allocator()) catch |err| panic("unable to parse command line args: {t}", .{err});

    var listen = false;
    var opt_cache_dir: ?[]const u8 = null;

    report_status = std.heap.page_allocator.alloc(u8, builtin.test_functions.len) catch @panic("test report allocation failed");
    @memset(report_status, 0);
    selected_tests = std.heap.page_allocator.alloc(usize, builtin.test_functions.len) catch @panic("test index allocation failed");

    var arg_index: usize = 1;
    while (arg_index < args.len) : (arg_index += 1) {
        const arg = args[arg_index];
        if (std.mem.eql(u8, arg, "--listen=-")) {
            listen = true;
        } else if (std.mem.startsWith(u8, arg, "--seed=")) {
            testing.random_seed = std.fmt.parseUnsigned(u32, arg["--seed=".len..], 0) catch
                @panic("unable to parse --seed command line argument");
        } else if (std.mem.startsWith(u8, arg, "--cache-dir")) {
            opt_cache_dir = arg["--cache-dir=".len..];
        } else if (std.mem.eql(u8, arg, "--test-filter")) {
            arg_index += 1;
            if (arg_index == args.len) @panic("--test-filter requires a value");
            filters.append(std.heap.page_allocator, args[arg_index]) catch @panic("test filter allocation failed");
        } else if (std.mem.startsWith(u8, arg, "--test-filter=")) {
            filters.append(std.heap.page_allocator, arg["--test-filter=".len..]) catch @panic("test filter allocation failed");
        } else if (std.mem.startsWith(u8, arg, "--roc-test-report=")) {
            report_path = arg["--roc-test-report=".len..];
        } else if (std.mem.startsWith(u8, arg, "--roc-test-report-dir=")) {
            report_directory = arg["--roc-test-report-dir=".len..];
        } else if (std.mem.startsWith(u8, arg, "--roc-test-report-name=")) {
            report_name = arg["--roc-test-report-name=".len..];
        } else {
            panic("unrecognized command line argument: {s}", .{arg});
        }
    }

    if (report_directory != null) {
        const name = report_name orelse @panic("--roc-test-report-dir requires --roc-test-report-name");
        if (report_path != null) @panic("conflicting test report destinations");
        if (name.len == 0 or !std.mem.eql(u8, name, Io.Dir.path.basename(name)) or
            std.mem.eql(u8, name, ".") or std.mem.eql(u8, name, ".."))
            @panic("test report name must be a basename");
    } else if (report_name != null) {
        @panic("--roc-test-report-name requires --roc-test-report-dir");
    }

    for (builtin.test_functions, 0..) |test_fn, index| {
        if (!matchesFilter(test_fn.name)) continue;
        selected_tests[selected_count] = index;
        selected_count += 1;
    }

    if (need_simple) {
        defer writeReport();
        return mainSimple() catch |err| panic("test failure: {t}", .{err});
    }

    if (builtin.fuzz) {
        const cache_dir = opt_cache_dir orelse @panic("missing --cache-dir=[path] argument");
        fuzz_abi.fuzzer_init(.fromSlice(cache_dir));
    }

    if (listen) {
        return mainServer(init) catch |err| panic("internal test runner failure: {t}", .{err});
    } else {
        return mainTerminal(init);
    }
}

fn mainServer(init: std.process.Init.Minimal) !void {
    @disableInstrumentation();
    stdin_reader = .initStreaming(.stdin(), runner_threaded_io, &stdin_buffer);
    stdout_writer = .initStreaming(.stdout(), runner_threaded_io, &stdout_buffer);
    var server: std.zig.Server = .{
        .in = &stdin_reader.interface,
        .out = &stdout_writer.interface,
    };
    try server.serveStringMessage(.zig_version, builtin.zig_version_string);

    while (true) {
        const hdr = try server.receiveMessage();
        switch (hdr.tag) {
            .exit => {
                writeReport();
                return std.process.exit(0);
            },
            .query_test_metadata => {
                var sa: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
                defer if (sa.deinit() != 0) @panic("internal test runner memory leak");
                const gpa = sa.allocator();

                var string_bytes: std.ArrayList(u8) = .empty;
                defer string_bytes.deinit(gpa);
                try string_bytes.append(gpa, 0); // Reserve 0 for null.

                const test_fns = builtin.test_functions;
                const names = try gpa.alloc(u32, selected_count);
                defer gpa.free(names);
                const expected_panic_msgs = try gpa.alloc(u32, selected_count);
                defer gpa.free(expected_panic_msgs);

                for (selected_tests[0..selected_count], names, expected_panic_msgs) |test_index, *name, *expected_panic_msg| {
                    const test_fn = test_fns[test_index];
                    name.* = @intCast(string_bytes.items.len);
                    try string_bytes.ensureUnusedCapacity(gpa, test_fn.name.len + 1);
                    string_bytes.appendSliceAssumeCapacity(test_fn.name);
                    string_bytes.appendAssumeCapacity(0);
                    expected_panic_msg.* = 0;
                }

                try server.serveTestMetadata(.{
                    .names = names,
                    .expected_panic_msgs = expected_panic_msgs,
                    .string_bytes = string_bytes.items,
                });
            },

            .run_test => {
                testing.environ = init.environ;
                testing.allocator_instance = .init(std.heap.page_allocator, .{
                    .canary = 0xc3a701ba,
                    .check_write_after_free = true,
                });
                testing.io_instance = .init(testing.allocator, .{
                    .argv0 = .init(init.args),
                    .environ = init.environ,
                });
                log_err_count = 0;
                const requested_index = try server.receiveBody_u32();
                const index = selected_tests[requested_index];
                const test_fn = builtin.test_functions[index];
                is_fuzz_test = false;

                // let the build server know we're starting the test now
                try server.serveStringMessage(.test_started, &.{});

                const TestResults = std.zig.Server.Message.TestResults;
                const status: TestResults.Status = if (test_fn.func()) |v| s: {
                    v;
                    break :s .pass;
                } else |err| switch (err) {
                    error.SkipZigTest => .skip,
                    else => s: {
                        if (@errorReturnTrace()) |trace| {
                            std.debug.dumpErrorReturnTrace(trace);
                        }
                        break :s .fail;
                    },
                };
                report_status[index] = switch (status) {
                    .pass => 1,
                    .skip => 2,
                    .fail => 3,
                };
                testing.io_instance.deinit();
                const leak_count = testing.allocator_instance.deinit();
                try server.serveTestResults(.{
                    .index = requested_index,
                    .flags = .{
                        .status = status,
                        .fuzz = is_fuzz_test,
                        .log_err_count = std.math.lossyCast(
                            @FieldType(TestResults.Flags, "log_err_count"),
                            log_err_count,
                        ),
                        .leak_count = std.math.lossyCast(
                            @FieldType(TestResults.Flags, "leak_count"),
                            leak_count,
                        ),
                    },
                });
            },
            .start_fuzzing => {
                // This ensures that this code won't be analyzed and hence reference fuzzer symbols
                // since they are not present.
                if (!builtin.fuzz) unreachable;

                var gpa_instance: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
                defer if (gpa_instance.deinit() != 0) {
                    @panic("internal test runner memory leak");
                };
                const gpa = gpa_instance.allocator();
                var io_instance: Io.Threaded = .init(gpa, .{
                    .argv0 = .init(init.args),
                    .environ = init.environ,
                });
                defer io_instance.deinit();

                const mode: fuzz_abi.LimitKind = @fromBackingInt(@intCast(try server.receiveBody_u8()));
                const amount_or_instance = try server.receiveBody_u64();
                const main_instance = mode == .iterations or amount_or_instance == 0;

                if (main_instance) {
                    const coverage = fuzz_abi.fuzzer_coverage();
                    try server.serveCoverageIdMessage(
                        coverage.id,
                        coverage.runs,
                        coverage.unique,
                        coverage.seen,
                    );
                }

                const n_tests: u32 = try server.receiveBody_u32();
                const test_indexes = try gpa.alloc(u32, n_tests);
                defer gpa.free(test_indexes);
                fuzz_runner = .{
                    .indexes = test_indexes,
                    .server = &server,
                    .gpa = gpa,
                    .threaded_io = &io_instance,
                    .input_poller = undefined,
                };

                {
                    var large_name_buf: std.ArrayList(u8) = .empty;
                    defer large_name_buf.deinit(gpa);
                    for (test_indexes) |*i| {
                        const name_len = try server.receiveBody_u32();
                        const name = if (name_len <= server.in.buffer.len)
                            try server.in.take(name_len)
                        else large_name: {
                            try large_name_buf.resize(gpa, name_len);
                            try server.in.readSliceAll(large_name_buf.items);
                            break :large_name large_name_buf.items;
                        };

                        for (0.., builtin.test_functions) |test_i, test_fn| {
                            if (std.mem.eql(u8, name, test_fn.name)) {
                                i.* = @intCast(test_i);
                                break;
                            }
                        } else {
                            panic("fuzz test {s} no longer exists", .{name});
                        }

                        if (main_instance) {
                            const relocated_entry_addr = @intFromPtr(builtin.test_functions[i.*].func);
                            const entry_addr = fuzz_abi.fuzzer_unslide_address(relocated_entry_addr);
                            try server.serveU64Message(.fuzz_start_addr, entry_addr);
                        }
                    }
                }

                fuzz_abi.fuzzer_main(n_tests, testing.random_seed, mode, amount_or_instance);

                assert(mode != .forever);
                std.process.exit(0);
            },

            else => {
                std.debug.print("unsupported message: {x}\n", .{@backingInt(hdr.tag)});
                std.process.exit(1);
            },
        }
    }
}

fn mainTerminal(init: std.process.Init.Minimal) void {
    @disableInstrumentation();
    defer writeReport();
    if (builtin.fuzz) @panic("fuzz test requires server");

    const test_fn_list = builtin.test_functions;
    var ok_count: usize = 0;
    var skip_count: usize = 0;
    var fail_count: usize = 0;
    var fuzz_count: usize = 0;
    const root_node = if (builtin.fuzz) std.Progress.Node.none else std.Progress.start(runner_threaded_io, .{
        .root_name = "Test",
        .estimated_total_items = selected_count,
    });
    const have_tty = Io.File.stderr().isTty(runner_threaded_io) catch unreachable;

    var leaks: usize = 0;
    for (selected_tests[0..selected_count], 0..) |index, i| {
        const test_fn = test_fn_list[index];
        testing.allocator_instance = .init(std.heap.page_allocator, .{
            .canary = 0xc3a701ba,
            .check_write_after_free = true,
        });
        testing.io_instance = .init(testing.allocator, .{
            .argv0 = .init(init.args),
            .environ = init.environ,
        });
        defer {
            testing.io_instance.deinit();
            if (testing.allocator_instance.deinit() != 0) leaks += 1;
        }
        testing.log_level = .warn;
        testing.environ = init.environ;

        const test_node = root_node.start(test_fn.name, 0);
        if (!have_tty) {
            std.debug.print("{d}/{d} {s}...", .{ i + 1, selected_count, test_fn.name });
        }
        is_fuzz_test = false;
        if (test_fn.func()) |_| {
            ok_count += 1;
            report_status[index] = 1;
            test_node.end();
            if (!have_tty) std.debug.print("OK\n", .{});
        } else |err| switch (err) {
            error.SkipZigTest => {
                skip_count += 1;
                report_status[index] = 2;
                if (have_tty) {
                    std.debug.print("{d}/{d} {s}...SKIP\n", .{ i + 1, selected_count, test_fn.name });
                } else {
                    std.debug.print("SKIP\n", .{});
                }
                test_node.end();
            },
            else => {
                fail_count += 1;
                report_status[index] = 3;
                if (have_tty) {
                    std.debug.print("{d}/{d} {s}...FAIL ({t})\n", .{
                        i + 1, selected_count, test_fn.name, err,
                    });
                } else {
                    std.debug.print("FAIL ({t})\n", .{err});
                }
                if (@errorReturnTrace()) |trace| {
                    std.debug.dumpErrorReturnTrace(trace);
                }
                test_node.end();
            },
        }
        fuzz_count += @intFromBool(is_fuzz_test);
    }
    root_node.end();
    if (ok_count == selected_count) {
        std.debug.print("All {d} tests passed.\n", .{ok_count});
    } else {
        std.debug.print("{d} passed; {d} skipped; {d} failed.\n", .{ ok_count, skip_count, fail_count });
    }
    if (log_err_count != 0) {
        std.debug.print("{d} errors were logged.\n", .{log_err_count});
    }
    if (leaks != 0) {
        std.debug.print("{d} tests leaked memory.\n", .{leaks});
    }
    if (fuzz_count != 0) {
        std.debug.print("{d} fuzz tests found.\n", .{fuzz_count});
    }
    if (leaks != 0 or log_err_count != 0 or fail_count != 0) {
        writeReport();
        std.process.exit(1);
    }
}

pub fn log(
    comptime message_level: std.log.Level,
    comptime scope: @EnumLiteral(),
    comptime format: []const u8,
    args: anytype,
) void {
    @disableInstrumentation();
    if (@backingInt(message_level) <= @backingInt(std.log.Level.err)) {
        log_err_count +|= 1;
    }
    if (@backingInt(message_level) <= @backingInt(testing.log_level)) {
        std.debug.print(
            "[" ++ @tagName(scope) ++ "] (" ++ @tagName(message_level) ++ "): " ++ format ++ "\n",
            args,
        );
    }
}

/// Simpler main(), exercising fewer language features, so that
/// work-in-progress backends can handle it.
pub fn mainSimple() anyerror!void {
    @disableInstrumentation();
    // is the backend capable of calling `Io.File.writeAll`?
    const enable_write = switch (builtin.zig_backend) {
        .stage2_aarch64, .stage2_riscv64 => true,
        else => false,
    };
    // is the backend capable of calling `Io.Writer.print`?
    const enable_print = switch (builtin.zig_backend) {
        .stage2_aarch64, .stage2_riscv64 => true,
        else => false,
    };

    testing.allocator_instance = .init(std.heap.page_allocator, .{});
    testing.io_instance = .init(testing.allocator, .{});

    var passed: u64 = 0;
    var skipped: u64 = 0;
    var failed: u64 = 0;

    // we don't want to bring in File and Writer if the backend doesn't support it
    const stdout = if (enable_write) Io.File.stdout() else {};

    for (builtin.test_functions, 0..) |test_fn, test_index| {
        if (!matchesFilter(test_fn.name)) continue;
        if (enable_write) {
            stdout.writeStreamingAll(runner_threaded_io, test_fn.name) catch {};
            stdout.writeStreamingAll(runner_threaded_io, "... ") catch {};
        }
        if (test_fn.func()) |_| {
            if (enable_write) stdout.writeStreamingAll(runner_threaded_io, "PASS\n") catch {};
        } else |err| {
            if (err != error.SkipZigTest) {
                if (enable_write) stdout.writeStreamingAll(runner_threaded_io, "FAIL\n") catch {};
                failed += 1;
                if (!enable_write) return err;
                continue;
            }
            if (enable_write) stdout.writeStreamingAll(runner_threaded_io, "SKIP\n") catch {};
            skipped += 1;
            continue;
        }
        passed += 1;
        report_status[test_index] = 1;
    }
    if (enable_print) {
        var unbuffered_stdout_writer = stdout.writer(runner_threaded_io, &.{});
        unbuffered_stdout_writer.interface.print(
            "{} passed, {} skipped, {} failed\n",
            .{ passed, skipped, failed },
        ) catch {};
    }
    if (failed != 0) std.process.exit(1);
}

var is_fuzz_test: bool = undefined;
var fuzz_runner: if (builtin.fuzz) struct {
    indexes: []u32,
    server: *std.zig.Server,
    gpa: std.mem.Allocator,
    threaded_io: *Io.Threaded,
    input_poller: Io.Future(Io.Cancelable!void),

    comptime {
        assert(builtin.fuzz); // `fuzz_runner` was analyzed in non-fuzzing compilation
    }

    export fn runner_test_run(i: u32) void {
        @disableInstrumentation();

        fuzz_runner.server.serveU32Message(.fuzz_test_change, i) catch |e| switch (e) {
            error.WriteFailed => panic("failed to write to stdout: {t}", .{stdout_writer.err.?}),
        };

        testing.allocator_instance = .init(std.heap.page_allocator, .{
            .canary = 0xc3a701ba,
            .check_write_after_free = true,
        });
        defer if (testing.allocator_instance.deinit() != 0) std.process.exit(1);
        is_fuzz_test = false;

        testing.io_instance = .init(testing.allocator, .{
            .argv0 = fuzz_runner.threaded_io.argv0,
            .environ = fuzz_runner.threaded_io.environ.process_environ,
        });
        defer testing.io_instance.deinit();

        builtin.test_functions[fuzz_runner.indexes[i]].func() catch |err| switch (err) {
            error.SkipZigTest => return,
            else => {
                if (@errorReturnTrace()) |trace| {
                    std.debug.dumpErrorReturnTrace(trace);
                }
                std.debug.print("failed with error.{t}\n", .{err});
                std.process.exit(1);
            },
        };

        if (!is_fuzz_test) @panic("missed call to std.testing.fuzz");
        if (log_err_count != 0) @panic("error logs detected");
    }

    export fn runner_test_name(i: u32) fuzz_abi.Slice {
        @disableInstrumentation();
        return .fromSlice(builtin.test_functions[fuzz_runner.indexes[i]].name);
    }

    export fn runner_broadcast_input(test_i: u32, bytes_slice: fuzz_abi.Slice) void {
        @disableInstrumentation();
        const bytes = bytes_slice.toSlice();
        fuzz_runner.server.serveBroadcastFuzzInputMessage(test_i, bytes) catch |e| switch (e) {
            error.WriteFailed => panic("failed to write to stdout: {t}", .{stdout_writer.err.?}),
        };
    }

    export fn runner_start_input_poller() void {
        @disableInstrumentation();
        const io = fuzz_runner.threaded_io.io();
        const future = io.concurrent(inputPoller, .{}) catch |e| switch (e) {
            error.ConcurrencyUnavailable => @panic("failed to spawn concurrent fuzz input poller"),
        };
        fuzz_runner.input_poller = future;
    }

    export fn runner_stop_input_poller() void {
        @disableInstrumentation();
        const io = fuzz_runner.threaded_io.io();
        assert(fuzz_runner.input_poller.cancel(io) == error.Canceled);
    }

    export fn runner_futex_wait(ptr: *const u32, expected: u32) bool {
        @disableInstrumentation();
        const io = fuzz_runner.threaded_io.io();
        return io.futexWait(u32, ptr, expected) == error.Canceled;
    }

    export fn runner_futex_wake(ptr: *const u32, waiters: u32) void {
        @disableInstrumentation();
        const io = fuzz_runner.threaded_io.io();
        io.futexWake(u32, ptr, waiters);
    }

    fn inputPoller() Io.Cancelable!void {
        @disableInstrumentation();
        switch (inputPollerInner()) {
            error.Canceled => |e| return e,
            error.ReadFailed => {
                if (stdin_reader.err.? == error.Canceled) return error.Canceled;
                panic("failed to read from stdin: {t}", .{stdin_reader.err.?});
            },
            error.EndOfStream => @panic("unexpected end of stdin"),
        }
    }

    fn inputPollerInner() (Io.Cancelable || Io.Reader.Error) {
        @disableInstrumentation();
        const server = fuzz_runner.server;
        var large_bytes_list: std.ArrayList(u8) = .empty;
        defer large_bytes_list.deinit(fuzz_runner.gpa);
        while (true) {
            const hdr = try server.receiveMessage();
            if (hdr.tag != .new_fuzz_input) {
                panic("unexpected message: {x}\n", .{@backingInt(hdr.tag)});
            }
            const test_i = try server.receiveBody_u32();
            const input_len = hdr.bytes_len - 4;
            const bytes = if (input_len <= server.in.buffer.len)
                try server.in.take(input_len)
            else bytes: {
                large_bytes_list.resize(fuzz_runner.gpa, @intCast(input_len)) catch @panic("OOM");
                try server.in.readSliceAll(large_bytes_list.items);
                break :bytes large_bytes_list.items;
            };
            if (fuzz_abi.fuzzer_receive_input(test_i, .fromSlice(bytes))) {
                return error.Canceled;
            }
        }
    }
} else void = undefined;

pub fn fuzz(
    context: anytype,
    comptime testOne: fn (context: @TypeOf(context), *std.testing.Smith) anyerror!void,
    options: testing.FuzzInputOptions,
) anyerror!void {
    // Prevent this function from confusing the fuzzer by omitting its own code
    // coverage from being considered.
    @disableInstrumentation();

    // Some compiler backends are not capable of handling fuzz testing yet but
    // we still want CI test coverage enabled.
    if (need_simple) return;

    // Smoke test to ensure the test did not use conditional compilation to
    // contradict itself by making it not actually be a fuzz test when the test
    // is built in fuzz mode.
    is_fuzz_test = true;

    // Ensure no test failure occurred before starting fuzzing.
    if (log_err_count != 0) @panic("error logs detected");

    // libfuzzer is in a separate compilation unit so that its own code can be
    // excluded from code coverage instrumentation. It needs a function pointer
    // it can call for checking exactly one input. Inside this function we do
    // our standard unit test checks such as memory leaks, and interaction with
    // error logs.
    const global = struct {
        var ctx: @TypeOf(context) = undefined;

        fn test_one() callconv(.c) bool {
            @disableInstrumentation();
            testing.allocator_instance = .init(std.heap.page_allocator, .{
                .canary = 0xcacce5e0,
                .check_write_after_free = true,
            });
            defer if (testing.allocator_instance.deinit() != 0) std.process.exit(1);
            log_err_count = 0;
            testOne(ctx, @constCast(&testing.Smith{ .in = null })) catch |err| switch (err) {
                error.SkipZigTest => return true,
                else => {
                    const stderr = std.debug.lockStderr(&.{}).terminal();
                    p: {
                        if (@errorReturnTrace()) |trace| {
                            std.debug.writeErrorReturnTrace(trace, stderr) catch break :p;
                        }
                        stderr.writer.print("failed with error.{t}\n", .{err}) catch break :p;
                    }
                    std.process.exit(1);
                },
            };
            if (log_err_count != 0) {
                const stderr = std.debug.lockStderr(&.{}).terminal();
                stderr.writer.print("error logs detected\n", .{}) catch {};
                std.process.exit(1);
            }
            return false;
        }
    };

    if (builtin.fuzz) {
        // Preserve the calling test's allocator state
        const prev_allocator_state = testing.allocator_instance;
        defer testing.allocator_instance = prev_allocator_state;

        global.ctx = context;
        fuzz_abi.fuzzer_set_test(&global.test_one);
        for (options.corpus) |elem|
            fuzz_abi.fuzzer_new_input(.fromSlice(elem));
        fuzz_abi.fuzzer_start_test();
        return;
    }

    // When the unit test executable is not built in fuzz mode, only run the
    // provided corpus.
    for (options.corpus) |input| {
        var smith: testing.Smith = .{ .in = input };
        try testOne(context, &smith);
    }

    // In case there is no provided corpus, also use an empty
    // string as a smoke test.
    var smith: testing.Smith = .{ .in = "" };
    try testOne(context, &smith);
}
