//! Roc's Zig test runner.
//!
//! The default Zig runner executes every test in a test binary serially. This
//! runner keeps Zig's server-mode protocol and leak detection, but lets the
//! build graph run multiple independent copies of the same test binary. Each
//! copy exposes only the tests whose compiler-provided index belongs to its
//! automatic numeric partition.

const builtin = @import("builtin");
const std = @import("std");

const Io = std.Io;
const testing = std.testing;

pub const std_options: std.Options = .{
    .logFn = log,
};

const runner_io: Io = Io.Threaded.global_single_threaded.io();

var log_err_count: usize = 0;

const RunnerConfig = struct {
    listen: bool = false,
    partition_index: usize = 0,
    partition_count: usize = 1,
};

pub fn main(init: std.process.Init.Minimal) void {
    @disableInstrumentation();

    if (builtin.cpu.arch.isSpirV()) return;
    if (builtin.fuzz) @panic("Roc partitioned test runner does not support fuzz tests");

    const allocator = std.heap.page_allocator;
    const args = init.args.toSlice(allocator) catch |err| {
        std.debug.panic("unable to parse command line args: {t}", .{err});
    };

    const config = parseArgs(args);
    if (config.partition_count == 0 or config.partition_index >= config.partition_count) {
        std.debug.panic(
            "invalid Roc test partition {d}/{d}",
            .{ config.partition_index, config.partition_count },
        );
    }

    if (config.listen) {
        mainServer(init, config) catch |err| {
            std.debug.panic("internal test runner failure: {t}", .{err});
        };
    } else {
        mainTerminal(init, config);
    }
}

fn parseArgs(args: []const []const u8) RunnerConfig {
    var config: RunnerConfig = .{};

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--listen=-")) {
            config.listen = true;
        } else if (std.mem.startsWith(u8, arg, "--seed=")) {
            testing.random_seed = std.fmt.parseUnsigned(u32, arg["--seed=".len..], 0) catch {
                @panic("unable to parse --seed command line argument");
            };
        } else if (std.mem.startsWith(u8, arg, "--cache-dir=")) {
            // Fuzz tests use this. Normal tests do not need it, but Zig's
            // build runner always passes it in server mode.
        } else if (std.mem.startsWith(u8, arg, "--roc-test-partition-index=")) {
            config.partition_index = std.fmt.parseUnsigned(
                usize,
                arg["--roc-test-partition-index=".len..],
                10,
            ) catch @panic("unable to parse --roc-test-partition-index");
        } else if (std.mem.startsWith(u8, arg, "--roc-test-partition-count=")) {
            config.partition_count = std.fmt.parseUnsigned(
                usize,
                arg["--roc-test-partition-count=".len..],
                10,
            ) catch @panic("unable to parse --roc-test-partition-count");
        } else {
            std.debug.panic("unrecognized command line argument: {s}", .{arg});
        }
    }

    return config;
}

fn partitionIncludes(config: RunnerConfig, test_index: usize) bool {
    return test_index % config.partition_count == config.partition_index;
}

fn selectedTestCount(config: RunnerConfig) usize {
    var count: usize = 0;
    for (0..builtin.test_functions.len) |test_i| {
        count += @intFromBool(partitionIncludes(config, test_i));
    }
    return count;
}

fn mainServer(init: std.process.Init.Minimal, config: RunnerConfig) !void {
    @disableInstrumentation();

    const allocator = std.heap.page_allocator;
    const stdin_buffer = try allocator.alloc(u8, 4096);
    defer allocator.free(stdin_buffer);
    const stdout_buffer = try allocator.alloc(u8, 4096);
    defer allocator.free(stdout_buffer);

    var stdin_reader = Io.File.Reader.initStreaming(.stdin(), runner_io, stdin_buffer);
    var stdout_writer = Io.File.Writer.initStreaming(.stdout(), runner_io, stdout_buffer);
    var server = try std.zig.Server.init(.{
        .in = &stdin_reader.interface,
        .out = &stdout_writer.interface,
        .zig_version = builtin.zig_version_string,
    });

    var selected_indices: []u32 = &.{};
    defer if (selected_indices.len != 0) allocator.free(selected_indices);

    while (true) {
        const hdr = try server.receiveMessage();
        switch (hdr.tag) {
            .exit => return std.process.exit(0),

            .query_test_metadata => {
                testing.allocator_instance = .{};
                defer if (testing.allocator_instance.deinit() == .leak) {
                    @panic("internal test runner memory leak");
                };

                if (selected_indices.len != 0) {
                    allocator.free(selected_indices);
                    selected_indices = &.{};
                }

                const selected_count = selectedTestCount(config);
                selected_indices = try allocator.alloc(u32, selected_count);

                var string_bytes: std.ArrayList(u8) = .empty;
                defer string_bytes.deinit(testing.allocator);
                try string_bytes.append(testing.allocator, 0);

                const names = try testing.allocator.alloc(u32, selected_count);
                defer testing.allocator.free(names);
                const expected_panic_msgs = try testing.allocator.alloc(u32, selected_count);
                defer testing.allocator.free(expected_panic_msgs);

                var selected_i: usize = 0;
                for (builtin.test_functions, 0..) |test_fn, test_i| {
                    if (!partitionIncludes(config, test_i)) continue;

                    selected_indices[selected_i] = @intCast(test_i);
                    names[selected_i] = @intCast(string_bytes.items.len);
                    expected_panic_msgs[selected_i] = 0;

                    try string_bytes.ensureUnusedCapacity(testing.allocator, test_fn.name.len + 1);
                    string_bytes.appendSliceAssumeCapacity(test_fn.name);
                    string_bytes.appendAssumeCapacity(0);

                    selected_i += 1;
                }

                try server.serveTestMetadata(.{
                    .names = names,
                    .expected_panic_msgs = expected_panic_msgs,
                    .string_bytes = string_bytes.items,
                });
            },

            .run_test => {
                testing.environ = init.environ;
                testing.allocator_instance = .{};
                testing.io_instance = .init(testing.allocator, .{
                    .argv0 = .init(init.args),
                    .environ = init.environ,
                });
                testing.log_level = .warn;
                log_err_count = 0;

                const metadata_index = try server.receiveBody_u32();
                const test_fn = builtin.test_functions[selected_indices[metadata_index]];

                try server.serveStringMessage(.test_started, &.{});

                const TestResults = std.zig.Server.Message.TestResults;
                const status: TestResults.Status = if (test_fn.func()) |_| status: {
                    break :status .pass;
                } else |err| switch (err) {
                    error.SkipZigTest => .skip,
                    else => status: {
                        if (@errorReturnTrace()) |trace| {
                            std.debug.dumpErrorReturnTrace(trace);
                        }
                        break :status .fail;
                    },
                };

                testing.io_instance.deinit();
                const leak_count = testing.allocator_instance.detectLeaks();
                testing.allocator_instance.deinitWithoutLeakChecks();

                try server.serveTestResults(.{
                    .index = metadata_index,
                    .flags = .{
                        .status = status,
                        .fuzz = false,
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

            else => {
                std.debug.print("unsupported message: {x}\n", .{@intFromEnum(hdr.tag)});
                std.process.exit(1);
            },
        }
    }
}

fn mainTerminal(init: std.process.Init.Minimal, config: RunnerConfig) void {
    @disableInstrumentation();

    const test_fn_list = builtin.test_functions;
    const selected_count = selectedTestCount(config);
    var ok_count: usize = 0;
    var skip_count: usize = 0;
    var fail_count: usize = 0;

    const root_node = std.Progress.start(runner_io, .{
        .root_name = "Test",
        .estimated_total_items = selected_count,
    });
    const have_tty = Io.File.stderr().isTty(runner_io) catch unreachable;

    var leaks: usize = 0;
    var selected_i: usize = 0;
    for (test_fn_list, 0..) |test_fn, test_i| {
        if (!partitionIncludes(config, test_i)) continue;
        selected_i += 1;

        testing.allocator_instance = .{};
        testing.io_instance = .init(testing.allocator, .{
            .argv0 = .init(init.args),
            .environ = init.environ,
        });
        defer {
            testing.io_instance.deinit();
            if (testing.allocator_instance.deinit() == .leak) leaks += 1;
        }
        testing.log_level = .warn;
        testing.environ = init.environ;
        log_err_count = 0;

        const test_node = root_node.start(test_fn.name, 0);
        if (!have_tty) {
            std.debug.print("{d}/{d} {s}...", .{ selected_i, selected_count, test_fn.name });
        }
        if (test_fn.func()) |_| {
            ok_count += 1;
            test_node.end();
            if (!have_tty) std.debug.print("OK\n", .{});
        } else |err| switch (err) {
            error.SkipZigTest => {
                skip_count += 1;
                if (have_tty) {
                    std.debug.print("{d}/{d} {s}...SKIP\n", .{ selected_i, selected_count, test_fn.name });
                } else {
                    std.debug.print("SKIP\n", .{});
                }
                test_node.end();
            },
            else => {
                fail_count += 1;
                if (have_tty) {
                    std.debug.print("{d}/{d} {s}...FAIL ({t})\n", .{
                        selected_i, selected_count, test_fn.name, err,
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
    if (leaks != 0 or log_err_count != 0 or fail_count != 0) {
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
    if (@intFromEnum(message_level) <= @intFromEnum(std.log.Level.err)) {
        log_err_count +|= 1;
    }
    if (@intFromEnum(message_level) <= @intFromEnum(testing.log_level)) {
        std.debug.print(
            "[" ++ @tagName(scope) ++ "] (" ++ @tagName(message_level) ++ "): " ++ format ++ "\n",
            args,
        );
    }
}
