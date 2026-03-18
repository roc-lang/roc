const builtin = @import("builtin");
const std = @import("std");
const testing = std.testing;

pub const std_options: std.Options = .{
    .logFn = log,
};

var log_err_count: usize = 0;
var stdin_buffer: [4096]u8 = undefined;
var stdout_buffer: [4096]u8 = undefined;

fn writeCurrentTest(name: []const u8) void {
    const file = std.fs.cwd().createFile(".context/current_eval_test.txt", .{
        .truncate = true,
    }) catch return;
    defer file.close();
    file.writeAll(name) catch return;
    file.writeAll("\n") catch return;
}

pub fn main() void {
    @disableInstrumentation();

    const allocator = std.heap.page_allocator;
    const args = std.process.argsAlloc(allocator) catch @panic("unable to parse command line args");
    defer std.process.argsFree(allocator, args);

    var listen = false;
    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--listen=-")) {
            listen = true;
        } else if (std.mem.startsWith(u8, arg, "--seed=")) {
            testing.random_seed = std.fmt.parseUnsigned(u32, arg["--seed=".len..], 0) catch
                @panic("unable to parse --seed command line argument");
        } else if (std.mem.startsWith(u8, arg, "--cache-dir=")) {
            continue;
        } else {
            @panic("unrecognized command line argument");
        }
    }

    if (listen) {
        mainServer() catch @panic("internal test runner failure");
    } else {
        mainTerminal();
    }
}

fn mainServer() !void {
    @disableInstrumentation();

    var stdin_reader = std.fs.File.stdin().readerStreaming(&stdin_buffer);
    var stdout_writer = std.fs.File.stdout().writerStreaming(&stdout_buffer);
    var server = try std.zig.Server.init(.{
        .in = &stdin_reader.interface,
        .out = &stdout_writer.interface,
        .zig_version = builtin.zig_version_string,
    });

    while (true) {
        const hdr = try server.receiveMessage();
        switch (hdr.tag) {
            .exit => std.process.exit(0),
            .query_test_metadata => {
                testing.allocator_instance = .{};
                defer if (testing.allocator_instance.deinit() == .leak) {
                    @panic("internal test runner memory leak");
                };

                var string_bytes: std.ArrayListUnmanaged(u8) = .empty;
                defer string_bytes.deinit(testing.allocator);
                try string_bytes.append(testing.allocator, 0);

                const test_fns = builtin.test_functions;
                const names = try testing.allocator.alloc(u32, test_fns.len);
                defer testing.allocator.free(names);
                const expected_panic_msgs = try testing.allocator.alloc(u32, test_fns.len);
                defer testing.allocator.free(expected_panic_msgs);

                for (test_fns, names, expected_panic_msgs) |test_fn, *name, *expected_panic_msg| {
                    name.* = @intCast(string_bytes.items.len);
                    try string_bytes.ensureUnusedCapacity(testing.allocator, test_fn.name.len + 1);
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
                testing.allocator_instance = .{};
                log_err_count = 0;
                testing.log_level = .warn;

                const index = try server.receiveBody_u32();
                const test_fn = builtin.test_functions[index];
                writeCurrentTest(test_fn.name);
                std.debug.print("RUN {d}/{d}: {s}\n", .{ index + 1, builtin.test_functions.len, test_fn.name });

                var fail = false;
                var skip = false;
                test_fn.func() catch |err| switch (err) {
                    error.SkipZigTest => skip = true,
                    else => {
                        fail = true;
                        if (@errorReturnTrace()) |trace| {
                            std.debug.dumpStackTrace(trace.*);
                        }
                    },
                };

                const leak = testing.allocator_instance.deinit() == .leak;
                try server.serveTestResults(.{
                    .index = index,
                    .flags = .{
                        .fail = fail,
                        .skip = skip,
                        .leak = leak,
                        .fuzz = false,
                        .log_err_count = std.math.lossyCast(
                            @FieldType(std.zig.Server.Message.TestResults.Flags, "log_err_count"),
                            log_err_count,
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

fn mainTerminal() void {
    @disableInstrumentation();

    var ok_count: usize = 0;
    var skip_count: usize = 0;
    var fail_count: usize = 0;
    var leak_count: usize = 0;

    for (builtin.test_functions, 0..) |test_fn, i| {
        testing.allocator_instance = .{};
        log_err_count = 0;
        testing.log_level = .warn;

        writeCurrentTest(test_fn.name);
        std.debug.print("RUN {d}/{d}: {s}\n", .{ i + 1, builtin.test_functions.len, test_fn.name });
        if (test_fn.func()) |_| {
            ok_count += 1;
        } else |err| switch (err) {
            error.SkipZigTest => skip_count += 1,
            else => {
                fail_count += 1;
                if (@errorReturnTrace()) |trace| {
                    std.debug.dumpStackTrace(trace.*);
                }
                std.debug.print("FAIL ({s})\n", .{@errorName(err)});
            },
        }

        if (testing.allocator_instance.deinit() == .leak) {
            leak_count += 1;
        }
    }

    std.debug.print(
        "{d} passed; {d} skipped; {d} failed; {d} leaked.\n",
        .{ ok_count, skip_count, fail_count, leak_count },
    );
    if (fail_count != 0 or leak_count != 0 or log_err_count != 0) {
        std.process.exit(1);
    }
}

pub fn log(
    comptime message_level: std.log.Level,
    comptime scope: @Type(.enum_literal),
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
