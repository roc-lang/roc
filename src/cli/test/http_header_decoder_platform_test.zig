//! Regression test for a zero-allocation HTTP header Decoder platform.

const std = @import("std");
const builtin = @import("builtin");
const util = @import("util.zig");

const testing = std.testing;
const io = std.testing.io;

const required_foo_value = "abcdefghijklmnopqrstuvwxyz";

const optional_headers = [_]OptionalHeader{
    .{ .name = "Explicit-Optional", .value = "abc" },
    .{ .name = "Wildcard-Optional", .value = "vwxyz" },
    .{ .name = "Question-Optional", .value = "1234567" },
};

const OptionalHeader = struct {
    name: []const u8,
    value: []const u8,
};

const invalid_utf8_request =
    "GET /bad-\xff HTTP/1.1\r\n" ++
    "Host: localhost\r\n" ++
    "Content-Length: 0\r\n" ++
    "\r\n";

test "HTTP header Decoder platform derives record decoder without runtime allocations" {
    const target_name = nativeRunnableTargetName() orelse return error.SkipZigTest;

    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_path = try tmp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(tmp_path);

    const exe_name = if (builtin.os.tag == .windows) "http_header_decoder_server.exe" else "http_header_decoder_server";
    const output_path = try std.fs.path.join(allocator, &.{ tmp_path, exe_name });
    defer allocator.free(output_path);

    var env_map = try util.buildIsolatedTestEnvMap(io, allocator, null);
    defer env_map.deinit();

    const target_arg = try std.fmt.allocPrint(allocator, "--target={s}", .{target_name});
    defer allocator.free(target_arg);

    const output_arg = try std.fmt.allocPrint(allocator, "--output={s}", .{output_path});
    defer allocator.free(output_arg);

    const build_result = try util.runChildWithTimeout(io, allocator, &.{
        util.roc_binary_path,
        "build",
        "--opt=speed",
        target_arg,
        output_arg,
        "test/http-headers/app.roc",
    }, .{
        .env_map = &env_map,
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer allocator.free(build_result.stdout);
    defer allocator.free(build_result.stderr);

    switch (build_result.term) {
        .exited => |code| {
            if (code != 0) {
                std.debug.print("roc build failed with exit code {}\nSTDOUT:\n{s}\nSTDERR:\n{s}\n", .{
                    code,
                    build_result.stdout,
                    build_result.stderr,
                });
                return error.RocBuildFailed;
            }
        },
        else => {
            std.debug.print("roc build terminated unexpectedly: {}\nSTDOUT:\n{s}\nSTDERR:\n{s}\n", .{
                build_result.term,
                build_result.stdout,
                build_result.stderr,
            });
            return error.RocBuildFailed;
        },
    }

    for (0..8) |case_index| {
        const mask: u8 = @intCast(case_index);
        const request = try buildRequest(allocator, mask);
        defer allocator.free(request);
        const expected_response = try buildExpectedResponse(allocator, expectedHeaderLength(mask));
        defer allocator.free(expected_response);

        try runServerAndCheckResponse(allocator, output_path, request, expected_response);
    }

    try runServerAndCheckInvalidUtf8(allocator, output_path);
}

fn buildRequest(allocator: std.mem.Allocator, optional_mask: u8) ![]u8 {
    var request: std.ArrayList(u8) = .empty;
    errdefer request.deinit(allocator);

    try request.appendSlice(allocator, "GET /header-lengths HTTP/1.1\r\n");
    try request.appendSlice(allocator, "Host: localhost\r\n");
    try request.appendSlice(allocator, "fOo: ");
    try request.appendSlice(allocator, required_foo_value);
    try request.appendSlice(allocator, "\r\n");

    for (optional_headers, 0..) |header, index| {
        const bit = @as(u8, 1) << @intCast(index);
        if ((optional_mask & bit) == 0) continue;

        try request.appendSlice(allocator, header.name);
        try request.appendSlice(allocator, ": ");
        try request.appendSlice(allocator, header.value);
        try request.appendSlice(allocator, "\r\n");
    }

    try request.appendSlice(allocator, "Content-Length: 0\r\n");
    try request.appendSlice(allocator, "\r\n");

    return request.toOwnedSlice(allocator);
}

fn expectedHeaderLength(optional_mask: u8) u64 {
    var total: u64 = required_foo_value.len;
    for (optional_headers, 0..) |header, index| {
        const bit = @as(u8, 1) << @intCast(index);
        if ((optional_mask & bit) != 0) {
            total += header.value.len;
        }
    }
    return total;
}

fn buildExpectedResponse(allocator: std.mem.Allocator, value: u64) ![]u8 {
    return std.fmt.allocPrint(
        allocator,
        "HTTP/1.1 200 OK\r\n" ++
            "Content-Type: text/plain\r\n" ++
            "Content-Length: {d}\r\n" ++
            "Connection: close\r\n" ++
            "\r\n" ++
            "{d}",
        .{ std.fmt.count("{d}", .{value}), value },
    );
}

fn nativeRunnableTargetName() ?[]const u8 {
    return switch (builtin.os.tag) {
        .macos => switch (builtin.cpu.arch) {
            .x86_64 => "x64mac",
            .aarch64 => "arm64mac",
            else => null,
        },
        .linux => switch (builtin.cpu.arch) {
            .x86_64 => "x64musl",
            .aarch64 => "arm64musl",
            else => null,
        },
        .windows => switch (builtin.cpu.arch) {
            .x86_64 => "x64win",
            .aarch64 => "arm64win",
            else => null,
        },
        else => null,
    };
}

fn runServerAndCheckResponse(allocator: std.mem.Allocator, exe_path: []const u8, request: []const u8, expected_response: []const u8) !void {
    var child = try std.process.spawn(io, .{
        .argv = &.{exe_path},
        .stdin = .ignore,
        .stdout = .pipe,
        .stderr = .pipe,
        .pgid = switch (builtin.os.tag) {
            .windows, .wasi => null,
            else => 0,
        },
    });
    var child_running = true;
    errdefer if (child_running) child.kill(io);

    var watch = Watch{
        .child_id = child.id orelse undefined,
        .timeout_ms = if (child.id == null) 0 else 30 * std.time.ms_per_s,
        .done = std.atomic.Value(bool).init(false),
        .timed_out = std.atomic.Value(bool).init(false),
    };
    const watch_thread = if (watch.timeout_ms == 0)
        null
    else
        try std.Thread.spawn(.{}, Watch.run, .{&watch});
    defer {
        watch.done.store(true, .release);
        if (watch_thread) |thread| thread.join();
    }

    const port = try readPortLine(child.stdout.?);
    const response = try sendHttpRequest(allocator, port, request);
    defer allocator.free(response);

    const stderr = try readRemaining(allocator, child.stderr.?);
    defer allocator.free(stderr);

    const term = try child.wait(io);
    child_running = false;

    if (watch.timed_out.load(.acquire)) {
        std.debug.print("HTTP header decoder server timed out\nSTDERR:\n{s}\n", .{stderr});
        return error.ServerTimedOut;
    }

    switch (term) {
        .exited => |code| {
            if (code != 0) {
                std.debug.print("server exited with code {}\nSTDERR:\n{s}\n", .{ code, stderr });
                return error.ServerFailed;
            }
        },
        else => {
            std.debug.print("server terminated unexpectedly: {}\nSTDERR:\n{s}\n", .{ term, stderr });
            return error.ServerFailed;
        },
    }

    try testing.expectEqualStrings(expected_response, response);
    try expectNoRuntimeAllocation(stderr);
}

fn runServerAndCheckInvalidUtf8(allocator: std.mem.Allocator, exe_path: []const u8) !void {
    var child = try std.process.spawn(io, .{
        .argv = &.{exe_path},
        .stdin = .ignore,
        .stdout = .pipe,
        .stderr = .pipe,
        .pgid = switch (builtin.os.tag) {
            .windows, .wasi => null,
            else => 0,
        },
    });
    var child_running = true;
    errdefer if (child_running) child.kill(io);

    var watch = Watch{
        .child_id = child.id orelse undefined,
        .timeout_ms = if (child.id == null) 0 else 30 * std.time.ms_per_s,
        .done = std.atomic.Value(bool).init(false),
        .timed_out = std.atomic.Value(bool).init(false),
    };
    const watch_thread = if (watch.timeout_ms == 0)
        null
    else
        try std.Thread.spawn(.{}, Watch.run, .{&watch});
    defer {
        watch.done.store(true, .release);
        if (watch_thread) |thread| thread.join();
    }

    const port = try readPortLine(child.stdout.?);
    try sendHttpRequestWithoutReading(port, invalid_utf8_request);

    const stderr = try readRemaining(allocator, child.stderr.?);
    defer allocator.free(stderr);

    const term = try child.wait(io);
    child_running = false;

    if (watch.timed_out.load(.acquire)) {
        std.debug.print("HTTP header decoder invalid UTF-8 server timed out\nSTDERR:\n{s}\n", .{stderr});
        return error.ServerTimedOut;
    }

    switch (term) {
        .exited => |code| {
            if (code == 0) {
                std.debug.print("server accepted invalid UTF-8 request\nSTDERR:\n{s}\n", .{stderr});
                return error.ServerFailed;
            }
        },
        else => {
            std.debug.print("server terminated unexpectedly for invalid UTF-8 request: {}\nSTDERR:\n{s}\n", .{ term, stderr });
            return error.ServerFailed;
        },
    }

    try testing.expect(std.mem.find(u8, stderr, "InvalidUtf8") != null);
    try expectNoRuntimeAllocation(stderr);
}

fn expectNoRuntimeAllocation(stderr: []const u8) !void {
    try testing.expect(std.mem.find(u8, stderr, "roc_alloc called") == null);
    try testing.expect(std.mem.find(u8, stderr, "roc_realloc called") == null);
    try testing.expect(std.mem.find(u8, stderr, "roc_dealloc called") == null);
}

fn readPortLine(stdout: std.Io.File) !u16 {
    var line: [32]u8 = undefined;
    var line_len: usize = 0;

    while (line_len < line.len) {
        var byte_buf: [1]u8 = undefined;
        var slices = [_][]u8{byte_buf[0..]};
        const n = stdout.readStreaming(io, &slices) catch |err| switch (err) {
            error.EndOfStream => return error.ServerExitedBeforePort,
            else => |e| return e,
        };
        if (n == 0) return error.ServerExitedBeforePort;
        if (byte_buf[0] == '\n') break;
        line[line_len] = byte_buf[0];
        line_len += 1;
    } else {
        return error.PortLineTooLong;
    }

    if (line_len == 0) return error.EmptyPortLine;

    var port: u32 = 0;
    for (line[0..line_len]) |byte| {
        if (byte < '0' or byte > '9') return error.InvalidPortLine;
        port = port * 10 + byte - '0';
        if (port > std.math.maxInt(u16)) return error.InvalidPortLine;
    }
    if (port == 0) return error.InvalidPortLine;

    return @intCast(port);
}

fn sendHttpRequest(allocator: std.mem.Allocator, port: u16, bytes: []const u8) ![]u8 {
    const net = std.Io.net;
    var address: net.IpAddress = .{ .ip4 = net.Ip4Address.loopback(port) };
    const stream = try net.IpAddress.connect(&address, io, .{ .mode = .stream });
    defer stream.close(io);

    var write_buffer: [1024]u8 = undefined;
    var writer = stream.writer(io, &write_buffer);
    try writer.interface.writeAll(bytes);
    try writer.interface.flush();

    var response: std.ArrayList(u8) = .empty;
    errdefer response.deinit(allocator);

    var read_buffer: [512]u8 = undefined;
    var stream_buffer: [512]u8 = undefined;
    var reader = stream.reader(io, &stream_buffer);

    while (true) {
        var slices = [_][]u8{read_buffer[0..]};
        const n = std.Io.Reader.readVec(&reader.interface, &slices) catch |err| switch (err) {
            error.EndOfStream => break,
            error.ReadFailed => return reader.err orelse error.Unexpected,
        };
        if (n == 0) break;
        try response.appendSlice(allocator, read_buffer[0..n]);
        if (response.items.len > 1024) return error.ResponseTooLarge;
    }

    return response.toOwnedSlice(allocator);
}

fn sendHttpRequestWithoutReading(port: u16, bytes: []const u8) !void {
    const net = std.Io.net;
    var address: net.IpAddress = .{ .ip4 = net.Ip4Address.loopback(port) };
    const stream = try net.IpAddress.connect(&address, io, .{ .mode = .stream });
    defer stream.close(io);

    var write_buffer: [1024]u8 = undefined;
    var writer = stream.writer(io, &write_buffer);
    try writer.interface.writeAll(bytes);
    try writer.interface.flush();
}

fn readRemaining(allocator: std.mem.Allocator, file: std.Io.File) ![]u8 {
    var output: std.ArrayList(u8) = .empty;
    errdefer output.deinit(allocator);

    var buffer: [512]u8 = undefined;
    while (true) {
        var slices = [_][]u8{buffer[0..]};
        const n = file.readStreaming(io, &slices) catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };
        if (n == 0) break;
        try output.appendSlice(allocator, buffer[0..n]);
        if (output.items.len > 16 * 1024) return error.StderrTooLarge;
    }

    return output.toOwnedSlice(allocator);
}

const Watch = struct {
    child_id: std.process.Child.Id,
    timeout_ms: u64,
    done: std.atomic.Value(bool),
    timed_out: std.atomic.Value(bool),

    fn run(self: *Watch) void {
        const start_ms = milliTimestamp();
        while (!self.done.load(.acquire)) {
            std.Io.sleep(io, std.Io.Duration.fromMilliseconds(100), .awake) catch {};
            if (self.done.load(.acquire)) return;

            const elapsed_ms: u64 = @intCast(@max(0, milliTimestamp() - start_ms));
            if (elapsed_ms >= self.timeout_ms) {
                self.timed_out.store(true, .release);
                terminateChildGroup(self.child_id);
                return;
            }
        }
    }
};

fn milliTimestamp() i64 {
    return std.Io.Timestamp.now(io, .awake).toMilliseconds();
}

fn terminateChildGroup(child_id: std.process.Child.Id) void {
    switch (builtin.os.tag) {
        .windows => {
            const kernel32 = struct {
                extern "kernel32" fn TerminateProcess(hProcess: std.os.windows.HANDLE, uExitCode: c_uint) callconv(.winapi) i32;
            };
            _ = kernel32.TerminateProcess(child_id, 1);
        },
        .wasi => {},
        else => {
            const pid: std.posix.pid_t = child_id;
            std.posix.kill(-pid, std.posix.SIG.KILL) catch {
                std.posix.kill(pid, std.posix.SIG.KILL) catch {};
            };
        },
    }
}
