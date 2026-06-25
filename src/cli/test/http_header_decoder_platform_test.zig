//! Regression test for a zero-allocation HTTP header parsing platform.

const std = @import("std");
const builtin = @import("builtin");
const util = @import("util.zig");

/// Explicit (superset) error set for this file's test helpers, composed from the portable
/// named std/util error sets they use plus the custom errors they return. Declaring a
/// broader set than any single helper needs is fine and keeps every helper explicit.
const TestError = util.RocRunError ||
    std.Io.File.ReadStreamingError ||
    std.Io.File.Reader.Error ||
    std.Io.File.Writer.Error ||
    std.fmt.ParseIntError ||
    error{
        TestExpectedEqual,
        TestUnexpectedResult,
        EndOfStream,
        StreamTooLong,
        ReadFailed,
        WriteFailed,
        Unexpected,
        SkipZigTest,
        ServerTimedOut,
        ServerFailed,
        ServerExitedBeforePort,
        ResponseTooLarge,
        StderrTooLarge,
        RocBuildFailed,
        BinaryContainsOriginalFieldName,
        EmptyPortLine,
        InvalidPortLine,
        PortLineTooLong,
        // net.IpAddress.connect (sendHttpRequest)
        AddressFamilyUnsupported,
        AddressUnavailable,
        ConnectionPending,
        ConnectionRefused,
        HostUnreachable,
        NetworkDown,
        NetworkUnreachable,
        OptionUnsupported,
        ProtocolUnsupportedByAddressFamily,
        ProtocolUnsupportedBySystem,
        SocketModeUnsupported,
    };

const testing = std.testing;
const io = std.testing.io;

const required_foo_value = "abcdefghijklmnopqrstuvwxyz";
const cache_control_value = "no-cache";
const request_body = "hello";
const request_count_value: u64 = 17;
const long_unknown_header_name = "X-Super-Long-Unknown-Header-Name-That-Would-Allocate-If-Converted";

const optional_headers = [_]OptionalHeader{
    .{ .name = "Explicit-Optional", .value = "abc" },
    .{ .name = "Wildcard-Optional", .value = "vwxyz" },
    .{ .name = "Question-Optional", .value = "1234567" },
    .{ .name = "X-Auth-Token", .value = "token1234" },
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

const too_large_content_length_request =
    "POST /too-large HTTP/1.1\r\n" ++
    "Host: localhost\r\n" ++
    "Foo: abcdefghijklmnopqrstuvwxyz\r\n" ++
    "Content-Length: 1025\r\n" ++
    "\r\n";

test "HTTP header parsing platform derives structural parser without runtime allocations" {
    const target_name = nativeRunnableTargetName() orelse return error.SkipZigTest;

    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_path = try tmp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(tmp_path);

    const prebuilt_path = try getEnvVarOwnedOrNull(allocator, "ROC_HTTP_HEADER_DECODER_PREBUILT_EXE");

    const exe_name = if (builtin.os.tag == .windows) "http_header_decoder_server.exe" else "http_header_decoder_server";
    const output_path = if (prebuilt_path) |path|
        path
    else
        try std.fs.path.join(allocator, &.{ tmp_path, exe_name });
    defer allocator.free(output_path);

    if (prebuilt_path == null) {
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
    }

    try expectBinaryOmits(allocator, output_path, &.{ "cache_control", "content_length", "request_count", "x_auth_token" });

    for (0..(@as(usize, 1) << optional_headers.len)) |case_index| {
        const mask: u8 = @intCast(case_index);
        const request = try buildRequest(allocator, mask);
        defer allocator.free(request);
        const expected_response = try buildExpectedResponse(allocator, expectedHeaderLength(mask));
        defer allocator.free(expected_response);

        try runServerAndCheckResponse(allocator, output_path, request, expected_response);
    }

    const all_optional_mask: u8 = (@as(u8, 1) << optional_headers.len) - 1;

    const record_order_request = try buildKnownHeadersRecordOrderRequest(allocator);
    defer allocator.free(record_order_request);
    const record_order_response = try buildExpectedResponse(allocator, expectedHeaderLength(all_optional_mask));
    defer allocator.free(record_order_response);
    try runServerAndCheckResponse(allocator, output_path, record_order_request, record_order_response);

    const reverse_order_request = try buildKnownHeadersReverseOrderRequest(allocator);
    defer allocator.free(reverse_order_request);
    const reverse_order_response = try buildExpectedResponse(allocator, expectedHeaderLength(all_optional_mask));
    defer allocator.free(reverse_order_response);
    try runServerAndCheckResponse(allocator, output_path, reverse_order_request, reverse_order_response);

    const scrambled_order_request = try buildKnownHeadersScrambledOrderRequest(allocator);
    defer allocator.free(scrambled_order_request);
    const scrambled_order_response = try buildExpectedResponse(allocator, expectedHeaderLength(all_optional_mask));
    defer allocator.free(scrambled_order_response);
    try runServerAndCheckResponse(allocator, output_path, scrambled_order_request, scrambled_order_response);

    const lower_case_request = try buildCacheControlCaseRequest(allocator, "cache-control");
    defer allocator.free(lower_case_request);
    const lower_case_response = try buildExpectedResponse(allocator, expectedHeaderLength(0));
    defer allocator.free(lower_case_response);
    try runServerAndCheckResponse(allocator, output_path, lower_case_request, lower_case_response);

    const upper_case_request = try buildCacheControlCaseRequest(allocator, "CACHE-CONTROL");
    defer allocator.free(upper_case_request);
    const upper_case_response = try buildExpectedResponse(allocator, expectedHeaderLength(0));
    defer allocator.free(upper_case_response);
    try runServerAndCheckResponse(allocator, output_path, upper_case_request, upper_case_response);

    const mixed_case_request = try buildCacheControlCaseRequest(allocator, "cAcHe-CoNtRoL");
    defer allocator.free(mixed_case_request);
    const mixed_case_response = try buildExpectedResponse(allocator, expectedHeaderLength(0));
    defer allocator.free(mixed_case_response);
    try runServerAndCheckResponse(allocator, output_path, mixed_case_request, mixed_case_response);

    const large_body_request = try buildLargeBodyRequest(allocator);
    defer allocator.free(large_body_request);
    const large_body_response = try buildExpectedResponse(allocator, 123 + request_count_value + cache_control_value.len + required_foo_value.len);
    defer allocator.free(large_body_response);
    try runServerAndCheckResponse(allocator, output_path, large_body_request, large_body_response);

    const duplicate_known_request = try buildDuplicateKnownRequest(allocator);
    defer allocator.free(duplicate_known_request);
    const duplicate_known_response = try buildExpectedResponse(allocator, request_body.len + request_count_value + cache_control_value.len + required_foo_value.len);
    defer allocator.free(duplicate_known_response);
    try runServerAndCheckResponse(allocator, output_path, duplicate_known_request, duplicate_known_response);

    const missing_required_request = try buildMissingRequiredRequest(allocator);
    defer allocator.free(missing_required_request);
    const missing_required_response = try buildExpectedResponse(allocator, 999999);
    defer allocator.free(missing_required_response);
    try runServerAndCheckResponse(allocator, output_path, missing_required_request, missing_required_response);

    const bad_header_request = try buildBadHeaderRequest(allocator);
    defer allocator.free(bad_header_request);
    const bad_header_response = try buildExpectedResponse(allocator, 999999);
    defer allocator.free(bad_header_response);
    try runServerAndCheckResponse(allocator, output_path, bad_header_request, bad_header_response);

    const empty_request_count = try buildInvalidRequestCountRequest(allocator, "");
    defer allocator.free(empty_request_count);
    const invalid_request_count_response = try buildExpectedResponse(allocator, 999999);
    defer allocator.free(invalid_request_count_response);
    try runServerAndCheckResponse(allocator, output_path, empty_request_count, invalid_request_count_response);

    const nondigit_request_count = try buildInvalidRequestCountRequest(allocator, "12x");
    defer allocator.free(nondigit_request_count);
    try runServerAndCheckResponse(allocator, output_path, nondigit_request_count, invalid_request_count_response);

    const overflow_request_count = try buildInvalidRequestCountRequest(allocator, "18446744073709551616");
    defer allocator.free(overflow_request_count);
    try runServerAndCheckResponse(allocator, output_path, overflow_request_count, invalid_request_count_response);

    const empty_content_length = try buildInvalidContentLengthRequest(allocator, "");
    defer allocator.free(empty_content_length);
    try runServerAndCheckRequestFailure(allocator, output_path, empty_content_length, "BadContentLength");

    const nondigit_content_length = try buildInvalidContentLengthRequest(allocator, "12x");
    defer allocator.free(nondigit_content_length);
    try runServerAndCheckRequestFailure(allocator, output_path, nondigit_content_length, "BadContentLength");

    const overflow_content_length = try buildInvalidContentLengthRequest(allocator, "18446744073709551616");
    defer allocator.free(overflow_content_length);
    try runServerAndCheckRequestFailure(allocator, output_path, overflow_content_length, "BadContentLength");

    try runServerAndCheckInvalidUtf8(allocator, output_path);
    try runServerAndCheckRequestFailure(allocator, output_path, too_large_content_length_request, "RequestTooLarge");
}

fn buildRequest(allocator: std.mem.Allocator, optional_mask: u8) TestError![]u8 {
    var request: std.ArrayList(u8) = .empty;
    errdefer request.deinit(allocator);

    try request.appendSlice(allocator, "GET /header-lengths HTTP/1.1\r\n");
    try request.appendSlice(allocator, "Host: localhost\r\n");
    try appendHeader(&request, allocator, long_unknown_header_name, "ignored");
    try appendHeader(&request, allocator, "Cache-Control", cache_control_value);

    if ((optional_mask & 1) == 0) {
        try appendHeader(&request, allocator, "fOo", required_foo_value);

        for (optional_headers, 0..) |header, index| {
            const bit = @as(u8, 1) << @intCast(index);
            if ((optional_mask & bit) == 0) continue;

            try appendHeader(&request, allocator, header.name, header.value);
        }
    } else {
        var index = optional_headers.len;
        while (index > 0) {
            index -= 1;
            const bit = @as(u8, 1) << @intCast(index);
            if ((optional_mask & bit) == 0) continue;

            try appendHeader(&request, allocator, optional_headers[index].name, optional_headers[index].value);
        }

        try appendHeader(&request, allocator, "fOo", required_foo_value);
    }

    try appendHeader(&request, allocator, "Content-Length", "5");
    try appendHeader(&request, allocator, "Request-Count", "17");
    try request.appendSlice(allocator, "\r\n");
    try request.appendSlice(allocator, request_body);

    return request.toOwnedSlice(allocator);
}

fn getEnvVarOwnedOrNull(allocator: std.mem.Allocator, key: []const u8) TestError!?[]u8 {
    const key_z = try allocator.dupeZ(u8, key);
    defer allocator.free(key_z);
    const value = std.c.getenv(key_z) orelse return null;
    return try allocator.dupe(u8, value[0..std.mem.len(value)]);
}

fn buildKnownHeadersRecordOrderRequest(allocator: std.mem.Allocator) TestError![]u8 {
    var request: std.ArrayList(u8) = .empty;
    errdefer request.deinit(allocator);

    try request.appendSlice(allocator, "GET /record-order HTTP/1.1\r\n");
    try request.appendSlice(allocator, "Host: localhost\r\n");
    try appendHeader(&request, allocator, "Cache-Control", cache_control_value);
    try appendHeader(&request, allocator, "Content-Length", "5");
    try appendHeader(&request, allocator, optional_headers[0].name, optional_headers[0].value);
    try appendHeader(&request, allocator, "Foo", required_foo_value);
    try appendHeader(&request, allocator, optional_headers[2].name, optional_headers[2].value);
    try appendHeader(&request, allocator, "Request-Count", "17");
    try appendHeader(&request, allocator, optional_headers[1].name, optional_headers[1].value);
    try appendHeader(&request, allocator, optional_headers[3].name, optional_headers[3].value);
    try request.appendSlice(allocator, "\r\n");
    try request.appendSlice(allocator, request_body);

    return request.toOwnedSlice(allocator);
}

fn buildKnownHeadersReverseOrderRequest(allocator: std.mem.Allocator) TestError![]u8 {
    var request: std.ArrayList(u8) = .empty;
    errdefer request.deinit(allocator);

    try request.appendSlice(allocator, "GET /reverse-order HTTP/1.1\r\n");
    try request.appendSlice(allocator, "Host: localhost\r\n");
    try appendHeader(&request, allocator, optional_headers[3].name, optional_headers[3].value);
    try appendHeader(&request, allocator, optional_headers[1].name, optional_headers[1].value);
    try appendHeader(&request, allocator, "Request-Count", "17");
    try appendHeader(&request, allocator, optional_headers[2].name, optional_headers[2].value);
    try appendHeader(&request, allocator, "Foo", required_foo_value);
    try appendHeader(&request, allocator, optional_headers[0].name, optional_headers[0].value);
    try appendHeader(&request, allocator, "Content-Length", "5");
    try appendHeader(&request, allocator, "Cache-Control", cache_control_value);
    try request.appendSlice(allocator, "\r\n");
    try request.appendSlice(allocator, request_body);

    return request.toOwnedSlice(allocator);
}

fn buildKnownHeadersScrambledOrderRequest(allocator: std.mem.Allocator) TestError![]u8 {
    var request: std.ArrayList(u8) = .empty;
    errdefer request.deinit(allocator);

    try request.appendSlice(allocator, "GET /scrambled-order HTTP/1.1\r\n");
    try request.appendSlice(allocator, "Host: localhost\r\n");
    try appendHeader(&request, allocator, "Request-Count", "17");
    try appendHeader(&request, allocator, "X-Unknown-Before", "ignored");
    try appendHeader(&request, allocator, optional_headers[2].name, optional_headers[2].value);
    try appendHeader(&request, allocator, "Cache-Control", cache_control_value);
    try appendHeader(&request, allocator, long_unknown_header_name, "ignored");
    try appendHeader(&request, allocator, optional_headers[0].name, optional_headers[0].value);
    try appendHeader(&request, allocator, "Foo", required_foo_value);
    try appendHeader(&request, allocator, "X-Unknown-Middle", "ignored");
    try appendHeader(&request, allocator, optional_headers[3].name, optional_headers[3].value);
    try appendHeader(&request, allocator, "Content-Length", "5");
    try appendHeader(&request, allocator, optional_headers[1].name, optional_headers[1].value);
    try appendHeader(&request, allocator, "X-Unknown-After", "ignored");
    try request.appendSlice(allocator, "\r\n");
    try request.appendSlice(allocator, request_body);

    return request.toOwnedSlice(allocator);
}

fn buildCacheControlCaseRequest(allocator: std.mem.Allocator, cache_control_name: []const u8) TestError![]u8 {
    var request: std.ArrayList(u8) = .empty;
    errdefer request.deinit(allocator);

    try request.appendSlice(allocator, "GET /cache-control-case HTTP/1.1\r\n");
    try request.appendSlice(allocator, "Host: localhost\r\n");
    try appendHeader(&request, allocator, cache_control_name, cache_control_value);
    try appendHeader(&request, allocator, "Foo", required_foo_value);
    try appendHeader(&request, allocator, "Content-Length", "5");
    try appendHeader(&request, allocator, "Request-Count", "17");
    try request.appendSlice(allocator, "\r\n");
    try request.appendSlice(allocator, request_body);

    return request.toOwnedSlice(allocator);
}

fn buildLargeBodyRequest(allocator: std.mem.Allocator) TestError![]u8 {
    var request: std.ArrayList(u8) = .empty;
    errdefer request.deinit(allocator);

    try request.appendSlice(allocator, "POST /large-body HTTP/1.1\r\n");
    try request.appendSlice(allocator, "Host: localhost\r\n");
    try appendHeader(&request, allocator, "Cache-Control", cache_control_value);
    try appendHeader(&request, allocator, "Foo", required_foo_value);
    try appendHeader(&request, allocator, "Request-Count", "17");
    try appendHeader(&request, allocator, "Content-Length", "123");
    try request.appendSlice(allocator, "\r\n");
    try request.appendNTimes(allocator, 'x', 123);

    return request.toOwnedSlice(allocator);
}

fn buildDuplicateKnownRequest(allocator: std.mem.Allocator) TestError![]u8 {
    var request: std.ArrayList(u8) = .empty;
    errdefer request.deinit(allocator);

    try request.appendSlice(allocator, "GET /duplicate-known HTTP/1.1\r\n");
    try request.appendSlice(allocator, "Host: localhost\r\n");
    try appendHeader(&request, allocator, "Foo", "short");
    try appendHeader(&request, allocator, "Cache-Control", "stale");
    try appendHeader(&request, allocator, "Request-Count", "1");
    try appendHeader(&request, allocator, "fOo", required_foo_value);
    try appendHeader(&request, allocator, "Cache-Control", cache_control_value);
    try appendHeader(&request, allocator, "Request-Count", "17");
    try appendHeader(&request, allocator, "Content-Length", "5");
    try request.appendSlice(allocator, "\r\n");
    try request.appendSlice(allocator, request_body);

    return request.toOwnedSlice(allocator);
}

fn appendHeader(request: *std.ArrayList(u8), allocator: std.mem.Allocator, name: []const u8, value: []const u8) TestError!void {
    try request.appendSlice(allocator, name);
    try request.appendSlice(allocator, ": ");
    try request.appendSlice(allocator, value);
    try request.appendSlice(allocator, "\r\n");
}

fn buildMissingRequiredRequest(allocator: std.mem.Allocator) TestError![]u8 {
    var request: std.ArrayList(u8) = .empty;
    errdefer request.deinit(allocator);

    try request.appendSlice(allocator, "GET /missing-required HTTP/1.1\r\n");
    try request.appendSlice(allocator, "Host: localhost\r\n");
    try appendHeader(&request, allocator, "Cache-Control", cache_control_value);
    try appendHeader(&request, allocator, "Request-Count", "17");
    try request.appendSlice(allocator, "Content-Length: 0\r\n");
    try request.appendSlice(allocator, "\r\n");

    return request.toOwnedSlice(allocator);
}

fn buildBadHeaderRequest(allocator: std.mem.Allocator) TestError![]u8 {
    var request: std.ArrayList(u8) = .empty;
    errdefer request.deinit(allocator);

    try request.appendSlice(allocator, "GET /bad-header HTTP/1.1\r\n");
    try request.appendSlice(allocator, "Host: localhost\r\n");
    try appendHeader(&request, allocator, "Cache-Control", cache_control_value);
    try request.appendSlice(allocator, "Foo: ");
    try request.appendSlice(allocator, required_foo_value);
    try request.appendSlice(allocator, "\r\n");
    try appendHeader(&request, allocator, "Request-Count", "17");
    try request.appendSlice(allocator, "BrokenHeaderLine\r\n");
    try request.appendSlice(allocator, "Content-Length: 0\r\n");
    try request.appendSlice(allocator, "\r\n");

    return request.toOwnedSlice(allocator);
}

fn buildInvalidRequestCountRequest(allocator: std.mem.Allocator, value: []const u8) TestError![]u8 {
    var request: std.ArrayList(u8) = .empty;
    errdefer request.deinit(allocator);

    try request.appendSlice(allocator, "GET /bad-request-count HTTP/1.1\r\n");
    try request.appendSlice(allocator, "Host: localhost\r\n");
    try appendHeader(&request, allocator, "Cache-Control", cache_control_value);
    try appendHeader(&request, allocator, "Foo", required_foo_value);
    try appendHeader(&request, allocator, "Request-Count", value);
    try request.appendSlice(allocator, "Content-Length: 0\r\n");
    try request.appendSlice(allocator, "\r\n");

    return request.toOwnedSlice(allocator);
}

fn buildInvalidContentLengthRequest(allocator: std.mem.Allocator, value: []const u8) TestError![]u8 {
    var request: std.ArrayList(u8) = .empty;
    errdefer request.deinit(allocator);

    try request.appendSlice(allocator, "GET /bad-content-length HTTP/1.1\r\n");
    try request.appendSlice(allocator, "Host: localhost\r\n");
    try appendHeader(&request, allocator, "Cache-Control", cache_control_value);
    try appendHeader(&request, allocator, "Foo", required_foo_value);
    try appendHeader(&request, allocator, "Request-Count", "17");
    try appendHeader(&request, allocator, "Content-Length", value);
    try request.appendSlice(allocator, "\r\n");

    return request.toOwnedSlice(allocator);
}

fn expectedHeaderLength(optional_mask: u8) u64 {
    var total: u64 = request_body.len + request_count_value + cache_control_value.len + required_foo_value.len;
    for (optional_headers, 0..) |header, index| {
        const bit = @as(u8, 1) << @intCast(index);
        if ((optional_mask & bit) != 0) {
            total += header.value.len;
        }
    }
    return total;
}

fn buildExpectedResponse(allocator: std.mem.Allocator, value: u64) TestError![]u8 {
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

fn runServerAndCheckResponse(allocator: std.mem.Allocator, exe_path: []const u8, request: []const u8, expected_response: []const u8) TestError!void {
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
        std.debug.print("HTTP header parser server timed out\nSTDERR:\n{s}\n", .{stderr});
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

fn runServerAndCheckInvalidUtf8(allocator: std.mem.Allocator, exe_path: []const u8) TestError!void {
    try runServerAndCheckRequestFailure(allocator, exe_path, invalid_utf8_request, "InvalidUtf8");
}

fn runServerAndCheckRequestFailure(allocator: std.mem.Allocator, exe_path: []const u8, request: []const u8, expected_error: []const u8) TestError!void {
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
    try sendHttpRequestWithoutReading(port, request);

    const stderr = try readRemaining(allocator, child.stderr.?);
    defer allocator.free(stderr);

    const term = try child.wait(io);
    child_running = false;

    if (watch.timed_out.load(.acquire)) {
        std.debug.print("HTTP header parser failure case timed out\nSTDERR:\n{s}\n", .{stderr});
        return error.ServerTimedOut;
    }

    switch (term) {
        .exited => |code| {
            if (code == 0) {
                std.debug.print("server accepted invalid request\nSTDERR:\n{s}\n", .{stderr});
                return error.ServerFailed;
            }
        },
        else => {
            std.debug.print("server terminated unexpectedly for invalid request: {}\nSTDERR:\n{s}\n", .{ term, stderr });
            return error.ServerFailed;
        },
    }

    try testing.expect(std.mem.find(u8, stderr, expected_error) != null);
    try expectNoRuntimeAllocation(stderr);
}

fn expectNoRuntimeAllocation(stderr: []const u8) TestError!void {
    try testing.expect(std.mem.find(u8, stderr, "roc_alloc called") == null);
    try testing.expect(std.mem.find(u8, stderr, "roc_realloc called") == null);
    try testing.expect(std.mem.find(u8, stderr, "roc_dealloc called") == null);
}

fn expectBinaryOmits(allocator: std.mem.Allocator, exe_path: []const u8, needles: []const []const u8) TestError!void {
    const bytes = try std.Io.Dir.cwd().readFileAlloc(io, exe_path, allocator, .limited(256 * 1024 * 1024));
    defer allocator.free(bytes);

    for (needles) |needle| {
        if (std.mem.find(u8, bytes, needle)) |_| {
            std.debug.print("optimized app binary {s} unexpectedly contains {s}\n", .{ exe_path, needle });
            return error.BinaryContainsOriginalFieldName;
        }
    }
}

fn readPortLine(stdout: std.Io.File) TestError!u16 {
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

fn sendHttpRequest(allocator: std.mem.Allocator, port: u16, bytes: []const u8) TestError![]u8 {
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

fn sendHttpRequestWithoutReading(port: u16, bytes: []const u8) TestError!void {
    const net = std.Io.net;
    var address: net.IpAddress = .{ .ip4 = net.Ip4Address.loopback(port) };
    const stream = try net.IpAddress.connect(&address, io, .{ .mode = .stream });
    defer stream.close(io);

    var write_buffer: [1024]u8 = undefined;
    var writer = stream.writer(io, &write_buffer);
    try writer.interface.writeAll(bytes);
    try writer.interface.flush();
}

fn readRemaining(allocator: std.mem.Allocator, file: std.Io.File) TestError![]u8 {
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
