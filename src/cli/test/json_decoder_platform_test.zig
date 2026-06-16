//! Regression test for a zero-allocation JSON parsing platform.

const std = @import("std");
const builtin = @import("builtin");
const util = @import("util.zig");

const testing = std.testing;
const io = std.testing.io;

const required_foo_value = "abcdefghijklmnopqrstuvwxyz";
const nested_bar_value = "nested-bar-value";
const token_input_value = "original-token-value";
const custom_token_value = "custom-token";
const status_values = [_][]const u8{ "Active", "Paused" };
const status_scores = [_]u64{ 11, 17 };
const mode_values = [_][]const u8{ "Warm", "Cold" };
const mode_scores = [_]u64{ 19, 23 };
const empty_record_score: u64 = 29;

const optional_fields = [_]OptionalField{
    .{ .name = "explicit_optional", .value = "abc" },
    .{ .name = "wildcard_optional", .value = "vwxyz" },
    .{ .name = "question_optional", .value = "1234567" },
};

const OptionalField = struct {
    name: []const u8,
    value: []const u8,
};

test "JSON parsing platform derives structural parser without runtime allocations" {
    const target_name = nativeRunnableTargetName() orelse return error.SkipZigTest;

    const allocator = testing.allocator;

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_path = try tmp_dir.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(tmp_path);

    const exe_name = if (builtin.os.tag == .windows) "json_decoder.exe" else "json_decoder";
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
        "test/json-decoder/app.roc",
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
        const status_index = case_index % status_values.len;
        const mode_index = (case_index / status_values.len) % mode_values.len;
        const json = try buildJson(allocator, mask, status_index, mode_index);
        defer allocator.free(json);
        const expected_stdout = try buildExpectedStdout(allocator, expectedJsonLength(mask, status_index, mode_index));
        defer allocator.free(expected_stdout);

        try runJsonDecoderAndCheckOutput(allocator, output_path, json, expected_stdout);
    }

    const missing_required_json = try buildMissingRequiredJson(allocator);
    defer allocator.free(missing_required_json);
    const missing_required_stdout = try buildExpectedStdout(allocator, 999999);
    defer allocator.free(missing_required_stdout);
    try runJsonDecoderAndCheckOutput(allocator, output_path, missing_required_json, missing_required_stdout);

    try runJsonDecoderAndCheckInvalidUtf8(allocator, output_path);
}

fn buildJson(
    allocator: std.mem.Allocator,
    optional_mask: u8,
    status_index: usize,
    mode_index: usize,
) ![]u8 {
    var json: std.ArrayList(u8) = .empty;
    errdefer json.deinit(allocator);

    try json.appendSlice(allocator, "{\n  \"foo\" : \"");
    try json.appendSlice(allocator, required_foo_value);
    try json.appendSlice(allocator, "\"");
    try json.appendSlice(allocator, ",\n  \"token\" : \"");
    try json.appendSlice(allocator, token_input_value);
    try json.appendSlice(allocator, "\"");
    try json.appendSlice(allocator, ",\n  \"status\" : \"");
    try json.appendSlice(allocator, status_values[status_index]);
    try json.appendSlice(allocator, "\"");
    try json.appendSlice(allocator, ",\n  \"nested\" : {\n    \"bar\" : \"");
    try json.appendSlice(allocator, nested_bar_value);
    try json.appendSlice(allocator, "\",\n    \"mode\" : \"");
    try json.appendSlice(allocator, mode_values[mode_index]);
    try json.appendSlice(allocator, "\"\n  }");

    for (optional_fields, 0..) |field, index| {
        const bit = @as(u8, 1) << @intCast(index);
        if ((optional_mask & bit) == 0) continue;

        try json.appendSlice(allocator, ",\n  \"");
        try json.appendSlice(allocator, field.name);
        try json.appendSlice(allocator, "\" : \"");
        try json.appendSlice(allocator, field.value);
        try json.appendSlice(allocator, "\"");
    }

    try json.appendSlice(allocator, "\n}\n");

    return json.toOwnedSlice(allocator);
}

fn buildMissingRequiredJson(allocator: std.mem.Allocator) ![]u8 {
    var json: std.ArrayList(u8) = .empty;
    errdefer json.deinit(allocator);

    try json.appendSlice(allocator, "{\n");
    try json.appendSlice(allocator, "  \"status\" : \"Active\",\n");
    try json.appendSlice(allocator, "  \"nested\" : {\n");
    try json.appendSlice(allocator, "    \"bar\" : \"");
    try json.appendSlice(allocator, nested_bar_value);
    try json.appendSlice(allocator, "\",\n");
    try json.appendSlice(allocator, "    \"mode\" : \"Warm\"\n");
    try json.appendSlice(allocator, "  }\n");
    try json.appendSlice(allocator, "}\n");

    return json.toOwnedSlice(allocator);
}

fn expectedJsonLength(optional_mask: u8, status_index: usize, mode_index: usize) u64 {
    var total: u64 = required_foo_value.len +
        nested_bar_value.len +
        custom_token_value.len +
        empty_record_score +
        status_scores[status_index] +
        mode_scores[mode_index];
    for (optional_fields, 0..) |field, index| {
        const bit = @as(u8, 1) << @intCast(index);
        if ((optional_mask & bit) != 0) {
            total += field.value.len;
        }
    }
    return total;
}

fn buildExpectedStdout(allocator: std.mem.Allocator, value: u64) ![]u8 {
    return std.fmt.allocPrint(allocator, "{d}\n", .{value});
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

fn runJsonDecoderAndCheckOutput(allocator: std.mem.Allocator, exe_path: []const u8, input: []const u8, expected_stdout: []const u8) !void {
    const result = try util.runChildWithTimeout(io, allocator, &.{exe_path}, .{
        .stdin = input,
        .max_output_bytes = 16 * 1024,
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    switch (result.term) {
        .exited => |code| {
            if (code != 0) {
                std.debug.print("json parser exited with code {}\nSTDOUT:\n{s}\nSTDERR:\n{s}\n", .{
                    code,
                    result.stdout,
                    result.stderr,
                });
                return error.JsonDecoderFailed;
            }
        },
        else => {
            std.debug.print("json parser terminated unexpectedly: {}\nSTDOUT:\n{s}\nSTDERR:\n{s}\n", .{
                result.term,
                result.stdout,
                result.stderr,
            });
            return error.JsonDecoderFailed;
        },
    }

    try testing.expectEqualStrings(expected_stdout, result.stdout);
    try expectNoRuntimeAllocation(result.stderr);
}

fn runJsonDecoderAndCheckInvalidUtf8(allocator: std.mem.Allocator, exe_path: []const u8) !void {
    const invalid_utf8_json = "{\"foo\":\"bad\xff\"}";
    const result = try util.runChildWithTimeout(io, allocator, &.{exe_path}, .{
        .stdin = invalid_utf8_json,
        .max_output_bytes = 16 * 1024,
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    switch (result.term) {
        .exited => |code| {
            if (code == 0) {
                std.debug.print("json parser accepted invalid UTF-8\nSTDOUT:\n{s}\nSTDERR:\n{s}\n", .{
                    result.stdout,
                    result.stderr,
                });
                return error.JsonDecoderFailed;
            }
        },
        else => {
            std.debug.print("json parser terminated unexpectedly for invalid UTF-8: {}\nSTDOUT:\n{s}\nSTDERR:\n{s}\n", .{
                result.term,
                result.stdout,
                result.stderr,
            });
            return error.JsonDecoderFailed;
        },
    }

    try testing.expect(std.mem.find(u8, result.stderr, "InvalidUtf8") != null);
    try expectNoRuntimeAllocation(result.stderr);
}

fn expectNoRuntimeAllocation(stderr: []const u8) !void {
    try testing.expect(std.mem.find(u8, stderr, "roc_alloc called") == null);
    try testing.expect(std.mem.find(u8, stderr, "roc_realloc called") == null);
    try testing.expect(std.mem.find(u8, stderr, "roc_dealloc called") == null);
}
