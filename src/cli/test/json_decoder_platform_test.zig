//! Regression test for a zero-allocation JSON parsing platform.

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
        JsonDecoderFailed,
        RocBuildFailed,
        BinaryContainsOriginalFieldName,
    };

const testing = std.testing;
const io = std.testing.io;

const required_foo_value = "abcdefghijklmnopqrstuvwxyz,with-comma";
const nested_bar_value = "nested-}-bar,value";
const long_unknown_key = "this_unknown_key_is_far_too_long_for_the_target_record";
const token_input_value = "original-token-value";
const custom_token_value = "custom-token";
const status_values = [_][]const u8{ "Active", "Paused" };
const status_scores = [_]u64{ 11, 17 };
const mode_values = [_][]const u8{ "Warm", "Cold" };
const mode_scores = [_]u64{ 19, 23 };
const top_level_string_value = "top-level-json";
const empty_record_score: u64 = 29;
const invalid_empty_record_score: u64 = 37;
const pair_score: u64 = 31;
const trailing_empty_record_score: u64 = 41;
const invalid_string_score: u64 = 43;
const null_string_score: u64 = 47;
const strict_trailing_comma_score: u64 = 53;
const lenient_trailing_comma_score: u64 = 59;
const strict_tag_trailing_comma_score: u64 = 67;
const lenient_tag_trailing_comma_score: u64 = 71;
const unknown_array_score: u64 = 61;
const strict_unknown_array_trailing_comma_score: u64 = 73;
const lenient_unknown_array_trailing_comma_score: u64 = 79;
const invalid_unknown_scalar_score: u64 = 83;
const invalid_unknown_array_scalar_score: u64 = 89;
const invalid_u64_plus_score: u64 = 97;
const invalid_u64_leading_zero_score: u64 = 101;
const invalid_missing_tag_payload_score: u64 = 103;

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

    const prebuilt_path = try getEnvVarOwnedOrNull(allocator, "ROC_JSON_DECODER_PREBUILT_EXE");
    const exe_name = if (builtin.os.tag == .windows) "json_decoder.exe" else "json_decoder";
    const output_path = if (prebuilt_path) |path|
        path
    else
        try std.fs.path.join(allocator, &.{ tmp_path, exe_name });
    defer allocator.free(output_path);

    const camel_prebuilt_path = try getEnvVarOwnedOrNull(allocator, "ROC_JSON_DECODER_CAMEL_PREBUILT_EXE");
    const camel_exe_name = if (builtin.os.tag == .windows) "json_decoder_camel.exe" else "json_decoder_camel";
    const camel_output_path = if (camel_prebuilt_path) |path|
        path
    else
        try std.fs.path.join(allocator, &.{ tmp_path, camel_exe_name });
    defer allocator.free(camel_output_path);

    const camel_direct_prebuilt_path = try getEnvVarOwnedOrNull(allocator, "ROC_JSON_DECODER_CAMEL_DIRECT_PREBUILT_EXE");
    const camel_direct_exe_name = if (builtin.os.tag == .windows) "json_decoder_camel_direct.exe" else "json_decoder_camel_direct";
    const camel_direct_output_path = if (camel_direct_prebuilt_path) |path|
        path
    else
        try std.fs.path.join(allocator, &.{ tmp_path, camel_direct_exe_name });
    defer allocator.free(camel_direct_output_path);

    var env_map = try util.buildIsolatedTestEnvMap(io, allocator, null);
    defer env_map.deinit();

    if (prebuilt_path == null) {
        try buildRocApp(allocator, &env_map, target_name, output_path, "test/json-decoder/app.roc");
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

    const record_order_json = try buildRecordOrderJson(allocator);
    defer allocator.free(record_order_json);
    const record_order_expected_stdout = try buildExpectedStdout(allocator, expectedJsonLength(7, 0, 0));
    defer allocator.free(record_order_expected_stdout);
    try runJsonDecoderAndCheckOutput(allocator, output_path, record_order_json, record_order_expected_stdout);

    const reverse_order_json = try buildReverseOrderJson(allocator);
    defer allocator.free(reverse_order_json);
    const reverse_order_expected_stdout = try buildExpectedStdout(allocator, expectedJsonLength(7, 1, 1));
    defer allocator.free(reverse_order_expected_stdout);
    try runJsonDecoderAndCheckOutput(allocator, output_path, reverse_order_json, reverse_order_expected_stdout);

    const reordered_json = try buildReorderedJson(allocator);
    defer allocator.free(reordered_json);
    const reordered_expected_stdout = try buildExpectedStdout(allocator, expectedJsonLength(7, 1, 1));
    defer allocator.free(reordered_expected_stdout);
    try runJsonDecoderAndCheckOutput(allocator, output_path, reordered_json, reordered_expected_stdout);

    const duplicate_json = try buildDuplicateJson(allocator);
    defer allocator.free(duplicate_json);
    const duplicate_expected_stdout = try buildExpectedStdout(allocator, expectedJsonLength(0, 1, 1));
    defer allocator.free(duplicate_expected_stdout);
    try runJsonDecoderAndCheckOutput(allocator, output_path, duplicate_json, duplicate_expected_stdout);

    const missing_required_json = try buildMissingRequiredJson(allocator);
    defer allocator.free(missing_required_json);
    const missing_required_stdout = try buildExpectedStdout(allocator, 999999);
    defer allocator.free(missing_required_stdout);
    try runJsonDecoderAndCheckOutput(allocator, output_path, missing_required_json, missing_required_stdout);

    try runJsonDecoderAndCheckInvalidUtf8(allocator, output_path);

    if (camel_prebuilt_path == null) {
        try buildRocApp(allocator, &env_map, target_name, camel_output_path, "test/json-decoder/camel_app.roc");
    }
    try expectBinaryOmits(allocator, camel_output_path, &.{ "cache_control", "first_value", "inner_value", "nested_record", "second_value", "user_id" });
    try runJsonDecoderAndCheckOutput(
        allocator,
        camel_output_path,
        "{ \"cacheControl\" : \"no-cache\", \"nestedRecord\" : { \"innerValue\" : \"xyz\" }, \"pair\" : { \"Pair\" : { \"firstValue\" : \"left\", \"secondValue\" : \"right\" } }, \"userId\" : \"abc\" }\n",
        "23\n",
    );

    if (camel_direct_prebuilt_path == null) {
        try buildRocApp(allocator, &env_map, target_name, camel_direct_output_path, "test/json-decoder/camel_direct_app.roc");
    }
    try runJsonDecoderAndCheckOutput(
        allocator,
        camel_direct_output_path,
        "{ \"cacheControl\" : \"no-cache\", \"userId\" : \"abc\" }\n",
        "11\n",
    );
}

fn getEnvVarOwnedOrNull(allocator: std.mem.Allocator, key: []const u8) TestError!?[]u8 {
    const key_z = try allocator.dupeZ(u8, key);
    defer allocator.free(key_z);
    const value = std.c.getenv(key_z) orelse return null;
    return try allocator.dupe(u8, value[0..std.mem.len(value)]);
}

fn buildRocApp(
    allocator: std.mem.Allocator,
    env_map: *const std.process.Environ.Map,
    target_name: []const u8,
    output_path: []const u8,
    roc_file: []const u8,
) TestError!void {
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
        roc_file,
    }, .{
        .env_map = env_map,
        .max_output_bytes = 10 * 1024 * 1024,
    });
    defer allocator.free(build_result.stdout);
    defer allocator.free(build_result.stderr);

    switch (build_result.term) {
        .exited => |code| {
            if (code != 0) {
                std.debug.print("roc build failed for {s} with exit code {}\nSTDOUT:\n{s}\nSTDERR:\n{s}\n", .{
                    roc_file,
                    code,
                    build_result.stdout,
                    build_result.stderr,
                });
                return error.RocBuildFailed;
            }
        },
        else => {
            std.debug.print("roc build for {s} terminated unexpectedly: {}\nSTDOUT:\n{s}\nSTDERR:\n{s}\n", .{
                roc_file,
                build_result.term,
                build_result.stdout,
                build_result.stderr,
            });
            return error.RocBuildFailed;
        },
    }
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

fn buildJson(
    allocator: std.mem.Allocator,
    optional_mask: u8,
    status_index: usize,
    mode_index: usize,
) TestError![]u8 {
    var json: std.ArrayList(u8) = .empty;
    errdefer json.deinit(allocator);

    try json.appendSlice(allocator, "{\n  \"");
    try json.appendSlice(allocator, long_unknown_key);
    try json.appendSlice(allocator, "\" : \"ignored\"");
    try json.appendSlice(allocator, ",\n  \"foo\" : \"");
    try json.appendSlice(allocator, required_foo_value);
    try json.appendSlice(allocator, "\"");
    try json.appendSlice(allocator, ",\n  \"token\" : \"");
    try json.appendSlice(allocator, token_input_value);
    try json.appendSlice(allocator, "\"");
    try json.appendSlice(allocator, ",\n  \"status\" : { \"");
    try json.appendSlice(allocator, status_values[status_index]);
    try json.appendSlice(allocator, "\" : {} }");
    try json.appendSlice(allocator, ",\n  \"pair\" : { \"Pair\" : { \"first\" : \"left\", \"second\" : \"right\" } }");
    try json.appendSlice(allocator, ",\n  \"nested\" : {\n    \"bar\" : \"");
    try json.appendSlice(allocator, nested_bar_value);
    try json.appendSlice(allocator, "\",\n    \"mode\" : { \"");
    try json.appendSlice(allocator, mode_values[mode_index]);
    try json.appendSlice(allocator, "\" : {} }\n  }");

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

fn buildRecordOrderJson(allocator: std.mem.Allocator) TestError![]u8 {
    var json: std.ArrayList(u8) = .empty;
    errdefer json.deinit(allocator);

    try json.appendSlice(allocator, "{\n");
    try json.appendSlice(allocator, "  \"explicit_optional\" : \"");
    try json.appendSlice(allocator, optional_fields[0].value);
    try json.appendSlice(allocator, "\",\n  \"foo\" : \"");
    try json.appendSlice(allocator, required_foo_value);
    try json.appendSlice(allocator, "\",\n  \"nested\" : {\n    \"bar\" : \"");
    try json.appendSlice(allocator, nested_bar_value);
    try json.appendSlice(allocator, "\",\n    \"mode\" : { \"Warm\" : {} }\n  },\n");
    try json.appendSlice(allocator, "  \"pair\" : { \"Pair\" : { \"first\" : \"left\", \"second\" : \"right\" } },\n");
    try json.appendSlice(allocator, "  \"question_optional\" : \"");
    try json.appendSlice(allocator, optional_fields[2].value);
    try json.appendSlice(allocator, "\",\n  \"status\" : { \"Active\" : {} },\n");
    try json.appendSlice(allocator, "  \"token\" : \"");
    try json.appendSlice(allocator, token_input_value);
    try json.appendSlice(allocator, "\",\n  \"wildcard_optional\" : \"");
    try json.appendSlice(allocator, optional_fields[1].value);
    try json.appendSlice(allocator, "\"\n}\n");

    return json.toOwnedSlice(allocator);
}

fn buildReverseOrderJson(allocator: std.mem.Allocator) TestError![]u8 {
    var json: std.ArrayList(u8) = .empty;
    errdefer json.deinit(allocator);

    try json.appendSlice(allocator, "{\n");
    try json.appendSlice(allocator, "  \"wildcard_optional\" : \"");
    try json.appendSlice(allocator, optional_fields[1].value);
    try json.appendSlice(allocator, "\",\n  \"token\" : \"");
    try json.appendSlice(allocator, token_input_value);
    try json.appendSlice(allocator, "\",\n  \"status\" : { \"Paused\" : {} },\n");
    try json.appendSlice(allocator, "  \"question_optional\" : \"");
    try json.appendSlice(allocator, optional_fields[2].value);
    try json.appendSlice(allocator, "\",\n  \"pair\" : { \"Pair\" : { \"second\" : \"right\", \"first\" : \"left\" } },\n");
    try json.appendSlice(allocator, "  \"nested\" : {\n    \"mode\" : { \"Cold\" : {} },\n    \"bar\" : \"");
    try json.appendSlice(allocator, nested_bar_value);
    try json.appendSlice(allocator, "\"\n  },\n  \"foo\" : \"");
    try json.appendSlice(allocator, required_foo_value);
    try json.appendSlice(allocator, "\",\n  \"explicit_optional\" : \"");
    try json.appendSlice(allocator, optional_fields[0].value);
    try json.appendSlice(allocator, "\"\n}\n");

    return json.toOwnedSlice(allocator);
}

fn buildReorderedJson(allocator: std.mem.Allocator) TestError![]u8 {
    var json: std.ArrayList(u8) = .empty;
    errdefer json.deinit(allocator);

    try json.appendSlice(allocator, "{\n  \"wildcard_optional\" : \"");
    try json.appendSlice(allocator, optional_fields[1].value);
    try json.appendSlice(allocator, "\"");
    try json.appendSlice(allocator, ",\n  \"token\" : \"");
    try json.appendSlice(allocator, token_input_value);
    try json.appendSlice(allocator, "\"");
    try json.appendSlice(allocator, ",\n  \"status\" : { \"");
    try json.appendSlice(allocator, status_values[1]);
    try json.appendSlice(allocator, "\" : {} }");
    try json.appendSlice(allocator, ",\n  \"question_optional\" : \"");
    try json.appendSlice(allocator, optional_fields[2].value);
    try json.appendSlice(allocator, "\"");
    try json.appendSlice(allocator, ",\n  \"pair\" : { \"Pair\" : { \"second\" : \"right\", \"first\" : \"left\" } }");
    try json.appendSlice(allocator, ",\n  \"nested\" : {\n    \"mode\" : { \"");
    try json.appendSlice(allocator, mode_values[1]);
    try json.appendSlice(allocator, "\" : {} },\n    \"");
    try json.appendSlice(allocator, long_unknown_key);
    try json.appendSlice(allocator, "\" : \"ignored\",\n    \"bar\" : \"");
    try json.appendSlice(allocator, nested_bar_value);
    try json.appendSlice(allocator, "\"\n  }");
    try json.appendSlice(allocator, ",\n  \"foo\" : \"");
    try json.appendSlice(allocator, required_foo_value);
    try json.appendSlice(allocator, "\"");
    try json.appendSlice(allocator, ",\n  \"explicit_optional\" : \"");
    try json.appendSlice(allocator, optional_fields[0].value);
    try json.appendSlice(allocator, "\"");
    try json.appendSlice(allocator, ",\n  \"");
    try json.appendSlice(allocator, long_unknown_key);
    try json.appendSlice(allocator, "\" : { \"nested\" : \"ignored\" }");
    try json.appendSlice(allocator, "\n}\n");

    return json.toOwnedSlice(allocator);
}

fn buildDuplicateJson(allocator: std.mem.Allocator) TestError![]u8 {
    var json: std.ArrayList(u8) = .empty;
    errdefer json.deinit(allocator);

    try json.appendSlice(allocator, "{\n");
    try json.appendSlice(allocator, "  \"foo\" : \"short\",\n");
    try json.appendSlice(allocator, "  \"nested\" : { \"bar\" : \"old\", \"mode\" : { \"Warm\" : {} } },\n");
    try json.appendSlice(allocator, "  \"token\" : \"old-token\",\n");
    try json.appendSlice(allocator, "  \"status\" : { \"Active\" : {} },\n");
    try json.appendSlice(allocator, "  \"pair\" : { \"Pair\" : { \"first\" : \"left\", \"second\" : \"right\" } },\n");
    try json.appendSlice(allocator, "  \"foo\" : \"");
    try json.appendSlice(allocator, required_foo_value);
    try json.appendSlice(allocator, "\",\n");
    try json.appendSlice(allocator, "  \"token\" : \"");
    try json.appendSlice(allocator, token_input_value);
    try json.appendSlice(allocator, "\",\n");
    try json.appendSlice(allocator, "  \"nested\" : { \"bar\" : \"");
    try json.appendSlice(allocator, nested_bar_value);
    try json.appendSlice(allocator, "\", \"mode\" : { \"Cold\" : {} } },\n");
    try json.appendSlice(allocator, "  \"status\" : { \"Paused\" : {} }\n");
    try json.appendSlice(allocator, "}\n");

    return json.toOwnedSlice(allocator);
}

fn buildMissingRequiredJson(allocator: std.mem.Allocator) TestError![]u8 {
    var json: std.ArrayList(u8) = .empty;
    errdefer json.deinit(allocator);

    try json.appendSlice(allocator, "{\n");
    try json.appendSlice(allocator, "  \"status\" : { \"Active\" : {} },\n");
    try json.appendSlice(allocator, "  \"pair\" : { \"Pair\" : { \"first\" : \"left\", \"second\" : \"right\" } },\n");
    try json.appendSlice(allocator, "  \"nested\" : {\n");
    try json.appendSlice(allocator, "    \"bar\" : \"");
    try json.appendSlice(allocator, nested_bar_value);
    try json.appendSlice(allocator, "\",\n");
    try json.appendSlice(allocator, "    \"mode\" : { \"Warm\" : {} }\n");
    try json.appendSlice(allocator, "  }\n");
    try json.appendSlice(allocator, "}\n");

    return json.toOwnedSlice(allocator);
}

fn expectedJsonLength(optional_mask: u8, status_index: usize, mode_index: usize) u64 {
    var total: u64 = required_foo_value.len +
        nested_bar_value.len +
        custom_token_value.len +
        top_level_string_value.len +
        empty_record_score +
        invalid_empty_record_score +
        pair_score +
        trailing_empty_record_score +
        invalid_string_score +
        null_string_score +
        strict_trailing_comma_score +
        lenient_trailing_comma_score +
        strict_tag_trailing_comma_score +
        lenient_tag_trailing_comma_score +
        unknown_array_score +
        strict_unknown_array_trailing_comma_score +
        lenient_unknown_array_trailing_comma_score +
        invalid_unknown_scalar_score +
        invalid_unknown_array_scalar_score +
        invalid_u64_plus_score +
        invalid_u64_leading_zero_score +
        invalid_missing_tag_payload_score +
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

fn buildExpectedStdout(allocator: std.mem.Allocator, value: u64) TestError![]u8 {
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

fn runJsonDecoderAndCheckOutput(allocator: std.mem.Allocator, exe_path: []const u8, input: []const u8, expected_stdout: []const u8) TestError!void {
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

fn runJsonDecoderAndCheckInvalidUtf8(allocator: std.mem.Allocator, exe_path: []const u8) TestError!void {
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

fn expectNoRuntimeAllocation(stderr: []const u8) TestError!void {
    try testing.expect(std.mem.find(u8, stderr, "roc_alloc called") == null);
    try testing.expect(std.mem.find(u8, stderr, "roc_realloc called") == null);
    try testing.expect(std.mem.find(u8, stderr, "roc_dealloc called") == null);
}
