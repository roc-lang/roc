//! MiniCI runner for split build/run jobs.

const std = @import("std");

const out_dir = "zig-out/minici";
const raw_dir = out_dir ++ "/raw";
const logs_dir = out_dir ++ "/logs";

const JobKind = enum {
    single,
    harness,
};

const Job = struct {
    name: []const u8,
    kind: JobKind = .single,
};

const jobs = [_]Job{
    .{ .name = "run-check-zig-format" },
    .{ .name = "run-check-zig-lints" },
    .{ .name = "run-check-tidy" },
    .{ .name = "run-check-git-lints" },
    .{ .name = "run-check-type-checker-patterns" },
    .{ .name = "run-check-enum-from-int-zero" },
    .{ .name = "run-check-unused-suppression" },
    .{ .name = "run-check-semantic-audit" },
    .{ .name = "run-check-postcheck-architecture" },
    .{ .name = "run-check-panic" },
    .{ .name = "run-check-cli-global-stdio" },
    .{ .name = "run-check-test-wiring" },
    .{ .name = "run-check-builtin-format" },
    .{ .name = "run-check-snapshots" },
    .{ .name = "run-check-fx-platform-test-coverage" },
    .{ .name = "run-test-zig" },
    .{ .name = "run-test-eval", .kind = .harness },
    .{ .name = "run-test-eval-host-effects", .kind = .harness },
    .{ .name = "run-test-playground", .kind = .harness },
    .{ .name = "run-test-cli-platforms", .kind = .harness },
    .{ .name = "run-test-cli-subcommands" },
    .{ .name = "run-test-cli-echo" },
    .{ .name = "run-test-cli-glue" },
    .{ .name = "run-test-serialization-sizes" },
    .{ .name = "run-coverage-parser" },
};

const CommandResult = struct {
    status: []const u8,
    duration_ns: u64,
    log_path: []const u8,
    command: []const []const u8,
    build_violation: bool = false,
    stats_path: ?[]const u8 = null,
};

fn nowNs(io: std.Io) u64 {
    return @intCast(@max(0, std.Io.Timestamp.now(io, .awake).nanoseconds));
}

fn durationSince(io: std.Io, started: u64) u64 {
    return nowNs(io) -| started;
}

fn writeFile(io: std.Io, path: []const u8, bytes: []const u8) !void {
    if (std.fs.path.dirname(path)) |dir| {
        try std.Io.Dir.cwd().createDirPath(io, dir);
    }
    var file = try std.Io.Dir.cwd().createFile(io, path, .{});
    defer file.close(io);
    try file.writeStreamingAll(io, bytes);
}

fn appendJsonString(out: *std.ArrayList(u8), allocator: std.mem.Allocator, value: []const u8) !void {
    try out.append(allocator, '"');
    for (value) |byte| {
        switch (byte) {
            '"' => try out.appendSlice(allocator, "\\\""),
            '\\' => try out.appendSlice(allocator, "\\\\"),
            '\n' => try out.appendSlice(allocator, "\\n"),
            '\r' => try out.appendSlice(allocator, "\\r"),
            '\t' => try out.appendSlice(allocator, "\\t"),
            else => {
                if (byte < 0x20) {
                    const escaped = try std.fmt.allocPrint(allocator, "\\u{x:0>4}", .{byte});
                    defer allocator.free(escaped);
                    try out.appendSlice(allocator, escaped);
                } else {
                    try out.append(allocator, byte);
                }
            },
        }
    }
    try out.append(allocator, '"');
}

fn appendU64(out: *std.ArrayList(u8), allocator: std.mem.Allocator, value: u64) !void {
    const text = try std.fmt.allocPrint(allocator, "{d}", .{value});
    defer allocator.free(text);
    try out.appendSlice(allocator, text);
}

fn commandText(allocator: std.mem.Allocator, argv: []const []const u8) ![]const u8 {
    var out = std.ArrayList(u8).empty;
    for (argv, 0..) |arg, i| {
        if (i > 0) try out.append(allocator, ' ');
        try out.appendSlice(allocator, arg);
    }
    return try out.toOwnedSlice(allocator);
}

fn detectRunPhaseBuildViolation(log: []const u8) bool {
    var lines = std.mem.splitScalar(u8, log, '\n');
    var in_summary = false;
    while (lines.next()) |line| {
        if (std.mem.startsWith(u8, line, "Build Summary:")) {
            in_summary = true;
            continue;
        }
        if (!in_summary) continue;
        const lower_contains_compile = std.mem.indexOf(u8, line, "Compile") != null or
            std.mem.indexOf(u8, line, "compile") != null;
        const lower_contains_install = std.mem.indexOf(u8, line, "Install") != null or
            std.mem.indexOf(u8, line, "install") != null;
        if (!lower_contains_compile and !lower_contains_install) continue;
        if (std.mem.indexOf(u8, line, "cached") != null) continue;
        if (std.mem.indexOf(u8, line, "Cache Hit") != null) continue;
        return true;
    }
    return false;
}

fn runCommand(
    allocator: std.mem.Allocator,
    io: std.Io,
    argv: []const []const u8,
    log_path: []const u8,
    check_build_violation: bool,
) !CommandResult {
    const started = nowNs(io);
    const result = std.process.run(allocator, io, .{ .argv = argv }) catch |err| {
        const message = try std.fmt.allocPrint(allocator, "spawn failed: {s}\n", .{@errorName(err)});
        try writeFile(io, log_path, message);
        return .{
            .status = "crash",
            .duration_ns = durationSince(io, started),
            .log_path = log_path,
            .command = argv,
        };
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    var log = std.ArrayList(u8).empty;
    defer log.deinit(allocator);
    try log.appendSlice(allocator, result.stdout);
    try log.appendSlice(allocator, result.stderr);
    try writeFile(io, log_path, log.items);

    const status: []const u8 = switch (result.term) {
        .exited => |code| if (code == 0) "pass" else "fail",
        else => "crash",
    };

    const violation = check_build_violation and detectRunPhaseBuildViolation(log.items);
    return .{
        .status = if (violation and std.mem.eql(u8, status, "pass")) "fail" else status,
        .duration_ns = durationSince(io, started),
        .log_path = log_path,
        .command = argv,
        .build_violation = violation,
    };
}

fn buildCommand(
    allocator: std.mem.Allocator,
    zig_exe: []const u8,
    step: []const u8,
    stats_path: ?[]const u8,
) ![]const []const u8 {
    var argv = std.ArrayList([]const u8).empty;
    try argv.append(allocator, zig_exe);
    try argv.append(allocator, "build");
    try argv.append(allocator, step);
    try argv.append(allocator, "--summary");
    try argv.append(allocator, "all");
    try argv.append(allocator, "--color");
    try argv.append(allocator, "off");
    if (stats_path) |path| {
        try argv.append(allocator, "--");
        try argv.append(allocator, "--stats-json");
        try argv.append(allocator, path);
    }
    return try argv.toOwnedSlice(allocator);
}

fn appendCommandJson(out: *std.ArrayList(u8), allocator: std.mem.Allocator, command: []const []const u8) !void {
    try out.appendSlice(allocator, "[");
    for (command, 0..) |arg, i| {
        if (i > 0) try out.appendSlice(allocator, ", ");
        try appendJsonString(out, allocator, arg);
    }
    try out.appendSlice(allocator, "]");
}

fn writeReportJson(
    allocator: std.mem.Allocator,
    io: std.Io,
    build_result: CommandResult,
    results: []const CommandResult,
) !void {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(allocator);

    try out.appendSlice(allocator, "{\n  \"schema_version\": 1,\n  \"build_ci\": ");
    try appendResultJson(&out, allocator, build_result);
    try out.appendSlice(allocator, ",\n  \"jobs\": [\n");
    for (results, 0..) |result, i| {
        if (i > 0) try out.appendSlice(allocator, ",\n");
        try appendResultJson(&out, allocator, result);
    }
    try out.appendSlice(allocator, "\n  ]\n}\n");
    try writeFile(io, out_dir ++ "/report.json", out.items);
}

fn appendResultJson(out: *std.ArrayList(u8), allocator: std.mem.Allocator, result: CommandResult) !void {
    try out.appendSlice(allocator, "{\n    \"status\": ");
    try appendJsonString(out, allocator, result.status);
    try out.appendSlice(allocator, ",\n    \"duration_ns\": ");
    try appendU64(out, allocator, result.duration_ns);
    try out.appendSlice(allocator, ",\n    \"log_path\": ");
    try appendJsonString(out, allocator, result.log_path);
    try out.appendSlice(allocator, ",\n    \"command\": ");
    try appendCommandJson(out, allocator, result.command);
    try out.appendSlice(allocator, ",\n    \"build_violation\": ");
    try out.appendSlice(allocator, if (result.build_violation) "true" else "false");
    try out.appendSlice(allocator, ",\n    \"stats_path\": ");
    if (result.stats_path) |path| {
        try appendJsonString(out, allocator, path);
    } else {
        try out.appendSlice(allocator, "null");
    }
    try out.appendSlice(allocator, "\n  }");
}

fn writeHtml(
    allocator: std.mem.Allocator,
    io: std.Io,
    build_result: CommandResult,
    results: []const CommandResult,
) !void {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(allocator);

    try out.appendSlice(allocator,
        \\<!doctype html>
        \\<meta charset="utf-8">
        \\<title>MiniCI</title>
        \\<style>
        \\body{font-family:system-ui,-apple-system,Segoe UI,sans-serif;margin:24px;background:#f7f7f8;color:#18181b}
        \\table{border-collapse:collapse;width:100%;background:white;border:1px solid #ddd}
        \\th,td{padding:8px 10px;border-bottom:1px solid #eee;text-align:left;font-size:14px}
        \\th{background:#f0f0f2}
        \\.pass{color:#167a35}.fail,.crash{color:#b42318}.skip{color:#6b7280}
        \\code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:12px}
        \\</style>
        \\<h1>MiniCI</h1>
        \\<table>
        \\<thead><tr><th>Job</th><th>Status</th><th>Duration</th><th>Build Violation</th><th>Outputs</th></tr></thead>
        \\<tbody>
    );
    try appendHtmlRow(&out, allocator, "build-ci", build_result);
    for (results) |result| {
        const name = result.command[2];
        try appendHtmlRow(&out, allocator, name, result);
    }
    try out.appendSlice(allocator, "</tbody></table>\n");
    try writeFile(io, out_dir ++ "/index.html", out.items);
}

fn appendHtmlEscaped(out: *std.ArrayList(u8), allocator: std.mem.Allocator, value: []const u8) !void {
    for (value) |byte| {
        switch (byte) {
            '&' => try out.appendSlice(allocator, "&amp;"),
            '<' => try out.appendSlice(allocator, "&lt;"),
            '>' => try out.appendSlice(allocator, "&gt;"),
            '"' => try out.appendSlice(allocator, "&quot;"),
            else => try out.append(allocator, byte),
        }
    }
}

fn appendHtmlRow(out: *std.ArrayList(u8), allocator: std.mem.Allocator, name: []const u8, result: CommandResult) !void {
    try out.appendSlice(allocator, "<tr><td><code>");
    try appendHtmlEscaped(out, allocator, name);
    try out.appendSlice(allocator, "</code></td><td class=\"");
    try appendHtmlEscaped(out, allocator, result.status);
    try out.appendSlice(allocator, "\">");
    try appendHtmlEscaped(out, allocator, result.status);
    try out.appendSlice(allocator, "</td><td>");
    const seconds = @as(f64, @floatFromInt(result.duration_ns)) / 1_000_000_000.0;
    const duration = try std.fmt.allocPrint(allocator, "{d:.1}s", .{seconds});
    defer allocator.free(duration);
    try appendHtmlEscaped(out, allocator, duration);
    try out.appendSlice(allocator, "</td><td>");
    try out.appendSlice(allocator, if (result.build_violation) "yes" else "");
    try out.appendSlice(allocator, "</td><td><code>");
    try appendHtmlEscaped(out, allocator, result.log_path);
    if (result.stats_path) |path| {
        try out.appendSlice(allocator, " ");
        try appendHtmlEscaped(out, allocator, path);
    }
    try out.appendSlice(allocator, "</code></td></tr>\n");
}

/// Runs build-ci followed by each named MiniCI run job and writes reports.
pub fn main(init: std.process.Init) !void {
    const io = init.io;
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const allocator = gpa_impl.allocator();

    const raw_args = try init.minimal.args.toSlice(allocator);
    const args: []const []const u8 = @ptrCast(raw_args);
    const zig_exe = if (args.len >= 2) args[1] else "zig";

    std.Io.Dir.cwd().deleteTree(io, out_dir) catch {};
    try std.Io.Dir.cwd().createDirPath(io, raw_dir);
    try std.Io.Dir.cwd().createDirPath(io, logs_dir);

    const build_argv = try buildCommand(allocator, zig_exe, "build-ci", null);
    const build_log = logs_dir ++ "/build-ci.txt";
    std.debug.print("minici start: build-ci\n", .{});
    const build_result = try runCommand(allocator, io, build_argv, build_log, false);
    std.debug.print("minici done: build-ci ({s})\n", .{build_result.status});

    var results = std.ArrayList(CommandResult).empty;
    defer results.deinit(allocator);

    if (!std.mem.eql(u8, build_result.status, "pass")) {
        try writeReportJson(allocator, io, build_result, results.items);
        try writeHtml(allocator, io, build_result, results.items);
        std.process.exit(1);
    }

    for (jobs) |job| {
        const log_path = try std.fmt.allocPrint(allocator, "{s}/{s}.txt", .{ logs_dir, job.name });
        const stats_path: ?[]const u8 = if (job.kind == .harness)
            try std.fmt.allocPrint(allocator, "{s}/{s}.json", .{ raw_dir, job.name })
        else
            null;
        const argv = try buildCommand(allocator, zig_exe, job.name, stats_path);
        std.debug.print("minici start: {s}\n", .{job.name});
        var result = try runCommand(allocator, io, argv, log_path, true);
        result.stats_path = stats_path;
        try results.append(allocator, result);
        std.debug.print("minici done: {s} ({s})\n", .{ job.name, result.status });
    }

    try writeReportJson(allocator, io, build_result, results.items);
    try writeHtml(allocator, io, build_result, results.items);

    for (results.items) |result| {
        if (!std.mem.eql(u8, result.status, "pass")) std.process.exit(1);
    }
}
