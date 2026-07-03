//! MiniCI runner for split build/run jobs.

const std = @import("std");

const out_dir = "zig-out/minici";
const raw_dir = out_dir ++ "/raw";
const logs_dir = out_dir ++ "/logs";
const heartbeat_env = "MINICI_HEARTBEAT_INTERVAL_MS";
const default_heartbeat_interval_ms: u64 = 30_000;

const JobKind = enum {
    single,
    harness,
};

const Job = struct {
    name: []const u8,
    kind: JobKind = .single,
    args: []const []const u8 = &.{},
    skip_reason: ?[]const u8 = null,
};

const jobs = [_]Job{
    // MiniCI trusts `build.zig` to keep build work behind `build-ci`. Keep this
    // list to leaf `run-*` steps. Do not add aliases or aggregate steps that
    // hide useful reporting boundaries.
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
    .{ .name = "run-test-zig-module-collections" },
    .{ .name = "run-test-zig-module-base" },
    .{ .name = "run-test-zig-module-types" },
    .{ .name = "run-test-zig-module-builtins" },
    .{ .name = "run-test-zig-module-compile" },
    .{ .name = "run-test-zig-module-reporting" },
    .{ .name = "run-test-zig-module-parse" },
    .{ .name = "run-test-zig-module-can" },
    .{ .name = "run-test-zig-module-check" },
    .{ .name = "run-test-zig-module-ctx" },
    .{ .name = "run-test-zig-module-layout" },
    .{ .name = "run-test-zig-module-interpreter_layout" },
    .{ .name = "run-test-zig-module-values" },
    .{ .name = "run-test-zig-module-ipc" },
    .{ .name = "run-test-zig-module-fmt" },
    .{ .name = "run-test-zig-module-watch" },
    .{ .name = "run-test-zig-module-bundle" },
    .{ .name = "run-test-zig-module-unbundle" },
    .{ .name = "run-test-zig-module-base58" },
    .{ .name = "run-test-zig-module-lsp_unit" },
    .{ .name = "run-test-zig-module-lsp_integration", .kind = .harness },
    .{ .name = "run-test-zig-module-backend" },
    .{ .name = "run-test-zig-module-lir_core" },
    .{ .name = "run-test-zig-module-postcheck" },
    .{ .name = "run-test-zig-module-lir" },
    .{ .name = "run-test-zig-module-symbol" },
    .{ .name = "run-test-zig-module-sljmp" },
    .{ .name = "run-test-zig-module-echo_platform" },
    .{ .name = "run-test-zig-module-docs" },
    .{ .name = "run-test-zig-snapshot-tool" },
    .{ .name = "run-test-zig-builtin-doc" },
    .{ .name = "run-test-zig-cli-main" },
    .{ .name = "run-test-zig-machine-code-shim" },
    .{ .name = "run-test-zig-watch-cli" },
    .{ .name = "run-test-zig-fx-platform" },
    .{ .name = "run-test-eval", .kind = .harness, .args = &.{ "--timeout", "120000" } },
    .{ .name = "run-test-eval-host-effects", .kind = .harness },
    .{ .name = "run-test-playground", .kind = .harness },
    .{ .name = "run-test-cli", .kind = .harness },
    .{ .name = "run-test-serialization-sizes" },
    .{ .name = "run-test-wasm-static-lib" },
    .{ .name = "run-test-dylib" },
    .{ .name = "run-test-archive" },
    .{ .name = "run-coverage-parser" },
};

const CommandResult = struct {
    status: []const u8,
    start_ns: u64,
    end_ns: u64,
    duration_ns: u64,
    log_path: []const u8,
    command: []const []const u8,
    stats_path: ?[]const u8 = null,
    heartbeat_printed: bool = false,
};

fn nowNs(io: std.Io) u64 {
    return @intCast(@max(0, std.Io.Timestamp.now(io, .awake).nanoseconds));
}

fn durationSince(io: std.Io, started: u64) u64 {
    return nowNs(io) -| started;
}

fn unixMs(io: std.Io) u64 {
    return @intCast(@divTrunc(@max(0, std.Io.Timestamp.now(io, .real).nanoseconds), std.time.ns_per_ms));
}

fn seconds(ns: u64) f64 {
    return @as(f64, @floatFromInt(ns)) / 1_000_000_000.0;
}

fn isPass(result: CommandResult) bool {
    return std.mem.eql(u8, result.status, "pass");
}

fn isSuccessful(result: CommandResult) bool {
    return isPass(result) or std.mem.eql(u8, result.status, "skip");
}

fn isCheckJob(name: []const u8) bool {
    return std.mem.startsWith(u8, name, "run-check-");
}

fn buildStatusText(result: CommandResult) []const u8 {
    if (isPass(result)) return "completed";
    if (std.mem.eql(u8, result.status, "crash")) return "crashed";
    return "failed";
}

fn runStatusText(result: CommandResult) []const u8 {
    if (isPass(result)) return "passed";
    if (std.mem.eql(u8, result.status, "skip")) return "skipped";
    if (std.mem.eql(u8, result.status, "crash")) return "crashed";
    return "failed";
}

fn printRerunHint(result: CommandResult) void {
    const step_name = if (result.command.len > 2) result.command[2] else "build-ci";
    std.debug.print("  Re-run failed step: `zig build {s} --summary all --color off", .{step_name});
    var i: usize = 0;
    while (i < result.command.len) : (i += 1) {
        if (!std.mem.eql(u8, result.command[i], "--")) continue;

        i += 1;
        var printed_separator = false;
        while (i < result.command.len) : (i += 1) {
            if (std.mem.eql(u8, result.command[i], "--stats-json") and i + 1 < result.command.len) {
                i += 1;
                continue;
            }
            if (!printed_separator) {
                std.debug.print(" --", .{});
                printed_separator = true;
            }
            std.debug.print(" {s}", .{result.command[i]});
        }
        break;
    }
    std.debug.print("`\n", .{});
    std.debug.print("  Log: `{s}`\n", .{result.log_path});
}

fn heartbeatIntervalMs(env: *const std.process.Environ.Map) u64 {
    const raw = env.get(heartbeat_env) orelse return default_heartbeat_interval_ms;
    if (raw.len == 0) return default_heartbeat_interval_ms;
    return std.fmt.parseInt(u64, raw, 10) catch |err| {
        std.debug.print("invalid {s}='{s}': {s}; using default {d}ms\n", .{ heartbeat_env, raw, @errorName(err), default_heartbeat_interval_ms });
        return default_heartbeat_interval_ms;
    };
}

fn commandStepName(argv: []const []const u8) []const u8 {
    return if (argv.len > 2) argv[2] else argv[0];
}

const Heartbeat = struct {
    io: std.Io,
    argv: []const []const u8,
    started: u64,
    interval_ms: u64,
    done: std.atomic.Value(bool),
    printed: std.atomic.Value(bool),

    fn run(self: *@This()) void {
        if (self.interval_ms == 0) return;

        var next_ms = self.interval_ms;
        while (!self.done.load(.acquire)) {
            std.Io.sleep(self.io, std.Io.Duration.fromMilliseconds(500), .awake) catch {};
            if (self.done.load(.acquire)) return;

            const elapsed_ms = durationSince(self.io, self.started) / std.time.ns_per_ms;
            if (elapsed_ms < next_ms) continue;

            const already_printed = self.printed.swap(true, .acq_rel);
            if (!already_printed) std.debug.print("\n", .{});
            std.debug.print("  still running `{s}` after {d:.1}s\n", .{
                commandStepName(self.argv),
                seconds(elapsed_ms * std.time.ns_per_ms),
            });
            next_ms += self.interval_ms;
        }
    }
};

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

fn runCommand(
    allocator: std.mem.Allocator,
    io: std.Io,
    argv: []const []const u8,
    log_path: []const u8,
    heartbeat_interval_ms: u64,
    run_started_ns: u64,
) !CommandResult {
    const started = nowNs(io);
    var heartbeat = Heartbeat{
        .io = io,
        .argv = argv,
        .started = started,
        .interval_ms = heartbeat_interval_ms,
        .done = std.atomic.Value(bool).init(false),
        .printed = std.atomic.Value(bool).init(false),
    };
    const heartbeat_thread = if (heartbeat.interval_ms == 0)
        null
    else
        std.Thread.spawn(.{}, Heartbeat.run, .{&heartbeat}) catch null;
    defer {
        heartbeat.done.store(true, .release);
        if (heartbeat_thread) |thread| thread.join();
    }

    const result = std.process.run(allocator, io, .{ .argv = argv }) catch |err| {
        const ended = nowNs(io);
        const message = try std.fmt.allocPrint(allocator, "spawn failed: {s}\n", .{@errorName(err)});
        try writeFile(io, log_path, message);
        return .{
            .status = "crash",
            .start_ns = started -| run_started_ns,
            .end_ns = ended -| run_started_ns,
            .duration_ns = ended -| started,
            .log_path = log_path,
            .command = argv,
            .heartbeat_printed = heartbeat.printed.load(.acquire),
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
    const ended = nowNs(io);

    return .{
        .status = status,
        .start_ns = started -| run_started_ns,
        .end_ns = ended -| run_started_ns,
        .duration_ns = ended -| started,
        .log_path = log_path,
        .command = argv,
        .heartbeat_printed = heartbeat.printed.load(.acquire),
    };
}

fn skipCommand(
    io: std.Io,
    argv: []const []const u8,
    log_path: []const u8,
    reason: []const u8,
    run_started_ns: u64,
) !CommandResult {
    const started = nowNs(io);
    try writeFile(io, log_path, reason);
    const ended = nowNs(io);
    return .{
        .status = "skip",
        .start_ns = started -| run_started_ns,
        .end_ns = ended -| run_started_ns,
        .duration_ns = ended -| started,
        .log_path = log_path,
        .command = argv,
    };
}

fn buildCommand(
    allocator: std.mem.Allocator,
    zig_exe: []const u8,
    build_args: []const []const u8,
    step: []const u8,
    stats_path: ?[]const u8,
    run_args: []const []const u8,
) ![]const []const u8 {
    var argv = std.ArrayList([]const u8).empty;
    try argv.append(allocator, zig_exe);
    try argv.append(allocator, "build");
    try argv.append(allocator, step);
    // GitHub CI gets this via the option's CI-environment default; passing it
    // explicitly keeps local MiniCI runs mirroring CI.
    try argv.append(allocator, "-Dforbid-arc-certifier-skips");
    try argv.append(allocator, "--summary");
    try argv.append(allocator, "all");
    try argv.append(allocator, "--color");
    try argv.append(allocator, "off");
    for (build_args) |arg| {
        try argv.append(allocator, arg);
    }
    if (stats_path != null or run_args.len != 0) {
        try argv.append(allocator, "--");
    }
    if (stats_path) |path| {
        try argv.append(allocator, "--stats-json");
        try argv.append(allocator, path);
    }
    for (run_args) |arg| {
        try argv.append(allocator, arg);
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
    run_started_unix_ms: u64,
    build_result: CommandResult,
    results: []const CommandResult,
) !void {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(allocator);

    try out.appendSlice(allocator, "{\n  \"schema_version\": 1,\n  \"run_started_unix_ms\": ");
    try appendU64(&out, allocator, run_started_unix_ms);
    try out.appendSlice(allocator, ",\n  \"build_ci\": ");
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
    try out.appendSlice(allocator, ",\n    \"start_ns\": ");
    try appendU64(out, allocator, result.start_ns);
    try out.appendSlice(allocator, ",\n    \"end_ns\": ");
    try appendU64(out, allocator, result.end_ns);
    try out.appendSlice(allocator, ",\n    \"duration_ns\": ");
    try appendU64(out, allocator, result.duration_ns);
    try out.appendSlice(allocator, ",\n    \"log_path\": ");
    try appendJsonString(out, allocator, result.log_path);
    try out.appendSlice(allocator, ",\n    \"command\": ");
    try appendCommandJson(out, allocator, result.command);
    try out.appendSlice(allocator, ",\n    \"stats_path\": ");
    if (result.stats_path) |path| {
        try appendJsonString(out, allocator, path);
    } else {
        try out.appendSlice(allocator, "null");
    }
    try out.appendSlice(allocator, "\n  }");
}

fn appendScriptJsonBytes(out: *std.ArrayList(u8), allocator: std.mem.Allocator, bytes: []const u8) !void {
    for (bytes) |byte| {
        switch (byte) {
            '<' => try out.appendSlice(allocator, "\\u003c"),
            '>' => try out.appendSlice(allocator, "\\u003e"),
            '&' => try out.appendSlice(allocator, "\\u0026"),
            else => try out.append(allocator, byte),
        }
    }
}

fn appendReportJsonObject(
    out: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    run_started_unix_ms: u64,
    build_result: CommandResult,
    results: []const CommandResult,
) !void {
    try out.appendSlice(allocator, "{\n  \"schema_version\": 1,\n  \"run_started_unix_ms\": ");
    try appendU64(out, allocator, run_started_unix_ms);
    try out.appendSlice(allocator, ",\n  \"build_ci\": ");
    try appendResultJson(out, allocator, build_result);
    try out.appendSlice(allocator, ",\n  \"jobs\": [\n");
    for (results, 0..) |result, i| {
        if (i > 0) try out.appendSlice(allocator, ",\n");
        try appendResultJson(out, allocator, result);
    }
    try out.appendSlice(allocator, "\n  ]\n}");
}

fn appendStatsJsonObject(
    out: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    io: std.Io,
    results: []const CommandResult,
) !void {
    try out.appendSlice(allocator, "{\n");
    var first = true;
    for (results) |result| {
        const stats_path = result.stats_path orelse continue;
        if (!first) try out.appendSlice(allocator, ",\n");
        first = false;
        try out.appendSlice(allocator, "  ");
        try appendJsonString(out, allocator, result.command[2]);
        try out.appendSlice(allocator, ": ");
        const stats = std.Io.Dir.cwd().readFileAlloc(io, stats_path, allocator, .limited(256 * 1024 * 1024)) catch {
            try out.appendSlice(allocator, "null");
            continue;
        };
        defer allocator.free(stats);
        try appendScriptJsonBytes(out, allocator, stats);
    }
    try out.appendSlice(allocator, "\n}");
}

fn writeHtml(
    allocator: std.mem.Allocator,
    io: std.Io,
    run_started_unix_ms: u64,
    build_result: CommandResult,
    results: []const CommandResult,
) !void {
    var out = std.ArrayList(u8).empty;
    defer out.deinit(allocator);

    try out.appendSlice(allocator,
        \\<!doctype html>
        \\<html lang="en">
        \\<head>
        \\  <meta charset="utf-8">
        \\  <meta name="viewport" content="width=device-width, initial-scale=1">
        \\  <title>MiniCI</title>
        \\  <style>
        \\    :root{color-scheme:light;--bg:#f6f7f9;--panel:#fff;--text:#15181d;--muted:#68707d;--line:#d9dee6;--line-soft:#edf0f4;--pass:#16834a;--fail:#b42318;--skip:#737b87;--bar:#356fb8;--select:#101828;--track:#eef2f6}
        \\    *{box-sizing:border-box}
        \\    body{margin:0;background:var(--bg);color:var(--text);font-family:system-ui,-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif;font-size:13px;line-height:1.4}
        \\    header{position:sticky;top:0;z-index:10;background:#fff;border-bottom:1px solid var(--line);padding:14px 20px}
        \\    h1{margin:0;font-size:20px;font-weight:700}
        \\    h2{margin:0 0 10px;font-size:15px;font-weight:700}
        \\    h3{margin:0 0 8px;font-size:13px;font-weight:700}
        \\    main{padding:16px 20px 28px;display:grid;grid-template-columns:minmax(0,1fr)360px;gap:16px;max-width:1800px;margin:0 auto}
        \\    .summary{display:flex;flex-wrap:wrap;gap:12px;margin-top:10px}
        \\    .metric{display:flex;gap:6px;align-items:baseline}
        \\    .metric b{font-size:16px}.metric span{color:var(--muted);font-size:12px;text-transform:uppercase;letter-spacing:.04em}
        \\    .panel{border:1px solid var(--line);background:var(--panel);border-radius:6px;overflow:hidden}
        \\    .section{margin-bottom:16px}
        \\    .section-head{display:flex;align-items:center;justify-content:space-between;gap:12px;margin-bottom:8px}
        \\    .controls{display:flex;gap:8px;align-items:center;flex-wrap:wrap}
        \\    input[type=search]{height:30px;border:1px solid var(--line);border-radius:4px;padding:0 9px;background:#fff;color:var(--text);min-width:220px}
        \\    button{height:30px;border:1px solid var(--line);background:#fff;color:var(--text);border-radius:4px;padding:0 10px;cursor:pointer}
        \\    button.active{border-color:var(--select);box-shadow:inset 0 0 0 1px var(--select)}
        \\    code{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;font-size:12px}
        \\    .status{font-weight:700}.pass{color:var(--pass)}.fail,.crash,.timeout{color:var(--fail)}.skip{color:var(--skip)}
        \\    .muted{color:var(--muted)}.small{font-size:12px}
        \\    .empty{padding:12px;color:var(--muted)}
        \\    .grid{display:grid;grid-template-columns:320px minmax(0,1fr);gap:12px;align-items:start}
        \\    .list{max-height:520px;overflow:auto;border:1px solid var(--line);background:#fff;border-radius:6px}
        \\    .row{display:grid;grid-template-columns:minmax(0,1fr)70px 80px;gap:8px;align-items:center;padding:7px 9px;border-top:1px solid var(--line-soft);cursor:pointer}
        \\    .row:first-child{border-top:0}.row:hover,.row.selected{background:#f2f5f9}.row .name{overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
        \\    .timeline{border:1px solid var(--line);background:#fff;border-radius:6px;overflow:hidden}
        \\    .axis{height:26px;position:relative;border-bottom:1px solid var(--line-soft);background:#fafbfc}
        \\    .tick{position:absolute;top:0;height:100%;border-left:1px solid var(--line-soft);font-size:11px;color:var(--muted);padding-left:4px;white-space:nowrap}
        \\    .chart-row{display:grid;grid-template-columns:260px minmax(0,1fr);gap:10px;min-height:34px;border-top:1px solid var(--line-soft);padding:6px 8px;align-items:center}
        \\    .chart-row:first-child{border-top:0}.label{overflow:hidden;text-overflow:ellipsis;white-space:nowrap}.label-meta{font-size:11px;color:var(--muted)}
        \\    .track{height:24px;position:relative;background:var(--track);border-radius:3px;overflow:hidden}
        \\    .lane-track{height:28px;position:relative;background:var(--track);border-radius:3px;overflow:hidden}
        \\    .bar{position:absolute;top:4px;height:16px;min-width:2px;border-radius:3px;background:var(--bar);cursor:pointer}
        \\    .lane-track .bar{top:5px;height:18px}.bar.pass{background:var(--pass)}.bar.fail,.bar.crash,.bar.timeout{background:var(--fail)}.bar.skip{background:var(--skip)}.bar.selected{outline:2px solid var(--select);outline-offset:1px}
        \\    .detail{padding:12px}.kv{display:grid;grid-template-columns:110px minmax(0,1fr);gap:6px 10px}.kv div{overflow-wrap:anywhere}
        \\    .failure-list{display:grid;gap:8px;max-height:440px;overflow:auto}.failure{border:1px solid var(--line);border-left:4px solid var(--fail);background:#fff;border-radius:4px;padding:9px;cursor:pointer}.failure:hover{background:#f7f8fa}
        \\    .event-data{margin-top:10px}.event-data pre{white-space:pre-wrap;overflow:auto;max-height:260px;margin:6px 0 0;padding:8px;background:#111827;color:#f8fafc;border-radius:4px;font-size:12px}
        \\    .split{display:grid;grid-template-columns:minmax(0,1fr)320px;gap:12px}
        \\    @media(max-width:1100px){main{grid-template-columns:1fr}.grid,.split{grid-template-columns:1fr}.chart-row{grid-template-columns:1fr}.label-meta{display:inline;margin-left:6px}}
        \\  </style>
        \\</head>
        \\<body>
        \\  <header>
        \\    <h1>MiniCI</h1>
        \\    <div id="summary" class="summary"><div class="metric"><b>Loading</b><span>Report</span></div></div>
        \\  </header>
        \\  <main>
        \\    <div>
        \\      <section class="section">
        \\        <div class="section-head"><h2>Run Timeline</h2><div class="controls"><input id="search" type="search" placeholder="Filter jobs and tests"><button id="failOnly">Failures</button></div></div>
        \\        <div id="runTimeline" class="timeline"><div class="empty">Loading run timeline...</div></div>
        \\      </section>
        \\      <section class="section grid">
        \\        <div><h2>Jobs</h2><div id="jobList" class="list"><div class="empty">Loading jobs...</div></div></div>
        \\        <div><h2 id="jobTitle">Job</h2><div id="jobDetail" class="panel"><div class="empty">Select a job.</div></div></div>
        \\      </section>
        \\      <section class="section split">
        \\        <div><h2 id="caseTitle">Case Detail</h2><div id="caseDetail" class="panel"><div class="empty">Select a harness case.</div></div></div>
        \\        <div><h2>Slowest Cases</h2><div id="caseList" class="list"><div class="empty">Select a harness job.</div></div></div>
        \\      </section>
        \\    </div>
        \\    <aside>
        \\      <section class="section"><h2>Failures</h2><div id="failures" class="failure-list"><div class="panel empty">Loading failures...</div></div></section>
        \\      <section class="section"><h2>Selection</h2><div id="selection" class="panel detail">Loading selection...</div></section>
        \\    </aside>
        \\  </main>
        \\  <script>
        \\  const REPORT =
    );
    var report_json = std.ArrayList(u8).empty;
    defer report_json.deinit(allocator);
    try appendReportJsonObject(&report_json, allocator, run_started_unix_ms, build_result, results);
    try appendScriptJsonBytes(&out, allocator, report_json.items);
    try out.appendSlice(allocator,
        \\;
        \\  const STATS =
    );
    try appendStatsJsonObject(&out, allocator, io, results);
    try out.appendSlice(allocator,
        \\;
        \\  const state = { selectedJob: null, selectedCase: null, query: "", failOnly: false };
        \\  const statusClass = value => value === "passed" ? "pass" : value === "skipped" ? "skip" : String(value || "");
        \\  const isFailure = status => { const s = statusClass(status); return s !== "pass" && s !== "skip"; };
        \\  const esc = value => String(value ?? "").replace(/[&<>"']/g, ch => ({ "&":"&amp;", "<":"&lt;", ">":"&gt;", "\"":"&quot;", "'":"&#39;" }[ch]));
        \\  const jobName = job => job.name || (job.command && job.command.length > 2 ? job.command[2] : "unknown");
        \\  const commandText = command => (command || []).map(part => /\s/.test(part) ? JSON.stringify(part) : part).join(" ");
        \\  function formatNs(ns) {
        \\    if (!Number.isFinite(ns)) return "";
        \\    if (ns >= 1e9) return `${(ns / 1e9).toFixed(1)}s`;
        \\    if (ns >= 1e6) return `${(ns / 1e6).toFixed(1)}ms`;
        \\    if (ns >= 1e3) return `${(ns / 1e3).toFixed(1)}us`;
        \\    return `${Math.round(ns)}ns`;
        \\  }
        \\  function normJob(name, job) {
        \\    const start = Number(job.start_ns ?? 0);
        \\    const duration = Number(job.duration_ns ?? 0);
        \\    const end = Number(job.end_ns ?? (start + duration));
        \\    return { ...job, name, start_ns: start, end_ns: end, duration_ns: duration || Math.max(0, end - start) };
        \\  }
        \\  const jobs = [normJob("build-ci", REPORT.build_ci), ...REPORT.jobs.map(job => normJob(jobName(job), job))];
        \\  const jobsByName = new Map(jobs.map(job => [job.name, job]));
        \\  const maxRunEnd = Math.max(1, ...jobs.map(job => job.end_ns || job.duration_ns || 0));
        \\  function statsFor(job) { return STATS[job.name] && Array.isArray(STATS[job.name].events) ? STATS[job.name] : null; }
        \\  function childrenByParent(stats) {
        \\    const map = new Map();
        \\    for (const event of stats?.events || []) {
        \\      const key = event.parent_id || "";
        \\      if (!map.has(key)) map.set(key, []);
        \\      map.get(key).push(event);
        \\    }
        \\    for (const list of map.values()) list.sort((a,b) => (a.start_ns || 0) - (b.start_ns || 0));
        \\    return map;
        \\  }
        \\  function rootCases(stats) { return (stats?.events || []).filter(event => event.parent_id == null); }
        \\  function sortedCases(stats) {
        \\    return rootCases(stats).sort((a,b) => (isFailure(a.status) ? 0 : 1) - (isFailure(b.status) ? 0 : 1) || (b.duration_ns || 0) - (a.duration_ns || 0));
        \\  }
        \\  function scaleStyle(start, end, max) {
        \\    const left = Math.max(0, (Number(start || 0) / max) * 100);
        \\    const width = Math.max(0.25, ((Number(end || 0) - Number(start || 0)) / max) * 100);
        \\    return `left:${left.toFixed(3)}%;width:${width.toFixed(3)}%`;
        \\  }
        \\  function axis(max) {
        \\    const ticks = [];
        \\    for (let i = 0; i <= 4; i++) ticks.push(`<div class="tick" style="left:${i * 25}%">${formatNs(max * i / 4)}</div>`);
        \\    return `<div class="axis">${ticks.join("")}</div>`;
        \\  }
        \\  function rowMatches(text, status) {
        \\    const q = state.query.trim().toLowerCase();
        \\    if (state.failOnly && !isFailure(status)) return false;
        \\    return q === "" || String(text || "").toLowerCase().includes(q);
        \\  }
        \\  function renderSummary() {
        \\    const counts = { pass:0, fail:0, crash:0, timeout:0, skip:0 };
        \\    for (const job of jobs) counts[statusClass(job.status)] = (counts[statusClass(job.status)] || 0) + 1;
        \\    const failed = (counts.fail || 0) + (counts.crash || 0) + (counts.timeout || 0);
        \\    const started = REPORT.run_started_unix_ms ? new Date(REPORT.run_started_unix_ms).toLocaleString() : "";
        \\    document.getElementById("summary").innerHTML = [
        \\      ["Jobs", jobs.length], ["Passed", counts.pass || 0], ["Failed", failed], ["Wall", formatNs(maxRunEnd)], ["Started", started]
        \\    ].filter(item => item[1] !== "").map(([label,value]) => `<div class="metric"><b>${esc(value)}</b><span>${esc(label)}</span></div>`).join("");
        \\  }
        \\  function renderRunTimeline() {
        \\    const rows = jobs.map(job => `<div class="chart-row"><div><div class="label"><code>${esc(job.name)}</code></div><div class="label-meta"><span class="${statusClass(job.status)}">${esc(job.status)}</span> ${formatNs(job.duration_ns)}</div></div><div class="track"><div class="bar ${statusClass(job.status)} ${state.selectedJob === job.name ? "selected" : ""}" data-job="${esc(job.name)}" title="${esc(job.name)} ${formatNs(job.duration_ns)}" style="${scaleStyle(job.start_ns, job.end_ns, maxRunEnd)}"></div></div></div>`).join("");
        \\    document.getElementById("runTimeline").innerHTML = axis(maxRunEnd) + rows;
        \\  }
        \\  function renderJobList() {
        \\    const visible = jobs.filter(job => rowMatches(job.name, job.status)).sort((a,b) => (isFailure(a.status) ? 0 : 1) - (isFailure(b.status) ? 0 : 1) || (b.duration_ns || 0) - (a.duration_ns || 0));
        \\    document.getElementById("jobList").innerHTML = visible.length ? visible.map(job => `<div class="row ${state.selectedJob === job.name ? "selected" : ""}" data-job="${esc(job.name)}"><div class="name"><code>${esc(job.name)}</code></div><span class="status ${statusClass(job.status)}">${esc(job.status)}</span><span>${formatNs(job.duration_ns)}</span></div>`).join("") : `<div class="empty">No jobs match.</div>`;
        \\  }
        \\  function renderJobDetail() {
        \\    const job = jobsByName.get(state.selectedJob) || jobs[0];
        \\    state.selectedJob = job.name;
        \\    document.getElementById("jobTitle").textContent = job.name;
        \\    const stats = statsFor(job);
        \\    document.getElementById("selection").innerHTML = renderJobMeta(job);
        \\    if (!stats) {
        \\      document.getElementById("jobDetail").innerHTML = `<div class="detail">${renderJobMeta(job)}</div>`;
        \\      renderCaseList(null);
        \\      renderCaseDetail(null, null);
        \\      return;
        \\    }
        \\    const cases = rootCases(stats);
        \\    const byLane = new Map();
        \\    for (const c of cases) {
        \\      const lane = c.worker_index ?? 0;
        \\      if (!byLane.has(lane)) byLane.set(lane, []);
        \\      byLane.get(lane).push(c);
        \\    }
        \\    const maxEnd = Math.max(1, ...cases.map(c => c.end_ns || c.duration_ns || 0));
        \\    const lanes = [...byLane.entries()].sort((a,b) => a[0] - b[0]).map(([lane, list]) => {
        \\      const bars = list.map(c => `<div class="bar ${statusClass(c.status)} ${state.selectedCase === c.id ? "selected" : ""}" data-case="${esc(c.id)}" title="${esc(c.name)} ${formatNs(c.duration_ns)}" style="${scaleStyle(c.start_ns || 0, c.end_ns || c.duration_ns || 0, maxEnd)}"></div>`).join("");
        \\      return `<div class="chart-row"><div><div class="label">worker ${esc(lane)}</div><div class="label-meta">${list.length} cases</div></div><div class="lane-track">${bars}</div></div>`;
        \\    }).join("");
        \\    const summary = stats.summary || {};
        \\    document.getElementById("jobDetail").innerHTML = `<div class="detail small muted">Runner <b>${esc(stats.runner || job.name)}</b>: ${esc(summary.passed || 0)} passed, ${esc(summary.failed || 0)} failed, ${esc(summary.crashed || 0)} crashed, ${esc(summary.timed_out || 0)} timed out, ${esc(summary.skipped || 0)} skipped</div><div class="timeline">${axis(maxEnd)}${lanes || `<div class="empty">No case events.</div>`}</div>`;
        \\    if (!state.selectedCase || !cases.some(c => c.id === state.selectedCase)) state.selectedCase = sortedCases(stats)[0]?.id || null;
        \\    renderCaseList(stats);
        \\    renderCaseDetail(stats, state.selectedCase);
        \\  }
        \\  function renderJobMeta(job) {
        \\    return `<div class="kv"><div>Status</div><div class="status ${statusClass(job.status)}">${esc(job.status)}</div><div>Duration</div><div>${formatNs(job.duration_ns)}</div><div>Command</div><div><code>${esc(commandText(job.command))}</code></div><div>Log</div><div><code>${esc(job.log_path || "")}</code></div>${job.stats_path ? `<div>Stats</div><div><code>${esc(job.stats_path)}</code></div>` : ""}</div>`;
        \\  }
        \\  function renderCaseList(stats) {
        \\    if (!stats) { document.getElementById("caseList").innerHTML = `<div class="empty">No harness cases.</div>`; return; }
        \\    const cases = sortedCases(stats).filter(c => rowMatches(c.name, c.status)).slice(0, 300);
        \\    document.getElementById("caseList").innerHTML = cases.length ? cases.map(c => `<div class="row ${state.selectedCase === c.id ? "selected" : ""}" data-case="${esc(c.id)}"><div class="name">${esc(c.name)}</div><span class="status ${statusClass(c.status)}">${esc(c.status)}</span><span>${formatNs(c.duration_ns)}</span></div>`).join("") : `<div class="empty">No cases match.</div>`;
        \\  }
        \\  function renderCaseDetail(stats, caseId) {
        \\    const root = document.getElementById("caseDetail");
        \\    if (!stats || !caseId) { document.getElementById("caseTitle").textContent = "Case Detail"; root.innerHTML = `<div class="empty">Select a harness case.</div>`; return; }
        \\    const byParent = childrenByParent(stats);
        \\    const event = (stats.events || []).find(e => e.id === caseId);
        \\    if (!event) { root.innerHTML = `<div class="empty">Selected case is missing.</div>`; return; }
        \\    document.getElementById("caseTitle").textContent = event.name;
        \\    const children = byParent.get(event.id) || [];
        \\    const spans = children.length ? children : [event];
        \\    const maxEnd = Math.max(1, ...spans.map(s => s.end_ns || s.duration_ns || 0));
        \\    const rows = spans.map(s => `<div class="chart-row"><div><div class="label">${esc(s.kind)} ${esc(s.name)}</div><div class="label-meta"><span class="${statusClass(s.status)}">${esc(s.status)}</span> ${formatNs(s.duration_ns)}</div></div><div class="track"><div class="bar ${statusClass(s.status)}" title="${esc(s.name)} ${formatNs(s.duration_ns)}" style="${scaleStyle(s.start_ns || 0, s.end_ns || s.duration_ns || 0, maxEnd)}"></div></div></div>`).join("");
        \\    root.innerHTML = `<div class="detail">${renderCaseMeta(event)}${renderData(event.data)}</div><div class="timeline">${axis(maxEnd)}${rows}</div>`;
        \\  }
        \\  function renderCaseMeta(event) {
        \\    const job = jobsByName.get(state.selectedJob);
        \\    const repro = job ? `zig build ${job.name} -- --test-filter ${JSON.stringify(event.name)}` : "";
        \\    return `<div class="kv"><div>Status</div><div class="status ${statusClass(event.status)}">${esc(event.status)}</div><div>Duration</div><div>${formatNs(event.duration_ns)}</div><div>Worker</div><div>${esc(event.worker_index ?? "")}</div><div>Rerun</div><div><code>${esc(repro)}</code></div></div>`;
        \\  }
        \\  function renderData(data) {
        \\    if (!data || Object.keys(data).length === 0) return "";
        \\    return `<div class="event-data">${Object.entries(data).slice(0,8).map(([key,value]) => `<b>${esc(key)}</b><pre>${esc(String(value).slice(0, 4000))}</pre>`).join("")}</div>`;
        \\  }
        \\  function collectFailures() {
        \\    const failures = [];
        \\    for (const job of jobs) {
        \\      if (job.name !== "build-ci" && isFailure(job.status)) failures.push({ job, event: null });
        \\      const stats = statsFor(job);
        \\      for (const event of rootCases(stats)) if (isFailure(event.status)) failures.push({ job, event });
        \\    }
        \\    return failures;
        \\  }
        \\  function renderFailures() {
        \\    const failures = collectFailures();
        \\    document.getElementById("failures").innerHTML = failures.length ? failures.map((item, i) => {
        \\      const title = item.event ? item.event.name : item.job.name;
        \\      const status = item.event ? item.event.status : item.job.status;
        \\      return `<article class="failure" data-failure="${i}"><div><b>${esc(item.job.name)}</b> <span class="${statusClass(status)}">${esc(status)}</span></div><div>${esc(title)}</div><div class="small muted"><code>${esc(item.event ? `zig build ${item.job.name} -- --test-filter ${JSON.stringify(item.event.name)}` : `zig build ${item.job.name}`)}</code></div></article>`;
        \\    }).join("") : `<div class="panel empty">No failing jobs or harness cases.</div>`;
        \\  }
        \\  function selectJob(name) { state.selectedJob = name; state.selectedCase = null; renderAll(); }
        \\  function selectCase(id) { state.selectedCase = id; renderAll(); }
        \\  function wireEvents() {
        \\    document.querySelectorAll("[data-job]").forEach(el => el.onclick = () => selectJob(el.getAttribute("data-job")));
        \\    document.querySelectorAll("[data-case]").forEach(el => el.onclick = () => selectCase(el.getAttribute("data-case")));
        \\    const failures = collectFailures();
        \\    document.querySelectorAll("[data-failure]").forEach(el => el.onclick = () => {
        \\      const item = failures[Number(el.getAttribute("data-failure"))];
        \\      if (!item) return;
        \\      state.selectedJob = item.job.name;
        \\      state.selectedCase = item.event ? item.event.id : null;
        \\      renderAll();
        \\    });
        \\  }
        \\  function chooseInitialSelection() {
        \\    const failure = collectFailures()[0];
        \\    state.selectedJob = failure ? failure.job.name : jobs[0].name;
        \\    state.selectedCase = failure && failure.event ? failure.event.id : null;
        \\  }
        \\  function renderAll() {
        \\    renderSummary();
        \\    renderRunTimeline();
        \\    renderJobList();
        \\    renderJobDetail();
        \\    renderFailures();
        \\    wireEvents();
        \\  }
        \\  function showRenderError(error) {
        \\    const message = error && error.stack ? error.stack : String(error);
        \\    const html = `<div class="detail"><b>Report render failed</b><div class="event-data"><pre>${esc(message)}</pre></div></div>`;
        \\    document.getElementById("selection").innerHTML = html;
        \\    document.getElementById("runTimeline").innerHTML = html;
        \\  }
        \\  function boot() {
        \\    try {
        \\      document.getElementById("search").addEventListener("input", event => { state.query = event.target.value; renderAll(); });
        \\      document.getElementById("failOnly").addEventListener("click", event => { state.failOnly = !state.failOnly; event.target.classList.toggle("active", state.failOnly); renderAll(); });
        \\      chooseInitialSelection();
        \\      renderAll();
        \\    } catch (error) {
        \\      showRenderError(error);
        \\    }
        \\  }
        \\  setTimeout(boot, 0);
        \\  </script>
        \\</body>
        \\</html>
        \\
    );
    try writeFile(io, out_dir ++ "/index.html", out.items);
}

/// Runs build-ci followed by each named MiniCI run job and writes reports.
pub fn main(init: std.process.Init) !void {
    const io = init.io;
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const allocator = arena_impl.allocator();

    const raw_args = try init.minimal.args.toSlice(allocator);
    const args: []const []const u8 = @ptrCast(raw_args);
    const zig_exe = if (args.len >= 2) args[1] else "zig";
    const build_args = if (args.len > 2) args[2..] else &.{};
    const heartbeat_interval_ms = heartbeatIntervalMs(init.environ_map);

    std.Io.Dir.cwd().deleteTree(io, out_dir) catch {};
    try std.Io.Dir.cwd().createDirPath(io, raw_dir);
    try std.Io.Dir.cwd().createDirPath(io, logs_dir);

    std.debug.print("=== MINICI ORCHESTRATOR ===\n", .{});
    const run_started_ns = nowNs(io);
    const run_started_unix_ms = unixMs(io);

    const build_argv = try buildCommand(allocator, zig_exe, build_args, "build-ci", null, &.{});
    const build_log = logs_dir ++ "/build-ci.txt";
    std.debug.print("Building CI steps ... ", .{});
    const build_result = try runCommand(allocator, io, build_argv, build_log, heartbeat_interval_ms, run_started_ns);
    if (build_result.heartbeat_printed) std.debug.print("Building CI steps ... ", .{});
    std.debug.print("{s} in {d:.3}s\n", .{ buildStatusText(build_result), seconds(build_result.duration_ns) });

    var results = std.ArrayList(CommandResult).empty;
    defer results.deinit(allocator);

    if (!isPass(build_result)) {
        printRerunHint(build_result);
        try writeReportJson(allocator, io, run_started_unix_ms, build_result, results.items);
        try writeHtml(allocator, io, run_started_unix_ms, build_result, results.items);
        std.process.exit(1);
    }

    for (jobs) |job| {
        const log_path = try std.fmt.allocPrint(allocator, "{s}/{s}.txt", .{ logs_dir, job.name });
        const stats_path: ?[]const u8 = if (job.kind == .harness)
            try std.fmt.allocPrint(allocator, "{s}/{s}.json", .{ raw_dir, job.name })
        else
            null;
        const argv = try buildCommand(allocator, zig_exe, build_args, job.name, stats_path, job.args);
        std.debug.print("Running `{s}` ... ", .{job.name});
        var result = if (job.skip_reason) |reason|
            try skipCommand(io, argv, log_path, reason, run_started_ns)
        else
            try runCommand(allocator, io, argv, log_path, heartbeat_interval_ms, run_started_ns);
        result.stats_path = if (job.skip_reason == null) stats_path else null;
        try results.append(allocator, result);
        if (result.heartbeat_printed) std.debug.print("Running `{s}` ... ", .{job.name});
        std.debug.print("{s} in {d:.3}s\n", .{ runStatusText(result), seconds(result.duration_ns) });

        if (!isSuccessful(result)) {
            printRerunHint(result);
        }

        if (isCheckJob(job.name) and !isSuccessful(result)) {
            try writeReportJson(allocator, io, run_started_unix_ms, build_result, results.items);
            try writeHtml(allocator, io, run_started_unix_ms, build_result, results.items);
            std.process.exit(1);
        }
    }

    try writeReportJson(allocator, io, run_started_unix_ms, build_result, results.items);
    try writeHtml(allocator, io, run_started_unix_ms, build_result, results.items);

    for (results.items) |result| {
        if (!isSuccessful(result)) std.process.exit(1);
    }
}
