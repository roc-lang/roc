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
    // MiniCI trusts `build.zig` to keep build work behind `build-ci`. Keep this
    // list to leaf `run-*` steps; do not add aggregate aliases that hide useful
    // reporting boundaries.
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
    .{ .name = "run-test-zig-module-lsp" },
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
    .{ .name = "run-test-zig-watch-cli" },
    .{ .name = "run-test-zig-fx-platform" },
    .{ .name = "run-test-eval", .kind = .harness },
    .{ .name = "run-test-eval-host-effects", .kind = .harness },
    .{ .name = "run-test-playground", .kind = .harness },
    .{ .name = "run-test-cli", .kind = .harness },
    .{ .name = "run-test-serialization-sizes" },
    .{ .name = "run-coverage-parser" },
};

const CommandResult = struct {
    status: []const u8,
    duration_ns: u64,
    log_path: []const u8,
    command: []const []const u8,
    stats_path: ?[]const u8 = null,
};

fn nowNs(io: std.Io) u64 {
    return @intCast(@max(0, std.Io.Timestamp.now(io, .awake).nanoseconds));
}

fn durationSince(io: std.Io, started: u64) u64 {
    return nowNs(io) -| started;
}

fn seconds(ns: u64) f64 {
    return @as(f64, @floatFromInt(ns)) / 1_000_000_000.0;
}

fn isPass(result: CommandResult) bool {
    return std.mem.eql(u8, result.status, "pass");
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
    if (std.mem.eql(u8, result.status, "crash")) return "crashed";
    return "failed";
}

fn printRerunHint(result: CommandResult) void {
    const step_name = if (result.command.len > 2) result.command[2] else "build-ci";
    std.debug.print("  Re-run failed step: `zig build {s} --summary all --color off`\n", .{step_name});
    std.debug.print("  Log: `{s}`\n", .{result.log_path});
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

fn runCommand(
    allocator: std.mem.Allocator,
    io: std.Io,
    argv: []const []const u8,
    log_path: []const u8,
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

    return .{
        .status = status,
        .duration_ns = durationSince(io, started),
        .log_path = log_path,
        .command = argv,
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
    build_result: CommandResult,
    results: []const CommandResult,
) !void {
    try out.appendSlice(allocator, "{\n  \"schema_version\": 1,\n  \"build_ci\": ");
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
        \\    :root{color-scheme:light;--bg:#f5f6f8;--panel:#fff;--text:#171717;--muted:#636873;--line:#dfe3e8;--line-soft:#edf0f3;--pass:#147a3f;--fail:#b42318;--skip:#68707c;--bar:#3168b7;--bar-pass:#1f8a55;--bar-fail:#c43d2f;--bar-skip:#8992a0}
        \\    *{box-sizing:border-box}
        \\    body{margin:0;background:var(--bg);color:var(--text);font-family:system-ui,-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif;font-size:14px;line-height:1.4}
        \\    header{padding:20px 24px 14px;border-bottom:1px solid var(--line);background:#fff}
        \\    h1{margin:0;font-size:24px;font-weight:700}
        \\    h2{margin:20px 0 10px;font-size:16px;font-weight:700}
        \\    main{padding:18px 24px 32px;max-width:1600px;margin:0 auto}
        \\    .summary{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:8px;margin-top:14px}
        \\    .metric{border:1px solid var(--line);background:#fafbfc;padding:10px 12px;border-radius:6px}
        \\    .metric b{display:block;font-size:20px}
        \\    .metric span{color:var(--muted);font-size:12px;text-transform:uppercase;letter-spacing:.04em}
        \\    .panel{border:1px solid var(--line);background:var(--panel);border-radius:6px;overflow:hidden}
        \\    .job{border-top:1px solid var(--line-soft)}
        \\    .job:first-child{border-top:0}
        \\    .job>summary{display:grid;grid-template-columns:minmax(260px,1.7fr)90px 90px 120px minmax(280px,2fr);gap:12px;align-items:center;padding:10px 12px;cursor:pointer;list-style:none}
        \\    .job>summary::-webkit-details-marker{display:none}
        \\    .job>summary:hover{background:#fafbfc}
        \\    .job-body{border-top:1px solid var(--line-soft);padding:12px;background:#fbfcfd}
        \\    code{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;font-size:12px}
        \\    .status{font-weight:700}.pass{color:var(--pass)}.fail,.crash,.timeout{color:var(--fail)}.skip{color:var(--skip)}
        \\    .muted{color:var(--muted)}.small{font-size:12px}
        \\    .paths{white-space:normal;overflow-wrap:anywhere}
        \\    .failure-list{display:grid;gap:8px}
        \\    .failure{border:1px solid var(--line);background:#fff;border-left:4px solid var(--fail);border-radius:4px;padding:10px}
        \\    .failure pre,.event-data pre{white-space:pre-wrap;overflow:auto;max-height:220px;margin:8px 0 0;padding:8px;background:#111827;color:#f8fafc;border-radius:4px;font-size:12px}
        \\    .harness-grid{display:grid;grid-template-columns:minmax(360px,1fr)minmax(460px,1.4fr);gap:14px;align-items:start}
        \\    .cases{max-height:760px;overflow:auto;border:1px solid var(--line);background:#fff;border-radius:4px}
        \\    .case{border-top:1px solid var(--line-soft)}
        \\    .case:first-child{border-top:0}
        \\    .case>summary{display:grid;grid-template-columns:1fr 70px 82px;gap:8px;padding:8px 10px;cursor:pointer;list-style:none}
        \\    .case>summary::-webkit-details-marker{display:none}
        \\    .case>summary:hover{background:#f7f8fa}
        \\    .children{padding:0 10px 8px 24px}
        \\    .child{display:grid;grid-template-columns:1fr 70px 82px;gap:8px;padding:5px 0;border-top:1px solid var(--line-soft)}
        \\    .timeline{border:1px solid var(--line);background:#fff;border-radius:4px;overflow:hidden}
        \\    .lane{display:grid;grid-template-columns:minmax(180px,320px)1fr;gap:10px;align-items:center;min-height:34px;border-top:1px solid var(--line-soft);padding:6px 8px}
        \\    .lane:first-child{border-top:0}
        \\    .track{position:relative;height:24px;background:#eef1f5;border-radius:3px;overflow:hidden}
        \\    .bar{position:absolute;top:3px;height:18px;border-radius:3px;background:var(--bar);min-width:2px}
        \\    .bar.pass{background:var(--bar-pass)}.bar.fail,.bar.crash,.bar.timeout{background:var(--bar-fail)}.bar.skip{background:var(--bar-skip)}
        \\    .bar span{position:absolute;left:5px;top:1px;max-width:240px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;color:#fff;font-size:11px;line-height:16px;text-shadow:0 1px 1px rgba(0,0,0,.35)}
        \\    .event-data{margin-top:8px}
        \\    .empty{padding:12px;color:var(--muted)}
        \\    @media(max-width:900px){main{padding:12px}.job>summary{grid-template-columns:1fr 70px 80px}.job>summary .paths{grid-column:1/-1}.harness-grid{grid-template-columns:1fr}.lane{grid-template-columns:1fr}.track{height:28px}}
        \\  </style>
        \\</head>
        \\<body>
        \\  <header>
        \\    <h1>MiniCI</h1>
        \\    <div id="summary" class="summary"></div>
        \\  </header>
        \\  <main>
        \\    <section id="failures"></section>
        \\    <h2>Jobs</h2>
        \\    <section id="jobs" class="panel"></section>
        \\  </main>
        \\  <script>
        \\  const REPORT =
    );
    var report_json = std.ArrayList(u8).empty;
    defer report_json.deinit(allocator);
    try appendReportJsonObject(&report_json, allocator, build_result, results);
    try appendScriptJsonBytes(&out, allocator, report_json.items);
    try out.appendSlice(allocator,
        \\;
        \\  const STATS =
    );
    try appendStatsJsonObject(&out, allocator, io, results);
    try out.appendSlice(allocator,
        \\;
        \\  const STATUS_ORDER = ["fail","crash","timeout","pass","skip"];
        \\  const statusClass = value => value === "passed" ? "pass" : value === "skipped" ? "skip" : value;
        \\  function formatNs(ns) {
        \\    if (!Number.isFinite(ns)) return "";
        \\    if (ns >= 1e9) return `${(ns / 1e9).toFixed(1)}s`;
        \\    if (ns >= 1e6) return `${(ns / 1e6).toFixed(1)}ms`;
        \\    if (ns >= 1e3) return `${(ns / 1e3).toFixed(1)}us`;
        \\    return `${ns}ns`;
        \\  }
        \\  function esc(value) {
        \\    return String(value ?? "").replace(/[&<>"']/g, ch => ({ "&":"&amp;", "<":"&lt;", ">":"&gt;", "\"":"&quot;", "'":"&#39;" }[ch]));
        \\  }
        \\  function commandText(command) {
        \\    return (command || []).map(part => /\s/.test(part) ? JSON.stringify(part) : part).join(" ");
        \\  }
        \\  function isFailure(status) {
        \\    const normalized = statusClass(status);
        \\    return normalized !== "pass" && normalized !== "skip";
        \\  }
        \\  function jobName(job) {
        \\    return job.command && job.command.length > 2 ? job.command[2] : "unknown";
        \\  }
        \\  function sortedCases(events) {
        \\    return events.filter(event => event.parent_id == null).sort((a, b) => {
        \\      const af = isFailure(a.status) ? 0 : 1;
        \\      const bf = isFailure(b.status) ? 0 : 1;
        \\      if (af !== bf) return af - bf;
        \\      return (b.duration_ns || 0) - (a.duration_ns || 0);
        \\    });
        \\  }
        \\  function renderSummary() {
        \\    const jobs = [REPORT.build_ci, ...REPORT.jobs];
        \\    const counts = { pass:0, fail:0, crash:0, timeout:0, skip:0 };
        \\    let totalNs = 0;
        \\    for (const job of jobs) {
        \\      const status = statusClass(job.status);
        \\      counts[status] = (counts[status] || 0) + 1;
        \\      totalNs += job.duration_ns || 0;
        \\    }
        \\    document.getElementById("summary").innerHTML = [
        \\      ["Jobs", jobs.length],
        \\      ["Passed", counts.pass || 0],
        \\      ["Failed", (counts.fail || 0) + (counts.crash || 0) + (counts.timeout || 0)],
        \\      ["Total Time", formatNs(totalNs)]
        \\    ].map(([label, value]) => `<div class="metric"><b>${esc(value)}</b><span>${esc(label)}</span></div>`).join("");
        \\  }
        \\  function renderFailures() {
        \\    const failures = [];
        \\    for (const job of REPORT.jobs) {
        \\      const name = jobName(job);
        \\      const stats = STATS[name];
        \\      if (!stats || !Array.isArray(stats.events)) {
        \\        if (isFailure(job.status)) failures.push({ job: name, event: null, data: { log: job.log_path } });
        \\        continue;
        \\      }
        \\      for (const event of stats.events) {
        \\        if (event.parent_id == null && isFailure(event.status)) failures.push({ job: name, event, data: event.data || {} });
        \\      }
        \\    }
        \\    const root = document.getElementById("failures");
        \\    if (failures.length === 0) {
        \\      root.innerHTML = "<h2>Failures</h2><div class=\"panel empty\">No failing jobs or harness cases.</div>";
        \\      return;
        \\    }
        \\    root.innerHTML = `<h2>Failures</h2><div class="failure-list">${failures.map(item => renderFailure(item)).join("")}</div>`;
        \\  }
        \\  function reproFor(job, event) {
        \\    if (!event) return `zig build ${job}`;
        \\    if (job === "run-test-playground") return `zig build ${job}`;
        \\    return `zig build ${job} -- --test-filter ${JSON.stringify(event.name)}`;
        \\  }
        \\  function renderFailure(item) {
        \\    const event = item.event;
        \\    const title = event ? event.name : item.job;
        \\    const data = item.data || {};
        \\    const entries = Object.entries(data).slice(0, 6);
        \\    const details = entries.map(([key, value]) => `<div class="event-data"><b>${esc(key)}</b><pre>${esc(String(value).slice(0, 4000))}</pre></div>`).join("");
        \\    return `<article class="failure"><div><b>${esc(item.job)}</b> <span class="${statusClass(event ? event.status : "fail")}">${esc(event ? event.status : "fail")}</span></div><div>${esc(title)}</div><div class="small muted"><code>${esc(reproFor(item.job, event))}</code></div>${details}</article>`;
        \\  }
        \\  function renderJobs() {
        \\    const jobs = [Object.assign({ name: "build-ci" }, REPORT.build_ci), ...REPORT.jobs.map(job => Object.assign({ name: jobName(job) }, job))];
        \\    document.getElementById("jobs").innerHTML = jobs.map(job => renderJob(job)).join("");
        \\  }
        \\  function renderJob(job) {
        \\    const stats = STATS[job.name];
        \\    const hasStats = stats && Array.isArray(stats.events);
        \\    const body = hasStats ? renderHarness(job.name, stats) : renderSingle(job);
        \\    const open = isFailure(job.status) ? " open" : "";
        \\    return `<details class="job"${open}><summary><code>${esc(job.name)}</code><span class="status ${statusClass(job.status)}">${esc(job.status)}</span><span>${formatNs(job.duration_ns || 0)}</span><span class="paths small"><code>${esc(job.log_path || "")}${job.stats_path ? " " + esc(job.stats_path) : ""}</code></span></summary><div class="job-body">${body}</div></details>`;
        \\  }
        \\  function renderSingle(job) {
        \\    return `<div class="small muted"><div><code>${esc(commandText(job.command))}</code></div><div>Log: <code>${esc(job.log_path || "")}</code></div></div>`;
        \\  }
        \\  function renderHarness(jobNameValue, stats) {
        \\    const events = stats.events || [];
        \\    const byParent = new Map();
        \\    for (const event of events) {
        \\      const key = event.parent_id || "";
        \\      if (!byParent.has(key)) byParent.set(key, []);
        \\      byParent.get(key).push(event);
        \\    }
        \\    for (const list of byParent.values()) {
        \\      list.sort((a, b) => (a.start_ns || 0) - (b.start_ns || 0));
        \\    }
        \\    const cases = sortedCases(events);
        \\    const summary = stats.summary || {};
        \\    return `<div class="small muted">Runner <b>${esc(stats.runner || jobNameValue)}</b>: ${esc(summary.passed || 0)} passed, ${esc(summary.failed || 0)} failed, ${esc(summary.crashed || 0)} crashed, ${esc(summary.timed_out || 0)} timed out, ${esc(summary.skipped || 0)} skipped</div><div class="harness-grid"><div class="cases">${cases.map(testCase => renderCase(testCase, byParent)).join("")}</div>${renderTimeline(cases, byParent)}</div>`;
        \\  }
        \\  function renderCase(testCase, byParent) {
        \\    const children = byParent.get(testCase.id) || [];
        \\    const open = isFailure(testCase.status) ? " open" : "";
        \\    const data = renderData(testCase.data);
        \\    return `<details class="case"${open}><summary><span>${esc(testCase.name)}</span><span class="status ${statusClass(testCase.status)}">${esc(testCase.status)}</span><span>${formatNs(testCase.duration_ns || 0)}</span></summary><div class="children">${children.map(renderChild).join("")}${data}</div></details>`;
        \\  }
        \\  function renderChild(event) {
        \\    return `<div class="child"><span><code>${esc(event.kind)}</code> ${esc(event.name)}</span><span class="status ${statusClass(event.status)}">${esc(event.status)}</span><span>${formatNs(event.duration_ns || 0)}</span></div>${renderData(event.data)}`;
        \\  }
        \\  function renderData(data) {
        \\    if (!data || Object.keys(data).length === 0) return "";
        \\    return `<div class="event-data">${Object.entries(data).map(([key, value]) => `<b>${esc(key)}</b><pre>${esc(String(value).slice(0, 4000))}</pre>`).join("")}</div>`;
        \\  }
        \\  function renderTimeline(cases, byParent) {
        \\    if (cases.length === 0) return `<div class="timeline empty">No events.</div>`;
        \\    return `<div class="timeline">${cases.map(testCase => renderLane(testCase, byParent)).join("")}</div>`;
        \\  }
        \\  function renderLane(testCase, byParent) {
        \\    const children = byParent.get(testCase.id) || [];
        \\    const spans = children.length > 0 ? children : [testCase];
        \\    const maxEnd = Math.max(testCase.end_ns || 0, ...spans.map(span => span.end_ns || 0), 1);
        \\    const bars = spans.map(span => {
        \\      const left = Math.max(0, ((span.start_ns || 0) / maxEnd) * 100);
        \\      const width = Math.max(.25, ((span.duration_ns || 0) / maxEnd) * 100);
        \\      return `<div class="bar ${statusClass(span.status)}" title="${esc(span.kind)} ${esc(span.name)} ${formatNs(span.duration_ns || 0)}" style="left:${left.toFixed(3)}%;width:${width.toFixed(3)}%"><span>${esc(span.kind)}</span></div>`;
        \\    }).join("");
        \\    return `<div class="lane"><div><div>${esc(testCase.name)}</div><div class="small muted">${formatNs(testCase.duration_ns || 0)} <span class="${statusClass(testCase.status)}">${esc(testCase.status)}</span></div></div><div class="track">${bars}</div></div>`;
        \\  }
        \\  renderSummary();
        \\  renderFailures();
        \\  renderJobs();
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

    std.Io.Dir.cwd().deleteTree(io, out_dir) catch {};
    try std.Io.Dir.cwd().createDirPath(io, raw_dir);
    try std.Io.Dir.cwd().createDirPath(io, logs_dir);

    std.debug.print("=== MINICI ORCHESTRATOR ===\n", .{});

    const build_argv = try buildCommand(allocator, zig_exe, "build-ci", null);
    const build_log = logs_dir ++ "/build-ci.txt";
    std.debug.print("Building CI steps ... ", .{});
    const build_result = try runCommand(allocator, io, build_argv, build_log);
    std.debug.print("{s} in {d:.3}s\n", .{ buildStatusText(build_result), seconds(build_result.duration_ns) });

    var results = std.ArrayList(CommandResult).empty;
    defer results.deinit(allocator);

    if (!isPass(build_result)) {
        printRerunHint(build_result);
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
        std.debug.print("Running `{s}` ... ", .{job.name});
        var result = try runCommand(allocator, io, argv, log_path);
        result.stats_path = stats_path;
        try results.append(allocator, result);
        std.debug.print("{s} in {d:.3}s\n", .{ runStatusText(result), seconds(result.duration_ns) });

        if (!isPass(result)) {
            printRerunHint(result);
        }

        if (isCheckJob(job.name) and !isPass(result)) {
            try writeReportJson(allocator, io, build_result, results.items);
            try writeHtml(allocator, io, build_result, results.items);
            std.process.exit(1);
        }
    }

    try writeReportJson(allocator, io, build_result, results.items);
    try writeHtml(allocator, io, build_result, results.items);

    for (results.items) |result| {
        if (!isPass(result)) std.process.exit(1);
    }
}
