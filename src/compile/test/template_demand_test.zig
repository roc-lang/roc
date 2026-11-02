//! Tests for the demand each checked procedure template's body makes on its
//! quantified variables (design.md "Specialization By Demand").

const std = @import("std");
const check = @import("check");
const roc_target = @import("roc_target");
const BuildEnv = @import("../compile_build.zig").BuildEnv;

const CheckedArtifact = check.CheckedArtifact;
const Demand = CheckedArtifact.SchemeVarDemand;

fn setupFailed(err: anytype) error{TestUnexpectedResult} {
    std.debug.print("building the test module failed: {s}\n", .{@errorName(err)});
    return error.TestUnexpectedResult;
}

/// Check `source` as module `Demand.roc` and compare the demand vector of
/// each named top-level function with its expected vector.
fn expectDemands(source: []const u8, expected: []const struct { []const u8, []const Demand }) error{ TestUnexpectedResult, TestExpectedEqual, OutOfMemory }!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    tmp.dir.writeFile(io, .{ .sub_path = "Demand.roc", .data = source }) catch |err| return setupFailed(err);
    const cwd = tmp.dir.realPathFileAlloc(io, ".", gpa) catch |err| return setupFailed(err);
    defer gpa.free(cwd);
    const path = tmp.dir.realPathFileAlloc(io, "Demand.roc", gpa) catch |err| return setupFailed(err);
    defer gpa.free(path);
    var build = BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io) catch |err| return setupFailed(err);
    defer build.deinit();
    build.build(path) catch |err| return setupFailed(err);
    const reports = build.drainReports() catch |err| return setupFailed(err);
    defer build.freeDrainedReports(reports);
    for (reports) |module_reports| for (module_reports.reports) |report| {
        if (std.mem.eql(u8, report.title, "Module Header Deprecated")) continue;
        std.debug.print("unexpected report: {s}\n", .{report.title});
        return error.TestUnexpectedResult;
    };
    const artifact = build.findModuleByPath(path).?.semanticData().?.checked_artifact.?;
    const templates = &artifact.checked_procedure_templates;
    for (expected) |entry| {
        const name, const demands = entry;
        const template = for (templates.templates.items) |*template| {
            const export_name = artifact.canonical_names.procBase(template.proc_base).export_name orelse continue;
            if (std.mem.eql(u8, artifact.canonical_names.exportNameText(export_name), name)) break template;
        } else {
            std.debug.print("no template named {s}\n", .{name});
            return error.TestUnexpectedResult;
        };
        std.testing.expectEqualSlices(Demand, demands, templates.templateDemands(template)) catch |err| {
            std.debug.print("template {s}\n", .{name});
            return err;
        };
    }
}

test "a body that only reads a list's length demands nothing of its element" {
    try expectDemands(
        \\module [count, empty, both]
        \\count : List(a) -> U64
        \\count = |list| List.len(list)
        \\empty : List(a) -> Bool
        \\empty = |list| list.is_empty()
        \\both : List(a), List(b) -> U64
        \\both = |xs, ys| List.len(xs) + List.len(ys)
    , &.{
        .{ "count", &.{.none} },
        .{ "empty", &.{.none} },
        .{ "both", &.{ .none, .none } },
    });
}

test "a body that moves elements demands their layout" {
    try expectDemands(
        \\module [first, push, keep]
        \\first : List(a) -> Try(a, [ListWasEmpty])
        \\first = |list| List.first(list)
        \\push : List(a), a -> List(a)
        \\push = |list, item| List.append(list, item)
        \\keep : List(a), List(b) -> List(a)
        \\keep = |xs, ys| if List.len(ys) == 0 { xs } else { List.drop_first(xs, 1) }
    , &.{
        // The error row stays open, and its extension is a row.
        .{ "first", &.{ .layout, .full } },
        .{ "push", &.{.layout} },
        .{ "keep", &.{ .layout, .none } },
    });
}

test "a body that inspects a variable demands all of it" {
    try expectDemands(
        \\module [holds, same, apply]
        \\holds : List(a), a -> Bool where [a.is_eq : a, a -> Bool]
        \\holds = |list, item| List.contains(list, item)
        \\same : a, a -> Bool where [a.is_eq : a, a -> Bool]
        \\same = |x, y| x == y
        \\apply : List(a), (a -> U64) -> List(U64)
        \\apply = |list, f| List.map(list, f)
    , &.{
        .{ "holds", &.{.full} },
        .{ "same", &.{.full} },
        .{ "apply", &.{.full} },
    });
}

test "a value held by itself sizes its parameter" {
    try expectDemands(
        \\module [ignore]
        \\ignore : a, U64 -> U64
        \\ignore = |_x, n| n
    , &.{
        .{ "ignore", &.{.layout} },
    });
}

test "demand propagates around recursion" {
    try expectDemands(
        \\module [ping, pong, look, seek]
        \\ping : List(a), U64 -> U64
        \\ping = |list, n| if n == 0 { List.len(list) } else { pong(list, n - 1) }
        \\pong : List(a), U64 -> U64
        \\pong = |list, n| if n == 0 { 0 } else { ping(list, n - 1) }
        \\look : List(a), a, U64 -> Bool where [a.is_eq : a, a -> Bool]
        \\look = |list, x, n| if n == 0 { List.contains(list, x) } else { seek(list, x, n - 1) }
        \\seek : List(a), a, U64 -> Bool where [a.is_eq : a, a -> Bool]
        \\seek = |list, x, n| if n == 0 { False } else { look(list, x, n - 1) }
    , &.{
        .{ "ping", &.{.none} },
        .{ "pong", &.{.none} },
        .{ "look", &.{.full} },
        .{ "seek", &.{.full} },
    });
}
