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

const Expected = []const struct { []const u8, []const Demand };

/// A module file written beside `Demand.roc`: its file name and source.
const ImportedSource = struct { []const u8, []const u8 };

/// Check `source` as module `Demand.roc` and compare the demand vector of
/// each named top-level function with its expected vector.
fn expectDemands(source: []const u8, expected: Expected) error{ TestUnexpectedResult, TestExpectedEqual, OutOfMemory }!void {
    return expectDemandsWithImports(source, &.{}, expected);
}

/// `expectDemands` with `imports` written beside `Demand.roc`, which may
/// import them.
fn expectDemandsWithImports(source: []const u8, imports: []const ImportedSource, expected: Expected) error{ TestUnexpectedResult, TestExpectedEqual, OutOfMemory }!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    tmp.dir.writeFile(io, .{ .sub_path = "Demand.roc", .data = source }) catch |err| return setupFailed(err);
    for (imports) |imported| {
        const file_name, const imported_source = imported;
        tmp.dir.writeFile(io, .{ .sub_path = file_name, .data = imported_source }) catch |err| return setupFailed(err);
    }
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
            const def_name = check.CheckedArtifact.procBaseDefName(artifact.moduleEnvConst(), template.proc_base) orelse continue;
            if (std.mem.eql(u8, def_name, name)) break template;
        } else {
            std.debug.print("no template named {s}\n", .{name});
            return error.TestUnexpectedResult;
        };
        std.testing.expectEqualSlices(Demand, demands, try artifact.dispatch().templateDemands(template.template_id)) catch |err| {
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

test "a nominal argument its backing extends a row with demands all of it" {
    try expectDemands(
        \\module [kind_of, label_of, count_of, links]
        \\Ref(r) := { ..r }
        \\Labeled(r) := { label : Str, ..r }
        \\Outer(r) := { inner : Ref(r), n : U64 }
        \\Chain(r) := [Link({ tag : Str, ..r }, Chain(r)), End]
        \\kind_of : Ref(r) -> Str
        \\kind_of = |_ref| "ref"
        \\label_of : Labeled(r) -> Str
        \\label_of = |item| item.label
        \\count_of : Outer(r) -> U64
        \\count_of = |outer| outer.n
        \\links : Chain(r) -> U64
        \\links = |chain| match chain {
        \\    Chain.Link(_, rest) => 1 + links(rest)
        \\    Chain.End => 0
        \\}
    , &.{
        .{ "kind_of", &.{.full} },
        .{ "label_of", &.{.full} },
        .{ "count_of", &.{.full} },
        .{ "links", &.{.full} },
    });
}

test "a nominal argument demands what its backing does with it" {
    try expectDemands(
        \\module [held, ignore_tag, ignore_pair]
        \\Wrap(a) := List(a)
        \\Tagged(t) := U64
        \\Pair(a, b) := (a, List(b))
        \\held : Wrap(a), U64 -> U64
        \\held = |_w, n| n
        \\ignore_tag : Tagged(t), U64 -> U64
        \\ignore_tag = |_t, n| n
        \\ignore_pair : Pair(a, b), U64 -> U64
        \\ignore_pair = |_p, n| n
    , &.{
        .{ "held", &.{.none} },
        .{ "ignore_tag", &.{.none} },
        .{ "ignore_pair", &.{ .layout, .none } },
    });
}

test "an imported nominal argument demands what its declaring module's backing does with it" {
    try expectDemandsWithImports(
        \\module [kind_of, held, ignore_tag]
        \\import Shapes exposing [Ref, Wrap, Tagged]
        \\kind_of : Ref(r) -> Str
        \\kind_of = |_ref| "ref"
        \\held : Wrap(a), U64 -> U64
        \\held = |_w, n| n
        \\ignore_tag : Tagged(t), U64 -> U64
        \\ignore_tag = |_t, n| n
    , &.{.{ "Shapes.roc",
        \\module [Ref, Wrap, Tagged]
        \\Ref(r) := { ..r }
        \\Wrap(a) := List(a)
        \\Tagged(t) := U64
    }}, &.{
        .{ "kind_of", &.{.full} },
        .{ "held", &.{.none} },
        .{ "ignore_tag", &.{.none} },
    });
}
