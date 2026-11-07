//! Regression tests for issue #11175: rejected literal patterns must record
//! checked errors instead of reaching the literal conversion invariant.

const std = @import("std");
const check = @import("check");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const PatternTestError = compile_build.InitError ||
    compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError ||
    error{ TestUnexpectedResult, TestExpectedEqual };

test "issue 11175: a numeral parameter pattern typed as a nominal reports rather than aborting" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "Pt.roc",
        .data =
        \\Pt := { depth : U8, n : U64 }.{
        \\    f : Pt -> U8
        \\
        \\    f = |3| match s {
        \\        Pt([a]) => a
        \\        _ => 0
        \\    }
        \\}
        \\
        ,
    });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const module_path = try tmp_dir.dir.realPathFileAlloc(io, "Pt.roc", gpa);
    defer gpa.free(module_path);
    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    try build_env.build(module_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);
    var found_type_mismatch = false;
    var found_name_not_in_scope = false;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Type Mismatch")) found_type_mismatch = true;
            if (std.mem.eql(u8, report.title, "Name Not In Scope")) found_name_not_in_scope = true;
        }
    }
    try std.testing.expect(found_type_mismatch);
    try std.testing.expect(found_name_not_in_scope);
}

fn expectCheckedPatterns(body: []const u8, expected_error: ?[]const u8) PatternTestError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const source = try std.fmt.allocPrint(gpa,
        \\module [f, good]
        \\Pt := {{ depth : U8, n : U64 }}
        \\{s}
        \\good : U64
        \\good = 123
        \\
    , .{body});
    defer gpa.free(source);
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "Repro.roc", .data = source });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(path);
    var build = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build.deinit();
    try build.build(path);
    const reports = try build.drainReports();
    defer build.freeDrainedReports(reports);
    var found_error = false;
    for (reports) |module_reports| {
        for (module_reports.reports) |report| {
            if (expected_error) |title| {
                if (std.mem.eql(u8, report.title, title)) found_error = true;
            } else {
                try std.testing.expectEqual(.warning, report.severity);
            }
        }
    }
    try std.testing.expectEqual(expected_error != null, found_error);

    // A failed pattern cannot discard an independent definition's checked body.
    const checked = build.findModuleByPath(path).?.semanticData().?.checked_module.?;
    var found_good = false;
    const bodies = try checked.bodies();
    const store = &checked.moduleEnvConst().store;
    for (0..bodies.nodeCount()) |i| {
        if (bodies.index().nodeKind(@intCast(i)) != .expr) continue;
        const expr: check.CheckedBodies.ExprId = @fromBackingInt(@intCast(i));
        const region = store.getExprRegion(expr);
        const text = source[region.start.offset..region.end.offset];
        if (std.mem.eql(u8, text, "123")) {
            try std.testing.expect(bodies.exprData(expr) == .numeral);
            found_good = true;
        }
    }
    try std.testing.expect(found_good);
}

test "issue 11175: quoted parameter failure checks" {
    try expectCheckedPatterns(
        \\f : Pt -> U8
        \\f = |"bad"| 0
    , "Type Mismatch");
}

test "issue 11175: nested parameter failure checks" {
    try expectCheckedPatterns(
        \\f : { value : Pt }, U8 -> U8
        \\f = |{ value: 3 }, x| x
    , "Type Mismatch");
}

test "issue 11175: captured parameter failure checks" {
    try expectCheckedPatterns(
        \\f = |n| {
        \\    inner : Pt -> U8
        \\    inner = |3| n
        \\    inner
        \\}
    , "Type Mismatch");
}

test "issue 11175: merged parameter failures check" {
    try expectCheckedPatterns(
        \\f : List(Pt) -> U8
        \\f = |[3, 4]| 0
    , "Type Mismatch");
}

test "issue 11175: local destructure failure checks" {
    try expectCheckedPatterns(
        \\f = || {
        \\    (3, n) = (Pt({ depth: 0, n: 0 }), 1)
        \\    n
        \\}
    , "Type Mismatch");
}

test "issue 11175: top-level destructure failure checks" {
    try expectCheckedPatterns(
        \\(3, f) = (Pt({ depth: 0, n: 0 }), 1)
    , "Type Mismatch");
}

test "issue 11175: loop pattern failure checks" {
    try expectCheckedPatterns(
        \\f = || {
        \\    for 3 in [Pt({ depth: 0, n: 0 })] {
        \\        {}
        \\    }
        \\    0
        \\}
    , "Type Mismatch");
}

test "issue 11175: builtin literal patterns remain valid" {
    try expectCheckedPatterns(
        \\f : U8 -> U8
        \\f = |x| match x {
        \\    3 => 1
        \\    _ => 0
        \\}
    , null);
}

test "issue 11175: successful custom conversion with rejected equality checks" {
    try expectCheckedPatterns(
        \\MyNum := [Wrap(U8)].{
        \\    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Ok(Wrap(3))
        \\}
        \\f : MyNum -> U8
        \\f = |3| 0
    , "Missing Method");
}

test "issue 11175: custom literal pattern remains valid" {
    try expectCheckedPatterns(
        \\MyNum := [Wrap(U8)].{
        \\    from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Ok(Wrap(3))
        \\    is_eq : MyNum, MyNum -> Bool
        \\    is_eq = |Wrap(a), Wrap(b)| a == b
        \\}
        \\f : MyNum -> U8
        \\f = |x| match x {
        \\    3 => 1
        \\    _ => 0
        \\}
    , null);
}

test "issue 11175: importers can consume rejected and independent destructure exports" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{
        .sub_path = "Broken.roc",
        .data =
        \\module [bad, bad_fn, good]
        \\(3, bad, bad_fn) = ("wrong", 1, |n| n)
        \\good : U64
        \\good = 123
        \\
        ,
    });
    try tmp.dir.writeFile(io, .{
        .sub_path = "Consumer.roc",
        .data =
        \\module [bad, call_bad, good]
        \\import Broken
        \\bad = Broken.bad
        \\call_bad = Broken.bad_fn(1.U64)
        \\good : U64
        \\good = Broken.good
        \\
        ,
    });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Consumer.roc", gpa);
    defer gpa.free(path);
    var build = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build.deinit();
    try build.build(path);
    try std.testing.expect(build.findModuleByPath(path).?.semanticData().?.checked_module != null);
    const reports = try build.drainReports();
    defer build.freeDrainedReports(reports);
    var found_error = false;
    for (reports) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Type Mismatch")) found_error = true;
        }
    }
    try std.testing.expect(found_error);
}
