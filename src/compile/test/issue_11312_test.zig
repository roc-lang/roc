//! Regression tests for issue #11312: a `crash` whose message is not a `Str`
//! reports the type mismatch once. The crash itself becomes the checked
//! runtime error, and a compile-time root whose evaluation can call into code
//! checking replaced with a runtime error is never requested, so the problem
//! is not reported a second time as a compile-time crash. Independent roots
//! are still evaluated.
//! repro for https://github.com/roc-lang/roc/issues/11312

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const RecoveryError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

const independent_defs =
    \\helper = |n| n + 1
    \\good = 123.U64
    \\fine = helper(1.U64)
    \\
;

/// Checks `source` as a module and expects exactly one type mismatch and no
/// other error. The root whose source is `blocked_expr` must not be requested,
/// while the independent `good` and `fine` roots are evaluated.
fn expectRecovery(source: []const u8, imported_source: ?[]const u8, blocked_expr: []const u8) RecoveryError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const module_source = try std.mem.concat(gpa, u8, &.{ "module []\n", source, independent_defs });
    defer gpa.free(module_source);
    try tmp.dir.writeFile(io, .{ .sub_path = "Repro.roc", .data = module_source });
    if (imported_source) |imported| try tmp.dir.writeFile(io, .{ .sub_path = "Broken.roc", .data = imported });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(path);
    var build = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build.deinit();
    try build.build(path);
    const reports = try build.drainReports();
    defer build.freeDrainedReports(reports);
    var mismatches: usize = 0;
    for (reports) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Type Mismatch")) {
                mismatches += 1;
            } else {
                if (report.severity != .warning) std.debug.print("unexpected report: {s}\n", .{report.title});
                try std.testing.expectEqual(.warning, report.severity);
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 1), mismatches);

    const artifact = build.findModuleByPath(path).?.semanticData().?.checked_artifact.?;
    var found_blocked = false;
    var found_good = false;
    var found_fine = false;
    for (artifact.compile_time_roots.roots) |root| {
        const expr = artifact.checked_bodies.expr(root.expr);
        const expr_source = module_source[expr.source_region.start.offset..expr.source_region.end.offset];
        if (std.mem.eql(u8, expr_source, blocked_expr)) {
            try std.testing.expect(artifact.compileTimeRootReachesCheckedError(root));
            try std.testing.expectEqual(.ineligible, root.request_eligibility);
            for (artifact.root_requests.compile_time_requests) |request| {
                try std.testing.expect(request.compile_time_root != root.id);
            }
            found_blocked = true;
        }
        if (std.mem.eql(u8, expr_source, "123.U64") or std.mem.eql(u8, expr_source, "helper(1.U64)")) {
            try std.testing.expect(!artifact.compileTimeRootReachesCheckedError(root));
            try std.testing.expectEqual(.eligible, root.request_eligibility);
            try std.testing.expect(root.payload == .const_node);
            if (expr_source[0] == '1') found_good = true else found_fine = true;
        }
    }
    try std.testing.expect(found_blocked);
    try std.testing.expect(found_good);
    try std.testing.expect(found_fine);
}

test "issue 11312: a root calling a function whose crash message is not a Str is not evaluated" {
    try expectRecovery(
        \\poly = || {
        \\    crash YYYYY
        \\    "x"
        \\}
        \\result = poly() == poly()
        \\
    , null, "poly() == poly()");
}

test "issue 11312: a root calling a function whose crash message is erroneous is not evaluated" {
    try expectRecovery(
        \\f : U64 -> Str
        \\f = |_| "m"
        \\poly = || {
        \\    crash f(Bad)
        \\    "x"
        \\}
        \\result = poly() == poly()
        \\
    , null, "poly() == poly()");
}

test "issue 11312: a root calling a function whose body contains a rejected call is not evaluated" {
    try expectRecovery(
        \\f : U64 -> Str
        \\f = |_| "m"
        \\poly = || {
        \\    _s = f(Bad)
        \\    "x"
        \\}
        \\result = poly() == poly()
        \\
    , null, "poly() == poly()");
}

test "issue 11312: a root reading a constant that calls into checked-error code is not evaluated" {
    try expectRecovery(
        \\poly = || {
        \\    crash YYYYY
        \\    "x"
        \\}
        \\first = poly()
        \\result = first == "x"
        \\
    , null, "first == \"x\"");
}

test "issue 11312: a root calling an imported function whose crash message is not a Str is not evaluated" {
    try expectRecovery(
        \\import Broken
        \\result = Broken.poly({}) == "x"
        \\
    ,
        \\module [poly]
        \\
        \\poly : {} -> Str
        \\poly = |_| {
        \\    crash YYYYY
        \\    "x"
        \\}
        \\
    , "Broken.poly({}) == \"x\"");
}
