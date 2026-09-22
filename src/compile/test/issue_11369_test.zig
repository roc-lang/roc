//! Regression tests for issue #11369: a where-clause method whose signature is
//! an erroneous annotation must publish as a checked error at every concrete
//! use instead of aborting publication with an unresolvable dispatch target.

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const RejectedSignatureTestError = compile_build.InitError ||
    compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError ||
    error{ TestUnexpectedResult, TestExpectedEqual };

fn expectReportsAndPublication(source: []const u8, expected_titles: []const []const u8) RejectedSignatureTestError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
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
    for (expected_titles) |title| {
        var found = false;
        for (reports) |module_reports| {
            for (module_reports.reports) |report| {
                if (std.mem.eql(u8, report.title, title)) found = true;
            }
        }
        if (!found) {
            std.debug.print("expected a \"{s}\" report\n", .{title});
            return error.TestUnexpectedResult;
        }
    }

    try std.testing.expect(build.findModuleByPath(path).?.semanticData().?.checked_artifact != null);
}

test "issue 11369: erroneous where-method signature used at a concrete type publishes" {
    // `y`'s annotation instantiates `b` at its use, where `b` meets `I64`.
    // The where-method `n` has an undeclared type as its whole signature, so
    // the obligation `I64.n` can never be discharged; checking reports the
    // undeclared type and the `{}` mismatch, and publication must finish.
    try expectReportsAndPublication(
        \\module [f]
        \\
        \\f : b -> I64 where [b.n : Missing]
        \\f = |_x| {
        \\    y : b
        \\    y = {}
        \\    y
        \\}
        \\
    , &.{ "Undeclared Type", "Type Mismatch" });
}

test "issue 11369: original reproduction publishes" {
    try expectReportsAndPublication(
        \\module [f, t]
        \\
        \\f : (a -> b) -> (I64 -> I64) where [a.d : a, b.n : I]
        \\f = |f| {
        \\    x = |0| {
        \\        y : b
        \\        y = ()
        \\        y
        \\    }
        \\    x
        \\}
        \\
        \\t = {
        \\    (|| 0)
        \\}
        \\
    , &.{"Undeclared Type"});
}
