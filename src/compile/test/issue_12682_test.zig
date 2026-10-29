//! Regression test for issue #12682: a record update that mismatches its
//! annotation, whose base is read by an earlier top-level constant, reports
//! the type mismatch instead of crashing in post-check monotype. The
//! companion cases pin the base's other mismatching consumers.
//! repro for https://github.com/roc-lang/roc/issues/12682

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const MismatchTestError = compile_build.InitError ||
    compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError ||
    error{TestExpectedEqual};

/// Build `source` as `main.roc` and require exactly one "Type Mismatch"
/// report, with every other report only a warning.
fn expectSingleTypeMismatch(source: []const u8) MismatchTestError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "main.roc", .data = source });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "main.roc", gpa);
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
}

test "issue 12682: mismatched record update whose base an earlier constant reads reports a type mismatch" {
    try expectSingleTypeMismatch(
        \\base : { n : U8 }
        \\base = { n: 1 }
        \\
        \\shown = Str.inspect(base)
        \\
        \\updated : { n : I64 }
        \\updated = { ..base, n: 2 }
        \\
        \\main! = |_args| {
        \\    echo!(shown)
        \\    Ok({})
        \\}
        \\
    );
}

test "issue 12682: mismatched lookup whose binding an earlier constant reads reports a type mismatch" {
    try expectSingleTypeMismatch(
        \\base : { n : U8 }
        \\base = { n: 1 }
        \\
        \\shown = Str.inspect(base)
        \\
        \\updated : { n : I64 }
        \\updated = base
        \\
        \\main! = |_args| {
        \\    echo!(shown)
        \\    Ok({})
        \\}
        \\
    );
}

test "issue 12682: record update field mismatch whose base an earlier constant reads reports a type mismatch" {
    try expectSingleTypeMismatch(
        \\base : { n : U8 }
        \\base = { n: 1 }
        \\
        \\shown = Str.inspect(base)
        \\
        \\updated = { ..base, n: "x" }
        \\
        \\main! = |_args| {
        \\    echo!(shown)
        \\    echo!(Str.inspect(updated))
        \\    Ok({})
        \\}
        \\
    );
}
