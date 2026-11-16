//! Regression tests for https://github.com/roc-lang/roc/issues/13005.
//!
//! A top-level constant that calls a function with a `where` clause on an
//! argument that failed to canonicalize reports only that argument's own
//! error. The retired call still evaluates its callee at compile time, so
//! that callee's use carries checked evidence like any other evaluated
//! operand, and the requirement it instantiated adds no report of its own.

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;
const harness = @import("lower_to_lir_harness.zig");

const apply_with_where_clause =
    \\apply : p -> Str where [p.to_str : p -> Str]
    \\apply = |x| x.to_str()
    \\
    \\
;

test "issue 13005: a top-level constant passing an undefined name to a where-clause function reports only the name" {
    try expectOnlyNameNotInScope("module []\n\n" ++ apply_with_where_clause ++
        \\shown = apply(nope)
        \\
    );
}

test "issue 13005: a discarded call passing an undefined name to a where-clause function reports only the name" {
    try expectOnlyNameNotInScope("module []\n\n" ++ apply_with_where_clause ++
        \\shown = {
        \\    _ = apply(nope)
        \\    "x"
        \\}
        \\
    );
}

test "issue 13005: a top-level constant reaching such a call through a function lowers" {
    try harness.expectLowersToLirWithOptions(apply_with_where_clause ++
        \\f = |_| apply(nope)
        \\
        \\shown = f(1)
        \\
        \\main! = |_args| {
        \\    _ = shown
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Name Not In Scope", .specialization_strategy = .boxy });
}

const BuildError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

fn expectOnlyNameNotInScope(source: []const u8) BuildError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "Repro.roc", .data = source });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(path);

    var build = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io, .lss);
    defer build.deinit();
    try build.build(path);
    const reports = try build.drainReports();
    defer build.freeDrainedReports(reports);

    var name_not_in_scope: usize = 0;
    for (reports) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Name Not In Scope")) {
                name_not_in_scope += 1;
            } else {
                // The retired call adds no error of its own.
                if (report.severity != .warning) std.debug.print("unexpected report: {s}\n", .{report.title});
                try std.testing.expectEqual(.warning, report.severity);
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 1), name_not_in_scope);
}
