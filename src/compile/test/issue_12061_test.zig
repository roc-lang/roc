//! Regression tests for https://github.com/roc-lang/roc/issues/12061.
//!
//! A method whose annotation names an undeclared type declares no type. A
//! dispatch to it, reached through an unannotated function's argument, must
//! be rejected during checking instead of relating its constraint to the
//! erroneous type, which would carry the error into an instantiation that
//! post-check lowering consumes.

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const method_with_undeclared_type =
    \\Foo := [Foo].{
    \\    get : Foo -> Try(U64, [Bad(Missing)])
    \\    get = |_| Ok(1)
    \\}
    \\
;

const generic_use =
    \\call = |x| x.get().is_ok()
    \\
    \\value = call(Foo.Foo)
    \\
;

test "issue 12061: a dispatch to a method with an undeclared type in its annotation reports it" {
    try expectOnlyUndeclaredType("module []\n\n" ++ method_with_undeclared_type ++ generic_use, null);
}

test "issue 12061: a dispatch checked before the method's body still reports the undeclared type" {
    try expectOnlyUndeclaredType("module []\n\n" ++ generic_use ++ method_with_undeclared_type, null);
}

test "issue 12061: an imported method with an undeclared type in its annotation reports it" {
    try expectOnlyUndeclaredType(
        \\module []
        \\
        \\import Foo
        \\
        \\
    ++ generic_use, method_with_undeclared_type);
}

const BuildError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

fn expectOnlyUndeclaredType(source: []const u8, imported_foo: ?[]const u8) BuildError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "Repro.roc", .data = source });
    if (imported_foo) |foo| try tmp.dir.writeFile(io, .{ .sub_path = "Foo.roc", .data = foo });
    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const path = try tmp.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(path);

    var build = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build.deinit();
    try build.build(path);
    const reports = try build.drainReports();
    defer build.freeDrainedReports(reports);

    var undeclared: usize = 0;
    for (reports) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Undeclared Type")) {
                undeclared += 1;
            } else {
                // The rejected dispatch adds no second error of its own.
                if (report.severity != .warning) std.debug.print("unexpected report: {s}\n", .{report.title});
                try std.testing.expectEqual(.warning, report.severity);
            }
        }
    }
    try std.testing.expectEqual(@as(usize, 1), undeclared);
}
