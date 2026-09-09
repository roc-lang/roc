//! Regression test for issue #11233: a chained range under a unary operator must
//! report the chained-range error instead of leaving an unresolved associated
//! lookup for checked artifact publication.
//! repro for https://github.com/roc-lang/roc/issues/11233

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

test "issue 11233: negated chained range reports rather than aborting publication" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "Repro.roc",
        .data =
        \\r = !1..<5..<10
        \\
        ,
    });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const module_path = try tmp_dir.dir.realPathFileAlloc(io, "Repro.roc", gpa);
    defer gpa.free(module_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    try build_env.build(module_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var found_chained_range = false;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Chained Range")) found_chained_range = true;
        }
    }
    try std.testing.expect(found_chained_range);
}
