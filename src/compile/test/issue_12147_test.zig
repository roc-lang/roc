//! Regression test for issue #12147.

const std = @import("std");
const roc_target = @import("roc_target");

const BuildEnv = @import("../compile_build.zig").BuildEnv;

test "issue 12147: a header declaring the same package shorthand twice is rejected" {
    // Repro for https://github.com/roc-lang/roc/issues/12147.
    // Declaring `dep` twice in one header must produce an error report rather
    // than silently binding `dep` to one of the two packages. Nothing imports
    // `dep`, so the duplicate shorthand is the only problem in this workspace.
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(io, "root");
    try tmp_dir.dir.createDirPath(io, "a");
    try tmp_dir.dir.createDirPath(io, "b");
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "root/main.roc",
        .data =
        \\package [Root] {
        \\    dep: "../a/main.roc",
        \\    dep: "../b/main.roc",
        \\}
        \\
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "root/Root.roc",
        .data =
        \\Root := [].{
        \\    value : Str
        \\    value = "root"
        \\}
        \\
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "a/main.roc",
        .data = "package [OnlyInA] {}\n",
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "a/OnlyInA.roc",
        .data =
        \\OnlyInA := [].{
        \\    value : Str
        \\    value = "a"
        \\}
        \\
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "b/main.roc",
        .data = "package [OnlyInB] {}\n",
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "b/OnlyInB.roc",
        .data =
        \\OnlyInB := [].{
        \\    value : Str
        \\    value = "b"
        \\}
        \\
        ,
    });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const root_main = try tmp_dir.dir.realPathFileAlloc(io, "root/main.roc", gpa);
    defer gpa.free(root_main);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    // A rejected header may stop the build early; either way the rejection
    // must surface as an error report.
    build_env.build(root_main) catch |err| switch (err) {
        error.OutOfMemory => return err,
        else => {},
    };

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var error_reports: usize = 0;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            if (report.severity.isError()) error_reports += 1;
        }
    }
    try std.testing.expect(error_reports > 0);
}
