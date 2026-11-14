//! Regression test for issue #12631.

const std = @import("std");
const roc_target = @import("roc_target");

const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

// repro for https://github.com/roc-lang/roc/issues/12631
//
// A record of closures passed to an annotated function, where one closure
// calls a method on the result of another closure's method call with a
// top-level constant as the argument, requires that method of the enclosing
// definition's own type variable. It must publish as a checked artifact with
// no report rather than violating a checked-artifact invariant.
test "issue 12631: record of closures capturing a top-level constant publishes its method requirement" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data =
        \\task : { run : (() -> result), resolve : (result -> Str) } -> Str
        \\task = |config| (config.resolve)((config.run)())
        \\
        \\limit : U64
        \\limit = 96
        \\
        \\read_file = |directory, name| task({
        \\    run: || directory.read(),
        \\    resolve: |result| Str.concat(name, result.take_first(limit)),
        \\})
        \\
        \\main! = |_| Ok({})
        \\
    });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const main_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(main_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    try build_env.build(main_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    for (drained) |mod| {
        try std.testing.expectEqual(@as(usize, 0), mod.reports.len);
    }
}
