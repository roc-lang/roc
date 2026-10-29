//! Regression test for issue #12624: an `if` whose `else` branch is a `Str`
//! while the `then` branch is an `Ok(..)` after a `?`, inside a callback
//! passed from an `expect` to a function that calls a method on the `Ok`
//! payload and interpolates the `Err` payload, reports the branch mismatch
//! instead of crashing in post-check monotype.
//! repro for https://github.com/roc-lang/roc/issues/12624

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const repro_module =
    \\Repro := [].{}
    \\
    \\run = |exec| {
    \\    match exec("a") {
    \\        Ok(output) => Ok(output.trim())
    \\        Err(e) => Err("${Str.inspect(e)}")
    \\    }
    \\}
    \\
    \\expect {
    \\    _ = run(|s| {
    \\        if Str.is_empty(s) {
    \\            _ = Err(Boom)?
    \\            Ok("")
    \\        } else {
    \\            s
    \\        }
    \\    })
    \\    Bool.True
    \\}
    \\
;

test "issue 12624: if branch mismatch in a callback using `?` reports a type mismatch" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "Repro.roc", .data = repro_module });
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
}
