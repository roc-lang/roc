//! Regression test for issue #10712.

const std = @import("std");
const roc_target = @import("roc_target");

const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const ExpectDuplicateDefinitionError = std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError ||
    compile_build.InitError ||
    compile_build.BuildRootError ||
    error{TestUnexpectedResult};

fn expectDuplicateDefinition(source: []const u8) ExpectDuplicateDefinitionError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "main.roc",
        .data = source,
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

    var found_duplicate_definition = false;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Duplicate Definition")) {
                found_duplicate_definition = true;
            }
        }
    }
    try std.testing.expect(found_duplicate_definition);
}

test "issue 10712: top-level value redefined after a lambda reports a duplicate definition" {
    try expectDuplicateDefinition(
        \\m = || {}
        \\m = {}
    );
}

test "issue 10712: mixed-shape shadowed bindings retain their exact identities" {
    const sources = [_][]const u8{
        \\m = || {}
        \\before = m
        \\m = {}
        \\after = m
        ,
        \\m = {}
        \\before = m
        \\m = || {}
        \\after = m
        ,
        \\m : {}
        \\other = {}
        \\m = {}
        \\after = m
        ,
    };

    for (sources) |source| {
        try expectDuplicateDefinition(source);
    }
}
