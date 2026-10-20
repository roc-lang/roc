//! Regression tests for issue #12455: public-module queries return only
//! modules that completed successfully, each with its checked artifact.

const std = @import("std");
const roc_target = @import("roc_target");

const BuildEnv = @import("../compile_build.zig").BuildEnv;

fn writePackage(tmp_dir: std.testing.TmpDir, foo_source: []const u8) std.Io.Dir.WriteFileError!void {
    const io = std.testing.io;
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data = "package [Foo.Bar] {}\n" });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Foo.roc", .data = foo_source });
    // A bidirectional control character rejects the source of `A`, so `A`
    // and every module importing it complete with failure.
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "A.roc",
        .data = "# \u{202E}\nA := [].{\n    a : Str\n    a = \"a\"\n}\n",
    });
}

test "issue 12455: a public module that completed with failure is not a compiled public module" {
    // `Foo` imports a module whose source is rejected, so it completes with
    // failure and its nested public type `Foo.Bar` is never resolved.
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writePackage(tmp_dir,
        \\import A
        \\
        \\Foo :: [].{
        \\    Bar :: [].{}
        \\
        \\    x : Str
        \\    x = A.a
        \\}
        \\
    );

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const root_main = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(root_main);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    build_env.setSyntheticRootPackageIdentity();
    try build_env.build(root_main);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);
    var error_reports: usize = 0;
    for (drained) |module| {
        for (module.reports) |report| {
            if (report.severity.isError()) error_reports += 1;
        }
    }
    try std.testing.expect(error_reports > 0);

    const public_modules = try build_env.getCompiledPublicModules(gpa);
    defer gpa.free(public_modules);
    try std.testing.expectEqual(@as(usize, 0), public_modules.len);
}

test "issue 12455: documentation modules carry the checked artifact of each public module" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try writePackage(tmp_dir,
        \\Foo :: [].{
        \\    Bar :: [].{}
        \\}
        \\
    );

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const root_main = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(root_main);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    build_env.setSyntheticRootPackageIdentity();
    try build_env.build(root_main);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);
    for (drained) |module| {
        for (module.reports) |report| {
            try std.testing.expect(!report.severity.isError());
        }
    }

    const docs_modules = try build_env.getDocumentationModules(gpa);
    defer gpa.free(docs_modules);
    try std.testing.expectEqual(@as(usize, 1), docs_modules.len);
    const bar = docs_modules[0];
    try std.testing.expectEqualStrings("Bar", bar.name);
    try std.testing.expect(bar.public_type_decl != null);
}
