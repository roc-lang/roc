//! Regression test for issue #10105.

const std = @import("std");
const roc_target = @import("roc_target");

const BuildEnv = @import("../compile_build.zig").BuildEnv;

test "issue 10105: package header resolves a local sibling package" {
    // Repro for https://github.com/roc-lang/roc/issues/10105.
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(io, "root");
    try tmp_dir.dir.createDirPath(io, "dep");
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "root/main.roc",
        .data = "package [Root] { dep: \"../dep/main.roc\" }\n",
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "dep/main.roc",
        .data = "package [Dep] {}\n",
    });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const root_main = try tmp_dir.dir.realPathFileAlloc(io, "root/main.roc", gpa);
    defer gpa.free(root_main);
    const dep_main = try tmp_dir.dir.realPathFileAlloc(io, "dep/main.roc", gpa);
    defer gpa.free(dep_main);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    try build_env.discoverDependencies(root_main);

    const root_pkg_name = build_env.discovered_pkg_name orelse return error.TestUnexpectedResult;
    const root_pkg = build_env.packages.getPtr(root_pkg_name) orelse return error.TestUnexpectedResult;
    const dep = root_pkg.shorthands.get("dep") orelse return error.TestUnexpectedResult;

    try std.testing.expectEqualStrings(dep_main, dep.root_file);
    try std.testing.expect(build_env.packages.getPtr(dep.name) != null);
}
