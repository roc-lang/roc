//! Regression test for issue #12434.
//!
//! When an app's public API reaches procedure templates owned by another
//! module, the root artifact's `lowering_visibility.module_ids` lists every
//! visible checked artifact exactly once: no two entries share key `.bytes`,
//! the identity `CheckedModuleArtifactKey.eql` compares.
//! repro for https://github.com/roc-lang/roc/issues/12434

const std = @import("std");
const roc_target = @import("roc_target");

const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const platform_source =
    \\platform ""
    \\    requires {
    \\        main! : () => {}
    \\    }
    \\    exposes []
    \\    packages {}
    \\    provides { "roc_main": main_for_host! }
    \\
    \\main_for_host! : () => {}
    \\main_for_host! = main!
    \\
;

const app_source =
    \\app [main!] { pf: platform "./platform.roc" }
    \\
    \\import Greeter
    \\
    \\main! = || {
    \\    dbg Greeter.greet("world")
    \\}
    \\
;

const greeter_source =
    \\import Helper
    \\
    \\Greeter :: {}.{
    \\    greet : Str -> Str
    \\    greet = |name| Helper.exclaim(Str.concat("Hello, ", name))
    \\}
    \\
;

const helper_source =
    \\Helper :: {}.{
    \\    exclaim : Str -> Str
    \\    exclaim = |text| Str.concat(text, "!")
    \\}
    \\
;

test "issue 12434: lowering visibility lists each visible artifact once" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{ .sub_path = "platform.roc", .data = platform_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data = app_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Greeter.roc", .data = greeter_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Helper.roc", .data = helper_source });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const main_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(main_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    try build_env.build(main_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);
    for (drained) |module_reports| {
        try std.testing.expectEqual(@as(usize, 0), module_reports.reports.len);
    }

    const root_artifact = build_env.executableRootCheckedArtifact();
    const module_ids = root_artifact.lowering_visibility.module_ids;
    for (module_ids, 0..) |key, i| {
        for (module_ids[i + 1 ..]) |other| {
            try std.testing.expect(!std.mem.eql(u8, &key.bytes, &other.bytes));
        }
    }

    // Every visible artifact other than the root and builtins resolves to a
    // distinct checked artifact.
    const artifacts = try build_env.collectVisibleArtifacts(gpa, root_artifact);
    defer gpa.free(artifacts);
    for (artifacts, 0..) |artifact, i| {
        for (artifacts[i + 1 ..]) |other| {
            try std.testing.expect(artifact != other);
        }
    }
}
