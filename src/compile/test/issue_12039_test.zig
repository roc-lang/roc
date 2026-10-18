//! Regression tests for issue #12039.
//!
//! A platform or package header that exposes a nested type its module does
//! not define (`Util.Missing`) is reported at that header entry, and an app
//! importing that public name is reported at its import, rather than either
//! check hanging.
//! repro for https://github.com/roc-lang/roc/issues/12039

const std = @import("std");
const roc_target = @import("roc_target");

const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const Issue12039TestError = std.Io.Dir.WriteFileError ||
    std.Io.Dir.CreateDirPathError ||
    std.Io.Dir.RealPathFileAllocError ||
    compile_build.InitError ||
    compile_build.BuildRootError ||
    error{ TestUnexpectedResult, TestExpectedEqual };

const SourceFile = struct {
    name: []const u8,
    source: []const u8,
};

const util_source =
    \\Util := [].{
    \\    answer : U64
    \\    answer = 42
    \\}
    \\
;

const app_platform_source =
    \\platform ""
    \\    requires {} { main! : Str => Str }
    \\    exposes []
    \\    packages {}
    \\    provides { "roc_main": run! }
    \\run! = |input| main!(input)
    \\
;

/// Check the root `main.roc` and assert that the reports more serious than a
/// warning carry exactly these titles, in report order.
fn expectErrorTitles(files: []const SourceFile, expected: []const []const u8) Issue12039TestError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    for (files) |file| {
        if (std.fs.path.dirname(file.name)) |dir| try tmp_dir.dir.createDirPath(io, dir);
        try tmp_dir.dir.writeFile(io, .{ .sub_path = file.name, .data = file.source });
    }

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const main_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(main_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    try build_env.build(main_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var error_count: usize = 0;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            if (report.severity == .warning) continue;
            try std.testing.expect(error_count < expected.len);
            try std.testing.expectEqualStrings(expected[error_count], report.title);
            error_count += 1;
        }
    }
    try std.testing.expectEqual(expected.len, error_count);
}

test "issue 12039: a platform exposing a type its imported module lacks reports an error" {
    try expectErrorTitles(&.{
        .{ .name = "Util.roc", .source = util_source },
        .{
            .name = "main.roc",
            .source =
            \\platform "demo"
            \\    requires {
            \\        main! : {} => U64
            \\    }
            \\    exposes [Util.Missing]
            \\    packages {}
            \\    provides { "roc_main": main_for_host! }
            \\    targets: {}
            \\
            \\import Util
            \\
            \\main_for_host! = |_| main!({})
            \\
            ,
        },
    }, &.{"Type Not Exposed"});
}

test "issue 12039: a package exposing a type its module lacks reports an error" {
    try expectErrorTitles(&.{
        .{ .name = "Util.roc", .source = util_source },
        .{ .name = "main.roc", .source = "package [Util.Missing] {}\n" },
    }, &.{"Type Not Exposed"});
}

test "issue 12039: an app importing a package's undefined public type reports an error" {
    try expectErrorTitles(&.{
        .{ .name = "platform.roc", .source = app_platform_source },
        .{ .name = "pkg/Util.roc", .source = util_source },
        .{ .name = "pkg/main.roc", .source = "package [Util.Missing] {}\n" },
        .{
            .name = "main.roc",
            .source =
            \\app [main!] { pf: platform "./platform.roc", pkg: "./pkg/main.roc" }
            \\
            \\import pkg.Missing
            \\
            \\main! = |input| input
            \\
            ,
        },
    }, &.{"Public Type Not Defined"});
}

test "issue 12039: an app importing a package's defined public type checks cleanly" {
    try expectErrorTitles(&.{
        .{ .name = "platform.roc", .source = app_platform_source },
        .{
            .name = "pkg/Util.roc",
            .source =
            \\Util := [].{
            \\    Present := [P(U64)].{
            \\        value : Present -> U64
            \\        value = |P(n)| n
            \\    }
            \\}
            \\
            ,
        },
        .{ .name = "pkg/main.roc", .source = "package [Util.Present] {}\n" },
        .{
            .name = "main.roc",
            .source =
            \\app [main!] { pf: platform "./platform.roc", pkg: "./pkg/main.roc" }
            \\
            \\import pkg.Present
            \\
            \\main! = |input| {
            \\    _n = Present.value(P(1))
            \\    input
            \\}
            \\
            ,
        },
    }, &.{});
}
