//! Regression tests for issue #10792.
//!
//! A hosted declaration the platform header's `hosted` section does not name
//! has no linker symbol and no dispatch slot. Checking reports it against the
//! section it is missing from, including when the declaration is in the
//! platform's own root module and when canonicalization rejected it outright,
//! and a platform that also carries a compile-time root reports rather than
//! aborting the compiler.

const std = @import("std");
const roc_target = @import("roc_target");

const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const CheckPlatformError = std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError ||
    compile_build.InitError ||
    compile_build.BuildRootError ||
    error{TestUnexpectedResult};

const SourceFile = struct {
    name: []const u8,
    source: []const u8,
};

/// Check a platform rooted at `main.roc` and report whether any diagnostic
/// titled `expected_title` came back.
fn checkPlatformReports(files: []const SourceFile, expected_title: []const u8) CheckPlatformError!bool {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    for (files) |file| {
        try tmp_dir.dir.writeFile(io, .{
            .sub_path = file.name,
            .data = file.source,
        });
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

    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, expected_title)) return true;
        }
    }
    return false;
}

test "issue 10792: an unnamed declaration in a platform root reports the hosted section" {
    try std.testing.expect(try checkPlatformReports(&.{.{
        .name = "main.roc",
        .source =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try(_, [Exit(I8), ..]) }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\
        \\never_hosted : Str
        \\
        \\comptime_root = {}
        \\
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |_| 0
        \\
        ,
    }}, "Invalid Hosted Section"));
}

test "issue 10792: a rejected declaration in a platform root names no hosted function" {
    try std.testing.expect(try checkPlatformReports(&.{.{
        .name = "main.roc",
        .source =
        \\platform ""
        \\    requires {
        \\        [M : l] for a : { e : e }
        \\    }
        \\    exposes []
        \\    packages {}
        \\    provides {}
        \\a : { i : I }
        \\
        \\r : o
        \\
        \\a0 = {}
        \\
        ,
    }}, "Invalid Hosted Section"));
}

test "issue 10792: a hosted declaration the platform header omits reports an invalid hosted section" {
    try std.testing.expect(try checkPlatformReports(&.{
        .{
            .name = "Host.roc",
            .source =
            \\Host := [].{
            \\    double! : I64 => I64
            \\    triple! : I64 => I64
            \\}
            \\
            ,
        },
        .{
            .name = "main.roc",
            .source =
            \\platform ""
            \\    requires {
            \\        main! : I64 => I64
            \\    }
            \\    exposes [Host]
            \\    packages {}
            \\    provides { "roc_main": main_for_host! }
            \\    hosted {
            \\        "roc_host_double": Host.double!,
            \\    }
            \\
            \\import Host
            \\
            \\comptime_root = {}
            \\
            \\main_for_host! : I64 => I64
            \\main_for_host! = |n| main!(n)
            \\
            ,
        },
    }, "Invalid Hosted Section"));
}
