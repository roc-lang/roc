//! Regression tests for https://github.com/roc-lang/roc/issues/12348.
//!
//! Only the entry module can be a default app. Any other headerless file is a
//! type module, so when it lacks a nominal type matching its file name it is
//! reported as a type module missing that type, never as a default app missing
//! `main!`, and a `main!` it defines neither satisfies nor fails validation.
const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const SourceFile = struct {
    path: []const u8,
    source: []const u8,
};

const ExpectedError = struct {
    file: []const u8,
    title: []const u8,
};

const app_importing_helper =
    \\import Helper
    \\
    \\main! = |_| {
    \\    Ok({})
    \\}
    \\
;

fn expectBuildErrors(files: []const SourceFile, expected: []const ExpectedError) !void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    for (files) |file| {
        try tmp.dir.writeFile(io, .{ .sub_path = file.path, .data = file.source });
    }

    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const main_path = try tmp.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(main_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    build_env.build(main_path) catch {};

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var errors: usize = 0;
    var matched: usize = 0;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            switch (report.severity) {
                .runtime_error, .fatal => {
                    errors += 1;
                    const is_expected = for (expected) |want| {
                        if (std.mem.endsWith(u8, module_reports.abs_path, want.file) and
                            std.mem.eql(u8, report.title, want.title)) break true;
                    } else false;
                    if (is_expected) {
                        matched += 1;
                    } else {
                        std.debug.print("unexpected {s} in {s}: {s}\n", .{
                            report.severity.toString(), module_reports.abs_path, report.title,
                        });
                    }
                },
                .warning => {},
            }
        }
    }

    try std.testing.expectEqual(expected.len, matched);
    try std.testing.expectEqual(expected.len, errors);
}

test "issue 12348: an empty module imported by a default app is reported as a type module" {
    try expectBuildErrors(&.{
        .{ .path = "EmptyFile.roc", .source = "" },
        .{ .path = "main.roc", .source =
        \\import EmptyFile
        \\
        \\main! = |_| {
        \\    Ok({})
        \\}
        \\
        },
    }, &.{
        .{ .file = "EmptyFile.roc", .title = "Type Module Missing Matching Type" },
    });
}

test "issue 12348: main! does not make an imported headerless module valid" {
    try expectBuildErrors(&.{
        .{ .path = "Helper.roc", .source =
        \\main! = |_| {
        \\    Ok({})
        \\}
        \\
        },
        .{ .path = "main.roc", .source = app_importing_helper },
    }, &.{
        .{ .file = "Helper.roc", .title = "Type Module Missing Matching Type" },
    });
}

test "issue 12348: main! arity is not checked in an imported headerless module" {
    try expectBuildErrors(&.{
        .{ .path = "Helper.roc", .source =
        \\Helper := [].{
        \\    greeting = "hi"
        \\}
        \\
        \\main! = |_a, _b| {
        \\    Ok({})
        \\}
        \\
        },
        .{ .path = "main.roc", .source = app_importing_helper },
    }, &.{});
}

test "issue 12348: an empty entry module is still reported as a default app missing main!" {
    try expectBuildErrors(&.{
        .{ .path = "main.roc", .source = "" },
    }, &.{
        .{ .file = "main.roc", .title = "Missing `main!` Function" },
    });
}
