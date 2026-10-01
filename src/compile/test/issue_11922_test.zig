//! Regression tests for issue #11922: a construction that omits a defaulted
//! field whose default was rejected in its declaring module reports nothing of
//! its own, because the problem was already reported at the default.
//! repro for https://github.com/roc-lang/roc/issues/11922

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const BuildTestError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError ||
    error{ WriteFailed, TestExpectedEqual };

const SourceFile = struct {
    path: []const u8,
    source: []const u8,
};

/// Build `entry` from `files` and return the markdown rendering of every
/// non-warning report, concatenated.
fn renderedErrors(gpa: std.mem.Allocator, files: []const SourceFile, entry: []const u8) BuildTestError![]u8 {
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    for (files) |file| {
        try tmp_dir.dir.writeFile(io, .{ .sub_path = file.path, .data = file.source });
    }

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const module_path = try tmp_dir.dir.realPathFileAlloc(io, entry, gpa);
    defer gpa.free(module_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    try build_env.build(module_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var rendered: std.Io.Writer.Allocating = .init(gpa);
    errdefer rendered.deinit();
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            switch (report.severity) {
                .warning => {},
                .runtime_error, .fatal => try report.render(&rendered.writer, .markdown),
            }
        }
    }
    return rendered.toOwnedSlice();
}

const style_module =
    \\Style := { name : Str ?? base_name }.{
    \\    base_name : Str
    \\    base_name = "default"
    \\}
    \\
;

const name_not_in_scope =
    \\**Name Not In Scope**
    \\Nothing is named `base_name` in this scope.
    \\Is it misspelled, or is there an import missing?
    \\
    \\```roc
    \\Style := { name : Str ?? base_name }.{
    \\```
    \\                         ^^^^^^^^^
    \\
    \\
    \\
;

fn expectOnlyTheDefaultsError(comptime app_body: []const u8) BuildTestError!void {
    const gpa = std.testing.allocator;
    const rendered = try renderedErrors(gpa, &.{
        .{ .path = "Style.roc", .source = style_module },
        .{ .path = "App.roc", .source = "import Style\n\nApp :: [].{\n" ++ app_body ++ "}\n" },
    }, "App.roc");
    defer gpa.free(rendered);
    try std.testing.expectEqualStrings(name_not_in_scope, rendered);
}

test "issue 11922: top-level constructions omitting a rejected imported default report nothing of their own" {
    try expectOnlyTheDefaultsError(
        \\    a = Style.{}
        \\    b = Style.{}
        \\
    );
}

test "issue 11922: local constructions omitting a rejected imported default report nothing of their own" {
    try expectOnlyTheDefaultsError(
        \\    names : {} -> Str
        \\    names = |{}| {
        \\        a = Style.{}
        \\        b = Style.{}
        \\        "${a.name} ${b.name}"
        \\    }
        \\
    );
}
