//! Regression tests for issue #11943: a member accessed on a qualified tag of
//! an imported type, written `Alias.Path.U.v`.
//! repro for https://github.com/roc-lang/roc/issues/11943

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

fn expectNoErrors(files: []const SourceFile, entry: []const u8) BuildTestError!void {
    const gpa = std.testing.allocator;
    const rendered = try renderedErrors(gpa, files, entry);
    defer gpa.free(rendered);
    try std.testing.expectEqualStrings("", rendered);
}

const color_module =
    \\Color := [Red, Green].{
    \\    Shade := [Light, Dark].{
    \\        describe : Shade -> Str
    \\        describe = |_| "shade"
    \\    }
    \\
    \\    to_hex : Color -> Str
    \\    to_hex = |color| match color {
    \\        Red => "#f00"
    \\        Green => "#0f0"
    \\    }
    \\
    \\    with_suffix : Color, Str -> Str
    \\    with_suffix = |_, suffix| suffix
    \\}
    \\
;

fn appUsing(comptime body: []const u8) []const SourceFile {
    return &.{
        .{ .path = "Color.roc", .source = color_module },
        .{ .path = "App.roc", .source = "import Color\n\nApp :: [].{\n    " ++ body ++ "\n}\n" },
    };
}

test "issue 11943: method called directly on a qualified tag of an imported type" {
    try expectNoErrors(appUsing("hex = Color.Red.to_hex()"), "App.roc");
}

test "issue 11943: method on a qualified tag of an imported type as an arrow target" {
    try expectNoErrors(appUsing("suffixed = \"!\"->Color.Green.with_suffix()"), "App.roc");
}

test "issue 11943: method on a tag of a type nested in an imported type" {
    try expectNoErrors(appUsing("shade = Color.Shade.Dark.describe()"), "App.roc");
}

test "issue 11943: an uncalled member of an imported qualified tag is a field access" {
    const gpa = std.testing.allocator;
    const rendered = try renderedErrors(gpa, appUsing("hex = Color.Red.to_hex"), "App.roc");
    defer gpa.free(rendered);

    try std.testing.expect(std.mem.startsWith(u8, rendered, "**Type Mismatch**"));
    try std.testing.expect(std.mem.find(u8, rendered, "This is not a record") != null);
}

test "issue 11943: a nested imported type takes precedence over a tag of the same name" {
    const gpa = std.testing.allocator;
    const rendered = try renderedErrors(gpa, appUsing("missing = Color.Shade.missing()"), "App.roc");
    defer gpa.free(rendered);

    try std.testing.expect(std.mem.startsWith(u8, rendered, "**Does Not Exist**"));
    try std.testing.expect(std.mem.find(u8, rendered, "`Color.Shade.missing` does not exist.") != null);
}
