//! Regression tests for issue #11465: a qualified record-builder suffix must
//! resolve `map2` on the type it names, not on whatever unqualified type of the
//! same final name happens to be in scope.
//! repro for https://github.com/roc-lang/roc/issues/11465

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

const builder_type_module =
    \\Builder(a) := [Builder(a)].{
    \\    map2 : Builder(a), Builder(b), (a, b -> c) -> Builder(c)
    \\    map2 = |Builder(a), Builder(b), combine| Builder(combine(a, b))
    \\
    \\    one : Builder(U64)
    \\    one = Builder(1)
    \\
    \\    two : Builder(U64)
    \\    two = Builder(2)
    \\
    \\    three : Builder(U64)
    \\    three = Builder(3)
    \\}
    \\
;

const nested_builder_gui =
    \\Gui :: [].{
    \\    Builder(a) := [Builder(a)].{
    \\        map2 : Builder(a), Builder(b), (a, b -> c) -> Builder(c)
    \\        map2 = |Builder(a), Builder(b), combine| Builder(combine(a, b))
    \\
    \\        one : Builder(U64)
    \\        one = Builder(1)
    \\
    \\        two : Builder(U64)
    \\        two = Builder(2)
    \\
    \\        three : Builder(U64)
    \\        three = Builder(3)
    \\    }
    \\}
    \\
;

test "issue 11465: qualified record-builder suffix resolves a type alias exposed by a facade module" {
    try expectNoErrors(&.{
        .{ .path = "Builder.roc", .source = builder_type_module },
        .{ .path = "Gui.roc", .source =
        \\import Builder
        \\
        \\Gui :: [].{
        \\    Builder(a) : Builder.Builder(a)
        \\
        \\    one : Builder(U64)
        \\    one = Builder.one
        \\
        \\    two : Builder(U64)
        \\    two = Builder.two
        \\
        \\    three : Builder(U64)
        \\    three = Builder.three
        \\}
        \\
        },
        .{ .path = "App.roc", .source =
        \\import Gui
        \\
        \\App :: [].{
        \\    built = { first: Gui.one, second: Gui.two, third: Gui.three }.Gui.Builder
        \\}
        \\
        },
    }, "App.roc");
}

test "issue 11465: qualified record-builder suffix resolves a nominal type nested in an imported module" {
    try expectNoErrors(&.{
        .{ .path = "Gui.roc", .source = nested_builder_gui },
        .{ .path = "App.roc", .source =
        \\import Gui
        \\
        \\App :: [].{
        \\    built = { first: Gui.Builder.one, second: Gui.Builder.two, third: Gui.Builder.three }.Gui.Builder
        \\}
        \\
        },
    }, "App.roc");
}

test "issue 11465: qualified record-builder suffix ignores an unrelated unqualified type of the same name" {
    // `Builder.map2` here only accepts `Builder.Builder` values, so binding it
    // instead of `Gui.Builder.map2` is a type mismatch.
    try expectNoErrors(&.{
        .{ .path = "Builder.roc", .source = builder_type_module },
        .{ .path = "Gui.roc", .source = nested_builder_gui },
        .{ .path = "App.roc", .source =
        \\import Builder
        \\import Gui
        \\
        \\App :: [].{
        \\    built = { first: Gui.Builder.one, second: Gui.Builder.two, third: Gui.Builder.three }.Gui.Builder
        \\    unqualified = { first: Builder.one, second: Builder.two, third: Builder.three }.Builder
        \\}
        \\
        },
    }, "App.roc");
}

test "issue 11465: record-builder suffix may qualify a type module's type with its module" {
    try expectNoErrors(&.{
        .{ .path = "Builder.roc", .source = builder_type_module },
        .{ .path = "App.roc", .source =
        \\import Builder
        \\
        \\App :: [].{
        \\    built = { first: Builder.one, second: Builder.two, third: Builder.three }.Builder.Builder
        \\}
        \\
        },
    }, "App.roc");
}

test "issue 11465: record-builder suffix resolves a type imported under another name" {
    try expectNoErrors(&.{
        .{ .path = "Builder.roc", .source = builder_type_module },
        .{ .path = "App.roc", .source =
        \\import Builder exposing [Builder as B]
        \\
        \\App :: [].{
        \\    built = { first: Builder.one, second: Builder.two, third: Builder.three }.B
        \\}
        \\
        },
    }, "App.roc");
}

test "issue 11465: qualified record-builder suffix without map2 names the full type path" {
    const gpa = std.testing.allocator;
    const rendered = try renderedErrors(gpa, &.{
        .{ .path = "Gui.roc", .source =
        \\Gui :: [].{
        \\    Plain := [Plain(U64)].{
        \\        one : Plain
        \\        one = Plain(1)
        \\    }
        \\}
        \\
        },
        .{ .path = "App.roc", .source =
        \\import Gui
        \\
        \\App :: [].{
        \\    built = { first: Gui.Plain.one, second: Gui.Plain.one }.Gui.Plain
        \\}
        \\
        },
    }, "App.roc");
    defer gpa.free(rendered);

    try std.testing.expect(std.mem.startsWith(u8, rendered, "**Record Builder Not Supported**"));
    try std.testing.expect(std.mem.find(u8, rendered, "The type `Gui.Plain` is used in a record builder expression") != null);
}
