//! Regression coverage for checking a module below its package root (#11302).
const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const nested_module_source =
    \\import ./Helper
    \\
    \\Widget :: [].{
    \\    default : Str
    \\    default = Helper.suffix()
    \\
    \\    message : Str -> Str
    \\    message = |value| value
    \\}
;

const sibling_module_source =
    \\Helper :: [].{
    \\    suffix : () -> Str
    \\    suffix = || "directory-qualified local module"
    \\}
;

test "issue 11302: checking a module below the package root does not alias the root's import of it" {
    try expectNestedModuleChecksCleanly(
        \\package [Widget] {}
        \\
        \\import Src/Widget as Widget
    );
}

test "issue 11302: a module below the package root resolves its own relative imports from its directory" {
    try expectNestedModuleChecksCleanly("package [] {}");
}

const NestedModuleCheckError = compile_build.InitError || std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError || std.Io.Dir.CreateDirPathError ||
    std.mem.Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult };

fn expectNestedModuleChecksCleanly(package_root_source: []const u8) NestedModuleCheckError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDirPath(io, "Src");
    try tmp.dir.writeFile(io, .{ .sub_path = "main.roc", .data = package_root_source });
    try tmp.dir.writeFile(io, .{ .sub_path = "Src/Widget.roc", .data = nested_module_source });
    try tmp.dir.writeFile(io, .{ .sub_path = "Src/Helper.roc", .data = sibling_module_source });

    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const nested_module = try tmp.dir.realPathFileAlloc(io, "Src/Widget.roc", gpa);
    defer gpa.free(nested_module);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    var build_failed = false;
    build_env.buildResolvingMain(nested_module, null) catch {
        build_failed = true;
    };

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var errors: usize = 0;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            switch (report.severity) {
                .runtime_error, .fatal => {
                    errors += 1;
                    std.debug.print("unexpected {s} in {s}: {s}\n", .{
                        report.severity.toString(), module_reports.abs_path, report.title,
                    });
                },
                .warning => {},
            }
        }
    }

    try std.testing.expectEqual(@as(usize, 0), errors);
    try std.testing.expect(!build_failed);
}

const File = struct { path: []const u8, data: []const u8 };

const Outcome = struct {
    build_error: ?compile_build.BuildWithMainError,
    error_titles: std.ArrayList([]const u8),

    fn deinit(self: *Outcome, gpa: std.mem.Allocator) void {
        for (self.error_titles.items) |title| gpa.free(title);
        self.error_titles.deinit(gpa);
    }
};

const HarnessError = compile_build.InitError || std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError || std.Io.Dir.CreateDirPathError ||
    std.mem.Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult, TestExpectedEqualStrings };

/// A module the entry's package graph contains, identified by the file it was
/// read from relative to the temporary directory.
const ExpectedModule = struct {
    file: []const u8,
    logical_path: []const u8,
    display_name: []const u8,
};

/// Check `entry` with `main` supplying its package, then assert that each
/// expected source file is exactly one module with the given identity.
fn checkEntry(
    gpa: std.mem.Allocator,
    files: []const File,
    entry: []const u8,
    main: []const u8,
    expected_modules: []const ExpectedModule,
) HarnessError!Outcome {
    const io = std.testing.io;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    for (files) |file| {
        if (std.fs.path.dirname(file.path)) |dir| try tmp.dir.createDirPath(io, dir);
        try tmp.dir.writeFile(io, .{ .sub_path = file.path, .data = file.data });
    }

    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const entry_path = try tmp.dir.realPathFileAlloc(io, entry, gpa);
    defer gpa.free(entry_path);
    const main_path = try tmp.dir.realPathFileAlloc(io, main, gpa);
    defer gpa.free(main_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    var outcome = Outcome{ .build_error = null, .error_titles = .empty };
    errdefer outcome.deinit(gpa);

    build_env.buildResolvingMain(entry_path, main_path) catch |err| {
        outcome.build_error = err;
    };

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            switch (report.severity) {
                .runtime_error, .fatal => try outcome.error_titles.append(gpa, try gpa.dupe(u8, report.title)),
                .warning => {},
            }
        }
    }

    for (expected_modules) |expected| {
        const expected_path = try tmp.dir.realPathFileAlloc(io, expected.file, gpa);
        defer gpa.free(expected_path);

        const coord = build_env.coordinator.?;
        const pkg = coord.getPackage(build_env.discovered_pkg_name.?).?;
        var matches: usize = 0;
        for (pkg.modules.items) |*module| {
            if (!std.mem.eql(u8, module.path, expected_path)) continue;
            matches += 1;
            try std.testing.expectEqualStrings(expected.logical_path, module.name);
            try std.testing.expectEqualStrings(expected.display_name, module.moduleEnv().?.module_name);
        }
        try std.testing.expectEqual(@as(usize, 1), matches);
    }

    return outcome;
}

test "issue 11302: the checked module and the root's import of it are one module named by its package path" {
    const gpa = std.testing.allocator;
    var outcome = try checkEntry(gpa, &.{
        .{ .path = "main.roc", .data = "package [Widget] {}\n\nimport Src/Widget as Widget" },
        .{ .path = "Src/Widget.roc", .data = nested_module_source },
        .{ .path = "Src/Helper.roc", .data = sibling_module_source },
    }, "Src/Widget.roc", "main.roc", &.{
        .{ .file = "Src/Widget.roc", .logical_path = "Src/Widget", .display_name = "Widget" },
        .{ .file = "Src/Helper.roc", .logical_path = "Src/Helper", .display_name = "Helper" },
    });
    defer outcome.deinit(gpa);

    try std.testing.expectEqual(@as(usize, 0), outcome.error_titles.items.len);
    try std.testing.expect(outcome.build_error == null);
}

test "issue 11302: a checked module keeps a distinct identity from a same-named module at the package root" {
    const gpa = std.testing.allocator;
    var outcome = try checkEntry(gpa, &.{
        .{ .path = "main.roc", .data = "package [Widget] {}" },
        .{ .path = "Widget.roc", .data = "Widget :: [].{\n    root : Str\n    root = \"root\"\n}" },
        .{ .path = "Src/Widget.roc", .data = nested_module_source },
        .{ .path = "Src/Helper.roc", .data = sibling_module_source },
    }, "Src/Widget.roc", "main.roc", &.{
        .{ .file = "Widget.roc", .logical_path = "Widget", .display_name = "Widget" },
        .{ .file = "Src/Widget.roc", .logical_path = "Src/Widget", .display_name = "Widget" },
    });
    defer outcome.deinit(gpa);

    try std.testing.expectEqual(@as(usize, 0), outcome.error_titles.items.len);
    try std.testing.expect(outcome.build_error == null);
}

test "issue 11302: a nested main.roc checked against the package's main.roc is its own module" {
    const gpa = std.testing.allocator;
    var outcome = try checkEntry(gpa, &.{
        .{ .path = "main.roc", .data = "package [] {}" },
        .{ .path = "Src/main.roc", .data = "import ./Helper\n\nvalue : Str\nvalue = Helper.suffix()" },
        .{ .path = "Src/Helper.roc", .data = sibling_module_source },
    }, "Src/main.roc", "main.roc", &.{
        .{ .file = "main.roc", .logical_path = "main", .display_name = "main" },
        .{ .file = "Src/main.roc", .logical_path = "Src/main", .display_name = "main" },
        .{ .file = "Src/Helper.roc", .logical_path = "Src/Helper", .display_name = "Helper" },
    });
    defer outcome.deinit(gpa);

    try std.testing.expectEqual(@as(usize, 0), outcome.error_titles.items.len);
    try std.testing.expect(outcome.build_error == null);
}

test "issue 11302: a module outside the explicit main's package is rejected with a report" {
    const gpa = std.testing.allocator;
    var outcome = try checkEntry(gpa, &.{
        .{ .path = "pkg/main.roc", .data = "package [] {}" },
        .{ .path = "pkg-other/Widget.roc", .data = nested_module_source },
        .{ .path = "pkg-other/Helper.roc", .data = sibling_module_source },
    }, "pkg-other/Widget.roc", "pkg/main.roc", &.{});
    defer outcome.deinit(gpa);

    try std.testing.expectEqual(@as(?compile_build.BuildWithMainError, error.PathOutsideWorkspace), outcome.build_error);
    try std.testing.expectEqual(@as(usize, 1), outcome.error_titles.items.len);
    try std.testing.expectEqualStrings("Module Outside Package", outcome.error_titles.items[0]);
}
