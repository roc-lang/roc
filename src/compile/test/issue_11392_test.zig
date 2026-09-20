//! Regression tests for issue #11392: `roc build` panicked demanding the
//! executable root's checked artifact after import resolution rejected one of
//! that root's imports.
//! repro for https://github.com/roc-lang/roc/issues/11392
//!
//! A rejected import—a package module that is not public, a relative import
//! that escapes the package source root, an import whose source path is not
//! the one its logical name selects—is a user diagnostic. Per design.md's
//! "Module Completion Boundary", a user diagnostic never selects a module
//! `Failure` and never propagates dependency failure: the importing module
//! still produces its complete `ModuleEnv` and CheckedModule, uses of the
//! rejected import become checked-error data, and every independent
//! definition, import, and compile-time root stays available.
//!
//! These tests drive finalization the way a build does, so a regression that
//! turns a rejected import back into a failed module reappears here as the
//! missing-artifact panic rather than in a released compiler.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoordinatorError = @import("../coordinator.zig").CoordinatorError;
const CoreCtx = @import("ctx").CoreCtx;

const File = struct {
    name: []const u8,
    data: []const u8,
};

const StageError = std.Io.Dir.CreateDirPathError || std.Io.Dir.WriteFileError;

const HarnessError = StageError ||
    std.mem.Allocator.Error ||
    std.Io.Dir.RealPathFileAllocError ||
    Coordinator.AppDiscoveryError ||
    eval.BuiltinModules.InitError ||
    std.Thread.SpawnError ||
    CoordinatorError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

/// A platform exposing exactly one module, so `pf.Hidden` below names a real
/// source file of the platform package that the package does not publish.
const platform_files = [_]File{
    .{
        .name = "pfroot/main.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try(_, [Exit(I8), ..]) }
        \\    exposes [Echo]
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\    hosted { "roc_echo_line": Echo.line! }
        \\
        \\import Echo
        \\
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |args|
        \\    match main!(args) {
        \\        Ok(_) => 0
        \\        Err(Exit(code)) => code
        \\        Err(_) => 1
        \\    }
        ,
    },
    .{
        .name = "pfroot/Echo.roc",
        .data =
        \\Echo := [].{
        \\    line! : Str => {}
        \\}
        ,
    },
    .{
        .name = "pfroot/Hidden.roc",
        .data =
        \\Hidden := [].{
        \\    secret : Str
        \\    secret = "hidden"
        \\}
        ,
    },
};

/// What finalization produced for one fixture.
const Outcome = struct {
    gpa: std.mem.Allocator,
    /// True when the app root published a complete checked artifact, which is
    /// exactly what the panic in this issue reported as missing.
    app_root_published: bool,
    report_titles: std.ArrayList([]const u8),

    fn deinit(self: *Outcome) void {
        for (self.report_titles.items) |title| self.gpa.free(title);
        self.report_titles.deinit(self.gpa);
    }

    fn hasTitle(self: *const Outcome, want: []const u8) bool {
        for (self.report_titles.items) |title| {
            if (std.mem.eql(u8, title, want)) return true;
        }
        return false;
    }

    fn countTitle(self: *const Outcome, want: []const u8) usize {
        var count: usize = 0;
        for (self.report_titles.items) |title| {
            if (std.mem.eql(u8, title, want)) count += 1;
        }
        return count;
    }

    fn printTitles(self: *const Outcome) void {
        for (self.report_titles.items) |title| std.debug.print("  - {s}\n", .{title});
    }
};

/// Whether the app package's root module published a checked artifact. This is
/// the exact fact `Coordinator.rootCheckedArtifact` panicked over when the app
/// module had been completed with failure instead of checked.
fn appRootPublished(coord: *Coordinator) bool {
    const app_package_name = coord.app_package_name orelse return false;
    const pkg = coord.packages.get(app_package_name) orelse return false;
    const root_id = pkg.root_module_id orelse return false;
    const mod = pkg.getModule(root_id) orelse return false;
    return mod.checkedArtifact() != null;
}

/// Stage `files` alongside the shared platform, compile `entry_rel` as an app,
/// and finalize its executable artifacts the way `roc build` does.
fn compileApp(gpa: std.mem.Allocator, files: []const File, entry_rel: []const u8) HarnessError!Outcome {
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    for (&platform_files) |file| {
        if (std.fs.path.dirname(file.name)) |sub_dir| try tmp_dir.dir.createDirPath(io, sub_dir);
        try tmp_dir.dir.writeFile(io, .{ .sub_path = file.name, .data = file.data });
    }
    for (files) |file| {
        if (std.fs.path.dirname(file.name)) |sub_dir| try tmp_dir.dir.createDirPath(io, sub_dir);
        try tmp_dir.dir.writeFile(io, .{ .sub_path = file.name, .data = file.data });
    }

    const app_path = try tmp_dir.dir.realPathFileAlloc(io, entry_rel, gpa);
    defer gpa.free(app_path);

    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var builtin_modules = try eval.BuiltinModules.init(gpa);
    defer builtin_modules.deinit();

    var coord = try Coordinator.init(
        gpa,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.default(gpa, arena, io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    // `.executable_artifacts` is the mode a build uses. It reaches the app
    // root's checked artifact, which a failed app module would not have.
    try coord.finishCheckedProgram(.executable_artifacts);

    var report_titles = std.ArrayList([]const u8).empty;
    errdefer {
        for (report_titles.items) |title| gpa.free(title);
        report_titles.deinit(gpa);
    }

    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        try report_titles.append(gpa, try gpa.dupe(u8, entry.report.title));
    }

    return .{
        .gpa = gpa,
        .app_root_published = appRootPublished(&coord),
        .report_titles = report_titles,
    };
}

test "issue 11392: a private package import leaves the app root published" {
    const gpa = std.testing.allocator;
    var outcome = try compileApp(gpa, &.{
        .{
            .name = "app.roc",
            .data =
            \\app [main!] { pf: platform "./pfroot/main.roc" }
            \\
            \\import pf.Echo
            \\import pf.Hidden
            \\
            \\never_reached! : {} => {}
            \\never_reached! = |{}| Echo.line!(Hidden.secret)
            \\
            \\main! = |_args| {
            \\    Echo.line!("independent work still runs")
            \\    Ok({})
            \\}
            ,
        },
    }, "app.roc");
    defer outcome.deinit();

    if (!outcome.app_root_published) {
        std.debug.print("app root produced no checked artifact; reports:\n", .{});
        outcome.printTitles();
    }
    try std.testing.expect(outcome.app_root_published);

    // The rejection is reported exactly once, at the boundary that owns it.
    try std.testing.expectEqual(@as(usize, 1), outcome.countTitle("Package Module Is Private"));

    // The use of the rejected import is checked-error data naming it, and the
    // sibling import it shares a call with still resolved.
    try std.testing.expect(outcome.hasTitle("Does Not Exist"));
    try std.testing.expect(!outcome.hasTitle("Module Not Imported"));
    try std.testing.expect(!outcome.hasTitle("Undeclared Type"));
}

test "issue 11392: a relative import escaping the package root leaves the app root published" {
    const gpa = std.testing.allocator;
    var outcome = try compileApp(gpa, &.{
        .{
            .name = "app.roc",
            .data =
            \\app [main!] { pf: platform "./pfroot/main.roc" }
            \\
            \\import pf.Echo
            \\import ../Escape
            \\
            \\never_reached! : {} => {}
            \\never_reached! = |{}| Echo.line!(Escape.secret)
            \\
            \\main! = |_args| {
            \\    Echo.line!("independent work still runs")
            \\    Ok({})
            \\}
            ,
        },
    }, "app.roc");
    defer outcome.deinit();

    if (!outcome.app_root_published) {
        std.debug.print("app root produced no checked artifact; reports:\n", .{});
        outcome.printTitles();
    }
    try std.testing.expect(outcome.app_root_published);
    try std.testing.expectEqual(@as(usize, 1), outcome.countTitle("Import Escapes Package Root"));
    try std.testing.expect(outcome.hasTitle("Does Not Exist"));
}

test "issue 11392: a rejected import does not reject its own exposed items' type bindings" {
    const gpa = std.testing.allocator;
    var outcome = try compileApp(gpa, &.{
        .{
            .name = "app.roc",
            .data =
            \\app [main!] { pf: platform "./pfroot/main.roc" }
            \\
            \\import pf.Echo
            \\import pf.Hidden exposing [Hidden]
            \\
            \\never_reached! : Hidden => {}
            \\never_reached! = |_held| {}
            \\
            \\main! = |_args| {
            \\    Echo.line!("independent work still runs")
            \\    Ok({})
            \\}
            ,
        },
    }, "app.roc");
    defer outcome.deinit();

    if (!outcome.app_root_published) {
        std.debug.print("app root produced no checked artifact; reports:\n", .{});
        outcome.printTitles();
    }
    try std.testing.expect(outcome.app_root_published);
    try std.testing.expectEqual(@as(usize, 1), outcome.countTitle("Package Module Is Private"));

    // The exposed type binds to the rejected import, so the annotation reports
    // a type from a missing module rather than an undeclared name.
    try std.testing.expect(outcome.hasTitle("Module Not Found"));
    try std.testing.expect(!outcome.hasTitle("Undeclared Type"));
}
