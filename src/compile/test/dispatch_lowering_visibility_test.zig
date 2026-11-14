//! Regression tests for https://github.com/roc-lang/roc/pull/12925.
//!
//! `Page` reaches `View.shown` only through a method call, never by naming
//! it, so `View` must reach the app's lowering visibility through `Page`'s
//! public API dependencies. Without it, lowering the stored list in
//! `View.shown` finds no const store for `View`: an invariant in a Debug
//! compiler, a segfault in a release one.

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
    \\import Page
    \\
    \\main! = || {
    \\    dbg Page.greet(1)
    \\}
    \\
;

const view_source =
    \\View := { n : U64, m : U64 }.{
    \\    shown : View -> List(Str)
    \\    shown = |_view| [greeting]
    \\}
    \\
    \\greeting : Str
    \\greeting = "hi"
    \\
;

const ExpectViewVisibleError = std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError ||
    compile_build.InitError ||
    compile_build.BuildRootError ||
    error{ TestExpectedEqual, ViewNotLoweringVisible };

fn expectViewVisible(page_source: []const u8) ExpectViewVisibleError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{ .sub_path = "platform.roc", .data = platform_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "main.roc", .data = app_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "View.roc", .data = view_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Page.roc", .data = page_source });

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
    const artifacts = try build_env.collectVisibleArtifacts(gpa, root_artifact);
    defer gpa.free(artifacts);
    for (artifacts) |artifact| {
        const name = artifact.canonical_names.moduleNameText(artifact.module_identity.display_module_name);
        if (std.mem.eql(u8, name, "View")) return;
    }
    return error.ViewNotLoweringVisible;
}

test "method call in an exported procedure makes its target's module lowering-visible" {
    try expectViewVisible(
        \\import View
        \\
        \\Page :: [].{
        \\    greet : U64 -> List(Str)
        \\    greet = |n| {
        \\        view : View
        \\        view = { n, m: 0 }
        \\        view.shown()
        \\    }
        \\}
        \\
    );
}

test "method call in a private helper makes its target's module lowering-visible" {
    try expectViewVisible(
        \\import View
        \\
        \\Page :: [].{
        \\    greet : U64 -> List(Str)
        \\    greet = |n| helper(n)
        \\}
        \\
        \\helper : U64 -> List(Str)
        \\helper = |n| {
        \\    view : View
        \\    view = { n, m: 0 }
        \\    view.shown()
        \\}
        \\
    );
}

test "method call in another exported method makes its target's module lowering-visible" {
    try expectViewVisible(
        \\import View
        \\
        \\Page := { n : U64 }.{
        \\    greet : U64 -> List(Str)
        \\    greet = |n| {
        \\        page : Page
        \\        page = { n: n }
        \\        page.inner()
        \\    }
        \\
        \\    inner : Page -> List(Str)
        \\    inner = |page| {
        \\        view : View
        \\        view = { n: page.n, m: 0 }
        \\        view.shown()
        \\    }
        \\}
        \\
    );
}

test "method call in a private type's method makes its target's module lowering-visible" {
    try expectViewVisible(
        \\import View
        \\
        \\Page :: [].{
        \\    greet : U64 -> List(Str)
        \\    greet = |n| {
        \\        helper : Helper
        \\        helper = { n: n }
        \\        helper.run()
        \\    }
        \\}
        \\
        \\Helper := { n : U64 }.{
        \\    run : Helper -> List(Str)
        \\    run = |helper| {
        \\        view : View
        \\        view = { n: helper.n, m: 0 }
        \\        view.shown()
        \\    }
        \\}
        \\
    );
}

test "method call in a local lambda makes its target's module lowering-visible" {
    try expectViewVisible(
        \\import View
        \\
        \\Page :: [].{
        \\    greet : U64 -> List(Str)
        \\    greet = |n| {
        \\        f = |x| {
        \\            view : View
        \\            view = { n: x, m: 0 }
        \\            view.shown()
        \\        }
        \\        f(n)
        \\    }
        \\}
        \\
    );
}

test "method a generic helper's where clause selects makes its module lowering-visible" {
    try expectViewVisible(
        \\import View
        \\
        \\Page :: [].{
        \\    greet : U64 -> List(Str)
        \\    greet = |n| {
        \\        view : View
        \\        view = { n, m: 0 }
        \\        show_any(view)
        \\    }
        \\}
        \\
        \\show_any : a -> List(Str) where [a.shown : a -> List(Str)]
        \\show_any = |x| x.shown()
        \\
    );
}
