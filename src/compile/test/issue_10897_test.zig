//! Regression test for issue #10897.
//!
//! repro for https://github.com/roc-lang/roc/issues/10897
//!
//! `import Folder/Foo exposing [log!]` must resolve the same way whether
//! `Folder/Foo` was compiled from source or restored from the checked-module
//! cache. Expected: both compiles report no user errors. Actual: the second
//! compile (the one that hits the cache) reports `Value Not Exposed` for `log!`
//! plus `Name Not In Scope` at the call site.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const roc_target = @import("roc_target");

const CacheManager = @import("../cache_manager.zig").CacheManager;
const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

const CompileResult = struct {
    has_user_errors: bool,
    cache_hits: u32,
};

const CompileError = eval.BuiltinModules.InitError || std.Thread.SpawnError || Coordinator.AppDiscoveryError || error{
    UnsupportedBuiltinAnnotationOnly,
    BuiltinLowLevelAnnotationMustBeFunction,
    LowLevelOperationsNotFound,
};

fn compileWithCache(
    gpa: std.mem.Allocator,
    cache_dir: []const u8,
    app_path: []const u8,
    label: []const u8,
) CompileError!CompileResult {
    const roc_ctx = CoreCtx.os(gpa, gpa, std.testing.io);

    var cache_manager = CacheManager.init(gpa, .{
        .enabled = true,
        .cache_dir = cache_dir,
    }, roc_ctx);

    var builtin_modules = try eval.BuiltinModules.init(gpa);
    defer builtin_modules.deinit();

    var coord = try Coordinator.init(
        gpa,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        &cache_manager,
        roc_ctx,
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();

    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        switch (entry.report.severity) {
            .warning => {},
            .runtime_error, .fatal => std.debug.print(
                "{s} compile reported {s}: {s}\n",
                .{ label, @tagName(entry.report.severity), entry.report.title },
            ),
        }
    }

    return .{
        .has_user_errors = coord.hasUserErrors(),
        .cache_hits = coord.getBuildStats().cache_hits,
    };
}

test "issue 10897: a value exposed from a directory-qualified module still resolves on a cache hit" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(io, "cache");
    try tmp_dir.dir.createDirPath(io, "app/.roc_test_platform");
    try tmp_dir.dir.createDirPath(io, "app/Folder");

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/main.roc",
        .data =
        \\app [main!] { pf: platform "./.roc_test_platform/main.roc" }
        \\
        \\import Folder/Foo exposing [log!]
        \\
        \\main! = |_args| {
        \\    log!("Hello World!")
        \\    Ok({})
        \\}
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/Folder/Foo.roc",
        .data =
        \\import pf.Stdout
        \\
        \\Foo := {}.{
        \\    log! = |message| Stdout.line!(message)
        \\}
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/.roc_test_platform/main.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
        \\    exposes [Stdout]
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\    hosted {
        \\        "roc_stdout_line": Stdout.line!,
        \\    }
        \\
        \\import Stdout
        \\
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |args|
        \\    match main!(args) {
        \\        Ok({}) => 0
        \\        Err(Exit(code)) => code
        \\        Err(_) => 1
        \\    }
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/.roc_test_platform/Stdout.roc",
        .data =
        \\Stdout := [].{
        \\    line! : Str => {}
        \\}
        ,
    });

    const cache_dir = try tmp_dir.dir.realPathFileAlloc(io, "cache", gpa);
    defer gpa.free(cache_dir);
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app/main.roc", gpa);
    defer gpa.free(app_path);

    const first = try compileWithCache(gpa, cache_dir, app_path, "first");
    try std.testing.expect(!first.has_user_errors);

    const second = try compileWithCache(gpa, cache_dir, app_path, "second");
    // The second compile must actually exercise the cache, otherwise it would
    // just repeat the first compile and prove nothing.
    if (second.cache_hits == 0) {
        std.debug.print("second compile hit no cached modules\n", .{});
        return error.CheckedModuleCacheNotExercised;
    }
    try std.testing.expect(!second.has_user_errors);
}
