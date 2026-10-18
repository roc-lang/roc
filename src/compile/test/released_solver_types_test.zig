//! A compilation that releases each module's solver type store once checking
//! finishes still publishes complete checked cache entries: the `ModuleEnv`
//! part is staged while the store is live, and the entry published after
//! evaluation must load on the next compilation with the same results.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const roc_target = @import("roc_target");

const CacheManager = @import("../cache_manager.zig").CacheManager;
const Coordinator = @import("../coordinator.zig").Coordinator;
const CoordinatorError = @import("../coordinator.zig").CoordinatorError;
const CoreCtx = @import("ctx").CoreCtx;

const CompileResult = struct {
    has_user_errors: bool,
    cache_hits: u32,
    released_modules: usize,
    store_failures: u64,
};

const CompileError = eval.BuiltinModules.InitError || std.Thread.SpawnError || Coordinator.AppDiscoveryError || CoordinatorError;

fn compileReleasingSolverTypes(
    gpa: std.mem.Allocator,
    cache_dir: []const u8,
    app_path: []const u8,
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
        build_options.compiler_compatibility_id,
        &cache_manager,
        roc_ctx,
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;
    coord.setReleaseCheckedSolverTypes(true);

    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try coord.finishCheckedProgram(.none);

    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        if (entry.report.severity.isError()) std.debug.print(
            "compile reported {s}: {s}\n",
            .{ @tagName(entry.report.severity), entry.report.title },
        );
    }

    var released_modules: usize = 0;
    var pkg_it = coord.packages.iterator();
    while (pkg_it.next()) |entry| {
        for (entry.value_ptr.*.modules.items) |mod| {
            if (mod.solver_types_released) released_modules += 1;
        }
    }

    return .{
        .has_user_errors = coord.hasUserErrors(),
        .cache_hits = coord.getBuildStats().cache_hits,
        .released_modules = released_modules,
        .store_failures = cache_manager.getStats().store_failures,
    };
}

fn countStagingFiles(gpa: std.mem.Allocator, io: std.Io, dir: std.Io.Dir) !usize {
    var walker = try dir.walk(gpa);
    defer walker.deinit();
    var count: usize = 0;
    while (try walker.next(io)) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.basename, ".tmp")) count += 1;
    }
    return count;
}

test "checked cache entries published after releasing solver types load on the next compilation" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(io, "cache");
    try tmp_dir.dir.createDirPath(io, "app/.roc_test_platform");

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/main.roc",
        .data =
        \\app [main!] { pf: platform "./.roc_test_platform/main.roc" }
        \\
        \\import Greeting exposing [message]
        \\import pf.Stdout
        \\
        \\main! = |_args| {
        \\    Stdout.line!(message)
        \\    Ok({})
        \\}
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/Greeting.roc",
        .data =
        \\Greeting := {}.{
        \\    message : Str
        \\    message = Str.concat("Hello", ", World!")
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

    const first = try compileReleasingSolverTypes(gpa, cache_dir, app_path);
    try std.testing.expect(!first.has_user_errors);
    // The compilation must actually release solver types, or the published
    // entries prove nothing about staging.
    try std.testing.expect(first.released_modules > 0);
    try std.testing.expectEqual(@as(u64, 0), first.store_failures);

    var cache = try tmp_dir.dir.openDir(io, "cache", .{ .iterate = true });
    defer cache.close(io);
    try std.testing.expectEqual(@as(usize, 0), try countStagingFiles(gpa, io, cache));

    const second = try compileReleasingSolverTypes(gpa, cache_dir, app_path);
    if (second.cache_hits == 0) {
        std.debug.print("second compile hit no cached modules\n", .{});
        return error.CheckedModuleCacheNotExercised;
    }
    try std.testing.expect(!second.has_user_errors);
    try std.testing.expectEqual(@as(u64, 0), second.store_failures);
    try std.testing.expectEqual(@as(usize, 0), try countStagingFiles(gpa, io, cache));
}
