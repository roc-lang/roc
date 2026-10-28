//! A compilation that releases each module's solver type store once checking
//! finishes still publishes complete checked cache entries: the `ModuleEnv`
//! part is staged while the store is live, and the entry published after
//! evaluation must load on the next compilation with the same results.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const roc_target = @import("roc_target");

const artifacts = @import("../artifact_store.zig");
const CacheManager = @import("../cache_manager.zig").CacheManager;
const Coordinator = @import("../coordinator.zig").Coordinator;
const CoordinatorError = @import("../coordinator.zig").CoordinatorError;
const ModuleState = @import("../coordinator.zig").ModuleState;
const reporting = @import("reporting");
const CoreCtx = @import("ctx").CoreCtx;

const CompileResult = struct {
    has_user_errors: bool,
    cache_hits: u32,
    canonicalized_cache_hits: u32,
    modules_total: u32,
    released_modules: usize,
    store_failures: u64,
    reports: []u8,
    greeting_key: [32]u8,
    greeting_cached: bool,
    greeting_checked: bool,
    greeting_released: bool,
    greeting_published: bool,
    greeting_staged: usize,
    greeting_awaited_evaluation: bool,
    greeting_value_completed: bool,
    warning_count: usize,

    fn deinit(self: CompileResult, gpa: std.mem.Allocator) void {
        gpa.free(self.reports);
    }
};

const StagingObserver = struct {
    filesystem: CoreCtx,
    store: ?*artifacts.Store = null,
    greeting: ?*ModuleState = null,
    key: artifacts.catalog.Key = @splat(0),
    attempts: usize = 0,
    fail: bool,

    fn write(raw: ?*anyopaque, _: std.Io, path: []const u8, stream: CoreCtx.FileStream) CoreCtx.WriteError!void {
        const self: *@This() = @ptrCast(@alignCast(raw.?));
        if (self.greeting) |mod| {
            // The environment is streamed before its solver types are released;
            // the final entry is streamed afterward. Identify its reservation
            // through the catalog, independently of physical slot assignment.
            if (!mod.solver_types_released and
                (registeredPending(self.store.?, path, self.key) catch return error.IoError))
            {
                self.attempts += 1;
                if (self.fail) {
                    // Leave a partial file, just as an interrupted disk write can.
                    try self.filesystem.writeFile(path, "partial");
                    return error.IoError;
                }
            }
        }
        try self.filesystem.writeFileStream(path, stream);
    }
};

const CompileError = artifacts.Error || eval.BuiltinModules.InitError || std.Thread.SpawnError || Coordinator.AppDiscoveryError || CoordinatorError || error{
    MissingGreetingModule,
    NoHomeDirectory,
    WriteFailed,
};

fn compileReleasingSolverTypes(
    gpa: std.mem.Allocator,
    cache_dir: ?[]const u8,
    app_path: []const u8,
    fail_staging: bool,
) CompileError!CompileResult {
    var roc_ctx = CoreCtx.os(gpa, gpa, std.testing.io);
    var observer = StagingObserver{ .filesystem = roc_ctx, .fail = fail_staging };
    roc_ctx.ctx = &observer;
    roc_ctx.vtable.writeFileStream = StagingObserver.write;

    var cache_manager = CacheManager.init(gpa, .{
        .enabled = cache_dir != null,
        .cache_dir = cache_dir orelse "",
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
        if (cache_dir != null) &cache_manager else null,
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
    var greeting: ?*ModuleState = null;
    var packages = coord.packages.iterator();
    while (packages.next()) |entry| {
        for (entry.value_ptr.*.modules.items) |*mod| {
            if (std.mem.eql(u8, mod.name, "Greeting")) greeting = mod;
        }
    }
    const mod = greeting orelse return error.MissingGreetingModule;
    const key = mod.checkedArtifact().?.key.bytes;
    const entries_dir = try cache_manager.config.getCheckedArtifactCacheDir(gpa);
    defer gpa.free(entries_dir);
    var store = try cache_manager.openStore(gpa);
    defer store.deinit();
    const identity = CacheManager.identity(.checked, key, entries_dir);
    observer.store = &store;
    observer.greeting = mod;
    observer.key = identity.indexedKey();
    const awaited_evaluation = mod.pending_evaluation != null;
    try coord.finishCheckedProgram(.none);

    var rendered = std.Io.Writer.Allocating.init(gpa);
    defer rendered.deinit();
    var warning_count: usize = 0;
    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        if (entry.report.severity == .warning and std.mem.eql(u8, entry.report.title, "Builtin Type Shadowed")) warning_count += 1;
        try reporting.renderReport(entry.report, &rendered.writer, .markdown);
    }

    var released_modules: usize = 0;
    var pkg_it = coord.packages.iterator();
    while (pkg_it.next()) |entry| {
        for (entry.value_ptr.*.modules.items) |other_mod| {
            if (other_mod.solver_types_released) released_modules += 1;
        }
    }

    const artifact = mod.checkedArtifact().?;
    var value_completed = false;
    for (artifact.compile_time_roots.roots) |root| {
        if (root.payload == .const_node) {
            const value = artifact.const_store.get(root.payload.const_node);
            if (value == .str and std.mem.eql(u8, artifact.const_store.strBytes(value.str), "Hello, World!")) value_completed = true;
        }
    }
    const published = if (cache_dir != null) try store.acquire(identity, store.now()) else null;
    defer if (published) |value| {
        var acquired = value;
        acquired.deinit();
    };
    return .{
        .reports = try gpa.dupe(u8, rendered.written()),
        .greeting_key = key,
        .greeting_cached = artifact.serialized_backing != null,
        .greeting_checked = mod.type_check_ns != 0,
        .greeting_released = mod.solver_types_released,
        .greeting_published = published != null,
        .greeting_staged = observer.attempts,
        .greeting_awaited_evaluation = awaited_evaluation,
        .greeting_value_completed = value_completed,
        .warning_count = warning_count,
        .has_user_errors = coord.hasUserErrors(),
        .cache_hits = coord.getBuildStats().cache_hits,
        .canonicalized_cache_hits = coord.getBuildStats().canonicalized_cache_hits,
        .modules_total = coord.getBuildStats().modules_total,
        .released_modules = released_modules,
        .store_failures = cache_manager.getStats().store_failures,
    };
}

/// Test-only inventory: every unfinished payload must have a registered owner.
fn registeredPending(store: *const artifacts.Store, path: []const u8, key: ?artifacts.catalog.Key) artifacts.Error!bool {
    for (0..artifacts.shard_count) |number| {
        var counters = artifacts.catalog.Counters{};
        var shard = artifacts.Shard.open(store, @intCast(number), false, &counters) catch |err| switch (err) {
            error.Missing => continue,
            else => return err,
        };
        defer shard.close();
        var tx = shard.index.transaction();
        var cursor: artifacts.catalog.Key = @splat(0);
        while (try tx.lowerBound(.owners, cursor)) |entry| {
            const metadata = try artifacts.Metadata.decode(entry.value);
            if (key == null or std.mem.eql(u8, &metadata.key, &key.?)) {
                const pending = try store.payloadPath(@intCast(number), metadata.slot, ".pending");
                defer store.allocator.free(pending);
                if (std.mem.eql(u8, path, pending)) return true;
            }
            cursor = entry.key;
            if (!artifacts.advanceKey(&cursor)) break;
        }
    }
    return false;
}

/// Aborted writes may remain for bounded maintenance, but cannot be unregistered.
fn countUnregisteredStagingFiles(gpa: std.mem.Allocator, io: std.Io, dir: std.Io.Dir) (artifacts.Error || std.Io.Dir.RealPathFileAllocError || std.Io.Dir.SelectiveWalker.Error)!usize {
    const root = try dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(root);
    var store = try artifacts.Store.init(gpa, io, root);
    defer store.deinit();
    var walker = try dir.walk(gpa);
    defer walker.deinit();
    var count: usize = 0;
    while (try walker.next(io)) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.basename, ".pending")) {
            // Payload paths join the store root and the components below it
            // with `/` on every host.
            const below_root = entry.path[artifacts.format_directory.len + 1 ..];
            const path = try std.fmt.allocPrint(gpa, "{s}/{s}", .{ store.root, below_root });
            defer gpa.free(path);
            std.mem.replaceScalar(u8, path[store.root.len..], '\\', '/');
            if (!try registeredPending(&store, path, null)) count += 1;
        }
    }
    return count;
}

fn writeFixture(tmp_dir: *std.testing.TmpDir) (std.Io.Dir.CreateDirPathError || std.Io.Dir.WriteFileError)!void {
    const io = std.testing.io;
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
}

test "checked cache entries published after releasing solver types load on the next compilation" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try writeFixture(&tmp_dir);

    const cache_dir = try tmp_dir.dir.realPathFileAlloc(io, "cache", gpa);
    defer gpa.free(cache_dir);
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app/main.roc", gpa);
    defer gpa.free(app_path);

    const first = try compileReleasingSolverTypes(gpa, cache_dir, app_path, false);
    defer first.deinit(gpa);
    try std.testing.expect(!first.has_user_errors);
    // The compilation must actually release solver types, or the published
    // entries prove nothing about staging.
    try std.testing.expect(first.released_modules > 0);
    try std.testing.expectEqual(@as(u64, 0), first.store_failures);

    var cache = try tmp_dir.dir.openDir(io, "cache", .{ .iterate = true });
    defer cache.close(io);
    try std.testing.expectEqual(@as(usize, 0), try countUnregisteredStagingFiles(gpa, io, cache));

    const second = try compileReleasingSolverTypes(gpa, cache_dir, app_path, false);
    defer second.deinit(gpa);
    if (second.cache_hits == 0) {
        std.debug.print("second compile hit no cached modules\n", .{});
        return error.CheckedModuleCacheNotExercised;
    }
    try std.testing.expect(!second.has_user_errors);
    try std.testing.expectEqual(@as(u64, 0), second.store_failures);
    try std.testing.expectEqual(@as(usize, 0), try countUnregisteredStagingFiles(gpa, io, cache));
}

const warning_source =
    \\Greeting := {}.{
    \\    U64 := {}
    \\    message : Str
    \\    message = Str.concat("Hello", ", World!")
    \\}
;

test "issue 12570: earlier warnings replay once while released solver types are cached" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFixture(&tmp);
    try tmp.dir.writeFile(io, .{ .sub_path = "app/Greeting.roc", .data = warning_source });
    const cache_dir = try tmp.dir.realPathFileAlloc(io, "cache", gpa);
    defer gpa.free(cache_dir);
    const app_path = try tmp.dir.realPathFileAlloc(io, "app/main.roc", gpa);
    defer gpa.free(app_path);

    const cold = try compileReleasingSolverTypes(gpa, cache_dir, app_path, false);
    defer cold.deinit(gpa);
    try std.testing.expect(!cold.has_user_errors);
    try std.testing.expectEqual(@as(usize, 1), cold.warning_count);
    try std.testing.expect(cold.greeting_checked);
    try std.testing.expect(cold.greeting_released);
    try std.testing.expectEqual(@as(usize, 1), cold.greeting_staged);
    try std.testing.expect(cold.greeting_published);
    try std.testing.expect(cold.greeting_value_completed);

    const warm = try compileReleasingSolverTypes(gpa, cache_dir, app_path, false);
    defer warm.deinit(gpa);
    // Every module, including Greeting, must have replayed its earlier reports
    // through the canonicalized cache before the checked artifact was installed.
    try std.testing.expectEqual(warm.modules_total, warm.canonicalized_cache_hits);
    try std.testing.expect(warm.greeting_cached);
    try std.testing.expect(!warm.greeting_checked);
    try std.testing.expect(!warm.greeting_awaited_evaluation);
    try std.testing.expect(warm.greeting_value_completed);
    const uncached = try compileReleasingSolverTypes(gpa, null, app_path, false);
    defer uncached.deinit(gpa);
    for ([_]CompileResult{ warm, uncached }) |run| {
        try std.testing.expectEqual(cold.has_user_errors, run.has_user_errors);
        try std.testing.expectEqual(cold.warning_count, run.warning_count);
        try std.testing.expectEqualStrings(cold.reports, run.reports);
        try std.testing.expectEqualSlices(u8, &cold.greeting_key, &run.greeting_key);
    }

    // Removing the warning changes the identity and cannot replay its report.
    try writeFixture(&tmp);
    const clean = try compileReleasingSolverTypes(gpa, cache_dir, app_path, false);
    defer clean.deinit(gpa);
    try std.testing.expect(!std.mem.eql(u8, &cold.greeting_key, &clean.greeting_key));
    try std.testing.expectEqualStrings("", clean.reports);
    try std.testing.expect(clean.greeting_checked);
    try std.testing.expect(clean.greeting_published);
    const clean_warm = try compileReleasingSolverTypes(gpa, cache_dir, app_path, false);
    defer clean_warm.deinit(gpa);
    try std.testing.expect(clean_warm.greeting_cached);
    try std.testing.expect(!clean_warm.greeting_checked);
    try std.testing.expectEqualStrings("", clean_warm.reports);

    // A new checker error must not reuse the earlier successful artifact.
    try tmp.dir.writeFile(io, .{ .sub_path = "app/Greeting.roc", .data =
        \\Greeting := {}.{
        \\    U64 := {}
        \\    message : Str
        \\    message = 42
        \\}
    });
    const bad = try compileReleasingSolverTypes(gpa, cache_dir, app_path, false);
    defer bad.deinit(gpa);
    try std.testing.expect(bad.has_user_errors);
    try std.testing.expect(bad.greeting_checked);
    try std.testing.expect(!bad.greeting_published);
    try std.testing.expectEqual(@as(usize, 0), bad.greeting_staged);
    const bad_warm = try compileReleasingSolverTypes(gpa, cache_dir, app_path, false);
    defer bad_warm.deinit(gpa);
    try std.testing.expect(bad_warm.greeting_checked);
    try std.testing.expect(!bad_warm.greeting_cached);
    try std.testing.expect(bad_warm.has_user_errors);
    try std.testing.expectEqualStrings(bad.reports, bad_warm.reports);
}

test "issue 12570: later evaluation diagnostics discard the staged warning-bearing environment" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFixture(&tmp);
    try tmp.dir.writeFile(io, .{ .sub_path = "app/Greeting.roc", .data =
        \\Greeting := {}.{
        \\    U64 := {}
        \\    message : Str
        \\    message = crash "late evaluation failure"
        \\}
    });
    const cache_dir = try tmp.dir.realPathFileAlloc(io, "cache", gpa);
    defer gpa.free(cache_dir);
    const app_path = try tmp.dir.realPathFileAlloc(io, "app/main.roc", gpa);
    defer gpa.free(app_path);
    var cache = try tmp.dir.openDir(io, "cache", .{ .iterate = true });
    defer cache.close(io);
    for (0..2) |_| {
        const run = try compileReleasingSolverTypes(gpa, cache_dir, app_path, false);
        defer run.deinit(gpa);
        try std.testing.expect(run.has_user_errors);
        try std.testing.expect(run.greeting_checked);
        try std.testing.expect(run.greeting_released);
        try std.testing.expectEqual(@as(usize, 1), run.greeting_staged);
        try std.testing.expect(!run.greeting_published);
        try std.testing.expect(std.mem.find(u8, run.reports, "late evaluation failure") != null);
        try std.testing.expectEqual(@as(usize, 0), try countUnregisteredStagingFiles(gpa, io, cache));
    }
}

test "issue 12570: staging write failure releases solver types without publishing an incomplete entry" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try writeFixture(&tmp);
    try tmp.dir.writeFile(io, .{ .sub_path = "app/Greeting.roc", .data = warning_source });
    const cache_dir = try tmp.dir.realPathFileAlloc(io, "cache", gpa);
    defer gpa.free(cache_dir);
    const app_path = try tmp.dir.realPathFileAlloc(io, "app/main.roc", gpa);
    defer gpa.free(app_path);
    const failed = try compileReleasingSolverTypes(gpa, cache_dir, app_path, true);
    defer failed.deinit(gpa);
    try std.testing.expect(!failed.has_user_errors);
    try std.testing.expect(failed.greeting_released);
    try std.testing.expect(failed.greeting_value_completed);
    try std.testing.expect(!failed.greeting_published);
    try std.testing.expectEqual(@as(usize, 1), failed.greeting_staged);
    try std.testing.expectEqual(@as(u64, 1), failed.store_failures);
    var cache = try tmp.dir.openDir(io, "cache", .{ .iterate = true });
    defer cache.close(io);
    try std.testing.expectEqual(@as(usize, 0), try countUnregisteredStagingFiles(gpa, io, cache));
    const retry = try compileReleasingSolverTypes(gpa, cache_dir, app_path, false);
    defer retry.deinit(gpa);
    try std.testing.expect(retry.greeting_checked);
    try std.testing.expect(retry.greeting_published);
    try std.testing.expectEqualStrings(failed.reports, retry.reports);
}
