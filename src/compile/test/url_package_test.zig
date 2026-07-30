//! Integration tests for building against URL packages served entirely from
//! the local package cache (no network), and for which modules belong in a
//! bundle of a package that has URL dependencies.

const std = @import("std");
const roc_target = @import("roc_target");

const BuildEnv = @import("../compile_build.zig").BuildEnv;

const fake_hash = "FakeHashAbcDefGhiJkLmNoPqRsTuVwXyZ123456789o";
const util_url = "https://example.com/foo/util/1.2.3/" ++ fake_hash ++ ".tar.zst";

test "URL dependency resolves from a warm cache with no network, and its modules are not bundleable" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // A pre-extracted bundle in the package cache, as a prior download would
    // have left it (sans sidecar, which must regenerate from the header).
    try tmp_dir.dir.createDirPath(io, "cache/" ++ fake_hash);
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "cache/" ++ fake_hash ++ "/main.roc",
        .data = "package [Util] {}\n",
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "cache/" ++ fake_hash ++ "/Util.roc",
        .data = "add_one : I64 -> I64\nadd_one = |n| n + 1\n",
    });

    // A local package depending on the URL.
    try tmp_dir.dir.createDirPath(io, "consumer");
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "consumer/main.roc",
        .data = "package [Consumer] { util: \"" ++ util_url ++ "\" }\n",
    });

    const cache_dir = try tmp_dir.dir.realPathFileAlloc(io, "cache", gpa);
    defer gpa.free(cache_dir);
    const consumer_main = try tmp_dir.dir.realPathFileAlloc(io, "consumer/main.roc", gpa);
    defer gpa.free(consumer_main);
    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    build_env.package_cache_dir = try gpa.dupe(u8, cache_dir);

    // Resolution must succeed without any network access: the bundle is
    // already in the cache, so the fetcher's warm path serves it.
    try build_env.discoverDependencies(consumer_main);

    // The URL package exists under its identity (the full URL) and knows it
    // came from a URL.
    const util_pkg = build_env.packages.getPtr(util_url) orelse return error.TestUnexpectedResult;
    try std.testing.expect(util_pkg.url != null);
    try std.testing.expectEqualStrings("example.com/foo/util", util_pkg.url.?.urlIdPrefix());

    // The root's shorthand points at the URL package.
    const root_pkg_name = build_env.discovered_pkg_name orelse return error.TestUnexpectedResult;
    const root_pkg = build_env.packages.getPtr(root_pkg_name) orelse return error.TestUnexpectedResult;
    const shorthand = root_pkg.shorthands.get("util") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqualStrings(util_url, shorthand.name);

    // A sidecar was regenerated next to the extracted bundle.
    tmp_dir.dir.access(io, "cache/" ++ fake_hash ++ ".deps.json", .{}) catch return error.TestUnexpectedResult;

    // Bundling the consumer must include its own modules but exclude the URL
    // dependency's modules (both by package and by cache location).
    try std.testing.expect(build_env.isBundleableModule(root_pkg_name, consumer_main));
    try std.testing.expect(!build_env.isBundleableModule(util_url, util_pkg.root_file));

    const cached_local_path = try std.fs.path.join(gpa, &.{ cache_dir, fake_hash, "Util.roc" });
    defer gpa.free(cached_local_path);
    try std.testing.expect(!build_env.isBundleableModule("module", cached_local_path));

    const watch_inputs = try build_env.collectWatchInputs();
    defer build_env.freeWatchInputs(watch_inputs);

    var found_consumer = false;
    var found_url_package_file = false;
    for (watch_inputs) |path| {
        if (std.mem.eql(u8, path, consumer_main)) {
            found_consumer = true;
        }
        if (std.mem.startsWith(u8, path, util_pkg.root_dir)) {
            found_url_package_file = true;
        }
    }

    try std.testing.expect(found_consumer);
    try std.testing.expect(!found_url_package_file);
}

test "transitive URL dependency compiles through the orchestration core from a warm cache" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // The transitive URL package, pre-extracted in the package cache.
    try tmp_dir.dir.createDirPath(io, "cache/" ++ fake_hash);
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "cache/" ++ fake_hash ++ "/main.roc",
        .data = "package [Util] {}\n",
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "cache/" ++ fake_hash ++ "/Util.roc",
        .data =
        \\Util := [].{
        \\    two : I64
        \\    two = 2
        \\}
        \\
        ,
    });

    // app -> mid (local path package) -> util (URL): the URL dependency is
    // transitive from the app, the exact shape issue 9509 regressed on.
    try tmp_dir.dir.createDirPath(io, "app/pf");
    try tmp_dir.dir.createDirPath(io, "app/mid");
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/pf/main.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try(_, [Exit(I8), ..]) }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |args|
        \\    match main!(args) {
        \\        Ok(_) => 0
        \\        Err(Exit(code)) => code
        \\        Err(_) => 1
        \\    }
        \\
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/mid/main.roc",
        .data = "package [Mid] { util: \"" ++ util_url ++ "\" }\n",
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/mid/Mid.roc",
        .data =
        \\import util.Util
        \\
        \\Mid := [].{
        \\    add_two : I64 -> I64
        \\    add_two = |n| n + Util.two
        \\}
        \\
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app/main.roc",
        .data =
        \\app [main!] { pf: platform "./pf/main.roc", mid: "./mid/main.roc" }
        \\
        \\import mid.Mid
        \\
        \\main! = |_args| Ok(Mid.add_two(40))
        \\
        ,
    });

    const cache_dir = try tmp_dir.dir.realPathFileAlloc(io, "cache", gpa);
    defer gpa.free(cache_dir);
    const app_main = try tmp_dir.dir.realPathFileAlloc(io, "app/main.roc", gpa);
    defer gpa.free(app_main);
    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    build_env.package_cache_dir = try gpa.dupe(u8, cache_dir);

    // Compile the app (which reaches through mid to the URL package) with no
    // network: every pipeline that consumes the core — check, build, run,
    // test — resolves and registers the same transitive graph.
    try build_env.build(app_main);

    // Drain and count without rendering: no diagnostics of any severity.
    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);
    for (drained) |mod| {
        try std.testing.expectEqual(@as(usize, 0), mod.reports.len);
    }

    // Executable finalization succeeded, so a run back half may lower this.
    try std.testing.expect(build_env.executable_artifacts_finalized);

    // The transitive URL package is registered under its URL identity.
    try std.testing.expect(build_env.packages.getPtr(util_url) != null);
}
