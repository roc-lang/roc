//! A build's runtime program must not depend on whether the build evaluated
//! its compile-time roots or found their values in the checked module cache
//! (design.md "Runtime Programs Read Stored Values"). Each test builds one app
//! twice against one cache directory, cold and then warm, and requires the
//! two runtime programs to print identically, procedure by procedure, with
//! the same static data.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");

const CacheManager = @import("../cache_manager.zig").CacheManager;
const Coordinator = @import("../coordinator.zig").Coordinator;
const CoordinatorError = @import("../coordinator.zig").CoordinatorError;
const CoreCtx = @import("ctx").CoreCtx;
const is_freestanding = @import("../threading.zig").is_freestanding;

const platform_main =
    \\platform ""
    \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
    \\    exposes [Echo]
    \\    packages {}
    \\    provides { "roc_main": main_for_host! }
    \\    hosted { "roc_echo_line": Echo.line! }
    \\import Echo
    \\main_for_host! : List(Str) => I8
    \\main_for_host! = |args|
    \\    match main!(args) {
    \\        Ok({}) => 0
    \\        Err(Exit(code)) => code
    \\        Err(_) => 1
    \\    }
;

const platform_echo =
    \\Echo := [].{
    \\    line! : Str => {}
    \\}
;

const BuildError = std.mem.Allocator.Error ||
    eval.BuiltinModules.InitError ||
    std.Thread.SpawnError ||
    Coordinator.AppDiscoveryError ||
    CoordinatorError ||
    lir.CheckedPipeline.LowerResourceError ||
    lir.DebugPrint.Error ||
    error{TestUnexpectedResult};

/// What one build of the app produced.
const Built = struct {
    /// Every runtime procedure and static-data value, printed.
    program: []u8,
    /// Whether the build specialized its runtime program again after
    /// evaluating compile-time roots.
    respecialized: bool,
    cache_hits: u32,
};

fn buildWithCache(gpa: std.mem.Allocator, cache_dir: []const u8, app_path: []const u8) BuildError!Built {
    const roc_ctx = CoreCtx.os(gpa, gpa, std.testing.io);
    var cache_manager = CacheManager.init(gpa, .{ .enabled = true, .cache_dir = cache_dir }, roc_ctx);
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
    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena_impl.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    if (coord.hasUserErrors()) return reportErrors(&coord);
    const target: lir.CheckedPipeline.TargetConfig = .{};
    coord.runtime_lowering = .{ .target = target };
    try coord.finishCheckedProgram(.executable_artifacts);
    if (coord.hasUserErrors()) return reportErrors(&coord);
    const session = &coord.program_session.?;
    const respecialized = session.runtime_stages == .respecialized;
    var runtime = try session.takeRuntime(gpa, session.runtime_roots, target);
    defer runtime.deinit();

    var out: std.Io.Writer.Allocating = .init(gpa);
    defer out.deinit();
    const result = &runtime.lir_result;
    for (0..result.store.procSpecCount()) |index| {
        try lir.DebugPrint.writeProc(gpa, &result.store, &result.layouts, @fromBackingInt(@intCast(index)), &out.writer);
        try out.writer.writeByte('\n');
    }
    for (result.static_data_values.items, 0..) |value, index| {
        try out.writer.print("static s{d} layout={d} initializer={?d}\n", .{ index, @backingInt(value.layout_idx), if (value.initializer) |proc| @backingInt(proc) else null });
    }
    if (runtime.frozen_static_data) |frozen| for (frozen.exports) |item| {
        try out.writer.print("frozen {s} {x}\n", .{ item.symbol_name, item.bytes });
    };
    return .{ .program = try out.toOwnedSlice(), .respecialized = respecialized, .cache_hits = coord.getBuildStats().cache_hits };
}

fn reportErrors(coord: *Coordinator) error{TestUnexpectedResult} {
    var reports = coord.iterReports();
    while (reports.next()) |entry| {
        if (entry.report.severity.isError()) std.debug.print("unexpected report: {s}\n", .{entry.report.title});
    }
    return error.TestUnexpectedResult;
}

const ExpectError = BuildError ||
    std.Io.Dir.CreateDirPathError ||
    std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError ||
    error{ SkipZigTest, TestExpectedEqual };

fn expectColdEqualsWarm(app_source: []const u8) ExpectError!void {
    if (is_freestanding) return error.SkipZigTest;
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.createDirPath(io, "cache");
    try tmp_dir.dir.createDirPath(io, "app/.roc_echo_platform");
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app/main.roc", .data = app_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app/.roc_echo_platform/main.roc", .data = platform_main });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app/.roc_echo_platform/Echo.roc", .data = platform_echo });
    const cache_dir = try tmp_dir.dir.realPathFileAlloc(io, "cache", gpa);
    defer gpa.free(cache_dir);
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app/main.roc", gpa);
    defer gpa.free(app_path);

    const cold = try buildWithCache(gpa, cache_dir, app_path);
    defer gpa.free(cold.program);
    const warm = try buildWithCache(gpa, cache_dir, app_path);
    defer gpa.free(warm.program);
    // The cold build evaluated compile-time roots, so its runtime program
    // was specialized again from their stored values; the warm one found
    // them in the cache and evaluated none.
    try std.testing.expectEqualStrings(cold.program, warm.program);
    try std.testing.expect(cold.respecialized);
    try std.testing.expect(!warm.respecialized);
    try std.testing.expect(warm.cache_hits > 0);
}

test "a hoisted string a body computes lowers the same whether evaluated or read from the cache" {
    try expectColdEqualsWarm(
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\main! = |_args| {
        \\    Echo.line!("One".concat(" Two"))
        \\    Echo.line!("Hello".concat(" there, this is a fairly long string"))
        \\    Echo.line!(Str.inspect([1.U8, 2, 3].map(|x| x + 1)))
        \\    Ok({})
        \\}
    );
}

test "top-level constants of every shape lower the same whether evaluated or read from the cache" {
    try expectColdEqualsWarm(
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\small = "ab".concat("c")
        \\big = "Hello".concat(" there, this is a fairly long string")
        \\count = [1.U64, 2, 3].map(|x| x * 2).len()
        \\status : Try(Str, Str)
        \\status = Ok(small)
        \\record = { name: big, size: count, status: status }
        \\table = List.repeat(7.U32, 100)
        \\main! = |args| {
        \\    Echo.line!(small)
        \\    Echo.line!(record.name)
        \\    Echo.line!(Str.inspect(record.size + count))
        \\    Echo.line!(Str.inspect(List.get(table, List.len(args))))
        \\    Echo.line!(Str.inspect(record.status))
        \\    Ok({})
        \\}
    );
}

test "a function value a constant holds lowers the same whether evaluated or read from the cache" {
    try expectColdEqualsWarm(
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\offset = 3.U64
        \\adders = [|x| x + offset, |x| x * offset]
        \\main! = |args| {
        \\    for adder in adders {
        \\        Echo.line!(Str.inspect(adder(List.len(args))))
        \\    }
        \\    Ok({})
        \\}
    );
}

test "issue 12595 shared row callables lower identically from fresh and stored values" {
    try expectColdEqualsWarm(
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\Payload : { call : (U64 -> U64), offset : U64 }
        \\Message : [First(Payload), Second(Payload), Third(Payload)]
        \\make : U64 -> (U64 -> U64)
        \\make = |offset| |value| value + offset
        \\stored = make(7)
        \\run : Message -> U64
        \\run = |message| {
        \\    payload = match message {
        \\        First(value) => value
        \\        Second(value) => value
        \\        Third(value) => value
        \\    }
        \\    callback = payload.call
        \\    callback(payload.offset)
        \\}
        \\main! = |args| {
        \\    n = args.len()
        \\    Echo.line!(Str.inspect(run(First({ call: stored, offset: n }))))
        \\    Echo.line!(Str.inspect(run(Second({ call: make(n), offset: n }))))
        \\    callback = Box.unbox(Box.box(make(n)))
        \\    Echo.line!(Str.inspect(run(Third({ call: callback, offset: n }))))
        \\    Ok({})
        \\}
    );
}
