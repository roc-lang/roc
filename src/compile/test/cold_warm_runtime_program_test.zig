//! A build's runtime program must not depend on whether the build evaluated
//! its compile-time roots or found their values in the checked module cache
//! (design.md "Runtime Programs Read Stored Values"). Each test builds one app
//! twice against one cache directory, cold and then warm, and requires the
//! two runtime programs to print identically, procedure by procedure, with
//! the same static data.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const check = @import("check");
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
    lss_functions: usize,
    boxy_functions: usize,
    pending_boxy_functions: usize,
    /// Only ids/tags, never pointers into the coordinator's artifact.
    first_lss_function: ?struct { root: check.CheckedArtifact.ComptimeRootId, payload: check.CheckedArtifact.CompileTimeRootPayload },
    first_boxy_function: ?check.CheckedArtifact.StoredConstTemplate,
    debug_observations: usize,
    static_callables: usize,
    invalid_static_parts: usize,
    /// Stored static values whose `ConstStore` belongs to a module outside
    /// the root, its relations, and its lowering-visible imports.
    stored_outside_input: usize,
};

fn buildWithCache(gpa: std.mem.Allocator, cache_dir: []const u8, app_path: []const u8) BuildError!Built {
    return buildWithCacheAtTarget(gpa, cache_dir, app_path, .{});
}

fn buildWithCacheAtTarget(gpa: std.mem.Allocator, cache_dir: []const u8, app_path: []const u8, target: lir.CheckedPipeline.TargetConfig) BuildError!Built {
    return buildWithCacheAndTests(gpa, cache_dir, app_path, target, false);
}

/// `program_tests` builds as a linked `roc build` does: every checked module
/// is one of the program's test modules, as `BuildEnv` makes every module of
/// the app's package, so their expects are the build's discovery-only roots,
/// and the runtime program carries internal static data.
fn buildWithCacheAndTests(gpa: std.mem.Allocator, cache_dir: []const u8, app_path: []const u8, target: lir.CheckedPipeline.TargetConfig, program_tests: bool) BuildError!Built {
    const roc_ctx = CoreCtx.os(gpa, gpa, std.testing.io);
    var cache_manager = CacheManager.init(gpa, .{ .enabled = true, .cache_dir = cache_dir }, roc_ctx);
    defer cache_manager.deinit();
    var builtin_modules = try eval.BuiltinModules.init(gpa, std.testing.io);
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
    coord.specialization_strategy = target.specialization_strategy;
    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena_impl.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    if (coord.hasUserErrors()) return reportErrors(&coord);
    coord.runtime_lowering = .{ .target = target };
    var test_modules = std.ArrayList(check.CheckedArtifact.CheckedModuleArtifactKey).empty;
    defer test_modules.deinit(gpa);
    if (program_tests) {
        var packages = coord.packages.iterator();
        while (packages.next()) |package| {
            for (package.value_ptr.*.modules.items) |*module| {
                if (module.checkedArtifact()) |artifact| try test_modules.append(gpa, artifact.key);
            }
        }
        coord.program_test_modules = test_modules.items;
        coord.runtime_lowering.?.include_provided_data_exports = true;
        coord.runtime_lowering.?.include_internal_static_data = true;
    }
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
    // The modules a `roc build` gives its static-data builder: the root, its
    // relations, and the imports the root's lowering can see.
    const root_artifact = coord.appRootCheckedArtifact();
    const imported_views = try coord.collectImportedArtifactViews(gpa, root_artifact);
    defer gpa.free(imported_views);
    const relation_views = try coord.collectRelationArtifactViews(gpa, root_artifact);
    defer gpa.free(relation_views);
    var stored_outside_input: usize = 0;
    for (result.static_data_values.items) |value| {
        const stored = value.stored orelse continue;
        if (std.mem.eql(u8, &stored.module.bytes, &root_artifact.key.digest().bytes)) continue;
        const visible = for (imported_views) |view| {
            if (std.mem.eql(u8, &view.key.bytes, &stored.module.bytes)) break true;
        } else for (relation_views) |view| {
            if (std.mem.eql(u8, &view.key.bytes, &stored.module.bytes)) break true;
        } else false;
        if (!visible) stored_outside_input += 1;
    }
    if (runtime.frozen_static_data) |frozen| for (frozen.exports) |item| {
        try out.writer.print("frozen {s} {x}\n", .{ item.symbol_name, item.bytes });
    };
    var facts: Built = .{
        .program = undefined,
        .respecialized = respecialized,
        .cache_hits = coord.getBuildStats().cache_hits,
        .lss_functions = 0,
        .boxy_functions = 0,
        .pending_boxy_functions = 0,
        .first_lss_function = null,
        .first_boxy_function = null,
        .debug_observations = coord.appRootCheckedArtifact().compile_time_debug.entries.len,
        .static_callables = 0,
        .invalid_static_parts = 0,
        .stored_outside_input = stored_outside_input,
    };
    for (coord.appRootCheckedArtifact().compile_time_roots.roots) |root| {
        if (root.lss_value_has_functions or root.payload == .fn_value) {
            facts.lss_functions += 1;
            if (root.needsEvaluation(.boxy)) facts.pending_boxy_functions += 1;
            if (facts.first_lss_function == null and root.kind == .constant) facts.first_lss_function = .{ .root = root.id, .payload = root.payload };
        }
        if (root.boxy_value) |stored| {
            facts.boxy_functions += 1;
            if (facts.first_boxy_function == null and root.kind == .constant) facts.first_boxy_function = stored;
        }
    }
    for (result.static_data_values.items) |value| {
        if (value.parts) |parts| {
            if (parts.kind == .callable) facts.static_callables += 1;
            if (value.initializer != null) facts.invalid_static_parts += 1;
            for (parts.children) |child| {
                const index = @backingInt(child);
                if (index >= result.static_data_values.items.len or result.static_data_values.items[index].initializer != null) facts.invalid_static_parts += 1;
            }
        }
    }
    facts.program = try out.toOwnedSlice();
    return facts;
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

// repro for https://github.com/roc-lang/roc/issues/12930
test "issue 12930: a program's expects stay out of its runtime program whether evaluated or read from the cache" {
    if (is_freestanding) return error.SkipZigTest;
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.createDirPath(io, "cache");
    try tmp_dir.dir.createDirPath(io, "app/.roc_echo_platform");
    try tmp_dir.dir.createDirPath(io, "lib");
    // The app and an expect of the local package `lib` both call `List.all`
    // over `List(Str)` with a `Str -> Bool` callback.
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app/main.roc", .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc", lib: "../lib/main.roc" }
        \\import pf.Echo
        \\import lib.Greet
        \\greeting = Greet.hello("there, this is a fairly long string")
        \\main! = |args| {
        \\    Echo.line!(greeting)
        \\    Echo.line!(Str.inspect(args.all(|arg| arg != greeting)))
        \\    Ok({})
        \\}
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "lib/main.roc", .data =
        \\package [Greet, Fixture] {}
    });
    // Only an expect of `Greet` reads `Fixture`, so nothing in the app's
    // runtime program can reach `Fixture`'s stored values. The literal of the
    // custom `from_quote` type makes the expect a discovery root of the build.
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "lib/Greet.roc", .data =
        \\import Fixture
        \\Greet := {}.{
        \\    hello : Str -> Str
        \\    hello = |name| "Hello ".concat(name)
        \\}
        \\quoted_after = |_name| "a literal whose type its caller picks"
        \\expect {
        \\    accepts = |name| {
        \\        quoted : Fixture
        \\        quoted = quoted_after(name)
        \\        Fixture.names.contains(name) and quoted.text() != name
        \\    }
        \\    ["a fixture string only an expect reads, long enough to be heap allocated"].all(accepts)
        \\}
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "lib/Fixture.roc", .data =
        \\Fixture := { text : Str }.{
        \\    from_quote : Str -> Try(Fixture, [BadQuotedBytes(Str)])
        \\    from_quote = |text| Ok(Fixture.{ text })
        \\
        \\    text : Fixture -> Str
        \\    text = |fixture| fixture.text
        \\
        \\    names : List(Str)
        \\    names = List.repeat("a fixture string only an expect reads, long enough to be heap allocated", 3)
        \\}
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app/.roc_echo_platform/main.roc", .data = platform_main });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app/.roc_echo_platform/Echo.roc", .data = platform_echo });
    const cache_dir = try tmp_dir.dir.realPathFileAlloc(io, "cache", gpa);
    defer gpa.free(cache_dir);
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app/main.roc", gpa);
    defer gpa.free(app_path);

    const cold = try buildWithCacheAndTests(gpa, cache_dir, app_path, .{}, true);
    defer gpa.free(cold.program);
    const warm = try buildWithCacheAndTests(gpa, cache_dir, app_path, .{}, true);
    defer gpa.free(warm.program);
    try std.testing.expect(warm.cache_hits > 0);
    // The expect is a discovery-only root: neither build's runtime program
    // contains its closure or the static data it reads, so every stored
    // static value is one the build's static-data builder can read.
    try std.testing.expectEqual(@as(usize, 0), cold.stored_outside_input);
    try std.testing.expectEqual(@as(usize, 0), warm.stored_outside_input);
    try std.testing.expectEqualStrings(cold.program, warm.program);
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

test "strategy root readiness preserves nested callable representations across a shared cache" {
    if (is_freestanding) return error.SkipZigTest;
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.createDirPath(io, "cache");
    try tmp.dir.createDirPath(io, "app/.roc_echo_platform");
    try tmp.dir.writeFile(io, .{ .sub_path = "app/.roc_echo_platform/main.roc", .data = platform_main });
    try tmp.dir.writeFile(io, .{ .sub_path = "app/.roc_echo_platform/Echo.roc", .data = platform_echo });
    try tmp.dir.writeFile(io, .{ .sub_path = "app/main.roc", .data =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\make : U64 -> (U64 -> U64)
        \\make = |offset| |value| value + offset
        \\stored : { first : (U64 -> U64), second : (U64 -> U64) }
        \\stored = {
        \\    dbg "strategy root produced"
        \\    { first: make(7), second: make(11) }
        \\}
        \\main! = |args| {
        \\    first = stored.first
        \\    second = stored.second
        \\    Echo.line!(Str.inspect(first(List.len(args))))
        \\    Echo.line!(Str.inspect(second(List.len(args))))
        \\    Ok({})
        \\}
    });
    const cache = try tmp.dir.realPathFileAlloc(io, "cache", gpa);
    defer gpa.free(cache);
    const app = try tmp.dir.realPathFileAlloc(io, "app/main.roc", gpa);
    defer gpa.free(app);
    const first = try buildWithCacheAtTarget(gpa, cache, app, dev_target);
    defer gpa.free(first.program);
    try std.testing.expect(first.lss_functions > 0);
    try std.testing.expect(first.first_lss_function != null);
    try std.testing.expect(first.first_lss_function.?.payload == .const_node);
    try std.testing.expectEqual(first.lss_functions, first.pending_boxy_functions);
    try std.testing.expectEqual(@as(usize, 0), first.boxy_functions);
    try std.testing.expect(first.debug_observations > 0);

    const same_lss = try buildWithCacheAtTarget(gpa, cache, app, dev_target);
    defer gpa.free(same_lss.program);
    try std.testing.expect(same_lss.cache_hits > 0);
    try std.testing.expectEqualDeep(first.first_lss_function, same_lss.first_lss_function);

    var boxy_target = dev_target;
    boxy_target.specialization_strategy = .boxy;
    const boxy = try buildWithCacheAtTarget(gpa, cache, app, boxy_target);
    defer gpa.free(boxy.program);
    try std.testing.expect(boxy.cache_hits > 0);
    try std.testing.expect(boxy.boxy_functions > 0);
    try std.testing.expect(boxy.first_boxy_function != null);
    try std.testing.expectEqual(@as(usize, 0), boxy.pending_boxy_functions);
    try std.testing.expectEqualDeep(first.first_lss_function, boxy.first_lss_function);
    try std.testing.expectEqual(first.lss_functions, boxy.lss_functions);
    try std.testing.expectEqual(first.debug_observations, boxy.debug_observations);
    // These are always-active RF assertions over the emitted immutable graph,
    // not an expectation that a debug invariant happens to catch the bug.
    try std.testing.expect(boxy.static_callables > 0);
    try std.testing.expectEqual(@as(usize, 0), boxy.invalid_static_parts);

    const same_boxy = try buildWithCacheAtTarget(gpa, cache, app, boxy_target);
    defer gpa.free(same_boxy.program);
    try std.testing.expect(same_boxy.cache_hits > 0);
    try std.testing.expectEqualDeep(boxy.first_boxy_function, same_boxy.first_boxy_function);
    try std.testing.expectEqualDeep(first.first_lss_function, same_boxy.first_lss_function);
    try std.testing.expectEqual(@as(usize, 0), same_boxy.invalid_static_parts);

    const last = try buildWithCacheAtTarget(gpa, cache, app, dev_target);
    defer gpa.free(last.program);
    try std.testing.expect(last.cache_hits > 0);
    try std.testing.expectEqualDeep(first.first_lss_function, last.first_lss_function);
    try std.testing.expectEqualDeep(boxy.first_boxy_function, last.first_boxy_function);
    try std.testing.expectEqual(first.debug_observations, last.debug_observations);
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

const dev_target: lir.CheckedPipeline.TargetConfig = .{
    .inline_mode = lir.CheckedPipeline.dev_inline_mode,
    .spec_constr_clone_inlining = lir.CheckedPipeline.dev_spec_constr_clone_inlining,
};

/// Evaluate an app's compile-time roots under a dev build's Solved policy and
/// under an optimized one, each in its own cache, then read the values each
/// stored under the other policy: a stored value means the same to every
/// command (design.md "Stored Function Capture Contract").
fn expectStoredValuesIndependentOfPolicy(app_source: []const u8) ExpectError!void {
    if (is_freestanding) return error.SkipZigTest;
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.createDirPath(io, "dev_first");
    try tmp_dir.dir.createDirPath(io, "optimized_first");
    try tmp_dir.dir.createDirPath(io, "app/.roc_echo_platform");
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app/main.roc", .data = app_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app/.roc_echo_platform/main.roc", .data = platform_main });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app/.roc_echo_platform/Echo.roc", .data = platform_echo });
    const dev_first = try tmp_dir.dir.realPathFileAlloc(io, "dev_first", gpa);
    defer gpa.free(dev_first);
    const optimized_first = try tmp_dir.dir.realPathFileAlloc(io, "optimized_first", gpa);
    defer gpa.free(optimized_first);
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app/main.roc", gpa);
    defer gpa.free(app_path);

    const cold_dev = try buildWithCacheAtTarget(gpa, dev_first, app_path, dev_target);
    defer gpa.free(cold_dev.program);
    const warm_optimized = try buildWithCacheAtTarget(gpa, dev_first, app_path, .{});
    defer gpa.free(warm_optimized.program);
    const cold_optimized = try buildWithCacheAtTarget(gpa, optimized_first, app_path, .{});
    defer gpa.free(cold_optimized.program);
    const warm_dev = try buildWithCacheAtTarget(gpa, optimized_first, app_path, dev_target);
    defer gpa.free(warm_dev.program);

    try std.testing.expect(cold_dev.respecialized);
    try std.testing.expect(cold_optimized.respecialized);
    try std.testing.expect(!warm_optimized.respecialized);
    try std.testing.expect(!warm_dev.respecialized);
    try std.testing.expectEqualStrings(cold_optimized.program, warm_optimized.program);
    try std.testing.expectEqualStrings(cold_dev.program, warm_dev.program);
}

test "a stored closure means the same to every Solved policy when its optimized body drops a capture" {
    try expectStoredValuesIndependentOfPolicy(
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\make : Str -> (Str -> Str)
        \\make = |captured| |text| {
        \\    pair = { first: captured, second: text }
        \\    pair.second
        \\}
        \\stored : Str -> Str
        \\stored = make("a captured string long enough to live on the heap")
        \\run : Str -> Str
        \\run = |text| stored(text)
        \\main! = |args| {
        \\    Echo.line!(run(Str.inspect(List.len(args))))
        \\    Ok({})
        \\}
    );
}

test "a stored encoder means the same to every Solved policy when its optimized body drops its names" {
    try expectStoredValuesIndependentOfPolicy(
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\Format := [Default].{
        \\    rename_field : Format, Str -> Str
        \\    rename_field = |_, name| name
        \\    encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)
        \\    encode_record = |state, _, write_fields| write_fields(state, |field_state, _, write_value| write_value(field_state))
        \\    encode_u64 : U64, U64 -> Try(U64, [Zero])
        \\    encode_u64 = |value, state| if value == 0 { Err(Zero) } else { Ok(state + value) }
        \\}
        \\encoder_of : shape -> (shape, U64 -> Try(U64, err))
        \\    where [shape.encoder_for : Format -> (shape, U64 -> Try(U64, err))]
        \\encoder_of = |_| {
        \\    Shape : shape
        \\    Shape.encoder_for(Format.Default)
        \\}
        \\stored : { n : U64 }, U64 -> Try(U64, [Zero])
        \\stored = encoder_of({ n: 0 })
        \\main! = |args| {
        \\    Echo.line!(Str.inspect(stored({ n: List.len(args) }, 4)))
        \\    Ok({})
        \\}
    );
}

/// Build an app, edit it, and build it again against one cache. The second
/// build restores the imported module's stored functions while evaluating the
/// app's constants, and stores them again in the app's own module.
fn expectRestoredFunctionsStoreAgain(app_source: []const u8, module_name: []const u8, module_source: []const u8) ExpectError!void {
    if (is_freestanding) return error.SkipZigTest;
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.createDirPath(io, "cache");
    try tmp_dir.dir.createDirPath(io, "app/.roc_echo_platform");
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app/main.roc", .data = app_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = module_name, .data = module_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app/.roc_echo_platform/main.roc", .data = platform_main });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app/.roc_echo_platform/Echo.roc", .data = platform_echo });
    const cache_dir = try tmp_dir.dir.realPathFileAlloc(io, "cache", gpa);
    defer gpa.free(cache_dir);
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app/main.roc", gpa);
    defer gpa.free(app_path);

    const first = try buildWithCacheAtTarget(gpa, cache_dir, app_path, .{});
    defer gpa.free(first.program);
    const edited = try std.fmt.allocPrint(gpa, "{s}\n# edited\n", .{app_source});
    defer gpa.free(edited);
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app/main.roc", .data = edited });
    const second = try buildWithCacheAtTarget(gpa, cache_dir, app_path, .{});
    defer gpa.free(second.program);
    try std.testing.expect(second.respecialized);
    try std.testing.expectEqualStrings(first.program, second.program);
}

test "an encoder restored from an imported module stores its captures again" {
    try expectRestoredFunctionsStoreAgain(
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\import Codecs
        \\held : { empty : ({}, U64 -> Try(U64, [Zero])), pair : ({ n : U64 }, U64 -> Try(U64, [Zero])) }
        \\held = { empty: Codecs.stored_empty, pair: Codecs.stored_pair }
        \\main! = |args| {
        \\    state = List.len(args)
        \\    Echo.line!(Str.inspect((held.empty)({}, state)))
        \\    Echo.line!(Str.inspect((held.pair)({ n: state }, state)))
        \\    Ok({})
        \\}
    , "app/Codecs.roc",
        \\Format := [Plain, Other].{
        \\    rename_field : Format, Str -> Str
        \\    rename_field = |_, name| name
        \\    encode_record : U64, U64, (U64, (U64, Str, (U64 -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)) -> Try(U64, err)
        \\    encode_record = |state, _, write_fields| write_fields(state, |field_state, _, write_value| write_value(field_state))
        \\    encode_u64 : U64, U64 -> Try(U64, [Zero])
        \\    encode_u64 = |value, state| if value == 0 { Err(Zero) } else { Ok(state + value) }
        \\}
        \\Codecs := [].{
        \\    encoder_with : Format, shape -> (shape, U64 -> Try(U64, err))
        \\        where [shape.encoder_for : Format -> (shape, U64 -> Try(U64, err))]
        \\    encoder_with = |format, _| {
        \\        Shape : shape
        \\        Shape.encoder_for(format)
        \\    }
        \\    stored_empty : {}, U64 -> Try(U64, [Zero])
        \\    stored_empty = encoder_with(Format.Other, {})
        \\    stored_pair : { n : U64 }, U64 -> Try(U64, [Zero])
        \\    stored_pair = encoder_with(Format.Other, { n: 0 })
        \\}
    );
}

test "a closure restored from an imported module stores its captures again" {
    try expectRestoredFunctionsStoreAgain(
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\import Closures
        \\held : { call : (Str -> Str) }
        \\held = { call: Closures.stored }
        \\main! = |args| {
        \\    Echo.line!((held.call)(Str.inspect(List.len(args))))
        \\    Ok({})
        \\}
    , "app/Closures.roc",
        \\Closures := [].{
        \\    make : Str -> (Str -> Str)
        \\    make = |captured| |text| {
        \\        pair = { first: captured, second: text }
        \\        pair.second
        \\    }
        \\    stored : Str -> Str
        \\    stored = make("a captured string long enough to live on the heap")
        \\}
    );
}
