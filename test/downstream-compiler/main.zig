//! A compiler driver using only the fetched package’s named modules.

const std = @import("std");
const build_options = @import("build_options");
const lir = @import("lir");
const eval = @import("eval");
const check = @import("check");
const base = @import("base");
const host_abi = @import("builtins").host_abi;
const roc_target = @import("roc_target");

const Coordinator = @import("compile").coordinator.Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

// This scalar program has no allocations, host effects, or diagnostics.
// Any callback means the fixture no longer exercises the intended contract.
fn testRocAlloc(_: *host_abi.RocOps, _: usize, _: usize) callconv(.c) *anyopaque {
    @panic("unexpected allocation");
}
fn testRocDealloc(_: *host_abi.RocOps, _: *anyopaque, _: usize) callconv(.c) void {
    @panic("unexpected deallocation");
}
fn testRocRealloc(_: *host_abi.RocOps, _: *anyopaque, _: usize, _: usize) callconv(.c) *anyopaque {
    @panic("unexpected reallocation");
}
fn testRocDbg(_: *host_abi.RocOps, _: [*]const u8, _: usize) callconv(.c) void {
    @panic("unexpected dbg");
}
fn testRocExpectFailed(_: *host_abi.RocOps, _: [*]const u8, _: usize) callconv(.c) void {
    @panic("unexpected expect failure");
}
fn testRocCrashed(_: *host_abi.RocOps, _: [*]const u8, _: usize) callconv(.c) void {
    @panic("unexpected Roc crash");
}

const SmokeError = std.process.Args.ToSliceError || std.mem.Allocator.Error ||
    eval.BuiltinModules.InitError || std.Thread.SpawnError ||
    Coordinator.AppDiscoveryError || @import("compile").coordinator.CoordinatorError ||
    @import("compile").package.PublishError || lir.CheckedPipeline.LowerResourceError ||
    lir.LirImage.ViewError || eval.LirInterpreter.Error ||
    error{ ExpectedAppPath, EntrypointNotFound, UnexpectedResult };

/// Check, lower, and execute the fixture through the exported compiler modules.
pub fn main(init: std.process.Init) SmokeError!void {
    const gpa = init.gpa;
    const arena = init.arena.allocator();
    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 2) return error.ExpectedAppPath;
    const app_path = args[1];
    const expected_names: []const []const u8 = &.{"roc_main"};

    // 1. Builtins + Coordinator.
    var builtin_modules = try eval.BuiltinModules.init(gpa);
    defer builtin_modules.deinit();

    var coord = try Coordinator.init(
        gpa,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_compatibility_id,
        null,
        CoreCtx.default(gpa, arena, init.io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    try coord.start();

    // 2. Discover + compile.
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();

    // 3. iterReports must walk without crashing; the fixture has no errors.
    var report_iter = coord.iterReports();
    while (report_iter.next()) |entry| {
        try expect(entry.report.severity != .fatal);
        try expect(entry.report.severity != .runtime_error);
    }
    try expect(!coord.hasUserErrors());

    // 4. Finalization always publishes executable artifacts; diagnostics do
    // not form a separate failure outcome.
    try coord.finishCheckedProgram(.executable_artifacts);
    try expect(!coord.hasUserErrors());

    // 5. Lower to LIR in a contiguous FixedBufferAllocator (the runtime
    //    arena pattern documented in README "Runtime arena"). 128 MiB is
    //    the recommended starting size for an embedder.
    const RUNTIME_ARENA_SIZE: usize = 128 * 1024 * 1024;
    const runtime_buffer = try gpa.alignedAlloc(u8, .@"16", RUNTIME_ARENA_SIZE);
    defer gpa.free(runtime_buffer);

    var runtime_fba = std.heap.FixedBufferAllocator.init(runtime_buffer);
    const runtime_alloc = runtime_fba.allocator();

    const root = coord.executableRootCheckedArtifact();
    const imports = try coord.collectImportedArtifactViews(arena, root);
    const relations = try coord.collectRelationArtifactViews(arena, root);

    const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(arena, root.root_requests.runtime_requests);

    const lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        runtime_alloc,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root, relations),
            .imports = imports,
        },
        .{ .requests = lir_roots },
        .{ .target_usize = base.target.TargetUsize.native },
    );

    // 6. Only provides declarations become entrypoints; requirements stay internal.
    const entrypoints = try lowered.platformEntrypoints(runtime_alloc);
    const entrypoint_names = try lowered.platformEntrypointNames(arena, root);
    try expectEqual(expected_names.len, lowered.lir_result.root_procs.items.len);
    try expectEqual(expected_names.len, entrypoints.len);
    try expectEqual(entrypoints.len, entrypoint_names.len);
    for (expected_names, entrypoint_names, entrypoints, 0..) |expected, actual, entrypoint, ordinal| {
        try expectEqualStrings(expected, actual);
        try expectEqual(@as(u32, @intCast(ordinal)), entrypoint.ordinal);
    }

    // 7. Fill the LIR image header in the contiguous buffer.
    try expectEqual(@as(usize, 0), lowered.lir_result.static_data_values.items.len);
    const image_header = try runtime_alloc.create(lir.LirImage.Header);
    try lir.LirImage.fillHeaderInBuffer(
        image_header,
        runtime_buffer.ptr,
        runtime_fba.end_index,
        &lowered.lir_result,
        entrypoints,
    );

    // 8. View the image for the width it was lowered for. The image is
    //    pointer-width independent, so the consumer supplies the target;
    //    viewMappedImage accepts `[*]align(1) const u8` so no @constCast is
    //    needed on the buffer pointer.
    var view = try lir.LirImage.viewMappedImage(
        image_header,
        runtime_buffer.ptr,
        runtime_fba.end_index,
        lowered.target_usize,
    );
    defer view.deinit();
    try expectEqual(entrypoints.len, view.platform_entrypoints.len);
    for (entrypoints, view.platform_entrypoints) |expected, actual| {
        try expectEqual(expected.ordinal, actual.ordinal);
        try expectEqual(expected.root_proc, actual.root_proc);
    }

    // 9. Wire RocOps with empty hosted_fns (the scalar fixture has no hosted-fn
    //    calls). For platforms with hosted functions, the dispatch index
    //    rule applies—see README "Host functions".
    var hosted_fns = host_abi.emptyHostedFunctions();
    var environment: u8 = 0;
    var roc_ops = host_abi.RocOps{
        .env = &environment, // unused by this scalar fixture
        .roc_alloc = &testRocAlloc,
        .roc_dealloc = &testRocDealloc,
        .roc_realloc = &testRocRealloc,
        .roc_dbg = &testRocDbg,
        .roc_expect_failed = &testRocExpectFailed,
        .roc_crashed = &testRocCrashed,
        .hosted_fns = hosted_fns,
    };
    _ = &hosted_fns;

    // 10. Initialize the interpreter and run the entrypoint.
    var static_strings = try eval.LirInterpreter.buildStaticStrings(gpa, &view.store);
    defer static_strings.deinit();
    var interp = try eval.LirInterpreter.initWithBoxyTables(
        gpa,
        &view.store,
        &view.layouts,
        eval.LirInterpreter.BoxyTables.fromImageView(&view),
        static_strings.view(),
        &roc_ops,
    );
    defer interp.deinit();

    var result: u64 = undefined;
    _ = try interp.runEntrypoint(&view, 0, null, @ptrCast(&result));
    try expectEqual(@as(u64, 42), result);
}

fn expect(ok: bool) error{UnexpectedResult}!void {
    if (!ok) return error.UnexpectedResult;
}
fn expectEqual(expected: anytype, actual: @TypeOf(expected)) error{UnexpectedResult}!void {
    try expect(expected == actual);
}
fn expectEqualStrings(expected: []const u8, actual: []const u8) error{UnexpectedResult}!void {
    try expect(std.mem.eql(u8, expected, actual));
}
