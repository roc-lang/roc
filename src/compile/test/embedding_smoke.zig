//! End-to-end smoke test for the public embedding API.
//!
//! Drives a minimal compile + execute through every method an embedder is
//! expected to call (`Coordinator.discoverAppFromPath` → `coordinatorLoop`
//! → `iterReports` → `finalizeExecutableArtifacts` → `lowerCheckedModulesToLir`
//! → `LoweredProgram.platformEntrypoints` → `LirImage.fillHeaderInBuffer`
//! → `LirImage.viewMappedImage` → `LirInterpreter.runEntrypoint`).
//!
//! If this test breaks, the public surface changed in a way that will break
//! external embedders. The test itself is documentation: the canonical
//! sequence shown in `src/compile/README.md` is the structure here.

const std = @import("std");
const build_options = @import("build_options");
const lir = @import("lir");
const eval = @import("eval");
const check = @import("check");
const base = @import("base");
const collections = @import("collections");
const host_abi = @import("builtins").host_abi;
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

// Allocator callbacks for the test's RocOps. The simple_success.roc app's
// `main!` is empty, so these are unlikely to fire — but they must be valid
// function pointers.
fn testRocAlloc(_: *host_abi.RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const align_enum = std.mem.Alignment.fromByteUnits(@max(alignment, @alignOf(usize)));
    const raw = base.defaultGpa().rawAlloc(length, align_enum, @returnAddress()) orelse {
        std.debug.panic("embedding smoke test roc_alloc OOM", .{});
    };
    return @ptrCast(raw);
}

fn testRocDealloc(_: *host_abi.RocOps, _: *anyopaque, _: usize) callconv(.c) void {
    // No-op for the smoke test — pages are reclaimed at exit.
}

fn testRocRealloc(_: *host_abi.RocOps, _: *anyopaque, _: usize, _: usize) callconv(.c) ?*anyopaque {
    std.debug.panic("embedding smoke test roc_realloc unexpected", .{});
}

fn testRocDbg(_: *host_abi.RocOps, _: [*]const u8, _: usize) callconv(.c) void {}
fn testRocExpectFailed(_: *host_abi.RocOps, _: [*]const u8, _: usize) callconv(.c) void {}
fn testRocCrashed(_: *host_abi.RocOps, _: [*]const u8, _: usize) callconv(.c) void {
    std.debug.panic("embedding smoke test received roc_crashed", .{});
}

test "embedding API: full canonical sequence on simple_success app" {
    // Path is resolved relative to the cwd at test time, which is the repo
    // root for `zig build run-test-zig-module-compile`.
    const app_path = "test/cli/simple_success.roc";

    const gpa = std.testing.allocator;
    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    // 1. Builtins + Coordinator.
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
        CoreCtx.default(gpa, arena, std.testing.io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    try coord.start();

    // 2. Discover + compile.
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();

    // 3. iterReports must walk without crashing; we expect no errors for
    //    simple_success.roc.
    var report_iter = coord.iterReports();
    while (report_iter.next()) |entry| {
        try std.testing.expect(entry.report.severity != .fatal);
        try std.testing.expect(entry.report.severity != .runtime_error);
    }
    try std.testing.expect(!coord.hasUserErrors());

    // 4. Finalize must succeed (no HasUserErrors).
    try coord.finalizeExecutableArtifacts();
    try std.testing.expect(!coord.hasUserErrors());

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

    // 6. Build entrypoint list. simple_success has a platform-provided main!
    //    so at least one entrypoint must exist.
    const entrypoints = try lowered.platformEntrypoints(runtime_alloc);
    const entrypoint_names = try lowered.platformEntrypointNames(arena, root);
    try std.testing.expect(entrypoints.len > 0);
    try std.testing.expectEqual(entrypoints.len, entrypoint_names.len);

    // 7. Fill the LIR image header in the contiguous buffer.
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
    const view = try lir.LirImage.viewMappedImage(
        image_header,
        runtime_buffer.ptr,
        runtime_fba.end_index,
        lowered.target_usize,
    );

    // 9. Wire RocOps with empty hosted_fns (simple_success has no hosted-fn
    //    calls). For platforms with hosted functions, the dispatch index
    //    rule applies — see README "Host functions".
    var hosted_fns = host_abi.emptyHostedFunctions();
    var roc_ops = host_abi.RocOps{
        .env = @ptrFromInt(0xdeadbeef), // unused by simple_success
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
    var interp = try eval.LirInterpreter.init(gpa, &view.store, &view.layouts, &roc_ops);
    defer interp.deinit();

    // simple_success has main! : () => {} — no args, returns unit.
    var ret_buf: [16]u8 align(16) = undefined;
    _ = try interp.runEntrypoint(&view, 0, null, @ptrCast(&ret_buf));
}
