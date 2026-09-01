//! Shared test harness: compile an app and lower it all the way to LIR,
//! asserting no checker errors and no ARC borrow-certifier violation. The
//! certifier runs inside `lowerCheckedModulesToLir` and panics on any
//! violation, so returning normally is the assertion.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const check = @import("check");
const collections = @import("collections");
const eval = @import("eval");
const layout = @import("layout");
const lir = @import("lir");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

var shared_test_builtins: ?eval.BuiltinModules = null;
var shared_test_builtins_mutex: std.Io.Mutex = .init;

fn sharedBuiltinModules() eval.BuiltinModules.InitError!*eval.BuiltinModules {
    shared_test_builtins_mutex.lockUncancelable(std.testing.io);
    defer shared_test_builtins_mutex.unlock(std.testing.io);

    if (shared_test_builtins == null) {
        shared_test_builtins = try eval.BuiltinModules.init(std.heap.page_allocator);
    }

    return &shared_test_builtins.?;
}

/// Error set shared by LIR-lowering harness helpers and focused inspectors.
pub const LowerToLirHarnessError = std.mem.Allocator.Error ||
    lir.CheckedPipeline.LowerResourceError ||
    std.Io.Dir.CreateDirPathError ||
    std.Io.Dir.RealPathFileAllocError ||
    std.Io.Dir.WriteFileError ||
    Coordinator.AppDiscoveryError ||
    check.CheckedArtifact.CompileTimeFinalizer.Error ||
    eval.BuiltinModules.InitError ||
    std.Thread.SpawnError ||
    error{
        BuiltinLowLevelAnnotationMustBeFunction,
        DownloadFailed,
        ExpectedPlatformString,
        ExpectedString,
        FileError,
        FileNotFound,
        Internal,
        InvalidDependency,
        InvalidNullByteInPath,
        InvalidUrl,
        LowLevelOperationsNotFound,
        NoCacheDir,
        NoPackageSource,
        PathOutsideWorkspace,
        TestExpectedEqual,
        TestUnexpectedResult,
        UnsupportedBuiltinAnnotationOnly,
        UnsupportedHeader,
        WriteFailed,
        Issue806UnsafeLargeStackStructAssign,
        Issue806UnsafeLargeStackTagAssign,
        Issue806UnsafeLargeStackSetLocalCopy,
        Issue806UnsafeLargeStackCallReturn,
        Issue806UnsafeLargeStackCallArgument,
        Issue806UnsafeLargeStackReturn,
        Issue806UnsafeLargeStackJoinParam,
        Issue806UnsafeLargeStackClosureCapture,
        Issue806UnsafeLargeStackPatternPayload,
        Issue806MissingStackProbe,
    };

/// Callback type for tests that inspect the lowered LIR store directly.
pub const LirInspectFn = *const fn (
    store: *const lir.LirStore,
    layouts: *const layout.Store,
) LowerToLirHarnessError!void;

/// Callback type for tests that inspect the whole lowered program. Backend
/// codegen needs the boxy side tables alongside the store and layout store,
/// so those tests take the lowering result rather than the two stores.
pub const LoweredInspectFn = *const fn (
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) LowerToLirHarnessError!void;

/// Options controlling how the harness lowers an app to LIR.
pub const LirLoweringOptions = struct {
    specialization_strategy: base.SpecializationStrategy = .lss,
    /// Number of coordinator workers available to post-check lowering.
    /// The default retains the harness's existing single-threaded behavior.
    specialization_workers: usize = 1,
    /// Add a second platform-required procedure so root lowering has a parallel
    /// batch rather than only the ordinary single-entrypoint workload.
    parallel_procedure_root_fixture: bool = false,
    target_usize: base.target.TargetUsize = base.target.TargetUsize.native,
    inline_mode: lir.CheckedPipeline.InlineMode = .none,
    consume_dead_boxes: bool = false,
    list_in_place_map: bool = false,
    proc_debug_names: bool = false,
    prove_ranges: bool = false,
    allow_user_errors: bool = false,
    /// Receives the expression count of the lifted program handed to lambda-set
    /// solving, for tests that assert on post-check program growth.
    lifted_expr_count_out: ?*usize = null,
};

/// Lower an app whose body is `app_body` (everything after the platform header
/// and the echo wiring) to LIR. Reaching the end without a panic means the
/// program checked cleanly and passed ARC certification.
pub fn expectLowersToLir(app_body: []const u8) LowerToLirHarnessError!void {
    try runToLir(app_body, null, .{}, null);
}

/// Lower an app whose body is `app_body` to LIR with explicit lowering
/// options. Reaching the end without a panic means the program checked cleanly
/// and passed ARC certification.
pub fn expectLowersToLirWithOptions(app_body: []const u8, opts: LirLoweringOptions) LowerToLirHarnessError!void {
    try runToLir(app_body, null, opts, null);
}

/// Lower an app at `app_path` to LIR. Reaching the end without a panic means
/// the app checked cleanly and passed ARC certification.
pub fn expectAppPathLowersToLir(app_path: []const u8) LowerToLirHarnessError!void {
    try lowerAppPathToLir(std.testing.allocator, app_path, null, .{}, null, null);
}

/// Lower an app at `app_path` to LIR, then run a focused invariant check
/// against the actual lowered store and layout store.
pub fn expectAppPathLirInspection(app_path: []const u8, inspect: LirInspectFn) LowerToLirHarnessError!void {
    try lowerAppPathToLir(std.testing.allocator, app_path, null, .{}, inspect, null);
}

/// Lower an app at `app_path` to LIR with explicit lowering options, then run
/// a focused invariant check against the whole lowered program, for tests that
/// drive a backend over the result.
pub fn runAppPathLoweredInspection(
    app_path: []const u8,
    opts: LirLoweringOptions,
    inspect: LoweredInspectFn,
) LowerToLirHarnessError!void {
    try lowerAppPathToLir(std.testing.allocator, app_path, null, opts, null, inspect);
}

/// Lower an app at `app_path` to LIR with explicit lowering options, then run
/// a focused invariant check against the actual lowered store and layout store.
pub fn runAppPathLirInspection(app_path: []const u8, opts: LirLoweringOptions, inspect: LirInspectFn) LowerToLirHarnessError!void {
    try lowerAppPathToLir(std.testing.allocator, app_path, null, opts, inspect, null);
}

/// Lower an app whose body is `app_body` to LIR, then run a focused invariant
/// check against the actual lowered store and layout store.
pub fn expectLirInspection(app_body: []const u8, inspect: LirInspectFn) LowerToLirHarnessError!void {
    try runToLir(app_body, null, .{}, inspect);
}

/// Lower an app whose body is `app_body` to LIR with explicit lowering
/// options, then run a focused invariant check against the actual lowered
/// store and layout store.
pub fn expectLirInspectionWithOptions(app_body: []const u8, opts: LirLoweringOptions, inspect: LirInspectFn) LowerToLirHarnessError!void {
    try runToLir(app_body, null, opts, inspect);
}

/// Lower `app_body` twice and assert the two LIR dumps are byte-identical, so
/// a regression that made lowering (e.g. capture order) depend on iteration or
/// scheduling order would fail here rather than silently.
pub fn expectDeterministicLir(app_body: []const u8) LowerToLirHarnessError!void {
    const gpa = std.testing.allocator;
    const cap = 1 << 22;
    const buf_a = try gpa.alloc(u8, cap);
    defer gpa.free(buf_a);
    const buf_b = try gpa.alloc(u8, cap);
    defer gpa.free(buf_b);
    var writer_a = std.Io.Writer.fixed(buf_a);
    var writer_b = std.Io.Writer.fixed(buf_b);
    try runToLir(app_body, &writer_a, .{}, null);
    try runToLir(app_body, &writer_b, .{}, null);
    try std.testing.expectEqualStrings(writer_a.buffered(), writer_b.buffered());
}

/// Lower `app_body` with one, two, and four post-check workers, comparing
/// each complete LIR dump with the single-worker result. Run each parallel
/// configuration twice so this checks both worker-count independence and
/// repeated scheduling independence without relying on timing.
pub fn expectSpecializationParallelismDeterministicLir(app_body: []const u8) LowerToLirHarnessError!void {
    try expectPostCheckParallelismDeterministicLir(app_body, false);
}

/// Lower an app with two independent platform-required procedure roots and
/// compare complete LIR output across one, two, and four post-check workers.
pub fn expectProcedureRootParallelismDeterministicLir(app_body: []const u8) LowerToLirHarnessError!void {
    try expectPostCheckParallelismDeterministicLir(app_body, true);
}

fn expectPostCheckParallelismDeterministicLir(
    app_body: []const u8,
    parallel_procedure_root_fixture: bool,
) LowerToLirHarnessError!void {
    const gpa = std.testing.allocator;
    const cap = 1 << 22;
    const reference = try gpa.alloc(u8, cap);
    defer gpa.free(reference);
    var reference_writer = std.Io.Writer.fixed(reference);
    try runToLir(app_body, &reference_writer, .{
        .specialization_workers = 1,
        .parallel_procedure_root_fixture = parallel_procedure_root_fixture,
    }, null);

    for ([_]usize{ 2, 4 }) |specialization_workers| {
        for (0..2) |_| {
            const candidate = try gpa.alloc(u8, cap);
            defer gpa.free(candidate);
            var candidate_writer = std.Io.Writer.fixed(candidate);
            try runToLir(app_body, &candidate_writer, .{
                .specialization_workers = specialization_workers,
                .parallel_procedure_root_fixture = parallel_procedure_root_fixture,
            }, null);
            try std.testing.expectEqualStrings(reference_writer.buffered(), candidate_writer.buffered());
        }
    }
}

/// Lower `app_body` for both pointer widths (with in-place `List.map` reuse
/// enabled) and assert the two LIR dumps are byte-identical. This guards that
/// lowering produces a target-independent op stream—the property that lets a
/// single lowered LIR image be cached across 32-bit and 64-bit targets. A
/// regression that reintroduced a pointer-width-dependent lowering decision
/// (for example, baking the `list_map_can_reuse` interchangeability check for
/// one width instead of carrying both) would make the dumps diverge and fail
/// here.
pub fn expectTargetIndependentLir(app_body: []const u8) LowerToLirHarnessError!void {
    const gpa = std.testing.allocator;
    const cap = 1 << 22;
    const buf_a = try gpa.alloc(u8, cap);
    defer gpa.free(buf_a);
    const buf_b = try gpa.alloc(u8, cap);
    defer gpa.free(buf_b);
    var writer_a = std.Io.Writer.fixed(buf_a);
    var writer_b = std.Io.Writer.fixed(buf_b);
    try runToLir(app_body, &writer_a, .{ .target_usize = .u32, .list_in_place_map = true }, null);
    try runToLir(app_body, &writer_b, .{ .target_usize = .u64, .list_in_place_map = true }, null);
    try std.testing.expectEqualStrings(writer_a.buffered(), writer_b.buffered());
}

fn runToLir(
    app_body: []const u8,
    dump: ?*std.Io.Writer,
    opts: LirLoweringOptions,
    inspect: ?LirInspectFn,
) LowerToLirHarnessError!void {
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.createDirPath(std.testing.io, ".roc_echo_platform");
    const app_exports = if (opts.parallel_procedure_root_fixture) "main!, auxiliary!" else "main!";
    const auxiliary_source = if (opts.parallel_procedure_root_fixture)
        "\nauxiliary! = |msg| Echo.line!(msg)\n"
    else
        "";
    const synthetic_source = try std.fmt.allocPrint(
        gpa,
        "app [{s}] {{ pf: platform \"./.roc_echo_platform/main.roc\" }}\n\n" ++
            "import pf.Echo\n\n" ++
            "echo! = |msg| Echo.line!(msg)\n\n" ++
            "{s}\n" ++
            "{s}",
        .{ app_exports, auxiliary_source, app_body },
    );
    defer gpa.free(synthetic_source);
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data = synthetic_source,
    });
    const platform_source: []const u8 = if (opts.parallel_procedure_root_fixture)
        \\platform ""
        \\    requires {} {
        \\        main! : List(Str) => Try({}, [Exit(I8), ..]),
        \\        auxiliary! : Str => {},
        \\    }
        \\    exposes [Echo]
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\    hosted { "roc_echo_line": Echo.line! }
        \\
        \\import Echo
        \\
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |args| {
        \\    auxiliary!("starting")
        \\    match main!(args) {
        \\        Ok({}) => 0
        \\        Err(Exit(code)) => code
        \\        Err(other) => {
        \\            Echo.line!("Program exited with error: ${Str.inspect(other)}")
        \\            1
        \\        }
        \\    }
        \\}
    else
        \\platform ""
        \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
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
        \\        Ok({}) => 0
        \\        Err(Exit(code)) => code
        \\        Err(other) => {
        \\            Echo.line!("Program exited with error: ${Str.inspect(other)}")
        \\            1
        \\        }
        \\    }
    ;
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = ".roc_echo_platform/main.roc",
        .data = platform_source,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = ".roc_echo_platform/Echo.roc",
        .data =
        \\Echo := [].{
        \\    line! : Str => {}
        \\}
        ,
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.roc", gpa);
    defer gpa.free(app_path);

    try lowerAppPathToLir(gpa, app_path, dump, opts, inspect, null);
}

fn lowerAppPathToLir(
    gpa: std.mem.Allocator,
    app_path: []const u8,
    dump: ?*std.Io.Writer,
    opts: LirLoweringOptions,
    inspect: ?LirInspectFn,
    inspect_lowered: ?LoweredInspectFn,
) LowerToLirHarnessError!void {
    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const builtin_modules = try sharedBuiltinModules();

    var coord = try Coordinator.init(
        gpa,
        if (opts.specialization_workers > 1) .multi_threaded else .single_threaded,
        opts.specialization_workers,
        roc_target.RocTarget.detectNative(),
        builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.default(gpa, arena, std.testing.io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    if (!opts.allow_user_errors) {
        try std.testing.expect(!coord.hasUserErrors());
    }

    try coord.finalizeExecutableArtifacts();
    if (!opts.allow_user_errors) {
        try std.testing.expect(!coord.hasUserErrors());
    }

    const root = coord.executableRootCheckedArtifact();
    const imports = try coord.collectImportedArtifactViews(arena, root);
    const relations = try coord.collectRelationArtifactViews(arena, root);

    const lir_roots = try lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root.root_requests.runtime_requests);
    defer gpa.free(lir_roots);
    if (opts.parallel_procedure_root_fixture) {
        var procedure_use_roots: usize = 0;
        for (lir_roots) |request| {
            if (request.procedure_use != null) procedure_use_roots += 1;
        }
        try std.testing.expectEqual(@as(usize, 2), procedure_use_roots);
    }

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        gpa,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root, relations),
            .imports = imports,
        },
        .{ .requests = lir_roots },
        .{
            .specialization_strategy = opts.specialization_strategy,
            .target_usize = opts.target_usize,
            .inline_mode = opts.inline_mode,
            .consume_dead_boxes = opts.consume_dead_boxes,
            .list_in_place_map = opts.list_in_place_map,
            .proc_debug_names = opts.proc_debug_names,
            .prove_ranges = opts.prove_ranges,
            .lifted_expr_count_out = opts.lifted_expr_count_out,
            .post_check_executor = if (opts.specialization_workers > 1)
                coord.postCheckExecutor()
            else
                null,
        },
    );
    defer lowered.deinit();

    if (dump) |writer| {
        const store = &lowered.lir_result.store;
        const layouts = &lowered.lir_result.layouts;
        for (0..store.getProcSpecs().len) |index| {
            try lir.DebugPrint.writeProc(gpa, store, layouts, @enumFromInt(@as(u32, @intCast(index))), writer);
        }
    }

    if (inspect) |inspect_fn| {
        try inspect_fn(&lowered.lir_result.store, &lowered.lir_result.layouts);
    }

    if (inspect_lowered) |inspect_fn| {
        try inspect_fn(&lowered);
    }
}
