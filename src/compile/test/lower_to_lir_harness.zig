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
const postcheck = @import("postcheck");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

const ReverseCompletionExecutor = struct {
    inner: base.post_check_task_executor.Executor,

    fn run(
        context: *anyopaque,
        tasks: []const base.post_check_task_executor.Task,
        completions: []base.post_check_task_executor.Completion,
    ) std.mem.Allocator.Error!void {
        const self: *ReverseCompletionExecutor = @ptrCast(@alignCast(context));
        try self.inner.run(tasks, completions);
        std.mem.reverse(base.post_check_task_executor.Completion, completions);
    }

    fn executor(self: *ReverseCompletionExecutor) base.post_check_task_executor.Executor {
        return .{
            .context = self,
            .worker_count = self.inner.worker_count,
            .runFn = ReverseCompletionExecutor.run,
        };
    }
};

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
    /// Require four app procedures that each call a distinct capture-free direct
    /// callee, so their worker-discovered callees form a second body-shard wave.
    prepared_direct_call_root_fixture: bool = false,
    target_usize: base.target.TargetUsize = base.target.TargetUsize.native,
    inline_mode: lir.CheckedPipeline.InlineMode = .none,
    spec_constr_clone_inlining: lir.CheckedPipeline.SpecConstrCloneInlining = .all_calls,
    consume_dead_boxes: bool = false,
    list_in_place_map: bool = false,
    proc_debug_names: bool = false,
    prove_ranges: bool = false,
    allow_user_errors: bool = false,
    /// Receives the expression count of the lifted program handed to lambda-set
    /// solving, for tests that assert on post-check program growth.
    lifted_expr_count_out: ?*usize = null,
    /// Receives the complete checked-to-LIR timing snapshot after lowering.
    timing_out: ?*lir.CheckedPipeline.TimingSnapshot = null,
    /// Receives deterministic solved-LIR body-shard task counts.
    solved_lir_parallel_metrics_out: ?*lir.CheckedPipeline.SolvedLirParallelMetrics = null,
    /// Deliver post-check completions in reverse order after callbacks finish.
    reverse_post_check_completions: bool = false,
    /// Stop after Monotype lowering. Focused postcheck regressions use this
    /// boundary when later LIR passes are outside the behavior under test.
    monotype_only: bool = false,
    /// Receives deterministic Monotype work counters. This is independent of
    /// elapsed-time measurement and is available at the `monotype_only` boundary.
    monotype_diagnostics_out: ?*postcheck.Monotype.Lower.Diagnostics = null,
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

/// Lower an app at `app_path` to LIR with explicit lowering options. Reaching
/// the end without a panic is the assertion, so `allow_user_errors` programs
/// use this to pin that a rejected app still lowers to a checked crash.
pub fn expectAppPathLowersToLirWithOptions(app_path: []const u8, opts: LirLoweringOptions) LowerToLirHarnessError!void {
    try lowerAppPathToLir(std.testing.allocator, app_path, null, opts, null, null);
}

/// Lower an app at `app_path` through Monotype specialization, without running
/// later LIR transforms or ARC insertion.
pub fn expectAppPathLowersToMonotype(app_path: []const u8) LowerToLirHarnessError!void {
    try lowerAppPathToLir(std.testing.allocator, app_path, null, .{ .monotype_only = true }, null, null);
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

/// Four independent, finite capture-free direct calls. Each required procedure
/// discovers its distinct direct callee, making a later worker wave
/// observable without depending on execution timing.
pub const prepared_finite_capture_free_direct_call_fixture =
    \\leaf_a : I64 -> I64
    \\leaf_a = |value| value
    \\
    \\leaf_b : I64 -> I64
    \\leaf_b = |value| value
    \\
    \\leaf_c : I64 -> I64
    \\leaf_c = |value| value
    \\
    \\leaf_d : I64 -> I64
    \\leaf_d = |value| value
    \\
    \\auxiliary_a! = |value| leaf_a(value)
    \\
    \\auxiliary_b! = |value| leaf_b(value)
    \\
    \\auxiliary_c! = |value| leaf_c(value)
    \\
    \\auxiliary_d! = |value| leaf_d(value)
    \\
    \\main! = |_args| {
    \\    Ok({})
    \\}
;

/// Assert that prepared finite capture-free direct calls lower identically
/// serially, in two and four worker lanes, and when worker completions are
/// committed in reverse order. The direct callees discovered by the first
/// eligible epoch form a later epoch; every submitted shard commits without a
/// retry.
pub fn expectPreparedFiniteCaptureFreeDirectCallsParallelismDeterministicLir() LowerToLirHarnessError!void {
    const gpa = std.testing.allocator;
    const cap = 1 << 22;
    const reference = try gpa.alloc(u8, cap);
    defer gpa.free(reference);
    var reference_writer = std.Io.Writer.fixed(reference);
    var serial_metrics: lir.CheckedPipeline.SolvedLirParallelMetrics = .{
        .task_waves = 11,
        .tasks_submitted = 22,
        .tasks_committed = 33,
        .tasks_retried_serial = 44,
    };
    var serial_timing: lir.CheckedPipeline.TimingSnapshot = .{};
    try runToLir(prepared_finite_capture_free_direct_call_fixture, &reference_writer, .{
        .specialization_workers = 1,
        .prepared_direct_call_root_fixture = true,
        .solved_lir_parallel_metrics_out = &serial_metrics,
        .timing_out = &serial_timing,
    }, null);
    try std.testing.expectEqual(@as(u64, 0), serial_metrics.task_waves);
    try std.testing.expectEqual(@as(u64, 0), serial_metrics.tasks_submitted);
    try std.testing.expectEqual(@as(u64, 0), serial_metrics.tasks_committed);
    try std.testing.expectEqual(@as(u64, 0), serial_metrics.tasks_retried_serial);
    try std.testing.expectEqual(@as(u64, 0), serial_metrics.workspace_initializations);
    try std.testing.expectEqual(@as(u64, 0), serial_metrics.workspace_reuses);
    const serial_parallel = serial_timing.monotype_parallel;
    try std.testing.expectEqual(@as(u64, 0), serial_parallel.root_tasks_submitted);
    try std.testing.expectEqual(@as(u64, 0), serial_parallel.root_tasks_committed);
    try std.testing.expectEqual(@as(u64, 0), serial_parallel.specialization_tasks_submitted);
    try std.testing.expectEqual(@as(u64, 0), serial_parallel.specialization_tasks_committed);
    try std.testing.expectEqual(@as(u64, 0), serial_parallel.task_waves);
    try std.testing.expectEqual(@as(u64, 0), serial_parallel.within_lowering_lane_reuse_tasks);

    for ([_]struct {
        specialization_workers: usize,
        solved_lir_task_waves: u64,
        monotype_task_waves: u64,
    }{
        .{ .specialization_workers = 2, .solved_lir_task_waves = 7, .monotype_task_waves = 5 },
        .{ .specialization_workers = 4, .solved_lir_task_waves = 4, .monotype_task_waves = 4 },
    }) |case| {
        for ([_]bool{ false, true }) |reverse_post_check_completions| {
            const candidate = try gpa.alloc(u8, cap);
            defer gpa.free(candidate);
            var candidate_writer = std.Io.Writer.fixed(candidate);
            var metrics: lir.CheckedPipeline.SolvedLirParallelMetrics = .{};
            var timing: lir.CheckedPipeline.TimingSnapshot = .{};
            try runToLir(prepared_finite_capture_free_direct_call_fixture, &candidate_writer, .{
                .specialization_workers = case.specialization_workers,
                .prepared_direct_call_root_fixture = true,
                .reverse_post_check_completions = reverse_post_check_completions,
                .solved_lir_parallel_metrics_out = &metrics,
                .timing_out = &timing,
            }, null);

            try std.testing.expectEqualStrings(reference_writer.buffered(), candidate_writer.buffered());
            try std.testing.expectEqual(case.solved_lir_task_waves, metrics.task_waves);
            try std.testing.expectEqual(@as(u64, 14), metrics.tasks_submitted);
            try std.testing.expectEqual(@as(u64, 14), metrics.tasks_committed);
            try std.testing.expectEqual(@as(u64, 0), metrics.tasks_retried_serial);
            try std.testing.expectEqual(
                metrics.tasks_submitted,
                metrics.workspace_initializations + metrics.workspace_reuses,
            );
            try std.testing.expect(metrics.workspace_initializations > 0);
            try std.testing.expect(metrics.workspace_reuses > 0);

            const parallel = timing.monotype_parallel;
            try std.testing.expectEqual(@as(u64, 5), parallel.root_tasks_submitted);
            try std.testing.expectEqual(parallel.root_tasks_submitted, parallel.root_tasks_committed);
            try std.testing.expectEqual(@as(u64, 0), parallel.root_tasks_retried_serial);
            try std.testing.expectEqual(@as(u64, 10), parallel.specialization_tasks_submitted);
            try std.testing.expect(
                parallel.specialization_tasks_submitted > parallel.peak_worker_lanes_available,
            );
            try std.testing.expectEqual(
                parallel.specialization_tasks_submitted,
                parallel.specialization_tasks_committed,
            );
            try std.testing.expectEqual(@as(u64, 0), parallel.specialization_tasks_retried_serial);
            try std.testing.expectEqual(@as(u64, 0), parallel.specialization_tasks_discarded_ready);
            // Ten specializations complete in two specialization waves after
            // the fixed root waves, proving each run can exceed lane count.
            try std.testing.expectEqual(case.monotype_task_waves, parallel.task_waves);
            try std.testing.expect(parallel.within_lowering_lane_reuse_tasks > 0);
        }
    }
}

fn expectNamedWorkerLocalCommitted(
    store: *const lir.LirStore,
    _: *const layout.Store,
) LowerToLirHarnessError!void {
    var matching_names: usize = 0;
    for (0..store.localCount()) |index| {
        const name = store.localName(@enumFromInt(@as(u32, @intCast(index)))) orelse continue;
        if (std.mem.eql(u8, name, "named_local")) matching_names += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), matching_names);
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
        for (0..2) |attempt| {
            const candidate = try gpa.alloc(u8, cap);
            defer gpa.free(candidate);
            var candidate_writer = std.Io.Writer.fixed(candidate);
            var timing: lir.CheckedPipeline.TimingSnapshot = .{};
            var solved_lir_parallel: lir.CheckedPipeline.SolvedLirParallelMetrics = .{};
            try runToLir(app_body, &candidate_writer, .{
                .specialization_workers = specialization_workers,
                .parallel_procedure_root_fixture = parallel_procedure_root_fixture,
                .timing_out = &timing,
                .solved_lir_parallel_metrics_out = &solved_lir_parallel,
                .reverse_post_check_completions = attempt == 1,
            }, if (parallel_procedure_root_fixture) expectNamedWorkerLocalCommitted else null);
            try std.testing.expectEqualStrings(reference_writer.buffered(), candidate_writer.buffered());
            if (parallel_procedure_root_fixture) {
                const parallel = timing.monotype_parallel;
                try std.testing.expectEqual(@as(u64, 2), parallel.root_tasks_submitted);
                try std.testing.expectEqual(@as(u64, 2), parallel.root_tasks_committed);
                try std.testing.expectEqual(@as(u64, 0), parallel.root_tasks_retried_serial);
                try std.testing.expectEqual(
                    @as(u64, @intCast(specialization_workers)),
                    parallel.peak_worker_lanes_available,
                );
                try std.testing.expect(parallel.peak_worker_lanes_used > 0);
                try std.testing.expect(parallel.peak_worker_lanes_used <= parallel.peak_worker_lanes_available);
                try std.testing.expect(solved_lir_parallel.task_waves > 0);
                try std.testing.expect(solved_lir_parallel.tasks_submitted >= 2);
                try std.testing.expect(solved_lir_parallel.tasks_committed > 0);
                try std.testing.expectEqual(
                    solved_lir_parallel.tasks_submitted,
                    solved_lir_parallel.tasks_committed + solved_lir_parallel.tasks_retried_serial,
                );
            }
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
    const app_exports = if (opts.prepared_direct_call_root_fixture)
        "main!, auxiliary_a!, auxiliary_b!, auxiliary_c!, auxiliary_d!"
    else if (opts.parallel_procedure_root_fixture)
        "main!, auxiliary!"
    else
        "main!";
    const auxiliary_source = if (opts.parallel_procedure_root_fixture)
        "\nauxiliary! = |arg| {\n    named_local = arg\n    named_local\n}\n"
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
    const platform_source: []const u8 = if (opts.prepared_direct_call_root_fixture)
        \\platform ""
        \\    requires {} {
        \\        main! : List(Str) => Try({}, [Exit(I8), ..]),
        \\        auxiliary_a! : I64 => I64,
        \\        auxiliary_b! : I64 => I64,
        \\        auxiliary_c! : I64 => I64,
        \\        auxiliary_d! : I64 => I64,
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
        \\    _a = auxiliary_a!(0)
        \\    _b = auxiliary_b!(0)
        \\    _c = auxiliary_c!(0)
        \\    _d = auxiliary_d!(0)
        \\    match main!(args) {
        \\        Ok({}) => 0
        \\        Err(Exit(code)) => code
        \\        Err(other) => {
        \\            Echo.line!("Program exited with error: ${Str.inspect(other)}")
        \\            1
        \\        }
        \\    }
        \\}
    else if (opts.parallel_procedure_root_fixture)
        \\platform ""
        \\    requires {} {
        \\        main! : List(Str) => Try({}, [Exit(I8), ..]),
        \\        auxiliary! : I64 => I64,
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
        \\    _auxiliary_result = auxiliary!(0)
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

    if (opts.monotype_only) {
        var diagnostics: postcheck.Monotype.Lower.Diagnostics = .{};
        var mono = try postcheck.Monotype.Lower.run(
            gpa,
            .{
                .root = check.CheckedArtifact.loweringViewWithRelations(root, relations),
                .imports = imports,
            },
            .{ .requests = lir_roots },
            .{ .diagnostics = if (opts.monotype_diagnostics_out != null) &diagnostics else null },
        );
        mono.deinit();
        if (opts.monotype_diagnostics_out) |out| out.* = diagnostics;
        return;
    }

    var timing = lir.CheckedPipeline.Timing.init(std.testing.io);
    const coordinator_executor = if (opts.specialization_workers > 1)
        coord.postCheckExecutor()
    else
        null;
    var reverse_executor = if (coordinator_executor) |executor|
        ReverseCompletionExecutor{ .inner = executor }
    else
        undefined;
    const post_check_executor = if (coordinator_executor) |executor|
        if (opts.reverse_post_check_completions) reverse_executor.executor() else executor
    else
        null;
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
            .spec_constr_clone_inlining = opts.spec_constr_clone_inlining,
            .consume_dead_boxes = opts.consume_dead_boxes,
            .list_in_place_map = opts.list_in_place_map,
            .proc_debug_names = opts.proc_debug_names,
            .prove_ranges = opts.prove_ranges,
            .lifted_expr_count_out = opts.lifted_expr_count_out,
            .post_check_executor = post_check_executor,
            .solved_lir_parallel_metrics_out = opts.solved_lir_parallel_metrics_out,
            .timing = if (opts.timing_out != null) &timing else null,
        },
    );
    defer lowered.deinit();
    if (opts.timing_out) |timing_out| timing_out.* = timing.snapshot();

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
