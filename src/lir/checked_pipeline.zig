//! Public checked-module-to-LIR lowering API.
//!
//! This is the only public lowering entrance after checking. It consumes
//! complete checked modules, explicit root requests, and target configuration.
//! It returns LIR or resource failure.

const std = @import("std");
const collections = @import("collections");
const builtin = @import("builtin");
const base = @import("base");
const check = @import("check");
const core = @import("lir_core");

const Arc = @import("arc.zig");
const ImmortalLocals = @import("immortal_locals.zig");
const ProcPasses = @import("proc_passes.zig");
const ReturnSlot = @import("return_slot.zig");
const StrAppend = @import("str_append.zig");
const SingleUseInline = @import("single_use_inline.zig");

/// Completed compile-time scalar roots a forked continuation lowers as literals.
pub const CompletedScalarValues = postcheck.ComptimeScalarValues.CompletedScalarValues;
const TagReachability = @import("tag_reachability.zig");
const ReachableProcs = @import("reachable_procs.zig");
const DebugPrint = @import("debug_print.zig");
const LIR = core.LIR;
const CheckedArithmetic = core.CheckedArithmetic;
const LirImage = @import("lir_image.zig");
const LirProgram = core.Program;
const postcheck = @import("postcheck");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;

/// Committed ordinary-data layouts for checked source ABI requests.
pub const CheckedAbiLayouts = struct {
    allocator: Allocator,
    layouts: @import("layout").Store,
    roots: []const @import("layout").Idx,

    pub fn deinit(self: *CheckedAbiLayouts) void {
        self.allocator.free(self.roots);
        self.layouts.deinit();
    }
};

/// Resolve public data layouts without running procedure specialization or LIR
/// lowering. Requests and results have the same order and share one layout store.
pub fn resolveCheckedAbiLayouts(
    allocator: Allocator,
    modules: CheckedModuleSet,
    requests: []const checked.CheckedTypeId,
    target_usize: base.target.TargetUsize,
) Allocator.Error!CheckedAbiLayouts {
    var plan = try postcheck.Boxy.Plan.analyzeHostAbi(allocator, .{
        .root_module = modules.root,
        .imports = modules.imports,
        .layout_requests = requests,
    });
    defer plan.deinit();
    var layouts = try @import("layout").Store.init(allocator, target_usize);
    errdefer layouts.deinit();
    const roots = try postcheck.Boxy.Layouts.commitHostAbi(allocator, &plan, &layouts);
    return .{ .allocator = allocator, .layouts = layouts, .roots = roots };
}

/// Resource failure while lowering checked modules to LIR, plus the one
/// checked input this entrance rejects outright: see
/// `requireHostedProceduresBound`.
pub const LowerResourceError = Allocator.Error || HostedBindingError;

/// A hosted declaration the platform header's hosted section never named. The
/// compile that produced it already carries checking's report of the missing
/// entry, so callers stop rather than adding a message of their own.
pub const HostedBindingError = error{HostedFunctionNotBound};
/// An explicit checked constant requested for target static-data materialization.
pub const StaticDataRequest = postcheck.Common.StaticDataRequest;

pub const SpecializationStrategy = base.SpecializationStrategy;

/// Root checked module plus the checked imports visible to post-check lowering.
pub const CheckedModuleSet = struct {
    root: checked.LoweringModuleView,
    imports: []const checked.ImportedModuleView = &.{},
};

/// Root requests that determine which checked definitions become LIR roots.
pub const RootRequestSet = struct {
    requests: []const checked.RootRequest = &.{},
    /// Checked module owning each request type; empty for root-module requests.
    source_modules: []const checked.ModuleId = &.{},
    layout_requests: []const checked.CheckedTypeId = &.{},
    /// Explicit checked constants to restore as readonly target data.
    static_data_requests: []const postcheck.Common.StaticDataRequest = &.{},
    /// Request layouts and materialization roots for host-visible provided data.
    include_provided_data_exports: bool = false,
    /// Restore eligible stored constants as internal readonly static values.
    include_internal_static_data: bool = false,
    test_plan_metadata: []const postcheck.Common.RootTestPlanMetadata = &.{},
};

/// Deterministic task counts for parallel solved-LIR body lowering.
pub const SolvedLirParallelMetrics = postcheck.SolvedLirLower.ParallelMetrics;

/// Exact staged SpecConstr work, independent of worker completion order.
pub const SpecConstrParallelMetrics = postcheck.MonotypeLifted.SpecConstr.ParallelMetrics;
pub const SpecConstrPhase = postcheck.MonotypeLifted.SpecConstr.Phase;

/// Deterministic worker counters for procedure-local LIR optimization phases.
pub const LirPassParallelMetrics = ProcPasses.ParallelMetrics;
/// ARC worker task counts and deterministic serial-or-parallel work totals.
pub const ArcParallelMetrics = Arc.ParallelMetrics;
/// Procedure-local optimization phases, in pipeline order.
pub const LirPassPhase = ProcPasses.Phase;

/// Producer-side work counts, independent of elapsed time and output size.
pub const WorkMetrics = struct {
    monotype_runs: u32 = 0,
    solved_runs: u32 = 0,
    lir_continuations: u32 = 0,
};

/// Target settings and checked module state for the checked-to-LIR pipeline.
pub const TargetConfig = struct {
    work_metrics: ?*WorkMetrics = null,
    target_usize: base.target.TargetUsize = base.target.TargetUsize.native,
    specialization_strategy: SpecializationStrategy = .lss,
    /// Reuse checking workers for generic post-check tasks when available.
    post_check_executor: ?base.post_check_task_executor.Executor = null,
    checked_module_state: CheckedModuleState = .complete,
    /// The compilation session supplies evaluated root slots for these reads.
    comptime_value_reads: bool = false,
    inline_mode: InlineMode = .none,
    /// Direct-call inlining scope for SpecConstr's value-aware clones.
    /// Optimized builds use `.all_calls`; dev builds use `.iterator_fusion`
    /// so post-check time and emitted program size stay bounded. Consulted
    /// only when `inline_mode` is not `.none`, since that is what gates
    /// SpecConstr itself.
    spec_constr_clone_inlining: SpecConstrCloneInlining = .all_calls,
    inline_expects: InlineExpectMode = .run,
    /// Whether ARC may consume a dead Box lender while unboxing.
    consume_dead_boxes: bool = false,
    /// Allow `List.map` and `List.update` to reuse a unique input list's
    /// allocation. Map additionally requires interchangeable input and output
    /// element layouts. Optimized builds enable this; dev builds and
    /// compile-time evaluation leave it off so the in-place branches are
    /// dropped during lowering.
    list_in_place_map: bool = false,
    /// Preserve source-level procedure names in LIR for runtime diagnostics.
    proc_debug_names: bool = false,
    /// The object cache Monotype asks for closed specializations.
    spec_cache: ?postcheck.Common.SpecCacheLookup = null,
    /// Whether Direct LIR may serve cache entries to the compile-time
    /// roots' closure. `prepareCheckedModulesMonotype` sets this from the
    /// modules: a match whose exhaustiveness only the evaluation can decide
    /// must run as the evaluator's own code, which reports the branches it
    /// takes, and a spliced entry reports nothing.
    comptime_closure_hits: bool = false,
    /// Keep every keyed specialization procedure through compaction; a pack
    /// program offers them from its manifest whether or not its export
    /// wrappers inlined their calls.
    keep_specialization_procs: bool = false,
    /// Thread slack counters through loop-carried append-only lists so the
    /// per-element ownership and capacity checks amortize. On by default;
    /// shape-comparison tests turn it off because promotion intentionally
    /// changes the loop skeleton of qualifying sides.
    promote_loop_appends: bool = true,
    /// The rewrites that exist only to make the produced program faster:
    /// tag-case fusion, join scalarization and box reuse. `--opt=dev` skips
    /// them, since it trades program speed for compile speed; TRMC and loop
    /// append promotion stay on everywhere because they bound stack depth and
    /// list copying rather than shave constant factors.
    fuse_tag_cases: bool = true,
    scalarize_joins: bool = true,
    reuse_boxes: bool = true,
    /// Build ConstStore materialization plans for requested layouts.
    /// Disable this only for consumers that read requested layout metadata and
    /// never materialize requested-layout values.
    layout_request_const_plans: bool = true,
    /// Delete LIR switch edges whose tag discriminants are unreachable. This
    /// is enabled for optimized builds and kept off for dev and compile-time
    /// evaluation.
    tag_reachability: bool = false,
    /// Elide checks proven always-safe by unsigned value-range analysis. This
    /// is enabled for optimized builds and kept off for dev and compile-time
    /// evaluation.
    prove_ranges: bool = false,
    /// Debug-only: forwarded to `SolvedLirLower.Options.debug_materialized_out`
    /// so a differential harness can execute the Debug verifier's materialized
    /// Lambda Mono program. The slot receives a value only in Debug builds.
    debug_materialized_out: ?*?postcheck.LambdaMono.Ast.Program = null,
    /// Optional deterministic task counts for solved-LIR body-shard lowering.
    solved_lir_parallel_metrics_out: ?*SolvedLirParallelMetrics = null,
    /// Per-run staged specialization counters; SpecConstr owns resetting them.
    spec_constr_parallel_metrics_out: ?*SpecConstrParallelMetrics = null,
    /// Reset once before the LIR pass pipeline, then accumulated across phases.
    lir_pass_parallel_metrics_out: ?*LirPassParallelMetrics = null,
    /// Per-run ARC counters; ARC insertion owns resetting this output.
    arc_parallel_metrics_out: ?*ArcParallelMetrics = null,
    /// Receives the expression count of the lifted program handed to lambda-set
    /// solving. Every later post-check stage walks that program in full, so the
    /// count is the size measure a growth regression shows up in.
    lifted_expr_count_out: ?*usize = null,
    /// Completed compile-time scalar roots for a continuation lowered after
    /// the host program completed; the lowerer emits them as literals so the
    /// LIR passes see the constants instead of slot reads.
    completed_scalar_values: ?*const CompletedScalarValues = null,
    /// Optional timing accumulator for the checked-to-LIR pipeline.
    timing: ?*Timing = null,
};

/// Thread-safe timing totals for the checked-to-LIR pipeline.
pub const Timing = struct {
    std_io: std.Io,
    detailed_monotype_body: bool = false,
    monotype_diagnostics_mutex: std.Io.Mutex = .init,
    monotype_diagnostics: postcheck.Monotype.Lower.Diagnostics = .{},
    solved_lir_parallel_mutex: std.Io.Mutex = .init,
    solved_lir_parallel: SolvedLirParallelMetrics = .{},
    spec_constr_parallel_mutex: std.Io.Mutex = .init,
    spec_constr_parallel: SpecConstrParallelMetrics = .{},
    lir_pass_parallel_mutex: std.Io.Mutex = .init,
    lir_pass_parallel: LirPassParallelMetrics = .{},
    arc_parallel_mutex: std.Io.Mutex = .init,
    arc_parallel: ArcParallelMetrics = .{},
    monotype_ns: TimingCounter = .{},
    monotype_setup_ns: TimingCounter = .{},
    monotype_procedure_specialization_ns: TimingCounter = .{},
    monotype_procedure_root_wrapper_ns: TimingCounter = .{},
    monotype_procedure_lookup_reservation_ns: TimingCounter = .{},
    monotype_procedure_dispatch_evidence_ns: TimingCounter = .{},
    monotype_procedure_body_graph_setup_ns: TimingCounter = .{},
    monotype_procedure_body_lowering_ns: TimingCounter = .{},
    monotype_procedure_body_type_graph_ns: TimingCounter = .{},
    monotype_procedure_body_call_dispatch_ns: TimingCounter = .{},
    monotype_procedure_body_draft_ir_ns: TimingCounter = .{},
    monotype_procedure_body_reachability_ns: TimingCounter = .{},
    monotype_procedure_body_source_mapping_ns: TimingCounter = .{},
    monotype_procedure_body_local_proc_context_ns: TimingCounter = .{},
    monotype_procedure_body_finalization_ns: TimingCounter = .{},
    monotype_procedure_completion_ns: TimingCounter = .{},
    monotype_procedure_parallel_wait_ns: TimingCounter = .{},
    monotype_layout_requests_ns: TimingCounter = .{},
    monotype_static_data_requests_ns: TimingCounter = .{},
    monotype_finalization_ns: TimingCounter = .{},
    monotype_parallel_worker_work_ns: TimingCounter = .{},
    monotype_parallel_coordinator_post_batch_work_ns: TimingCounter = .{},
    monotype_parallel_root_tasks_submitted: TimingCounter = .{},
    monotype_parallel_root_tasks_committed: TimingCounter = .{},
    monotype_parallel_specialization_tasks_submitted: TimingCounter = .{},
    monotype_parallel_specialization_tasks_committed: TimingCounter = .{},
    monotype_parallel_specialization_tasks_discarded_ready: TimingCounter = .{},
    monotype_parallel_task_waves: TimingCounter = .{},
    monotype_parallel_peak_worker_lanes_available: TimingCounter = .{},
    monotype_parallel_peak_worker_lanes_used: TimingCounter = .{},
    monotype_parallel_within_lowering_lane_reuse_tasks: TimingCounter = .{},
    monotype_parallel_peak_specialization_jobs_pending: TimingCounter = .{},
    monotype_parallel_peak_specialization_shards_retained: TimingCounter = .{},
    boxy_plan_ns: TimingCounter = .{},
    boxy_lower_ns: TimingCounter = .{},
    lift_ns: TimingCounter = .{},
    spec_constr_ns: TimingCounter = .{},
    lambda_solve_ns: TimingCounter = .{},
    inline_plan_ns: TimingCounter = .{},
    lir_gen_ns: TimingCounter = .{},
    lir_passes_ns: TimingCounter = .{},
    arc_ns: TimingCounter = .{},

    pub fn init(std_io: std.Io) Timing {
        return .{ .std_io = std_io };
    }

    /// Enable fine-grained, per-node Monotype body timing. This is opt-in
    /// because timestamping interleaved body work has measurable overhead.
    pub fn enableDetailedMonotypeBody(self: *Timing) void {
        self.detailed_monotype_body = true;
    }

    pub fn snapshot(self: *const Timing) TimingSnapshot {
        const diagnostics = self.monotypeDiagnosticsSnapshot();
        return .{
            .monotype_ns = self.monotype_ns.load(),
            .monotype_setup_ns = self.monotype_setup_ns.load(),
            .monotype_procedure_specialization_ns = self.monotype_procedure_specialization_ns.load(),
            .monotype_procedure_root_wrapper_ns = self.monotype_procedure_root_wrapper_ns.load(),
            .monotype_procedure_lookup_reservation_ns = self.monotype_procedure_lookup_reservation_ns.load(),
            .monotype_procedure_dispatch_evidence_ns = self.monotype_procedure_dispatch_evidence_ns.load(),
            .monotype_procedure_body_graph_setup_ns = self.monotype_procedure_body_graph_setup_ns.load(),
            .monotype_procedure_body_lowering_ns = self.monotype_procedure_body_lowering_ns.load(),
            .monotype_procedure_body_type_graph_ns = self.monotype_procedure_body_type_graph_ns.load(),
            .monotype_procedure_body_call_dispatch_ns = self.monotype_procedure_body_call_dispatch_ns.load(),
            .monotype_procedure_body_draft_ir_ns = self.monotype_procedure_body_draft_ir_ns.load(),
            .monotype_procedure_body_reachability_ns = self.monotype_procedure_body_reachability_ns.load(),
            .monotype_procedure_body_source_mapping_ns = self.monotype_procedure_body_source_mapping_ns.load(),
            .monotype_procedure_body_local_proc_context_ns = self.monotype_procedure_body_local_proc_context_ns.load(),
            .monotype_procedure_body_finalization_ns = self.monotype_procedure_body_finalization_ns.load(),
            .monotype_procedure_completion_ns = self.monotype_procedure_completion_ns.load(),
            .monotype_procedure_parallel_wait_ns = self.monotype_procedure_parallel_wait_ns.load(),
            .monotype_layout_requests_ns = self.monotype_layout_requests_ns.load(),
            .monotype_static_data_requests_ns = self.monotype_static_data_requests_ns.load(),
            .monotype_finalization_ns = self.monotype_finalization_ns.load(),
            .monotype_parallel = .{
                .worker_work_ns = self.monotype_parallel_worker_work_ns.load(),
                .coordinator_post_batch_work_ns = self.monotype_parallel_coordinator_post_batch_work_ns.load(),
                .root_tasks_submitted = self.monotype_parallel_root_tasks_submitted.load(),
                .root_tasks_committed = self.monotype_parallel_root_tasks_committed.load(),
                .specialization_tasks_submitted = self.monotype_parallel_specialization_tasks_submitted.load(),
                .specialization_tasks_committed = self.monotype_parallel_specialization_tasks_committed.load(),
                .specialization_tasks_discarded_ready = self.monotype_parallel_specialization_tasks_discarded_ready.load(),
                .task_waves = self.monotype_parallel_task_waves.load(),
                .peak_worker_lanes_available = self.monotype_parallel_peak_worker_lanes_available.load(),
                .peak_worker_lanes_used = self.monotype_parallel_peak_worker_lanes_used.load(),
                .within_lowering_lane_reuse_tasks = self.monotype_parallel_within_lowering_lane_reuse_tasks.load(),
                .peak_specialization_jobs_pending = self.monotype_parallel_peak_specialization_jobs_pending.load(),
                .peak_specialization_shards_retained = self.monotype_parallel_peak_specialization_shards_retained.load(),
            },
            .boxy_plan_ns = self.boxy_plan_ns.load(),
            .boxy_lower_ns = self.boxy_lower_ns.load(),
            .lift_ns = self.lift_ns.load(),
            .spec_constr_ns = self.spec_constr_ns.load(),
            .lambda_solve_ns = self.lambda_solve_ns.load(),
            .inline_plan_ns = self.inline_plan_ns.load(),
            .lir_gen_ns = self.lir_gen_ns.load(),
            .lir_passes_ns = self.lir_passes_ns.load(),
            .arc_ns = self.arc_ns.load(),
            .monotype_diagnostics = diagnostics,
            .solved_lir_parallel = self.solvedLirParallelSnapshot(),
            .spec_constr_parallel = self.specConstrParallelSnapshot(),
            .lir_pass_parallel = self.lirPassParallelSnapshot(),
            .arc_parallel = self.arcParallelSnapshot(),
        };
    }

    pub fn addSnapshot(self: *Timing, snapshot_value: TimingSnapshot) void {
        self.monotype_ns.add(snapshot_value.monotype_ns);
        self.monotype_setup_ns.add(snapshot_value.monotype_setup_ns);
        self.monotype_procedure_specialization_ns.add(snapshot_value.monotype_procedure_specialization_ns);
        self.monotype_procedure_root_wrapper_ns.add(snapshot_value.monotype_procedure_root_wrapper_ns);
        self.monotype_procedure_lookup_reservation_ns.add(snapshot_value.monotype_procedure_lookup_reservation_ns);
        self.monotype_procedure_dispatch_evidence_ns.add(snapshot_value.monotype_procedure_dispatch_evidence_ns);
        self.monotype_procedure_body_graph_setup_ns.add(snapshot_value.monotype_procedure_body_graph_setup_ns);
        self.monotype_procedure_body_lowering_ns.add(snapshot_value.monotype_procedure_body_lowering_ns);
        self.monotype_procedure_body_type_graph_ns.add(snapshot_value.monotype_procedure_body_type_graph_ns);
        self.monotype_procedure_body_call_dispatch_ns.add(snapshot_value.monotype_procedure_body_call_dispatch_ns);
        self.monotype_procedure_body_draft_ir_ns.add(snapshot_value.monotype_procedure_body_draft_ir_ns);
        self.monotype_procedure_body_reachability_ns.add(snapshot_value.monotype_procedure_body_reachability_ns);
        self.monotype_procedure_body_source_mapping_ns.add(snapshot_value.monotype_procedure_body_source_mapping_ns);
        self.monotype_procedure_body_local_proc_context_ns.add(snapshot_value.monotype_procedure_body_local_proc_context_ns);
        self.monotype_procedure_body_finalization_ns.add(snapshot_value.monotype_procedure_body_finalization_ns);
        self.monotype_procedure_completion_ns.add(snapshot_value.monotype_procedure_completion_ns);
        self.monotype_procedure_parallel_wait_ns.add(snapshot_value.monotype_procedure_parallel_wait_ns);
        self.monotype_layout_requests_ns.add(snapshot_value.monotype_layout_requests_ns);
        self.monotype_static_data_requests_ns.add(snapshot_value.monotype_static_data_requests_ns);
        self.monotype_finalization_ns.add(snapshot_value.monotype_finalization_ns);
        self.addMonotypeParallel(snapshot_value.monotype_parallel);
        self.addSolvedLirParallel(snapshot_value.solved_lir_parallel);
        self.addSpecConstrParallel(snapshot_value.spec_constr_parallel);
        self.addLirPassParallel(snapshot_value.lir_pass_parallel);
        self.addArcParallel(snapshot_value.arc_parallel);
        self.boxy_plan_ns.add(snapshot_value.boxy_plan_ns);
        self.boxy_lower_ns.add(snapshot_value.boxy_lower_ns);
        self.lift_ns.add(snapshot_value.lift_ns);
        self.spec_constr_ns.add(snapshot_value.spec_constr_ns);
        self.lambda_solve_ns.add(snapshot_value.lambda_solve_ns);
        self.inline_plan_ns.add(snapshot_value.inline_plan_ns);
        self.lir_gen_ns.add(snapshot_value.lir_gen_ns);
        self.lir_passes_ns.add(snapshot_value.lir_passes_ns);
        self.arc_ns.add(snapshot_value.arc_ns);
        self.addMonotypeDiagnostics(snapshot_value.monotype_diagnostics);
    }

    fn start(self: *const Timing) i64 {
        return timingNowNs(self.std_io);
    }

    fn finish(self: *Timing, started_ns: i64, phase: TimingPhase) void {
        const finished_ns = timingNowNs(self.std_io);
        const elapsed_ns: u64 = @intCast(@max(0, finished_ns - started_ns));
        switch (phase) {
            .monotype => self.monotype_ns.add(elapsed_ns),
            .lift => self.lift_ns.add(elapsed_ns),
            .spec_constr => self.spec_constr_ns.add(elapsed_ns),
            .lambda_solve => self.lambda_solve_ns.add(elapsed_ns),
            .inline_plan => self.inline_plan_ns.add(elapsed_ns),
            .lir_gen => self.lir_gen_ns.add(elapsed_ns),
            .lir_passes => self.lir_passes_ns.add(elapsed_ns),
            .arc => self.arc_ns.add(elapsed_ns),
            .boxy_plan => self.boxy_plan_ns.add(elapsed_ns),
            .boxy_lower => self.boxy_lower_ns.add(elapsed_ns),
        }
    }

    fn addMonotypeSnapshot(self: *Timing, snapshot_value: postcheck.Monotype.Lower.TimingSnapshot) void {
        self.monotype_setup_ns.add(snapshot_value.setup_ns);
        self.monotype_procedure_specialization_ns.add(snapshot_value.procedure_specialization_ns);
        self.monotype_procedure_root_wrapper_ns.add(snapshot_value.procedure_root_wrapper_ns);
        self.monotype_procedure_lookup_reservation_ns.add(snapshot_value.procedure_lookup_reservation_ns);
        self.monotype_procedure_dispatch_evidence_ns.add(snapshot_value.procedure_dispatch_evidence_ns);
        self.monotype_procedure_body_graph_setup_ns.add(snapshot_value.procedure_body_graph_setup_ns);
        self.monotype_procedure_body_lowering_ns.add(snapshot_value.procedure_body_lowering_ns);
        self.monotype_procedure_body_type_graph_ns.add(snapshot_value.procedure_body_type_graph_ns);
        self.monotype_procedure_body_call_dispatch_ns.add(snapshot_value.procedure_body_call_dispatch_ns);
        self.monotype_procedure_body_draft_ir_ns.add(snapshot_value.procedure_body_draft_ir_ns);
        self.monotype_procedure_body_reachability_ns.add(snapshot_value.procedure_body_reachability_ns);
        self.monotype_procedure_body_source_mapping_ns.add(snapshot_value.procedure_body_source_mapping_ns);
        self.monotype_procedure_body_local_proc_context_ns.add(snapshot_value.procedure_body_local_proc_context_ns);
        self.monotype_procedure_body_finalization_ns.add(snapshot_value.procedure_body_finalization_ns);
        self.monotype_procedure_completion_ns.add(snapshot_value.procedure_completion_ns);
        self.monotype_procedure_parallel_wait_ns.add(snapshot_value.procedure_parallel_wait_ns);
        self.monotype_layout_requests_ns.add(snapshot_value.layout_requests_ns);
        self.monotype_static_data_requests_ns.add(snapshot_value.static_data_requests_ns);
        self.monotype_finalization_ns.add(snapshot_value.finalization_ns);
        self.addMonotypeParallel(snapshot_value.parallel);
    }

    fn addMonotypeParallel(self: *Timing, parallel: postcheck.Monotype.Lower.ParallelMetricsSnapshot) void {
        self.monotype_parallel_worker_work_ns.add(parallel.worker_work_ns);
        self.monotype_parallel_coordinator_post_batch_work_ns.add(parallel.coordinator_post_batch_work_ns);
        self.monotype_parallel_root_tasks_submitted.add(parallel.root_tasks_submitted);
        self.monotype_parallel_root_tasks_committed.add(parallel.root_tasks_committed);
        self.monotype_parallel_specialization_tasks_submitted.add(parallel.specialization_tasks_submitted);
        self.monotype_parallel_specialization_tasks_committed.add(parallel.specialization_tasks_committed);
        self.monotype_parallel_specialization_tasks_discarded_ready.add(parallel.specialization_tasks_discarded_ready);
        self.monotype_parallel_task_waves.add(parallel.task_waves);
        self.monotype_parallel_peak_worker_lanes_available.max(parallel.peak_worker_lanes_available);
        self.monotype_parallel_peak_worker_lanes_used.max(parallel.peak_worker_lanes_used);
        self.monotype_parallel_within_lowering_lane_reuse_tasks.add(parallel.within_lowering_lane_reuse_tasks);
        self.monotype_parallel_peak_specialization_jobs_pending.max(parallel.peak_specialization_jobs_pending);
        self.monotype_parallel_peak_specialization_shards_retained.max(parallel.peak_specialization_shards_retained);
    }

    fn addSolvedLirParallel(self: *Timing, parallel: SolvedLirParallelMetrics) void {
        self.solved_lir_parallel_mutex.lockUncancelable(self.std_io);
        defer self.solved_lir_parallel_mutex.unlock(self.std_io);
        // All Solved-LIR metrics count completed work, not peaks or durations.
        inline for (std.meta.fields(SolvedLirParallelMetrics)) |field| {
            @field(self.solved_lir_parallel, field.name) +|= @field(parallel, field.name);
        }
    }

    fn solvedLirParallelSnapshot(self: *const Timing) SolvedLirParallelMetrics {
        const mutable = @constCast(self);
        mutable.solved_lir_parallel_mutex.lockUncancelable(self.std_io);
        defer mutable.solved_lir_parallel_mutex.unlock(self.std_io);
        return self.solved_lir_parallel;
    }

    fn addSpecConstrParallel(self: *Timing, parallel: SpecConstrParallelMetrics) void {
        self.spec_constr_parallel_mutex.lockUncancelable(self.std_io);
        defer self.spec_constr_parallel_mutex.unlock(self.std_io);
        self.spec_constr_parallel.add(parallel);
    }

    fn specConstrParallelSnapshot(self: *const Timing) SpecConstrParallelMetrics {
        const mutable = @constCast(self);
        mutable.spec_constr_parallel_mutex.lockUncancelable(self.std_io);
        defer mutable.spec_constr_parallel_mutex.unlock(self.std_io);
        return self.spec_constr_parallel;
    }

    fn addLirPassParallel(self: *Timing, parallel: LirPassParallelMetrics) void {
        self.lir_pass_parallel_mutex.lockUncancelable(self.std_io);
        defer self.lir_pass_parallel_mutex.unlock(self.std_io);
        self.lir_pass_parallel.add(parallel);
    }

    fn lirPassParallelSnapshot(self: *const Timing) LirPassParallelMetrics {
        const mutable = @constCast(self);
        mutable.lir_pass_parallel_mutex.lockUncancelable(self.std_io);
        defer mutable.lir_pass_parallel_mutex.unlock(self.std_io);
        return self.lir_pass_parallel;
    }

    fn addArcParallel(self: *Timing, parallel: ArcParallelMetrics) void {
        self.arc_parallel_mutex.lockUncancelable(self.std_io);
        defer self.arc_parallel_mutex.unlock(self.std_io);
        self.arc_parallel.add(parallel);
    }

    fn arcParallelSnapshot(self: *const Timing) ArcParallelMetrics {
        const mutable = @constCast(self);
        mutable.arc_parallel_mutex.lockUncancelable(self.std_io);
        defer mutable.arc_parallel_mutex.unlock(self.std_io);
        return self.arc_parallel;
    }

    fn addMonotypeDiagnostics(self: *Timing, diagnostics: postcheck.Monotype.Lower.Diagnostics) void {
        self.monotype_diagnostics_mutex.lockUncancelable(self.std_io);
        defer self.monotype_diagnostics_mutex.unlock(self.std_io);
        self.monotype_diagnostics.add(diagnostics);
    }

    fn monotypeDiagnosticsSnapshot(self: *const Timing) postcheck.Monotype.Lower.Diagnostics {
        const mutable = @constCast(self);
        mutable.monotype_diagnostics_mutex.lockUncancelable(self.std_io);
        defer mutable.monotype_diagnostics_mutex.unlock(self.std_io);
        return self.monotype_diagnostics;
    }
};

const TimingCounter = base.ConcurrentU64;

/// Immutable checked-to-LIR timings for progress reporting.
pub const TimingSnapshot = struct {
    monotype_ns: u64 = 0,
    monotype_setup_ns: u64 = 0,
    monotype_procedure_specialization_ns: u64 = 0,
    monotype_procedure_root_wrapper_ns: u64 = 0,
    monotype_procedure_lookup_reservation_ns: u64 = 0,
    monotype_procedure_dispatch_evidence_ns: u64 = 0,
    monotype_procedure_body_graph_setup_ns: u64 = 0,
    monotype_procedure_body_lowering_ns: u64 = 0,
    monotype_procedure_body_type_graph_ns: u64 = 0,
    monotype_procedure_body_call_dispatch_ns: u64 = 0,
    monotype_procedure_body_draft_ir_ns: u64 = 0,
    monotype_procedure_body_reachability_ns: u64 = 0,
    monotype_procedure_body_source_mapping_ns: u64 = 0,
    monotype_procedure_body_local_proc_context_ns: u64 = 0,
    monotype_procedure_body_finalization_ns: u64 = 0,
    monotype_procedure_completion_ns: u64 = 0,
    monotype_procedure_parallel_wait_ns: u64 = 0,
    monotype_layout_requests_ns: u64 = 0,
    monotype_static_data_requests_ns: u64 = 0,
    monotype_finalization_ns: u64 = 0,
    monotype_parallel: postcheck.Monotype.Lower.ParallelMetricsSnapshot = .{},
    solved_lir_parallel: SolvedLirParallelMetrics = .{},
    spec_constr_parallel: SpecConstrParallelMetrics = .{},
    lir_pass_parallel: LirPassParallelMetrics = .{},
    arc_parallel: ArcParallelMetrics = .{},
    boxy_plan_ns: u64 = 0,
    boxy_lower_ns: u64 = 0,
    lift_ns: u64 = 0,
    spec_constr_ns: u64 = 0,
    lambda_solve_ns: u64 = 0,
    inline_plan_ns: u64 = 0,
    lir_gen_ns: u64 = 0,
    lir_passes_ns: u64 = 0,
    arc_ns: u64 = 0,
    monotype_diagnostics: postcheck.Monotype.Lower.Diagnostics = .{},
};

const TimingPhase = enum {
    monotype,
    lift,
    spec_constr,
    lambda_solve,
    inline_plan,
    lir_gen,
    lir_passes,
    arc,
    boxy_plan,
    boxy_lower,
};

const PipelineTimingScope = struct {
    timing: ?*Timing = null,
    started_ns: i64 = 0,
    phase: TimingPhase = undefined,

    fn begin(timing: ?*Timing, phase: TimingPhase) PipelineTimingScope {
        const active = timing orelse return .{};
        return .{
            .timing = active,
            .started_ns = active.start(),
            .phase = phase,
        };
    }

    fn end(self: *PipelineTimingScope) void {
        const timing = self.timing orelse return;
        timing.finish(self.started_ns, self.phase);
        self.timing = null;
    }
};

fn timingNowNs(std_io: std.Io) i64 {
    return @intCast(@max(0, std.Io.Timestamp.now(std_io, .awake).nanoseconds));
}

test "pipeline timing aggregates Solved-LIR counters with saturation and fresh reset" {
    var timing = Timing.init(std.testing.io);
    var first: SolvedLirParallelMetrics = .{};
    inline for (std.meta.fields(SolvedLirParallelMetrics), 0..) |field, i| {
        @field(first, field.name) = i + 1;
    }
    timing.addSolvedLirParallel(first);
    var aggregate = Timing.init(std.testing.io);
    aggregate.addSnapshot(timing.snapshot());
    aggregate.addSnapshot(timing.snapshot());
    const doubled = aggregate.snapshot();
    inline for (std.meta.fields(SolvedLirParallelMetrics), 0..) |field, i| {
        try std.testing.expectEqual(@as(u64, 2 * (i + 1)), @field(doubled.solved_lir_parallel, field.name));
        @field(first, field.name) = std.math.maxInt(u64);
    }
    aggregate.addSolvedLirParallel(first);
    const saturated = aggregate.snapshot();
    inline for (std.meta.fields(SolvedLirParallelMetrics)) |field| {
        try std.testing.expectEqual(std.math.maxInt(u64), @field(saturated.solved_lir_parallel, field.name));
    }
    try std.testing.expectEqual(@as(u64, 0), saturated.lir_gen_ns);
    aggregate = Timing.init(std.testing.io);
    try std.testing.expectEqualDeep(SolvedLirParallelMetrics{}, aggregate.snapshot().solved_lir_parallel);
}

test "pipeline timing preserves explicit Solved-LIR metrics output" {
    var timing = Timing.init(std.testing.io);
    var local: SolvedLirParallelMetrics = .{};
    var explicit: SolvedLirParallelMetrics = .{ .tasks_submitted = 99 };
    try std.testing.expect(solvedLirMetricsOutput(.{}, &local) == null);
    try std.testing.expect(solvedLirMetricsOutput(.{ .timing = &timing }, &local).? == &local);
    try std.testing.expect(solvedLirMetricsOutput(.{ .solved_lir_parallel_metrics_out = &explicit }, &local).? == &explicit);
    const output = solvedLirMetricsOutput(.{
        .timing = &timing,
        .solved_lir_parallel_metrics_out = &explicit,
    }, &local).?;
    try std.testing.expect(output == &explicit);
    try std.testing.expectEqual(@as(u64, 99), explicit.tasks_submitted);
    // Simulate the lowerer's per-run reset and completed output.
    output.* = .{ .tasks_submitted = 3, .tasks_committed = 3 };
    timing.addSolvedLirParallel(output.*);
    try std.testing.expectEqualDeep(explicit, timing.snapshot().solved_lir_parallel);
    try std.testing.expectEqual(@as(u64, 3), explicit.tasks_submitted);
    try std.testing.expectEqualDeep(SolvedLirParallelMetrics{}, local);
}

test "pipeline timing aggregates SpecConstr totals and preserves peaks" {
    var timing = Timing.init(std.testing.io);
    var first: SpecConstrParallelMetrics = .{
        .tasks_submitted = 3,
        .tasks_committed = 3,
        .patterns_recorded = 5,
        .patterns_admitted = 2,
        .bodies_committed = 4,
        .expressions_committed = 30,
        .peak_retained_shards = 2,
        .committed_by_phase = .{ 1, 2, 3 },
        .changed_by_phase = .{ 1, 1, 2 },
    };
    timing.addSpecConstrParallel(first);
    var aggregate = Timing.init(std.testing.io);
    aggregate.addSnapshot(timing.snapshot());
    aggregate.addSnapshot(timing.snapshot());
    const doubled = aggregate.snapshot().spec_constr_parallel;
    inline for (std.meta.fields(SpecConstrParallelMetrics)) |field| {
        if (comptime std.mem.eql(u8, field.name, "peak_retained_shards")) {
            try std.testing.expectEqual(@field(first, field.name), @field(doubled, field.name));
        } else if (field.type == u64) {
            try std.testing.expectEqual(2 * @field(first, field.name), @field(doubled, field.name));
        } else {
            for (@field(first, field.name), @field(doubled, field.name)) |value, total| {
                try std.testing.expectEqual(2 * value, total);
            }
        }
        if (field.type == u64) {
            @field(first, field.name) = std.math.maxInt(u64);
        } else {
            @memset(&@field(first, field.name), std.math.maxInt(u64));
        }
    }
    aggregate.addSpecConstrParallel(first);
    aggregate.addSpecConstrParallel(first);
    try std.testing.expectEqualDeep(first, aggregate.snapshot().spec_constr_parallel);
    try std.testing.expectEqual(@as(u64, 0), aggregate.snapshot().spec_constr_ns);
    aggregate = Timing.init(std.testing.io);
    try std.testing.expectEqualDeep(SpecConstrParallelMetrics{}, aggregate.snapshot().spec_constr_parallel);
}

test "pipeline timing preserves explicit SpecConstr metrics output" {
    var timing = Timing.init(std.testing.io);
    var local: SpecConstrParallelMetrics = .{};
    var explicit: SpecConstrParallelMetrics = .{ .tasks_submitted = 99 };
    try std.testing.expect(specConstrMetricsOutput(.{}, &local) == null);
    try std.testing.expect(specConstrMetricsOutput(.{ .timing = &timing }, &local).? == &local);
    try std.testing.expect(specConstrMetricsOutput(.{ .spec_constr_parallel_metrics_out = &explicit }, &local).? == &explicit);
    const output = specConstrMetricsOutput(.{
        .timing = &timing,
        .spec_constr_parallel_metrics_out = &explicit,
    }, &local).?;
    try std.testing.expect(output == &explicit);
    try std.testing.expectEqual(@as(u64, 99), explicit.tasks_submitted);
    // The pass owns resetting its output; aggregation only observes it.
    output.* = .{ .tasks_submitted = 3, .tasks_committed = 3 };
    timing.addSpecConstrParallel(output.*);
    try std.testing.expectEqualDeep(explicit, timing.snapshot().spec_constr_parallel);
    try std.testing.expectEqualDeep(SpecConstrParallelMetrics{}, local);
}

test "pipeline timing aggregates ARC counters with saturation and fresh reset" {
    var timing = Timing.init(std.testing.io);
    var first: ArcParallelMetrics = .{};
    inline for (std.meta.fields(ArcParallelMetrics), 0..) |field, i| {
        if (field.type == u64) @field(first, field.name) = i + 1;
    }
    inline for (std.meta.fields(Arc.UniquenessMetrics), 0..) |field, i| @field(first.uniqueness, field.name) = i + 1;
    timing.addArcParallel(first);
    var aggregate = Timing.init(std.testing.io);
    aggregate.addSnapshot(timing.snapshot());
    aggregate.addSnapshot(timing.snapshot());
    const doubled = aggregate.snapshot();
    inline for (std.meta.fields(ArcParallelMetrics), 0..) |field, i| {
        if (field.type == u64) {
            try std.testing.expectEqual(@as(u64, 2 * (i + 1)), @field(doubled.arc_parallel, field.name));
            @field(first, field.name) = std.math.maxInt(u64);
        }
    }
    inline for (std.meta.fields(Arc.UniquenessMetrics), 0..) |field, i| {
        try std.testing.expectEqual(@as(u64, 2 * (i + 1)), @field(doubled.arc_parallel.uniqueness, field.name));
        @field(first.uniqueness, field.name) = std.math.maxInt(u64);
    }
    aggregate.addArcParallel(first);
    const saturated = aggregate.snapshot();
    inline for (std.meta.fields(ArcParallelMetrics)) |field| {
        if (field.type == u64) try std.testing.expectEqual(std.math.maxInt(u64), @field(saturated.arc_parallel, field.name));
    }
    inline for (std.meta.fields(Arc.UniquenessMetrics)) |field| try std.testing.expectEqual(std.math.maxInt(u64), @field(saturated.arc_parallel.uniqueness, field.name));
    try std.testing.expectEqual(@as(u64, 0), saturated.arc_ns);
    aggregate = Timing.init(std.testing.io);
    try std.testing.expectEqualDeep(ArcParallelMetrics{}, aggregate.snapshot().arc_parallel);
}

test "pipeline timing preserves explicit ARC metrics output" {
    var timing = Timing.init(std.testing.io);
    var local: ArcParallelMetrics = .{};
    var explicit: ArcParallelMetrics = .{ .source_tasks_submitted = 99 };
    try std.testing.expect(arcMetricsOutput(.{}, &local) == null);
    try std.testing.expect(arcMetricsOutput(.{ .timing = &timing }, &local).? == &local);
    try std.testing.expect(arcMetricsOutput(.{ .arc_parallel_metrics_out = &explicit }, &local).? == &explicit);
    const output = arcMetricsOutput(.{
        .timing = &timing,
        .arc_parallel_metrics_out = &explicit,
    }, &local).?;
    try std.testing.expect(output == &explicit);
    try std.testing.expectEqual(@as(u64, 99), explicit.source_tasks_submitted);
    // Simulate insertion's sole per-run reset and completed output.
    output.* = .{ .source_tasks_submitted = 3, .source_tasks_committed = 3, .waves = 1, .variants_reserved = 2 };
    timing.addArcParallel(output.*);
    try std.testing.expectEqualDeep(explicit, timing.snapshot().arc_parallel);
    try std.testing.expectEqual(@as(u64, 3), explicit.source_tasks_submitted);
    try std.testing.expectEqualDeep(ArcParallelMetrics{}, local);
}

test "pipeline timing aggregates LIR pass totals and preserves peaks" {
    var timing = Timing.init(std.testing.io);
    const counters: LirPassParallelMetrics = .{
        .tasks_submitted = 15,
        .tasks_committed = 15,
        .prepared_statement_rows = 100,
        .appended_statements = 30,
        .peak_retained_shards = 7,
        .committed_by_phase = @splat(3),
        .changed_by_phase = @splat(1),
    };
    timing.addLirPassParallel(counters);
    timing.addSnapshot(.{ .lir_pass_parallel = counters });
    const result = timing.snapshot().lir_pass_parallel;
    try std.testing.expectEqual(@as(u64, 30), result.tasks_committed);
    try std.testing.expectEqual(@as(u64, 7), result.peak_retained_shards);
    try std.testing.expectEqual(@as(u64, 200), result.prepared_statement_rows);
    try std.testing.expectEqual(@as(u64, 60), result.appended_statements);
    try std.testing.expectEqualDeep(@as(@TypeOf(result.committed_by_phase), @splat(6)), result.committed_by_phase);
    try std.testing.expectEqualDeep(@as(@TypeOf(result.changed_by_phase), @splat(2)), result.changed_by_phase);
    timing.addLirPassParallel(.{ .tasks_committed = std.math.maxInt(u64), .changed_by_phase = @splat(std.math.maxInt(u64)) });
    const saturated = timing.snapshot().lir_pass_parallel;
    try std.testing.expectEqual(std.math.maxInt(u64), saturated.tasks_committed);
    for (saturated.changed_by_phase) |count| try std.testing.expectEqual(std.math.maxInt(u64), count);
}

test "pipeline timing aggregates Monotype diagnostics" {
    var timing = Timing.init(std.testing.io);
    var first: postcheck.Monotype.Lower.Diagnostics = .{};
    first.specialization.template_requests = 3;
    first.graph.nodes_created = 5;
    first.body.call_expressions = 7;
    timing.addMonotypeDiagnostics(first);

    var second: postcheck.Monotype.Lower.Diagnostics = .{};
    second.specialization.template_requests = 11;
    second.graph.nodes_created = 13;
    second.body.call_expressions = 17;
    timing.addMonotypeDiagnostics(second);

    const diagnostics = timing.snapshot().monotype_diagnostics;
    try std.testing.expectEqual(@as(u64, 14), diagnostics.specialization.template_requests);
    try std.testing.expectEqual(@as(u64, 18), diagnostics.graph.nodes_created);
    try std.testing.expectEqual(@as(u64, 24), diagnostics.body.call_expressions);
}

test "pipeline timing keeps aggregate Monotype worker work separate from wall time" {
    var timing = Timing.init(std.testing.io);
    timing.monotype_ns.add(97);
    timing.addMonotypeParallel(.{
        .worker_work_ns = 11,
        .coordinator_post_batch_work_ns = 12,
        .root_tasks_submitted = 13,
        .root_tasks_committed = 14,
        .specialization_tasks_submitted = 16,
        .specialization_tasks_committed = 17,
        .specialization_tasks_discarded_ready = 19,
        .task_waves = 20,
        .peak_worker_lanes_available = 4,
        .peak_worker_lanes_used = 3,
        .within_lowering_lane_reuse_tasks = 21,
        .peak_specialization_jobs_pending = 22,
        .peak_specialization_shards_retained = 23,
    });
    timing.addMonotypeParallel(.{
        .worker_work_ns = 31,
        .coordinator_post_batch_work_ns = 32,
        .root_tasks_submitted = 33,
        .root_tasks_committed = 34,
        .specialization_tasks_submitted = 36,
        .specialization_tasks_committed = 37,
        .specialization_tasks_discarded_ready = 39,
        .task_waves = 40,
        .peak_worker_lanes_available = 8,
        .peak_worker_lanes_used = 5,
        .within_lowering_lane_reuse_tasks = 41,
        .peak_specialization_jobs_pending = 42,
        .peak_specialization_shards_retained = 43,
    });
    timing.addSnapshot(.{ .boxy_plan_ns = 43, .boxy_lower_ns = 47 });

    const snapshot_value = timing.snapshot();
    const parallel = snapshot_value.monotype_parallel;
    try std.testing.expectEqual(@as(u64, 97), snapshot_value.monotype_ns);
    try std.testing.expectEqual(@as(u64, 42), parallel.worker_work_ns);
    try std.testing.expectEqual(@as(u64, 44), parallel.coordinator_post_batch_work_ns);
    try std.testing.expectEqual(@as(u64, 46), parallel.root_tasks_submitted);
    try std.testing.expectEqual(@as(u64, 48), parallel.root_tasks_committed);
    try std.testing.expectEqual(@as(u64, 52), parallel.specialization_tasks_submitted);
    try std.testing.expectEqual(@as(u64, 54), parallel.specialization_tasks_committed);
    try std.testing.expectEqual(@as(u64, 58), parallel.specialization_tasks_discarded_ready);
    try std.testing.expectEqual(@as(u64, 60), parallel.task_waves);
    try std.testing.expectEqual(@as(u64, 8), parallel.peak_worker_lanes_available);
    try std.testing.expectEqual(@as(u64, 5), parallel.peak_worker_lanes_used);
    try std.testing.expectEqual(@as(u64, 62), parallel.within_lowering_lane_reuse_tasks);
    try std.testing.expectEqual(@as(u64, 42), parallel.peak_specialization_jobs_pending);
    try std.testing.expectEqual(@as(u64, 43), parallel.peak_specialization_shards_retained);
    try std.testing.expectEqual(@as(u64, 43), snapshot_value.boxy_plan_ns);
    try std.testing.expectEqual(@as(u64, 47), snapshot_value.boxy_lower_ns);
}

/// Whether the root checked module is complete or inside checking finalization.
pub const CheckedModuleState = enum {
    complete,
    checking_finalization,
};

pub const RuntimeRecordFieldSchema = postcheck.SolvedLirLower.RuntimeRecordFieldSchema;
pub const RuntimeRecordSchema = postcheck.SolvedLirLower.RuntimeRecordSchema;
pub const RuntimeTagSchema = postcheck.SolvedLirLower.RuntimeTagSchema;
pub const RuntimeTagUnionSchema = postcheck.SolvedLirLower.RuntimeTagUnionSchema;
pub const InlineMode = postcheck.SolvedInline.Mode;
pub const SpecConstrCloneInlining = postcheck.MonotypeLifted.SpecConstr.CloneInlining;
pub const InlineExpectMode = postcheck.SolvedLirLower.InlineExpectMode;

/// One consumer's explicit share of a shared producer program: the producer
/// root positions it is responsible for, and whether it materializes the
/// producer's layout, static-data and runtime-schema requests.
pub const ConsumerRoots = postcheck.SolvedLirLower.RootManifest;

/// One evaluated root a consumer asks to have materialized into its program.
pub const CompletedValueRequest = postcheck.SolvedLirLower.CompletedValueRequest;

/// Command-level test-plan metadata keyed by producer root position.
pub const RootTestPlanMetadata = postcheck.Common.RootTestPlanMetadata;

/// Live destinations that belong to a lowering invocation rather than to the
/// producer it continues. Preparation captures semantics; these carry results
/// and workers back to whoever asked for this continuation.
pub const Observers = struct {
    work_metrics: ?*WorkMetrics = null,
    timing: ?*Timing = null,
    post_check_executor: ?base.post_check_task_executor.Executor = null,
    debug_materialized_out: ?*?postcheck.LambdaMono.Ast.Program = null,
    solved_lir_parallel_metrics_out: ?*SolvedLirParallelMetrics = null,
    lir_pass_parallel_metrics_out: ?*LirPassParallelMetrics = null,
    arc_parallel_metrics_out: ?*ArcParallelMetrics = null,
    lifted_expr_count_out: ?*usize = null,

    pub fn fromTarget(target: TargetConfig) Observers {
        var observers = Observers{};
        inline for (@typeInfo(Observers).@"struct".fields) |field| {
            @field(observers, field.name) = @field(target, field.name);
        }
        return observers;
    }

    fn applyTo(self: Observers, target: *TargetConfig) void {
        inline for (@typeInfo(Observers).@"struct".fields) |field| {
            @field(target, field.name) = @field(self, field.name);
        }
    }
};

/// One consumer continuation of a prepared solved program. Everything not
/// named here is the producer's captured decision and cannot be changed by a
/// consumer: preparation already lowered specializations under it.
pub const Consumer = struct {
    roots: ConsumerRoots,
    /// Target pointer width this continuation commits layouts for.
    target_usize: base.target.TargetUsize,
    /// This consumer's answer to the shared `inline_expects_enabled` input.
    inline_expects: InlineExpectMode,
    /// Completed compile-time scalar roots this consumer reads as literals.
    completed_scalar_values: ?*const CompletedScalarValues = null,
    /// Completed values produced by an earlier consumer, materialized in this
    /// consumer's representation after LIR generation and before reachability.
    /// The materializer receives the un-compacted target program so callable
    /// identities can resolve against its complete representation tables.
    frozen_materializer: ?FrozenMaterializer = null,
    observers: Observers = .{},
};

/// Materializes completed values into a program's frozen static data and
/// applies evaluation outcomes to the guards inserted around them.
pub const FrozenMaterializer = struct {
    context: *anyopaque,
    materialize: *const fn (Allocator, *anyopaque, *LirProgram.Result) Allocator.Error!LirProgram.FrozenStaticData,
    /// Apply evaluation outcomes after guard insertion. Successful values
    /// bypass their guards; failed values retain the emitted failure path.
    complete_guards: *const fn (*anyopaque, *LirProgram.Result) void,
};

/// Materialized Lambda Mono program type, re-exported for harnesses that
/// receive one through `TargetConfig.debug_materialized_out`.
pub const LambdaMonoProgram = postcheck.LambdaMono.Ast.Program;

/// Runtime record and tag-union schemas needed by dev tooling.
pub const RuntimeValueSchemaStore = struct {
    allocator: Allocator,
    records: std.ArrayList(RuntimeRecordSchema),
    tag_unions: std.ArrayList(RuntimeTagUnionSchema),

    pub fn init(allocator: Allocator) RuntimeValueSchemaStore {
        return .{
            .allocator = allocator,
            .records = .empty,
            .tag_unions = .empty,
        };
    }

    pub fn deinit(self: *RuntimeValueSchemaStore) void {
        for (self.records.items) |schema| {
            for (schema.fields) |field| self.allocator.free(field.name);
            self.allocator.free(schema.fields);
            self.allocator.free(schema.type_name);
        }
        for (self.tag_unions.items) |schema| {
            for (schema.tags) |tag| self.allocator.free(tag.name);
            self.allocator.free(schema.tags);
            self.allocator.free(schema.type_name);
        }
        self.tag_unions.deinit(self.allocator);
        self.records.deinit(self.allocator);
        self.* = RuntimeValueSchemaStore.init(self.allocator);
    }

    pub fn record(self: *const RuntimeValueSchemaStore, type_name: []const u8) RuntimeRecordSchema {
        for (self.records.items) |schema| {
            if (std.mem.eql(u8, schema.type_name, type_name)) return schema;
        }
        if (builtin.mode == .Debug) {
            std.debug.panic("runtime schema invariant violated: missing record schema for {s}", .{type_name});
        }
        unreachable;
    }

    pub fn tagUnion(self: *const RuntimeValueSchemaStore, type_name: []const u8) RuntimeTagUnionSchema {
        for (self.tag_unions.items) |schema| {
            if (std.mem.eql(u8, schema.type_name, type_name)) return schema;
        }
        if (builtin.mode == .Debug) {
            std.debug.panic("runtime schema invariant violated: missing tag union schema for {s}", .{type_name});
        }
        unreachable;
    }
};

/// Select the runtime roots by the producer-recorded positions in the shared
/// request list. One program serves both consumers when their target width and
/// expect mode agree, so this is how the runtime consumer takes its roots out
/// of the program the compile-time roots were evaluated in.
pub fn retainRuntimeRoots(lowered: *LoweredProgram, root_indices: []const u32) Allocator.Error!void {
    const allocator = lowered.lir_result.store.allocator;
    const result = &lowered.lir_result;
    var procs = try std.ArrayList(core.LIR.LirProcSpecId).initCapacity(allocator, root_indices.len);
    errdefer procs.deinit(allocator);
    var metadata = try std.ArrayList(@import("lir_core").RootMetadata.RootMetadata).initCapacity(allocator, root_indices.len);
    errdefer metadata.deinit(allocator);
    for (root_indices) |index| {
        if (index >= result.root_procs.items.len or index >= result.root_metadata.items.len) checkedPipelineInvariant("runtime root position exceeds shared root plan");
        procs.appendAssumeCapacity(result.root_procs.items[index]);
        metadata.appendAssumeCapacity(result.root_metadata.items[index]);
    }
    result.root_procs.deinit(allocator);
    result.root_metadata.deinit(allocator);
    result.root_procs = procs;
    result.root_metadata = metadata;
    procs = .empty;
    metadata = .empty;
    result.const_roots.clearRetainingCapacity();
    try completeComptimeValueSlots(lowered);
}

/// Turn a runtime consumer's completed compile-time value slots into ordinary
/// frozen program data.
///
/// This consumer lowered only its own roots, so nothing here selects roots. It
/// reaches each compile-time value through a slot the evaluation has since
/// filled, and its own roots are the program's roots already.
pub fn adoptCompletedComptimeValues(lowered: *LoweredProgram) Allocator.Error!void {
    if (lowered.lir_result.const_roots.items.len != 0) checkedPipelineInvariant("runtime consumer lowered a compile-time root");
    if (lowered.frozen_static_data == null) checkedPipelineInvariant("runtime consumer has no completed compile-time values to adopt");
    try completeComptimeValueSlots(lowered);
}

/// Adopt completed values whose frozen graph already participated in this
/// consumer's reachability pass. This is the separate-target continuation;
/// rerunning compaction here would repeat work after ARC and cannot discover
/// any new procedure edge.
pub fn adoptReachableCompletedComptimeValues(lowered: *LoweredProgram) Allocator.Error!void {
    if (lowered.lir_result.const_roots.items.len != 0) checkedPipelineInvariant("runtime consumer lowered a compile-time root");
    if (lowered.frozen_static_data == null) checkedPipelineInvariant("runtime consumer has no completed compile-time values to adopt");
    const result = &lowered.lir_result;
    result.comptime_value_guards.clearRetainingCapacity();
    for (lowered.frozen_static_data.?.exports) |item| {
        if (item.value_id) |id| result.static_data_values.items[@intFromEnum(id)].initializer = null;
    }
    for (result.static_data_values.items) |*value| value.compile_time_root = null;
}

/// The completed frozen bytes are now each compile-time value's definition, so
/// the slot keeps no initializer and no compile-time root identity, and the
/// program's procedures are compacted against the frozen graph's own
/// references. Completion has already rewritten successful failure guards;
/// remaining guard CFG belongs to failed roots and remains ordinary runtime LIR.
fn completeComptimeValueSlots(lowered: *LoweredProgram) Allocator.Error!void {
    const result = &lowered.lir_result;
    result.comptime_value_guards.clearRetainingCapacity();
    if (lowered.frozen_static_data) |*frozen| {
        for (frozen.exports) |item| {
            if (item.value_id) |id| result.static_data_values.items[@intFromEnum(id)].initializer = null;
        }
        for (result.static_data_values.items) |*value| value.compile_time_root = null;
        try ReachableProcs.runWithFrozen(result, frozen);
    } else {
        try ReachableProcs.run(result);
    }
    lowered.main_proc = if (result.root_procs.items.len == 0) null else result.root_procs.items[0];
}

/// Fully lowered LIR program plus root and runtime schema metadata.
pub const LoweredProgram = struct {
    lir_result: LirProgram.Result,
    main_proc: ?LIR.LirProcSpecId,
    target_usize: base.target.TargetUsize,
    runtime_value_schemas: RuntimeValueSchemaStore,
    frozen_static_data: ?LirProgram.FrozenStaticData = null,

    pub fn deinit(self: *LoweredProgram) void {
        if (self.frozen_static_data) |*data| data.deinit();
        self.runtime_value_schemas.deinit();
        self.lir_result.deinit();
    }

    /// Release this program's procedure bodies, keeping its completed values,
    /// their representation metadata, and its procedure inventory. A
    /// compile-time program whose values have been read is only that metadata
    /// from then on; see `LirStore.releaseCode`.
    pub fn releaseCode(self: *LoweredProgram) void {
        self.lir_result.store.releaseCode();
    }

    /// Host compilation selects only provided roots before lowering. Their
    /// emitted order is the common symbol/procedure/dispatch-ordinal mapping.
    pub fn platformEntrypoints(
        self: *const LoweredProgram,
        allocator: Allocator,
    ) Allocator.Error![]LirImage.PlatformEntrypoint {
        const root_procs = self.lir_result.root_procs.items;
        const root_metadata = self.lir_result.root_metadata.items;
        std.debug.assert(root_procs.len == root_metadata.len);

        const entrypoints = try allocator.alloc(LirImage.PlatformEntrypoint, root_procs.len);
        for (root_procs, root_metadata, entrypoints, 0..) |root_proc, metadata, *entrypoint, ordinal| {
            std.debug.assert(metadata.kind == .provided_export);
            std.debug.assert(metadata.abi == .platform and metadata.exposure == .exported);
            entrypoint.* = .{ .ordinal = @intCast(ordinal), .root_proc = root_proc };
        }
        return entrypoints;
    }

    /// Own the symbol strings: run images outlive their checked modules.
    pub fn platformEntrypointNames(
        self: *const LoweredProgram,
        allocator: Allocator,
        root_module: *const checked.Module,
    ) Allocator.Error![]const []const u8 {
        const root_metadata = self.lir_result.root_metadata.items;
        const names = try allocator.alloc([]const u8, root_metadata.len);
        var initialized: usize = 0;
        errdefer {
            for (names[0..initialized]) |name| allocator.free(name);
            allocator.free(names);
        }

        for (root_metadata, names) |metadata, *name| {
            std.debug.assert(metadata.kind == .provided_export);
            std.debug.assert(metadata.abi == .platform and metadata.exposure == .exported);
            const root = root_module.lookupRootRequestByOrder(metadata.order) orelse
                checkedPipelineInvariant("platform entrypoint root metadata has no checked root request");
            const symbol = root_module.providedEntrypointName(root) orelse
                checkedPipelineInvariant("platform entrypoint root metadata has no checked export declaration");
            name.* = try allocator.dupe(u8, symbol);
            initialized += 1;
        }
        return names;
    }
};

/// Owns the specialization result and its continuation configuration. Checked
/// module storage and pointer-valued TargetConfig outputs must outlive this value.
/// Target options are captured here: resuming cannot silently change the meaning
/// of specializations already lowered under those options.
pub const PreparedMonotype = struct {
    allocator: Allocator,
    program: postcheck.Monotype.Ast.Program,
    target: TargetConfig,
    root_count: usize,
    test_plan_metadata: []postcheck.Common.RootTestPlanMetadata,

    /// Fork only the target-dependent continuation. Specialization output is
    /// copied exactly; checked lowering and its executor are not run again.
    pub fn forkForTarget(self: *const PreparedMonotype, target_usize: base.target.TargetUsize) Allocator.Error!PreparedMonotype {
        return self.forkForConsumer(target_usize, self.target.inline_expects);
    }

    /// A shared program preserves both expect semantics explicitly. A program
    /// specialized for one mode cannot acquire the missing continuation later.
    pub fn forkForConsumer(self: *const PreparedMonotype, target_usize: base.target.TargetUsize, inline_expects: InlineExpectMode) Allocator.Error!PreparedMonotype {
        if (!self.target.comptime_value_reads and inline_expects != self.target.inline_expects) {
            checkedPipelineInvariant("changing expect mode requires shared Monotype lowering");
        }
        var program = try self.program.cloneFrozen(self.allocator);
        errdefer program.deinit();
        const metadata = try self.allocator.dupe(postcheck.Common.RootTestPlanMetadata, self.test_plan_metadata);
        var target = self.target;
        target.target_usize = target_usize;
        target.inline_expects = inline_expects;
        return .{
            .allocator = self.allocator,
            .program = program,
            .target = target,
            .root_count = self.root_count,
            .test_plan_metadata = metadata,
        };
    }

    pub fn deinit(self: *PreparedMonotype) void {
        self.program.deinit();
        self.allocator.free(self.test_plan_metadata);
        self.* = undefined;
    }
};

/// Lower checked modules and explicit roots directly into an ARC-ready LIR program.
pub fn lowerCheckedModulesToLir(
    allocator: Allocator,
    modules: CheckedModuleSet,
    roots: RootRequestSet,
    target: TargetConfig,
) LowerResourceError!LoweredProgram {
    if (target.specialization_strategy == .lss) {
        return lowerPreparedMonotypeToLir(try prepareCheckedModulesMonotype(allocator, modules, roots, target));
    }
    try verifyCheckedBoundary(modules, target);
    try requireHostedProceduresBound(modules, target);

    const layout_requests = try collectLayoutRequests(allocator, modules.root.module, roots.layout_requests, roots.include_provided_data_exports);
    defer allocator.free(layout_requests);
    const static_data_requests = try collectStaticDataRequests(
        allocator,
        modules.root.module,
        roots.static_data_requests,
        roots.include_provided_data_exports,
    );
    defer allocator.free(static_data_requests);

    return lowerBoxyCheckedModulesToLir(allocator, modules, roots, target, layout_requests, static_data_requests);
}

/// Lower the complete explicit root set once, retaining ownership at the
/// Monotype boundary. Boxy has no Monotype stage and uses the direct entrance.
pub fn prepareCheckedModulesMonotype(
    allocator: Allocator,
    modules: CheckedModuleSet,
    roots: RootRequestSet,
    target: TargetConfig,
) LowerResourceError!PreparedMonotype {
    if (target.specialization_strategy != .lss) checkedPipelineInvariant("Monotype preparation requires LSS");
    try verifyCheckedBoundary(modules, target);
    try requireHostedProceduresBound(modules, target);

    const layout_requests = try collectLayoutRequests(allocator, modules.root.module, roots.layout_requests, roots.include_provided_data_exports);
    defer allocator.free(layout_requests);
    const static_data_requests = try collectStaticDataRequests(
        allocator,
        modules.root.module,
        roots.static_data_requests,
        roots.include_provided_data_exports,
    );
    defer allocator.free(static_data_requests);

    const test_plan_metadata = try allocator.dupe(postcheck.Common.RootTestPlanMetadata, roots.test_plan_metadata);
    errdefer allocator.free(test_plan_metadata);
    const monotype_started_ns = if (target.timing) |timing| timing.start() else 0;
    var monotype_timing: ?postcheck.Monotype.Lower.Timing = if (target.timing) |timing|
        postcheck.Monotype.Lower.Timing.init(timing.std_io)
    else
        null;
    var monotype_diagnostics: ?postcheck.Monotype.Lower.Diagnostics = if (target.timing) |timing|
        if (timing.detailed_monotype_body) .{} else null
    else
        null;
    if (monotype_timing) |*detail| {
        detail.body_work_timing_enabled = target.timing.?.detailed_monotype_body;
    }
    const mono = monotype: {
        defer if (target.timing) |timing| {
            timing.finish(monotype_started_ns, .monotype);
            if (monotype_timing) |*detail| timing.addMonotypeSnapshot(detail.snapshot());
            if (monotype_diagnostics) |diagnostics| timing.addMonotypeDiagnostics(diagnostics);
        };
        if (target.work_metrics) |metrics| metrics.monotype_runs += 1;
        break :monotype try postcheck.Monotype.Lower.run(
            allocator,
            checkedModules(modules),
            rootRequests(roots, layout_requests, static_data_requests),
            .{
                .proc_debug_names = target.proc_debug_names or LirDump.filter() != null or SpecCensus.enabled(),
                // A program that is also the compile-time evaluator's host
                // takes its hits in Direct LIR, after the compile-time
                // closure is known; only a runtime-only program can take
                // them here.
                .spec_cache = if (target.checked_module_state == .complete) target.spec_cache else null,
                .post_check_executor = target.post_check_executor,
                .static_data_literals = target.checked_module_state == .checking_finalization or roots.include_internal_static_data,
                .comptime_value_reads = target.comptime_value_reads,
                .target_usize = target.target_usize,
                .inline_expects = if (target.comptime_value_reads) .shared else switch (target.inline_expects) {
                    .run => .run,
                    .omit => .omit,
                },
                .timing = if (monotype_timing) |*timing| timing else null,
                .diagnostics = if (monotype_diagnostics) |*diagnostics| diagnostics else null,
            },
        );
    };
    if (SpecCensus.enabled()) try SpecCensus.runMonotype(allocator, modules, &mono);
    var prepared_target = target;
    prepared_target.comptime_closure_hits = comptimeClosureHitsAllowed(modules);
    return .{
        .allocator = allocator,
        .program = mono,
        .target = prepared_target,
        .root_count = roots.requests.len,
        .test_plan_metadata = test_plan_metadata,
    };
}

/// Whether every exhaustiveness site of the program resolves without the
/// evaluator observing the branch it takes. A site reachable only at compile
/// time is resolved by the evaluation reaching it, which a spliced
/// object-cache entry never reports.
fn comptimeClosureHitsAllowed(modules: CheckedModuleSet) bool {
    if (hasCompileTimeOnlySite(&modules.root.module.exhaustiveness_sites)) return false;
    for (modules.root.relation_modules) |relation| {
        if (hasCompileTimeOnlySite(relation.exhaustiveness_sites)) return false;
    }
    for (modules.imports) |import| {
        if (hasCompileTimeOnlySite(import.exhaustiveness_sites)) return false;
    }
    return true;
}

fn hasCompileTimeOnlySite(sites: *const checked.CheckedExhaustivenessSiteTable) bool {
    for (sites.sites) |site| {
        switch (site.policy) {
            .compile_time_only => return true,
            .compile_time_replaced_by_root, .runtime_reachable, .not_pending => {},
        }
    }
    return false;
}

/// Consumes the prepared program on success and failure. No specialization
/// lowering is repeated, and all continuation options come from preparation.
pub fn prepareMonotypeToSolved(prepared: PreparedMonotype) Allocator.Error!PreparedSolved {
    const allocator = prepared.allocator;
    const target = prepared.target;
    if (target.work_metrics) |metrics| metrics.solved_runs += 1;
    errdefer allocator.free(prepared.test_plan_metadata);
    var mono = prepared.program;
    var mono_owned = true;
    errdefer if (mono_owned) mono.deinit();

    var lift_timing_scope = PipelineTimingScope.begin(target.timing, .lift);
    defer lift_timing_scope.end();

    // Each post-check transform consumes its input even when it returns an
    // error. Transfer ownership before entering the transform so its cleanup
    // and this function's cleanup can never both deinitialize the same IR.
    const mono_input = mono;
    mono_owned = false;
    mono = undefined;
    var lifted = try postcheck.MonotypeLifted.Lift.run(allocator, mono_input);
    var lifted_owned = true;
    errdefer if (lifted_owned) lifted.deinit();
    lift_timing_scope.end();

    var local_spec_constr_metrics: SpecConstrParallelMetrics = .{};
    const spec_constr_metrics = specConstrMetricsOutput(target, &local_spec_constr_metrics);
    const spec_constr_options: postcheck.MonotypeLifted.SpecConstr.Options = .{
        .executor = target.post_check_executor,
        .metrics_out = spec_constr_metrics,
    };
    var procedure_usage = if (target.inline_mode != .none) blk: {
        var spec_constr_timing_scope = PipelineTimingScope.begin(target.timing, .spec_constr);
        defer spec_constr_timing_scope.end();
        const usage = try postcheck.MonotypeLifted.SpecConstr.runAndCollectProcedureUsageWithOptions(allocator, &lifted, target.spec_constr_clone_inlining, spec_constr_options);
        spec_constr_timing_scope.end();
        break :blk usage;
    } else blk: {
        const spec_constr_started_ns = if (target.timing) |timing| timing.start() else 0;
        try postcheck.MonotypeLifted.SpecConstr.runIteratorFusionWithOptions(allocator, &lifted, spec_constr_options);
        if (target.timing) |timing| timing.finish(spec_constr_started_ns, .spec_constr);
        break :blk postcheck.MonotypeLifted.SpecConstr.OwnedProcedureUsage.empty(allocator);
    };
    defer procedure_usage.deinit();
    if (target.timing) |timing| timing.addSpecConstrParallel(spec_constr_metrics.?.*);

    const lifted_expr_count = lifted.exprCount();
    if (target.lifted_expr_count_out) |slot| slot.* = lifted_expr_count;
    if (SpecCensus.enabled()) SpecCensus.runLifted(&lifted);

    var lambda_solve_timing_scope = PipelineTimingScope.begin(target.timing, .lambda_solve);
    defer lambda_solve_timing_scope.end();
    const lifted_input = lifted;
    lifted_owned = false;
    lifted = undefined;
    var solved = try postcheck.LambdaSolved.Solve.run(allocator, lifted_input);
    errdefer solved.deinit();
    lambda_solve_timing_scope.end();

    var inline_plan_timing_scope = PipelineTimingScope.begin(
        if (target.inline_mode != .none) target.timing else null,
        .inline_plan,
    );
    defer inline_plan_timing_scope.end();
    const inline_plan = try postcheck.SolvedInline.analyze(allocator, target.inline_mode, procedure_usage.view(), &solved, target.keep_specialization_procs);
    inline_plan_timing_scope.end();

    return .{
        .allocator = allocator,
        .program = solved,
        .inline_plan = inline_plan,
        .target = target,
        .root_count = prepared.root_count,
        .test_plan_metadata = prepared.test_plan_metadata,
        .lifted_expr_count = lifted_expr_count,
    };
}

/// Owns one solved producer identity domain and its consumer continuation data.
pub const PreparedSolved = struct {
    allocator: Allocator,
    program: postcheck.LambdaSolved.Ast.Program,
    inline_plan: postcheck.SolvedInline.OwnedPlan,
    target: TargetConfig,
    root_count: usize,
    test_plan_metadata: []postcheck.Common.RootTestPlanMetadata,
    lifted_expr_count: usize,

    /// The evaluated roots this producer program records reads of. Whoever
    /// materializes completed values materializes these and no others.
    pub fn comptimeValueReads(self: *const PreparedSolved) []const postcheck.Common.ComptimeValueRoot {
        return self.program.lifted.comptimeValueReadsView();
    }

    /// Release the retained solved program and its owned continuation metadata.
    pub fn deinit(self: *PreparedSolved) void {
        self.program.deinit();
        self.inline_plan.deinit();
        self.allocator.free(self.test_plan_metadata);
        self.* = undefined;
    }
};

/// Preserve the immediate lowering API while sharing the solved preparation path.
pub fn lowerPreparedMonotypeToLir(prepared: PreparedMonotype) LowerResourceError!LoweredProgram {
    return lowerPreparedSolvedToLir(try prepareMonotypeToSolved(prepared));
}

/// Consume a solved owner into one LIR continuation over its whole program.
pub fn lowerPreparedSolvedToLir(prepared: PreparedSolved) LowerResourceError!LoweredProgram {
    // Naming the whole root plan allocates nothing, so ownership of the
    // prepared program transfers with no step in between that could fail.
    return lowerFinalConsumerToLir(prepared, .{
        .roots = .{},
        .target_usize = prepared.target.target_usize,
        .inline_expects = prepared.target.inline_expects,
        .completed_scalar_values = prepared.target.completed_scalar_values,
        .observers = Observers.fromTarget(prepared.target),
    });
}

/// Lower one consumer's share of a shared solved producer program.
///
/// The producer program is borrowed, not copied: its specialization,
/// lambda-solving and inline decisions are one immutable identity domain that
/// every consumer reads. A consumer names the producer roots it is
/// responsible for, and demand discovery starts from those alone, so no
/// consumer generates code for another's roots. Use this only while a later
/// consumer still needs the producer; the last one should release it.
pub fn lowerConsumerToLir(prepared: *PreparedSolved, consumer: Consumer) LowerResourceError!LoweredProgram {
    var generated = try generateConsumerLir(prepared, consumer);
    errdefer generated.output.deinit();
    var frozen = if (consumer.frozen_materializer) |materializer|
        try materializer.materialize(prepared.allocator, materializer.context, &generated.output.lir_result)
    else
        null;
    errdefer if (frozen) |*data| data.deinit();
    return finishLoweredOutput(prepared.allocator, consumerRootCount(prepared.*, consumer), generated.target, &generated.output, &frozen, consumer.frozen_materializer);
}

/// Lower the producer program's last consumer, and release the producer as
/// soon as LIR generation stops reading it.
///
/// Nothing after LIR generation consults the producer, and the procedure
/// passes, ARC and the emitted program are where a continuation's footprint
/// peaks, so holding the specialized program across them would hold two large
/// programs at once for no reader.
pub fn lowerFinalConsumerToLir(prepared: PreparedSolved, consumer: Consumer) LowerResourceError!LoweredProgram {
    var owned = prepared;
    var owned_live = true;
    errdefer if (owned_live) owned.deinit();
    var generated = try generateConsumerLir(&owned, consumer);
    errdefer generated.output.deinit();
    const allocator = owned.allocator;
    const root_count = consumerRootCount(owned, consumer);
    const target = generated.target;
    var frozen = if (consumer.frozen_materializer) |materializer|
        try materializer.materialize(allocator, materializer.context, &generated.output.lir_result)
    else
        null;
    errdefer if (frozen) |*data| data.deinit();
    owned_live = false;
    owned.deinit();
    return finishLoweredOutput(allocator, root_count, target, &generated.output, &frozen, consumer.frozen_materializer);
}

/// How many roots this consumer lowers: its own share, or the producer's
/// whole root plan when it named no share.
fn consumerRootCount(prepared: PreparedSolved, consumer: Consumer) usize {
    return if (consumer.roots.roots) |positions| positions.len else prepared.root_count;
}

/// One consumer's generated LIR, before the procedure passes and ARC, plus
/// the resolved target the rest of that continuation runs under.
const GeneratedConsumerLir = struct {
    output: postcheck.SolvedLirLower.Output,
    target: TargetConfig,
};

fn generateConsumerLir(prepared: *PreparedSolved, consumer: Consumer) LowerResourceError!GeneratedConsumerLir {
    const allocator = prepared.allocator;
    var target = prepared.target;
    target.target_usize = consumer.target_usize;
    target.inline_expects = consumer.inline_expects;
    target.completed_scalar_values = consumer.completed_scalar_values;
    consumer.observers.applyTo(&target);
    if (!prepared.target.comptime_value_reads and consumer.inline_expects != prepared.target.inline_expects) {
        checkedPipelineInvariant("changing expect mode requires shared Monotype lowering");
    }
    if (consumer.roots.roots) |positions| {
        for (positions) |position| {
            if (position >= prepared.root_count) checkedPipelineInvariant("consumer root manifest named a position outside the producer's root plan");
        }
    }
    if (target.work_metrics) |metrics| metrics.lir_continuations += 1;
    if (target.lifted_expr_count_out) |slot| slot.* = prepared.lifted_expr_count;
    var lir_gen_timing_scope = PipelineTimingScope.begin(target.timing, .lir_gen);
    defer lir_gen_timing_scope.end();
    var local_parallel_metrics: SolvedLirParallelMetrics = .{};
    const parallel_metrics = solvedLirMetricsOutput(target, &local_parallel_metrics);
    const lowered = try postcheck.SolvedLirLower.runBorrowed(allocator, target.target_usize, &prepared.program, .{
        .root_manifest = consumer.roots,
        .spec_cache = target.spec_cache,
        .comptime_closure_hits = target.comptime_closure_hits,
        .inline_plan = prepared.inline_plan.view(),
        .post_check_executor = target.post_check_executor,
        .inline_expects = target.inline_expects,
        .list_in_place_map = target.list_in_place_map,
        .dict_seed_mode = if (target.comptime_value_reads) .runtime else switch (target.checked_module_state) {
            .complete => .runtime,
            .checking_finalization => .comptime_zero,
        },
        .proc_debug_names = target.proc_debug_names or LirDump.filter() != null or SpecCensus.enabled(),
        .layout_request_const_plans = target.layout_request_const_plans,
        .test_plan_metadata = prepared.test_plan_metadata,
        .debug_materialized_out = target.debug_materialized_out,
        .parallel_metrics = parallel_metrics,
        .completed_scalar_values = target.completed_scalar_values,
    });
    if (target.timing) |timing| timing.addSolvedLirParallel(parallel_metrics.?.*);
    lir_gen_timing_scope.end();

    return .{ .output = lowered, .target = target };
}

/// The lowerer owns resetting its per-run output. Prefer the caller's slot so
/// collecting aggregate timings neither resets nor overwrites it a second time.
fn solvedLirMetricsOutput(target: TargetConfig, local: *SolvedLirParallelMetrics) ?*SolvedLirParallelMetrics {
    return target.solved_lir_parallel_metrics_out orelse
        if (target.timing != null) local else null;
}

/// SpecConstr alone resets per-run counters, including caller-owned output.
fn specConstrMetricsOutput(target: TargetConfig, local: *SpecConstrParallelMetrics) ?*SpecConstrParallelMetrics {
    return target.spec_constr_parallel_metrics_out orelse
        if (target.timing != null) local else null;
}

/// ARC insertion alone resets per-run counters, including caller-owned output.
fn arcMetricsOutput(target: TargetConfig, local: *ArcParallelMetrics) ?*ArcParallelMetrics {
    return target.arc_parallel_metrics_out orelse
        if (target.timing != null) local else null;
}

fn runProcedurePass(
    allocator: Allocator,
    result: *LirProgram.Result,
    target: TargetConfig,
    phase: ProcPasses.Phase,
) Allocator.Error!void {
    try ProcPasses.run(
        allocator,
        &result.store,
        &result.layouts,
        phase,
        target.post_check_executor,
        target.lir_pass_parallel_metrics_out,
    );
}

fn finishLoweredOutput(
    allocator: Allocator,
    root_count: usize,
    target: TargetConfig,
    lowered: anytype,
    frozen: *?LirProgram.FrozenStaticData,
    frozen_materializer: ?FrozenMaterializer,
) LowerResourceError!LoweredProgram {
    verifyArithmeticBoundary(&lowered.lir_result.store, false);
    var lir_passes_timing_scope = PipelineTimingScope.begin(target.timing, .lir_passes);
    defer lir_passes_timing_scope.end();
    var local_pass_metrics: LirPassParallelMetrics = .{};
    var pass_target = target;
    pass_target.lir_pass_parallel_metrics_out = target.lir_pass_parallel_metrics_out orelse
        if (target.timing != null) &local_pass_metrics else null;
    if (pass_target.lir_pass_parallel_metrics_out) |metrics| metrics.* = .{};
    defer if (target.timing) |timing| {
        if (pass_target.lir_pass_parallel_metrics_out) |metrics| timing.addLirPassParallel(metrics.*);
    };

    // TRMC/TCE must rewrite recursive procs before ARC insertion: it deletes
    // calls and changes allocation sites, and ARC panics on pre-existing RC
    // statements (see src/lir/trmc.zig).
    try runProcedurePass(allocator, &lowered.lir_result, pass_target, .trmc);
    if (target.specialization_strategy == .lss) {
        if (target.inline_mode == .none) {
            try SingleUseInline.run(&lowered.lir_result);
            try runProcedurePass(allocator, &lowered.lir_result, pass_target, .forwarding_join);
        }
        // Every inline mode produces tag-valued joins whose body matches the
        // tag at once: the iterator-fusion clone in `.none`, and the checked
        // wrappers substituted at their call sites in `.wrappers`, whose
        // `Try` results the caller matches immediately.
        if (target.fuse_tag_cases) {
            try runProcedurePass(allocator, &lowered.lir_result, pass_target, .tag_fusion);
        }
    }
    if (target.scalarize_joins) {
        try runProcedurePass(allocator, &lowered.lir_result, pass_target, .scalarize);
    }
    if (target.promote_loop_appends) {
        try runProcedurePass(allocator, &lowered.lir_result, pass_target, .loop_append);
    }
    verifyArithmeticBoundary(&lowered.lir_result.store, true);
    if (target.prove_ranges) {
        try runProcedurePass(allocator, &lowered.lir_result, pass_target, .range);
    }
    if (target.reuse_boxes) {
        try runProcedurePass(allocator, &lowered.lir_result, pass_target, .box_reuse);
    }
    try ReturnSlot.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
    try StrAppend.run(&lowered.lir_result.store);
    if (target.tag_reachability) {
        try TagReachability.run(&lowered.lir_result);
    }
    if (target.keep_specialization_procs) {
        if (frozen.*) |*data| {
            try ReachableProcs.runKeepingSpecializationsWithFrozen(&lowered.lir_result, data);
        } else {
            try ReachableProcs.runKeepingSpecializations(&lowered.lir_result);
        }
    } else {
        if (frozen.*) |*data| {
            try ReachableProcs.runWithFrozen(&lowered.lir_result, data);
        } else {
            try ReachableProcs.run(&lowered.lir_result);
        }
    }
    lir_passes_timing_scope.end();

    var arc_timing_scope = PipelineTimingScope.begin(target.timing, .arc);
    defer arc_timing_scope.end();
    var local_arc_metrics: ArcParallelMetrics = .{};
    const arc_metrics = arcMetricsOutput(target, &local_arc_metrics);
    var arc_roots = std.ArrayList(LIR.LirProcSpecId).empty;
    defer arc_roots.deinit(allocator);
    try arc_roots.appendSlice(allocator, lowered.lir_result.root_procs.items);
    if (frozen.*) |data| {
        var seen = try allocator.alloc(bool, lowered.lir_result.store.procSpecCount());
        defer allocator.free(seen);
        @memset(seen, false);
        for (arc_roots.items) |proc| seen[@intFromEnum(proc)] = true;
        for (data.exports) |export_| for (export_.relocations) |relocation| {
            const proc = relocation.procedure orelse continue;
            if (seen[@intFromEnum(proc)]) continue;
            seen[@intFromEnum(proc)] = true;
            try arc_roots.append(allocator, proc);
        };
    }
    try Arc.insert(&lowered.lir_result.store, &lowered.lir_result.layouts, .{
        .roots = arc_roots.items,
        .specialize = target.inline_mode != .none,
        .consume_dead_boxes = target.consume_dead_boxes,
        .post_check_executor = if (target.post_check_executor) |*executor| executor else null,
        .metrics_out = arc_metrics,
    });
    if (target.timing) |timing| timing.addArcParallel(arc_metrics.?.*);
    arc_timing_scope.end();

    // After the certifier has checked ARC's ledger, so that what it verified
    // is the placement ARC produced.
    _ = try ImmortalLocals.elide(allocator, &lowered.lir_result.store);

    try @import("comptime_value_guards.zig").insert(allocator, &lowered.lir_result);
    if (frozen_materializer) |materializer| {
        materializer.complete_guards(materializer.context, &lowered.lir_result);
    }

    try LirDump.run(&lowered.lir_result);
    if (SpecCensus.enabled()) try SpecCensus.runLir(allocator, &lowered.lir_result);

    if (root_count != 0 and lowered.lir_result.root_procs.items.len == 0) {
        checkedPipelineInvariant("explicit root set produced no LIR roots");
    }

    const main_proc: ?LIR.LirProcSpecId = if (lowered.lir_result.root_procs.items.len == 0)
        null
    else
        lowered.lir_result.root_procs.items[0];
    const runtime_value_schemas = convertRuntimeSchemas(allocator, lowered.runtime_schemas);
    lowered.runtime_schemas = postcheck.SolvedLirLower.RuntimeSchemaStore.init(allocator);
    errdefer runtime_value_schemas.deinit();

    const lir_result = lowered.lir_result;
    lowered.lir_result = undefined;

    return .{
        .lir_result = lir_result,
        .main_proc = main_proc,
        .target_usize = target.target_usize,
        .runtime_value_schemas = runtime_value_schemas,
        .frozen_static_data = frozen.*,
    };
}

fn lowerBoxyCheckedModulesToLir(
    allocator: Allocator,
    modules: CheckedModuleSet,
    roots: RootRequestSet,
    target: TargetConfig,
    layout_requests: []const checked.CheckedTypeId,
    static_data_requests: []const postcheck.Common.StaticDataRequest,
) LowerResourceError!LoweredProgram {
    var boxy_plan_timing_scope = PipelineTimingScope.begin(target.timing, .boxy_plan);
    defer boxy_plan_timing_scope.end();
    var boxy_layout_requests = std.ArrayList(checked.CheckedTypeId).empty;
    defer boxy_layout_requests.deinit(allocator);
    try boxy_layout_requests.appendSlice(allocator, layout_requests);

    var plan = try postcheck.Boxy.Plan.analyzeProgram(allocator, .{
        .root_module = modules.root,
        .imports = modules.imports,
        .roots = roots.requests,
        .source_modules = roots.source_modules,
        .layout_requests = boxy_layout_requests.items,
        .static_data_requests = static_data_requests,
    }, .{});
    defer plan.deinit();
    boxy_plan_timing_scope.end();

    var boxy_lower_timing_scope = PipelineTimingScope.begin(target.timing, .boxy_lower);
    defer boxy_lower_timing_scope.end();
    var lowered = try postcheck.Boxy.Lower.run(
        allocator,
        checkedModules(modules),
        rootRequests(roots, layout_requests, static_data_requests),
        &plan,
        .{
            .target_usize = target.target_usize,
            .list_in_place_map = target.list_in_place_map,
            .proc_debug_names = target.proc_debug_names,
            .observe_expects = roots.test_plan_metadata.len != 0,
        },
    );
    errdefer lowered.deinit();
    boxy_lower_timing_scope.end();

    var frozen: ?LirProgram.FrozenStaticData = null;
    return finishLoweredOutput(allocator, roots.requests.len, target, &lowered, &frozen, null);
}

fn verifyArithmeticBoundary(store: *const core.LirStore, before_prover: bool) void {
    if (builtin.mode != .Debug) return;
    for (store.getCFStmts()) |stmt| {
        if (stmt != .assign_low_level) continue;
        const op = stmt.assign_low_level.op;
        if (CheckedArithmetic.isSourcePolicyOp(op)) {
            checkedPipelineInvariant("source-policy arithmetic operation reached LIR");
        }
        if (before_prover) {
            if (CheckedArithmetic.classify(op)) |entry| {
                if (entry.mode == .proven_cannot_overflow) {
                    checkedPipelineInvariant("proven integer arithmetic existed before range proving");
                }
            }
        }
    }
}

fn verifyCheckedBoundary(modules: CheckedModuleSet, target: TargetConfig) Allocator.Error!void {
    if (builtin.mode != .Debug) return;
    switch (target.checked_module_state) {
        .complete => try modules.root.module.verifyComplete(),
        .checking_finalization => modules.root.module.verifyReadyForCompileTimeLowering(),
    }
}

/// Reject a checked program whose platform header left one of its hosted
/// declarations out of the hosted section.
///
/// The section is the complete list of functions the host supplies, and it is
/// what gives each one its external symbol and its host dispatch slot. A
/// declaration the section never names has neither, so a call to it has no
/// symbol to reach and lowering has nothing to emit. Checking reports that
/// declaration against the section it is missing from, so the compile has
/// already failed by the time lowering starts; this stops it there instead of
/// lowering a call that names no host function.
///
/// A platform module publishes its bindings when its checked artifact is
/// published, so a module still being checked has none to bind against and
/// this reads nothing into their absence.
fn requireHostedProceduresBound(
    modules: CheckedModuleSet,
    target: TargetConfig,
) HostedBindingError!void {
    switch (target.checked_module_state) {
        .complete => {},
        .checking_finalization => return,
    }

    const root_view = checked.importedView(modules.root.module);
    const bindings = platformHostedBindings(root_view, modules) orelse return;

    if (!hostedProceduresBound(root_view, bindings)) return error.HostedFunctionNotBound;
    for (modules.imports) |imported| {
        if (!hostedProceduresBound(imported, bindings)) return error.HostedFunctionNotBound;
    }
    for (modules.root.relation_modules) |relation| {
        if (!hostedProceduresBound(relation, bindings)) return error.HostedFunctionNotBound;
    }
}

/// The hosted bindings of the one platform module visible to this lowering, or
/// null when no platform module is in scope and so no section binds anything.
fn platformHostedBindings(
    root_view: checked.ImportedModuleView,
    modules: CheckedModuleSet,
) ?[]const checked.HostedBinding {
    if (root_view.module_identity.kind == .platform) return root_view.hosted_bindings.bindings;
    for (modules.imports) |imported| {
        if (imported.module_identity.kind == .platform) return imported.hosted_bindings.bindings;
    }
    for (modules.root.relation_modules) |relation| {
        if (relation.module_identity.kind == .platform) return relation.hosted_bindings.bindings;
    }
    return null;
}

fn hostedProceduresBound(
    view: checked.ImportedModuleView,
    bindings: []const checked.HostedBinding,
) bool {
    for (view.hosted_procs.procs) |proc| {
        var bound = false;
        for (bindings) |binding| {
            if (!std.mem.eql(u8, &binding.target_checked_module.bytes, &view.key.bytes)) continue;
            if (binding.target_def != proc.def_idx) continue;
            bound = true;
            break;
        }
        if (!bound) return false;
    }
    return true;
}

fn checkedModules(modules: CheckedModuleSet) postcheck.Common.CheckedModules {
    return .{
        .root = modules.root,
        .imports = modules.imports,
    };
}

fn rootRequests(
    roots: RootRequestSet,
    layout_requests: []const checked.CheckedTypeId,
    static_data_requests: []const postcheck.Common.StaticDataRequest,
) postcheck.Common.RootRequests {
    return .{
        .requests = roots.requests,
        .source_modules = roots.source_modules,
        .layout_requests = layout_requests,
        .static_data_requests = static_data_requests,
        .test_plan_metadata = roots.test_plan_metadata,
    };
}

fn collectLayoutRequests(
    allocator: Allocator,
    _: *const checked.Module,
    explicit: []const checked.CheckedTypeId,
    _: bool,
) Allocator.Error![]checked.CheckedTypeId {
    var requests = std.ArrayList(checked.CheckedTypeId).empty;
    errdefer requests.deinit(allocator);

    try requests.appendSlice(allocator, explicit);
    return try requests.toOwnedSlice(allocator);
}

/// Select ABI roots for native object/archive/shared-library outputs.
pub fn selectPlatformExportRoots(
    allocator: Allocator,
    requests: []const checked.RootRequest,
) Allocator.Error![]checked.RootRequest {
    var selected = std.ArrayList(checked.RootRequest).empty;
    errdefer selected.deinit(allocator);

    for (requests) |request| {
        if (request.kind != .provided_export) continue;
        try selected.append(allocator, request);
    }

    return try selected.toOwnedSlice(allocator);
}

/// Host shims and linked outputs have the same checked export roots.
pub const selectPlatformEntrypointRoots = selectPlatformExportRoots;

fn collectStaticDataRequests(
    allocator: Allocator,
    root: *const checked.Module,
    explicit: []const postcheck.Common.StaticDataRequest,
    include_provided: bool,
) Allocator.Error![]postcheck.Common.StaticDataRequest {
    var requests = std.ArrayList(postcheck.Common.StaticDataRequest).empty;
    errdefer requests.deinit(allocator);

    try requests.appendSlice(allocator, explicit);

    if (!include_provided) return try requests.toOwnedSlice(allocator);

    for (root.provided_exports.exports) |provided| {
        switch (provided) {
            .data => |data| {
                try requests.append(allocator, .{
                    .const_locator = data.const_ref,
                    .checked_type = data.checked_type,
                });
            },
            .procedure => {},
        }
    }

    return try requests.toOwnedSlice(allocator);
}

fn convertRuntimeSchemas(
    allocator: Allocator,
    input: postcheck.SolvedLirLower.RuntimeSchemaStore,
) RuntimeValueSchemaStore {
    return .{
        .allocator = allocator,
        .records = input.records,
        .tag_unions = input.tag_unions,
    };
}

fn checkedPipelineInvariant(comptime message: []const u8) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic("checked pipeline invariant violated: {s}", .{message});
    }
    unreachable;
}

test "checked pipeline declarations are referenced" {
    std.testing.refAllDecls(@This());
}

/// Print the final LIR of every procedure whose debug name contains
/// `ROC_LIR_DUMP`, for reading what the passes actually produced. Off unless
/// the variable is set, and absent on targets without an environment.
/// Printing the final LIR of selected procedures, for reading what the passes
/// actually produced. Selected by comptime target so a build without an
/// environment or a standard error stream carries none of it.
const LirDump = if (builtin.os.tag == .freestanding) struct {
    fn filter() ?[]const u8 {
        return null;
    }

    fn run(_: *const LirProgram.Result) Allocator.Error!void {}
} else struct {
    fn filter() ?[]const u8 {
        const raw = std.c.getenv("ROC_LIR_DUMP") orelse return null;
        return std.mem.span(raw);
    }

    fn run(result: *const LirProgram.Result) Allocator.Error!void {
        const name_filter = filter() orelse return;
        const store = &result.store;
        const layouts = &result.layouts;
        for (0..store.procSpecCount()) |index| {
            const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
            const name = store.procDebugName(proc_id);
            if (name_filter.len != 0) {
                const named = name orelse continue;
                if (std.mem.find(u8, named, name_filter) == null) continue;
            }
            var buffer: std.Io.Writer.Allocating = .init(store.allocator);
            defer buffer.deinit();
            DebugPrint.writeProc(store.allocator, store, layouts, proc_id, &buffer.writer) catch |err| switch (err) {
                error.OutOfMemory, error.WriteFailed => return error.OutOfMemory,
            };
            std.debug.print("=== LIR {s} (p{d}) ===\n{s}\n", .{ name orelse "<unnamed>", index, buffer.written() });
        }
    }
};

test "runtime extraction consumes producer root positions and preserves their order" {
    const allocator = std.testing.allocator;
    var lowered = LoweredProgram{
        .lir_result = try LirProgram.Result.init(allocator, base.target.TargetUsize.native),
        .main_proc = null,
        .target_usize = base.target.TargetUsize.native,
        .runtime_value_schemas = RuntimeValueSchemaStore.init(allocator),
    };
    defer lowered.deinit();
    const local = try lowered.lir_result.store.addLocal(.{ .layout_idx = .zst });
    const ret = try lowered.lir_result.store.addCFStmt(.{ .ret = .{ .value = local } }, .test_fixture);
    for (0..3) |index| {
        const proc = try lowered.lir_result.store.addProcSpec(.{
            .name = lowered.lir_result.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.forTest(1),
            .args = .empty(),
            .body = ret,
            .ret_layout = .zst,
        }, .none);
        try lowered.lir_result.root_procs.append(allocator, proc);
        try lowered.lir_result.root_metadata.append(allocator, .{
            .order = @intCast(index),
            .kind = if (index == 0) .compile_time_constant else .runtime_entrypoint,
            .abi = .roc,
            .exposure = .private,
        });
    }
    try retainRuntimeRoots(&lowered, &.{ 2, 1 });
    try std.testing.expectEqual(@as(usize, 2), lowered.lir_result.store.procSpecCount());
    try std.testing.expectEqual(@as(u32, 2), lowered.lir_result.root_metadata.items[0].order);
    try std.testing.expectEqual(@as(u32, 1), lowered.lir_result.root_metadata.items[1].order);
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(lowered.lir_result.root_procs.items[0]));
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(lowered.lir_result.root_procs.items[1]));
    try std.testing.expectEqual(lowered.lir_result.root_procs.items[0], lowered.main_proc.?);
}

test "adopting completed compile-time values drops their initializers and identity" {
    const allocator = std.testing.allocator;
    var lowered = LoweredProgram{
        .lir_result = try LirProgram.Result.init(allocator, base.target.TargetUsize.native),
        .main_proc = null,
        .target_usize = base.target.TargetUsize.native,
        .runtime_value_schemas = RuntimeValueSchemaStore.init(allocator),
    };
    defer lowered.deinit();
    const local = try lowered.lir_result.store.addLocal(.{ .layout_idx = .zst });
    const ret = try lowered.lir_result.store.addCFStmt(.{ .ret = .{ .value = local } }, .test_fixture);
    var procs: [2]LIR.LirProcSpecId = undefined;
    for (&procs, 0..) |*proc, index| {
        proc.* = try lowered.lir_result.store.addProcSpec(.{
            .name = lowered.lir_result.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.forTest(@intCast(index + 1)),
            .args = .empty(),
            .body = ret,
            .ret_layout = .zst,
        }, .none);
    }
    // One runtime root, and one slot whose value the evaluation completed:
    // its initializer is the only reference to the second procedure.
    try lowered.lir_result.root_procs.append(allocator, procs[0]);
    try lowered.lir_result.root_metadata.append(allocator, .{
        .order = 0,
        .kind = .runtime_entrypoint,
        .abi = .roc,
        .exposure = .private,
    });
    const completed_slot: LIR.StaticDataId = @enumFromInt(lowered.lir_result.static_data_values.items.len);
    try lowered.lir_result.static_data_values.append(allocator, .{
        .initializer = procs[1],
        .layout_idx = .zst,
        .compile_time_root = .{
            .module = .{ .bytes = @splat(0) },
            .root = undefined, // Adoption reads the slot's initializer and role, never its checked-root identity.
            .const_locator = null,
            .role = .{ .failure_message = .{ .failed_field = 0, .message_field = 1, .failed_offset = 0, .message_offset = 0 } },
        },
    });
    const exports = try allocator.alloc(LirProgram.StaticDataExport, 1);
    exports[0] = .{
        .symbol_name = try allocator.dupe(u8, "roc__completed"),
        .value_id = completed_slot,
        .bytes = try allocator.alloc(u8, 0),
        .alignment = 1,
        .relocations = try allocator.alloc(LirProgram.StaticDataRelocation, 0),
        .empty_list_capacities = try allocator.alloc(LirProgram.EmptyListCapacity, 0),
    };
    lowered.frozen_static_data = .{ .allocator = allocator, .exports = exports };

    try adoptCompletedComptimeValues(&lowered);

    const slot = lowered.lir_result.static_data_values.items[0];
    try std.testing.expectEqual(@as(?LIR.LirProcSpecId, null), slot.initializer);
    try std.testing.expect(slot.compile_time_root == null);
    try std.testing.expectEqual(@as(usize, 1), lowered.lir_result.store.procSpecCount());
    try std.testing.expectEqual(@as(usize, 1), lowered.lir_result.root_procs.items.len);
    try std.testing.expectEqual(lowered.lir_result.root_procs.items[0], lowered.main_proc.?);
}

/// Specialization census diagnostic, enabled by setting `ROC_SPEC_CENSUS` in
/// the environment. Prints one tab-separated line per checked module, per
/// Monotype specialization record, and per final LIR procedure to stderr, so
/// the shape of a program's specialization set can be measured offline. Each
/// specialization line carries its callable kind, module kind and name,
/// procedure base and template ordinals, callable name, whether the declared
/// source type has type variables, whether the request type contains a
/// function type or an erased callable, and the request and solved type
/// digests. Never enabled by default; it only reads the finished stores.
const SpecCensus = if (builtin.os.tag == .freestanding) struct {
    fn enabled() bool {
        return false;
    }
    fn runMonotype(_: Allocator, _: CheckedModuleSet, _: *const postcheck.Monotype.Ast.Program) Allocator.Error!void {}
    fn runLifted(_: *const postcheck.MonotypeLifted.Ast.Program) void {}
    fn runLir(_: Allocator, _: *const LirProgram.Result) Allocator.Error!void {}
} else struct {
    const MonoAst = postcheck.Monotype.Ast;
    const MonoType = postcheck.Monotype.Type;

    fn enabled() bool {
        return std.c.getenv("ROC_SPEC_CENSUS") != null;
    }

    fn runLifted(lifted: *const postcheck.MonotypeLifted.Ast.Program) void {
        std.debug.print("CENSUS_LIFTED\t{d}\t{d}\n", .{ lifted.fnCount(), lifted.exprCount() });
        // Bodies are appended in lowering order, so consecutive body ids
        // bound each function's expression count from above.
        for (0..lifted.fnCount()) |index| {
            const lifted_fn = lifted.getFn(@enumFromInt(@as(u32, @intCast(index))));
            const body: usize = switch (lifted_fn.body) {
                .roc => |body| @intFromEnum(body),
                .hosted => 0,
            };
            const name = if (lifted.procDebugName(lifted_fn.symbol)) |id| lifted.names.exportNameText(id) else "?";
            std.debug.print("CENSUS_LIFTED_FN\t{d}\t{d}\t{s}\n", .{ index, body, name });
        }
    }

    const ModuleEnvPtr = @TypeOf(@as(*const checked.CheckedModuleArtifact, undefined).moduleEnvConst());

    const ModuleInfo = struct {
        key: [32]u8,
        name: []const u8,
        kind: []const u8,
        names: *const check.CheckedNames.NameStore,
        types: checked.CheckedTypeStoreView,
        templates: *const checked.CheckedProcedureTemplateTable,
        env: ModuleEnvPtr,
    };

    fn defName(info: *const ModuleInfo, proc_base: u32) []const u8 {
        const key = info.names.procBase(@enumFromInt(proc_base));
        if (key.export_name) |export_name| return info.names.exportNameText(export_name);
        const def_idx = key.source_def_idx orelse return "?";
        const def = info.env.store.getDef(@enumFromInt(def_idx));
        return switch (info.env.store.getPattern(def.pattern)) {
            .assign => |assign| info.env.getIdent(assign.ident),
            .var_assign => |assign| info.env.getIdent(assign.ident),
            .as,
            .applied_tag,
            .nominal,
            .nominal_external,
            .record_destructure,
            .list,
            .tuple,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .num_from_numeral_literal,
            .str_literal,
            .str_interpolation,
            .underscore,
            .runtime_error,
            => "?pat",
            .deferred_import_ref => checkedPipelineInvariant("deferred import reference pattern reached LIR lowering"),
        };
    }

    fn collectModules(allocator: Allocator, modules: CheckedModuleSet) Allocator.Error![]ModuleInfo {
        var list = std.ArrayList(ModuleInfo).empty;
        errdefer list.deinit(allocator);
        const root = modules.root.module;
        try list.append(allocator, .{
            .key = root.key.bytes,
            .name = root.canonical_names.moduleNameText(root.module_identity.display_module_name),
            .kind = @tagName(root.module_identity.kind),
            .names = &root.canonical_names,
            .types = root.checked_types.view(),
            .templates = &root.checked_procedure_templates,
            .env = root.moduleEnvConst(),
        });
        for (modules.imports) |imported| {
            try list.append(allocator, .{
                .key = imported.key.bytes,
                .name = imported.canonical_names.moduleNameText(imported.module_identity.display_module_name),
                .kind = @tagName(imported.module_identity.kind),
                .names = imported.canonical_names,
                .types = imported.checked_types,
                .templates = imported.checked_procedure_templates,
                .env = imported.module_env,
            });
        }
        for (modules.root.relation_modules) |relation| {
            try list.append(allocator, .{
                .key = relation.key.bytes,
                .name = relation.canonical_names.moduleNameText(relation.module_identity.display_module_name),
                .kind = @tagName(relation.module_identity.kind),
                .names = relation.canonical_names,
                .types = relation.checked_types,
                .templates = relation.checked_procedure_templates,
                .env = relation.module_env,
            });
        }
        return list.toOwnedSlice(allocator);
    }

    fn findModule(infos: []const ModuleInfo, key: [32]u8) ?*const ModuleInfo {
        for (infos) |*info| {
            if (std.mem.eql(u8, &info.key, &key)) return info;
        }
        return null;
    }

    fn checkedTypeHasVariable(allocator: Allocator, types: checked.CheckedTypeStoreView, root: checked.CheckedTypeId) Allocator.Error!bool {
        var visited = collections.DenseMap(checked.CheckedTypeId, void).init(allocator);
        defer visited.deinit();
        var stack = std.ArrayList(checked.CheckedTypeId).empty;
        defer stack.deinit(allocator);
        try stack.append(allocator, root);
        while (stack.pop()) |ty| {
            const gop = try visited.getOrPut(ty);
            if (gop.found_existing) continue;
            switch (types.payload(ty)) {
                .pending, .err, .empty_record, .empty_tag_union => {},
                .flex, .rigid => return true,
                .alias => |alias| {
                    try stack.append(allocator, alias.backing);
                    try stack.appendSlice(allocator, alias.args);
                },
                .record => |record| {
                    for (record.fields) |field| try stack.append(allocator, field.ty);
                    try stack.append(allocator, record.ext);
                },
                .tuple => |elems| try stack.appendSlice(allocator, elems),
                .nominal => |nominal| try stack.appendSlice(allocator, nominal.args),
                .function => |func| {
                    try stack.appendSlice(allocator, func.args);
                    try stack.append(allocator, func.ret);
                },
                .tag_union => |tag_union| {
                    for (tag_union.tags) |tag| try stack.appendSlice(allocator, tag.argsSlice(types));
                    try stack.append(allocator, tag_union.ext);
                },
            }
        }
        return false;
    }

    const RequestShape = struct {
        has_fn: bool = false,
        has_erased: bool = false,
        /// A bare first-order function-typed parameter (a call-only
        /// candidate for demand-based specialization).
        fn_bare_param: bool = false,
        /// A function type anywhere in the result.
        fn_in_return: bool = false,
        /// A function type nested inside a data type of a parameter.
        fn_nested_in_data: bool = false,
        /// A bare function-typed parameter whose own signature mentions a
        /// function type.
        fn_higher_order: bool = false,

        fn classes(self: RequestShape) [4]u8 {
            return .{
                if (self.fn_bare_param) 'P' else '-',
                if (self.fn_in_return) 'R' else '-',
                if (self.fn_nested_in_data) 'D' else '-',
                if (self.fn_higher_order) 'H' else '-',
            };
        }
    };

    fn typeMentionsFn(allocator: Allocator, types: MonoType.Store.View, root: MonoType.TypeId) Allocator.Error!bool {
        var visited = collections.DenseMap(MonoType.TypeId, void).init(allocator);
        defer visited.deinit();
        var stack = std.ArrayList(MonoType.TypeId).empty;
        defer stack.deinit(allocator);
        try stack.append(allocator, root);
        while (stack.pop()) |ty| {
            const gop = try visited.getOrPut(ty);
            if (gop.found_existing) continue;
            switch (types.get(ty)) {
                .primitive, .zst => {},
                .erased, .func => return true,
                .named => |named| {
                    try stack.appendSlice(allocator, types.span(named.args));
                    if (named.backing) |backing| try stack.append(allocator, backing.ty);
                },
                .record => |span| for (types.fieldSpan(span)) |field| try stack.append(allocator, field.ty),
                .tuple => |span| try stack.appendSlice(allocator, types.span(span)),
                .tag_union => |span| for (types.tagSpan(span)) |tag| try stack.appendSlice(allocator, types.span(tag.payloads)),
                .list, .box => |elem| try stack.append(allocator, elem),
            }
        }
        return false;
    }

    fn classifyRequestShape(allocator: Allocator, types: MonoType.Store.View, root: MonoType.TypeId, shape: *RequestShape) Allocator.Error!void {
        const func = switch (types.get(root)) {
            .func => |func| func,
            .primitive, .zst, .erased, .named, .record, .tuple, .tag_union, .list, .box => return,
        };
        for (types.span(func.args)) |arg| {
            switch (types.get(arg)) {
                .func => |inner| {
                    var higher = false;
                    for (types.span(inner.args)) |inner_arg| {
                        if (try typeMentionsFn(allocator, types, inner_arg)) higher = true;
                    }
                    if (try typeMentionsFn(allocator, types, inner.ret)) higher = true;
                    if (higher) shape.fn_higher_order = true else shape.fn_bare_param = true;
                },
                .erased => shape.fn_bare_param = true,
                .primitive, .zst, .named, .record, .tuple, .tag_union, .list, .box => {
                    if (try typeMentionsFn(allocator, types, arg)) shape.fn_nested_in_data = true;
                },
            }
        }
        if (try typeMentionsFn(allocator, types, func.ret)) shape.fn_in_return = true;
    }

    fn requestShape(allocator: Allocator, types: MonoType.Store.View, root: MonoType.TypeId) Allocator.Error!RequestShape {
        var shape: RequestShape = .{};
        var visited = collections.DenseMap(MonoType.TypeId, void).init(allocator);
        defer visited.deinit();
        var stack = std.ArrayList(MonoType.TypeId).empty;
        defer stack.deinit(allocator);
        switch (types.get(root)) {
            .func => |func| {
                try stack.appendSlice(allocator, types.span(func.args));
                try stack.append(allocator, func.ret);
            },
            .primitive,
            .zst,
            .erased,
            .named,
            .record,
            .tuple,
            .tag_union,
            .list,
            .box,
            => try stack.append(allocator, root),
        }
        while (stack.pop()) |ty| {
            const gop = try visited.getOrPut(ty);
            if (gop.found_existing) continue;
            switch (types.get(ty)) {
                .primitive, .zst => {},
                .erased => shape.has_erased = true,
                .func => |func| {
                    shape.has_fn = true;
                    try stack.appendSlice(allocator, types.span(func.args));
                    try stack.append(allocator, func.ret);
                },
                .named => |named| {
                    try stack.appendSlice(allocator, types.span(named.args));
                    if (named.backing) |backing| try stack.append(allocator, backing.ty);
                },
                .record => |span| for (types.fieldSpan(span)) |field| try stack.append(allocator, field.ty),
                .tuple => |span| try stack.appendSlice(allocator, types.span(span)),
                .tag_union => |span| for (types.tagSpan(span)) |tag| try stack.appendSlice(allocator, types.span(tag.payloads)),
                .list, .box => |elem| try stack.append(allocator, elem),
            }
        }
        try classifyRequestShape(allocator, types, root, &shape);
        return shape;
    }

    /// Render a Monotype type as text for the census, depth-limited.
    fn renderType(w: *std.Io.Writer, allocator: Allocator, mono: *const MonoAst.Program, types: MonoType.Store.View, ty: MonoType.TypeId, depth: u32) std.Io.Writer.Error!void {
        if (depth > 6) {
            try w.writeAll("…");
            return;
        }
        switch (types.get(ty)) {
            .primitive => |p| try w.writeAll(@tagName(p)),
            .zst => try w.writeAll("zst"),
            .erased => try w.writeAll("erased"),
            .func => |func| {
                try w.writeAll("(");
                for (types.span(func.args), 0..) |arg, i| {
                    if (i != 0) try w.writeAll(", ");
                    try renderType(w, allocator, mono, types, arg, depth + 1);
                }
                try w.writeAll(" -> ");
                try renderType(w, allocator, mono, types, func.ret, depth + 1);
                try w.writeAll(")");
            },
            .named => |named| {
                try w.print("{s}#{x}", .{ mono.names.typeNameText(named.def.type_name), mono.names.moduleIdentityBytes(named.def.module)[0..3] });
                if (named.def.source_decl) |decl| try w.print("@{d}", .{decl});
                try w.print("/{s}", .{@tagName(named.kind)});
                const args = types.span(named.args);
                if (args.len != 0) {
                    try w.writeAll("<");
                    for (args, 0..) |arg, i| {
                        if (i != 0) try w.writeAll(", ");
                        try renderType(w, allocator, mono, types, arg, depth + 1);
                    }
                    try w.writeAll(">");
                }
                if (named.backing) |backing| {
                    try w.writeAll("{");
                    try renderType(w, allocator, mono, types, backing.ty, depth + 1);
                    try w.writeAll("}");
                }
            },
            .record => |span| {
                try w.writeAll("{");
                for (types.fieldSpan(span), 0..) |field, i| {
                    if (i != 0) try w.writeAll(", ");
                    try w.print("{s}: ", .{mono.names.recordFieldLabelText(field.name)});
                    try renderType(w, allocator, mono, types, field.ty, depth + 1);
                }
                try w.writeAll("}");
            },
            .tuple => |span| {
                try w.writeAll("(");
                for (types.span(span), 0..) |item, i| {
                    if (i != 0) try w.writeAll(", ");
                    try renderType(w, allocator, mono, types, item, depth + 1);
                }
                try w.writeAll(")");
            },
            .tag_union => |span| {
                try w.writeAll("[");
                for (types.tagSpan(span), 0..) |tag, i| {
                    if (i != 0) try w.writeAll(", ");
                    try w.writeAll(mono.names.tagLabelText(tag.name));
                    const payloads = types.span(tag.payloads);
                    if (payloads.len != 0) {
                        try w.writeAll("(");
                        for (payloads, 0..) |payload, j| {
                            if (j != 0) try w.writeAll(", ");
                            try renderType(w, allocator, mono, types, payload, depth + 1);
                        }
                        try w.writeAll(")");
                    }
                }
                try w.writeAll("]");
            },
            .list => |elem| {
                try w.writeAll("List(");
                try renderType(w, allocator, mono, types, elem, depth + 1);
                try w.writeAll(")");
            },
            .box => |elem| {
                try w.writeAll("Box(");
                try renderType(w, allocator, mono, types, elem, depth + 1);
                try w.writeAll(")");
            },
        }
    }

    fn runMonotype(allocator: Allocator, modules: CheckedModuleSet, mono: *const MonoAst.Program) Allocator.Error!void {
        const infos = try collectModules(allocator, modules);
        defer allocator.free(infos);
        for (infos) |info| {
            std.debug.print("CENSUS_MODULE\t{s}\t{s}\t{s}\n", .{ info.kind, info.name, std.fmt.bytesToHex(info.key, .lower)[0..16] });
        }
        const view = mono.view();
        for (view.specs, 0..) |spec, index| {
            var callable_kind: []const u8 = "generated";
            var callable_kind_is_template = false;
            var module_key: ?[32]u8 = null;
            var proc_base: u32 = 0;
            var sub: u32 = 0;
            switch (spec.identity.callable) {
                .proc_template => |template| {
                    callable_kind = "template";
                    callable_kind_is_template = true;
                    module_key = template.module.bytes;
                    proc_base = template.proc_base;
                    sub = template.template;
                },
                .nested_site => |nested| {
                    callable_kind = "nested";
                    module_key = nested.module.bytes;
                    proc_base = nested.owner_proc_base;
                    sub = nested.site;
                },
                .hosted => callable_kind = "hosted",
                .generated => callable_kind = "generated",
            }
            var module_kind: []const u8 = "?";
            var module_name: []const u8 = "?";
            var callable_name: []const u8 = "?";
            var poly: u8 = 2;
            if (module_key) |key| {
                if (findModule(infos, key)) |info| {
                    module_kind = info.kind;
                    module_name = info.name;
                    callable_name = defName(info, proc_base);
                    if (callable_kind_is_template and sub < info.templates.templates.items.len) {
                        const source_fn_ty = info.templates.templates.items[sub].checked_fn_root;
                        if (@intFromEnum(source_fn_ty) < info.types.stored_payloads.len) {
                            poly = if (try checkedTypeHasVariable(allocator, info.types, source_fn_ty)) 1 else 0;
                        }
                    }
                }
            }
            const shape = try requestShape(allocator, view.types, spec.identity.request_fn_ty);
            var rendered: std.Io.Writer.Allocating = .init(allocator);
            defer rendered.deinit();
            renderType(&rendered.writer, allocator, mono, view.types, spec.identity.request_fn_ty, 0) catch return error.OutOfMemory;
            const key_hex = std.fmt.bytesToHex(spec.request_fn_ty_digest.bytes, .lower);
            const req_hex = std.fmt.bytesToHex(spec.identity.request_fn_ty_digest.bytes, .lower);
            const solved_hex = std.fmt.bytesToHex(spec.solved_fn_ty_digest.bytes, .lower);
            std.debug.print("CENSUS_SPEC\t{d}\t{s}\t{s}\t{s}\t{d}\t{d}\t{s}\t{d}\t{d}\t{d}\t{s}\t{s}\t{s}\t{s}\n", .{
                index,
                callable_kind,
                module_kind,
                module_name,
                proc_base,
                sub,
                callable_name,
                poly,
                @intFromBool(shape.has_fn),
                @intFromBool(shape.has_erased),
                req_hex[0..16],
                solved_hex[0..16],
                @tagName(spec.status),
                &shape.classes(),
            });
            std.debug.print("CENSUS_REQ\t{s}\t{s}\t{s}\n", .{ callable_name, key_hex[0..16], rendered.written() });
        }
    }

    fn runLir(allocator: Allocator, result: *const LirProgram.Result) Allocator.Error!void {
        std.debug.print("CENSUS_STATIC\t{d}\t{d}\n", .{ result.static_data_values.items.len, result.const_plans.items.len });
        const store = &result.store;
        for (0..store.procSpecCount()) |index| {
            const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
            const spec = store.getProcSpec(proc_id);
            const name = store.procDebugName(proc_id) orelse "?";
            if (spec.body == null) {
                std.debug.print("CENSUS_PROC\t{d}\t{s}\t0\t0\t{s}\n", .{ index, name, if (spec.is_static_initializer) "nobody-init" else "nobody" });
                continue;
            }
            var buffer: std.Io.Writer.Allocating = .init(allocator);
            defer buffer.deinit();
            DebugPrint.writeProc(allocator, store, &result.layouts, proc_id, &buffer.writer) catch |err| switch (err) {
                error.OutOfMemory, error.WriteFailed => return error.OutOfMemory,
            };
            const text = buffer.written();
            var lines: usize = 0;
            for (text) |c| {
                if (c == '\n') lines += 1;
            }
            std.debug.print("CENSUS_PROC\t{d}\t{s}\t{d}\t{d}\t{s}\n", .{ index, name, text.len, lines, if (spec.is_static_initializer) "init" else "body" });
            if (lines <= 120) {
                std.debug.print("CENSUS_BODY\t{d}\n{s}\n", .{ index, text });
            }
            if (lines > 20000) {
                var shown: usize = 0;
                var start: usize = 0;
                for (text, 0..) |c, i| {
                    if (c == '\n') {
                        std.debug.print("CENSUS_HEAD\t{d}\t{s}\n", .{ index, text[start..i] });
                        start = i + 1;
                        shown += 1;
                        if (shown >= 80) break;
                    }
                }
            }
        }
    }
};
