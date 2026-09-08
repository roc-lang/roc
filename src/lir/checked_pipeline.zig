//! Public checked-module-to-LIR lowering API.
//!
//! This is the only public lowering entrance after checking. It consumes
//! complete checked modules, explicit root requests, and target configuration.
//! It returns LIR or resource failure.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const check = @import("check");
const core = @import("lir_core");

const Arc = @import("arc.zig");
const ImmortalLocals = @import("immortal_locals.zig");
const Trmc = @import("trmc.zig");
const BoxReuse = @import("box_reuse.zig");
const ReturnSlot = @import("return_slot.zig");
const StrAppend = @import("str_append.zig");
const ScalarizeJoins = @import("scalarize_joins.zig");
const SingleUseInline = @import("single_use_inline.zig");
const ForwardingJoinInline = @import("forwarding_join_inline.zig");
const TagCaseFusion = @import("tag_case_fusion.zig");
const LoopAppendPromote = @import("loop_append_promote.zig");
const RangeProve = @import("range_prove.zig");
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

/// Target settings and checked module state for the checked-to-LIR pipeline.
pub const TargetConfig = struct {
    target_usize: base.target.TargetUsize = base.target.TargetUsize.native,
    specialization_strategy: SpecializationStrategy = .lss,
    /// Reuse checking workers for generic post-check tasks when available.
    post_check_executor: ?base.post_check_task_executor.Executor = null,
    checked_module_state: CheckedModuleState = .complete,
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
    /// Thread slack counters through loop-carried append-only lists so the
    /// per-element ownership and capacity checks amortize. On by default;
    /// shape-comparison tests turn it off because promotion intentionally
    /// changes the loop skeleton of qualifying sides.
    promote_loop_appends: bool = true,
    /// Control Monotype specialization cache reads and writes.
    monotype_cache: MonotypeCacheControl = .{},
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
    /// Receives the expression count of the lifted program handed to lambda-set
    /// solving. Every later post-check stage walks that program in full, so the
    /// count is the size measure a growth regression shows up in.
    lifted_expr_count_out: ?*usize = null,
    /// Optional timing accumulator for the checked-to-LIR pipeline.
    timing: ?*Timing = null,
};

/// Thread-safe timing totals for the checked-to-LIR pipeline.
pub const Timing = struct {
    std_io: std.Io,
    detailed_monotype_body: bool = false,
    monotype_diagnostics_mutex: std.Io.Mutex = .init,
    monotype_diagnostics: postcheck.Monotype.Lower.Diagnostics = .{},
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
    monotype_parallel_root_tasks_retried_serial: TimingCounter = .{},
    monotype_parallel_specialization_tasks_submitted: TimingCounter = .{},
    monotype_parallel_specialization_tasks_committed: TimingCounter = .{},
    monotype_parallel_specialization_tasks_retried_serial: TimingCounter = .{},
    monotype_parallel_specialization_tasks_discarded_ready: TimingCounter = .{},
    monotype_parallel_task_waves: TimingCounter = .{},
    monotype_parallel_peak_worker_lanes_available: TimingCounter = .{},
    monotype_parallel_peak_worker_lanes_used: TimingCounter = .{},
    monotype_parallel_within_lowering_lane_reuse_tasks: TimingCounter = .{},
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
                .root_tasks_retried_serial = self.monotype_parallel_root_tasks_retried_serial.load(),
                .specialization_tasks_submitted = self.monotype_parallel_specialization_tasks_submitted.load(),
                .specialization_tasks_committed = self.monotype_parallel_specialization_tasks_committed.load(),
                .specialization_tasks_retried_serial = self.monotype_parallel_specialization_tasks_retried_serial.load(),
                .specialization_tasks_discarded_ready = self.monotype_parallel_specialization_tasks_discarded_ready.load(),
                .task_waves = self.monotype_parallel_task_waves.load(),
                .peak_worker_lanes_available = self.monotype_parallel_peak_worker_lanes_available.load(),
                .peak_worker_lanes_used = self.monotype_parallel_peak_worker_lanes_used.load(),
                .within_lowering_lane_reuse_tasks = self.monotype_parallel_within_lowering_lane_reuse_tasks.load(),
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
        self.monotype_parallel_root_tasks_retried_serial.add(parallel.root_tasks_retried_serial);
        self.monotype_parallel_specialization_tasks_submitted.add(parallel.specialization_tasks_submitted);
        self.monotype_parallel_specialization_tasks_committed.add(parallel.specialization_tasks_committed);
        self.monotype_parallel_specialization_tasks_retried_serial.add(parallel.specialization_tasks_retried_serial);
        self.monotype_parallel_specialization_tasks_discarded_ready.add(parallel.specialization_tasks_discarded_ready);
        self.monotype_parallel_task_waves.add(parallel.task_waves);
        self.monotype_parallel_peak_worker_lanes_available.max(parallel.peak_worker_lanes_available);
        self.monotype_parallel_peak_worker_lanes_used.max(parallel.peak_worker_lanes_used);
        self.monotype_parallel_within_lowering_lane_reuse_tasks.add(parallel.within_lowering_lane_reuse_tasks);
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
        .root_tasks_retried_serial = 15,
        .specialization_tasks_submitted = 16,
        .specialization_tasks_committed = 17,
        .specialization_tasks_retried_serial = 18,
        .specialization_tasks_discarded_ready = 19,
        .task_waves = 20,
        .peak_worker_lanes_available = 4,
        .peak_worker_lanes_used = 3,
        .within_lowering_lane_reuse_tasks = 21,
    });
    timing.addMonotypeParallel(.{
        .worker_work_ns = 31,
        .coordinator_post_batch_work_ns = 32,
        .root_tasks_submitted = 33,
        .root_tasks_committed = 34,
        .root_tasks_retried_serial = 35,
        .specialization_tasks_submitted = 36,
        .specialization_tasks_committed = 37,
        .specialization_tasks_retried_serial = 38,
        .specialization_tasks_discarded_ready = 39,
        .task_waves = 40,
        .peak_worker_lanes_available = 8,
        .peak_worker_lanes_used = 5,
        .within_lowering_lane_reuse_tasks = 41,
    });
    timing.addSnapshot(.{ .boxy_plan_ns = 43, .boxy_lower_ns = 47 });

    const snapshot_value = timing.snapshot();
    const parallel = snapshot_value.monotype_parallel;
    try std.testing.expectEqual(@as(u64, 97), snapshot_value.monotype_ns);
    try std.testing.expectEqual(@as(u64, 42), parallel.worker_work_ns);
    try std.testing.expectEqual(@as(u64, 44), parallel.coordinator_post_batch_work_ns);
    try std.testing.expectEqual(@as(u64, 46), parallel.root_tasks_submitted);
    try std.testing.expectEqual(@as(u64, 48), parallel.root_tasks_committed);
    try std.testing.expectEqual(@as(u64, 50), parallel.root_tasks_retried_serial);
    try std.testing.expectEqual(@as(u64, 52), parallel.specialization_tasks_submitted);
    try std.testing.expectEqual(@as(u64, 54), parallel.specialization_tasks_committed);
    try std.testing.expectEqual(@as(u64, 56), parallel.specialization_tasks_retried_serial);
    try std.testing.expectEqual(@as(u64, 58), parallel.specialization_tasks_discarded_ready);
    try std.testing.expectEqual(@as(u64, 60), parallel.task_waves);
    try std.testing.expectEqual(@as(u64, 8), parallel.peak_worker_lanes_available);
    try std.testing.expectEqual(@as(u64, 5), parallel.peak_worker_lanes_used);
    try std.testing.expectEqual(@as(u64, 62), parallel.within_lowering_lane_reuse_tasks);
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
pub const MonotypeCacheControl = postcheck.Monotype.Lower.SpecializationCacheControl;

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

/// Fully lowered LIR program plus root and runtime schema metadata.
pub const LoweredProgram = struct {
    lir_result: LirProgram.Result,
    main_proc: ?LIR.LirProcSpecId,
    target_usize: base.target.TargetUsize,
    runtime_value_schemas: RuntimeValueSchemaStore,

    pub fn deinit(self: *LoweredProgram) void {
        self.runtime_value_schemas.deinit();
        self.lir_result.deinit();
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

/// Lower checked modules and explicit roots directly into an ARC-ready LIR program.
pub fn lowerCheckedModulesToLir(
    allocator: Allocator,
    modules: CheckedModuleSet,
    roots: RootRequestSet,
    target: TargetConfig,
) LowerResourceError!LoweredProgram {
    try verifyCheckedBoundary(modules, target);
    try requireHostedProceduresBound(modules, target);

    const layout_requests = try collectLayoutRequests(allocator, modules.root.module, roots.layout_requests, roots.include_provided_data_exports);
    defer allocator.free(layout_requests);
    const static_data_requests = switch (target.checked_module_state) {
        .complete => try collectStaticDataRequests(
            allocator,
            modules.root.module,
            roots.static_data_requests,
            roots.include_provided_data_exports,
        ),
        .checking_finalization => try allocator.dupe(postcheck.Common.StaticDataRequest, roots.static_data_requests),
    };
    defer allocator.free(static_data_requests);

    switch (target.specialization_strategy) {
        .lss => {},
        .boxy => return lowerBoxyCheckedModulesToLir(
            allocator,
            modules,
            roots,
            target,
            layout_requests,
            static_data_requests,
        ),
    }

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
    var mono = monotype: {
        defer if (target.timing) |timing| {
            timing.finish(monotype_started_ns, .monotype);
            if (monotype_timing) |*detail| timing.addMonotypeSnapshot(detail.snapshot());
            if (monotype_diagnostics) |diagnostics| timing.addMonotypeDiagnostics(diagnostics);
        };
        break :monotype try postcheck.Monotype.Lower.run(
            allocator,
            checkedModules(modules),
            rootRequests(roots, layout_requests, static_data_requests),
            .{
                .proc_debug_names = target.proc_debug_names or LirDump.filter() != null,
                .specialization_cache = target.monotype_cache,
                .post_check_executor = target.post_check_executor,
                .static_data_literals = target.checked_module_state == .checking_finalization or roots.include_internal_static_data,
                .target_usize = target.target_usize,
                .inline_expects = switch (target.inline_expects) {
                    .run => .run,
                    .omit => .omit,
                },
                .timing = if (monotype_timing) |*timing| timing else null,
                .diagnostics = if (monotype_diagnostics) |*diagnostics| diagnostics else null,
            },
        );
    };
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

    var procedure_usage = if (target.inline_mode != .none) blk: {
        var spec_constr_timing_scope = PipelineTimingScope.begin(target.timing, .spec_constr);
        defer spec_constr_timing_scope.end();
        const usage = try postcheck.MonotypeLifted.SpecConstr.runAndCollectProcedureUsage(allocator, &lifted, target.spec_constr_clone_inlining);
        spec_constr_timing_scope.end();
        break :blk usage;
    } else blk: {
        const spec_constr_started_ns = if (target.timing) |timing| timing.start() else 0;
        try postcheck.MonotypeLifted.SpecConstr.runIteratorFusion(allocator, &lifted);
        if (target.timing) |timing| timing.finish(spec_constr_started_ns, .spec_constr);
        break :blk postcheck.MonotypeLifted.SpecConstr.OwnedProcedureUsage.empty(allocator);
    };
    defer procedure_usage.deinit();

    if (target.lifted_expr_count_out) |slot| slot.* = lifted.exprCount();

    var lambda_solve_timing_scope = PipelineTimingScope.begin(target.timing, .lambda_solve);
    defer lambda_solve_timing_scope.end();
    const lifted_input = lifted;
    lifted_owned = false;
    lifted = undefined;
    var solved = try postcheck.LambdaSolved.Solve.run(allocator, lifted_input);
    var solved_owned = true;
    errdefer if (solved_owned) solved.deinit();
    lambda_solve_timing_scope.end();

    var inline_plan_timing_scope = PipelineTimingScope.begin(
        if (target.inline_mode != .none) target.timing else null,
        .inline_plan,
    );
    defer inline_plan_timing_scope.end();
    var inline_plan = try postcheck.SolvedInline.analyze(allocator, target.inline_mode, procedure_usage.view(), &solved);
    defer inline_plan.deinit();
    inline_plan_timing_scope.end();

    var lir_gen_timing_scope = PipelineTimingScope.begin(target.timing, .lir_gen);
    defer lir_gen_timing_scope.end();
    const solved_input = solved;
    solved_owned = false;
    solved = undefined;
    var lowered = try postcheck.SolvedLirLower.run(allocator, target.target_usize, solved_input, .{
        .inline_plan = inline_plan.view(),
        .post_check_executor = target.post_check_executor,
        .inline_expects = target.inline_expects,
        .list_in_place_map = target.list_in_place_map,
        .dict_seed_mode = switch (target.checked_module_state) {
            .complete => .runtime,
            .checking_finalization => .comptime_zero,
        },
        .proc_debug_names = target.proc_debug_names or LirDump.filter() != null,
        .layout_request_const_plans = target.layout_request_const_plans,
        .test_plan_metadata = roots.test_plan_metadata,
        .debug_materialized_out = target.debug_materialized_out,
        .parallel_metrics = target.solved_lir_parallel_metrics_out,
    });
    lir_gen_timing_scope.end();
    errdefer lowered.deinit();

    return finishLoweredOutput(allocator, roots, target, &lowered);
}

fn finishLoweredOutput(
    allocator: Allocator,
    roots: RootRequestSet,
    target: TargetConfig,
    lowered: anytype,
) LowerResourceError!LoweredProgram {
    verifyArithmeticBoundary(&lowered.lir_result.store, false);
    var lir_passes_timing_scope = PipelineTimingScope.begin(target.timing, .lir_passes);
    defer lir_passes_timing_scope.end();

    // TRMC/TCE must rewrite recursive procs before ARC insertion: it deletes
    // calls and changes allocation sites, and ARC panics on pre-existing RC
    // statements (see src/lir/trmc.zig).
    try Trmc.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
    if (target.specialization_strategy == .lss and target.inline_mode == .none) {
        try SingleUseInline.run(&lowered.lir_result);
        try ForwardingJoinInline.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
        try TagCaseFusion.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
    }
    try ScalarizeJoins.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
    if (target.promote_loop_appends) {
        try LoopAppendPromote.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
    }
    verifyArithmeticBoundary(&lowered.lir_result.store, true);
    if (target.prove_ranges) {
        try RangeProve.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
    }
    try BoxReuse.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
    try ReturnSlot.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
    try StrAppend.run(&lowered.lir_result.store);
    if (target.tag_reachability) {
        try TagReachability.run(&lowered.lir_result);
    }
    try ReachableProcs.run(&lowered.lir_result);
    lir_passes_timing_scope.end();

    var arc_timing_scope = PipelineTimingScope.begin(target.timing, .arc);
    defer arc_timing_scope.end();
    try Arc.insert(&lowered.lir_result.store, &lowered.lir_result.layouts, .{
        .roots = lowered.lir_result.root_procs.items,
        .specialize = target.inline_mode != .none,
        .consume_dead_boxes = target.consume_dead_boxes,
    });
    arc_timing_scope.end();

    // After the certifier has checked ARC's ledger, so that what it verified
    // is the placement ARC produced.
    _ = try ImmortalLocals.elide(allocator, &lowered.lir_result.store);

    try LirDump.run(&lowered.lir_result);

    if (roots.requests.len != 0 and lowered.lir_result.root_procs.items.len == 0) {
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

    return finishLoweredOutput(allocator, roots, target, &lowered);
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
            const name = store.procDebugName(proc_id) orelse continue;
            if (name_filter.len != 0 and std.mem.find(u8, name, name_filter) == null) continue;
            var buffer: std.Io.Writer.Allocating = .init(store.allocator);
            defer buffer.deinit();
            DebugPrint.writeProc(store.allocator, store, layouts, proc_id, &buffer.writer) catch |err| switch (err) {
                error.OutOfMemory, error.WriteFailed => return error.OutOfMemory,
            };
            std.debug.print("=== LIR {s} (p{d}) ===\n{s}\n", .{ name, index, buffer.written() });
        }
    }
};
