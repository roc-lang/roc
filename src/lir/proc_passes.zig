//! Procedure-local LIR rewrites over frozen phase inputs.
//!
//! Workers own copies of their procedure's body chunk rows, the rows they may
//! write, and append-only output. A phase commits in procedure order only
//! after every callback has finished, keeping identities independent of
//! worker count without copying a store per task.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const collections = @import("collections");
const core = @import("lir_core");
const layout = @import("layout");
const BodyClone = @import("body_clone.zig");
const Trmc = @import("trmc.zig");
const PruneJoinParams = @import("prune_join_params.zig");
const ScalarizeJoins = @import("scalarize_joins.zig");
const LoopAppendPromote = @import("loop_append_promote.zig");
const RangeProve = @import("range_prove.zig");
const BoxReuse = @import("box_reuse.zig");
const TagCaseFusion = @import("tag_case_fusion.zig");
const BranchExpectation = @import("branch_expectation.zig");
const KnownTagJump = @import("known_tag_jump.zig");

const Allocator = std.mem.Allocator;
const LIR = core.LIR;
const LirStore = core.LirStore;
const TaskExecutor = base.post_check_task_executor;

/// Phase boundaries preserve the established optimization order.
pub const Phase = enum {
    branch_expectation,
    trmc,
    tag_fusion,
    prune_join_params,
    scalarize,
    loop_append,
    range,
    box_reuse,
};

/// The number of procedure-local phases.
pub const phase_count = @typeInfo(Phase).@"enum".field_names.len;

/// Phases a segment may hold several of: none reads another procedure's
/// body, allocates fresh join identities, or prepares layouts from an
/// earlier phase's output, so running them one after another on one shard
/// gives each the input the commit of the phase before it would.
pub fn fusible(phase: Phase) bool {
    return switch (phase) {
        .branch_expectation, .trmc, .scalarize, .prune_join_params => true,
        .tag_fusion, .loop_append, .range, .box_reuse => false,
    };
}

/// Per-phase durations and procedure counts, recorded only when a caller
/// asks for detailed timing. Segments run on the coordinating thread, so the
/// fields are plain; each lane measures its phases in its own row, folded
/// after the segment's run. A segment's elapsed time, preparation, rewrite,
/// capacity, task time and commit are recorded under its first phase.
pub const Timing = struct {
    std_io: std.Io,
    /// Each segment's elapsed time, from layout preparation to its last
    /// commit.
    wall_ns: [phase_count]u64 = @splat(0),
    /// Layout preparation and the scan admitting procedures.
    prepare_ns: [phase_count]u64 = @splat(0),
    /// Elapsed time of the rewrite callbacks, on the workers or inline.
    rewrite_ns: [phase_count]u64 = @splat(0),
    /// Each phase's own rewrites, summed over workers.
    worker_ns: [phase_count]u64 = @splat(0),
    /// The workers' capacity over the rewrite: its elapsed time times the
    /// executor's worker count, or the elapsed time when it ran inline.
    capacity_ns: [phase_count]u64 = @splat(0),
    /// The segment's whole task callbacks, summed over workers: its phases
    /// together with cloning each procedure and comparing it with its
    /// source.
    segment_worker_ns: [phase_count]u64 = @splat(0),
    /// Validation and the ordered commit of rewritten procedures.
    commit_ns: [phase_count]u64 = @splat(0),
    /// Procedures the phase admitted and rewrote.
    visited: [phase_count]u64 = @splat(0),
    /// Admitted procedures that this phase changed and no earlier phase of
    /// its segment had.
    changed: [phase_count]u64 = @splat(0),
    lanes: []Lane = &.{},

    /// One lane's phase times; lanes never share a cache line.
    const Lane = struct {
        ns: [phase_count]u64 = @splat(0),
        _padding: [std.atomic.cache_line]u8 = undefined,
    };

    fn now(self: *const Timing) i96 {
        return std.Io.Timestamp.now(self.std_io, .awake).nanoseconds;
    }

    fn since(self: *const Timing, started: i96) u64 {
        return @intCast(@max(0, self.now() - started));
    }
};

/// Worker work only; inline execution leaves these counters zero.
pub const ParallelMetrics = struct {
    tasks_submitted: u64 = 0,
    tasks_committed: u64 = 0,
    prepared_statement_rows: u64 = 0,
    appended_statements: u64 = 0,
    peak_retained_shards: u64 = 0,
    /// Procedures each phase ran on, admitted or checked.
    committed_by_phase: [phase_count]u64 = @splat(0),
    /// Procedures each phase changed that no earlier phase of its segment had.
    changed_by_phase: [phase_count]u64 = @splat(0),

    /// Accumulate completed lowering runs without treating a peak as a sum.
    pub fn add(self: *ParallelMetrics, other: ParallelMetrics) void {
        self.tasks_submitted +|= other.tasks_submitted;
        self.tasks_committed +|= other.tasks_committed;
        self.prepared_statement_rows +|= other.prepared_statement_rows;
        self.appended_statements +|= other.appended_statements;
        self.peak_retained_shards = @max(self.peak_retained_shards, other.peak_retained_shards);
        for (&self.committed_by_phase, other.committed_by_phase) |*count, value| count.* +|= value;
        for (&self.changed_by_phase, other.changed_by_phase) |*count, value| count.* +|= value;
    }
};

/// One pass's storage retained by an exclusive executor lane, at a stable
/// address; `key`'s address is the lane-state identity of `T`.
fn LaneBox(comptime T: type) type {
    return struct {
        allocator: Allocator,
        value: T,

        var key: u8 = undefined;

        fn destroy(opaque_state: *anyopaque) void {
            const box: *@This() = @ptrCast(@alignCast(opaque_state));
            box.value.deinit();
            box.allocator.destroy(box);
        }
    };
}

/// The lane's retained `T`, created empty on the lane's first use. `T`
/// returns to empty after every procedure, so only capacity crosses tasks.
fn laneScratch(comptime T: type, lane: *TaskExecutor.LaneState) Allocator.Error!*T {
    const Box = LaneBox(T);
    if (lane.get(&Box.key)) |state| return &@as(*Box, @ptrCast(@alignCast(state))).value;
    const box = try lane.allocator.create(Box);
    box.* = .{ .allocator = lane.allocator, .value = T.init(lane.allocator) };
    errdefer Box.destroy(box);
    try lane.put(&Box.key, box, Box.destroy);
    return &box.value;
}

fn phaseMask(phase: Phase) u16 {
    return @as(u16, 1) << @intCast(@backingInt(phase));
}

/// The body a phase rewrites, or null when the phase has nothing to rewrite.
fn phaseBody(store: *const LirStore, phase: Phase, proc: LIR.LirProcSpecId) ?LIR.CFStmtId {
    return switch (phase) {
        .tag_fusion => TagCaseFusion.rewritableProcBody(store, proc),
        .scalarize => ScalarizeJoins.rewritableProcBody(store, proc),
        .branch_expectation, .trmc, .prune_join_params, .loop_append, .range, .box_reuse => BodyClone.rewritableProcBody(store, proc),
    };
}

const TaskContext = struct {
    source: *const LirStore,
    layouts: *const layout.Store,
    /// The segment's phases, run in order on one shard.
    phases: []const Phase,
    proc: LIR.LirProcSpecId,
    /// The procedure's shapes excluded it from every phase of the segment;
    /// the task runs only to verify that the phases indeed rewrite nothing,
    /// and never commits.
    verify_only: bool = false,
    /// Static data reads the compile-time value guards will guard; see
    /// `runSegmentTimed`.
    guarded_reads: ?*const std.DynamicBitSetUnmanaged = null,
    shard: ?LirStore = null,
    failed: bool = false,
    completed: bool = false,
    changed: bool = false,
    /// The phases that ran on the procedure, admitted or checked.
    ran: u16 = 0,
    /// The phases whose shapes admitted the procedure.
    visited: u16 = 0,
    /// The first phase that changed the procedure.
    first_changed: ?Phase = null,
    trmc_report: ?Trmc.Report = null,
    range_work: RangeProve.WorkStats = .{},
    first_fresh_join: u32 = 0,
    fresh_join_count: u32 = 0,
    timing: ?*Timing = null,

    fn run(context_opaque: *anyopaque, worker: TaskExecutor.Worker) ?*anyopaque {
        const self: *TaskContext = @ptrCast(@alignCast(context_opaque));
        self.executeOnLane(worker) catch {
            self.failed = true;
        };
        return self;
    }

    fn executeOnLane(self: *TaskContext, worker: TaskExecutor.Worker) Allocator.Error!void {
        try self.execute(worker.allocator, worker.scratch, worker.lane_state, worker.id);
    }

    fn execute(self: *TaskContext, output_allocator: Allocator, scratch_allocator: Allocator, lane: *TaskExecutor.LaneState, lane_id: usize) Allocator.Error!void {
        var shard = try self.source.cloneForProcRewrite(output_allocator, self.proc);
        errdefer shard.deinit();
        var next_join = self.first_fresh_join;
        var changed = false;
        for (self.phases) |phase| {
            if (phaseBody(&shard, phase, self.proc) == null) continue;
            const spec = shard.getProcSpecImmediate(self.proc);
            const admitted = shapesAdmit(spec.shapes.merged(shard.shapes), phase);
            // Debug builds also run a phase the shapes exclude, on every
            // procedure no earlier phase of the segment changed, and fail
            // if it would rewrite anything.
            if (!admitted and (builtin.mode != .debug or changed)) continue;
            const started: i96 = if (self.timing) |active| active.now() else 0;
            try self.runPhase(&shard, phase, &next_join, scratch_allocator, lane);
            if (self.timing) |active| active.lanes[lane_id].ns[@backingInt(phase)] +|= active.since(started);
            self.ran |= phaseMask(phase);
            const now_changed = shard.procRewriteChanged();
            if (!admitted) {
                if (now_changed) base.invariant("LIR pass {s} rewrote procedure {d} whose shapes {any} excluded it from the phase", .{ @tagName(phase), @backingInt(self.proc), spec.shapes });
                continue;
            }
            self.visited |= phaseMask(phase);
            if (now_changed and !changed) self.first_changed = phase;
            changed = changed or now_changed;
        }
        self.fresh_join_count = next_join - self.first_fresh_join;
        self.changed = shard.procRewriteChanged();
        self.shard = shard;
    }

    fn runPhase(self: *TaskContext, shard: *LirStore, phase: Phase, next_join: *u32, scratch_allocator: Allocator, lane: *TaskExecutor.LaneState) Allocator.Error!void {
        const analysis = switch (phase) {
            .tag_fusion, .loop_append, .range, .box_reuse => try laneScratch(BodyClone.AnalysisScratch, lane),
            .branch_expectation, .trmc, .prune_join_params, .scalarize => null,
        };
        switch (phase) {
            .branch_expectation => try BranchExpectation.runProc(shard, self.proc, scratch_allocator),
            .trmc => self.trmc_report = try Trmc.runProcWithScratch(shard, self.layouts, self.proc, try laneScratch(Trmc.LaneScratch, lane)),
            .tag_fusion => {
                var joins = BodyClone.JoinParamIndex.init(scratch_allocator);
                defer joins.deinit();
                joins.next_join_point = next_join.*;
                try TagCaseFusion.runProcWithScratch(shard, self.layouts, self.proc, scratch_allocator, &joins, analysis.?);
                try KnownTagJump.runProc(shard, self.proc, scratch_allocator, &joins);
                next_join.* = joins.next_join_point;
            },
            .prune_join_params => try PruneJoinParams.runProc(shard, self.proc, scratch_allocator, self.guarded_reads),
            .scalarize => try ScalarizeJoins.runProcWithScratch(shard, self.layouts, self.proc, try laneScratch(ScalarizeJoins.LaneScratch, lane)),
            .loop_append => try LoopAppendPromote.runProcInLane(shard, self.layouts, self.proc, try laneScratch(LoopAppendPromote.LaneScratch, lane), analysis.?),
            .range => {
                var joins = BodyClone.JoinParamIndex.init(scratch_allocator);
                defer joins.deinit();
                joins.next_join_point = next_join.*;
                self.range_work = try RangeProve.runProcWithScratch(shard, self.layouts, self.proc, try laneScratch(RangeProve.LaneScratch, lane), analysis.?, &joins);
                next_join.* = joins.next_join_point;
            },
            .box_reuse => try BoxReuse.runProcWithScratch(shard, self.layouts, self.proc, scratch_allocator, analysis.?),
        }
    }
};

/// The smallest summed frame size of the procedures one lane claims at a
/// time, so short procedures share a claim.
const min_chunk_locals = 64;

/// The procedure tasks of one segment as the items of an ordered run.
const SegmentBatch = struct {
    pub const Error = Allocator.Error;

    contexts: []TaskContext,
    source: *const LirStore,

    pub fn weight(self: *const SegmentBatch, index: usize) u64 {
        return 1 + @as(u64, self.source.getProcSpecImmediate(self.contexts[index].proc).frame_locals.len);
    }

    pub fn runItem(self: *SegmentBatch, index: usize, worker: TaskExecutor.Worker) Error!void {
        try self.contexts[index].executeOnLane(worker);
    }

    pub fn commitItem(_: *SegmentBatch, _: usize) Error!void {}
};

/// Apply one phase using the same rewrite/commit boundary for serial and
/// parallel execution. No callback can observe another procedure's output.
pub fn run(
    allocator: Allocator,
    store: *LirStore,
    layouts: *layout.Store,
    phase: Phase,
    executor: ?TaskExecutor.Executor,
    metrics: ?*ParallelMetrics,
) Allocator.Error!void {
    return runSegmentTimed(allocator, store, layouts, &.{phase}, executor, metrics, null, null, null);
}

/// `run`, recording the phase's durations and procedure counts in `timing`
/// when it is given, and adding to `range_work` what the range prover's work
/// on each procedure the range phase admits cost. Serial and parallel
/// execution count the same work.
pub fn runTimed(
    allocator: Allocator,
    store: *LirStore,
    layouts: *layout.Store,
    phase: Phase,
    executor: ?TaskExecutor.Executor,
    metrics: ?*ParallelMetrics,
    timing: ?*Timing,
    range_work: ?*RangeProve.WorkStats,
) Allocator.Error!void {
    return runSegmentTimed(allocator, store, layouts, &.{phase}, executor, metrics, timing, range_work, null);
}

/// Apply a segment of phases in order: one task per procedure clones it once,
/// runs every phase of the segment its shapes admit on that one shard, and
/// the segment commits once, in procedure order, after every task. A segment
/// of several phases holds only `fusible` ones, so each phase reads exactly
/// what the ordered commit of the phase before it would have left. No
/// callback can observe another procedure's output.
///
/// `guarded_reads`, for a segment holding `prune_join_params` in a program
/// that reads compile-time values, holds each static data slot whose reads
/// `comptime_value_guards.insert` will guard: such a read fails when the
/// value's evaluation failed, so it is never a pure definition to delete.
///
/// `range_work` receives what the range prover's work on each procedure the
/// range phase admits cost (`runTimed`).
pub fn runSegmentTimed(
    allocator: Allocator,
    store: *LirStore,
    layouts: *layout.Store,
    phases: []const Phase,
    executor: ?TaskExecutor.Executor,
    metrics: ?*ParallelMetrics,
    timing: ?*Timing,
    range_work: ?*RangeProve.WorkStats,
    guarded_reads: ?*const std.DynamicBitSetUnmanaged,
) Allocator.Error!void {
    std.debug.assert(phases.len != 0);
    if (phases.len > 1) for (phases) |phase| std.debug.assert(fusible(phase));
    const leader = @backingInt(phases[0]);
    const segment_started: i96 = if (timing) |active| active.now() else 0;
    defer if (timing) |active| {
        active.wall_ns[leader] +|= active.since(segment_started);
    };
    for (phases) |phase| switch (phase) {
        .trmc => try Trmc.prepareLayouts(store, layouts),
        .box_reuse => try BoxReuse.prepareLayouts(store, layouts),
        .branch_expectation, .tag_fusion, .prune_join_params, .scalarize, .loop_append, .range => {},
    };
    var contexts = std.ArrayList(TaskContext).empty;
    defer {
        for (contexts.items) |*context| if (context.shard) |*shard| shard.deinit();
        contexts.deinit(allocator);
    }
    for (0..store.procSpecCount()) |index| {
        const proc: LIR.LirProcSpecId = @fromBackingInt(@intCast(@as(u32, @intCast(index))));
        // A static initializer is straight-line construction LIR that the
        // static data materializer evaluates and no backend emits: it has no
        // branch, call, join, or loop for a phase to rewrite, and ARC places
        // no reference count in it.
        if (store.getProcSpecImmediate(proc).is_static_initializer) continue;
        // A phase can add shapes only by running, so a procedure no phase
        // of the segment admits now is one none of them would rewrite.
        var has_body = false;
        var admitted = false;
        for (phases) |phase| {
            if (phaseBody(store, phase, proc) == null) continue;
            has_body = true;
            if (phaseAdmits(store, phase, proc)) admitted = true;
        }
        if (!has_body) continue;
        if (!admitted and builtin.mode != .debug) continue;
        try contexts.append(allocator, .{
            .source = store,
            .layouts = layouts,
            .phases = phases,
            .proc = proc,
            .verify_only = !admitted,
            .timing = timing,
            .guarded_reads = guarded_reads,
        });
    }
    if (timing) |active| active.prepare_ns[leader] +|= active.since(segment_started);
    if (contexts.items.len == 0) return;

    // Only the coordinator scans the phase's identity domain. Workers reserve
    // fresh joins above this boundary; ordered commit rebases only those IDs.
    // A segment of several phases allocates none.
    const first_fresh_join = switch (phases[0]) {
        .tag_fusion, .range => BodyClone.firstFreshJoinPoint(store),
        .branch_expectation, .trmc, .prune_join_params, .scalarize, .loop_append, .box_reuse => 0,
    };
    for (contexts.items) |*context| context.first_fresh_join = first_fresh_join;
    const prefix = store.captureBodyPrefix();
    const layout_count = layouts.layoutCount();
    const parallel = if (executor) |value| value.worker_count > 1 and contexts.items.len > 1 else false;
    if (timing) |active| {
        active.lanes = try allocator.alloc(Timing.Lane, if (parallel) executor.?.lane_count else 1);
        for (active.lanes) |*lane| lane.* = .{};
    }
    defer if (timing) |active| {
        for (active.lanes) |lane| for (&active.worker_ns, lane.ns) |*total, ns| {
            total.* +|= ns;
        };
        allocator.free(active.lanes);
        active.lanes = &.{};
    };
    const rewrite_started: i96 = if (timing) |active| active.now() else 0;
    if (parallel) {
        var batch: SegmentBatch = .{ .contexts = contexts.items, .source = store };
        var ordered: ?TaskExecutor.OrderedTiming = if (timing) |active| .{ .std_io = active.std_io } else null;
        var sequential_lane = TaskExecutor.LaneState.init(allocator);
        defer sequential_lane.deinit();
        if (metrics) |counts| {
            counts.tasks_submitted +|= @intCast(contexts.items.len);
            counts.peak_retained_shards = @max(counts.peak_retained_shards, contexts.items.len);
        }
        // Retained output is at most the segment's rewritten bodies, never a
        // whole-program copy per procedure. Every rewrite reads only the
        // frozen store, and the commit below runs once every rewrite has
        // finished, so the run's own commit does nothing.
        try TaskExecutor.runOrdered(SegmentBatch, allocator, &batch, contexts.items.len, executor.?, &sequential_lane, .{
            .min_chunk_weight = min_chunk_locals,
            .timing = if (ordered) |*active| active else null,
        });
        for (contexts.items) |*context| context.completed = true;
        if (timing) |active| {
            active.rewrite_ns[leader] +|= active.since(rewrite_started);
            active.segment_worker_ns[leader] +|= ordered.?.worker_ns;
            active.capacity_ns[leader] +|= ordered.?.capacity_ns;
        }
    } else {
        var scratch = base.SingleThreadArena.init(allocator);
        defer scratch.deinit();
        var lane = TaskExecutor.LaneState.init(allocator);
        defer lane.deinit();
        for (contexts.items) |*context| {
            try context.execute(allocator, scratch.allocator(), &lane, 0);
            context.completed = true;
            _ = scratch.reset(.retain_capacity);
        }
        if (timing) |active| {
            const elapsed = active.since(rewrite_started);
            active.rewrite_ns[leader] +|= elapsed;
            active.segment_worker_ns[leader] +|= elapsed;
            active.capacity_ns[leader] +|= elapsed;
        }
    }
    const commit_started: i96 = if (timing) |active| active.now() else 0;
    defer if (timing) |active| {
        active.commit_ns[leader] +|= active.since(commit_started);
        for (contexts.items) |context| {
            for (phases) |phase| {
                if ((context.visited & phaseMask(phase)) != 0) active.visited[@backingInt(phase)] += 1;
            }
            if (context.first_changed) |phase| active.changed[@backingInt(phase)] += 1;
        }
    };
    if (!std.meta.eql(prefix, store.captureBodyPrefix()) or layout_count != layouts.layoutCount()) {
        invariant("LIR pass mutated frozen coordinator storage");
    }
    for (contexts.items) |context| {
        if (!context.completed) invariant("LIR pass lost a task completion");
        if (context.failed) return error.OutOfMemory;
        if (context.shard == null) invariant("LIR pass completed without a procedure rewrite");
    }
    if (builtin.mode == .debug) {
        validateWritableOwnership(allocator, contexts.items) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.SharedWritableStatement => invariant("procedure-local LIR rewrites shared a writable statement"),
        };
    }

    var next_fresh_join = first_fresh_join;
    for (contexts.items) |*context| {
        const shard = &context.shard.?;
        if (parallel) if (metrics) |counts| {
            counts.tasks_committed +|= 1;
            for (phases) |phase| {
                if ((context.ran & phaseMask(phase)) != 0) counts.committed_by_phase[@backingInt(phase)] +|= 1;
            }
            if (context.first_changed) |phase| counts.changed_by_phase[@backingInt(phase)] +|= 1;
        };
        if (context.verify_only) {
            if (context.changed) base.invariant("LIR pass segment starting with {s} rewrote procedure {d} whose shapes {any} excluded it from every phase", .{ @tagName(phases[0]), @backingInt(context.proc), store.getProcSpecImmediate(context.proc).shapes });
            shard.deinit();
            context.shard = null;
            continue;
        }
        if (context.changed) {
            const reservation = reserveJoins(first_fresh_join, next_fresh_join, context.fresh_join_count) catch
                invariant("LIR rewrite exhausted join-point identities");
            store.commitProcRewriteWithJoinRelocation(shard, reservation.relocation) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.InvalidBodyPrefix => invariant("LIR rewrite lost its frozen prefix"),
                error.UnsupportedShardMetadata => invariant("LIR rewrite emitted unprepared metadata"),
            };
            next_fresh_join = reservation.next;
        }
        if (parallel) if (metrics) |counts| {
            counts.prepared_statement_rows +|= shard.procRewriteStatementCount();
            counts.appended_statements +|= @intCast(shard.cf_stmts.len());
        };
        if (context.trmc_report) |report| Trmc.reportProc(store, layouts, context.proc, report);
        if ((context.visited & phaseMask(.range)) != 0) RangeProve.reportWork(store, context.proc, context.range_work);
        if (range_work) |work| work.add(context.range_work);
        shard.deinit();
        context.shard = null;
    }
}

/// Whether the procedure's recorded shapes admit it to the phase: the phase
/// can only rewrite a shape the body's flags say it contains.
fn phaseAdmits(store: *const LirStore, phase: Phase, proc: LIR.LirProcSpecId) bool {
    return shapesAdmit(store.getProcSpecImmediate(proc).shapes, phase);
}

/// Whether `shapes` admit a body to the phase.
fn shapesAdmit(shapes: LIR.ProcShapes, phase: Phase) bool {
    return switch (phase) {
        .branch_expectation => shapes.switch_stmt,
        .trmc => shapes.self_call,
        .loop_append => shapes.loop,
        .prune_join_params => shapes.join_param,
        .tag_fusion => shapes.join_param and shapes.switch_stmt,
        .scalarize => shapes.join_aggregate_param or shapes.struct_build or shapes.tag_build,
        .range => shapes.checked_arithmetic or shapes.checked_division or shapes.switch_stmt or shapes.unsigned_compare or shapes.simd_concat_shift,
        .box_reuse => shapes.box_box,
    };
}

const JoinReservation = struct {
    relocation: ?LirStore.JoinPointRelocation,
    next: u32,
};

fn reserveJoins(first_fresh: u32, next: u32, count: u32) error{JoinIdExhausted}!JoinReservation {
    std.debug.assert(next >= first_fresh);
    return .{
        .next = std.math.add(u32, next, count) catch return error.JoinIdExhausted,
        .relocation = if (count == 0) null else .{ .first_fresh = first_fresh, .offset = next - first_fresh },
    };
}

fn validateWritableOwnership(allocator: Allocator, contexts: []const TaskContext) (Allocator.Error || error{SharedWritableStatement})!void {
    var owners = collections.DenseMap(u32, LIR.LirProcSpecId).init(allocator);
    defer owners.deinit();
    inline for (LirStore.rewrite_columns) |field| {
        owners.clearRetainingCapacity();
        for (contexts) |*context| {
            for (context.shard.?.procRewriteChunks()) |chunk| {
                const range = @field(chunk, field);
                for (range.start..range.start + range.len) |id| {
                    const entry = try owners.getOrPut(@intCast(id));
                    if (entry.found_existing and entry.value_ptr.* != context.proc) return error.SharedWritableStatement;
                    entry.value_ptr.* = context.proc;
                }
            }
        }
    }
}

test "procedure fusion join reservations retain allocation counts and reject cumulative overflow" {
    const max = std.math.maxInt(u32);
    const empty = try reserveJoins(max, max, 0);
    try std.testing.expectEqual(@as(u32, max), empty.next);
    try std.testing.expect(empty.relocation == null);
    const last = try reserveJoins(max - 1, max - 1, 1);
    try std.testing.expectEqual(@as(u32, max), last.next);
    try std.testing.expectEqual(@as(u32, 0), last.relocation.?.offset);
    try std.testing.expectError(error.JoinIdExhausted, reserveJoins(max - 1, last.next, 1));

    // Allocations eliminated during a fixed point still consume their IDs.
    const first = try reserveJoins(10, 10, 2);
    const second = try reserveJoins(10, first.next, 3);
    try std.testing.expectEqual(@as(u32, 2), second.relocation.?.offset);
    try std.testing.expectEqual(@as(u32, 15), second.next);
}

test "procedure rewrite ownership includes statements reached through shared metadata" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    const value = try store.addLocal(.{ .layout_idx = .u64 });
    const shared = store.bodyMark();
    const shared_body = try store.addCFStmt(.{ .ret = .{ .value = value } }, .test_fixture);
    const branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = shared_body }});
    const shared_end = store.bodyMark();
    var contexts: [2]TaskContext = undefined;
    var initialized: usize = 0;
    defer for (contexts[0..initialized]) |*context| context.shard.?.deinit();
    for (&contexts, 0..) |*context, i| {
        const own = store.bodyMark();
        const fallback = try store.addCFStmt(.{ .ret = .{ .value = value } }, .test_fixture);
        const body = try store.addCFStmt(.{ .switch_stmt = .{
            .cond = value,
            .branches = branches,
            .default_branch = fallback,
        } }, .test_fixture);
        const proc = try store.addProcSpec(.{
            .identity = LIR.ProcIdentity.forTest(@intCast(i)),
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&.{value}),
            .body = .some(body),
            .ret_layout = .u64,
        }, .none);
        // Each procedure's chunks hold the shared rows its body reaches.
        try store.addBodyChunk(proc, .{
            .cf_stmts = .{ .start = shared.cf_stmts, .len = shared_end.cf_stmts - shared.cf_stmts },
            .cf_switch_branches = .{ .start = shared.cf_switch_branches, .len = shared_end.cf_switch_branches - shared.cf_switch_branches },
        });
        try store.recordBodyChunk(proc, own);
        context.* = .{ .source = &store, .layouts = undefined, .phases = &.{.tag_fusion}, .proc = proc };
    }
    const prefix = store.captureBodyPrefix();
    for (&contexts) |*context| {
        context.shard = try store.cloneForProcRewrite(allocator, context.proc);
        initialized += 1;
    }
    try std.testing.expectError(error.SharedWritableStatement, validateWritableOwnership(allocator, &contexts));
    try std.testing.expectEqualDeep(prefix, store.captureBodyPrefix());
}

fn invariant(comptime message: []const u8) noreturn {
    base.invariant("{s}", .{message});
}

test "issue 11325 procedure counting reuses lane storage across distant local IDs" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(testing.allocator, .u64);
    defer layouts.deinit();
    const low = try store.addLocal(.{ .layout_idx = .u64 });
    for (0..100000) |_| _ = try store.addLocal(.{ .layout_idx = .u64 });
    const high = try store.addLocal(.{ .layout_idx = .u64 });
    for (0..16) |index| {
        const body_mark = store.bodyMark();
        const ret = try store.addCFStmt(.{ .ret = .{ .value = high } }, .test_fixture);
        const body = try store.addCFStmt(.{ .assign_ref = .{ .target = high, .op = .local(low), .next = ret } }, .test_fixture);
        const proc = try store.addProcSpec(.{
            .name = store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.forTest(@intCast(index)),
            .args = try store.addLocalSpan(&.{low}),
            .frame_locals = try store.addLocalSpan(&.{ low, high }),
            .body = .some(body),
            .ret_layout = .u64,
        }, .none);
        try store.recordBodyChunk(proc, body_mark);
    }
    var meter = testing.FailingAllocator.init(testing.allocator, .{});
    var lane = TaskExecutor.LaneState.init(meter.allocator());
    defer lane.deinit();
    var warmed_bytes: usize = 0;
    for (0..store.procSpecCount()) |index| {
        var context: TaskContext = .{
            .source = &store,
            .layouts = &layouts,
            .phases = &.{.box_reuse},
            .proc = @fromBackingInt(@intCast(index)),
        };
        _ = TaskContext.run(&context, .{
            .id = 0,
            .allocator = testing.allocator,
            .scratch = meter.allocator(),
            .lane_state = &lane,
        });
        defer if (context.shard) |*shard| shard.deinit();
        try testing.expect(!context.failed);
        try testing.expect(!context.changed);
        if (index == 0) {
            warmed_bytes = meter.allocated_bytes;
        } else {
            try testing.expectEqual(warmed_bytes, meter.allocated_bytes);
        }
    }
}

fn testLaneCountingAllocation(allocator: Allocator, store: *LirStore, layouts: *const layout.Store, proc: LIR.LirProcSpecId) Allocator.Error!void {
    var lane = TaskExecutor.LaneState.init(allocator);
    defer lane.deinit();
    var context: TaskContext = .{
        .source = store,
        .layouts = layouts,
        .phases = &.{.box_reuse},
        .proc = proc,
    };
    _ = TaskContext.run(&context, .{
        .id = 0,
        .allocator = std.testing.allocator,
        .scratch = std.testing.allocator,
        .lane_state = &lane,
    });
    defer if (context.shard) |*shard| shard.deinit();
    if (context.failed) return error.OutOfMemory;
}

test "issue 11325 lane registration and counting allocation failures release storage" {
    const testing = std.testing;
    var store = LirStore.init(testing.allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(testing.allocator, .u64);
    defer layouts.deinit();
    const local = try store.addLocal(.{ .layout_idx = .u64 });
    const body_mark = store.bodyMark();
    const proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(0),
        .args = try store.addLocalSpan(&.{local}),
        .frame_locals = try store.addLocalSpan(&.{local}),
        .body = .some(try store.addCFStmt(.{ .ret = .{ .value = local } }, .test_fixture)),
        .ret_layout = .u64,
    }, .none);
    try store.recordBodyChunk(proc, body_mark);
    try testing.checkAllAllocationFailures(testing.allocator, testLaneCountingAllocation, .{ &store, &layouts, proc });
}
