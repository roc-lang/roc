//! Procedure-local LIR rewrites over frozen phase inputs.
//!
//! Workers own sparse copies of writable rows and append-only output. A phase
//! commits in procedure order only after every callback has finished, keeping
//! identities independent of worker count without copying a store per task.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const collections = @import("collections");
const core = @import("lir_core");
const layout = @import("layout");
const BodyClone = @import("body_clone.zig");
const Trmc = @import("trmc.zig");
const ScalarizeJoins = @import("scalarize_joins.zig");
const LoopAppendPromote = @import("loop_append_promote.zig");
const RangeProve = @import("range_prove.zig");
const BoxReuse = @import("box_reuse.zig");
const ForwardingJoinInline = @import("forwarding_join_inline.zig");
const TagCaseFusion = @import("tag_case_fusion.zig");

const Allocator = std.mem.Allocator;
const LIR = core.LIR;
const LirStore = core.LirStore;
const TaskExecutor = base.post_check_task_executor;

/// Phase boundaries preserve the established optimization order.
pub const Phase = enum {
    trmc,
    forwarding_join,
    tag_fusion,
    scalarize,
    loop_append,
    range,
    box_reuse,
};

const phase_count = std.meta.fields(Phase).len;

/// Worker work only; inline execution leaves these counters zero.
pub const ParallelMetrics = struct {
    tasks_submitted: u64 = 0,
    tasks_committed: u64 = 0,
    prepared_statement_rows: u64 = 0,
    appended_statements: u64 = 0,
    peak_retained_shards: u64 = 0,
    committed_by_phase: [phase_count]u64 = @splat(0),
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

const TaskContext = struct {
    source: *const LirStore,
    layouts: *const layout.Store,
    phase: Phase,
    proc: LIR.LirProcSpecId,
    /// The procedure's facts excluded it from this phase; the task runs only
    /// to verify that the phase indeed rewrites nothing, and never commits.
    verify_only: bool = false,
    shard: ?LirStore = null,
    failed: bool = false,
    completed: bool = false,
    changed: bool = false,
    trmc_report: ?Trmc.Report = null,
    first_fresh_join: u32 = 0,
    fresh_join_count: u32 = 0,

    fn run(context_opaque: *anyopaque, worker: TaskExecutor.Worker) ?*anyopaque {
        const self: *TaskContext = @ptrCast(@alignCast(context_opaque));
        self.execute(worker.allocator, worker.scratch) catch {
            self.failed = true;
        };
        return self;
    }

    fn execute(self: *TaskContext, output_allocator: Allocator, scratch_allocator: Allocator) Allocator.Error!void {
        var shard = try self.source.cloneForProcRewrite(output_allocator, self.proc);
        errdefer shard.deinit();
        switch (self.phase) {
            .trmc => self.trmc_report = try Trmc.runProc(&shard, self.layouts, self.proc, scratch_allocator),
            .forwarding_join, .tag_fusion => {
                var joins = BodyClone.JoinParamIndex.init(scratch_allocator);
                defer joins.deinit();
                joins.next_join_point = self.first_fresh_join;
                if (self.phase == .forwarding_join) {
                    try ForwardingJoinInline.runProc(&shard, self.layouts, self.proc, scratch_allocator, &joins);
                } else {
                    try TagCaseFusion.runProc(&shard, self.layouts, self.proc, scratch_allocator, &joins);
                }
                self.fresh_join_count = joins.next_join_point - self.first_fresh_join;
            },
            .scalarize => try ScalarizeJoins.runProc(&shard, self.layouts, self.proc, scratch_allocator),
            .loop_append => try LoopAppendPromote.runProc(&shard, self.layouts, self.proc, scratch_allocator),
            .range => try RangeProve.runProc(&shard, self.layouts, self.proc, scratch_allocator),
            .box_reuse => try BoxReuse.runProc(&shard, self.layouts, self.proc, scratch_allocator),
        }
        self.changed = shard.procRewriteChanged();
        self.shard = shard;
    }
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
    switch (phase) {
        .trmc => try Trmc.prepareLayouts(store, layouts),
        .box_reuse => try BoxReuse.prepareLayouts(store, layouts),
        .forwarding_join, .tag_fusion, .scalarize, .loop_append, .range => {},
    }
    var contexts = std.ArrayList(TaskContext).empty;
    defer {
        for (contexts.items) |*context| if (context.shard) |*shard| shard.deinit();
        contexts.deinit(allocator);
    }
    for (0..store.procSpecCount()) |index| {
        const proc: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const body = switch (phase) {
            .forwarding_join => ForwardingJoinInline.rewritableProcBody(store, proc),
            .tag_fusion => TagCaseFusion.rewritableProcBody(store, proc),
            .scalarize => ScalarizeJoins.rewritableProcBody(store, proc),
            .trmc, .loop_append, .range, .box_reuse => BodyClone.rewritableProcBody(store, proc),
        };
        if (body == null) continue;
        const admitted = phaseAdmits(store, phase, proc);
        if (!admitted and builtin.mode != .Debug) continue;
        try contexts.append(allocator, .{
            .source = store,
            .layouts = layouts,
            .phase = phase,
            .proc = proc,
            .verify_only = !admitted,
        });
    }
    if (contexts.items.len == 0) return;

    // Only the coordinator scans the phase's identity domain. Workers reserve
    // fresh joins above this boundary; ordered commit rebases only those IDs.
    const first_fresh_join = switch (phase) {
        .forwarding_join, .tag_fusion => BodyClone.firstFreshJoinPoint(store),
        .trmc, .scalarize, .loop_append, .range, .box_reuse => 0,
    };
    for (contexts.items) |*context| context.first_fresh_join = first_fresh_join;
    const prefix = store.captureBodyPrefix();
    const layout_count = layouts.layoutCount();
    const parallel = if (executor) |value| value.worker_count > 1 and contexts.items.len > 1 else false;
    if (parallel) {
        const tasks = try allocator.alloc(TaskExecutor.Task, contexts.items.len);
        defer allocator.free(tasks);
        const completions = try allocator.alloc(TaskExecutor.Completion, contexts.items.len);
        defer allocator.free(completions);
        for (contexts.items, tasks, 0..) |*context, *task, index| {
            task.* = .{ .id = index, .context = context, .run = TaskContext.run };
        }
        if (metrics) |counts| {
            counts.tasks_submitted +|= @intCast(tasks.len);
            counts.peak_retained_shards = @max(counts.peak_retained_shards, tasks.len);
        }
        // Executor.run bounds active callbacks. Retained output is at most the
        // phase's rewritten bodies, never a whole-program copy per procedure.
        try executor.?.run(tasks, completions);
        for (completions) |completion| {
            if (completion.id >= contexts.items.len) invariant("LIR pass returned an unknown task");
            const context = &contexts.items[completion.id];
            if (context.completed) invariant("LIR pass completed one task more than once");
            if (completion.worker_id >= executor.?.worker_count) invariant("LIR pass returned an invalid worker lane");
            if (completion.value != @as(*anyopaque, @ptrCast(context))) invariant("LIR pass returned the wrong task context");
            context.completed = true;
        }
    } else {
        var scratch = std.heap.ArenaAllocator.init(allocator);
        defer scratch.deinit();
        for (contexts.items) |*context| {
            try context.execute(allocator, scratch.allocator());
            context.completed = true;
            _ = scratch.reset(.retain_capacity);
        }
    }
    if (!std.meta.eql(prefix, store.captureBodyPrefix()) or layout_count != layouts.layoutCount()) {
        invariant("LIR pass mutated frozen coordinator storage");
    }
    for (contexts.items) |context| {
        if (!context.completed) invariant("LIR pass lost a task completion");
        if (context.failed) return error.OutOfMemory;
        if (context.shard == null) invariant("LIR pass completed without a procedure rewrite");
    }
    if (builtin.mode == .Debug) {
        validateWritableOwnership(allocator, contexts.items) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.SharedWritableStatement => invariant("procedure-local LIR rewrites shared a writable statement"),
        };
    }

    var next_fresh_join = first_fresh_join;
    for (contexts.items) |*context| {
        const shard = &context.shard.?;
        if (context.verify_only) {
            if (context.changed) invariant("LIR pass rewrote a procedure whose facts excluded it from the phase");
            if (parallel) if (metrics) |counts| {
                counts.tasks_committed +|= 1;
                counts.committed_by_phase[@intFromEnum(phase)] +|= 1;
            };
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
            counts.tasks_committed +|= 1;
            counts.committed_by_phase[@intFromEnum(phase)] +|= 1;
            counts.changed_by_phase[@intFromEnum(phase)] +|= @intFromBool(context.changed);
            counts.prepared_statement_rows +|= @intCast(shard.procRewriteStatementIds().len);
            counts.appended_statements +|= @intCast(shard.cf_stmts.len());
        };
        if (context.trmc_report) |report| Trmc.reportProc(store, layouts, context.proc, report);
        shard.deinit();
        context.shard = null;
    }
}

/// Whether the procedure's recorded facts admit it to the phase: the phase
/// can only rewrite a shape the facts say the body contains.
fn phaseAdmits(store: *const LirStore, phase: Phase, proc: LIR.LirProcSpecId) bool {
    const facts = store.getProcSpec(proc).facts;
    return switch (phase) {
        .trmc => facts.self_call,
        .loop_append => facts.loop,
        .forwarding_join => facts.join_param,
        .tag_fusion => facts.join_param and facts.tag_build,
        .scalarize => facts.join_interned_param or facts.struct_build,
        .range => facts.checked_arithmetic or facts.switch_stmt,
        .box_reuse => facts.box_box,
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
    for (contexts) |*context| {
        for (context.shard.?.procRewriteStatementIds()) |id| {
            const entry = try owners.getOrPut(id);
            if (entry.found_existing and entry.value_ptr.* != context.proc) return error.SharedWritableStatement;
            entry.value_ptr.* = context.proc;
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
    const shared_body = try store.addCFStmt(.{ .ret = .{ .value = value } });
    const branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = shared_body }});
    var contexts: [2]TaskContext = undefined;
    var initialized: usize = 0;
    defer for (contexts[0..initialized]) |*context| context.shard.?.deinit();
    for (&contexts, 0..) |*context, i| {
        const fallback = try store.addCFStmt(.{ .ret = .{ .value = value } });
        const body = try store.addCFStmt(.{ .switch_stmt = .{
            .cond = value,
            .branches = branches,
            .default_branch = fallback,
        } });
        const proc = try store.addProcSpec(.{
            .identity = LIR.ProcIdentity.forTest(@intCast(i)),
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&.{value}),
            .body = body,
            .ret_layout = .u64,
        });
        context.* = .{ .source = &store, .layouts = undefined, .phase = .tag_fusion, .proc = proc };
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
    if (builtin.mode == .Debug) @panic(message);
    unreachable;
}
