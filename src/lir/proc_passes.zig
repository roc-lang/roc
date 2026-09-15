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

const Allocator = std.mem.Allocator;
const LIR = core.LIR;
const LirStore = core.LirStore;
const TaskExecutor = base.post_check_task_executor;

/// Phase boundaries preserve the established optimization order.
pub const Phase = enum {
    trmc,
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
    callees: ?*const LoopAppendPromote.PreparedCallees,
    shard: ?LirStore = null,
    failed: bool = false,
    completed: bool = false,
    changed: bool = false,
    trmc_report: ?Trmc.Report = null,

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
            .scalarize => try ScalarizeJoins.runProc(&shard, self.layouts, self.proc, scratch_allocator),
            .loop_append => try LoopAppendPromote.runProc(&shard, self.layouts, self.proc, scratch_allocator, self.callees.?),
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
        .scalarize, .loop_append, .range => {},
    }
    var callees = if (phase == .loop_append)
        try LoopAppendPromote.prepareCallees(store, layouts, allocator)
    else
        null;
    defer if (callees) |*prepared| prepared.deinit();

    var contexts = std.ArrayList(TaskContext).empty;
    defer {
        for (contexts.items) |*context| if (context.shard) |*shard| shard.deinit();
        contexts.deinit(allocator);
    }
    for (0..store.procSpecCount()) |index| {
        const proc: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const body = switch (phase) {
            .scalarize => ScalarizeJoins.rewritableProcBody(store, proc),
            .trmc, .loop_append, .range, .box_reuse => BodyClone.rewritableProcBody(store, proc),
        };
        if (body == null) continue;
        // Constructor recursion is not an ordinary tail call: TRMC discovers
        // it inside tag-union producers even when no TCE sites were recorded.
        if (phase == .trmc) {
            const spec = store.getProcSpec(proc);
            if (spec.tail_calls == null and layouts.getLayout(spec.ret_layout).tag != .tag_union) continue;
        }
        try contexts.append(allocator, .{
            .source = store,
            .layouts = layouts,
            .phase = phase,
            .proc = proc,
            .callees = if (callees) |*prepared| prepared else null,
        });
    }
    if (contexts.items.len == 0) return;

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
        var owners = collections.DenseMap(u32, LIR.LirProcSpecId).init(allocator);
        defer owners.deinit();
        for (contexts.items) |*context| {
            for (context.shard.?.procRewriteStatementIds()) |id| {
                const entry = try owners.getOrPut(id);
                if (entry.found_existing and entry.value_ptr.* != context.proc) {
                    invariant("procedure-local LIR rewrites shared a writable statement");
                }
                entry.value_ptr.* = context.proc;
            }
        }
    }

    for (contexts.items) |*context| {
        const shard = &context.shard.?;
        if (context.changed) {
            store.commitProcRewrite(shard) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.InvalidBodyPrefix => invariant("LIR rewrite lost its frozen prefix"),
                error.UnsupportedShardMetadata => invariant("LIR rewrite emitted unprepared metadata"),
            };
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

fn invariant(comptime message: []const u8) noreturn {
    if (builtin.mode == .Debug) @panic(message);
    unreachable;
}
