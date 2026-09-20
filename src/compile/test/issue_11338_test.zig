//! Regression coverage for issue #11338: discovery depth must not add executor barriers.
const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");
const specialization_workers: usize = 4;

const chained_discovery_app =
    \\body1 : U64 -> U64
    \\body1 = |value| body2(value) + 1
    \\
    \\body2 : U64 -> U64
    \\body2 = |value| body3(value) + 2
    \\
    \\body3 : U64 -> U64
    \\body3 = |value| body4(value) + 3
    \\
    \\body4 : U64 -> U64
    \\body4 = |value| body5(value) + 4
    \\
    \\body5 : U64 -> U64
    \\body5 = |value| body6(value) + 5
    \\
    \\body6 : U64 -> U64
    \\body6 = |value| value + 6
    \\
    \\body7 : U64 -> U64
    \\body7 = |value| value + 7
    \\
    \\body8 : U64 -> U64
    \\body8 = |value| value + 8
    \\
    \\body9 : U64 -> U64
    \\body9 = |value| value + 9
    \\
    \\main! : List(Str) => Try({}, [Exit(I8), ..])
    \\main! = |args| {
    \\    runtime = args.len()
    \\    total = body1(runtime) + body7(runtime) + body8(runtime) + body9(runtime)
    \\    echo!(total.to_str())
    \\    Ok({})
    \\}
;

const independent_discovery_app =
    \\body1 : U64 -> U64
    \\body1 = |value| value + 1
    \\
    \\body2 : U64 -> U64
    \\body2 = |value| value + 2
    \\
    \\body3 : U64 -> U64
    \\body3 = |value| value + 3
    \\
    \\body4 : U64 -> U64
    \\body4 = |value| value + 4
    \\
    \\body5 : U64 -> U64
    \\body5 = |value| value + 5
    \\
    \\body6 : U64 -> U64
    \\body6 = |value| value + 6
    \\
    \\body7 : U64 -> U64
    \\body7 = |value| value + 7
    \\
    \\body8 : U64 -> U64
    \\body8 = |value| value + 8
    \\
    \\body9 : U64 -> U64
    \\body9 = |value| value + 9
    \\
    \\main! : List(Str) => Try({}, [Exit(I8), ..])
    \\main! = |args| {
    \\    runtime = args.len()
    \\    total = body1(runtime) + body2(runtime) + body3(runtime) + body4(runtime) + body5(runtime) + body6(runtime) + body7(runtime) + body8(runtime) + body9(runtime)
    \\    echo!(total.to_str())
    \\    Ok({})
    \\}
;

fn lowerAndMeasureMonotypeParallelism(app_body: []const u8) harness.LowerToLirHarnessError!lir.CheckedPipeline.TimingSnapshot {
    var timing: lir.CheckedPipeline.TimingSnapshot = .{};
    try harness.expectLowersToLirWithOptions(app_body, .{
        .specialization_workers = specialization_workers,
        .timing_out = &timing,
    });
    return timing;
}

test "chained specialization discovery costs no extra executor barriers" {
    const chained = (try lowerAndMeasureMonotypeParallelism(chained_discovery_app)).monotype_parallel;
    const independent = (try lowerAndMeasureMonotypeParallelism(independent_discovery_app)).monotype_parallel;
    try std.testing.expect(chained.specialization_tasks_submitted >= 9);
    try std.testing.expectEqual(independent.specialization_tasks_submitted, chained.specialization_tasks_submitted);
    try std.testing.expectEqual(chained.specialization_tasks_submitted, chained.specialization_tasks_committed);
    try std.testing.expectEqual(independent.specialization_tasks_submitted, independent.specialization_tasks_committed);
    try std.testing.expectEqual(@as(u64, specialization_workers), chained.peak_worker_lanes_available);
    if (chained.task_waves > independent.task_waves) {
        std.debug.print("chained discovery drained {d} specialization bodies in {d} executor barriers; the same {d} bodies discovered at one level drained in {d}\n", .{
            chained.specialization_tasks_submitted,     chained.task_waves,
            independent.specialization_tasks_submitted, independent.task_waves,
        });
        return error.SpecializationDiscoveryDepthAddedExecutorBarriers;
    }
}

// This executor deliberately leaves body7 unfinished while body1 completes.
// It runs callbacks only on receive, so the overlap assertion is deterministic:
// body2 must be submitted before body7's callback has even begun. The held task
// uses its own lane to preserve cumulative epoch order on every lane.
const DiscoveryExecutor = struct {
    const executor_api = @import("base").post_check_task_executor;
    const Queued = struct { task: executor_api.Task, sequence: usize };
    pending: [4]?Queued = @splat(null),
    held: ?Queued = null,
    peak_unaccepted_while_held: usize = 0,
    submitted: usize = 0,
    outstanding: usize = 0,
    peak_outstanding: usize = 0,
    discovered_while_unfinished: bool = false,
    open: bool = false,
    fail_at_submission: ?usize = null,
    lanes: [2]executor_api.LaneState = @splat(executor_api.LaneState.init(std.testing.allocator)),

    fn deinit(self: *@This()) void {
        for (&self.lanes) |*lane| lane.deinit();
    }

    fn execute(self: *@This(), task: executor_api.Task, lane: usize) executor_api.Completion {
        var scratch = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer scratch.deinit();
        return .{ .id = task.id, .worker_id = lane, .value = task.run(task.context, .{
            .id = lane,
            .allocator = std.testing.allocator,
            .scratch = scratch.allocator(),
            .lane_state = &self.lanes[lane],
        }) };
    }

    fn begin(context: *anyopaque) void {
        const self: *@This() = @ptrCast(@alignCast(context));
        std.debug.assert(!self.open and self.outstanding == 0);
        self.open = true;
    }

    fn submit(context: *anyopaque, task: executor_api.Task) std.mem.Allocator.Error!void {
        const self: *@This() = @ptrCast(@alignCast(context));
        if (self.fail_at_submission == self.submitted) return error.OutOfMemory;
        std.debug.assert(self.open and self.outstanding < 4);
        // main discovers six jobs, including body1 and the unrelated body7.
        // The eighth submission is body2, discovered by committing body1.
        if (self.submitted == 7 and self.held != null) self.discovered_while_unfinished = true;
        if (self.submitted == 3) {
            self.held = .{ .task = task, .sequence = self.submitted };
        } else {
            for (&self.pending) |*slot| if (slot.* == null) {
                slot.* = .{ .task = task, .sequence = self.submitted };
                break;
            };
        }
        self.submitted += 1;
        if (self.held != null) self.peak_unaccepted_while_held = @max(self.peak_unaccepted_while_held, self.submitted - 3);
        self.outstanding += 1;
        self.peak_outstanding = @max(self.peak_outstanding, self.outstanding);
    }

    fn receive(context: *anyopaque) executor_api.Completion {
        const self: *@This() = @ptrCast(@alignCast(context));
        std.debug.assert(self.outstanding > 0);
        self.outstanding -= 1;
        // Always execute ordinary tasks in dispatch order within lane zero.
        var next: ?usize = null;
        for (self.pending, 0..) |slot, index| if (slot) |task| {
            if (next == null or task.sequence < self.pending[next.?].?.sequence) next = index;
        };
        if (next) |index| {
            const task = self.pending[index].?;
            self.pending[index] = null;
            return self.execute(task.task, 0);
        }
        const task = self.held.?;
        self.held = null;
        return self.execute(task.task, 1);
    }

    fn end(context: *anyopaque) void {
        const self: *@This() = @ptrCast(@alignCast(context));
        std.debug.assert(self.open and self.outstanding == 0);
        self.open = false;
    }

    fn executor(self: *@This()) executor_api.Executor {
        return .{
            .context = self,
            .worker_count = 4,
            .beginFn = begin,
            .submitFn = submit,
            .receiveFn = receive,
            .endFn = end,
        };
    }
};

test "specialization discovery submits a child before an unrelated task finishes" {
    var executor: DiscoveryExecutor = .{};
    defer executor.deinit();
    var timing: lir.CheckedPipeline.TimingSnapshot = .{};
    var solved_lir: lir.CheckedPipeline.SolvedLirParallelMetrics = .{};
    try harness.expectLowersToLirWithOptions(chained_discovery_app, .{
        .specialization_workers = 4,
        .post_check_executor_override = executor.executor(),
        .timing_out = &timing,
        .solved_lir_parallel_metrics_out = &solved_lir,
    });
    try std.testing.expect(executor.discovered_while_unfinished);
    // The controlled overlap occurs in Monotype's first session. Subsequent
    // solved-LIR, rewrite, and ARC batches reuse this executor, but are not new specializations.
    const monotype = timing.monotype_parallel;
    const rewrites = timing.lir_pass_parallel;
    const arc = timing.arc_parallel;
    const spec_constr = timing.spec_constr_parallel;
    try std.testing.expectEqual(@as(u64, 0), monotype.root_tasks_submitted);
    try std.testing.expectEqual(@as(u64, 12), monotype.specialization_tasks_submitted);
    try std.testing.expectEqual(monotype.specialization_tasks_submitted, monotype.specialization_tasks_committed);
    try std.testing.expectEqual(solved_lir.tasks_submitted, solved_lir.tasks_committed);
    try std.testing.expectEqual(rewrites.tasks_submitted, rewrites.tasks_committed);
    try std.testing.expectEqual(spec_constr.tasks_submitted, spec_constr.tasks_committed);
    var spec_constr_tasks: u64 = 0;
    for (spec_constr.committed_by_phase) |count| spec_constr_tasks += count;
    try std.testing.expectEqual(spec_constr.tasks_committed, spec_constr_tasks);
    try std.testing.expectEqual(arc.source_tasks_submitted, arc.source_tasks_committed);
    try std.testing.expectEqual(arc.planning_tasks_submitted, arc.planning_tasks_committed);
    try std.testing.expectEqual(arc.emission_tasks_submitted, arc.emission_tasks_committed);
    try std.testing.expectEqual(arc.uniqueness.task_submitted, arc.uniqueness.task_committed);
    const arc_tasks = arc.source_tasks_submitted + arc.planning_tasks_submitted + arc.emission_tasks_submitted + arc.uniqueness.task_submitted;
    try std.testing.expectEqual(monotype.specialization_tasks_submitted + spec_constr.tasks_submitted + solved_lir.tasks_submitted + rewrites.tasks_submitted + arc_tasks, executor.submitted);
    try std.testing.expect(executor.peak_outstanding <= 4);
    try std.testing.expect(!executor.open);
}

test "specialization streaming joins accepted tasks after submission failure" {
    // Failure with a pending sibling, and failure after some ordered commits.
    for ([_]usize{ 2, 7 }) |fail_at| {
        var executor: DiscoveryExecutor = .{ .fail_at_submission = fail_at };
        defer executor.deinit();
        try std.testing.expectError(error.OutOfMemory, harness.expectLowersToLirWithOptions(chained_discovery_app, .{
            .specialization_workers = 4,
            .post_check_executor_override = executor.executor(),
        }));
        try std.testing.expectEqual(@as(usize, 0), executor.outstanding);
        try std.testing.expect(!executor.open);
    }
}

test "chained streaming specialization preserves serial LIR under reverse completions" {
    try harness.expectSpecializationParallelismDeterministicLir(chained_discovery_app);
}

test "streaming recursive references consume their reserved signature" {
    try harness.expectSpecializationParallelismDeterministicLir(
        \\count : U64 -> U64
        \\count = |value| if value == 0 { 0 } else { count(value - 1) + 1 }
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |args| {
        \\    echo!(count(args.len()).to_str())
        \\    Ok({})
        \\}
    );
}

test "specialization streaming bounds completed shards behind an unfinished predecessor" {
    const allocator = std.testing.allocator;
    var app: std.ArrayList(u8) = .empty;
    defer app.deinit(allocator);
    for (0..40) |index| try app.print(allocator, "body{d} : U64 -> U64\nbody{d} = |value| value + {d}\n\n", .{ index, index, index });
    try app.appendSlice(allocator, "main! : List(Str) => Try({}, [Exit(I8), ..])\nmain! = |args| {\n    runtime = args.len()\n    total = ");
    for (0..40) |index| {
        if (index != 0) try app.appendSlice(allocator, " + ");
        try app.print(allocator, "body{d}(runtime)", .{index});
    }
    try app.appendSlice(allocator, "\n    echo!(total.to_str())\n    Ok({})\n}\n");
    var executor: DiscoveryExecutor = .{};
    defer executor.deinit();
    try harness.expectLowersToLirWithOptions(app.items, .{
        .specialization_workers = 4,
        .post_check_executor_override = executor.executor(),
    });
    try std.testing.expectEqual(@as(usize, 16), executor.peak_unaccepted_while_held);
    try std.testing.expect(executor.submitted > 40);
    try std.testing.expect(!executor.open);
}
