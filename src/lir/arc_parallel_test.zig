//! ARC executor determinism and retained-owner cleanup across failed waves.

const std = @import("std");
const core = @import("lir_core");
const layout = @import("layout");
const executor = @import("base").post_check_task_executor;
const arc = @import("arc.zig");
const arc_solve = @import("arc_solve.zig");
const arc_sig = @import("arc_sig.zig");
const arc_dismantle = @import("arc_dismantle.zig");
const debug_print = @import("debug_print.zig");
const testing = std.testing;

const Fixture = struct {
    store: core.LirStore,
    layouts: layout.Store,

    fn init(count: usize) std.mem.Allocator.Error!Fixture {
        var self: Fixture = .{
            .store = core.LirStore.init(testing.allocator),
            .layouts = try layout.Store.init(testing.allocator, .u64),
        };
        errdefer self.deinit();
        for (0..count) |_| {
            const number = try self.store.addLocal(.{ .layout_idx = .i64 });
            const text = try self.store.addLocal(.{ .layout_idx = .str });
            const frame = try self.store.addLocalSpan(&.{ number, text });
            const body = try self.store.addCFStmt(.{ .ret = .{ .value = number } }, .test_fixture);
            _ = try self.store.addProcSpec(.{
                .identity = core.LIR.ProcIdentity.forTest(@intCast(self.store.procSpecCount())),
                .name = self.store.freshSyntheticSymbol(),
                .args = frame,
                .frame_locals = frame,
                .body = body,
                .ret_layout = .i64,
            }, .none);
        }
        return self;
    }

    fn deinit(self: *Fixture) void {
        self.store.deinit();
        self.layouts.deinit();
    }

    fn run(self: *Fixture, runner: ?*const executor.Executor, metrics: *arc.ParallelMetrics) arc.ResourceError!void {
        var roots: [40]core.LIR.LirProcSpecId = undefined;
        const count = self.store.procSpecCount();
        for (roots[0..count], 0..) |*root, index| root.* = @fromBackingInt(@intCast(index));
        // ABI roots own their unused string arguments, so every body must gain
        // a decref. Borrow inference cannot turn this into a no-op fixture.
        // Forty procedures over 32-wide waves give the two base waves these
        // tests' session and commit expectations are written against.
        try arc.insert(&self.store, &self.layouts, .{
            .roots = roots[0..count],
            .post_check_executor = runner,
            .metrics_out = metrics,
            .emission_wave_size = 32,
        });
    }

    fn dump(self: *Fixture, writer: *std.Io.Writer) debug_print.Error!void {
        for (0..self.store.procSpecCount()) |index| {
            try debug_print.writeProc(testing.allocator, &self.store, &self.layouts, @fromBackingInt(@intCast(index)), writer);
        }
    }
};

const ReverseExecutor = struct {
    lanes: usize = 4,
    pending: [4]executor.Task = undefined,
    pending_len: usize = 0,
    sessions: usize = 0,
    ended: usize = 0,
    accepted: usize = 0,
    received: usize = 0,
    fail_session: ?usize = null,
    fail_submit: bool = false,
    fail_task: usize = 2,
    rotate_wave_lane: bool = false,
    output_failure: testing.FailingAllocator = testing.FailingAllocator.init(testing.allocator, .{ .fail_index = 0 }),
    lane_states: [4]executor.LaneState = @splat(executor.LaneState.init(testing.allocator)),

    fn deinit(self: *ReverseExecutor) void {
        for (&self.lane_states) |*lane| lane.deinit();
    }

    fn begin(context: *anyopaque) void {
        const self: *ReverseExecutor = @ptrCast(@alignCast(context));
        std.debug.assert(self.pending_len == 0);
        self.sessions += 1;
    }

    fn selected(self: *ReverseExecutor, id: usize) bool {
        return self.fail_session == self.sessions - 1 and self.fail_task == id;
    }

    fn submit(context: *anyopaque, task: executor.Task) std.mem.Allocator.Error!void {
        const self: *ReverseExecutor = @ptrCast(@alignCast(context));
        if (self.fail_submit and self.selected(task.id)) return error.OutOfMemory;
        self.pending[self.pending_len] = task;
        self.pending_len += 1;
        self.accepted += 1;
    }

    fn receive(context: *anyopaque) executor.Completion {
        const self: *ReverseExecutor = @ptrCast(@alignCast(context));
        self.pending_len -= 1;
        const task = self.pending[self.pending_len];
        // Execute only when draining, including after a submit failure. This
        // catches releasing callback contexts before accepted work finishes.
        // Rotating whole waves forces retained component owners across lanes.
        const worker_id = if (self.rotate_wave_lane) (self.sessions - 1) % self.lanes else task.id % self.lanes;
        var scratch_bytes: [64 * 1024]u8 = undefined;
        var scratch = std.heap.FixedBufferAllocator.init(&scratch_bytes);
        const result = task.run(task.context, .{
            .id = worker_id,
            .allocator = if (!self.fail_submit and self.selected(task.id)) self.output_failure.allocator() else testing.allocator,
            .scratch = scratch.allocator(),
            .lane_state = &self.lane_states[worker_id],
        });
        @memset(&scratch_bytes, 0xa5);
        self.received += 1;
        return .{ .id = task.id, .worker_id = worker_id, .value = result };
    }

    fn end(context: *anyopaque) void {
        const self: *ReverseExecutor = @ptrCast(@alignCast(context));
        std.debug.assert(self.pending_len == 0);
        self.ended += 1;
    }

    fn interface(self: *ReverseExecutor) executor.Executor {
        return .{
            .context = self,
            .worker_count = self.lanes,
            .beginFn = begin,
            .submitFn = submit,
            .receiveFn = receive,
            .endFn = end,
        };
    }

    fn expectDrained(self: *ReverseExecutor) error{ TestExpectedEqual, TestUnexpectedResult }!void {
        try testing.expectEqual(self.sessions, self.ended);
        try testing.expectEqual(self.accepted, self.received);
        try testing.expectEqual(@as(usize, 0), self.pending_len);
    }
};

fn expectUniquenessMetrics(expected: arc_solve.UniquenessMetrics, actual: arc_solve.UniquenessMetrics) error{ TestExpectedEqual, TestUnexpectedResult }!void {
    try testing.expectEqual(@as(u64, 0), expected.task_submitted);
    try testing.expectEqual(@as(u64, 0), expected.task_committed);
    try testing.expect(actual.task_submitted > 0);
    try testing.expectEqual(actual.task_submitted, actual.task_committed);
    inline for (.{ "settlements", "components", "component_runs", "signature_waves", "signature_changes", "statement_visits", "local_visits" }) |counter| {
        try testing.expectEqual(@field(expected, counter), @field(actual, counter));
    }
}

fn expectSame(expected: *Fixture, actual: *Fixture) (debug_print.Error || error{TestExpectedEqual})!void {
    var left = std.Io.Writer.Allocating.init(testing.allocator);
    defer left.deinit();
    var right = std.Io.Writer.Allocating.init(testing.allocator);
    defer right.deinit();
    try expected.dump(&left.writer);
    try actual.dump(&right.writer);
    try testing.expectEqualStrings(left.written(), right.written());
    try testing.expectEqualDeep(expected.store.captureBodyPrefix(), actual.store.captureBodyPrefix());
    try testing.expectEqual(expected.store.procSpecCount(), actual.store.procSpecCount());
    for (0..expected.store.procSpecCount()) |index| {
        const id: core.LIR.LirProcSpecId = @fromBackingInt(@intCast(index));
        try testing.expectEqualDeep(expected.store.getProcSpec(id), actual.store.getProcSpec(id));
    }
    try testing.expectEqual(expected.store.next_synthetic_symbol, actual.store.next_synthetic_symbol);
}

test "ARC executor reverse completions match inline RC bodies and identities across waves" {
    var direct = try Fixture.init(40);
    defer direct.deinit();
    const before = direct.store.cfStmtCount();
    var inline_metrics: arc.ParallelMetrics = .{};
    try direct.run(null, &inline_metrics);
    try testing.expect(direct.store.cfStmtCount() > before);
    try testing.expectEqual(@as(u64, 2), inline_metrics.waves);
    for (0..direct.store.procSpecCount()) |index| {
        const spec = direct.store.getProcSpec(@fromBackingInt(@intCast(index)));
        const body = direct.store.getCFStmt(spec.body.?);
        try testing.expect(body == .decref);
        try testing.expectEqual(core.LirStore.GuardedList.at(direct.store.getLocalSpan(spec.args), 1), body.decref.value);
        try testing.expect(direct.store.getCFStmtImmediate(body.decref.next).* == .ret);
    }
    var expected_metrics: ?arc.ParallelMetrics = null;
    for ([_]usize{ 2, 4 }) |lanes| {
        var candidate = try Fixture.init(40);
        defer candidate.deinit();
        var runner: ReverseExecutor = .{ .lanes = lanes };
        defer runner.deinit();
        const interface = runner.interface();
        var metrics: arc.ParallelMetrics = .{};
        try candidate.run(&interface, &metrics);
        try runner.expectDrained();
        try testing.expectEqual(@as(u64, 120) + metrics.uniqueness.task_committed + metrics.dismantle_tasks_committed, runner.received);
        try expectUniquenessMetrics(inline_metrics.uniqueness, metrics.uniqueness);
        // Forty statements fit one dismantle fact range.
        try testing.expectEqual(@as(u64, 1), metrics.dismantle_tasks_submitted);
        try testing.expectEqual(@as(u64, 1), metrics.dismantle_tasks_committed);
        // These pinned, independent bodies return scalars: no signature can
        // change, so uniqueness is one initial wave before source preparation.
        try testing.expectEqual(@as(u64, 1), metrics.uniqueness.signature_waves);
        try testing.expectEqual(@as(u64, 40), metrics.uniqueness.component_runs);
        try testing.expectEqual(@as(u64, 40), metrics.source_tasks_submitted);
        try testing.expectEqual(@as(u64, 40), metrics.source_tasks_committed);
        try testing.expectEqual(@as(u64, 40), metrics.planning_tasks_submitted);
        try testing.expectEqual(@as(u64, 40), metrics.planning_tasks_committed);
        try testing.expectEqual(@as(u64, 40), metrics.emission_tasks_submitted);
        try testing.expectEqual(@as(u64, 40), metrics.emission_tasks_committed);
        try testing.expectEqual(inline_metrics.waves, metrics.waves);
        try testing.expectEqual(inline_metrics.variants_reserved, metrics.variants_reserved);
        if (expected_metrics) |expected| try testing.expectEqualDeep(expected, metrics);
        expected_metrics = metrics;
        try expectSame(&direct, &candidate);
    }
}

/// One executor session of `Fixture.init(8)`: the task whose submission or
/// output fails, and the callbacks every earlier session already received.
const FixturePhase = struct {
    session: usize,
    fail_task: usize,
    earlier: usize,
};

/// Uniqueness, dismantle facts, source, planning, emission. The pinned
/// scalar-return fixture has exactly one uniqueness wave, asserted by the
/// successful test above, and its eight statements fit one fact range.
const fixture_phases = [_]FixturePhase{
    .{ .session = 0, .fail_task = 2, .earlier = 0 },
    .{ .session = 1, .fail_task = 0, .earlier = 8 },
    .{ .session = 2, .fail_task = 2, .earlier = 9 },
    .{ .session = 3, .fail_task = 2, .earlier = 17 },
    .{ .session = 4, .fail_task = 2, .earlier = 25 },
};

test "ARC executor submit failures drain accepted callbacks in each phase" {
    for (fixture_phases) |phase| {
        var source = try Fixture.init(8);
        defer source.deinit();
        var candidate = try Fixture.init(8);
        defer candidate.deinit();
        var runner: ReverseExecutor = .{ .fail_session = phase.session, .fail_submit = true, .fail_task = phase.fail_task };
        defer runner.deinit();
        const interface = runner.interface();
        var metrics: arc.ParallelMetrics = .{};
        try testing.expectError(error.OutOfMemory, candidate.run(&interface, &metrics));
        try runner.expectDrained();
        try testing.expectEqual(phase.earlier + phase.fail_task, runner.received);
        try testing.expectEqual(@as(u64, 0), metrics.emission_tasks_committed);
        try expectSame(&source, &candidate);
        try candidate.run(null, &metrics);
    }
}

test "ARC executor sweeps retained allocations through uniqueness source planning and emission bodies" {
    // The dismantle fact range of these return-only bodies allocates nothing;
    // its own sweep runs against a fixture with statements to record.
    for (fixture_phases) |phase| {
        if (phase.session == 1) continue;
        var fail_index: usize = 0;
        while (true) : (fail_index += 1) {
            var source = try Fixture.init(8);
            defer source.deinit();
            var candidate = try Fixture.init(8);
            defer candidate.deinit();
            var runner: ReverseExecutor = .{
                .fail_session = phase.session,
                .fail_task = phase.fail_task,
                .output_failure = testing.FailingAllocator.init(testing.allocator, .{ .fail_index = fail_index }),
            };
            defer runner.deinit();
            const interface = runner.interface();
            var metrics: arc.ParallelMetrics = .{};
            const result = candidate.run(&interface, &metrics);
            try runner.expectDrained();
            if (runner.output_failure.has_induced_failure) {
                try testing.expectError(error.OutOfMemory, result);
                try testing.expect(runner.received >= phase.earlier + 4);
                try testing.expectEqual(@as(u64, 0), metrics.emission_tasks_committed);
                try expectSame(&source, &candidate);
            } else {
                try result;
                try testing.expect(fail_index > 1);
                try testing.expectEqual(@as(u64, 8), metrics.emission_tasks_committed);
                break;
            }
        }
    }
}

test "ARC executor later wave failure drains without rolling back earlier commits" {
    var candidate = try Fixture.init(40);
    defer candidate.deinit();
    const before = candidate.store.cfStmtCount();
    // One uniqueness wave, one dismantle fact range, two source batches, then
    // planning/emission for the first 32 bodies, followed by planning/emission
    // for the final eight.
    var runner: ReverseExecutor = .{ .fail_session = 7, .fail_submit = true };
    defer runner.deinit();
    const interface = runner.interface();
    var metrics: arc.ParallelMetrics = .{};
    try testing.expectError(error.OutOfMemory, candidate.run(&interface, &metrics));
    try runner.expectDrained();
    try testing.expectEqual(@as(u64, 32), metrics.emission_tasks_committed);
    try testing.expect(candidate.store.cfStmtCount() > before);
    // The caller discards this incomplete result; no stronger rollback is owed.
}

/// A unique-return chain and fork, an unrelated fresh birth, shared parameter
/// definitions, and an independent conditional return retained across waves.
const UniquenessFixture = struct {
    fixture: Fixture,

    fn init() std.mem.Allocator.Error!UniquenessFixture {
        var f = try Fixture.init(0);
        errdefer f.deinit();
        const s = &f.store;
        const list = try f.layouts.insertList(.str);
        for (0..6) |index| {
            const param = try s.addLocal(.{ .layout_idx = list });
            const result = if (index <= 3)
                try s.addLocal(.{ .layout_idx = list })
            else
                param;
            const ret = try s.addCFStmt(.{ .ret = .{ .value = result } }, .test_fixture);
            const body = if (index == 0)
                try s.addCFStmt(.{ .assign_low_level = .{
                    .target = result,
                    .op = .list_reverse,
                    .rc_effect = try s.addRcEffect(core.LIR.LowLevel.RcEffect.runtimeUniqueness(1)),
                    .args = try s.addLocalSpan(&.{param}),
                    .next = ret,
                } }, .test_fixture)
            else if (index <= 3)
                try s.addCFStmt(.{ .assign_call = .{
                    .target = result,
                    .proc = @fromBackingInt(@intCast(@as(u32, if (index == 2) 1 else 0))),
                    .args = try s.addLocalSpan(&.{param}),
                    .next = ret,
                } }, .test_fixture)
            else if (index == 4)
                try s.addCFStmt(.{ .assign_list = .{
                    .target = result,
                    .elems = .empty(),
                    .next = ret,
                } }, .test_fixture)
            else
                ret;
            _ = try s.addProcSpec(.{
                .identity = core.LIR.ProcIdentity.forTest(@intCast(index)),
                .name = s.freshSyntheticSymbol(),
                .args = if (index == 4) .empty() else try s.addLocalSpan(&.{param}),
                .frame_locals = try s.addLocalSpan(if (param == result) &.{param} else &.{ param, result }),
                .body = body,
                .ret_layout = list,
            }, .none);
        }
        const shared = s.getProcSpec(@fromBackingInt(@intCast(5)));
        for (0..2) |index| {
            var spec = shared;
            spec.identity = core.LIR.ProcIdentity.forTest(@intCast(s.procSpecCount()));
            spec.name = s.freshSyntheticSymbol();
            if (index == 1) spec.body = try s.addCFStmt(s.getCFStmt(shared.body.?), .test_fixture);
            _ = try s.addProcSpec(spec, .none);
        }
        const independent = try s.addLocal(.{ .layout_idx = list });
        _ = try s.addProcSpec(.{
            .identity = core.LIR.ProcIdentity.forTest(8),
            .name = s.freshSyntheticSymbol(),
            .args = try s.addLocalSpan(&.{independent}),
            .frame_locals = try s.addLocalSpan(&.{independent}),
            .body = try s.addCFStmt(.{ .ret = .{ .value = independent } }, .test_fixture),
            .ret_layout = list,
        }, .none);
        return .{ .fixture = f };
    }

    fn solve(self: *UniquenessFixture, runner: ?*const executor.Executor, metrics: *arc_solve.UniquenessMetrics) arc_solve.SolveError!arc_solve.Solution {
        const rc = try testing.allocator.alloc(bool, self.fixture.store.localCount());
        defer testing.allocator.free(rc);
        @memset(rc, true);
        return arc_solve.solveWithOptions(testing.allocator, &self.fixture.store, &self.fixture.layouts, rc, &.{}, &.{}, true, true, .{
            .executor = runner,
            .metrics = metrics,
        });
    }
};

fn expectSameUniqueness(expected: *const arc_solve.Solution, actual: *const arc_solve.Solution) error{TestExpectedEqual}!void {
    try testing.expectEqualDeep(expected.sigs, actual.sigs);
    try testing.expectEqualDeep(expected.ret_conditions, actual.ret_conditions);
    try testing.expectEqualDeep(expected.unique_conds, actual.unique_conds);
    for (0..expected.unique.bit_length) |index| {
        try testing.expectEqual(expected.unique.isSet(index), actual.unique.isSet(index));
        try testing.expectEqual(expected.unique_born.isSet(index), actual.unique_born.isSet(index));
        try testing.expectEqual(expected.unique_destroyed.isSet(index), actual.unique_destroyed.isSet(index));
    }
}

test "ARC public solve component barriers preserve conditional rows and shared global local verdicts" {
    var fixture = try UniquenessFixture.init();
    defer fixture.fixture.deinit();
    var inline_metrics: arc_solve.UniquenessMetrics = .{};
    var direct = try fixture.solve(null, &inline_metrics);
    defer direct.deinit();
    // Seven components, not nine: shared body and shared RC-local ownership
    // merge procedures 5..7 even though the last body is distinct.
    try testing.expectEqual(@as(u64, 7), inline_metrics.components);
    try testing.expect(inline_metrics.signature_waves >= 3);
    try testing.expect(inline_metrics.component_runs < inline_metrics.components * inline_metrics.signature_waves);
    for (direct.sigs, 0..) |sig, index| {
        if (index <= 4) {
            try testing.expect(sig.ret_unique);
        } else if (index == 8) {
            try testing.expect(!sig.ret_unique);
            try testing.expectEqualDeep(&[_]arc_sig.RetCondition{.{ .field = arc_sig.RetCondition.whole_value, .params = 1 }}, direct.sigTable().retConditionsOf(sig));
        } else {
            // Multiple definitions of one shared parameter are not independent
            // births. Keep the base solver's combined proof.
            try testing.expect(!sig.ret_unique);
            try testing.expectEqual(@as(usize, 0), direct.sigTable().retConditionsOf(sig).len);
        }
    }
    const shared_param = core.LirStore.GuardedList.at(fixture.fixture.store.getLocalSpan(fixture.fixture.store.getProcSpecImmediate(@fromBackingInt(@intCast(5))).args), 0);
    try testing.expect(!direct.unique_born.isSet(@backingInt(shared_param)));
    for ([_]usize{ 2, 4 }) |lanes| {
        var runner: ReverseExecutor = .{ .lanes = lanes, .rotate_wave_lane = true };
        defer runner.deinit();
        const interface = runner.interface();
        var metrics: arc_solve.UniquenessMetrics = .{};
        var actual = try fixture.solve(&interface, &metrics);
        defer actual.deinit();
        try runner.expectDrained();
        try expectSameUniqueness(&direct, &actual);
        try expectUniquenessMetrics(inline_metrics, metrics);
        try testing.expect(metrics.component_runs > metrics.components);
        const settled = metrics;
        const rc = try testing.allocator.alloc(bool, fixture.fixture.store.localCount());
        defer testing.allocator.free(rc);
        @memset(rc, true);
        // A post-take settlement reanalyzes every component, even when no
        // signature changed. Metrics accumulate; only Arc.insert resets them.
        try arc_solve.settleUniquenessWithOptions(testing.allocator, &fixture.fixture.store, &fixture.fixture.layouts, rc, &actual, .stamped, true, .{
            .executor = &interface,
            .metrics = &metrics,
        });
        try runner.expectDrained();
        try expectSameUniqueness(&direct, &actual);
        try testing.expectEqual(settled.settlements + 1, metrics.settlements);
        try testing.expectEqual(settled.components + 7, metrics.components);
        try testing.expectEqual(settled.component_runs + 7, metrics.component_runs);
        try testing.expectEqual(settled.signature_waves + 1, metrics.signature_waves);
        try testing.expectEqual(settled.signature_changes, metrics.signature_changes);
        try testing.expectEqual(settled.task_submitted + 7, metrics.task_submitted);
        try testing.expectEqual(metrics.task_submitted, metrics.task_committed);
    }
}

test "ARC public solve sweeps changing uniqueness callbacks and drains later wave failures" {
    var fixture = try UniquenessFixture.init();
    defer fixture.fixture.deinit();
    var reference_metrics: arc_solve.UniquenessMetrics = .{};
    var reference = try fixture.solve(null, &reference_metrics);
    defer reference.deinit();
    // Initial analysis (task 0) changes a unique-return signature; the later wave
    // fails its first submission only after the initial barrier committed.
    for ([_]bool{ false, true }) |fail_submit| {
        var fail_index: usize = 0;
        while (true) : (fail_index += 1) {
            var runner: ReverseExecutor = .{
                .fail_session = if (fail_submit) 1 else 0,
                .fail_task = 0,
                .fail_submit = fail_submit,
                .output_failure = testing.FailingAllocator.init(testing.allocator, .{ .fail_index = fail_index }),
            };
            defer runner.deinit();
            const interface = runner.interface();
            var metrics: arc_solve.UniquenessMetrics = .{};
            const result = fixture.solve(&interface, &metrics);
            try runner.expectDrained();
            if (fail_submit or runner.output_failure.has_induced_failure) {
                try testing.expectError(error.OutOfMemory, result);
                if (fail_submit) {
                    try testing.expect(metrics.task_committed > 0);
                    break;
                }
            } else {
                var actual = try result;
                defer actual.deinit();
                try testing.expect(fail_index > 1);
                try expectSameUniqueness(&reference, &actual);
                break;
            }
        }
    }
}

const OutcomeFixture = struct {
    fixture: Fixture,
    input: core.LIR.LocalId,

    fn init() std.mem.Allocator.Error!OutcomeFixture {
        var f = try Fixture.init(0);
        errdefer f.deinit();
        const list = try f.layouts.insertList(.str);
        const result_layout = try f.layouts.putTagUnion(&.{
            try f.layouts.ensureZstLayout(),
            list,
        });
        const s = &f.store;
        const param = try s.addLocal(.{ .layout_idx = list });
        const choose = try s.addLocal(.{ .layout_idx = .i64 });
        const changed = try s.addLocal(.{ .layout_idx = list });
        const result = try s.addLocal(.{ .layout_idx = result_layout });
        const ret = try s.addCFStmt(.{ .ret = .{ .value = result } }, .test_fixture);
        const success = try s.addCFStmt(.{ .assign_tag = .{
            .target = result,
            .variant_index = 1,
            .discriminant = 1,
            .payload = changed,
            .next = ret,
        } }, .test_fixture);
        const mutate = try s.addCFStmt(.{ .assign_low_level = .{
            .target = changed,
            .op = .list_reverse,
            .rc_effect = try s.addRcEffect(core.LIR.LowLevel.RcEffect.runtimeUniqueness(1)),
            .args = try s.addLocalSpan(&.{param}),
            .next = success,
        } }, .test_fixture);
        const failure = try s.addCFStmt(.{ .assign_tag = .{
            .target = result,
            .variant_index = 0,
            .discriminant = 0,
            .payload = null,
            .next = ret,
        } }, .test_fixture);
        const callee_body = try switchStmt(s, choose, mutate, failure);
        const callee = try s.addProcSpec(.{
            .identity = core.LIR.ProcIdentity.forTest(@intCast(s.procSpecCount())),
            .name = s.freshSyntheticSymbol(),
            .args = try s.addLocalSpan(&.{ param, choose }),
            .frame_locals = try s.addLocalSpan(&.{ param, choose, changed, result }),
            .body = callee_body,
            .ret_layout = result_layout,
        }, .none);
        const item = try s.addLocal(.{ .layout_idx = .str });
        const input = try s.addLocal(.{ .layout_idx = list });
        const caller_choose = try s.addLocal(.{ .layout_idx = .i64 });
        const call_result = try s.addLocal(.{ .layout_idx = result_layout });
        const discriminant = try s.addLocal(.{ .layout_idx = .u8 });
        const answer = try s.addLocal(.{ .layout_idx = .u64 });
        const caller_ret = try s.addCFStmt(.{ .ret = .{ .value = answer } }, .test_fixture);
        const caller_success = try s.addCFStmt(.{ .assign_literal = .{
            .target = answer,
            .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
            .next = caller_ret,
        } }, .test_fixture);
        // Only failure reads the exact original list; success consumes it.
        // This needs outcome restitution, not ordinary owned specialization.
        const caller_failure = try s.addCFStmt(.{ .assign_low_level = .{
            .target = answer,
            .op = .list_len,
            .rc_effect = try s.addRcEffect(core.LIR.LowLevel.list_len.rcEffect()),
            .args = try s.addLocalSpan(&.{input}),
            .next = caller_ret,
        } }, .test_fixture);
        const refine = try switchStmt(s, discriminant, caller_success, caller_failure);
        const read = try s.addCFStmt(.{ .assign_ref = .{
            .target = discriminant,
            .op = .{ .discriminant = .{ .source = call_result } },
            .next = refine,
        } }, .test_fixture);
        const call = try s.addCFStmt(.{ .assign_call = .{
            .target = call_result,
            .proc = callee,
            .args = try s.addLocalSpan(&.{ input, caller_choose }),
            .next = read,
        } }, .test_fixture);
        const make_list = try s.addCFStmt(.{ .assign_list = .{
            .target = input,
            .elems = try s.addLocalSpan(&.{item}),
            .next = call,
        } }, .test_fixture);
        const caller_body = try s.addCFStmt(.{ .assign_literal = .{
            .target = item,
            .value = .{ .str_literal = try s.insertStringView("nested", 0, 6) },
            .next = make_list,
        } }, .test_fixture);
        _ = try s.addProcSpec(.{
            .identity = core.LIR.ProcIdentity.forTest(@intCast(s.procSpecCount())),
            .name = s.freshSyntheticSymbol(),
            .args = try s.addLocalSpan(&.{caller_choose}),
            .frame_locals = try s.addLocalSpan(&.{ item, input, caller_choose, call_result, discriminant, answer }),
            .body = caller_body,
            .ret_layout = .u64,
        }, .none);
        return .{ .fixture = f, .input = input };
    }

    fn switchStmt(s: *core.LirStore, cond: core.LIR.LocalId, yes: core.LIR.CFStmtId, no: core.LIR.CFStmtId) std.mem.Allocator.Error!core.LIR.CFStmtId {
        return s.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = try s.addCFSwitchBranches(&.{.{ .value = 1, .body = yes }}),
            .default_branch = no,
            .continuation = null,
        } }, .test_fixture);
    }

    fn run(self: *OutcomeFixture, runner: ?*const executor.Executor, metrics: *arc.ParallelMetrics) arc.ResourceError!void {
        try arc.insert(&self.fixture.store, &self.fixture.layouts, .{
            .specialize = true,
            .post_check_executor = runner,
            .metrics_out = metrics,
        });
    }

    fn expectOutcome(self: *OutcomeFixture) error{ TestExpectedEqual, TestUnexpectedResult, UnexpectedFixtureStatement }!void {
        const s = &self.fixture.store;
        try testing.expectEqual(@as(usize, 3), s.procSpecCount());
        for ([_]u32{ 0, 2 }) |proc| {
            var cursor = s.getProcSpecImmediate(@fromBackingInt(@intCast(proc))).body.?;
            while (true) {
                const stmt = s.getCFStmt(cursor);
                if (stmt == .assign_low_level) {
                    try testing.expectEqual(core.LIR.LowLevel.list_reverse, stmt.assign_low_level.op);
                    try testing.expectEqual(@as(u64, if (proc == 0) 0 else 1), s.stmtRowOrDefault(stmt.assign_low_level.extra).unique_args);
                    break;
                }
                if (stmt == .switch_stmt) {
                    cursor = core.LirStore.GuardedList.at(s.getCFSwitchBranches(stmt.switch_stmt.branches), 0).body;
                } else cursor = try linearNext(stmt);
            }
        }
        var cursor = s.getProcSpecImmediate(@fromBackingInt(@intCast(1))).body.?;
        while (true) {
            const stmt = s.getCFStmt(cursor);
            if (stmt == .incref) try testing.expect(stmt.incref.value != self.input);
            if (stmt == .assign_call) {
                try testing.expectEqual(@as(core.LIR.LirProcSpecId, @fromBackingInt(@intCast(2))), stmt.assign_call.proc);
                break;
            }
            cursor = try linearNext(stmt);
        }
    }

    fn linearNext(stmt: core.LIR.CFStmt) error{UnexpectedFixtureStatement}!core.LIR.CFStmtId {
        inline for (.{ "assign_literal", "assign_list", "assign_ref", "incref", "decref", "free" }) |tag| {
            if (stmt == @field(std.meta.Tag(core.LIR.CFStmt), tag)) return @field(stmt, tag).next;
        }
        return error.UnexpectedFixtureStatement;
    }
};

test "ARC executor outcome restitution variant closure matches inline under reverse completions" {
    var direct = try OutcomeFixture.init();
    defer direct.fixture.deinit();
    var inline_metrics: arc.ParallelMetrics = .{};
    try direct.run(null, &inline_metrics);
    try direct.expectOutcome();
    try testing.expectEqual(@as(u64, 1), inline_metrics.variants_reserved);
    try testing.expectEqual(@as(u64, 2), inline_metrics.waves);
    var expected_metrics: ?arc.ParallelMetrics = null;
    for ([_]usize{ 2, 4 }) |lanes| {
        var candidate = try OutcomeFixture.init();
        defer candidate.fixture.deinit();
        var runner: ReverseExecutor = .{ .lanes = lanes };
        defer runner.deinit();
        const interface = runner.interface();
        var metrics: arc.ParallelMetrics = .{};
        try candidate.run(&interface, &metrics);
        try runner.expectDrained();
        try candidate.expectOutcome();
        try expectSame(&direct.fixture, &candidate.fixture);
        try testing.expectEqual(@as(u64, 2), metrics.source_tasks_committed);
        try testing.expectEqual(@as(u64, 3), metrics.planning_tasks_committed);
        try testing.expectEqual(@as(u64, 3), metrics.emission_tasks_committed);
        try testing.expectEqual(metrics.source_tasks_committed, metrics.source_tasks_submitted);
        try testing.expectEqual(metrics.planning_tasks_committed, metrics.planning_tasks_submitted);
        try testing.expectEqual(metrics.emission_tasks_committed, metrics.emission_tasks_submitted);
        try testing.expectEqual(inline_metrics.waves, metrics.waves);
        try testing.expectEqual(inline_metrics.variants_reserved, metrics.variants_reserved);
        try expectUniquenessMetrics(inline_metrics.uniqueness, metrics.uniqueness);
        if (expected_metrics) |expected| try testing.expectEqualDeep(expected, metrics);
        expected_metrics = metrics;
    }
}

/// Divergent field takes of a dying pair on both sides of a straight-line
/// procedure long enough to span several dismantle fact ranges.
const DismantleRangeFixture = struct {
    fixture: Fixture,
    takes: [2][2]core.LIR.CFStmtId = undefined,

    fn init() std.mem.Allocator.Error!DismantleRangeFixture {
        var self: DismantleRangeFixture = .{ .fixture = try Fixture.init(0) };
        errdefer self.fixture.deinit();
        const pair_layout = try self.fixture.layouts.putStructFields(&[_]layout.StructField{
            .{ .index = 0, .layout = .str },
            .{ .index = 1, .layout = .str },
        });
        try self.addPairProc(pair_layout, 0);
        const s = &self.fixture.store;
        const counter = try s.addLocal(.{ .layout_idx = .i64 });
        const tick = try s.addProcSpec(.{
            .identity = core.LIR.ProcIdentity.forTest(@intCast(s.procSpecCount())),
            .name = s.freshSyntheticSymbol(),
            .args = .empty(),
            .body = null,
            .ret_layout = .i64,
        }, .none);
        var body = try s.addCFStmt(.{ .ret = .{ .value = counter } }, .test_fixture);
        for (0..20_000) |index| {
            body = if (index % 2 == 0)
                try s.addCFStmt(.{ .assign_literal = .{
                    .target = counter,
                    .value = .{ .i64_literal = .{ .value = @intCast(index), .layout_idx = .i64 } },
                    .next = body,
                } }, .test_fixture)
            else
                try s.addCFStmt(.{ .assign_call = .{
                    .target = counter,
                    .proc = tick,
                    .args = .empty(),
                    .next = body,
                } }, .test_fixture);
        }
        _ = try s.addProcSpec(.{
            .identity = core.LIR.ProcIdentity.forTest(@intCast(s.procSpecCount())),
            .name = s.freshSyntheticSymbol(),
            .args = .empty(),
            .frame_locals = try s.addLocalSpan(&.{counter}),
            .body = body,
            .ret_layout = .i64,
        }, .none);
        try self.addPairProc(pair_layout, 1);
        return self;
    }

    fn addPairProc(self: *DismantleRangeFixture, pair_layout: layout.Idx, slot: usize) std.mem.Allocator.Error!void {
        const s = &self.fixture.store;
        const sink = try s.addProcSpec(.{
            .identity = core.LIR.ProcIdentity.forTest(@intCast(s.procSpecCount())),
            .name = s.freshSyntheticSymbol(),
            .args = .empty(),
            .body = null,
            .ret_layout = .i64,
        }, .none);
        const cond = try s.addLocal(.{ .layout_idx = .i64 });
        const first = try s.addLocal(.{ .layout_idx = .str });
        const second = try s.addLocal(.{ .layout_idx = .str });
        const pair = try s.addLocal(.{ .layout_idx = pair_layout });
        const first_read = try s.addLocal(.{ .layout_idx = .str });
        const second_read = try s.addLocal(.{ .layout_idx = .str });
        const first_sink = try s.addLocal(.{ .layout_idx = .i64 });
        const second_sink = try s.addLocal(.{ .layout_idx = .i64 });
        const result = try s.addLocal(.{ .layout_idx = .i64 });
        const ret = try s.addCFStmt(.{ .ret = .{ .value = result } }, .test_fixture);
        const first_call = try s.addCFStmt(.{ .assign_call = .{
            .target = first_sink,
            .proc = sink,
            .args = try s.addLocalSpan(&.{first_read}),
            .next = ret,
        } }, .test_fixture);
        const first_branch = try s.addCFStmt(.{ .assign_ref = .{
            .target = first_read,
            .op = .{ .field = .{ .source = pair, .field_idx = 0 } },
            .next = first_call,
        } }, .test_fixture);
        const second_call = try s.addCFStmt(.{ .assign_call = .{
            .target = second_sink,
            .proc = sink,
            .args = try s.addLocalSpan(&.{second_read}),
            .next = ret,
        } }, .test_fixture);
        const second_branch = try s.addCFStmt(.{ .assign_ref = .{
            .target = second_read,
            .op = .{ .field = .{ .source = pair, .field_idx = 1 } },
            .next = second_call,
        } }, .test_fixture);
        const choose = try s.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = try s.addCFSwitchBranches(&.{.{ .value = 0, .body = first_branch }}),
            .default_branch = second_branch,
            .continuation = null,
        } }, .test_fixture);
        const assign_result = try s.addCFStmt(.{ .assign_literal = .{
            .target = result,
            .value = .{ .i64_literal = .{ .value = 7, .layout_idx = .i64 } },
            .next = choose,
        } }, .test_fixture);
        const assign_pair = try s.addCFStmt(.{ .assign_struct = .{
            .target = pair,
            .fields = try s.addLocalSpan(&.{ first, second }),
            .next = assign_result,
        } }, .test_fixture);
        const assign_second = try s.addCFStmt(.{ .assign_literal = .{
            .target = second,
            .value = .{ .str_literal = try s.insertStringView("second", 0, 6) },
            .next = assign_pair,
        } }, .test_fixture);
        const body = try s.addCFStmt(.{ .assign_literal = .{
            .target = first,
            .value = .{ .str_literal = try s.insertStringView("first", 0, 5) },
            .next = assign_second,
        } }, .test_fixture);
        _ = try s.addProcSpec(.{
            .identity = core.LIR.ProcIdentity.forTest(@intCast(s.procSpecCount())),
            .name = s.freshSyntheticSymbol(),
            .args = try s.addLocalSpan(&.{cond}),
            .frame_locals = try s.addLocalSpan(&.{ cond, first, second, pair, first_read, second_read, first_sink, second_sink, result }),
            .body = body,
            .ret_layout = .i64,
        }, .none);
        self.takes[slot] = .{ first_branch, second_branch };
    }

    fn compute(self: *DismantleRangeFixture, options: arc_dismantle.ComputeOptions) arc_dismantle.Error!arc_dismantle.Dismantles {
        const store = &self.fixture.store;
        const rc = try arc_solve.computeLocalContainsRefcounted(testing.allocator, store, &self.fixture.layouts);
        defer testing.allocator.free(rc);
        var solution = try arc_solve.solve(testing.allocator, store, &self.fixture.layouts, rc, &.{}, &.{}, true);
        defer solution.deinit();
        return arc_dismantle.compute(testing.allocator, store, &self.fixture.layouts, rc, &solution, options);
    }
};

fn expectSameMap(expected: anytype, actual: anytype) error{TestExpectedEqual}!void {
    try testing.expectEqual(expected.count(), actual.count());
    // Iteration takes a mutable map; these copies share the maps' storage.
    var expected_map = expected;
    var actual_map = actual;
    var expected_it = expected_map.iterator();
    var actual_it = actual_map.iterator();
    while (expected_it.next()) |entry| {
        const other = actual_it.next().?;
        try testing.expectEqualDeep(entry.key_ptr.*, other.key_ptr.*);
        try testing.expectEqualDeep(entry.value_ptr.*, other.value_ptr.*);
    }
}

fn expectSameDismantles(expected: *const arc_dismantle.Dismantles, actual: *const arc_dismantle.Dismantles) error{TestExpectedEqual}!void {
    try expectSameMap(expected.takes, actual.takes);
    try expectSameMap(expected.containers, actual.containers);
    try expectSameMap(expected.owned_only_takes, actual.owned_only_takes);
    try expectSameMap(expected.owned_only_containers, actual.owned_only_containers);
    try expectSameMap(expected.field_restitution_args, actual.field_restitution_args);
    try expectSameMap(expected.projection_units, actual.projection_units);
    try expectSameMap(expected.complete_takes, actual.complete_takes);
    try expectSameMap(expected.projection_aliases, actual.projection_aliases);
    try testing.expectEqualSlices(arc_sig.ParamMask, expected.owned_only_param_benefits, actual.owned_only_param_benefits);
    try testing.expectEqualSlices(bool, expected.take_bindings, actual.take_bindings);
    try testing.expectEqualSlices(u32, expected.owned_only_binding_roots, actual.owned_only_binding_roots);
}

test "ARC dismantle fact ranges match inline analysis under reverse completions and drain failures" {
    var direct = try DismantleRangeFixture.init();
    defer direct.fixture.deinit();
    var inline_metrics: arc_dismantle.TaskMetrics = .{};
    var expected = try direct.compute(.{ .metrics = &inline_metrics });
    defer expected.deinit();
    try testing.expectEqual(arc_dismantle.TaskMetrics{}, inline_metrics);
    for (direct.takes) |pair_reads| {
        for (pair_reads) |read| try testing.expect(expected.takes.contains(read));
    }
    const range_count: u64 = 3;
    try testing.expect(direct.fixture.store.cfStmtCount() > 2 * 8192);
    try testing.expect(direct.fixture.store.cfStmtCount() <= 3 * 8192);

    for ([_]usize{ 2, 4 }) |lanes| {
        var candidate = try DismantleRangeFixture.init();
        defer candidate.fixture.deinit();
        var runner: ReverseExecutor = .{ .lanes = lanes };
        defer runner.deinit();
        const interface = runner.interface();
        var metrics: arc_dismantle.TaskMetrics = .{};
        var actual = try candidate.compute(.{ .executor = &interface, .metrics = &metrics });
        defer actual.deinit();
        try runner.expectDrained();
        try testing.expectEqual(range_count, runner.received);
        try testing.expectEqual(arc_dismantle.TaskMetrics{ .submitted = range_count, .committed = range_count }, metrics);
        try expectSameDismantles(&expected, &actual);
    }

    // Every range records direct calls, so a failing output allocator fails
    // its range; earlier and later accepted ranges still drain.
    for (0..range_count) |fail_task| {
        var fail_index: usize = 0;
        while (true) : (fail_index += 1) {
            var candidate = try DismantleRangeFixture.init();
            defer candidate.fixture.deinit();
            var runner: ReverseExecutor = .{
                .lanes = 2,
                .fail_session = 0,
                .fail_task = fail_task,
                .output_failure = testing.FailingAllocator.init(testing.allocator, .{ .fail_index = fail_index }),
            };
            defer runner.deinit();
            const interface = runner.interface();
            var metrics: arc_dismantle.TaskMetrics = .{};
            const result = candidate.compute(.{ .executor = &interface, .metrics = &metrics });
            try runner.expectDrained();
            if (runner.output_failure.has_induced_failure) {
                try testing.expectError(error.OutOfMemory, result);
                try testing.expect(metrics.committed < metrics.submitted);
            } else {
                var actual = try result;
                defer actual.deinit();
                try testing.expect(fail_index > 0);
                try expectSameDismantles(&expected, &actual);
                break;
            }
        }
    }

    // A refused submission stops admission after the ranges already accepted.
    var candidate = try DismantleRangeFixture.init();
    defer candidate.fixture.deinit();
    var runner: ReverseExecutor = .{ .lanes = 2, .fail_session = 0, .fail_task = 2, .fail_submit = true };
    defer runner.deinit();
    const interface = runner.interface();
    var metrics: arc_dismantle.TaskMetrics = .{};
    try testing.expectError(error.OutOfMemory, candidate.compute(.{ .executor = &interface, .metrics = &metrics }));
    try runner.expectDrained();
    try testing.expectEqual(@as(usize, 2), runner.received);
    try testing.expectEqual(arc_dismantle.TaskMetrics{ .submitted = 2, .committed = 2 }, metrics);
}
