//! ARC executor determinism and retained-owner cleanup across failed waves.

const std = @import("std");
const core = @import("lir_core");
const layout = @import("layout");
const executor = @import("base").post_check_task_executor;
const arc = @import("arc.zig");
const debug_print = @import("debug_print.zig");
const testing = std.testing;

const Fixture = struct {
    store: core.LirStore,
    layouts: layout.Store,

    fn init(count: usize) !Fixture {
        var self: Fixture = .{
            .store = core.LirStore.init(testing.allocator),
            .layouts = try layout.Store.init(testing.allocator, .u64),
        };
        errdefer self.deinit();
        for (0..count) |_| {
            const number = try self.store.addLocal(.{ .layout_idx = .i64 });
            const text = try self.store.addLocal(.{ .layout_idx = .str });
            const frame = try self.store.addLocalSpan(&.{ number, text });
            const body = try self.store.addCFStmt(.{ .ret = .{ .value = number } });
            _ = try self.store.addProcSpec(.{
                .name = self.store.freshSyntheticSymbol(),
                .args = frame,
                .frame_locals = frame,
                .body = body,
                .ret_layout = .i64,
            });
        }
        return self;
    }

    fn deinit(self: *Fixture) void {
        self.store.deinit();
        self.layouts.deinit();
    }

    fn run(self: *Fixture, runner: ?*const executor.Executor, metrics: *arc.ParallelMetrics) !void {
        var roots: [40]core.LIR.LirProcSpecId = undefined;
        const count = self.store.procSpecCount();
        for (roots[0..count], 0..) |*root, index| root.* = @enumFromInt(index);
        // ABI roots own their unused string arguments, so every body must gain
        // a decref. Borrow inference cannot turn this into a no-op fixture.
        try arc.insert(&self.store, &self.layouts, .{
            .roots = roots[0..count],
            .post_check_executor = runner,
            .metrics_out = metrics,
        });
    }

    fn dump(self: *Fixture, writer: *std.Io.Writer) !void {
        for (0..self.store.procSpecCount()) |index| {
            try debug_print.writeProc(testing.allocator, &self.store, &self.layouts, @enumFromInt(index), writer);
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
    output_failure: testing.FailingAllocator = testing.FailingAllocator.init(testing.allocator, .{ .fail_index = 0 }),
    scratch_failure: testing.FailingAllocator = testing.FailingAllocator.init(testing.allocator, .{ .fail_index = 0 }),
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
        const worker_id = task.id % self.lanes;
        const result = task.run(task.context, .{
            .id = worker_id,
            .allocator = if (!self.fail_submit and self.selected(task.id)) self.output_failure.allocator() else testing.allocator,
            .scratch = self.scratch_failure.allocator(),
            .lane_state = &self.lane_states[worker_id],
        });
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

    fn expectDrained(self: *ReverseExecutor) !void {
        try testing.expectEqual(self.sessions, self.ended);
        try testing.expectEqual(self.accepted, self.received);
        try testing.expectEqual(@as(usize, 0), self.pending_len);
        // ARC deliberately retains owner arenas, not lane scratch. Supplying
        // fail-at-zero scratch pins that lifetime distinction in every phase.
        try testing.expect(!self.scratch_failure.has_induced_failure);
    }
};

fn expectSame(expected: *Fixture, actual: *Fixture) !void {
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
        const id: core.LIR.LirProcSpecId = @enumFromInt(index);
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
        const spec = direct.store.getProcSpec(@enumFromInt(index));
        const body = direct.store.getCFStmt(spec.body.?);
        try testing.expect(body == .decref);
        try testing.expectEqual(core.LirStore.GuardedList.at(direct.store.getLocalSpan(spec.args), 1), body.decref.value);
        try testing.expect(direct.store.getCFStmt(body.decref.next) == .ret);
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
        try testing.expectEqual(@as(usize, 120), runner.received);
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

test "ARC executor submit failures drain accepted callbacks in each phase" {
    for (0..3) |phase| {
        var source = try Fixture.init(8);
        defer source.deinit();
        var candidate = try Fixture.init(8);
        defer candidate.deinit();
        var runner: ReverseExecutor = .{ .fail_session = phase, .fail_submit = true };
        defer runner.deinit();
        const interface = runner.interface();
        var metrics: arc.ParallelMetrics = .{};
        try testing.expectError(error.OutOfMemory, candidate.run(&interface, &metrics));
        try runner.expectDrained();
        try testing.expectEqual(phase * 8 + 2, runner.received);
        try testing.expectEqual(@as(u64, 0), metrics.emission_tasks_committed);
        try expectSame(&source, &candidate);
        try candidate.run(null, &metrics);
    }
}

test "ARC executor sweeps retained allocations through source planning and emission bodies" {
    for (0..3) |phase| {
        var fail_index: usize = 0;
        while (true) : (fail_index += 1) {
            var source = try Fixture.init(8);
            defer source.deinit();
            var candidate = try Fixture.init(8);
            defer candidate.deinit();
            var runner: ReverseExecutor = .{
                .fail_session = phase,
                .output_failure = testing.FailingAllocator.init(testing.allocator, .{ .fail_index = fail_index }),
            };
            defer runner.deinit();
            const interface = runner.interface();
            var metrics: arc.ParallelMetrics = .{};
            const result = candidate.run(&interface, &metrics);
            try runner.expectDrained();
            if (runner.output_failure.has_induced_failure) {
                try testing.expectError(error.OutOfMemory, result);
                try testing.expect(runner.received >= phase * 8 + 4);
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
    // Two source batches, then planning/emission for the first 32 bodies,
    // followed by planning/emission for the final eight.
    var runner: ReverseExecutor = .{ .fail_session = 5, .fail_submit = true };
    defer runner.deinit();
    const interface = runner.interface();
    var metrics: arc.ParallelMetrics = .{};
    try testing.expectError(error.OutOfMemory, candidate.run(&interface, &metrics));
    try runner.expectDrained();
    try testing.expectEqual(@as(u64, 32), metrics.emission_tasks_committed);
    try testing.expect(candidate.store.cfStmtCount() > before);
    // The caller discards this incomplete result; no stronger rollback is owed.
}

const OutcomeFixture = struct {
    fixture: Fixture,
    input: core.LIR.LocalId,

    fn init() !OutcomeFixture {
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
        const ret = try s.addCFStmt(.{ .ret = .{ .value = result } });
        const success = try s.addCFStmt(.{ .assign_tag = .{
            .target = result,
            .variant_index = 1,
            .discriminant = 1,
            .payload = changed,
            .next = ret,
        } });
        const mutate = try s.addCFStmt(.{ .assign_low_level = .{
            .target = changed,
            .op = .list_reverse,
            .rc_effect = core.LIR.LowLevel.RcEffect.runtimeUniqueness(1),
            .args = try s.addLocalSpan(&.{param}),
            .next = success,
        } });
        const failure = try s.addCFStmt(.{ .assign_tag = .{
            .target = result,
            .variant_index = 0,
            .discriminant = 0,
            .payload = null,
            .next = ret,
        } });
        const callee_body = try switchStmt(s, choose, mutate, failure);
        const callee = try s.addProcSpec(.{
            .name = s.freshSyntheticSymbol(),
            .args = try s.addLocalSpan(&.{ param, choose }),
            .frame_locals = try s.addLocalSpan(&.{ param, choose, changed, result }),
            .body = callee_body,
            .ret_layout = result_layout,
        });
        const item = try s.addLocal(.{ .layout_idx = .str });
        const input = try s.addLocal(.{ .layout_idx = list });
        const caller_choose = try s.addLocal(.{ .layout_idx = .i64 });
        const call_result = try s.addLocal(.{ .layout_idx = result_layout });
        const discriminant = try s.addLocal(.{ .layout_idx = .u8 });
        const answer = try s.addLocal(.{ .layout_idx = .u64 });
        const caller_ret = try s.addCFStmt(.{ .ret = .{ .value = answer } });
        const caller_success = try s.addCFStmt(.{ .assign_literal = .{
            .target = answer,
            .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } },
            .next = caller_ret,
        } });
        // Only failure reads the exact original list; success consumes it.
        // This needs outcome restitution, not ordinary owned specialization.
        const caller_failure = try s.addCFStmt(.{ .assign_low_level = .{
            .target = answer,
            .op = .list_len,
            .rc_effect = core.LIR.LowLevel.list_len.rcEffect(),
            .args = try s.addLocalSpan(&.{input}),
            .next = caller_ret,
        } });
        const refine = try switchStmt(s, discriminant, caller_success, caller_failure);
        const read = try s.addCFStmt(.{ .assign_ref = .{
            .target = discriminant,
            .op = .{ .discriminant = .{ .source = call_result } },
            .next = refine,
        } });
        const call = try s.addCFStmt(.{ .assign_call = .{
            .target = call_result,
            .proc = callee,
            .args = try s.addLocalSpan(&.{ input, caller_choose }),
            .next = read,
        } });
        const make_list = try s.addCFStmt(.{ .assign_list = .{
            .target = input,
            .elems = try s.addLocalSpan(&.{item}),
            .next = call,
        } });
        const caller_body = try s.addCFStmt(.{ .assign_literal = .{
            .target = item,
            .value = .{ .str_literal = try s.insertStringView("nested", 0, 6) },
            .next = make_list,
        } });
        _ = try s.addProcSpec(.{
            .name = s.freshSyntheticSymbol(),
            .args = try s.addLocalSpan(&.{caller_choose}),
            .frame_locals = try s.addLocalSpan(&.{ item, input, caller_choose, call_result, discriminant, answer }),
            .body = caller_body,
            .ret_layout = .u64,
        });
        return .{ .fixture = f, .input = input };
    }

    fn switchStmt(s: *core.LirStore, cond: core.LIR.LocalId, yes: core.LIR.CFStmtId, no: core.LIR.CFStmtId) !core.LIR.CFStmtId {
        return s.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = try s.addCFSwitchBranches(&.{.{ .value = 1, .body = yes }}),
            .default_branch = no,
            .continuation = null,
        } });
    }

    fn run(self: *OutcomeFixture, runner: ?*const executor.Executor, metrics: *arc.ParallelMetrics) !void {
        try arc.insert(&self.fixture.store, &self.fixture.layouts, .{
            .specialize = true,
            .post_check_executor = runner,
            .metrics_out = metrics,
        });
    }

    fn expectOutcome(self: *OutcomeFixture) !void {
        const s = &self.fixture.store;
        try testing.expectEqual(@as(usize, 3), s.procSpecCount());
        for ([_]u32{ 0, 2 }) |proc| {
            var cursor = s.getProcSpec(@enumFromInt(proc)).body.?;
            while (true) {
                const stmt = s.getCFStmt(cursor);
                if (stmt == .assign_low_level) {
                    try testing.expectEqual(core.LIR.LowLevel.list_reverse, stmt.assign_low_level.op);
                    try testing.expectEqual(@as(u64, if (proc == 0) 0 else 1), stmt.assign_low_level.unique_args);
                    break;
                }
                if (stmt == .switch_stmt) {
                    cursor = core.LirStore.GuardedList.at(s.getCFSwitchBranches(stmt.switch_stmt.branches), 0).body;
                } else cursor = try linearNext(stmt);
            }
        }
        var cursor = s.getProcSpec(@enumFromInt(1)).body.?;
        while (true) {
            const stmt = s.getCFStmt(cursor);
            if (stmt == .incref) try testing.expect(stmt.incref.value != self.input);
            if (stmt == .assign_call) {
                try testing.expectEqual(@as(core.LIR.LirProcSpecId, @enumFromInt(2)), stmt.assign_call.proc);
                break;
            }
            cursor = try linearNext(stmt);
        }
    }

    fn linearNext(stmt: core.LIR.CFStmt) !core.LIR.CFStmtId {
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
        if (expected_metrics) |expected| try testing.expectEqualDeep(expected, metrics);
        expected_metrics = metrics;
    }
}
