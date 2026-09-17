//! Deterministic procedure commits and worker-failure isolation at the LIR pass boundary.

const std = @import("std");
const core = @import("lir_core");
const layout = @import("layout");
const executor = @import("base").post_check_task_executor;
const passes = @import("proc_passes.zig");
const debug_print = @import("debug_print.zig");
const testing = std.testing;

const Fixture = struct {
    store: core.LirStore,
    layouts: layout.Store,

    fn init() std.mem.Allocator.Error!Fixture {
        var self: Fixture = .{
            .store = core.LirStore.init(testing.allocator),
            .layouts = try layout.Store.init(testing.allocator, .u64),
        };
        errdefer self.deinit();
        const pair = try self.layouts.putStructFields(&.{
            .{ .index = 0, .layout = .i64 },
            .{ .index = 1, .layout = .str },
        });
        // More procedures than lanes exercises replenishment as well as a
        // reversed first group. Each body has a concrete scalarization.
        for (0..8) |_| {
            const number = try self.store.addLocal(.{ .layout_idx = .i64 });
            const text = try self.store.addLocal(.{ .layout_idx = .str });
            const wrapper = try self.store.addLocal(.{ .layout_idx = pair });
            const projected = try self.store.addLocal(.{ .layout_idx = .i64 });
            const ret = try self.store.addCFStmt(.{ .ret = .{ .value = projected } });
            const read = try self.store.addCFStmt(.{ .assign_ref = .{
                .target = projected,
                .op = .{ .field = .{ .source = wrapper, .field_idx = 0 } },
                .next = ret,
            } });
            const body = try self.store.addCFStmt(.{ .assign_struct = .{
                .target = wrapper,
                .fields = try self.store.addLocalSpan(&.{ number, text }),
                .next = read,
            } });
            _ = try self.store.addProcSpec(.{
                .identity = core.LIR.ProcIdentity.forTest(@intCast(self.store.procSpecCount())),
                .name = self.store.freshSyntheticSymbol(),
                .args = try self.store.addLocalSpan(&.{ number, text }),
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

    fn dump(self: *Fixture, writer: *std.Io.Writer) (std.mem.Allocator.Error || std.Io.Writer.Error)!void {
        for (0..self.store.procSpecCount()) |index| {
            try debug_print.writeProc(testing.allocator, &self.store, &self.layouts, @enumFromInt(index), writer);
        }
    }

    fn useErasedAbi(self: *Fixture) std.mem.Allocator.Error!void {
        const capture_layout = try self.layouts.insertPtr(.zst);
        const reuse_layout = try self.layouts.insertErasedCallable();
        for (0..self.store.procSpecCount()) |index| {
            const id: core.LIR.LirProcSpecId = @enumFromInt(index);
            const source_args = self.store.getLocalSpan(self.store.getProcSpec(id).args);
            const number = core.LirStore.GuardedList.at(source_args, 0);
            const text = core.LirStore.GuardedList.at(source_args, 1);
            const capture = try self.store.addLocal(.{ .layout_idx = capture_layout });
            const reuse = try self.store.addLocal(.{ .layout_idx = reuse_layout });
            const args = try self.store.addLocalSpan(&.{ number, text, capture, reuse });
            const proc = self.store.getProcSpecPtr(id);
            proc.abi = .erased_callable;
            proc.args = args;
            proc.erased_capture_arg = capture;
            proc.erased_reuse_arg = reuse;
        }
    }

    fn initJoins() std.mem.Allocator.Error!Fixture {
        var self: Fixture = .{
            .store = core.LirStore.init(testing.allocator),
            .layouts = try layout.Store.init(testing.allocator, .u64),
        };
        errdefer self.deinit();
        const pair = try self.layouts.putStructFields(&.{
            .{ .index = 0, .layout = .i64 },
            .{ .index = 1, .layout = .str },
        });
        for (0..8) |index| {
            const state = try self.store.addLocal(.{ .layout_idx = pair });
            const input = try self.store.addLocal(.{ .layout_idx = pair });
            const number = try self.store.addLocal(.{ .layout_idx = .i64 });
            const text = try self.store.addLocal(.{ .layout_idx = .str });
            const join_id: core.LIR.JoinPointId = @enumFromInt(index);
            const ret = try self.store.addCFStmt(.{ .ret = .{ .value = number } });
            const read_text = try self.store.addCFStmt(.{ .assign_ref = .{
                .target = text,
                .op = .{ .field = .{ .source = state, .field_idx = 1 } },
                .next = ret,
            } });
            const read_number = try self.store.addCFStmt(.{ .assign_ref = .{
                .target = number,
                .op = .{ .field = .{ .source = state, .field_idx = 0 } },
                .next = read_text,
            } });
            const jump = try self.store.addCFStmt(.{ .jump = .{ .target = join_id } });
            // A non-constructor initializer requires appended field reads and
            // assignments, rather than merely deleting an existing constructor.
            const set = try self.store.addCFStmt(.{ .set_local = .{
                .target = state,
                .value = input,
                .mode = .initialize_join_param,
                .next = jump,
            } });
            const body = try self.store.addCFStmt(.{ .join = .{
                .id = join_id,
                .params = try self.store.addLocalSpan(&.{state}),
                .body = read_number,
                .remainder = set,
            } });
            _ = try self.store.addProcSpec(.{
                .identity = core.LIR.ProcIdentity.forTest(@intCast(self.store.procSpecCount())),
                .name = self.store.freshSyntheticSymbol(),
                .args = try self.store.addLocalSpan(&.{input}),
                .body = body,
                .ret_layout = .i64,
            });
        }
        return self;
    }
};

/// Deliberately synchronous callbacks isolate ordering and allocator ownership
/// from OS scheduling; compile harness tests separately exercise real workers.
const ReverseExecutor = struct {
    lanes: usize,
    pending: [4]executor.Completion = undefined,
    pending_len: usize = 0,
    received: usize = 0,
    ended: bool = false,
    lane_states: [4]executor.LaneState = @splat(executor.LaneState.init(testing.allocator)),
    fail_output_task: ?usize = null,
    fail_scratch_task: ?usize = null,
    fail_submit_task: ?usize = null,
    // Successful shard allocations survive submit until the phase is drained.
    output_failure: testing.FailingAllocator = testing.FailingAllocator.init(testing.allocator, .{ .fail_index = 0 }),
    scratch_failure: testing.FailingAllocator = testing.FailingAllocator.init(testing.allocator, .{ .fail_index = 0 }),
    accepted: usize = 0,

    fn deinit(self: *ReverseExecutor) void {
        for (&self.lane_states) |*lane| lane.deinit();
    }

    fn begin(context: *anyopaque) void {
        const self: *ReverseExecutor = @ptrCast(@alignCast(context));
        std.debug.assert(self.pending_len == 0);
        self.ended = false;
        self.received = 0;
        self.accepted = 0;
    }

    fn submit(context: *anyopaque, task: executor.Task) std.mem.Allocator.Error!void {
        const self: *ReverseExecutor = @ptrCast(@alignCast(context));
        if (self.fail_submit_task == task.id) return error.OutOfMemory;
        var scratch = std.heap.ArenaAllocator.init(testing.allocator);
        defer scratch.deinit();
        const worker_id = task.id % self.lanes;
        const result = task.run(task.context, .{
            .id = worker_id,
            .allocator = if (self.fail_output_task == task.id) self.output_failure.allocator() else testing.allocator,
            .scratch = if (self.fail_scratch_task == task.id) self.scratch_failure.allocator() else scratch.allocator(),
            .lane_state = &self.lane_states[worker_id],
        });
        self.pending[self.pending_len] = .{ .id = task.id, .worker_id = worker_id, .value = result };
        self.pending_len += 1;
        self.accepted += 1;
    }

    fn receive(context: *anyopaque) executor.Completion {
        const self: *ReverseExecutor = @ptrCast(@alignCast(context));
        self.pending_len -= 1;
        self.received += 1;
        return self.pending[self.pending_len];
    }

    fn end(context: *anyopaque) void {
        const self: *ReverseExecutor = @ptrCast(@alignCast(context));
        std.debug.assert(self.pending_len == 0);
        self.ended = true;
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
};

test "LIR proc pass scalarization keeps erased-body eligibility" {
    var direct = try Fixture.init();
    defer direct.deinit();
    try direct.useErasedAbi();
    var candidate = try Fixture.init();
    defer candidate.deinit();
    try candidate.useErasedAbi();
    try @import("scalarize_joins.zig").run(&direct.store, &direct.layouts);
    var runner: ReverseExecutor = .{ .lanes = 4 };
    defer runner.deinit();
    var metrics: passes.ParallelMetrics = .{};
    try passes.run(testing.allocator, &candidate.store, &candidate.layouts, .scalarize, runner.interface(), &metrics);
    var expected = std.Io.Writer.Allocating.init(testing.allocator);
    defer expected.deinit();
    var actual = std.Io.Writer.Allocating.init(testing.allocator);
    defer actual.deinit();
    try direct.dump(&expected.writer);
    try candidate.dump(&actual.writer);
    try testing.expectEqualStrings(expected.written(), actual.written());
    try testing.expectEqual(@as(u64, 8), metrics.changed_by_phase[@intFromEnum(passes.Phase.scalarize)]);
}

test "LIR proc pass reverse completions preserve serial scalarization and accounting" {
    var serial = try Fixture.init();
    defer serial.deinit();
    var before = std.Io.Writer.Allocating.init(testing.allocator);
    defer before.deinit();
    try serial.dump(&before.writer);
    try passes.run(testing.allocator, &serial.store, &serial.layouts, .scalarize, null, null);
    var reference = std.Io.Writer.Allocating.init(testing.allocator);
    defer reference.deinit();
    try serial.dump(&reference.writer);
    try testing.expect(!std.mem.eql(u8, before.written(), reference.written()));
    var expected: ?passes.ParallelMetrics = null;
    for ([_]usize{ 2, 4 }) |lanes| {
        var candidate = try Fixture.init();
        defer candidate.deinit();
        var mock: ReverseExecutor = .{ .lanes = lanes };
        defer mock.deinit();
        var metrics: passes.ParallelMetrics = .{};
        try passes.run(testing.allocator, &candidate.store, &candidate.layouts, .scalarize, mock.interface(), &metrics);
        try testing.expect(mock.ended);
        try testing.expectEqual(@as(usize, 8), mock.received);
        try testing.expectEqual(@as(u64, 8), metrics.tasks_submitted);
        try testing.expectEqual(metrics.tasks_submitted, metrics.tasks_committed);
        try testing.expectEqual(@as(u64, 8), metrics.changed_by_phase[@intFromEnum(passes.Phase.scalarize)]);
        if (expected) |counts| try testing.expectEqualDeep(counts, metrics);
        expected = metrics;
        var output = std.Io.Writer.Allocating.init(testing.allocator);
        defer output.deinit();
        try candidate.dump(&output.writer);
        try testing.expectEqualStrings(reference.written(), output.written());
    }
}

test "LIR proc pass no-op workers do not append duplicate source bodies" {
    var fixture = try Fixture.init();
    defer fixture.deinit();
    var before = std.Io.Writer.Allocating.init(testing.allocator);
    defer before.deinit();
    try fixture.dump(&before.writer);
    const prefix = fixture.store.captureBodyPrefix();
    var mock: ReverseExecutor = .{ .lanes = 4 };
    defer mock.deinit();
    var metrics: passes.ParallelMetrics = .{};
    // Constructor and field projection have no arithmetic facts to prove.
    try passes.run(testing.allocator, &fixture.store, &fixture.layouts, .range, mock.interface(), &metrics);
    try testing.expectEqual(@as(u64, 8), metrics.tasks_submitted);
    try testing.expectEqual(metrics.tasks_submitted, metrics.tasks_committed);
    try testing.expectEqual(@as(u64, 0), metrics.changed_by_phase[@intFromEnum(passes.Phase.range)]);
    try testing.expectEqual(@as(u64, 0), metrics.appended_statements);
    try testing.expectEqualDeep(prefix, fixture.store.captureBodyPrefix());
    var after = std.Io.Writer.Allocating.init(testing.allocator);
    defer after.deinit();
    try fixture.dump(&after.writer);
    try testing.expectEqualStrings(before.written(), after.written());
}

test "LIR proc pass worker and submission OOM drain without partial source mutation" {
    for (0..3) |failure| {
        var fixture = try Fixture.init();
        defer fixture.deinit();
        var before = std.Io.Writer.Allocating.init(testing.allocator);
        defer before.deinit();
        try fixture.dump(&before.writer);
        const prefix = fixture.store.captureBodyPrefix();
        var mock: ReverseExecutor = .{
            .lanes = 4,
            .fail_output_task = if (failure == 0) 7 else null,
            .fail_scratch_task = if (failure == 1) 7 else null,
            .fail_submit_task = if (failure == 2) 7 else null,
        };
        defer mock.deinit();
        var metrics: passes.ParallelMetrics = .{};
        try testing.expectError(error.OutOfMemory, passes.run(
            testing.allocator,
            &fixture.store,
            &fixture.layouts,
            .scalarize,
            mock.interface(),
            &metrics,
        ));
        try testing.expect(mock.ended);
        try testing.expectEqual(@as(usize, if (failure == 2) 7 else 8), mock.received);
        try testing.expectEqual(@as(u64, 0), metrics.tasks_committed);
        try testing.expectEqualDeep(prefix, fixture.store.captureBodyPrefix());
        var after = std.Io.Writer.Allocating.init(testing.allocator);
        defer after.deinit();
        try fixture.dump(&after.writer);
        try testing.expectEqualStrings(before.written(), after.written());
        // The same source remains usable after failed shards are destroyed.
        try passes.run(testing.allocator, &fixture.store, &fixture.layouts, .scalarize, null, null);
    }
}

test "LIR proc pass sweeps output and scratch OOM through appended join rewrites" {
    for (0..2) |kind| {
        var fail_index: usize = 0;
        while (true) : (fail_index += 1) {
            var fixture = try Fixture.initJoins();
            defer fixture.deinit();
            var before = std.Io.Writer.Allocating.init(testing.allocator);
            defer before.deinit();
            try fixture.dump(&before.writer);
            const prefix = fixture.store.captureBodyPrefix();
            var runner: ReverseExecutor = .{
                .lanes = 4,
                .fail_output_task = if (kind == 0) 7 else null,
                .fail_scratch_task = if (kind == 1) 7 else null,
                .output_failure = testing.FailingAllocator.init(testing.allocator, .{ .fail_index = fail_index }),
                .scratch_failure = testing.FailingAllocator.init(testing.allocator, .{ .fail_index = fail_index }),
            };
            defer runner.deinit();
            var metrics: passes.ParallelMetrics = .{};
            const result = passes.run(testing.allocator, &fixture.store, &fixture.layouts, .scalarize, runner.interface(), &metrics);
            try testing.expect(runner.ended);
            try testing.expectEqual(@as(usize, 8), runner.accepted);
            try testing.expectEqual(runner.accepted, runner.received);
            try testing.expectEqual(@as(usize, 0), runner.pending_len);
            const failed = if (kind == 0) runner.output_failure.has_induced_failure else runner.scratch_failure.has_induced_failure;
            if (failed) {
                try testing.expectError(error.OutOfMemory, result);
                try testing.expectEqual(@as(u64, 0), metrics.tasks_committed);
                try testing.expectEqualDeep(prefix, fixture.store.captureBodyPrefix());
                var after = std.Io.Writer.Allocating.init(testing.allocator);
                defer after.deinit();
                try fixture.dump(&after.writer);
                try testing.expectEqualStrings(before.written(), after.written());
            } else {
                try result;
                try testing.expect(fail_index > 1);
                try testing.expectEqual(@as(u64, 8), metrics.tasks_committed);
                try testing.expectEqual(@as(u64, 8), metrics.changed_by_phase[@intFromEnum(passes.Phase.scalarize)]);
                try testing.expect(metrics.appended_statements > 0);
                const after = fixture.store.captureBodyPrefix();
                try testing.expect(after.locals > prefix.locals);
                try testing.expect(after.cf_stmts > prefix.cf_stmts);
                break;
            }
        }
    }
}
