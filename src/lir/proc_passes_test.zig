//! Deterministic procedure commits and worker-failure isolation at the LIR pass boundary.

const std = @import("std");
const core = @import("lir_core");
const layout = @import("layout");
const executor = @import("base").post_check_task_executor;
const passes = @import("proc_passes.zig");
const debug_print = @import("debug_print.zig");
const testing = std.testing;
const body_clone = @import("body_clone.zig");
const collections = @import("collections");

const FusionJoins = struct {
    candidate: core.LIR.JoinPointId,
    external: core.LIR.JoinPointId,
    nested: core.LIR.JoinPointId,
    forwarding: core.LIR.JoinPointId,
};

/// Every procedure deliberately reuses the same reserved source join IDs. The consumer
/// contains a nested binder and a jump to an enclosing, non-cloned binder.
fn fusionFixture(phase: passes.Phase) std.mem.Allocator.Error!Fixture {
    var fixture: Fixture = .{
        .store = core.LirStore.init(testing.allocator),
        .layouts = try layout.Store.init(testing.allocator, .u64),
    };
    errdefer fixture.deinit();
    const store = &fixture.store;
    var join_ids = body_clone.JoinParamIndex.init(testing.allocator);
    defer join_ids.deinit();
    const joins: FusionJoins = .{
        .candidate = join_ids.freshJoinPoint(),
        .external = join_ids.freshJoinPoint(),
        .nested = join_ids.freshJoinPoint(),
        .forwarding = join_ids.freshJoinPoint(),
    };
    fixture.fusion_joins = joins;
    for (0..8) |index| {
        const selector = try store.addLocal(.{ .layout_idx = .u64 });
        const result = try store.addLocal(.{ .layout_idx = .u64 });
        const outer = try store.addLocal(.{ .layout_idx = if (phase == .tag_fusion) .bool else .u64 });
        const inner = try store.addLocal(.{ .layout_idx = .u64 });
        const disc = try store.addLocal(.{ .layout_idx = .u16 });
        const carried = try store.addLocal(.{ .layout_idx = .u64 });
        const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
        const external_jump = try store.addCFStmt(.{ .jump = .{ .target = joins.external } });
        const internal_jump = try store.addCFStmt(.{ .jump = .{ .target = joins.nested } });
        const initialize = try store.addCFStmt(.{ .assign_literal = .{
            .target = result,
            .value = .{ .i64_literal = .{ .value = 42, .layout_idx = .u64 } },
            .next = internal_jump,
        } });
        const arm = try store.addCFStmt(.{ .join = .{
            .id = joins.nested,
            .params = try store.addLocalSpan(&.{result}),
            .body = external_jump,
            .remainder = initialize,
        } });
        const jump_outer = try store.addCFStmt(.{ .jump = .{ .target = joins.candidate } });
        var consumer = arm;
        var producer: core.LIR.CFStmtId = undefined;
        if (phase == .tag_fusion) {
            const choose = try store.addCFStmt(.{ .switch_stmt = .{
                .cond = disc,
                .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = arm }}),
                .default_branch = arm,
            } });
            consumer = try store.addCFStmt(.{ .assign_ref = .{
                .target = disc,
                .op = .{ .discriminant = .{ .source = outer } },
                .next = choose,
            } });
            const first = try store.addCFStmt(.{ .assign_tag = .{
                .target = outer,
                .variant_index = 0,
                .discriminant = 0,
                .payload = null,
                .next = jump_outer,
            } });
            const second = try store.addCFStmt(.{ .assign_tag = .{
                .target = outer,
                .variant_index = 1,
                .discriminant = 1,
                .payload = null,
                .next = try store.addCFStmt(.{ .jump = .{ .target = joins.candidate } }),
            } });
            producer = try store.addCFStmt(.{ .switch_stmt = .{
                .cond = selector,
                .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = first }}),
                .default_branch = second,
            } });
        } else {
            const forward = try store.addCFStmt(.{ .set_local = .{
                .target = outer,
                .value = inner,
                .mode = .initialize_join_param,
                .next = jump_outer,
            } });
            const jump_inner = try store.addCFStmt(.{ .jump = .{ .target = joins.forwarding } });
            const set_inner = try store.addCFStmt(.{ .set_local = .{
                .target = inner,
                .value = selector,
                .mode = .initialize_join_param,
                .next = jump_inner,
            } });
            producer = try store.addCFStmt(.{ .join = .{
                .id = joins.forwarding,
                .params = try store.addLocalSpan(&.{inner}),
                .body = forward,
                .remainder = set_inner,
            } });
        }
        const candidate = try store.addCFStmt(.{ .join = .{
            .id = joins.candidate,
            .params = try store.addLocalSpan(&.{outer}),
            .body = consumer,
            .remainder = producer,
        } });
        const initialize_carried = try store.addCFStmt(.{ .assign_literal = .{
            .target = carried,
            .value = .{ .i64_literal = .{ .value = 17, .layout_idx = .u64 } },
            .next = candidate,
        } });
        const root = try store.addCFStmt(.{
            .join = .{
                .id = joins.external,
                // Keep this external continuation outside the one-parameter
                // forwarding rule, so its original identity survives the fixed point.
                .params = try store.addLocalSpan(&.{ result, carried }),
                .body = ret,
                .remainder = initialize_carried,
            },
        });
        _ = try store.addProcSpec(.{
            .identity = core.LIR.ProcIdentity.forTest(@intCast(index)),
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&.{selector}),
            .frame_locals = try store.addLocalSpan(&.{ selector, result, outer, inner, disc, carried }),
            .body = root,
            .iterator_fusion_scope = true,
            .ret_layout = .u64,
        });
    }
    return fixture;
}

const TestError = std.mem.Allocator.Error || std.Io.Writer.Error || error{ TestExpectedEqual, TestUnexpectedResult };

fn expectFusionJoins(fixture: *Fixture, phase: passes.Phase) TestError!void {
    const joins = fixture.fusion_joins.?;
    var fresh = collections.DenseMap(core.LIR.JoinPointId, void).init(testing.allocator);
    defer fresh.deinit();
    for (0..fixture.store.procSpecCount()) |index| {
        const proc = fixture.store.getProcSpec(@enumFromInt(index));
        var walk = try body_clone.ReachableStmts.init(&fixture.store, proc.body.?);
        defer walk.deinit();
        var nested: usize = 0;
        var external_jumps: usize = 0;
        while (try walk.next()) |id| {
            const stmt = fixture.store.getCFStmt(id);
            if (stmt == .jump and stmt.jump.target == joins.external) external_jumps += 1;
            const last_source_join = if (phase == .tag_fusion) joins.nested else joins.forwarding;
            if (stmt != .join or @intFromEnum(stmt.join.id) <= @intFromEnum(last_source_join)) continue;
            try testing.expect(!fresh.contains(stmt.join.id));
            try fresh.put(stmt.join.id, {});
            const params = fixture.store.getLocalSpan(stmt.join.params);
            if (params.len == 0) continue;
            nested += 1;
            const initialized = fixture.store.getCFStmt(stmt.join.remainder).assign_literal;
            try testing.expectEqual(core.LirStore.GuardedList.at(params, 0), initialized.target);
            try testing.expectEqual(stmt.join.id, fixture.store.getCFStmt(initialized.next).jump.target);
            const bridge = fixture.store.getCFStmt(stmt.join.body).set_local;
            try testing.expectEqual(core.LirStore.GuardedList.at(params, 0), bridge.value);
            try testing.expectEqual(joins.external, fixture.store.getCFStmt(bridge.next).jump.target);
        }
        try testing.expectEqual(@as(usize, if (phase == .tag_fusion) 2 else 1), nested);
        try testing.expect(external_jumps > 0);
    }
}

const Fixture = struct {
    store: core.LirStore,
    layouts: layout.Store,
    fusion_joins: ?FusionJoins = null,

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
    frozen_source: ?*const core.LirStore = null,
    frozen_prefix: core.LirStore.BodyPrefix = undefined,
    source_unchanged_until_end: bool = true,

    fn deinit(self: *ReverseExecutor) void {
        for (&self.lane_states) |*lane| lane.deinit();
    }

    fn begin(context: *anyopaque) void {
        const self: *ReverseExecutor = @ptrCast(@alignCast(context));
        std.debug.assert(self.pending_len == 0);
        self.ended = false;
        self.received = 0;
        self.accepted = 0;
        if (self.frozen_source) |source| self.frozen_prefix = source.captureBodyPrefix();
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
        if (self.frozen_source) |source| {
            self.source_unchanged_until_end = self.source_unchanged_until_end and
                std.meta.eql(self.frozen_prefix, source.captureBodyPrefix());
        }
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

test "LIR proc pass fusion relocates only generated joins in procedure order" {
    inline for (.{ passes.Phase.forwarding_join, passes.Phase.tag_fusion }) |phase| {
        var serial = try fusionFixture(phase);
        defer serial.deinit();
        try passes.run(testing.allocator, &serial.store, &serial.layouts, phase, null, null);
        try expectFusionJoins(&serial, phase);
        var reference = std.Io.Writer.Allocating.init(testing.allocator);
        defer reference.deinit();
        try serial.dump(&reference.writer);
        var expected: ?passes.ParallelMetrics = null;
        for ([_]usize{ 2, 4 }) |lanes| {
            var fixture = try fusionFixture(phase);
            defer fixture.deinit();
            var runner: ReverseExecutor = .{ .lanes = lanes, .frozen_source = &fixture.store };
            defer runner.deinit();
            var metrics: passes.ParallelMetrics = .{};
            try passes.run(testing.allocator, &fixture.store, &fixture.layouts, phase, runner.interface(), &metrics);
            try expectFusionJoins(&fixture, phase);
            try testing.expect(runner.ended);
            try testing.expect(runner.source_unchanged_until_end);
            try testing.expectEqual(@as(usize, 8), runner.received);
            try testing.expectEqual(@as(u64, 8), metrics.tasks_submitted);
            try testing.expectEqual(@as(u64, 8), metrics.tasks_committed);
            for (metrics.changed_by_phase, metrics.committed_by_phase, 0..) |changed, committed, index| {
                try testing.expectEqual(@as(u64, if (index == @intFromEnum(phase)) 8 else 0), changed);
                try testing.expectEqual(changed, committed);
            }
            if (expected) |counts| try testing.expectEqualDeep(counts, metrics);
            expected = metrics;
            var output = std.Io.Writer.Allocating.init(testing.allocator);
            defer output.deinit();
            try fixture.dump(&output.writer);
            try testing.expectEqualStrings(reference.written(), output.written());
            // DebugPrint does not expose all procedure metadata.
            for (0..fixture.store.procSpecCount()) |index| {
                const id: core.LIR.LirProcSpecId = @enumFromInt(index);
                try testing.expectEqualDeep(serial.store.getProcSpec(id), fixture.store.getProcSpec(id));
                const serial_frame = serial.store.getLocalSpan(serial.store.getProcSpec(id).frame_locals);
                const parallel_frame = fixture.store.getLocalSpan(fixture.store.getProcSpec(id).frame_locals);
                try testing.expectEqual(serial_frame.len, parallel_frame.len);
                for (0..serial_frame.len) |local_index| {
                    const serial_local = core.LirStore.GuardedList.at(serial_frame, local_index);
                    const parallel_local = core.LirStore.GuardedList.at(parallel_frame, local_index);
                    try testing.expectEqual(serial_local, parallel_local);
                    try testing.expectEqualDeep(serial.store.getLocal(serial_local), fixture.store.getLocal(parallel_local));
                }
            }
        }
    }
}

/// Move the entire tag-fusion graph, including its external join, into an
/// iterator continuation. That fresh external identity becomes source input
/// to the following tag phase and must not be relocated a second time.
fn wrapFusionInForwarder(fixture: *Fixture) std.mem.Allocator.Error!void {
    const store = &fixture.store;
    for (0..store.procSpecCount()) |index| {
        const id: core.LIR.LirProcSpecId = @enumFromInt(index);
        const proc = store.getProcSpec(id);
        const outer = try store.addLocal(.{ .layout_idx = .u64 });
        const inner = try store.addLocal(.{ .layout_idx = .u64 });
        const jump_outer = try store.addCFStmt(.{ .jump = .{ .target = @enumFromInt(4) } });
        const forward = try store.addCFStmt(.{ .set_local = .{
            .target = outer,
            .value = inner,
            .mode = .initialize_join_param,
            .next = jump_outer,
        } });
        const jump_inner = try store.addCFStmt(.{ .jump = .{ .target = @enumFromInt(3) } });
        const initialize = try store.addCFStmt(.{ .set_local = .{
            .target = inner,
            .value = core.LirStore.GuardedList.at(store.getLocalSpan(proc.args), 0),
            .mode = .initialize_join_param,
            .next = jump_inner,
        } });
        const inner_join = try store.addCFStmt(.{ .join = .{
            .id = @enumFromInt(3),
            .params = try store.addLocalSpan(&.{inner}),
            .body = forward,
            .remainder = initialize,
        } });
        const body = try store.addCFStmt(.{ .join = .{
            .id = @enumFromInt(4),
            .params = try store.addLocalSpan(&.{outer}),
            .body = proc.body.?,
            .remainder = inner_join,
        } });
        var frame = std.ArrayList(core.LIR.LocalId).empty;
        defer frame.deinit(testing.allocator);
        const old_frame = store.getLocalSpan(proc.frame_locals);
        for (0..old_frame.len) |local| try frame.append(testing.allocator, core.LirStore.GuardedList.at(old_frame, local));
        try frame.appendSlice(testing.allocator, &.{ outer, inner });
        const frame_span = try store.addLocalSpan(frame.items);
        store.getProcSpecPtr(id).body = body;
        store.getProcSpecPtr(id).frame_locals = frame_span;
    }
}

test "LIR proc pass forwarding generated joins become frozen source for tag fusion" {
    var reference = std.Io.Writer.Allocating.init(testing.allocator);
    defer reference.deinit();
    var expected: ?passes.ParallelMetrics = null;
    for ([_]usize{ 1, 2, 4 }) |lanes| {
        var fixture = try fusionFixture(.tag_fusion);
        defer fixture.deinit();
        try wrapFusionInForwarder(&fixture);
        var runner: ReverseExecutor = .{ .lanes = lanes };
        defer runner.deinit();
        var metrics: passes.ParallelMetrics = .{};
        const exec: ?executor.Executor = if (lanes == 1) null else runner.interface();
        try passes.run(testing.allocator, &fixture.store, &fixture.layouts, .forwarding_join, exec, &metrics);
        var external_ids: [8]core.LIR.JoinPointId = undefined;
        for (&external_ids, 0..) |*external, index| {
            const proc = fixture.store.getProcSpec(@enumFromInt(index));
            const inner = fixture.store.getCFStmt(proc.body.?).join;
            try testing.expectEqual(@as(u32, 3), @intFromEnum(inner.id));
            external.* = fixture.store.getCFStmt(inner.body).join.id;
            try testing.expect(@intFromEnum(external.*) > 4);
        }
        try passes.run(testing.allocator, &fixture.store, &fixture.layouts, .tag_fusion, exec, &metrics);
        for (external_ids, 0..) |external, index| {
            const proc = fixture.store.getProcSpec(@enumFromInt(index));
            const inner = fixture.store.getCFStmt(proc.body.?).join;
            try testing.expectEqual(external, fixture.store.getCFStmt(inner.body).join.id);
            var walk = try body_clone.ReachableStmts.init(&fixture.store, proc.body.?);
            defer walk.deinit();
            var external_jumps: usize = 0;
            while (try walk.next()) |id| {
                const stmt = fixture.store.getCFStmt(id);
                if (stmt == .jump and stmt.jump.target == external) external_jumps += 1;
            }
            try testing.expectEqual(@as(usize, 2), external_jumps);
        }
        if (lanes == 1) {
            try fixture.dump(&reference.writer);
        } else {
            try testing.expectEqual(@as(u64, 8), metrics.changed_by_phase[@intFromEnum(passes.Phase.forwarding_join)]);
            try testing.expectEqual(@as(u64, 8), metrics.changed_by_phase[@intFromEnum(passes.Phase.tag_fusion)]);
            if (expected) |counts| try testing.expectEqualDeep(counts, metrics);
            expected = metrics;
            var actual = std.Io.Writer.Allocating.init(testing.allocator);
            defer actual.deinit();
            try fixture.dump(&actual.writer);
            try testing.expectEqualStrings(reference.written(), actual.written());
        }
    }
}

test "LIR proc pass fusion output scratch and submission failures drain before commit" {
    inline for (.{ passes.Phase.forwarding_join, passes.Phase.tag_fusion }) |phase| {
        for (0..3) |kind| {
            var fail_index: usize = 0;
            while (true) : (fail_index += 1) {
                var fixture = try fusionFixture(phase);
                defer fixture.deinit();
                var before = std.Io.Writer.Allocating.init(testing.allocator);
                defer before.deinit();
                try fixture.dump(&before.writer);
                const prefix = fixture.store.captureBodyPrefix();
                var procs: [8]core.LIR.LirProcSpec = undefined;
                for (&procs, 0..) |*proc, index| proc.* = fixture.store.getProcSpec(@enumFromInt(index));
                var runner: ReverseExecutor = .{
                    .lanes = 4,
                    .frozen_source = &fixture.store,
                    .fail_output_task = if (kind == 0) 7 else null,
                    .fail_scratch_task = if (kind == 1) 7 else null,
                    .fail_submit_task = if (kind == 2) 7 else null,
                    .output_failure = testing.FailingAllocator.init(testing.allocator, .{ .fail_index = fail_index }),
                    .scratch_failure = testing.FailingAllocator.init(testing.allocator, .{ .fail_index = fail_index }),
                };
                defer runner.deinit();
                var metrics: passes.ParallelMetrics = .{};
                const result = passes.run(testing.allocator, &fixture.store, &fixture.layouts, phase, runner.interface(), &metrics);
                try testing.expect(runner.ended);
                try testing.expect(runner.source_unchanged_until_end);
                try testing.expectEqual(@as(usize, if (kind == 2) 7 else 8), runner.accepted);
                try testing.expectEqual(runner.accepted, runner.received);
                try testing.expectEqual(@as(usize, 0), runner.pending_len);
                const failed = kind == 2 or (if (kind == 0) runner.output_failure.has_induced_failure else runner.scratch_failure.has_induced_failure);
                if (failed) {
                    try testing.expectError(error.OutOfMemory, result);
                    try testing.expectEqual(@as(u64, 0), metrics.tasks_committed);
                    try testing.expectEqualDeep(prefix, fixture.store.captureBodyPrefix());
                    for (procs, 0..) |proc, index| try testing.expectEqualDeep(proc, fixture.store.getProcSpec(@enumFromInt(index)));
                    var after = std.Io.Writer.Allocating.init(testing.allocator);
                    defer after.deinit();
                    try fixture.dump(&after.writer);
                    try testing.expectEqualStrings(before.written(), after.written());
                    if (kind == 2) break;
                } else {
                    try result;
                    try testing.expect(fail_index > 1);
                    try testing.expectEqual(@as(u64, 8), metrics.changed_by_phase[@intFromEnum(phase)]);
                    try expectFusionJoins(&fixture, phase);
                    break;
                }
            }
        }
    }
}

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
