//! Explicitize linear switch-tail sharing into join/jump form for structured backends.

const std = @import("std");
const builtin = @import("builtin");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;
const CFStmt = LIR.CFStmt;
const CFStmtId = LIR.CFStmtId;
const CFSwitchBranch = LIR.CFSwitchBranch;
const JoinPointId = LIR.JoinPointId;

/// Public struct `Pass`.
pub const Pass = struct {
    store: *LirStore,
    allocator: Allocator,
    rewritten_stmt_ids: std.AutoHashMap(u32, CFStmtId),
    incoming_counts: std.AutoHashMap(u32, u32),
    next_join_id: u32,

    pub fn init(store: *LirStore, allocator: Allocator) Pass {
        var pass: Pass = .{
            .store = store,
            .allocator = allocator,
            .rewritten_stmt_ids = std.AutoHashMap(u32, CFStmtId).init(allocator),
            .incoming_counts = std.AutoHashMap(u32, u32).init(allocator),
            .next_join_id = nextJoinId(store),
        };
        pass.buildIncomingCounts() catch unreachable;
        return pass;
    }

    pub fn deinit(self: *Pass) void {
        self.rewritten_stmt_ids.deinit();
        self.incoming_counts.deinit();
    }

    fn nextJoin(self: *Pass) JoinPointId {
        const id: JoinPointId = @enumFromInt(self.next_join_id);
        self.next_join_id += 1;
        return id;
    }

    fn assertValidStmtId(self: *const Pass, stmt_id: CFStmtId, comptime context: []const u8) void {
        if (builtin.mode != .Debug) return;

        const stmt_index = @intFromEnum(stmt_id);
        if (stmt_index >= self.store.cf_stmts.items.len) {
            std.debug.panic(
                "SharedSwitchTail invariant violated: invalid stmt {d} in {s} (store len {d})",
                .{ stmt_index, context, self.store.cf_stmts.items.len },
            );
        }
    }

    fn incrementIncomingCount(self: *Pass, target: CFStmtId) Allocator.Error!void {
        const gop = try self.incoming_counts.getOrPut(@intFromEnum(target));
        if (gop.found_existing) {
            gop.value_ptr.* += 1;
        } else {
            gop.value_ptr.* = 1;
        }
    }

    fn buildIncomingCounts(self: *Pass) Allocator.Error!void {
        const original_stmt_count = self.store.cf_stmts.items.len;
        for (0..original_stmt_count) |stmt_index| {
            const stmt_id: CFStmtId = @enumFromInt(@as(u32, @intCast(stmt_index)));
            switch (self.store.getCFStmt(stmt_id)) {
                .assign_symbol => |assign| try self.incrementIncomingCount(assign.next),
                .assign_ref => |assign| try self.incrementIncomingCount(assign.next),
                .assign_literal => |assign| try self.incrementIncomingCount(assign.next),
                .assign_call => |assign| try self.incrementIncomingCount(assign.next),
                .assign_call_indirect => |assign| try self.incrementIncomingCount(assign.next),
                .assign_low_level => |assign| try self.incrementIncomingCount(assign.next),
                .assign_list => |assign| try self.incrementIncomingCount(assign.next),
                .assign_struct => |assign| try self.incrementIncomingCount(assign.next),
                .assign_tag => |assign| try self.incrementIncomingCount(assign.next),
                .set_local => |assign| try self.incrementIncomingCount(assign.next),
                .debug => |debug_stmt| try self.incrementIncomingCount(debug_stmt.next),
                .expect => |expect_stmt| try self.incrementIncomingCount(expect_stmt.next),
                .incref => |inc| try self.incrementIncomingCount(inc.next),
                .decref => |dec| try self.incrementIncomingCount(dec.next),
                .free => |free_stmt| try self.incrementIncomingCount(free_stmt.next),
                .switch_stmt => |switch_stmt| {
                    try self.incrementIncomingCount(switch_stmt.default_branch);
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try self.incrementIncomingCount(branch.body);
                    }
                },
                .borrow_scope => |scope| {
                    try self.incrementIncomingCount(scope.body);
                    try self.incrementIncomingCount(scope.remainder);
                },
                .for_list => |for_stmt| {
                    try self.incrementIncomingCount(for_stmt.body);
                    try self.incrementIncomingCount(for_stmt.next);
                },
                .join => |join| {
                    try self.incrementIncomingCount(join.body);
                    try self.incrementIncomingCount(join.remainder);
                },
                .jump, .loop_continue, .scope_exit, .ret, .runtime_error, .crash => {},
            }
        }
    }

    fn hasMultipleIncoming(self: *const Pass, stmt_id: CFStmtId) bool {
        return (self.incoming_counts.get(@intFromEnum(stmt_id)) orelse 0) > 1;
    }

    fn cloneLinearWithNext(self: *Pass, stmt: CFStmt, next: CFStmtId) Allocator.Error!CFStmtId {
        return self.store.addCFStmt(linearWithNext(stmt, next));
    }

    fn linearWithNext(stmt: CFStmt, next: CFStmtId) CFStmt {
        return switch (stmt) {
            .assign_symbol => |assign| .{ .assign_symbol = .{
                .target = assign.target,
                .symbol = assign.symbol,
                .next = next,
            } },
            .assign_ref => |assign| .{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .op = assign.op,
                .next = next,
            } },
            .assign_literal => |assign| .{ .assign_literal = .{
                .target = assign.target,
                .result = assign.result,
                .value = assign.value,
                .next = next,
            } },
            .assign_call => |assign| .{ .assign_call = .{
                .target = assign.target,
                .result = assign.result,
                .proc = assign.proc,
                .args = assign.args,
                .next = next,
            } },
            .assign_call_indirect => |assign| .{ .assign_call_indirect = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .closure = assign.closure,
                .args = assign.args,
                .capture_layout = assign.capture_layout,
                .next = next,
            } },
            .assign_low_level => |assign| .{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .op = assign.op,
                .args = assign.args,
                .next = next,
            } },
            .assign_list => |assign| .{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .elems = assign.elems,
                .next = next,
            } },
            .assign_struct => |assign| .{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .fields = assign.fields,
                .next = next,
            } },
            .assign_tag => |assign| .{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .discriminant = assign.discriminant,
                .payload = assign.payload,
                .next = next,
            } },
            .set_local => |assign| .{ .set_local = .{
                .target = assign.target,
                .value = assign.value,
                .next = next,
            } },
            .debug => |debug_stmt| .{ .debug = .{
                .message = debug_stmt.message,
                .next = next,
            } },
            .expect => |expect_stmt| .{ .expect = .{
                .condition = expect_stmt.condition,
                .next = next,
            } },
            .incref => |inc| .{ .incref = .{
                .value = inc.value,
                .count = inc.count,
                .next = next,
            } },
            .decref => |dec| .{ .decref = .{
                .value = dec.value,
                .next = next,
            } },
            .free => |free_stmt| .{ .free = .{
                .value = free_stmt.value,
                .next = next,
            } },
            else => unreachable,
        };
    }

    fn appendSuccessors(
        self: *Pass,
        stmt_id: CFStmtId,
        out: *std.ArrayListUnmanaged(CFStmtId),
    ) Allocator.Error!void {
        self.assertValidStmtId(stmt_id, "appendSuccessors input");
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| {
                self.assertValidStmtId(assign.next, "appendSuccessors assign_symbol.next");
                try out.append(self.allocator, assign.next);
            },
            .assign_ref => |assign| {
                self.assertValidStmtId(assign.next, "appendSuccessors assign_ref.next");
                try out.append(self.allocator, assign.next);
            },
            .assign_literal => |assign| {
                self.assertValidStmtId(assign.next, "appendSuccessors assign_literal.next");
                try out.append(self.allocator, assign.next);
            },
            .assign_call => |assign| {
                self.assertValidStmtId(assign.next, "appendSuccessors assign_call.next");
                try out.append(self.allocator, assign.next);
            },
            .assign_call_indirect => |assign| {
                self.assertValidStmtId(assign.next, "appendSuccessors assign_call_indirect.next");
                try out.append(self.allocator, assign.next);
            },
            .assign_low_level => |assign| {
                self.assertValidStmtId(assign.next, "appendSuccessors assign_low_level.next");
                try out.append(self.allocator, assign.next);
            },
            .assign_list => |assign| {
                self.assertValidStmtId(assign.next, "appendSuccessors assign_list.next");
                try out.append(self.allocator, assign.next);
            },
            .assign_struct => |assign| {
                self.assertValidStmtId(assign.next, "appendSuccessors assign_struct.next");
                try out.append(self.allocator, assign.next);
            },
            .assign_tag => |assign| {
                self.assertValidStmtId(assign.next, "appendSuccessors assign_tag.next");
                try out.append(self.allocator, assign.next);
            },
            .set_local => |assign| {
                self.assertValidStmtId(assign.next, "appendSuccessors set_local.next");
                try out.append(self.allocator, assign.next);
            },
            .debug => |debug_stmt| {
                self.assertValidStmtId(debug_stmt.next, "appendSuccessors debug.next");
                try out.append(self.allocator, debug_stmt.next);
            },
            .expect => |expect_stmt| {
                self.assertValidStmtId(expect_stmt.next, "appendSuccessors expect.next");
                try out.append(self.allocator, expect_stmt.next);
            },
            .incref => |inc| {
                self.assertValidStmtId(inc.next, "appendSuccessors incref.next");
                try out.append(self.allocator, inc.next);
            },
            .decref => |dec| {
                self.assertValidStmtId(dec.next, "appendSuccessors decref.next");
                try out.append(self.allocator, dec.next);
            },
            .free => |free_stmt| {
                self.assertValidStmtId(free_stmt.next, "appendSuccessors free.next");
                try out.append(self.allocator, free_stmt.next);
            },
            .switch_stmt => |sw| {
                for (self.store.getCFSwitchBranches(sw.branches)) |branch| {
                    self.assertValidStmtId(branch.body, "appendSuccessors switch.branch.body");
                    try out.append(self.allocator, branch.body);
                }
                self.assertValidStmtId(sw.default_branch, "appendSuccessors switch.default_branch");
                try out.append(self.allocator, sw.default_branch);
            },
            .borrow_scope => |scope| {
                self.assertValidStmtId(scope.body, "appendSuccessors borrow_scope.body");
                self.assertValidStmtId(scope.remainder, "appendSuccessors borrow_scope.remainder");
                try out.append(self.allocator, scope.body);
                try out.append(self.allocator, scope.remainder);
            },
            .for_list => |for_stmt| {
                self.assertValidStmtId(for_stmt.body, "appendSuccessors for_list.body");
                self.assertValidStmtId(for_stmt.next, "appendSuccessors for_list.next");
                try out.append(self.allocator, for_stmt.body);
                try out.append(self.allocator, for_stmt.next);
            },
            .join => |join| {
                self.assertValidStmtId(join.body, "appendSuccessors join.body");
                self.assertValidStmtId(join.remainder, "appendSuccessors join.remainder");
                try out.append(self.allocator, join.body);
                try out.append(self.allocator, join.remainder);
            },
            .jump, .loop_continue, .scope_exit, .ret, .runtime_error, .crash => {},
        }
    }

    fn collectReachableDistances(
        self: *Pass,
        start: CFStmtId,
    ) Allocator.Error!std.AutoHashMap(u32, u32) {
        const QueueItem = struct {
            stmt_id: CFStmtId,
            distance: u32,
        };

        var distances = std.AutoHashMap(u32, u32).init(self.allocator);
        errdefer distances.deinit();

        var queue = std.ArrayListUnmanaged(QueueItem).empty;
        defer queue.deinit(self.allocator);
        try queue.append(self.allocator, .{ .stmt_id = start, .distance = 0 });

        var index: usize = 0;
        while (index < queue.items.len) : (index += 1) {
            const item = queue.items[index];
            const stmt_key = @intFromEnum(item.stmt_id);
            const gop = try distances.getOrPut(stmt_key);
            if (gop.found_existing) continue;
            gop.value_ptr.* = item.distance;

            var successors = std.ArrayListUnmanaged(CFStmtId).empty;
            defer successors.deinit(self.allocator);
            try self.appendSuccessors(item.stmt_id, &successors);
            for (successors.items) |succ| {
                try queue.append(self.allocator, .{
                    .stmt_id = succ,
                    .distance = item.distance + 1,
                });
            }
        }

        return distances;
    }

    fn rewritePrefixUntil(
        self: *Pass,
        stmt_id: CFStmtId,
        stop_id: CFStmtId,
        join_id: JoinPointId,
        clone_suffix: bool,
        clone_map: ?*std.AutoHashMap(u32, CFStmtId),
    ) Allocator.Error!CFStmtId {
        var prefix_map = std.AutoHashMap(u32, CFStmtId).init(self.allocator);
        defer prefix_map.deinit();
        const raw_prefix = try self.rewritePrefixUntilInner(stmt_id, stop_id, join_id, clone_suffix, clone_map, &prefix_map);
        return self.rewriteStmtWithCloneModeInner(raw_prefix, true, null);
    }

    fn rewritePrefixUntilInner(
        self: *Pass,
        stmt_id: CFStmtId,
        stop_id: CFStmtId,
        join_id: JoinPointId,
        clone_suffix: bool,
        clone_map: ?*std.AutoHashMap(u32, CFStmtId),
        prefix_map: *std.AutoHashMap(u32, CFStmtId),
    ) Allocator.Error!CFStmtId {
        self.assertValidStmtId(stmt_id, "rewritePrefixUntilInner input");
        if (stmt_id == stop_id) {
            return self.store.addCFStmt(.{ .jump = .{
                .target = join_id,
                .args = LIR.LocalSpan.empty(),
            } });
        }

        const stmt_key = @intFromEnum(stmt_id);
        if (prefix_map.get(stmt_key)) |rewritten| return rewritten;

        const stmt = self.store.getCFStmt(stmt_id);
        switch (stmt) {
            .jump, .loop_continue, .scope_exit, .ret, .runtime_error, .crash => {
                const cloned = try self.store.addCFStmt(stmt);
                try prefix_map.put(stmt_key, cloned);
                return cloned;
            },
            else => {},
        }

        const placeholder = try self.store.addCFStmt(.runtime_error);
        try prefix_map.put(stmt_key, placeholder);

        const rewritten_stmt = switch (stmt) {
            .assign_symbol => |assign| linearWithNext(
                stmt,
                try self.rewritePrefixChild(assign.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner assign_symbol.next"),
            ),
            .assign_ref => |assign| linearWithNext(
                stmt,
                try self.rewritePrefixChild(assign.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner assign_ref.next"),
            ),
            .assign_literal => |assign| linearWithNext(
                stmt,
                try self.rewritePrefixChild(assign.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner assign_literal.next"),
            ),
            .assign_call => |assign| linearWithNext(
                stmt,
                try self.rewritePrefixChild(assign.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner assign_call.next"),
            ),
            .assign_call_indirect => |assign| linearWithNext(
                stmt,
                try self.rewritePrefixChild(assign.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner assign_call_indirect.next"),
            ),
            .assign_low_level => |assign| linearWithNext(
                stmt,
                try self.rewritePrefixChild(assign.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner assign_low_level.next"),
            ),
            .assign_list => |assign| linearWithNext(
                stmt,
                try self.rewritePrefixChild(assign.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner assign_list.next"),
            ),
            .assign_struct => |assign| linearWithNext(
                stmt,
                try self.rewritePrefixChild(assign.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner assign_struct.next"),
            ),
            .assign_tag => |assign| linearWithNext(
                stmt,
                try self.rewritePrefixChild(assign.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner assign_tag.next"),
            ),
            .set_local => |assign| linearWithNext(
                stmt,
                try self.rewritePrefixChild(assign.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner set_local.next"),
            ),
            .debug => |debug_stmt| linearWithNext(
                stmt,
                try self.rewritePrefixChild(debug_stmt.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner debug.next"),
            ),
            .expect => |expect_stmt| linearWithNext(
                stmt,
                try self.rewritePrefixChild(expect_stmt.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner expect.next"),
            ),
            .incref => |inc| linearWithNext(
                stmt,
                try self.rewritePrefixChild(inc.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner incref.next"),
            ),
            .decref => |dec| linearWithNext(
                stmt,
                try self.rewritePrefixChild(dec.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner decref.next"),
            ),
            .free => |free_stmt| linearWithNext(
                stmt,
                try self.rewritePrefixChild(free_stmt.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner free.next"),
            ),
            .switch_stmt => |sw| blk: {
                const original_branches = self.store.getCFSwitchBranches(sw.branches);
                const branch_snapshot = try self.allocator.dupe(CFSwitchBranch, original_branches);
                defer self.allocator.free(branch_snapshot);
                var rewritten_branches = std.ArrayList(CFSwitchBranch).empty;
                defer rewritten_branches.deinit(self.allocator);
                for (branch_snapshot) |branch| {
                    if (builtin.mode == .Debug and @intFromEnum(branch.body) >= self.store.cf_stmts.items.len) {
                        std.debug.panic(
                            "SharedSwitchTail invariant violated: switch stmt {d} has invalid branch body {d} for value {d} in prefix rewrite (store len {d})",
                            .{ @intFromEnum(stmt_id), @intFromEnum(branch.body), branch.value, self.store.cf_stmts.items.len },
                        );
                    }
                    try rewritten_branches.append(self.allocator, .{
                        .value = branch.value,
                        .body = try self.rewritePrefixChild(branch.body, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner switch.branch.body"),
                    });
                }
                const branch_span = try self.store.addCFSwitchBranches(rewritten_branches.items);
                break :blk CFStmt{ .switch_stmt = .{
                    .cond = sw.cond,
                    .branches = branch_span,
                    .default_branch = try self.rewritePrefixChild(sw.default_branch, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner switch.default_branch"),
                } };
            },
            .borrow_scope => |scope| CFStmt{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.rewritePrefixChild(scope.body, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner borrow_scope.body"),
                .remainder = try self.rewritePrefixChild(scope.remainder, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner borrow_scope.remainder"),
            } },
            .for_list => |for_stmt| CFStmt{ .for_list = .{
                .elem = for_stmt.elem,
                .elem_result = for_stmt.elem_result,
                .elem_ownership = for_stmt.elem_ownership,
                .iterable = for_stmt.iterable,
                .iterable_elem_layout = for_stmt.iterable_elem_layout,
                .body = try self.rewritePrefixChild(for_stmt.body, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner for_list.body"),
                .next = try self.rewritePrefixChild(for_stmt.next, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner for_list.next"),
            } },
            .join => |join| CFStmt{ .join = .{
                .id = join.id,
                .params = join.params,
                .body = try self.rewritePrefixChild(join.body, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner join.body"),
                .remainder = try self.rewritePrefixChild(join.remainder, stop_id, join_id, clone_suffix, clone_map, prefix_map, "rewritePrefixUntilInner join.remainder"),
            } },
            .jump, .loop_continue, .scope_exit, .ret, .runtime_error, .crash => unreachable,
        };

        self.store.getCFStmtPtr(placeholder).* = rewritten_stmt;
        return placeholder;
    }

    fn rewritePrefixChild(
        self: *Pass,
        stmt_id: CFStmtId,
        stop_id: CFStmtId,
        join_id: JoinPointId,
        clone_suffix: bool,
        clone_map: ?*std.AutoHashMap(u32, CFStmtId),
        prefix_map: *std.AutoHashMap(u32, CFStmtId),
        comptime context: []const u8,
    ) Allocator.Error!CFStmtId {
        self.assertValidStmtId(stmt_id, context);
        return self.rewritePrefixUntilInner(stmt_id, stop_id, join_id, clone_suffix, clone_map, prefix_map);
    }

    fn rewriteSwitch(
        self: *Pass,
        switch_stmt: @FieldType(CFStmt, "switch_stmt"),
        clone_suffix: bool,
        clone_map: ?*std.AutoHashMap(u32, CFStmtId),
    ) Allocator.Error!CFStmt {
        const original_branches = self.store.getCFSwitchBranches(switch_stmt.branches);
        const branch_snapshot = try self.allocator.dupe(CFSwitchBranch, original_branches);
        defer self.allocator.free(branch_snapshot);

        const entry_count = branch_snapshot.len + 1;
        const entry_starts = try self.allocator.alloc(CFStmtId, entry_count);
        defer self.allocator.free(entry_starts);

        for (branch_snapshot, 0..) |branch, i| {
            entry_starts[i] = branch.body;
        }
        entry_starts[branch_snapshot.len] = switch_stmt.default_branch;

        const reachable_maps = try self.allocator.alloc(std.AutoHashMap(u32, u32), entry_count);
        defer {
            for (reachable_maps) |*reachable| reachable.deinit();
            self.allocator.free(reachable_maps);
        }

        var shared_counts = std.AutoHashMap(u32, u32).init(self.allocator);
        defer shared_counts.deinit();

        for (entry_starts, 0..) |entry_start, i| {
            reachable_maps[i] = try self.collectReachableDistances(entry_start);
            var it = reachable_maps[i].iterator();
            while (it.next()) |entry| {
                const gop = try shared_counts.getOrPut(entry.key_ptr.*);
                if (gop.found_existing) {
                    gop.value_ptr.* += 1;
                } else {
                    gop.value_ptr.* = 1;
                }
            }
        }

        const first_shared_starts = try self.allocator.alloc(?CFStmtId, entry_count);
        defer self.allocator.free(first_shared_starts);

        for (reachable_maps, 0..) |*reachable, i| {
            first_shared_starts[i] = null;
            var best_count: u32 = 0;
            var best_distance: ?u32 = null;
            var best_stmt: ?CFStmtId = null;
            var it = reachable.iterator();
            while (it.next()) |entry| {
                const count = shared_counts.get(entry.key_ptr.*) orelse unreachable;
                if (count < 2) continue;

                const distance = entry.value_ptr.*;
                const stmt_id: CFStmtId = @enumFromInt(entry.key_ptr.*);
                if (count > best_count or
                    (count == best_count and (best_distance == null or distance < best_distance.?)) or
                    (count == best_count and distance == best_distance.? and @intFromEnum(stmt_id) < @intFromEnum(best_stmt.?)))
                {
                    best_count = count;
                    best_distance = distance;
                    best_stmt = stmt_id;
                }
            }
            first_shared_starts[i] = best_stmt;
        }

        const SharedGroup = struct {
            start: CFStmtId,
            join_id: JoinPointId,
            body: CFStmtId,
        };

        var shared_groups = std.ArrayList(SharedGroup).empty;
        defer shared_groups.deinit(self.allocator);

        for (first_shared_starts) |maybe_start| {
            const shared_start = maybe_start orelse continue;

            var already_recorded = false;
            for (shared_groups.items) |group| {
                if (group.start == shared_start) {
                    already_recorded = true;
                    break;
                }
            }
            if (already_recorded) continue;

            var group_size: usize = 0;
            for (first_shared_starts) |candidate| {
                if (candidate != null and candidate.? == shared_start) {
                    group_size += 1;
                }
            }
            if (group_size < 2) continue;

            try shared_groups.append(self.allocator, .{
                .start = shared_start,
                .join_id = self.nextJoin(),
                .body = try self.rewriteStmtWithCloneModeInner(shared_start, clone_suffix, clone_map),
            });
        }

        var rewritten_branches = std.ArrayList(CFSwitchBranch).empty;
        defer rewritten_branches.deinit(self.allocator);

        for (branch_snapshot, 0..) |branch, i| {
            var rewritten_body: ?CFStmtId = null;
            if (first_shared_starts[i]) |shared_start| {
                for (shared_groups.items) |group| {
                    if (group.start == shared_start) {
                        rewritten_body = try self.rewritePrefixUntil(
                            branch.body,
                            shared_start,
                            group.join_id,
                            clone_suffix,
                            clone_map,
                        );
                        break;
                    }
                }
            }

            try rewritten_branches.append(self.allocator, .{
                .value = branch.value,
                .body = rewritten_body orelse try self.rewriteStmtWithCloneModeInner(branch.body, clone_suffix, clone_map),
            });
        }

        const branch_span = try self.store.addCFSwitchBranches(rewritten_branches.items);
        const default_index = branch_snapshot.len;
        var rewritten_default: ?CFStmtId = null;
        if (first_shared_starts[default_index]) |shared_start| {
            for (shared_groups.items) |group| {
                if (group.start == shared_start) {
                    rewritten_default = try self.rewritePrefixUntil(
                        switch_stmt.default_branch,
                        shared_start,
                        group.join_id,
                        clone_suffix,
                        clone_map,
                    );
                    break;
                }
            }
        }

        var rewritten_stmt: CFStmt = .{ .switch_stmt = .{
            .cond = switch_stmt.cond,
            .branches = branch_span,
            .default_branch = rewritten_default orelse try self.rewriteStmtWithCloneModeInner(switch_stmt.default_branch, clone_suffix, clone_map),
        } };

        for (shared_groups.items) |group| {
            const remainder = try self.store.addCFStmt(rewritten_stmt);
            rewritten_stmt = .{ .join = .{
                .id = group.join_id,
                .params = LIR.LocalSpan.empty(),
                .body = group.body,
                .remainder = remainder,
            } };
        }

        return rewritten_stmt;
    }

    fn rewriteStmt(self: *Pass, stmt_id: CFStmtId) Allocator.Error!CFStmtId {
        return self.rewriteStmtWithCloneModeInner(stmt_id, false, null);
    }

    fn rewriteStmtWithCloneModeInner(
        self: *Pass,
        stmt_id: CFStmtId,
        clone_suffix: bool,
        clone_map: ?*std.AutoHashMap(u32, CFStmtId),
    ) Allocator.Error!CFStmtId {
        const must_clone = clone_suffix;
        const stmt_key = @intFromEnum(stmt_id);
        if (must_clone and clone_map == null) {
            var local_clone_map = std.AutoHashMap(u32, CFStmtId).init(self.allocator);
            defer local_clone_map.deinit();
            return self.rewriteStmtWithCloneModeInner(stmt_id, clone_suffix, &local_clone_map);
        }

        const stmt = self.store.getCFStmt(stmt_id);
        if (must_clone) {
            const active_clone_map = clone_map.?;
            if (active_clone_map.get(stmt_key)) |rewritten| return rewritten;
        } else {
            if (self.rewritten_stmt_ids.get(stmt_key)) |rewritten| return rewritten;
        }

        switch (stmt) {
            .jump, .ret, .runtime_error, .crash, .loop_continue, .scope_exit => {
                const rewritten = if (must_clone)
                    try self.store.addCFStmt(stmt)
                else
                    stmt_id;
                if (must_clone) {
                    try clone_map.?.put(stmt_key, rewritten);
                } else {
                    try self.rewritten_stmt_ids.put(stmt_key, rewritten);
                }
                return rewritten;
            },
            else => {},
        }

        const placeholder = try self.store.addCFStmt(.runtime_error);
        if (must_clone) {
            try clone_map.?.put(stmt_key, placeholder);
        } else {
            try self.rewritten_stmt_ids.put(stmt_key, placeholder);
        }

        const rewritten_stmt = switch (stmt) {
            .assign_symbol => |assign| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(assign.next, must_clone, clone_map)),
            .assign_ref => |assign| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(assign.next, must_clone, clone_map)),
            .assign_literal => |assign| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(assign.next, must_clone, clone_map)),
            .assign_call => |assign| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(assign.next, must_clone, clone_map)),
            .assign_call_indirect => |assign| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(assign.next, must_clone, clone_map)),
            .assign_low_level => |assign| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(assign.next, must_clone, clone_map)),
            .assign_list => |assign| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(assign.next, must_clone, clone_map)),
            .assign_struct => |assign| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(assign.next, must_clone, clone_map)),
            .assign_tag => |assign| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(assign.next, must_clone, clone_map)),
            .set_local => |assign| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(assign.next, must_clone, clone_map)),
            .debug => |debug_stmt| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(debug_stmt.next, must_clone, clone_map)),
            .expect => |expect_stmt| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(expect_stmt.next, must_clone, clone_map)),
            .incref => |inc| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(inc.next, must_clone, clone_map)),
            .decref => |dec| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(dec.next, must_clone, clone_map)),
            .free => |free_stmt| linearWithNext(stmt, try self.rewriteStmtWithCloneModeInner(free_stmt.next, must_clone, clone_map)),
            .switch_stmt => |switch_stmt| try self.rewriteSwitch(switch_stmt, must_clone, clone_map),
            .borrow_scope => |scope| CFStmt{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.rewriteStmtWithCloneModeInner(scope.body, must_clone, clone_map),
                .remainder = try self.rewriteStmtWithCloneModeInner(scope.remainder, must_clone, clone_map),
            } },
            .for_list => |for_stmt| CFStmt{ .for_list = .{
                .elem = for_stmt.elem,
                .elem_result = for_stmt.elem_result,
                .elem_ownership = for_stmt.elem_ownership,
                .iterable = for_stmt.iterable,
                .iterable_elem_layout = for_stmt.iterable_elem_layout,
                .body = try self.rewriteStmtWithCloneModeInner(for_stmt.body, must_clone, clone_map),
                .next = try self.rewriteStmtWithCloneModeInner(for_stmt.next, must_clone, clone_map),
            } },
            .join => |join| CFStmt{ .join = .{
                .id = join.id,
                .params = join.params,
                .body = try self.rewriteStmtWithCloneModeInner(join.body, must_clone, clone_map),
                .remainder = try self.rewriteStmtWithCloneModeInner(join.remainder, must_clone, clone_map),
            } },
            .jump, .ret, .runtime_error, .crash, .loop_continue, .scope_exit => unreachable,
        };

        self.store.getCFStmtPtr(placeholder).* = rewritten_stmt;
        return placeholder;
    }
};

fn nextJoinId(store: *const LirStore) u32 {
    var next_id: u32 = 0;
    for (store.cf_stmts.items) |stmt| {
        if (stmt == .join) {
            next_id = @max(next_id, @intFromEnum(stmt.join.id) + 1);
        }
    }
    return next_id;
}

/// Run this compilation stage.
pub fn run(allocator: Allocator, store: *LirStore) Allocator.Error!void {
    var pass = Pass.init(store, allocator);
    defer pass.deinit();

    for (0..store.getProcSpecs().len) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        const proc = store.getProcSpec(proc_id);
        if (proc.body) |body| {
            store.getProcSpecPtr(proc_id).body = try pass.rewriteStmt(body);
        }
    }
}
