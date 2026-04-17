//! Explicitize linear switch-tail sharing into join/jump form for structured backends.

const std = @import("std");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;
const CFStmt = LIR.CFStmt;
const CFStmtId = LIR.CFStmtId;
const CFSwitchBranch = LIR.CFSwitchBranch;
const JoinPointId = LIR.JoinPointId;

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
        return switch (stmt) {
            .assign_symbol => |assign| self.store.addCFStmt(.{ .assign_symbol = .{
                .target = assign.target,
                .symbol = assign.symbol,
                .next = next,
            } }),
            .assign_ref => |assign| self.store.addCFStmt(.{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .op = assign.op,
                .next = next,
            } }),
            .assign_literal => |assign| self.store.addCFStmt(.{ .assign_literal = .{
                .target = assign.target,
                .result = assign.result,
                .value = assign.value,
                .next = next,
            } }),
            .assign_call => |assign| self.store.addCFStmt(.{ .assign_call = .{
                .target = assign.target,
                .result = assign.result,
                .proc = assign.proc,
                .args = assign.args,
                .next = next,
            } }),
            .assign_call_indirect => |assign| self.store.addCFStmt(.{ .assign_call_indirect = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .closure = assign.closure,
                .args = assign.args,
                .capture_layout = assign.capture_layout,
                .next = next,
            } }),
            .assign_low_level => |assign| self.store.addCFStmt(.{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .op = assign.op,
                .args = assign.args,
                .next = next,
            } }),
            .assign_list => |assign| self.store.addCFStmt(.{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .elems = assign.elems,
                .next = next,
            } }),
            .assign_struct => |assign| self.store.addCFStmt(.{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .fields = assign.fields,
                .next = next,
            } }),
            .assign_tag => |assign| self.store.addCFStmt(.{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = assign.ownership,
                .discriminant = assign.discriminant,
                .payload = assign.payload,
                .next = next,
            } }),
            .set_local => |assign| self.store.addCFStmt(.{ .set_local = .{
                .target = assign.target,
                .value = assign.value,
                .next = next,
            } }),
            .debug => |debug_stmt| self.store.addCFStmt(.{ .debug = .{
                .message = debug_stmt.message,
                .next = next,
            } }),
            .expect => |expect_stmt| self.store.addCFStmt(.{ .expect = .{
                .condition = expect_stmt.condition,
                .next = next,
            } }),
            .incref => |inc| self.store.addCFStmt(.{ .incref = .{
                .value = inc.value,
                .count = inc.count,
                .next = next,
            } }),
            .decref => |dec| self.store.addCFStmt(.{ .decref = .{
                .value = dec.value,
                .next = next,
            } }),
            .free => |free_stmt| self.store.addCFStmt(.{ .free = .{
                .value = free_stmt.value,
                .next = next,
            } }),
            else => unreachable,
        };
    }

    fn linearNext(stmt: CFStmt) ?CFStmtId {
        return switch (stmt) {
            .assign_symbol => |assign| assign.next,
            .assign_ref => |assign| assign.next,
            .assign_literal => |assign| assign.next,
            .assign_call => |assign| assign.next,
            .assign_call_indirect => |assign| assign.next,
            .assign_low_level => |assign| assign.next,
            .assign_list => |assign| assign.next,
            .assign_struct => |assign| assign.next,
            .assign_tag => |assign| assign.next,
            .set_local => |assign| assign.next,
            .debug => |debug_stmt| debug_stmt.next,
            .expect => |expect_stmt| expect_stmt.next,
            .incref => |inc| inc.next,
            .decref => |dec| dec.next,
            .free => |free_stmt| free_stmt.next,
            else => null,
        };
    }

    fn collectLinearChain(self: *Pass, start: CFStmtId) Allocator.Error![]CFStmtId {
        var chain = std.ArrayList(CFStmtId).empty;
        errdefer chain.deinit(self.allocator);

        var visited = std.AutoHashMap(u32, void).init(self.allocator);
        defer visited.deinit();

        var current = start;
        while (true) {
            const gop = try visited.getOrPut(@intFromEnum(current));
            if (gop.found_existing) break;
            try chain.append(self.allocator, current);
            const next = linearNext(self.store.getCFStmt(current)) orelse break;
            current = next;
        }

        return chain.toOwnedSlice(self.allocator);
    }

    fn rewritePrefixUntil(self: *Pass, stmt_id: CFStmtId, stop_id: CFStmtId, join_id: JoinPointId, clone_suffix: bool) Allocator.Error!CFStmtId {
        if (stmt_id == stop_id) {
            return self.store.addCFStmt(.{ .jump = .{
                .target = join_id,
                .args = LIR.LocalSpan.empty(),
            } });
        }

        const stmt = self.store.getCFStmt(stmt_id);
        const next = linearNext(stmt) orelse unreachable;
        const rewritten_next = try self.rewritePrefixUntil(next, stop_id, join_id, clone_suffix);
        return self.cloneLinearWithNext(stmt, rewritten_next);
    }

    fn rewriteSwitch(self: *Pass, switch_stmt: @FieldType(CFStmt, "switch_stmt"), clone_suffix: bool) Allocator.Error!CFStmtId {
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

        const chains = try self.allocator.alloc([]CFStmtId, entry_count);
        defer {
            for (chains) |chain| self.allocator.free(chain);
            self.allocator.free(chains);
        }

        var shared_counts = std.AutoHashMap(u32, u32).init(self.allocator);
        defer shared_counts.deinit();

        for (entry_starts, 0..) |entry_start, i| {
            const chain = try self.collectLinearChain(entry_start);
            chains[i] = chain;
            for (chain) |stmt_id| {
                const gop = try shared_counts.getOrPut(@intFromEnum(stmt_id));
                if (gop.found_existing) {
                    gop.value_ptr.* += 1;
                } else {
                    gop.value_ptr.* = 1;
                }
            }
        }

        const first_shared_starts = try self.allocator.alloc(?CFStmtId, entry_count);
        defer self.allocator.free(first_shared_starts);

        for (chains, 0..) |chain, i| {
            first_shared_starts[i] = null;
            for (chain) |stmt_id| {
                const count = shared_counts.get(@intFromEnum(stmt_id)) orelse unreachable;
                if (count >= 2) {
                    first_shared_starts[i] = stmt_id;
                    break;
                }
            }
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
                .body = try self.rewriteStmtWithCloneMode(shared_start, false),
            });
        }

        var rewritten_branches = std.ArrayList(CFSwitchBranch).empty;
        defer rewritten_branches.deinit(self.allocator);

        for (branch_snapshot, 0..) |branch, i| {
            var rewritten_body: ?CFStmtId = null;
            if (first_shared_starts[i]) |shared_start| {
                for (shared_groups.items) |group| {
                    if (group.start == shared_start) {
                        rewritten_body = try self.rewritePrefixUntil(branch.body, shared_start, group.join_id, clone_suffix);
                        break;
                    }
                }
            }

            try rewritten_branches.append(self.allocator, .{
                .value = branch.value,
                .body = rewritten_body orelse try self.rewriteStmtWithCloneMode(branch.body, clone_suffix),
            });
        }

        const branch_span = try self.store.addCFSwitchBranches(rewritten_branches.items);
        const default_index = branch_snapshot.len;
        var rewritten_default: ?CFStmtId = null;
        if (first_shared_starts[default_index]) |shared_start| {
            for (shared_groups.items) |group| {
                if (group.start == shared_start) {
                    rewritten_default = try self.rewritePrefixUntil(switch_stmt.default_branch, shared_start, group.join_id, clone_suffix);
                    break;
                }
            }
        }

        var rewritten_stmt = try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = switch_stmt.cond,
            .branches = branch_span,
            .default_branch = rewritten_default orelse try self.rewriteStmtWithCloneMode(switch_stmt.default_branch, clone_suffix),
        } });

        for (shared_groups.items) |group| {
            rewritten_stmt = try self.store.addCFStmt(.{ .join = .{
                .id = group.join_id,
                .params = LIR.LocalSpan.empty(),
                .body = group.body,
                .remainder = rewritten_stmt,
            } });
        }

        return rewritten_stmt;
    }

    fn rewriteStmt(self: *Pass, stmt_id: CFStmtId) Allocator.Error!CFStmtId {
        return self.rewriteStmtWithCloneMode(stmt_id, false);
    }

    fn rewriteStmtWithCloneMode(self: *Pass, stmt_id: CFStmtId, clone_suffix: bool) Allocator.Error!CFStmtId {
        const must_clone = clone_suffix or self.hasMultipleIncoming(stmt_id);
        const stmt_key = @intFromEnum(stmt_id);
        if (!must_clone) {
            if (self.rewritten_stmt_ids.get(stmt_key)) |rewritten| return rewritten;
        }

        const stmt = self.store.getCFStmt(stmt_id);
        const rewritten = switch (stmt) {
            .assign_symbol => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(assign.next, must_clone)),
            .assign_ref => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(assign.next, must_clone)),
            .assign_literal => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(assign.next, must_clone)),
            .assign_call => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(assign.next, must_clone)),
            .assign_call_indirect => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(assign.next, must_clone)),
            .assign_low_level => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(assign.next, must_clone)),
            .assign_list => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(assign.next, must_clone)),
            .assign_struct => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(assign.next, must_clone)),
            .assign_tag => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(assign.next, must_clone)),
            .set_local => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(assign.next, must_clone)),
            .debug => |debug_stmt| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(debug_stmt.next, must_clone)),
            .expect => |expect_stmt| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(expect_stmt.next, must_clone)),
            .incref => |inc| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(inc.next, must_clone)),
            .decref => |dec| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(dec.next, must_clone)),
            .free => |free_stmt| try self.cloneLinearWithNext(stmt, try self.rewriteStmtWithCloneMode(free_stmt.next, must_clone)),
            .switch_stmt => |switch_stmt| try self.rewriteSwitch(switch_stmt, must_clone),
            .borrow_scope => |scope| try self.store.addCFStmt(.{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.rewriteStmtWithCloneMode(scope.body, must_clone),
                .remainder = try self.rewriteStmtWithCloneMode(scope.remainder, must_clone),
            } }),
            .for_list => |for_stmt| try self.store.addCFStmt(.{ .for_list = .{
                .elem = for_stmt.elem,
                .elem_result = for_stmt.elem_result,
                .elem_ownership = for_stmt.elem_ownership,
                .iterable = for_stmt.iterable,
                .iterable_elem_layout = for_stmt.iterable_elem_layout,
                .body = try self.rewriteStmtWithCloneMode(for_stmt.body, must_clone),
                .next = try self.rewriteStmtWithCloneMode(for_stmt.next, must_clone),
            } }),
            .join => |join| try self.store.addCFStmt(.{ .join = .{
                .id = join.id,
                .params = join.params,
                .body = try self.rewriteStmtWithCloneMode(join.body, must_clone),
                .remainder = try self.rewriteStmtWithCloneMode(join.remainder, must_clone),
            } }),
            .jump, .ret, .runtime_error, .crash, .loop_continue, .scope_exit => stmt_id,
        };

        if (!must_clone) {
            try self.rewritten_stmt_ids.put(stmt_key, rewritten);
        }
        return rewritten;
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
