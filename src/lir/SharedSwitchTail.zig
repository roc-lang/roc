//! Explicitize linear switch-tail sharing into join/jump form for structured backends.

const std = @import("std");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;
const CFStmt = LIR.CFStmt;
const CFStmtId = LIR.CFStmtId;
const CFSwitchBranch = LIR.CFSwitchBranch;
const CFSwitchBranchSpan = LIR.CFSwitchBranchSpan;
const JoinPointId = LIR.JoinPointId;

pub const Pass = struct {
    store: *LirStore,
    allocator: Allocator,
    rewritten_stmt_ids: std.AutoHashMap(u32, CFStmtId),
    next_join_id: u32,

    pub fn init(store: *LirStore, allocator: Allocator) Pass {
        return .{
            .store = store,
            .allocator = allocator,
            .rewritten_stmt_ids = std.AutoHashMap(u32, CFStmtId).init(allocator),
            .next_join_id = nextJoinId(store),
        };
    }

    pub fn deinit(self: *Pass) void {
        self.rewritten_stmt_ids.deinit();
    }

    fn nextJoin(self: *Pass) JoinPointId {
        const id: JoinPointId = @enumFromInt(self.next_join_id);
        self.next_join_id += 1;
        return id;
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

    fn findCommonLinearSuffix(self: *Pass, stmt_ids: []const CFStmtId) Allocator.Error!?CFStmtId {
        if (stmt_ids.len < 2) return null;

        var chains = std.ArrayList([]CFStmtId).empty;
        defer {
            for (chains.items) |chain| self.allocator.free(chain);
            chains.deinit(self.allocator);
        }

        var min_len: usize = std.math.maxInt(usize);
        for (stmt_ids) |stmt_id| {
            const chain = try self.collectLinearChain(stmt_id);
            min_len = @min(min_len, chain.len);
            try chains.append(self.allocator, chain);
        }

        if (min_len == 0) return null;

        var suffix_len: usize = 0;
        while (suffix_len < min_len) : (suffix_len += 1) {
            const expected = chains.items[0][chains.items[0].len - 1 - suffix_len];
            for (chains.items[1..]) |chain| {
                if (chain[chain.len - 1 - suffix_len] != expected) {
                    return if (suffix_len == 0) null else chains.items[0][chains.items[0].len - suffix_len];
                }
            }
        }

        return chains.items[0][chains.items[0].len - suffix_len];
    }

    fn rewritePrefixUntil(self: *Pass, stmt_id: CFStmtId, stop_id: CFStmtId, join_id: JoinPointId) Allocator.Error!CFStmtId {
        if (stmt_id == stop_id) {
            return self.store.addCFStmt(.{ .jump = .{
                .target = join_id,
                .args = LIR.LocalSpan.empty(),
            } });
        }

        const stmt = self.store.getCFStmt(stmt_id);
        const next = linearNext(stmt) orelse unreachable;
        const rewritten_next = try self.rewritePrefixUntil(next, stop_id, join_id);
        return self.cloneLinearWithNext(stmt, rewritten_next);
    }

    fn rewriteSwitch(self: *Pass, switch_stmt: @FieldType(CFStmt, "switch_stmt")) Allocator.Error!CFStmtId {
        var branch_entries = std.ArrayList(CFStmtId).empty;
        defer branch_entries.deinit(self.allocator);

        for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
            try branch_entries.append(self.allocator, branch.body);
        }
        try branch_entries.append(self.allocator, switch_stmt.default_branch);

        if (try self.findCommonLinearSuffix(branch_entries.items)) |shared_start| {
            const join_id = self.nextJoin();
            const initial_jump = try self.store.addCFStmt(.{ .jump = .{
                .target = join_id,
                .args = LIR.LocalSpan.empty(),
            } });

            var rewritten_branches = std.ArrayList(CFSwitchBranch).empty;
            defer rewritten_branches.deinit(self.allocator);

            for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                try rewritten_branches.append(self.allocator, .{
                    .value = branch.value,
                    .body = try self.rewritePrefixUntil(branch.body, shared_start, join_id),
                });
            }

            const rewritten_default = try self.rewritePrefixUntil(switch_stmt.default_branch, shared_start, join_id);
            const branch_span = try self.store.addCFSwitchBranches(rewritten_branches.items);
            const rewritten_switch = try self.store.addCFStmt(.{ .switch_stmt = .{
                .cond = switch_stmt.cond,
                .branches = branch_span,
                .default_branch = rewritten_default,
            } });

            const rewritten_body = try self.rewriteStmt(shared_start);
            return self.store.addCFStmt(.{ .join = .{
                .id = join_id,
                .params = LIR.LocalSpan.empty(),
                .body = rewritten_body,
                .remainder = rewritten_switch,
            } });
        }

        var rewritten_branches = std.ArrayList(CFSwitchBranch).empty;
        defer rewritten_branches.deinit(self.allocator);

        for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
            try rewritten_branches.append(self.allocator, .{
                .value = branch.value,
                .body = try self.rewriteStmt(branch.body),
            });
        }

        const branch_span = try self.store.addCFSwitchBranches(rewritten_branches.items);
        return self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = switch_stmt.cond,
            .branches = branch_span,
            .default_branch = try self.rewriteStmt(switch_stmt.default_branch),
        } });
    }

    fn rewriteStmt(self: *Pass, stmt_id: CFStmtId) Allocator.Error!CFStmtId {
        const stmt_key = @intFromEnum(stmt_id);
        if (self.rewritten_stmt_ids.get(stmt_key)) |rewritten| return rewritten;

        const stmt = self.store.getCFStmt(stmt_id);
        const rewritten = switch (stmt) {
            .assign_symbol => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(assign.next)),
            .assign_ref => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(assign.next)),
            .assign_literal => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(assign.next)),
            .assign_call => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(assign.next)),
            .assign_call_indirect => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(assign.next)),
            .assign_low_level => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(assign.next)),
            .assign_list => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(assign.next)),
            .assign_struct => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(assign.next)),
            .assign_tag => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(assign.next)),
            .set_local => |assign| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(assign.next)),
            .debug => |debug_stmt| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(debug_stmt.next)),
            .expect => |expect_stmt| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(expect_stmt.next)),
            .incref => |inc| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(inc.next)),
            .decref => |dec| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(dec.next)),
            .free => |free_stmt| try self.cloneLinearWithNext(stmt, try self.rewriteStmt(free_stmt.next)),
            .switch_stmt => |switch_stmt| try self.rewriteSwitch(switch_stmt),
            .borrow_scope => |scope| try self.store.addCFStmt(.{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.rewriteStmt(scope.body),
                .remainder = try self.rewriteStmt(scope.remainder),
            } }),
            .for_list => |for_stmt| try self.store.addCFStmt(.{ .for_list = .{
                .elem = for_stmt.elem,
                .elem_result = for_stmt.elem_result,
                .elem_ownership = for_stmt.elem_ownership,
                .iterable = for_stmt.iterable,
                .iterable_elem_layout = for_stmt.iterable_elem_layout,
                .body = try self.rewriteStmt(for_stmt.body),
                .next = try self.rewriteStmt(for_stmt.next),
            } }),
            .join => |join| try self.store.addCFStmt(.{ .join = .{
                .id = join.id,
                .params = join.params,
                .body = try self.rewriteStmt(join.body),
                .remainder = try self.rewriteStmt(join.remainder),
            } }),
            .jump, .ret, .runtime_error, .crash, .loop_continue, .scope_exit => stmt_id,
        };

        try self.rewritten_stmt_ids.put(stmt_key, rewritten);
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
