//! Thread a jump that carries a known tag straight to the arm it selects.
//!
//! A loop written with a flag (`$done = Bool.True`) ends an iteration by
//! jumping to a merge join with a literal tag for the flag and the unchanged
//! loop state for everything else. The merge join forwards its parameters to
//! the loop header, whose body immediately switches on the flag and jumps to
//! the exit. Along that edge the switch outcome is already explicit in the
//! LIR: the edge constructs the tag. Rewriting the edge to jump to the
//! selected exit continuation removes the forwarded state from the path, so
//! a value the exit never reads is no longer live across the iteration's
//! producers. Without this, ARC must retain loop state across the call that
//! consumes it on every iteration, and a list in that state is copied each
//! time it grows.
//!
//! The rewrite is exact. The edge's statements, the forwarding bodies, the
//! header's prefix before its switch, and the selected arm must have the
//! declared shapes; the exit continuation must lexically enclose the edge;
//! and nothing the exit executes may read a local whose value the rewrite
//! would change. Any other shape is left unchanged.

const std = @import("std");
const core = @import("lir_core");
const body_clone = @import("body_clone.zig");
const collections = @import("collections");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = LirStore.GuardedList;
const Allocator = std.mem.Allocator;
const LocalId = LIR.LocalId;
const CFStmtId = LIR.CFStmtId;

/// Allocation failures produced while threading known-tag jumps.
pub const ResourceError = Allocator.Error;

/// What the rewritten edge knows about a local's value.
const Value = union(enum) {
    /// A tag with no payload and this discriminant.
    tag: u16,
    /// The discriminant of a tag, as read by `ref.discriminant`.
    discriminant: u16,
    /// A struct built on the edge, one value per field.
    fields: []const Value,
    /// Whatever this local holds when the edge runs.
    local: LocalId,
    /// A value the rewrite cannot describe.
    opaque_value,
};

const Env = collections.DenseMap(LocalId, Value);

const Candidate = struct {
    /// The edge's statements in execution order, ending with its jump.
    chain: []CFStmtId,
    exit: LIR.JoinPointId,
};

/// Rewrite every eligible edge of one procedure. Layouts are not consulted:
/// only statement structure and explicit tag discriminants decide.
pub fn runProc(store: *LirStore, proc: LIR.LirProcSpecId, allocator: Allocator) ResourceError!void {
    const body = body_clone.rewritableProcBody(store, proc) orelse return;
    // Each rewrite deletes at least one join-parameter write, so the loop
    // runs at most once per such write.
    while (true) {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        var analysis = try Analysis.init(store, body, arena.allocator());
        const candidate = try analysis.findCandidate() orelse return;
        try analysis.apply(candidate);
    }
}

const Analysis = struct {
    store: *LirStore,
    body: CFStmtId,
    allocator: Allocator,
    joins: collections.DenseMap(LIR.JoinPointId, CFStmtId),
    /// The unique structural predecessor of every statement that has one.
    single_pred: collections.DenseMap(CFStmtId, CFStmtId),
    /// Statements with more than one structural predecessor.
    shared: collections.DenseMap(CFStmtId, void),
    /// Discriminants of payload-free tags bound exactly once.
    tag_defs: collections.DenseMap(LocalId, u16),
    reads: body_clone.ReadCounts,

    fn init(store: *LirStore, body: CFStmtId, allocator: Allocator) ResourceError!Analysis {
        var self: Analysis = .{
            .store = store,
            .body = body,
            .allocator = allocator,
            .joins = collections.DenseMap(LIR.JoinPointId, CFStmtId).init(allocator),
            .single_pred = collections.DenseMap(CFStmtId, CFStmtId).init(allocator),
            .shared = collections.DenseMap(CFStmtId, void).init(allocator),
            .tag_defs = collections.DenseMap(LocalId, u16).init(allocator),
            .reads = try body_clone.countReachableReadsWithAllocator(store, body, allocator),
        };
        var defs = try body_clone.countReachableDefsWithAllocator(store, body, allocator);
        var successors = std.ArrayList(CFStmtId).empty;
        var walk = try body_clone.ReachableStmts.initWithAllocator(store, body, allocator);
        while (try walk.next()) |stmt_id| {
            const stmt = store.getCFStmt(stmt_id);
            if (stmt == .join) {
                try self.joins.put(stmt.join.id, stmt_id);
            } else if (stmt == .assign_tag) {
                const tag = stmt.assign_tag;
                if (tag.payload == null and tag.target_desc == null and defs.get(tag.target) == 1) {
                    try self.tag_defs.put(tag.target, tag.discriminant);
                }
            }
            successors.clearRetainingCapacity();
            try body_clone.appendSuccessorsWithAllocator(store, &successors, stmt_id, allocator);
            for (successors.items) |successor| {
                if (self.shared.contains(successor)) continue;
                if (self.single_pred.fetchRemove(successor)) |_| {
                    try self.shared.put(successor, {});
                } else {
                    try self.single_pred.put(successor, stmt_id);
                }
            }
        }
        return self;
    }

    fn findCandidate(self: *Analysis) ResourceError!?Candidate {
        var walk = try body_clone.ReachableStmts.initWithAllocator(self.store, self.body, self.allocator);
        while (try walk.next()) |stmt_id| {
            const stmt = self.store.getCFStmt(stmt_id);
            if (stmt != .jump) continue;
            if (try self.candidateAt(stmt_id)) |candidate| return candidate;
        }
        return null;
    }

    /// The edge ending at `jump_id`, when threading it is exact.
    fn candidateAt(self: *Analysis, jump_id: CFStmtId) ResourceError!?Candidate {
        const chain = try self.edgeChain(jump_id);
        const first_target = self.store.getCFStmt(jump_id).jump.target;

        // The values the edge passes to its target join's parameters.
        var env = Env.init(self.allocator);
        var pending = Env.init(self.allocator);
        for (chain[0 .. chain.len - 1]) |stmt_id| {
            const stmt = self.store.getCFStmt(stmt_id);
            if (stmt == .set_local) {
                try pending.put(stmt.set_local.target, self.resolve(&env, stmt.set_local.value));
            } else {
                try env.put(edgeTarget(stmt).?, try self.edgeValue(&env, stmt));
            }
        }

        // Locals whose value on the threaded path differs from the original
        // path: every forwarded parameter the edge does not pass unchanged,
        // and every definition the threaded path skips.
        var changed = collections.DenseMap(LocalId, void).init(self.allocator);
        var target = first_target;
        var visited = collections.DenseMap(LIR.JoinPointId, void).init(self.allocator);
        const exit = while (true) {
            if ((try visited.getOrPut(target)).found_existing) return null;
            const join_stmt = self.joins.get(target) orelse return null;
            const join = self.store.getCFStmt(join_stmt).join;
            if (!plainJoin(join)) return null;
            const params = self.store.getLocalSpan(join.params);
            if (params.len == 0 or pending.count() != params.len) return null;
            env.clearRetainingCapacity();
            for (0..params.len) |index| {
                const param = GuardedList.at(params, index);
                const value = pending.get(param) orelse return null;
                try env.put(param, value);
                const unchanged = value == .local and value.local == param;
                if (!unchanged) try changed.put(param, {});
            }
            pending.clearRetainingCapacity();

            var cursor = join.body;
            const step: union(enum) { forward: LIR.JoinPointId, exit: LIR.JoinPointId } = while (true) {
                const stmt = self.store.getCFStmt(cursor);
                if (stmt == .assign_ref) {
                    const assign = stmt.assign_ref;
                    const value: Value = switch (assign.op) {
                        .local => |source| self.resolve(&env, source),
                        .discriminant => |op| switch (self.resolve(&env, op.source)) {
                            .tag => |discriminant| .{ .discriminant = discriminant },
                            .discriminant, .fields, .local, .opaque_value => .opaque_value,
                        },
                        .field => |op| switch (self.resolve(&env, op.source)) {
                            .fields => |fields| if (op.field_idx < fields.len) fields[op.field_idx] else return null,
                            .tag, .discriminant, .local, .opaque_value => .opaque_value,
                        },
                        .tag_payload, .tag_payload_struct, .list_reinterpret, .nominal => return null,
                    };
                    try env.put(assign.target, value);
                    try changed.put(assign.target, {});
                    cursor = assign.next;
                } else if (stmt == .assign_literal) {
                    try env.put(stmt.assign_literal.target, .opaque_value);
                    try changed.put(stmt.assign_literal.target, {});
                    cursor = stmt.assign_literal.next;
                } else if (stmt == .join) {
                    cursor = stmt.join.remainder;
                } else if (stmt == .set_local) {
                    const set = stmt.set_local;
                    if (set.mode != .initialize_join_param) return null;
                    try pending.put(set.target, self.resolve(&env, set.value));
                    cursor = set.next;
                } else if (stmt == .jump) {
                    break .{ .forward = stmt.jump.target };
                } else if (stmt == .switch_stmt) {
                    if (pending.count() != 0) return null;
                    const discriminant = switch (self.resolve(&env, stmt.switch_stmt.cond)) {
                        .tag, .discriminant => |value| value,
                        .fields, .local, .opaque_value => return null,
                    };
                    const arm = self.selectedArm(stmt.switch_stmt, discriminant);
                    const arm_stmt = self.store.getCFStmt(arm);
                    if (arm_stmt != .jump) return null;
                    break .{ .exit = arm_stmt.jump.target };
                } else {
                    return null;
                }
            };
            switch (step) {
                .forward => |next_target| target = next_target,
                .exit => |exit_target| break exit_target,
            }
        };

        if (exit == first_target) return null;
        const exit_stmt = self.joins.get(exit) orelse return null;
        const exit_join = self.store.getCFStmt(exit_stmt).join;
        if (!plainJoin(exit_join) or exit_join.params.len != 0) return null;
        if (try self.reachableOutside(chain[0], exit_stmt)) return null;
        if (try self.exitReads(exit_join.body, &changed)) return null;
        return .{ .chain = chain, .exit = exit };
    }

    /// The maximal linear run of edge statements ending at `jump_id`. Every
    /// statement has exactly one structural predecessor, so rewriting the
    /// run changes no other path.
    fn edgeChain(self: *Analysis, jump_id: CFStmtId) ResourceError![]CFStmtId {
        var reversed = std.ArrayList(CFStmtId).empty;
        try reversed.append(self.allocator, jump_id);
        var cursor = jump_id;
        while (true) {
            if (self.shared.contains(cursor)) break;
            const pred = self.single_pred.get(cursor) orelse break;
            if (!edgeStatement(self.store.getCFStmt(pred))) break;
            if (self.shared.contains(pred)) break;
            try reversed.append(self.allocator, pred);
            cursor = pred;
        }
        std.mem.reverse(CFStmtId, reversed.items);
        return reversed.items;
    }

    /// The value an edge definition binds.
    fn edgeValue(self: *Analysis, env: *const Env, stmt: LIR.CFStmt) ResourceError!Value {
        if (stmt == .assign_ref) return self.resolve(env, stmt.assign_ref.op.local);
        if (stmt == .assign_tag) return .{ .tag = stmt.assign_tag.discriminant };
        if (stmt == .assign_struct) return .{ .fields = try self.resolveSpan(env, stmt.assign_struct.fields) };
        return .opaque_value;
    }

    fn resolveSpan(self: *Analysis, env: *const Env, span: LIR.LocalSpan) ResourceError![]const Value {
        const locals = self.store.getLocalSpan(span);
        const values = try self.allocator.alloc(Value, locals.len);
        for (values, 0..) |*value, index| value.* = self.resolve(env, GuardedList.at(locals, index));
        return values;
    }

    fn resolve(self: *const Analysis, env: *const Env, local: LocalId) Value {
        if (env.get(local)) |value| return value;
        if (self.tag_defs.get(local)) |discriminant| return .{ .tag = discriminant };
        return .{ .local = local };
    }

    fn selectedArm(self: *const Analysis, switch_stmt: @FieldType(LIR.CFStmt, "switch_stmt"), discriminant: u16) CFStmtId {
        const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
        for (0..branches.len) |index| {
            const branch = GuardedList.at(branches, index);
            if (branch.value == discriminant) return branch.body;
        }
        return switch_stmt.default_branch;
    }

    /// Whether `stmt` is reachable from the procedure body without entering
    /// the declaration `scope`. When it is not, `scope` lexically encloses
    /// every occurrence of `stmt`, so a jump there may target it.
    fn reachableOutside(self: *Analysis, stmt: CFStmtId, scope: CFStmtId) ResourceError!bool {
        var seen = collections.DenseMap(CFStmtId, void).init(self.allocator);
        var work = std.ArrayList(CFStmtId).empty;
        try work.append(self.allocator, self.body);
        while (work.pop()) |current| {
            if (current == stmt) return true;
            if (current == scope) continue;
            if ((try seen.getOrPut(current)).found_existing) continue;
            try body_clone.appendSuccessorsWithAllocator(self.store, &work, current, self.allocator);
        }
        return false;
    }

    /// Whether anything the exit continuation executes, following jumps into
    /// join bodies, reads a local in `changed`.
    fn exitReads(self: *Analysis, exit_body: CFStmtId, changed: *const collections.DenseMap(LocalId, void)) ResourceError!bool {
        const Probe = struct {
            changed: *const collections.DenseMap(LocalId, void),
            hit: bool = false,

            fn note(probe: *@This(), local: LocalId) void {
                if (probe.changed.contains(local)) probe.hit = true;
            }
        };
        var probe: Probe = .{ .changed = changed };
        var seen = collections.DenseMap(CFStmtId, void).init(self.allocator);
        var work = std.ArrayList(CFStmtId).empty;
        try work.append(self.allocator, exit_body);
        while (work.pop()) |current| {
            if ((try seen.getOrPut(current)).found_existing) continue;
            const stmt = self.store.getCFStmt(current);
            body_clone.forEachStmtRead(self.store, stmt, &probe, Probe.note);
            if (probe.hit) return true;
            if (stmt == .jump) {
                const target = self.joins.get(stmt.jump.target) orelse return true;
                try work.append(self.allocator, self.store.getCFStmt(target).join.body);
            } else {
                try body_clone.appendSuccessorsWithAllocator(self.store, &work, current, self.allocator);
            }
        }
        return false;
    }

    /// Replace the edge with its surviving statements followed by a jump to
    /// the exit. Parameter writes go, and so does every edge definition left
    /// unread once they have.
    fn apply(self: *Analysis, candidate: Candidate) ResourceError!void {
        const chain = candidate.chain;
        const removed = try self.allocator.alloc(bool, chain.len);
        @memset(removed, false);
        var remaining = collections.DenseMap(LocalId, u32).init(self.allocator);
        for (chain[0 .. chain.len - 1], 0..) |stmt_id, index| {
            const stmt = self.store.getCFStmt(stmt_id);
            if (stmt == .set_local) removed[index] = true;
            const target = edgeTarget(stmt) orelse continue;
            try remaining.put(target, self.reads.get(target));
        }
        var index = chain.len - 1;
        while (index > 0) {
            index -= 1;
            const stmt = self.store.getCFStmt(chain[index]);
            if (!removed[index]) {
                const target = edgeTarget(stmt).?;
                if (remaining.get(target).? != 0) continue;
                removed[index] = true;
            }
            body_clone.forEachStmtRead(self.store, stmt, &remaining, releaseRead);
        }

        const old_jump = chain[chain.len - 1];
        var next = try self.store.addCFStmt(.{ .jump = .{ .target = candidate.exit } }, self.store.stmtOrigin(old_jump));
        index = chain.len - 1;
        while (index > 0) {
            index -= 1;
            if (removed[index]) continue;
            const stmt = withNext(self.store.getCFStmt(chain[index]), next);
            next = try self.store.addCFStmt(stmt, self.store.stmtOrigin(chain[index]));
        }
        try self.store.replaceCFStmt(chain[0], self.store.getCFStmt(next), self.store.stmtOrigin(next));
    }
};

fn releaseRead(remaining: *collections.DenseMap(LocalId, u32), local: LocalId) void {
    if (remaining.getPtr(local)) |count| count.* -= 1;
}

/// Statements an edge may consist of before its jump: pure local aliases,
/// payload-free tags, literals, structs, and join-parameter writes.
fn edgeStatement(stmt: LIR.CFStmt) bool {
    if (stmt == .assign_ref) return stmt.assign_ref.op == .local and stmt.assign_ref.take_kind == .none;
    if (stmt == .assign_tag) return stmt.assign_tag.payload == null and stmt.assign_tag.target_desc == null;
    if (stmt == .assign_literal) return true;
    if (stmt == .assign_struct) return stmt.assign_struct.contents_desc == null;
    if (stmt == .set_local) return stmt.set_local.mode == .initialize_join_param;
    return false;
}

/// The local an edge definition binds.
fn edgeTarget(stmt: LIR.CFStmt) ?LocalId {
    if (stmt == .assign_ref) return stmt.assign_ref.target;
    if (stmt == .assign_tag) return stmt.assign_tag.target;
    if (stmt == .assign_literal) return stmt.assign_literal.target;
    if (stmt == .assign_struct) return stmt.assign_struct.target;
    return null;
}

/// An edge definition continuing at `next` instead.
fn withNext(stmt: LIR.CFStmt, next: CFStmtId) LIR.CFStmt {
    var updated = stmt;
    if (updated == .assign_ref) {
        updated.assign_ref.next = next;
    } else if (updated == .assign_tag) {
        updated.assign_tag.next = next;
    } else if (updated == .assign_literal) {
        updated.assign_literal.next = next;
    } else if (updated == .assign_struct) {
        updated.assign_struct.next = next;
    } else {
        unreachable;
    }
    return updated;
}

/// A join with ordinary parameters only: no retained units and no
/// maybe-uninitialized environment.
fn plainJoin(join: @FieldType(LIR.CFStmt, "join")) bool {
    return join.retained.len == 0 and join.maybe_uninitialized_params.len == 0;
}

const testing = std.testing;

const FlagLoop = struct {
    store: LirStore,
    proc: LIR.LirProcSpecId,
    /// The first statement of the loop body's `Done` arm.
    done_arm: CFStmtId,
    exit: LIR.JoinPointId,
    merge: LIR.JoinPointId,

    fn deinit(self: *FlagLoop) void {
        self.store.deinit();
    }

    /// The join the `Done` arm finally jumps to, and whether it still writes
    /// join parameters on the way.
    fn doneEdge(self: *const FlagLoop) struct { target: LIR.JoinPointId, writes_params: bool } {
        var writes_params = false;
        var cursor = self.done_arm;
        while (true) {
            const stmt = self.store.getCFStmt(cursor);
            if (stmt == .jump) return .{ .target = stmt.jump.target, .writes_params = writes_params };
            if (stmt == .set_local) writes_params = true;
            cursor = if (stmt == .set_local)
                stmt.set_local.next
            else if (stmt == .assign_ref)
                stmt.assign_ref.next
            else if (stmt == .assign_tag)
                stmt.assign_tag.next
            else
                stmt.assign_literal.next;
        }
    }
};

/// `while !$done { match step($state) { Done => $done = True, Next(s) => { $state = s; $steps += 1 } } }`
/// lowered with the loop exit reading either `$steps` or `$done`.
fn flagLoop(exit_reads: enum { steps, done }) Allocator.Error!FlagLoop {
    var store = LirStore.init(testing.allocator);
    errdefer store.deinit();
    var joins = body_clone.JoinParamIndex.init(testing.allocator);
    defer joins.deinit();
    const header = joins.freshJoinPoint();
    const exit = joins.freshJoinPoint();
    const loop_body = joins.freshJoinPoint();
    const merge = joins.freshJoinPoint();

    const local = struct {
        fn add(s: *LirStore, layout_idx: @import("layout").Idx) Allocator.Error!LocalId {
            return try s.addLocal(.{ .layout_idx = layout_idx });
        }
    }.add;
    const done = try local(&store, .bool);
    const state = try local(&store, .u64);
    const steps = try local(&store, .u64);
    const merge_done = try local(&store, .bool);
    const merge_state = try local(&store, .u64);
    const merge_steps = try local(&store, .u64);

    // Loop exit.
    const exit_value = try local(&store, if (exit_reads == .steps) .u64 else .bool);
    const ret = try store.addCFStmt(.{ .ret = .{ .value = exit_value } }, .test_fixture);
    const exit_body = try store.addCFStmt(.{ .assign_ref = .{
        .target = exit_value,
        .op = .{ .local = if (exit_reads == .steps) steps else done },
        .next = ret,
    } }, .test_fixture);

    // Merge join body: forward every parameter to the header.
    const back_edge = try store.addCFStmt(.{ .jump = .{ .target = header } }, .test_fixture);
    var forward = back_edge;
    for ([_]LocalId{ steps, state, done }, [_]LocalId{ merge_steps, merge_state, merge_done }) |header_param, merge_param| {
        const copy = try local(&store, store.getLocal(merge_param).layout_idx);
        forward = try store.addCFStmt(.{ .set_local = .{ .target = header_param, .value = copy, .mode = .initialize_join_param, .next = forward } }, .test_fixture);
        forward = try store.addCFStmt(.{ .assign_ref = .{ .target = copy, .op = .{ .local = merge_param }, .next = forward } }, .test_fixture);
    }

    // `Done`: the flag becomes True, the rest of the state is unchanged.
    const done_jump = try store.addCFStmt(.{ .jump = .{ .target = merge } }, .test_fixture);
    const true_tag = try local(&store, .bool);
    const same_state = try local(&store, .u64);
    const same_steps = try local(&store, .u64);
    var done_arm = done_jump;
    done_arm = try store.addCFStmt(.{ .set_local = .{ .target = merge_steps, .value = same_steps, .mode = .initialize_join_param, .next = done_arm } }, .test_fixture);
    done_arm = try store.addCFStmt(.{ .set_local = .{ .target = merge_state, .value = same_state, .mode = .initialize_join_param, .next = done_arm } }, .test_fixture);
    done_arm = try store.addCFStmt(.{ .set_local = .{ .target = merge_done, .value = true_tag, .mode = .initialize_join_param, .next = done_arm } }, .test_fixture);
    done_arm = try store.addCFStmt(.{ .assign_ref = .{ .target = same_steps, .op = .{ .local = steps }, .next = done_arm } }, .test_fixture);
    done_arm = try store.addCFStmt(.{ .assign_ref = .{ .target = same_state, .op = .{ .local = state }, .next = done_arm } }, .test_fixture);
    done_arm = try store.addCFStmt(.{ .assign_tag = .{ .target = true_tag, .variant_index = 1, .discriminant = 1, .payload = null, .next = done_arm } }, .test_fixture);

    // `Next`: the flag is unchanged and the state is replaced.
    const next_jump = try store.addCFStmt(.{ .jump = .{ .target = merge } }, .test_fixture);
    const same_done = try local(&store, .bool);
    const next_state = try local(&store, .u64);
    const next_steps = try local(&store, .u64);
    var next_arm = next_jump;
    next_arm = try store.addCFStmt(.{ .set_local = .{ .target = merge_steps, .value = next_steps, .mode = .initialize_join_param, .next = next_arm } }, .test_fixture);
    next_arm = try store.addCFStmt(.{ .set_local = .{ .target = merge_state, .value = next_state, .mode = .initialize_join_param, .next = next_arm } }, .test_fixture);
    next_arm = try store.addCFStmt(.{ .set_local = .{ .target = merge_done, .value = same_done, .mode = .initialize_join_param, .next = next_arm } }, .test_fixture);
    next_arm = try store.addCFStmt(.{ .assign_literal = .{ .target = next_steps, .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } }, .next = next_arm } }, .test_fixture);
    next_arm = try store.addCFStmt(.{ .assign_literal = .{ .target = next_state, .value = .{ .i64_literal = .{ .value = 2, .layout_idx = .u64 } }, .next = next_arm } }, .test_fixture);
    next_arm = try store.addCFStmt(.{ .assign_ref = .{ .target = same_done, .op = .{ .local = done }, .next = next_arm } }, .test_fixture);

    // The step outcome, standing in for a call's result.
    const step = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = state,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = done_arm }}),
        .default_branch = next_arm,
    } }, .test_fixture);
    const merge_join = try store.addCFStmt(.{ .join = .{
        .id = merge,
        .params = try store.addLocalSpan(&.{ merge_done, merge_state, merge_steps }),
        .body = forward,
        .remainder = step,
    } }, .test_fixture);

    // Header: `while !$done`.
    const flag_copy = try local(&store, .bool);
    const flag_discriminant = try local(&store, .u16);
    const test_flag = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = flag_discriminant,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = try store.addCFStmt(.{ .jump = .{ .target = exit } }, .test_fixture) }}),
        .default_branch = try store.addCFStmt(.{ .jump = .{ .target = loop_body } }, .test_fixture),
    } }, .test_fixture);
    const read_discriminant = try store.addCFStmt(.{ .assign_ref = .{ .target = flag_discriminant, .op = .{ .discriminant = .{ .source = flag_copy } }, .next = test_flag } }, .test_fixture);
    const copy_flag = try store.addCFStmt(.{ .assign_ref = .{ .target = flag_copy, .op = .{ .local = done }, .next = read_discriminant } }, .test_fixture);
    const body_join = try store.addCFStmt(.{ .join = .{ .id = loop_body, .params = .empty(), .body = merge_join, .remainder = copy_flag } }, .test_fixture);
    const exit_join = try store.addCFStmt(.{ .join = .{ .id = exit, .params = .empty(), .body = exit_body, .remainder = body_join } }, .test_fixture);

    // Entry.
    const enter = try store.addCFStmt(.{ .jump = .{ .target = header } }, .test_fixture);
    const initial_done = try local(&store, .bool);
    const initial_state = try local(&store, .u64);
    const initial_steps = try local(&store, .u64);
    var entry = enter;
    entry = try store.addCFStmt(.{ .set_local = .{ .target = steps, .value = initial_steps, .mode = .initialize_join_param, .next = entry } }, .test_fixture);
    entry = try store.addCFStmt(.{ .set_local = .{ .target = state, .value = initial_state, .mode = .initialize_join_param, .next = entry } }, .test_fixture);
    entry = try store.addCFStmt(.{ .set_local = .{ .target = done, .value = initial_done, .mode = .initialize_join_param, .next = entry } }, .test_fixture);
    entry = try store.addCFStmt(.{ .assign_literal = .{ .target = initial_steps, .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } }, .next = entry } }, .test_fixture);
    entry = try store.addCFStmt(.{ .assign_literal = .{ .target = initial_state, .value = .{ .i64_literal = .{ .value = 5, .layout_idx = .u64 } }, .next = entry } }, .test_fixture);
    entry = try store.addCFStmt(.{ .assign_tag = .{ .target = initial_done, .variant_index = 0, .discriminant = 0, .payload = null, .next = entry } }, .test_fixture);
    const header_join = try store.addCFStmt(.{ .join = .{
        .id = header,
        .params = try store.addLocalSpan(&.{ done, state, steps }),
        .body = exit_join,
        .remainder = entry,
    } }, .test_fixture);

    const proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .identity = LIR.ProcIdentity.forTest(0),
        .args = .empty(),
        .body = header_join,
        .ret_layout = store.getLocal(exit_value).layout_idx,
    }, .none);
    return .{ .store = store, .proc = proc, .done_arm = done_arm, .exit = exit, .merge = merge };
}

test "known tag jump sends a flag loop's exit edge straight to the exit" {
    var fixture = try flagLoop(.steps);
    defer fixture.deinit();
    try runProc(&fixture.store, fixture.proc, testing.allocator);
    const edge = fixture.doneEdge();
    try testing.expectEqual(fixture.exit, edge.target);
    try testing.expect(!edge.writes_params);
}

test "known tag jump keeps the edge when the exit reads the flag it sets" {
    var fixture = try flagLoop(.done);
    defer fixture.deinit();
    try runProc(&fixture.store, fixture.proc, testing.allocator);
    const edge = fixture.doneEdge();
    try testing.expectEqual(fixture.merge, edge.target);
    try testing.expect(edge.writes_params);
}
