//! ARC borrow inference over ownership-neutral LIR.
//!
//! Solving runs before RC statement emission and decides, for every
//! refcounted local, whether its binding is owned (it carries exactly one
//! ownership unit that emission must move or release) or borrowed (it is an
//! alias into another local's value and emits no RC statements at all).
//!
//! A binding solves to borrowed when all of the following hold:
//!
//! - its single defining statement is borrow-capable: a payload read
//!   (`assign_ref` with `.field`/`.tag_payload`/`.tag_payload_struct`), a
//!   local alias (`.local`, `.list_reinterpret`, `.nominal`), or a low-level
//!   op whose `RcEffect.result_borrows_args` names exactly one refcounted
//!   argument
//! - no occurrence of the binding demands ownership: it is never a call or
//!   hosted-call argument position solved owned, a consumed or retained
//!   low-level argument, an aggregate or capture operand, a `set_local`
//!   source, or a returned value
//! - the lender chain resolves to an owned leader local that is bound exactly
//!   once, so emission can extend the leader's lifetime past every use of the
//!   borrow by placing the leader's release after the group's last use
//!
//! Everything else stays owned, which is always sound: an owned occurrence is
//! emitted as a move or an incref exactly as all-owned insertion would emit
//! it. The solution is ARC-stage-local and is dropped when insertion ends.

const std = @import("std");
const core = @import("lir_core");

const LIR = core.LIR;
const LirStore = core.LirStore;
const Allocator = std.mem.Allocator;

pub const SolveError = std.mem.Allocator.Error;

const no_local: u32 = std.math.maxInt(u32);

/// Per-local binding-mode solution plus liveness groups. A group is one owned
/// leader local together with every borrowed local whose liveness anchors on
/// it; emission keeps the leader's ownership unit alive until the last use of
/// any group member.
pub const Solution = struct {
    allocator: Allocator,
    /// Bit set => the local's binding is borrowed.
    borrowed: std.bit_set.DynamicBitSetUnmanaged,
    /// Owned leader anchoring each local's liveness; the local itself when
    /// the binding is owned.
    leader: []u32,
    /// Flat group-member storage indexed through `member_offsets`. Every
    /// local's group contains at least itself.
    member_offsets: []u32,
    member_lens: []u32,
    members: []u32,

    pub fn deinit(self: *Solution) void {
        self.borrowed.deinit(self.allocator);
        self.allocator.free(self.leader);
        self.allocator.free(self.member_offsets);
        self.allocator.free(self.member_lens);
        self.allocator.free(self.members);
    }

    pub fn isBorrowed(self: *const Solution, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        if (index >= self.leader.len) return false;
        return self.borrowed.isSet(index);
    }

    pub fn leaderOf(self: *const Solution, local: LIR.LocalId) LIR.LocalId {
        const index = @intFromEnum(local);
        if (index >= self.leader.len) return local;
        return @enumFromInt(self.leader[index]);
    }

    /// Members of the leader's liveness group, including the leader.
    pub fn groupMembers(self: *const Solution, leader: LIR.LocalId) []const u32 {
        const index = @intFromEnum(leader);
        if (index >= self.member_offsets.len) return &.{};
        const offset = self.member_offsets[index];
        const len = self.member_lens[index];
        return self.members[offset..][0..len];
    }

    /// Reports whether any group member of `leader` extends beyond the
    /// leader itself.
    pub fn groupHasBorrowers(self: *const Solution, leader: LIR.LocalId) bool {
        return self.groupMembers(leader).len > 1;
    }
};

const DefKind = union(enum) {
    none,
    multi,
    fresh,
    borrow_capable: u32,
};

/// Solves binding modes for every local in the store.
pub fn solve(
    allocator: Allocator,
    store: *const LirStore,
    rc_local: []const bool,
) SolveError!Solution {
    const local_count = store.locals.items.len;

    const defs = try allocator.alloc(DefKind, local_count);
    defer allocator.free(defs);
    @memset(defs, .none);
    const demand = try allocator.alloc(bool, local_count);
    defer allocator.free(demand);
    @memset(demand, false);

    var visited = std.AutoHashMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();
    var stack = std.ArrayList(LIR.CFStmtId).empty;
    defer stack.deinit(allocator);

    for (store.proc_specs.items) |proc| {
        const body = proc.body orelse continue;
        for (store.getLocalSpan(proc.args)) |param| {
            // Parameters are bound once at entry; a body assignment makes
            // them multi-bound.
            noteDef(defs, param, .fresh);
        }
        visited.clearRetainingCapacity();
        stack.clearRetainingCapacity();
        try stack.append(allocator, body);
        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});
            try collectStmt(store, rc_local, defs, demand, &stack, allocator, current);
        }
    }

    var solution = Solution{
        .allocator = allocator,
        .borrowed = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count),
        .leader = try allocator.alloc(u32, local_count),
        .member_offsets = &.{},
        .member_lens = &.{},
        .members = &.{},
    };
    errdefer solution.deinit();

    for (0..local_count) |index| {
        solution.leader[index] = @intCast(index);
    }

    // Resolve each local's lender chain. A chain link stays borrowed only if
    // the link itself qualifies and the chain bottoms out at a once-bound
    // owned leader.
    var chain = std.ArrayList(u32).empty;
    defer chain.deinit(allocator);
    var resolved = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer resolved.deinit(allocator);
    var on_chain = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, local_count);
    defer on_chain.deinit(allocator);

    for (0..local_count) |start_index| {
        if (resolved.isSet(start_index)) continue;
        chain.clearRetainingCapacity();
        var cursor: u32 = @intCast(start_index);

        const leader: u32 = while (true) {
            if (resolved.isSet(cursor)) {
                // Already solved: its leader is final.
                break solution.leader[cursor];
            }
            if (on_chain.isSet(cursor)) {
                // Defensive: a def cycle cannot borrow; the cycle head owns.
                break cursor;
            }
            if (!borrowQualifies(rc_local, defs, demand, cursor)) {
                break cursor;
            }
            on_chain.set(cursor);
            try chain.append(allocator, cursor);
            cursor = defs[cursor].borrow_capable;
        };

        const leader_once_bound = switch (defs[leader]) {
            .fresh, .borrow_capable => true,
            .none, .multi => false,
        };
        const leader_is_anchor = rc_local[leader] and leader_once_bound and
            !solution.borrowed.isSet(leader);

        for (chain.items) |link| {
            on_chain.unset(link);
            resolved.set(link);
            if (leader_is_anchor and link != leader) {
                solution.borrowed.set(link);
                solution.leader[link] = leader;
            } else {
                solution.leader[link] = link;
            }
        }
        resolved.set(leader);
    }

    // Build flat group-member lists: every local lists at least itself;
    // borrowed locals are appended to their leader's group.
    const member_offsets = try allocator.alloc(u32, local_count);
    errdefer allocator.free(member_offsets);
    const member_lens = try allocator.alloc(u32, local_count);
    errdefer allocator.free(member_lens);
    @memset(member_lens, 0);

    for (0..local_count) |index| {
        member_lens[solution.leader[index]] += 1;
    }
    var offset: u32 = 0;
    for (0..local_count) |index| {
        member_offsets[index] = offset;
        offset += member_lens[index];
    }
    const members = try allocator.alloc(u32, local_count);
    errdefer allocator.free(members);
    var fill = try allocator.alloc(u32, local_count);
    defer allocator.free(fill);
    @memset(fill, 0);
    for (0..local_count) |index| {
        const leader = solution.leader[index];
        members[member_offsets[leader] + fill[leader]] = @intCast(index);
        fill[leader] += 1;
    }

    solution.member_offsets = member_offsets;
    solution.member_lens = member_lens;
    solution.members = members;
    return solution;
}

fn borrowQualifies(
    rc_local: []const bool,
    defs: []const DefKind,
    demand: []const bool,
    index: u32,
) bool {
    if (!rc_local[index]) return false;
    if (demand[index]) return false;
    return switch (defs[index]) {
        .borrow_capable => true,
        .none, .multi, .fresh => false,
    };
}

fn noteDef(defs: []DefKind, local: LIR.LocalId, kind: DefKind) void {
    const index = @intFromEnum(local);
    defs[index] = switch (defs[index]) {
        .none => kind,
        .multi, .fresh, .borrow_capable => .multi,
    };
}

fn noteDemand(demand: []bool, local: LIR.LocalId) void {
    demand[@intFromEnum(local)] = true;
}

fn noteDemandSpan(store: *const LirStore, demand: []bool, span: LIR.LocalSpan) void {
    for (store.getLocalSpan(span)) |local| {
        noteDemand(demand, local);
    }
}

fn collectStmt(
    store: *const LirStore,
    rc_local: []const bool,
    defs: []DefKind,
    demand: []bool,
    stack: *std.ArrayList(LIR.CFStmtId),
    allocator: Allocator,
    current: LIR.CFStmtId,
) SolveError!void {
    switch (store.getCFStmt(current)) {
        .assign_ref => |assign| {
            switch (assign.op) {
                .local => |source| {
                    if (assign.target != source) {
                        noteDef(defs, assign.target, .{ .borrow_capable = @intFromEnum(source) });
                    } else {
                        noteDef(defs, assign.target, .multi);
                    }
                },
                .discriminant => noteDef(defs, assign.target, .fresh),
                .field => |op| noteDef(defs, assign.target, .{ .borrow_capable = @intFromEnum(op.source) }),
                .tag_payload => |op| noteDef(defs, assign.target, .{ .borrow_capable = @intFromEnum(op.source) }),
                .tag_payload_struct => |op| noteDef(defs, assign.target, .{ .borrow_capable = @intFromEnum(op.source) }),
                .list_reinterpret => |op| noteDef(defs, assign.target, .{ .borrow_capable = @intFromEnum(op.backing_ref) }),
                .nominal => |op| noteDef(defs, assign.target, .{ .borrow_capable = @intFromEnum(op.backing_ref) }),
            }
            try stack.append(allocator, assign.next);
        },
        .assign_literal => |assign| {
            noteDef(defs, assign.target, .fresh);
            try stack.append(allocator, assign.next);
        },
        .assign_call => |assign| {
            noteDef(defs, assign.target, .fresh);
            noteDemandSpan(store, demand, assign.args);
            try stack.append(allocator, assign.next);
        },
        .assign_call_erased => |assign| {
            noteDef(defs, assign.target, .fresh);
            noteDemand(demand, assign.closure);
            noteDemandSpan(store, demand, assign.args);
            try stack.append(allocator, assign.next);
        },
        .assign_packed_erased_fn => |assign| {
            noteDef(defs, assign.target, .fresh);
            if (assign.capture) |capture| noteDemand(demand, capture);
            try stack.append(allocator, assign.next);
        },
        .assign_low_level => |assign| {
            const args = store.getLocalSpan(assign.args);
            const borrow_source = lowLevelBorrowSource(rc_local, assign.rc_effect, args);
            if (assign.rc_effect.retain_result and borrow_source != no_local) {
                noteDef(defs, assign.target, .{ .borrow_capable = borrow_source });
            } else {
                noteDef(defs, assign.target, .fresh);
            }
            for (args, 0..) |arg, index| {
                if (index >= 64) {
                    noteDemand(demand, arg);
                    continue;
                }
                const bit = @as(u64, 1) << @as(u6, @intCast(index));
                if ((assign.rc_effect.consume_args & bit) != 0 or
                    (assign.rc_effect.retain_args & bit) != 0)
                {
                    noteDemand(demand, arg);
                }
            }
            try stack.append(allocator, assign.next);
        },
        .assign_list => |assign| {
            noteDef(defs, assign.target, .fresh);
            noteDemandSpan(store, demand, assign.elems);
            try stack.append(allocator, assign.next);
        },
        .assign_struct => |assign| {
            noteDef(defs, assign.target, .fresh);
            noteDemandSpan(store, demand, assign.fields);
            try stack.append(allocator, assign.next);
        },
        .assign_tag => |assign| {
            noteDef(defs, assign.target, .fresh);
            if (assign.payload) |payload| noteDemand(demand, payload);
            try stack.append(allocator, assign.next);
        },
        .set_local => |assign| {
            noteDef(defs, assign.target, .fresh);
            if (assign.target != assign.value) noteDemand(demand, assign.value);
            try stack.append(allocator, assign.next);
        },
        .debug => |debug_stmt| try stack.append(allocator, debug_stmt.next),
        .expect => |expect_stmt| try stack.append(allocator, expect_stmt.next),
        .incref => |rc| try stack.append(allocator, rc.next),
        .decref => |rc| try stack.append(allocator, rc.next),
        .free => |rc| try stack.append(allocator, rc.next),
        .switch_stmt => |switch_stmt| {
            for (store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                try stack.append(allocator, branch.body);
            }
            try stack.append(allocator, switch_stmt.default_branch);
            if (switch_stmt.continuation) |continuation| {
                try stack.append(allocator, continuation);
            }
        },
        .join => |join_stmt| {
            // Join parameters are written at every jump; they stay owned.
            for (store.getLocalSpan(join_stmt.params)) |param| {
                noteDef(defs, param, .multi);
            }
            try stack.append(allocator, join_stmt.body);
            try stack.append(allocator, join_stmt.remainder);
        },
        .ret => |ret_stmt| noteDemand(demand, ret_stmt.value),
        .jump, .crash, .runtime_error, .loop_continue, .loop_break => {},
    }
}

/// Returns the single refcounted argument named by `result_borrows_args`, or
/// `no_local` when the mask names zero or several refcounted arguments.
fn lowLevelBorrowSource(
    rc_local: []const bool,
    rc_effect: LIR.LowLevel.RcEffect,
    args: []const LIR.LocalId,
) u32 {
    if (rc_effect.result_borrows_args == 0) return no_local;
    var source: u32 = no_local;
    for (args, 0..) |arg, index| {
        if (index >= 64) break;
        const bit = @as(u64, 1) << @as(u6, @intCast(index));
        if ((rc_effect.result_borrows_args & bit) == 0) continue;
        const arg_index = @intFromEnum(arg);
        if (arg_index >= rc_local.len or !rc_local[arg_index]) continue;
        if (source != no_local and source != arg_index) return no_local;
        source = arg_index;
    }
    return source;
}

test "solve declarations are referenced" {
    std.testing.refAllDecls(@This());
}
