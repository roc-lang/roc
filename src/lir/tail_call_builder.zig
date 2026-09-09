//! Procedure-local return-continuation proofs owned by LIR construction.
//!
//! Statement emission records self calls and join definitions. Finalization
//! resolves only those calls' forwarding continuations, memoizing shared
//! suffixes. No control-flow walk, path enumeration, or proof budget is needed.
const std = @import("std");
const collections = @import("collections");
const LIR = @import("LIR.zig");

const Self = @This();
const Proof = union(enum) { visiting, done: ?LIR.LocalId };

allocator: std.mem.Allocator,
proc: ?LIR.LirProcSpecId = null,
joins: collections.DenseMap(LIR.JoinPointId, LIR.CFStmtId),
calls: std.ArrayList(LIR.CFStmtId) = .empty,
proofs: collections.DenseMap(LIR.CFStmtId, Proof),
path: std.ArrayList(LIR.CFStmtId) = .empty,
next_join: u32 = 0,
published: bool = false,
/// Final producer-selected adaptation plans, borrowed during finalization.
adapters: []const @import("program.zig").BoxyAdapter = &.{},

/// Start recording one exact procedure identity.
pub fn init(allocator: std.mem.Allocator, proc: LIR.LirProcSpecId) Self {
    var self = initScratch(allocator);
    self.reset(proc);
    return self;
}

/// Allocate reusable scratch before a procedure is selected. `reset` must
/// supply its exact identity before recording or publishing statements.
pub fn initScratch(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .joins = .init(allocator),
        .proofs = .init(allocator),
    };
}

/// Release the procedure-local proof scratch.
pub fn deinit(self: *Self) void {
    self.joins.deinit();
    self.calls.deinit(self.allocator);
    self.proofs.deinit();
    self.path.deinit(self.allocator);
}

/// Reuse the same worker-local buffers for the next procedure.
pub fn reset(self: *Self, proc: LIR.LirProcSpecId) void {
    self.proc = proc;
    self.joins.clearRetainingCapacity();
    self.calls.clearRetainingCapacity();
    self.proofs.clearRetainingCapacity();
    self.path.clearRetainingCapacity();
    self.next_join = 0;
    self.published = false;
    self.adapters = &.{};
}

/// Called by the statement producer, including when filling a placeholder.
pub fn record(self: *Self, id: LIR.CFStmtId, stmt: LIR.CFStmt) std.mem.Allocator.Error!void {
    std.debug.assert(!self.published);
    const proc = self.proc.?;
    switch (stmt) {
        .join => |join| {
            try self.joins.put(join.id, join.body);
            self.next_join = @max(self.next_join, @intFromEnum(join.id) + 1);
        },
        .assign_call => |call| if (call.proc == proc) {
            try self.calls.append(self.allocator, id);
        },
        .init_uninitialized,
        .assign_ref,
        .assign_literal,
        .assign_call_erased,
        .assign_packed_erased_fn,
        .assign_boxy_desc_ref,
        .assign_boxy_dict_ref,
        .assign_boxy_box,
        .assign_boxy_reuse_box,
        .assign_boxy_unbox,
        .assign_boxy_adapt,
        .assign_boxy_inspect,
        .assign_boxy_eq,
        .assign_boxy_tag,
        .assign_boxy_tag_payload,
        .boxy_tag_match,
        .assign_call_dict,
        .assign_low_level,
        .assign_list,
        .assign_struct,
        .assign_tag,
        .store_struct,
        .store_tag,
        .set_local,
        .debug,
        .expect,
        .expect_err,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .comptime_branch_taken,
        .incref,
        .decref,
        .decref_if_initialized,
        .free,
        .switch_stmt,
        .switch_initialized_payload,
        .str_match,
        .str_match_set,
        .loop_continue,
        .loop_break,
        .jump,
        .ret,
        .crash,
        => {},
    }
}

/// Publish the exact self-tail sites after all producer fixups are complete.
/// The linked list lives in the call nodes, so body-shard relocation carries
/// the proof along with the calls without another store-wide side table.
pub fn finish(self: *Self, store: anytype) std.mem.Allocator.Error!?LIR.TailCalls {
    std.debug.assert(!self.published);
    const proc = self.proc.?;
    self.published = true;
    var head: ?LIR.CFStmtId = null;
    for (self.calls.items) |id| {
        const stmt = store.getCFStmt(id);
        // A producer can move a provisional call and retire its old node.
        if (stmt != .assign_call) continue;
        const call = stmt.assign_call;
        std.debug.assert(call.proc == proc);
        if (call.tail_call != null) continue;
        // A runtime result descriptor is another call output; forwarding the
        // value alone does not establish that descriptor's return contract.
        if (call.out_desc != null) continue;
        if (try self.returnedLocal(store, call.next)) |returned| {
            if (returned != call.target) continue;
            store.getCFStmtPtr(id).assign_call.tail_call = .{ .next = head };
            head = id;
        }
    }
    return if (head) |first| .{ .head = first, .loop = @enumFromInt(self.next_join) } else null;
}

fn successor(self: *const Self, stmt: LIR.CFStmt) ?LIR.CFStmtId {
    return switch (stmt) {
        .jump => |jump| self.joins.get(jump.target) orelse unreachable,
        .join => |join| join.remainder,
        .assign_ref => |assign| switch (assign.op) {
            .local, .nominal, .list_reinterpret => assign.next,
            .discriminant,
            .field,
            .tag_payload,
            .tag_payload_struct,
            => null,
        },
        .assign_boxy_adapt => |assign| if (assign.source_mode == .move and
            self.adapters[@intFromEnum(assign.adapter)].operation == .relabel)
            assign.next
        else
            null,
        .set_local => |assign| if (assign.mode == .initialize_join_param) assign.next else null,
        .init_uninitialized,
        .assign_literal,
        .assign_call,
        .assign_call_erased,
        .assign_packed_erased_fn,
        .assign_boxy_desc_ref,
        .assign_boxy_dict_ref,
        .assign_boxy_box,
        .assign_boxy_reuse_box,
        .assign_boxy_unbox,
        .assign_boxy_inspect,
        .assign_boxy_eq,
        .assign_boxy_tag,
        .assign_boxy_tag_payload,
        .boxy_tag_match,
        .assign_call_dict,
        .assign_low_level,
        .assign_list,
        .assign_struct,
        .assign_tag,
        .store_struct,
        .store_tag,
        .debug,
        .expect,
        .expect_err,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .comptime_branch_taken,
        .incref,
        .decref,
        .decref_if_initialized,
        .free,
        .switch_stmt,
        .switch_initialized_payload,
        .str_match,
        .str_match_set,
        .loop_continue,
        .loop_break,
        .ret,
        .crash,
        => null,
    };
}

fn returnedLocal(self: *Self, store: anytype, start: LIR.CFStmtId) std.mem.Allocator.Error!?LIR.LocalId {
    self.path.clearRetainingCapacity();
    var current = start;
    var result: ?LIR.LocalId = while (true) {
        if (self.proofs.get(current)) |proof| break switch (proof) {
            // A forwarding cycle never returns. It is not a return proof.
            .visiting => null,
            .done => |local| local,
        };
        const stmt = store.getCFStmt(current);
        if (stmt == .ret) break stmt.ret.value;
        const next = self.successor(stmt) orelse break null;
        try self.proofs.put(current, .visiting);
        try self.path.append(self.allocator, current);
        current = next;
    };
    while (self.path.pop()) |id| {
        if (result) |local| {
            switch (store.getCFStmt(id)) {
                .assign_ref => |assign| {
                    result = if (assign.target != local) null else switch (assign.op) {
                        .local => |source| source,
                        .nominal => |nominal| nominal.backing_ref,
                        .list_reinterpret => |list| list.backing_ref,
                        .discriminant,
                        .field,
                        .tag_payload,
                        .tag_payload_struct,
                        => unreachable,
                    };
                },
                .set_local => |assign| result = if (assign.target == local) assign.value else null,
                .assign_boxy_adapt => |assign| result = if (assign.target == local) assign.source else null,
                .join, .jump => {},
                .init_uninitialized,
                .assign_literal,
                .assign_call,
                .assign_call_erased,
                .assign_packed_erased_fn,
                .assign_boxy_desc_ref,
                .assign_boxy_dict_ref,
                .assign_boxy_box,
                .assign_boxy_reuse_box,
                .assign_boxy_unbox,
                .assign_boxy_inspect,
                .assign_boxy_eq,
                .assign_boxy_tag,
                .assign_boxy_tag_payload,
                .boxy_tag_match,
                .assign_call_dict,
                .assign_low_level,
                .assign_list,
                .assign_struct,
                .assign_tag,
                .store_struct,
                .store_tag,
                .debug,
                .expect,
                .expect_err,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .comptime_branch_taken,
                .incref,
                .decref,
                .decref_if_initialized,
                .free,
                .switch_stmt,
                .switch_initialized_payload,
                .str_match,
                .str_match_set,
                .loop_continue,
                .loop_break,
                .ret,
                .crash,
                => unreachable,
            }
        }
        self.proofs.getPtr(id).?.* = .{ .done = result };
    }
    return result;
}
