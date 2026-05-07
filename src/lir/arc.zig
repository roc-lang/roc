//! Mechanical ARC insertion for LIR.
//!
//! This pass is the only non-builtin stage that may synthesize explicit
//! baseline automatic `incref` and `decref` statements. `decref` owns ordinary
//! zero-count cleanup; backends consume explicit RC statements without doing
//! reference-counting analysis.

const std = @import("std");
const layout_mod = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

pub const ResourceError = std.mem.Allocator.Error;

/// Public `insert` function.
pub fn insert(store: *LirStore, layouts: *const layout_mod.Store) ResourceError!void {
    var inserter = Inserter{
        .store = store,
        .layouts = layouts,
    };

    for (store.proc_specs.items) |*proc| {
        const body = proc.body orelse continue;
        var join_bodies = JoinBodyMap.init(store.allocator);
        defer join_bodies.deinit();
        var join_visit = std.AutoHashMap(LIR.CFStmtId, void).init(store.allocator);
        defer join_visit.deinit();
        try inserter.collectJoinBodies(body, &join_bodies, &join_visit);
        inserter.join_bodies = &join_bodies;
        defer inserter.join_bodies = null;

        var owned = try OwnedSet.init(store.allocator, store.locals.items.len);
        defer owned.deinit();
        for (store.getLocalSpan(proc.args)) |param| {
            inserter.addOwnedIfRc(&owned, param);
        }
        proc.body = try inserter.rewritePath(body, &owned, .{});
    }
}

const RewriteOptions = struct {
    stop: ?LIR.CFStmtId = null,
    stop_replacement: ?LIR.CFStmtId = null,
    keep_at_stop: ?*const OwnedSet = null,
    loop_keep: ?*const OwnedSet = null,
};

const Inserter = struct {
    store: *LirStore,
    layouts: *const layout_mod.Store,
    join_bodies: ?*const JoinBodyMap = null,

    const CallArgOwnership = struct {
        retain_mask: u64 = 0,
        transfer_mask: u64 = 0,
    };

    fn rewritePath(self: *Inserter, start: LIR.CFStmtId, owned: *OwnedSet, options: RewriteOptions) ResourceError!LIR.CFStmtId {
        if (options.stop_replacement) |replacement| {
            if (start == replacement) return start;
        }
        if (options.stop) |stop| {
            if (start == stop) {
                const keep = options.keep_at_stop orelse arcInvariant("ARC stop reached without keep set");
                const replacement = options.stop_replacement orelse stop;
                return try self.releaseDifference(owned, keep, replacement);
            }
        }

        const stmt = self.store.getCFStmt(start);
        switch (stmt) {
            .assign_ref => |assign| {
                var current_start = start;
                current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                self.addOwnedIfRc(owned, assign.target);
                var next = try self.rewritePath(assign.next, owned, options);
                next = try self.retainLocalIfRc(assign.target, next);
                self.store.getCFStmtPtr(start).* = .{ .assign_ref = .{
                    .target = assign.target,
                    .op = assign.op,
                    .next = next,
                } };
                return current_start;
            },
            .assign_literal => |assign| {
                var current_start = start;
                current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                self.addOwnedIfRc(owned, assign.target);
                self.store.getCFStmtPtr(start).* = .{ .assign_literal = .{
                    .target = assign.target,
                    .value = assign.value,
                    .next = try self.rewritePath(assign.next, owned, options),
                } };
                return current_start;
            },
            .assign_call => |assign| {
                var current_start = start;
                const arg_ownership = try self.callArgOwnership(owned, assign.args, assign.next, assign.target, options.loop_keep);
                if (!self.spanUsesLocal(assign.args, assign.target)) {
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                }
                self.unsetMaskedArgs(owned, assign.args, arg_ownership.transfer_mask);
                self.addOwnedIfRc(owned, assign.target);
                const next = try self.rewritePath(assign.next, owned, options);
                self.store.getCFStmtPtr(start).* = .{ .assign_call = .{
                    .target = assign.target,
                    .proc = assign.proc,
                    .args = assign.args,
                    .next = next,
                } };
                current_start = try self.retainMaskedArgs(assign.args, arg_ownership.retain_mask, current_start);
                return current_start;
            },
            .assign_call_erased => |assign| {
                var current_start = start;
                current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                self.addOwnedIfRc(owned, assign.target);
                var next = try self.rewritePath(assign.next, owned, options);
                next = try self.releaseSpan(assign.args, next);
                next = try self.releaseLocalIfRc(assign.closure, next);
                self.store.getCFStmtPtr(start).* = .{ .assign_call_erased = .{
                    .target = assign.target,
                    .closure = assign.closure,
                    .args = assign.args,
                    .capture_layout = assign.capture_layout,
                    .next = next,
                } };
                current_start = try self.retainLocalIfRc(assign.closure, current_start);
                current_start = try self.retainSpan(assign.args, current_start);
                return current_start;
            },
            .assign_packed_erased_fn => |assign| {
                var current_start = start;
                current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                self.addOwnedIfRc(owned, assign.target);
                var next = try self.rewritePath(assign.next, owned, options);
                if (assign.capture) |capture| {
                    next = try self.retainLocalIfRc(capture, next);
                }
                self.store.getCFStmtPtr(start).* = .{ .assign_packed_erased_fn = .{
                    .target = assign.target,
                    .proc = assign.proc,
                    .capture = assign.capture,
                    .capture_layout = assign.capture_layout,
                    .next = next,
                } };
                return current_start;
            },
            .assign_low_level => |assign| {
                var current_start = start;
                if ((assign.rc_effect.result_aliases_consumed_args & ~assign.rc_effect.consume_args) != 0) {
                    arcInvariant("ARC low-level result-token metadata referenced a non-consumed argument");
                }
                const preserve_consumed_args = try self.preserveConsumedArgMask(
                    assign.args,
                    assign.rc_effect.consume_args,
                    assign.next,
                    assign.target,
                    options.loop_keep,
                );
                const target_consumed = self.maskedArgsContainLocal(assign.args, assign.rc_effect.consume_args, assign.target);
                if (target_consumed) {
                    owned.unset(assign.target);
                } else {
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                }
                if (assign.rc_effect.consume_args != 0) {
                    self.unsetMaskedArgsExcept(owned, assign.args, assign.rc_effect.consume_args & ~preserve_consumed_args, assign.target);
                }
                self.addOwnedIfRc(owned, assign.target);
                var next = try self.rewritePath(assign.next, owned, options);
                if (assign.rc_effect.retain_args != 0) {
                    next = try self.retainMaskedArgs(assign.args, assign.rc_effect.retain_args, next);
                }
                if (assign.rc_effect.retain_result) {
                    next = try self.retainLocalIfRc(assign.target, next);
                }
                self.store.getCFStmtPtr(start).* = .{ .assign_low_level = .{
                    .target = assign.target,
                    .op = assign.op,
                    .rc_effect = assign.rc_effect,
                    .args = assign.args,
                    .next = next,
                } };
                if (assign.rc_effect.consume_args != 0) {
                    current_start = try self.retainMaskedArgs(assign.args, preserve_consumed_args, current_start);
                }
                return current_start;
            },
            .assign_list => |assign| {
                var current_start = start;
                current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                self.addOwnedIfRc(owned, assign.target);
                var next = try self.rewritePath(assign.next, owned, options);
                next = try self.retainSpan(assign.elems, next);
                self.store.getCFStmtPtr(start).* = .{ .assign_list = .{
                    .target = assign.target,
                    .elems = assign.elems,
                    .next = next,
                } };
                return current_start;
            },
            .assign_struct => |assign| {
                var current_start = start;
                current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                self.addOwnedIfRc(owned, assign.target);
                var next = try self.rewritePath(assign.next, owned, options);
                next = try self.retainSpan(assign.fields, next);
                self.store.getCFStmtPtr(start).* = .{ .assign_struct = .{
                    .target = assign.target,
                    .fields = assign.fields,
                    .next = next,
                } };
                return current_start;
            },
            .assign_tag => |assign| {
                var current_start = start;
                current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                self.addOwnedIfRc(owned, assign.target);
                var next = try self.rewritePath(assign.next, owned, options);
                if (assign.payload) |payload| {
                    next = try self.retainLocalIfRc(payload, next);
                }
                self.store.getCFStmtPtr(start).* = .{ .assign_tag = .{
                    .target = assign.target,
                    .discriminant = assign.discriminant,
                    .payload = assign.payload,
                    .next = next,
                } };
                return current_start;
            },
            .set_local => |assign| {
                if (assign.target == assign.value) {
                    self.store.getCFStmtPtr(start).* = .{ .set_local = .{
                        .target = assign.target,
                        .value = assign.value,
                        .mode = assign.mode,
                        .next = try self.rewritePath(assign.next, owned, options),
                    } };
                    return start;
                }

                var current_start = start;
                switch (assign.mode) {
                    .replace_existing => current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start),
                    .initialize_join_result, .initialize_join_param => {},
                }
                self.addOwnedIfRc(owned, assign.target);
                var next = try self.rewritePath(assign.next, owned, options);
                next = try self.retainLocalIfRc(assign.target, next);
                self.store.getCFStmtPtr(start).* = .{ .set_local = .{
                    .target = assign.target,
                    .value = assign.value,
                    .mode = assign.mode,
                    .next = next,
                } };
                return current_start;
            },
            .debug => |debug_stmt| {
                self.store.getCFStmtPtr(start).* = .{ .debug = .{
                    .message = debug_stmt.message,
                    .next = try self.rewritePath(debug_stmt.next, owned, options),
                } };
                return start;
            },
            .expect => |expect_stmt| {
                self.store.getCFStmtPtr(start).* = .{ .expect = .{
                    .condition = expect_stmt.condition,
                    .next = try self.rewritePath(expect_stmt.next, owned, options),
                } };
                return start;
            },
            .runtime_error => return try self.releaseAll(owned, start),
            .incref => |rc| {
                if (options.stop_replacement == null) arcInvariant("ARC insertion received already-reference-counted LIR");
                self.addOwnedIfRc(owned, rc.value);
                self.store.getCFStmtPtr(start).* = .{ .incref = .{
                    .value = rc.value,
                    .count = rc.count,
                    .next = try self.rewritePath(rc.next, owned, options),
                } };
                return start;
            },
            .decref => |rc| {
                if (options.stop_replacement == null) arcInvariant("ARC insertion received already-reference-counted LIR");
                owned.unset(rc.value);
                self.store.getCFStmtPtr(start).* = .{ .decref = .{
                    .value = rc.value,
                    .next = try self.rewritePath(rc.next, owned, options),
                } };
                return start;
            },
            .free => |rc| {
                if (options.stop_replacement == null) arcInvariant("ARC insertion received already-reference-counted LIR");
                owned.unset(rc.value);
                self.store.getCFStmtPtr(start).* = .{ .free = .{
                    .value = rc.value,
                    .next = try self.rewritePath(rc.next, owned, options),
                } };
                return start;
            },
            .switch_stmt => |switch_stmt| return try self.rewriteSwitch(start, switch_stmt, owned, options),
            .for_list => |for_stmt| return try self.rewriteForList(start, for_stmt, owned, options),
            .loop_continue => {
                if (options.loop_keep) |keep| return try self.releaseDifference(owned, keep, start);
                return start;
            },
            .loop_break => {
                if (options.loop_keep) |keep| return try self.releaseDifference(owned, keep, start);
                return start;
            },
            .join => |join_stmt| {
                var loop_keep = try owned.clone();
                defer loop_keep.deinit();
                const body = try self.rewritePath(join_stmt.body, owned, .{
                    .stop = options.stop,
                    .stop_replacement = options.stop_replacement,
                    .keep_at_stop = options.keep_at_stop,
                    .loop_keep = &loop_keep,
                });
                var remainder_owned = try loop_keep.clone();
                defer remainder_owned.deinit();
                const remainder = try self.rewritePath(join_stmt.remainder, &remainder_owned, .{
                    .stop = options.stop,
                    .stop_replacement = options.stop_replacement,
                    .keep_at_stop = options.keep_at_stop,
                    .loop_keep = &loop_keep,
                });
                self.store.getCFStmtPtr(start).* = .{ .join = .{
                    .id = join_stmt.id,
                    .params = join_stmt.params,
                    .body = body,
                    .remainder = remainder,
                } };
                return start;
            },
            .jump => |jump_stmt| {
                if (self.store.getLocalSpan(jump_stmt.args).len != 0) {
                    arcInvariant("ARC insertion reached join arguments before explicit join-parameter ARC lowering");
                }
                if (options.loop_keep) |keep| return try self.releaseDifference(owned, keep, start);
                return start;
            },
            .ret => |ret_stmt| {
                var next = start;
                next = try self.releaseAll(owned, next);
                next = try self.retainLocalIfRc(ret_stmt.value, next);
                return next;
            },
            .crash => return try self.releaseAll(owned, start),
        }
    }

    fn rewriteSwitch(
        self: *Inserter,
        start: LIR.CFStmtId,
        switch_stmt: anytype,
        owned: *OwnedSet,
        options: RewriteOptions,
    ) ResourceError!LIR.CFStmtId {
        if (switch_stmt.continuation) |continuation| {
            var exit_states = std.ArrayList(OwnedSet).empty;
            defer {
                for (exit_states.items) |*state| state.deinit();
                exit_states.deinit(self.store.allocator);
            }

            for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                var branch_owned = try owned.clone();
                defer branch_owned.deinit();
                try self.analyzeUntil(branch.body, &branch_owned, continuation, &exit_states, options.loop_keep);
            }
            var default_owned = try owned.clone();
            defer default_owned.deinit();
            try self.analyzeUntil(switch_stmt.default_branch, &default_owned, continuation, &exit_states, options.loop_keep);

            if (exit_states.items.len == 0) {
                const branches = self.store.getCFSwitchBranchesMut(switch_stmt.branches);
                for (branches) |*branch| {
                    var branch_owned = try owned.clone();
                    defer branch_owned.deinit();
                    branch.body = try self.rewritePath(branch.body, &branch_owned, options);
                }
                var default_owned_terminal = try owned.clone();
                defer default_owned_terminal.deinit();
                self.store.getCFStmtPtr(start).* = .{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = switch_stmt.branches,
                    .default_branch = try self.rewritePath(switch_stmt.default_branch, &default_owned_terminal, options),
                    .continuation = switch_stmt.continuation,
                } };
                return start;
            }

            var common = try exit_states.items[0].clone();
            defer common.deinit();
            for (exit_states.items[1..]) |*state| {
                common.intersect(state);
            }

            var continuation_owned = try common.clone();
            defer continuation_owned.deinit();
            const rewritten_continuation = try self.rewritePath(continuation, &continuation_owned, options);

            const branches = self.store.getCFSwitchBranchesMut(switch_stmt.branches);
            for (branches) |*branch| {
                var branch_owned = try owned.clone();
                defer branch_owned.deinit();
                branch.body = try self.rewritePath(branch.body, &branch_owned, .{
                    .stop = continuation,
                    .stop_replacement = rewritten_continuation,
                    .keep_at_stop = &common,
                    .loop_keep = options.loop_keep,
                });
            }
            var default_owned_rewrite = try owned.clone();
            defer default_owned_rewrite.deinit();
            const default_branch = try self.rewritePath(switch_stmt.default_branch, &default_owned_rewrite, .{
                .stop = continuation,
                .stop_replacement = rewritten_continuation,
                .keep_at_stop = &common,
                .loop_keep = options.loop_keep,
            });

            self.store.getCFStmtPtr(start).* = .{ .switch_stmt = .{
                .cond = switch_stmt.cond,
                .branches = switch_stmt.branches,
                .default_branch = default_branch,
                .continuation = rewritten_continuation,
            } };
            return start;
        }

        const branches = self.store.getCFSwitchBranchesMut(switch_stmt.branches);
        for (branches) |*branch| {
            var branch_owned = try owned.clone();
            defer branch_owned.deinit();
            branch.body = try self.rewritePath(branch.body, &branch_owned, options);
        }
        var default_owned = try owned.clone();
        defer default_owned.deinit();
        self.store.getCFStmtPtr(start).* = .{ .switch_stmt = .{
            .cond = switch_stmt.cond,
            .branches = switch_stmt.branches,
            .default_branch = try self.rewritePath(switch_stmt.default_branch, &default_owned, options),
            .continuation = null,
        } };
        return start;
    }

    fn rewriteForList(
        self: *Inserter,
        start: LIR.CFStmtId,
        for_stmt: anytype,
        owned: *OwnedSet,
        options: RewriteOptions,
    ) ResourceError!LIR.CFStmtId {
        if (for_stmt.elem_source != .aliases_iterable_element) {
            arcInvariant("ARC insertion reached unknown for_list element ARC source");
        }

        var loop_keep = try owned.clone();
        defer loop_keep.deinit();
        var body_owned = try loop_keep.clone();
        defer body_owned.deinit();
        const body = try self.rewritePath(for_stmt.body, &body_owned, .{
            .stop = options.stop,
            .stop_replacement = options.stop_replacement,
            .keep_at_stop = options.keep_at_stop,
            .loop_keep = &loop_keep,
        });

        self.store.getCFStmtPtr(start).* = .{ .for_list = .{
            .elem = for_stmt.elem,
            .elem_source = for_stmt.elem_source,
            .iterable = for_stmt.iterable,
            .iterable_elem_layout = for_stmt.iterable_elem_layout,
            .body = body,
            .next = try self.rewritePath(for_stmt.next, owned, options),
        } };
        return start;
    }

    fn analyzeUntil(
        self: *Inserter,
        start: LIR.CFStmtId,
        owned: *OwnedSet,
        stop: LIR.CFStmtId,
        exits: *std.ArrayList(OwnedSet),
        loop_keep: ?*const OwnedSet,
    ) ResourceError!void {
        if (start == stop) {
            try exits.append(self.store.allocator, try owned.clone());
            return;
        }

        const stmt = self.store.getCFStmt(start);
        switch (stmt) {
            .assign_ref => |assign| {
                self.addOwnedIfRc(owned, assign.target);
                try self.analyzeUntil(assign.next, owned, stop, exits, loop_keep);
            },
            .assign_literal => |assign| {
                self.addOwnedIfRc(owned, assign.target);
                try self.analyzeUntil(assign.next, owned, stop, exits, loop_keep);
            },
            .assign_call => |assign| {
                const arg_ownership = try self.callArgOwnership(owned, assign.args, assign.next, assign.target, loop_keep);
                self.unsetMaskedArgs(owned, assign.args, arg_ownership.transfer_mask);
                self.addOwnedIfRc(owned, assign.target);
                try self.analyzeUntil(assign.next, owned, stop, exits, loop_keep);
            },
            .assign_call_erased => |assign| {
                self.addOwnedIfRc(owned, assign.target);
                try self.analyzeUntil(assign.next, owned, stop, exits, loop_keep);
            },
            .assign_packed_erased_fn => |assign| {
                self.addOwnedIfRc(owned, assign.target);
                try self.analyzeUntil(assign.next, owned, stop, exits, loop_keep);
            },
            .assign_low_level => |assign| {
                const preserve_consumed_args = try self.preserveConsumedArgMask(
                    assign.args,
                    assign.rc_effect.consume_args,
                    assign.next,
                    assign.target,
                    loop_keep,
                );
                const target_consumed = self.maskedArgsContainLocal(assign.args, assign.rc_effect.consume_args, assign.target);
                if (target_consumed) {
                    owned.unset(assign.target);
                }
                self.unsetMaskedArgsExcept(owned, assign.args, assign.rc_effect.consume_args & ~preserve_consumed_args, assign.target);
                self.addOwnedIfRc(owned, assign.target);
                try self.analyzeUntil(assign.next, owned, stop, exits, loop_keep);
            },
            .assign_list => |assign| {
                self.addOwnedIfRc(owned, assign.target);
                try self.analyzeUntil(assign.next, owned, stop, exits, loop_keep);
            },
            .assign_struct => |assign| {
                self.addOwnedIfRc(owned, assign.target);
                try self.analyzeUntil(assign.next, owned, stop, exits, loop_keep);
            },
            .assign_tag => |assign| {
                self.addOwnedIfRc(owned, assign.target);
                try self.analyzeUntil(assign.next, owned, stop, exits, loop_keep);
            },
            .set_local => |assign| {
                self.addOwnedIfRc(owned, assign.target);
                try self.analyzeUntil(assign.next, owned, stop, exits, loop_keep);
            },
            .debug => |debug_stmt| try self.analyzeUntil(debug_stmt.next, owned, stop, exits, loop_keep),
            .expect => |expect_stmt| try self.analyzeUntil(expect_stmt.next, owned, stop, exits, loop_keep),
            .switch_stmt => |switch_stmt| {
                if (switch_stmt.continuation) |continuation| {
                    var switch_exits = std.ArrayList(OwnedSet).empty;
                    defer {
                        for (switch_exits.items) |*state| state.deinit();
                        switch_exits.deinit(self.store.allocator);
                    }
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        var branch_owned = try owned.clone();
                        defer branch_owned.deinit();
                        try self.analyzeUntil(branch.body, &branch_owned, continuation, &switch_exits, loop_keep);
                    }
                    var default_owned = try owned.clone();
                    defer default_owned.deinit();
                    try self.analyzeUntil(switch_stmt.default_branch, &default_owned, continuation, &switch_exits, loop_keep);
                    if (switch_exits.items.len == 0) return;
                    var common = try switch_exits.items[0].clone();
                    defer common.deinit();
                    for (switch_exits.items[1..]) |*state| common.intersect(state);
                    try self.analyzeUntil(continuation, &common, stop, exits, loop_keep);
                    return;
                }
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    var branch_owned = try owned.clone();
                    defer branch_owned.deinit();
                    try self.analyzeUntil(branch.body, &branch_owned, stop, exits, loop_keep);
                }
                var default_owned = try owned.clone();
                defer default_owned.deinit();
                try self.analyzeUntil(switch_stmt.default_branch, &default_owned, stop, exits, loop_keep);
            },
            .for_list => |for_stmt| try self.analyzeUntil(for_stmt.next, owned, stop, exits, loop_keep),
            .join => |join_stmt| try self.analyzeUntil(join_stmt.body, owned, stop, exits, loop_keep),
            .incref => |rc| {
                self.addOwnedIfRc(owned, rc.value);
                try self.analyzeUntil(rc.next, owned, stop, exits, loop_keep);
            },
            .decref => |rc| {
                owned.unset(rc.value);
                try self.analyzeUntil(rc.next, owned, stop, exits, loop_keep);
            },
            .free => |rc| {
                owned.unset(rc.value);
                try self.analyzeUntil(rc.next, owned, stop, exits, loop_keep);
            },
            .runtime_error, .loop_continue, .loop_break, .jump, .ret, .crash => {},
        }
    }

    fn addOwnedIfRc(self: *Inserter, owned: *OwnedSet, local: LIR.LocalId) void {
        if (self.localContainsRefcounted(local)) owned.set(local);
    }

    fn releaseOldTargetIfNeeded(self: *Inserter, target: LIR.LocalId, owned: *OwnedSet, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        if (!owned.contains(target)) return next;
        owned.unset(target);
        return try self.releaseLocalIfRc(target, next);
    }

    fn retainMaskedArgs(self: *Inserter, span: LIR.LocalSpan, mask: u64, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var current = next;
        const locals = self.store.getLocalSpan(span);
        var i = locals.len;
        while (i > 0) {
            i -= 1;
            if ((mask & argMaskBit(i)) != 0) {
                current = try self.retainLocalIfRc(locals[i], current);
            }
        }
        return current;
    }

    fn preserveConsumedArgMask(
        self: *Inserter,
        span: LIR.LocalSpan,
        mask: u64,
        next: LIR.CFStmtId,
        target: LIR.LocalId,
        loop_keep: ?*const OwnedSet,
    ) ResourceError!u64 {
        if (mask == 0) return 0;
        var preserve: u64 = 0;
        const locals = self.store.getLocalSpan(span);
        for (locals, 0..) |local, i| {
            const bit = argMaskBit(i);
            if ((mask & bit) == 0) continue;
            if (local == target) continue;
            if (try self.localUsedInPath(next, local, loop_keep)) {
                preserve |= bit;
            }
        }
        return preserve;
    }

    fn callArgOwnership(
        self: *Inserter,
        owned: *const OwnedSet,
        span: LIR.LocalSpan,
        next: LIR.CFStmtId,
        target: LIR.LocalId,
        loop_keep: ?*const OwnedSet,
    ) ResourceError!CallArgOwnership {
        var result = CallArgOwnership{};
        var transferred = try OwnedSet.init(self.store.allocator, owned.bits.len);
        defer transferred.deinit();

        const locals = self.store.getLocalSpan(span);
        for (locals, 0..) |local, i| {
            if (!self.localContainsRefcounted(local)) continue;

            const bit = argMaskBit(i);
            const used_after_call = local != target and try self.localUsedInPath(next, local, loop_keep);
            const can_transfer = owned.contains(local) and !used_after_call and !transferred.contains(local);

            if (can_transfer) {
                result.transfer_mask |= bit;
                transferred.set(local);
            } else {
                result.retain_mask |= bit;
            }
        }

        return result;
    }

    fn maskedArgsContainLocal(self: *Inserter, span: LIR.LocalSpan, mask: u64, needle: LIR.LocalId) bool {
        if (mask == 0) return false;
        const locals = self.store.getLocalSpan(span);
        for (locals, 0..) |local, i| {
            if ((mask & argMaskBit(i)) != 0 and local == needle) return true;
        }
        return false;
    }

    fn unsetMaskedArgsExcept(
        self: *Inserter,
        owned: *OwnedSet,
        span: LIR.LocalSpan,
        mask: u64,
        except: LIR.LocalId,
    ) void {
        if (mask == 0) return;
        const locals = self.store.getLocalSpan(span);
        for (locals, 0..) |local, i| {
            if ((mask & argMaskBit(i)) != 0 and local != except) {
                owned.unset(local);
            }
        }
    }

    fn unsetMaskedArgs(
        self: *Inserter,
        owned: *OwnedSet,
        span: LIR.LocalSpan,
        mask: u64,
    ) void {
        if (mask == 0) return;
        const locals = self.store.getLocalSpan(span);
        for (locals, 0..) |local, i| {
            if ((mask & argMaskBit(i)) != 0) {
                owned.unset(local);
            }
        }
    }

    fn collectJoinBodies(
        self: *Inserter,
        start: LIR.CFStmtId,
        join_bodies: *JoinBodyMap,
        visited: *std.AutoHashMap(LIR.CFStmtId, void),
    ) ResourceError!void {
        if (visited.contains(start)) return;
        try visited.put(start, {});

        const stmt = self.store.getCFStmt(start);
        switch (stmt) {
            .assign_ref => |assign| try self.collectJoinBodies(assign.next, join_bodies, visited),
            .assign_literal => |assign| try self.collectJoinBodies(assign.next, join_bodies, visited),
            .assign_call => |assign| try self.collectJoinBodies(assign.next, join_bodies, visited),
            .assign_call_erased => |assign| try self.collectJoinBodies(assign.next, join_bodies, visited),
            .assign_packed_erased_fn => |assign| try self.collectJoinBodies(assign.next, join_bodies, visited),
            .assign_low_level => |assign| try self.collectJoinBodies(assign.next, join_bodies, visited),
            .assign_list => |assign| try self.collectJoinBodies(assign.next, join_bodies, visited),
            .assign_struct => |assign| try self.collectJoinBodies(assign.next, join_bodies, visited),
            .assign_tag => |assign| try self.collectJoinBodies(assign.next, join_bodies, visited),
            .set_local => |assign| try self.collectJoinBodies(assign.next, join_bodies, visited),
            .debug => |debug_stmt| try self.collectJoinBodies(debug_stmt.next, join_bodies, visited),
            .expect => |expect_stmt| try self.collectJoinBodies(expect_stmt.next, join_bodies, visited),
            .switch_stmt => |switch_stmt| {
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.collectJoinBodies(branch.body, join_bodies, visited);
                }
                try self.collectJoinBodies(switch_stmt.default_branch, join_bodies, visited);
                if (switch_stmt.continuation) |continuation| {
                    try self.collectJoinBodies(continuation, join_bodies, visited);
                }
            },
            .for_list => |for_stmt| {
                try self.collectJoinBodies(for_stmt.body, join_bodies, visited);
                try self.collectJoinBodies(for_stmt.next, join_bodies, visited);
            },
            .join => |join_stmt| {
                const previous = try join_bodies.getOrPut(join_stmt.id);
                if (previous.found_existing and previous.value_ptr.* != join_stmt.body) {
                    arcInvariant("ARC join-body collection saw one join id with multiple bodies");
                }
                previous.value_ptr.* = join_stmt.body;
                try self.collectJoinBodies(join_stmt.body, join_bodies, visited);
                try self.collectJoinBodies(join_stmt.remainder, join_bodies, visited);
            },
            .jump,
            .ret,
            .runtime_error,
            .loop_continue,
            .loop_break,
            .crash,
            => {},
            .incref, .decref, .free => arcInvariant("ARC join-body collection received already-reference-counted LIR"),
        }
    }

    fn localUsedInPath(
        self: *Inserter,
        start: LIR.CFStmtId,
        needle: LIR.LocalId,
        loop_keep: ?*const OwnedSet,
    ) ResourceError!bool {
        var visited = std.AutoHashMap(LIR.CFStmtId, void).init(self.store.allocator);
        defer visited.deinit();
        return try self.localUsedInPathInner(start, needle, loop_keep, &visited);
    }

    fn localUsedInPathInner(
        self: *Inserter,
        start: LIR.CFStmtId,
        needle: LIR.LocalId,
        loop_keep: ?*const OwnedSet,
        visited: *std.AutoHashMap(LIR.CFStmtId, void),
    ) ResourceError!bool {
        if (visited.contains(start)) return false;
        try visited.put(start, {});

        const stmt = self.store.getCFStmt(start);
        return switch (stmt) {
            .assign_ref => |assign| (refOpUsesLocal(assign.op, needle) or
                try self.localUsedInPathInner(assign.next, needle, loop_keep, visited)),
            .assign_literal => |assign| try self.localUsedInPathInner(assign.next, needle, loop_keep, visited),
            .assign_call => |assign| (self.spanUsesLocal(assign.args, needle) or
                try self.localUsedInPathInner(assign.next, needle, loop_keep, visited)),
            .assign_call_erased => |assign| (assign.closure == needle or
                self.spanUsesLocal(assign.args, needle) or
                try self.localUsedInPathInner(assign.next, needle, loop_keep, visited)),
            .assign_packed_erased_fn => |assign| ((assign.capture != null and assign.capture.? == needle) or
                try self.localUsedInPathInner(assign.next, needle, loop_keep, visited)),
            .assign_low_level => |assign| (self.spanUsesLocal(assign.args, needle) or
                try self.localUsedInPathInner(assign.next, needle, loop_keep, visited)),
            .assign_list => |assign| (self.spanUsesLocal(assign.elems, needle) or
                try self.localUsedInPathInner(assign.next, needle, loop_keep, visited)),
            .assign_struct => |assign| (self.spanUsesLocal(assign.fields, needle) or
                try self.localUsedInPathInner(assign.next, needle, loop_keep, visited)),
            .assign_tag => |assign| ((assign.payload != null and assign.payload.? == needle) or
                try self.localUsedInPathInner(assign.next, needle, loop_keep, visited)),
            .set_local => |assign| (assign.value == needle or
                try self.localUsedInPathInner(assign.next, needle, loop_keep, visited)),
            .debug => |debug_stmt| (debug_stmt.message == needle or
                try self.localUsedInPathInner(debug_stmt.next, needle, loop_keep, visited)),
            .expect => |expect_stmt| (expect_stmt.condition == needle or
                try self.localUsedInPathInner(expect_stmt.next, needle, loop_keep, visited)),
            .switch_stmt => |switch_stmt| blk: {
                if (switch_stmt.cond == needle) break :blk true;
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    if (try self.localUsedInPathInner(branch.body, needle, loop_keep, visited)) break :blk true;
                }
                if (try self.localUsedInPathInner(switch_stmt.default_branch, needle, loop_keep, visited)) break :blk true;
                if (switch_stmt.continuation) |continuation| {
                    if (try self.localUsedInPathInner(continuation, needle, loop_keep, visited)) break :blk true;
                }
                break :blk false;
            },
            .for_list => |for_stmt| (for_stmt.iterable == needle or
                try self.localUsedInPathInner(for_stmt.body, needle, loop_keep, visited) or
                try self.localUsedInPathInner(for_stmt.next, needle, loop_keep, visited)),
            .join => |join_stmt| (try self.localUsedInPathInner(join_stmt.body, needle, loop_keep, visited) or
                try self.localUsedInPathInner(join_stmt.remainder, needle, loop_keep, visited)),
            .jump => |jump_stmt| blk: {
                if (self.spanUsesLocal(jump_stmt.args, needle)) break :blk true;
                const join_bodies = self.join_bodies orelse arcInvariant("ARC liveness reached jump without collected join bodies");
                const target_body = join_bodies.get(jump_stmt.target) orelse arcInvariant("ARC liveness reached jump to unknown join point");
                break :blk try self.localUsedInPathInner(target_body, needle, loop_keep, visited);
            },
            .ret => |ret_stmt| ret_stmt.value == needle,
            .loop_continue,
            .loop_break,
            => if (loop_keep) |keep| keep.contains(needle) else false,
            .runtime_error,
            .crash,
            => false,
            .incref => |rc| try self.localUsedInPathInner(rc.next, needle, loop_keep, visited),
            .decref => |rc| try self.localUsedInPathInner(rc.next, needle, loop_keep, visited),
            .free => |rc| try self.localUsedInPathInner(rc.next, needle, loop_keep, visited),
        };
    }

    fn spanUsesLocal(self: *Inserter, span: LIR.LocalSpan, needle: LIR.LocalId) bool {
        for (self.store.getLocalSpan(span)) |local| {
            if (local == needle) return true;
        }
        return false;
    }

    fn retainSpan(self: *Inserter, span: LIR.LocalSpan, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var current = next;
        const locals = self.store.getLocalSpan(span);
        var i = locals.len;
        while (i > 0) {
            i -= 1;
            current = try self.retainLocalIfRc(locals[i], current);
        }
        return current;
    }

    fn releaseSpan(self: *Inserter, span: LIR.LocalSpan, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var current = next;
        const locals = self.store.getLocalSpan(span);
        var i = locals.len;
        while (i > 0) {
            i -= 1;
            current = try self.releaseLocalIfRc(locals[i], current);
        }
        return current;
    }

    fn releaseAll(self: *Inserter, owned: *const OwnedSet, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var keep = try OwnedSet.init(self.store.allocator, owned.bits.len);
        defer keep.deinit();
        return try self.releaseDifference(owned, &keep, next);
    }

    fn releaseDifference(self: *Inserter, owned: *const OwnedSet, keep: *const OwnedSet, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var current = next;
        var i = owned.bits.len;
        while (i > 0) {
            i -= 1;
            if (!owned.bits[i] or keep.bits[i]) continue;
            const local: LIR.LocalId = @enumFromInt(@as(u32, @intCast(i)));
            current = try self.releaseLocalIfRc(local, current);
        }
        return current;
    }

    fn retainLocalIfRc(self: *Inserter, local: LIR.LocalId, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        if (!self.localContainsRefcounted(local)) return next;
        return try self.store.addCFStmt(.{ .incref = .{
            .value = local,
            .count = 1,
            .next = next,
        } });
    }

    fn releaseLocalIfRc(self: *Inserter, local: LIR.LocalId, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        if (!self.localContainsRefcounted(local)) return next;
        return try self.store.addCFStmt(.{ .decref = .{
            .value = local,
            .next = next,
        } });
    }

    fn localContainsRefcounted(self: *const Inserter, local: LIR.LocalId) bool {
        const layout_idx = self.store.getLocal(local).layout_idx;
        return self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx));
    }
};

const JoinBodyMap = std.AutoHashMap(LIR.JoinPointId, LIR.CFStmtId);

const OwnedSet = struct {
    allocator: std.mem.Allocator,
    bits: []bool,

    fn init(allocator: std.mem.Allocator, len: usize) ResourceError!OwnedSet {
        const bits = try allocator.alloc(bool, len);
        @memset(bits, false);
        return .{ .allocator = allocator, .bits = bits };
    }

    fn deinit(self: *OwnedSet) void {
        self.allocator.free(self.bits);
        self.bits = &.{};
    }

    fn clone(self: *const OwnedSet) ResourceError!OwnedSet {
        const bits = try self.allocator.dupe(bool, self.bits);
        return .{ .allocator = self.allocator, .bits = bits };
    }

    fn set(self: *OwnedSet, local: LIR.LocalId) void {
        self.bits[@intFromEnum(local)] = true;
    }

    fn unset(self: *OwnedSet, local: LIR.LocalId) void {
        self.bits[@intFromEnum(local)] = false;
    }

    fn contains(self: *const OwnedSet, local: LIR.LocalId) bool {
        return self.bits[@intFromEnum(local)];
    }

    fn intersect(self: *OwnedSet, other: *const OwnedSet) void {
        if (self.bits.len != other.bits.len) arcInvariant("ARC owned-set intersection length mismatch");
        for (self.bits, other.bits) |*bit, other_bit| {
            bit.* = bit.* and other_bit;
        }
    }
};

fn refOpUsesLocal(op: LIR.RefOp, needle: LIR.LocalId) bool {
    return switch (op) {
        .local => |local| local == needle,
        .discriminant => |ref| ref.source == needle,
        .field => |ref| ref.source == needle,
        .tag_payload => |ref| ref.source == needle,
        .tag_payload_struct => |ref| ref.source == needle,
        .list_reinterpret => |ref| ref.backing_ref == needle,
        .nominal => |ref| ref.backing_ref == needle,
    };
}

fn argMaskBit(index: usize) u64 {
    if (index >= 64) arcInvariant("ARC low-level runtime mutation argument mask exceeded 64 args");
    return @as(u64, 1) << @as(u6, @intCast(index));
}

fn arcInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

test "arc insertion boundary exists" {
    std.testing.refAllDecls(@This());
}
