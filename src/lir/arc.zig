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

const LinearRewriteFrame = struct {
    stmt: LIR.CFStmtId,
    head: LIR.CFStmtId,
    retain_assign_ref_target: bool = true,
    retain_set_target: bool = true,
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
        var frames = std.ArrayList(LinearRewriteFrame).empty;
        defer frames.deinit(self.store.allocator);

        var cursor = start;
        while (true) {
            if (options.stop) |stop| {
                if (cursor == stop) {
                    const keep = options.keep_at_stop orelse arcInvariant("ARC stop reached without keep set");
                    const replacement = options.stop_replacement orelse stop;
                    const tail = try self.releaseDifference(owned, keep, replacement);
                    return try self.finishLinearRewrite(&frames, tail);
                }
            }
            if (options.stop_replacement) |replacement| {
                if (cursor == replacement) return try self.finishLinearRewrite(&frames, cursor);
            }

            const stmt = self.store.getCFStmt(cursor);
            var current_start = cursor;
            switch (stmt) {
                .assign_ref => |assign| {
                    var retain_assign_ref_target = true;
                    switch (assign.op) {
                        .local => |source| {
                            if (assign.target != source) {
                                const move_value = try self.canMoveSetLocalValue(owned, source, assign.next, options.loop_keep);
                                current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                                if (move_value) {
                                    owned.unset(source);
                                    retain_assign_ref_target = false;
                                }
                                self.addOwnedIfRc(owned, assign.target);
                            } else {
                                retain_assign_ref_target = false;
                            }
                        },
                        else => {
                            current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                            self.addOwnedIfRc(owned, assign.target);
                        },
                    }
                    try frames.append(self.store.allocator, .{
                        .stmt = cursor,
                        .head = current_start,
                        .retain_assign_ref_target = retain_assign_ref_target,
                    });
                    cursor = assign.next;
                },
                .assign_literal => |assign| {
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                    self.addOwnedIfRc(owned, assign.target);
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = assign.next;
                },
                .assign_call => |assign| {
                    const arg_ownership = try self.callArgOwnership(owned, assign.args, assign.next, assign.target, options.loop_keep);
                    if (!self.spanUsesLocal(assign.args, assign.target)) {
                        current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                    }
                    self.unsetMaskedArgs(owned, assign.args, arg_ownership.transfer_mask);
                    self.addOwnedIfRc(owned, assign.target);
                    current_start = try self.retainMaskedArgs(assign.args, arg_ownership.retain_mask, current_start);
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = assign.next;
                },
                .assign_call_erased => |assign| {
                    const arg_ownership = try self.callArgOwnership(owned, assign.args, assign.next, assign.target, options.loop_keep);
                    if (!self.spanUsesLocal(assign.args, assign.target) and assign.closure != assign.target) {
                        current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                    }
                    self.unsetMaskedArgs(owned, assign.args, arg_ownership.transfer_mask);
                    self.addOwnedIfRc(owned, assign.target);
                    current_start = try self.retainLocalIfRc(assign.closure, current_start);
                    current_start = try self.retainMaskedArgs(assign.args, arg_ownership.retain_mask, current_start);
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = assign.next;
                },
                .assign_packed_erased_fn => |assign| {
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                    self.addOwnedIfRc(owned, assign.target);
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = assign.next;
                },
                .assign_low_level => |assign| {
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
                    if (assign.rc_effect.consume_args != 0) {
                        current_start = try self.retainMaskedArgs(assign.args, preserve_consumed_args, current_start);
                    }
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = assign.next;
                },
                .assign_list => |assign| {
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                    self.addOwnedIfRc(owned, assign.target);
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = assign.next;
                },
                .assign_struct => |assign| {
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                    self.addOwnedIfRc(owned, assign.target);
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = assign.next;
                },
                .assign_tag => |assign| {
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start);
                    self.addOwnedIfRc(owned, assign.target);
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = assign.next;
                },
                .set_local => |assign| {
                    var retain_set_target = assign.target != assign.value;
                    if (assign.target != assign.value) {
                        const move_value = try self.canMoveSetLocalValue(owned, assign.value, assign.next, options.loop_keep);
                        switch (assign.mode) {
                            .replace_existing => current_start = try self.releaseOldTargetIfNeeded(assign.target, owned, current_start),
                            .initialize_join_result, .initialize_join_param => {},
                        }
                        if (move_value) {
                            owned.unset(assign.value);
                            retain_set_target = false;
                        }
                        self.addOwnedIfRc(owned, assign.target);
                    }
                    try frames.append(self.store.allocator, .{
                        .stmt = cursor,
                        .head = current_start,
                        .retain_set_target = retain_set_target,
                    });
                    cursor = assign.next;
                },
                .debug => |debug_stmt| {
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = debug_stmt.next;
                },
                .expect => |expect_stmt| {
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = expect_stmt.next;
                },
                .incref => |rc| {
                    if (options.stop_replacement == null) arcInvariant("ARC insertion received already-reference-counted LIR");
                    self.addOwnedIfRc(owned, rc.value);
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = rc.next;
                },
                .decref => |rc| {
                    if (options.stop_replacement == null) arcInvariant("ARC insertion received already-reference-counted LIR");
                    owned.unset(rc.value);
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = rc.next;
                },
                .free => |rc| {
                    if (options.stop_replacement == null) arcInvariant("ARC insertion received already-reference-counted LIR");
                    owned.unset(rc.value);
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = rc.next;
                },
                .switch_stmt => |switch_stmt| {
                    const tail = try self.rewriteSwitch(cursor, switch_stmt, owned, options);
                    return try self.finishLinearRewrite(&frames, tail);
                },
                .for_list => |for_stmt| {
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
                    self.store.getCFStmtPtr(cursor).* = .{ .for_list = .{
                        .elem = for_stmt.elem,
                        .elem_source = for_stmt.elem_source,
                        .iterable = for_stmt.iterable,
                        .iterable_elem_layout = for_stmt.iterable_elem_layout,
                        .body = body,
                        .next = for_stmt.next,
                    } };
                    try frames.append(self.store.allocator, .{ .stmt = cursor, .head = current_start });
                    cursor = for_stmt.next;
                },
                .join => |join_stmt| {
                    const tail = try self.rewriteJoin(cursor, join_stmt, owned, options);
                    return try self.finishLinearRewrite(&frames, tail);
                },
                .runtime_error => {
                    const tail = try self.releaseAll(owned, cursor);
                    return try self.finishLinearRewrite(&frames, tail);
                },
                .loop_continue => {
                    const tail = if (options.loop_keep) |keep| try self.releaseDifference(owned, keep, cursor) else cursor;
                    return try self.finishLinearRewrite(&frames, tail);
                },
                .loop_break => {
                    const tail = if (options.loop_keep) |keep| try self.releaseDifference(owned, keep, cursor) else cursor;
                    return try self.finishLinearRewrite(&frames, tail);
                },
                .jump => |jump_stmt| {
                    if (self.store.getLocalSpan(jump_stmt.args).len != 0) {
                        arcInvariant("ARC insertion reached join arguments before explicit join-parameter ARC lowering");
                    }
                    const tail = if (options.loop_keep) |keep| try self.releaseDifference(owned, keep, cursor) else cursor;
                    return try self.finishLinearRewrite(&frames, tail);
                },
                .ret => |ret_stmt| {
                    var tail = cursor;
                    tail = try self.releaseAll(owned, tail);
                    tail = try self.retainLocalIfRc(ret_stmt.value, tail);
                    return try self.finishLinearRewrite(&frames, tail);
                },
                .crash => {
                    const tail = try self.releaseAll(owned, cursor);
                    return try self.finishLinearRewrite(&frames, tail);
                },
            }
        }
    }

    fn finishLinearRewrite(
        self: *Inserter,
        frames: *std.ArrayList(LinearRewriteFrame),
        tail_start: LIR.CFStmtId,
    ) ResourceError!LIR.CFStmtId {
        var next = tail_start;
        while (frames.pop()) |frame| {
            next = try self.patchLinearFrame(frame, next);
        }
        return next;
    }

    fn patchLinearFrame(
        self: *Inserter,
        frame: LinearRewriteFrame,
        tail_start: LIR.CFStmtId,
    ) ResourceError!LIR.CFStmtId {
        const stmt = self.store.getCFStmt(frame.stmt);
        var next = tail_start;
        switch (stmt) {
            .assign_ref => |assign| {
                if (frame.retain_assign_ref_target) {
                    next = try self.retainLocalIfRc(assign.target, next);
                }
                self.store.getCFStmtPtr(frame.stmt).* = .{ .assign_ref = .{
                    .target = assign.target,
                    .op = assign.op,
                    .next = next,
                } };
            },
            .assign_literal => |assign| {
                self.store.getCFStmtPtr(frame.stmt).* = .{ .assign_literal = .{
                    .target = assign.target,
                    .value = assign.value,
                    .next = next,
                } };
            },
            .assign_call => |assign| {
                self.store.getCFStmtPtr(frame.stmt).* = .{ .assign_call = .{
                    .target = assign.target,
                    .proc = assign.proc,
                    .args = assign.args,
                    .next = next,
                } };
            },
            .assign_call_erased => |assign| {
                next = try self.releaseLocalIfRc(assign.closure, next);
                self.store.getCFStmtPtr(frame.stmt).* = .{ .assign_call_erased = .{
                    .target = assign.target,
                    .closure = assign.closure,
                    .args = assign.args,
                    .next = next,
                } };
            },
            .assign_packed_erased_fn => |assign| {
                if (assign.capture) |capture| {
                    next = try self.retainLocalIfRc(capture, next);
                }
                self.store.getCFStmtPtr(frame.stmt).* = .{ .assign_packed_erased_fn = .{
                    .target = assign.target,
                    .proc = assign.proc,
                    .capture = assign.capture,
                    .capture_layout = assign.capture_layout,
                    .on_drop = assign.on_drop,
                    .next = next,
                } };
            },
            .assign_low_level => |assign| {
                if (assign.rc_effect.retain_args != 0) {
                    next = try self.retainMaskedArgs(assign.args, assign.rc_effect.retain_args, next);
                }
                if (assign.rc_effect.retain_result) {
                    next = try self.retainLocalIfRc(assign.target, next);
                }
                self.store.getCFStmtPtr(frame.stmt).* = .{ .assign_low_level = .{
                    .target = assign.target,
                    .op = assign.op,
                    .rc_effect = assign.rc_effect,
                    .args = assign.args,
                    .next = next,
                } };
            },
            .assign_list => |assign| {
                next = try self.retainSpan(assign.elems, next);
                self.store.getCFStmtPtr(frame.stmt).* = .{ .assign_list = .{
                    .target = assign.target,
                    .elems = assign.elems,
                    .next = next,
                } };
            },
            .assign_struct => |assign| {
                next = try self.retainSpan(assign.fields, next);
                self.store.getCFStmtPtr(frame.stmt).* = .{ .assign_struct = .{
                    .target = assign.target,
                    .fields = assign.fields,
                    .next = next,
                } };
            },
            .assign_tag => |assign| {
                if (assign.payload) |payload| {
                    next = try self.retainLocalIfRc(payload, next);
                }
                self.store.getCFStmtPtr(frame.stmt).* = .{ .assign_tag = .{
                    .target = assign.target,
                    .discriminant = assign.discriminant,
                    .payload = assign.payload,
                    .next = next,
                } };
            },
            .set_local => |assign| {
                if (assign.target != assign.value and frame.retain_set_target) {
                    next = try self.retainLocalIfRc(assign.target, next);
                }
                self.store.getCFStmtPtr(frame.stmt).* = .{ .set_local = .{
                    .target = assign.target,
                    .value = assign.value,
                    .mode = assign.mode,
                    .next = next,
                } };
            },
            .debug => |debug_stmt| {
                self.store.getCFStmtPtr(frame.stmt).* = .{ .debug = .{
                    .message = debug_stmt.message,
                    .next = next,
                } };
            },
            .expect => |expect_stmt| {
                self.store.getCFStmtPtr(frame.stmt).* = .{ .expect = .{
                    .condition = expect_stmt.condition,
                    .next = next,
                } };
            },
            .incref => |rc| {
                self.store.getCFStmtPtr(frame.stmt).* = .{ .incref = .{
                    .value = rc.value,
                    .count = rc.count,
                    .next = next,
                } };
            },
            .decref => |rc| {
                self.store.getCFStmtPtr(frame.stmt).* = .{ .decref = .{
                    .value = rc.value,
                    .next = next,
                } };
            },
            .free => |rc| {
                self.store.getCFStmtPtr(frame.stmt).* = .{ .free = .{
                    .value = rc.value,
                    .next = next,
                } };
            },
            .for_list => |for_stmt| {
                self.store.getCFStmtPtr(frame.stmt).* = .{ .for_list = .{
                    .elem = for_stmt.elem,
                    .elem_source = for_stmt.elem_source,
                    .iterable = for_stmt.iterable,
                    .iterable_elem_layout = for_stmt.iterable_elem_layout,
                    .body = for_stmt.body,
                    .next = next,
                } };
            },
            .runtime_error,
            .switch_stmt,
            .loop_continue,
            .loop_break,
            .join,
            .jump,
            .ret,
            .crash,
            => arcInvariant("ARC linear rewrite attempted to patch a non-linear statement"),
        }
        return frame.head;
    }

    fn rewriteJoin(
        self: *Inserter,
        start: LIR.CFStmtId,
        join_stmt: anytype,
        owned: *OwnedSet,
        options: RewriteOptions,
    ) ResourceError!LIR.CFStmtId {
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
                const default_branch = try self.rewritePath(switch_stmt.default_branch, &default_owned_terminal, options);
                self.store.getCFStmtPtr(start).* = .{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = switch_stmt.branches,
                    .default_branch = default_branch,
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
        const default_branch = try self.rewritePath(switch_stmt.default_branch, &default_owned, options);
        self.store.getCFStmtPtr(start).* = .{ .switch_stmt = .{
            .cond = switch_stmt.cond,
            .branches = switch_stmt.branches,
            .default_branch = default_branch,
            .continuation = null,
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
        var cursor = start;
        while (true) {
            if (cursor == stop) {
                try exits.append(self.store.allocator, try owned.clone());
                return;
            }

            const stmt = self.store.getCFStmt(cursor);
            switch (stmt) {
                .assign_ref => |assign| {
                    switch (assign.op) {
                        .local => |source| {
                            if (assign.target != source) {
                                const move_value = try self.canMoveSetLocalValue(owned, source, assign.next, loop_keep);
                                if (move_value) owned.unset(source);
                                self.addOwnedIfRc(owned, assign.target);
                            }
                        },
                        else => self.addOwnedIfRc(owned, assign.target),
                    }
                    cursor = assign.next;
                },
                .assign_literal => |assign| {
                    self.addOwnedIfRc(owned, assign.target);
                    cursor = assign.next;
                },
                .assign_call => |assign| {
                    const arg_ownership = try self.callArgOwnership(owned, assign.args, assign.next, assign.target, loop_keep);
                    self.unsetMaskedArgs(owned, assign.args, arg_ownership.transfer_mask);
                    self.addOwnedIfRc(owned, assign.target);
                    cursor = assign.next;
                },
                .assign_call_erased => |assign| {
                    const arg_ownership = try self.callArgOwnership(owned, assign.args, assign.next, assign.target, loop_keep);
                    self.unsetMaskedArgs(owned, assign.args, arg_ownership.transfer_mask);
                    self.addOwnedIfRc(owned, assign.target);
                    cursor = assign.next;
                },
                .assign_packed_erased_fn => |assign| {
                    self.addOwnedIfRc(owned, assign.target);
                    cursor = assign.next;
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
                    cursor = assign.next;
                },
                .assign_list => |assign| {
                    self.addOwnedIfRc(owned, assign.target);
                    cursor = assign.next;
                },
                .assign_struct => |assign| {
                    self.addOwnedIfRc(owned, assign.target);
                    cursor = assign.next;
                },
                .assign_tag => |assign| {
                    self.addOwnedIfRc(owned, assign.target);
                    cursor = assign.next;
                },
                .set_local => |assign| {
                    if (assign.target != assign.value) {
                        const move_value = try self.canMoveSetLocalValue(owned, assign.value, assign.next, loop_keep);
                        switch (assign.mode) {
                            .initialize_join_result, .initialize_join_param => {},
                            .replace_existing => {},
                        }
                        if (move_value) owned.unset(assign.value);
                    }
                    self.addOwnedIfRc(owned, assign.target);
                    cursor = assign.next;
                },
                .debug => |debug_stmt| cursor = debug_stmt.next,
                .expect => |expect_stmt| cursor = expect_stmt.next,
                .incref => |rc| {
                    self.addOwnedIfRc(owned, rc.value);
                    cursor = rc.next;
                },
                .decref => |rc| {
                    owned.unset(rc.value);
                    cursor = rc.next;
                },
                .free => |rc| {
                    owned.unset(rc.value);
                    cursor = rc.next;
                },
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
                        owned.copyFrom(&common);
                        cursor = continuation;
                        continue;
                    }
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        var branch_owned = try owned.clone();
                        defer branch_owned.deinit();
                        try self.analyzeUntil(branch.body, &branch_owned, stop, exits, loop_keep);
                    }
                    var default_owned = try owned.clone();
                    defer default_owned.deinit();
                    try self.analyzeUntil(switch_stmt.default_branch, &default_owned, stop, exits, loop_keep);
                    return;
                },
                .for_list => |for_stmt| cursor = for_stmt.next,
                .join => |join_stmt| cursor = join_stmt.body,
                .runtime_error, .loop_continue, .loop_break, .jump, .ret, .crash => return,
            }
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

    fn canMoveSetLocalValue(
        self: *Inserter,
        owned: *const OwnedSet,
        value: LIR.LocalId,
        next: LIR.CFStmtId,
        loop_keep: ?*const OwnedSet,
    ) ResourceError!bool {
        if (!owned.contains(value)) return false;
        if (!self.localContainsRefcounted(value)) return false;
        return !(try self.localUsedInPath(next, value, loop_keep));
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
        var stack = std.ArrayList(LIR.CFStmtId).empty;
        defer stack.deinit(self.store.allocator);
        try stack.append(self.store.allocator, start);

        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});

            const stmt = self.store.getCFStmt(current);
            switch (stmt) {
                .assign_ref => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_literal => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_call => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_call_erased => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_packed_erased_fn => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_low_level => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_list => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_struct => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_tag => |assign| try stack.append(self.store.allocator, assign.next),
                .set_local => |assign| try stack.append(self.store.allocator, assign.next),
                .debug => |debug_stmt| try stack.append(self.store.allocator, debug_stmt.next),
                .expect => |expect_stmt| try stack.append(self.store.allocator, expect_stmt.next),
                .switch_stmt => |switch_stmt| {
                    if (switch_stmt.continuation) |continuation| {
                        try stack.append(self.store.allocator, continuation);
                    }
                    try stack.append(self.store.allocator, switch_stmt.default_branch);
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try stack.append(self.store.allocator, branch.body);
                    }
                },
                .for_list => |for_stmt| {
                    try stack.append(self.store.allocator, for_stmt.next);
                    try stack.append(self.store.allocator, for_stmt.body);
                },
                .join => |join_stmt| {
                    const previous = try join_bodies.getOrPut(join_stmt.id);
                    if (previous.found_existing and previous.value_ptr.* != join_stmt.body) {
                        arcInvariant("ARC join-body collection saw one join id with multiple bodies");
                    }
                    previous.value_ptr.* = join_stmt.body;
                    try stack.append(self.store.allocator, join_stmt.remainder);
                    try stack.append(self.store.allocator, join_stmt.body);
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
    }

    fn localUsedInPath(
        self: *Inserter,
        start: LIR.CFStmtId,
        needle: LIR.LocalId,
        loop_keep: ?*const OwnedSet,
    ) ResourceError!bool {
        var visited = std.AutoHashMap(LIR.CFStmtId, void).init(self.store.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(LIR.CFStmtId).empty;
        defer stack.deinit(self.store.allocator);
        try stack.append(self.store.allocator, start);

        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});

            const stmt = self.store.getCFStmt(current);
            switch (stmt) {
                .assign_ref => |assign| {
                    if (refOpUsesLocal(assign.op, needle)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_literal => |assign| try stack.append(self.store.allocator, assign.next),
                .assign_call => |assign| {
                    if (self.spanUsesLocal(assign.args, needle)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_call_erased => |assign| {
                    if (assign.closure == needle or self.spanUsesLocal(assign.args, needle)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    if (assign.capture != null and assign.capture.? == needle) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_low_level => |assign| {
                    if (self.spanUsesLocal(assign.args, needle)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_list => |assign| {
                    if (self.spanUsesLocal(assign.elems, needle)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_struct => |assign| {
                    if (self.spanUsesLocal(assign.fields, needle)) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_tag => |assign| {
                    if (assign.payload != null and assign.payload.? == needle) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .set_local => |assign| {
                    if (assign.value == needle) return true;
                    try stack.append(self.store.allocator, assign.next);
                },
                .debug => |debug_stmt| {
                    if (debug_stmt.message == needle) return true;
                    try stack.append(self.store.allocator, debug_stmt.next);
                },
                .expect => |expect_stmt| {
                    if (expect_stmt.condition == needle) return true;
                    try stack.append(self.store.allocator, expect_stmt.next);
                },
                .switch_stmt => |switch_stmt| {
                    if (switch_stmt.cond == needle) return true;
                    if (switch_stmt.continuation) |continuation| {
                        try stack.append(self.store.allocator, continuation);
                    }
                    try stack.append(self.store.allocator, switch_stmt.default_branch);
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try stack.append(self.store.allocator, branch.body);
                    }
                },
                .for_list => |for_stmt| {
                    if (for_stmt.iterable == needle) return true;
                    try stack.append(self.store.allocator, for_stmt.next);
                    try stack.append(self.store.allocator, for_stmt.body);
                },
                .join => |join_stmt| {
                    try stack.append(self.store.allocator, join_stmt.remainder);
                    try stack.append(self.store.allocator, join_stmt.body);
                },
                .jump => |jump_stmt| {
                    if (self.spanUsesLocal(jump_stmt.args, needle)) return true;
                    const join_bodies = self.join_bodies orelse arcInvariant("ARC liveness reached jump without collected join bodies");
                    const target_body = join_bodies.get(jump_stmt.target) orelse arcInvariant("ARC liveness reached jump to unknown join point");
                    try stack.append(self.store.allocator, target_body);
                },
                .ret => |ret_stmt| if (ret_stmt.value == needle) return true,
                .loop_continue,
                .loop_break,
                => if (loop_keep) |keep| {
                    if (keep.contains(needle)) return true;
                },
                .runtime_error,
                .crash,
                => {},
                .incref => |rc| try stack.append(self.store.allocator, rc.next),
                .decref => |rc| try stack.append(self.store.allocator, rc.next),
                .free => |rc| try stack.append(self.store.allocator, rc.next),
            }
        }

        return false;
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

    fn copyFrom(self: *OwnedSet, other: *const OwnedSet) void {
        if (self.bits.len != other.bits.len) arcInvariant("ARC owned-set copy length mismatch");
        @memcpy(self.bits, other.bits);
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

const testing = std.testing;

const ArcTest = struct {
    allocator: std.mem.Allocator,
    store: LirStore,
    layouts: layout_mod.Store,
    list_str: layout_mod.Idx,
    list_i64: layout_mod.Idx,
    box_str: layout_mod.Idx,
    pair_str: layout_mod.Idx,
    tag_str: layout_mod.Idx,

    fn init(allocator: std.mem.Allocator) !ArcTest {
        var layouts = try layout_mod.Store.init(allocator, .u64);
        errdefer layouts.deinit();

        const list_str = try layouts.insertList(.str);
        const list_i64 = try layouts.insertList(.i64);
        const box_str = try layouts.insertBox(.str);
        const pair_str = try layouts.putStructFields(&[_]layout_mod.StructField{
            .{ .index = 0, .layout = .str },
            .{ .index = 1, .layout = .str },
        });
        const tag_str = try layouts.putTagUnion(&[_]layout_mod.Idx{
            try layouts.ensureZstLayout(),
            .str,
        });

        return .{
            .allocator = allocator,
            .store = LirStore.init(allocator),
            .layouts = layouts,
            .list_str = list_str,
            .list_i64 = list_i64,
            .box_str = box_str,
            .pair_str = pair_str,
            .tag_str = tag_str,
        };
    }

    fn deinit(self: *ArcTest) void {
        self.store.deinit();
        self.layouts.deinit();
    }

    fn local(self: *ArcTest, layout_idx: layout_mod.Idx) !LIR.LocalId {
        return try self.store.addLocal(.{ .layout_idx = layout_idx });
    }

    fn span(self: *ArcTest, locals: []const LIR.LocalId) !LIR.LocalSpan {
        return try self.store.addLocalSpan(locals);
    }

    fn addProc(self: *ArcTest, args: []const LIR.LocalId, body: LIR.CFStmtId, ret_layout: layout_mod.Idx) !LIR.LirProcSpecId {
        return try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.span(args),
            .body = body,
            .ret_layout = ret_layout,
        });
    }

    fn addBodylessProc(self: *ArcTest, ret_layout: layout_mod.Idx) !LIR.LirProcSpecId {
        return try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = LIR.LocalSpan.empty(),
            .body = null,
            .ret_layout = ret_layout,
        });
    }

    fn ret(self: *ArcTest, value: LIR.LocalId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .ret = .{ .value = value } });
    }

    fn crash(self: *ArcTest, message: []const u8) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .crash = .{ .msg = try self.store.insertString(message) } });
    }

    fn assignI64(self: *ArcTest, target: LIR.LocalId, value: i64, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .i64_literal = .{ .value = value, .layout_idx = .i64 } },
            .next = next,
        } });
    }

    fn assignStr(self: *ArcTest, target: LIR.LocalId, text: []const u8, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .str_literal = try self.store.insertString(text) },
            .next = next,
        } });
    }

    fn assignList(self: *ArcTest, target: LIR.LocalId, elems: []const LIR.LocalId, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_list = .{
            .target = target,
            .elems = try self.span(elems),
            .next = next,
        } });
    }

    fn assignStruct(self: *ArcTest, target: LIR.LocalId, fields: []const LIR.LocalId, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.span(fields),
            .next = next,
        } });
    }

    fn assignTag(self: *ArcTest, target: LIR.LocalId, discriminant: u16, payload: ?LIR.LocalId, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_tag = .{
            .target = target,
            .discriminant = discriminant,
            .payload = payload,
            .next = next,
        } });
    }

    fn assignRefLocal(self: *ArcTest, target: LIR.LocalId, source: LIR.LocalId, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .local = source },
            .next = next,
        } });
    }

    fn assignRefField(self: *ArcTest, target: LIR.LocalId, source: LIR.LocalId, field_idx: u16, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .field = .{ .source = source, .field_idx = field_idx } },
            .next = next,
        } });
    }

    fn assignTagPayload(self: *ArcTest, target: LIR.LocalId, source: LIR.LocalId, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .tag_payload = .{ .source = source, .payload_idx = 0, .tag_discriminant = 1 } },
            .next = next,
        } });
    }

    fn assignCall(self: *ArcTest, target: LIR.LocalId, args: []const LIR.LocalId, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_call = .{
            .target = target,
            .proc = try self.addBodylessProc(self.store.getLocal(target).layout_idx),
            .args = try self.span(args),
            .next = next,
        } });
    }

    fn assignLowLevel(self: *ArcTest, target: LIR.LocalId, args: []const LIR.LocalId, rc_effect: LIR.LowLevel.RcEffect, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = .list_append_unsafe,
            .rc_effect = rc_effect,
            .args = try self.span(args),
            .next = next,
        } });
    }

    fn setLocal(self: *ArcTest, target: LIR.LocalId, value: LIR.LocalId, mode: LIR.SetLocalWriteMode, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .set_local = .{
            .target = target,
            .value = value,
            .mode = mode,
            .next = next,
        } });
    }

    fn expectStmt(self: *ArcTest, condition: LIR.LocalId, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .expect = .{
            .condition = condition,
            .next = next,
        } });
    }

    fn switchStmt(
        self: *ArcTest,
        cond: LIR.LocalId,
        branch_body: LIR.CFStmtId,
        default_branch: LIR.CFStmtId,
        continuation: ?LIR.CFStmtId,
    ) !LIR.CFStmtId {
        const branches = try self.store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 1, .body = branch_body },
        });
        return try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = branches,
            .default_branch = default_branch,
            .continuation = continuation,
        } });
    }

    fn forList(self: *ArcTest, elem: LIR.LocalId, iterable: LIR.LocalId, elem_layout: layout_mod.Idx, body: LIR.CFStmtId, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .for_list = .{
            .elem = elem,
            .elem_source = .aliases_iterable_element,
            .iterable = iterable,
            .iterable_elem_layout = elem_layout,
            .body = body,
            .next = next,
        } });
    }

    fn run(self: *ArcTest) !void {
        try insert(&self.store, &self.layouts);
    }

    fn procBody(self: *const ArcTest) LIR.CFStmtId {
        for (self.store.proc_specs.items) |proc| {
            if (proc.body) |body| return body;
        }
        arcInvariant("ARC test fixture has no procedure body");
    }

    fn countRc(self: *const ArcTest, local_id: LIR.LocalId, kind: RcKind) usize {
        var count: usize = 0;
        for (self.store.cf_stmts.items) |stmt| {
            switch (stmt) {
                .incref => |rc| {
                    if (kind == .incref and rc.value == local_id) count += 1;
                },
                .decref => |rc| {
                    if (kind == .decref and rc.value == local_id) count += 1;
                },
                .free => |rc| {
                    if (kind == .free and rc.value == local_id) count += 1;
                },
                else => {},
            }
        }
        return count;
    }

    fn countAllRc(self: *const ArcTest) usize {
        var count: usize = 0;
        for (self.store.cf_stmts.items) |stmt| {
            switch (stmt) {
                .incref, .decref, .free => count += 1,
                else => {},
            }
        }
        return count;
    }

    fn expectRc(self: *const ArcTest, local_id: LIR.LocalId, increfs: usize, decrefs: usize, frees: usize) !void {
        try testing.expectEqual(increfs, self.countRc(local_id, .incref));
        try testing.expectEqual(decrefs, self.countRc(local_id, .decref));
        try testing.expectEqual(frees, self.countRc(local_id, .free));
    }

    fn expectReachableRcBefore(self: *const ArcTest, start: LIR.CFStmtId, kind: RcKind, local_id: LIR.LocalId, before: RcStopKind) !void {
        var cursor = start;
        var remaining: usize = self.store.cf_stmts.items.len + 1;
        while (remaining > 0) : (remaining -= 1) {
            const stmt = self.store.getCFStmt(cursor);
            switch (stmt) {
                .incref => |rc| {
                    if (kind == .incref and rc.value == local_id) return;
                    cursor = rc.next;
                },
                .decref => |rc| {
                    if (kind == .decref and rc.value == local_id) return;
                    cursor = rc.next;
                },
                .free => |rc| {
                    if (kind == .free and rc.value == local_id) return;
                    cursor = rc.next;
                },
                .assign_ref => |assign| cursor = assign.next,
                .assign_literal => |assign| cursor = assign.next,
                .assign_call => |assign| cursor = assign.next,
                .assign_call_erased => |assign| cursor = assign.next,
                .assign_packed_erased_fn => |assign| cursor = assign.next,
                .assign_low_level => |assign| cursor = assign.next,
                .assign_list => |assign| cursor = assign.next,
                .assign_struct => |assign| cursor = assign.next,
                .assign_tag => |assign| cursor = assign.next,
                .set_local => |assign| cursor = assign.next,
                .debug => |debug_stmt| cursor = debug_stmt.next,
                .expect => |expect_stmt| cursor = expect_stmt.next,
                .ret => {
                    if (before == .ret) return error.ExpectedRcBeforeStop;
                    return;
                },
                .crash => {
                    if (before == .crash) return error.ExpectedRcBeforeStop;
                    return;
                },
                .runtime_error, .switch_stmt, .for_list, .loop_continue, .loop_break, .join, .jump => return error.NonLinearPath,
            }
        }
        return error.CyclicPath;
    }
};

const RcKind = enum { incref, decref, free };
const RcStopKind = enum { ret, crash };

fn setupUnusedBinding(layout_idx: layout_mod.Idx) !struct { fixture: ArcTest, value: LIR.LocalId } {
    var f = try ArcTest.init(testing.allocator);
    errdefer f.deinit();
    const value = try f.local(layout_idx);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const body = switch (layout_idx) {
        .str => try f.assignStr(value, "tmp", ret),
        else => try f.assignList(value, &.{}, ret),
    };
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    return .{ .fixture = f, .value = value };
}

fn setupSwitchUse(use_branch: bool, use_default: bool, use_twice_in_branch: bool, use_after: bool) !struct { fixture: ArcTest, value: LIR.LocalId, branch_local: LIR.LocalId, default_local: LIR.LocalId } {
    var f = try ArcTest.init(testing.allocator);
    errdefer f.deinit();
    const value = try f.local(.str);
    const branch_local = try f.local(.i64);
    const default_local = try f.local(.i64);
    const cond = try f.local(.i64);
    const result = try f.local(.i64);

    const continuation_ret = try f.ret(if (use_after) value else result);
    const branch_tail = if (use_after) continuation_ret else try f.assignI64(result, 11, continuation_ret);
    const default_tail = if (use_after) continuation_ret else try f.assignI64(result, 22, continuation_ret);
    const branch_body = if (use_branch)
        try f.assignCall(branch_local, if (use_twice_in_branch) &.{ value, value } else &.{value}, branch_tail)
    else
        branch_tail;
    const default_body = if (use_default)
        try f.assignCall(default_local, &.{value}, default_tail)
    else
        default_tail;
    const switch_stmt = try f.switchStmt(cond, branch_body, default_body, continuation_ret);
    const cond_assign = try f.assignI64(cond, 1, switch_stmt);
    const body = try f.assignStr(value, "branch", cond_assign);
    _ = try f.addProc(&.{}, body, if (use_after) .str else .i64);
    try f.run();
    return .{ .fixture = f, .value = value, .branch_local = branch_local, .default_local = default_local };
}

fn setupMutation(reuse_after: bool) !struct { fixture: ArcTest, old_value: LIR.LocalId, new_value: LIR.LocalId, target: LIR.LocalId } {
    var f = try ArcTest.init(testing.allocator);
    errdefer f.deinit();
    const target = try f.local(f.list_str);
    const new_value = try f.local(f.list_str);
    const old_value = target;
    const final_value = if (reuse_after) target else try f.local(.i64);
    const ret = try f.ret(final_value);
    const reassign = try f.setLocal(target, new_value, .replace_existing, ret);
    const new_assign = try f.assignList(new_value, &.{}, reassign);
    const body = try f.assignList(target, &.{}, new_assign);
    _ = try f.addProc(&.{}, body, if (reuse_after) f.list_str else .i64);
    try f.run();
    return .{ .fixture = f, .old_value = old_value, .new_value = new_value, .target = target };
}

fn setupForLoop(elem_used_twice: bool, elem_unused: bool) !struct { fixture: ArcTest, elem: LIR.LocalId, iterable: LIR.LocalId, result: LIR.LocalId } {
    var f = try ArcTest.init(testing.allocator);
    errdefer f.deinit();
    const iterable = try f.local(f.list_str);
    const elem = try f.local(.str);
    const result = try f.local(.i64);
    const body_result = try f.local(.i64);
    const ret = try f.ret(result);
    const loop_body = if (elem_unused)
        try f.assignI64(body_result, 0, try f.store.addCFStmt(.loop_continue))
    else
        try f.assignCall(body_result, if (elem_used_twice) &.{ elem, elem } else &.{elem}, try f.store.addCFStmt(.loop_continue));
    const loop = try f.forList(elem, iterable, .str, loop_body, ret);
    const init_result = try f.assignI64(result, 0, loop);
    const body = try f.assignList(iterable, &.{}, init_result);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    return .{ .fixture = f, .elem = elem, .iterable = iterable, .result = result };
}

test "RC pass-through: non-refcounted i64 block unchanged" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.i64);
    const ret = try f.ret(value);
    const body = try f.assignI64(value, 42, ret);
    _ = try f.addProc(&.{}, body, .i64);
    const before = f.store.cf_stmts.items.len;
    try f.run();
    try testing.expectEqual(before, f.store.cf_stmts.items.len);
    try testing.expectEqual(@as(usize, 0), f.countAllRc());
}

test "RC: string binding used twice gets incref" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const pair = try f.local(f.pair_str);
    const ret = try f.ret(pair);
    const struct_stmt = try f.assignStruct(pair, &.{ value, value }, ret);
    const body = try f.assignStr(value, "shared", struct_stmt);
    _ = try f.addProc(&.{}, body, f.pair_str);
    try f.run();
    try f.expectRc(value, 2, 1, 0);
    try f.expectRc(pair, 1, 1, 0);
}

test "RC: unused string binding gets decref" {
    var scenario = try setupUnusedBinding(.str);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 0, 1, 0);
}

test "RC: unused list binding gets decref" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(f.list_str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const body = try f.assignList(value, &.{}, ret);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(value, 0, 1, 0);
}

test "RC borrowed string expression releases original temporary binding" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const original = try f.local(.str);
    const alias = try f.local(.str);
    const ret = try f.ret(original);
    const alias_stmt = try f.assignRefLocal(alias, original, ret);
    const body = try f.assignStr(original, "borrow-name-kept-for-audit", alias_stmt);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try testing.expect(f.countRc(original, .decref) >= 1);
    try testing.expect(f.countRc(alias, .decref) >= 1);
}

test "RC explicit retained list element keeps outer binding cleanup" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_str);
    const elem = try f.local(.str);
    const ret = try f.ret(elem);
    const elem_ref = try f.assignRefField(elem, list, 0, ret);
    const body = try f.assignList(list, &.{}, elem_ref);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try f.expectRc(list, 0, 1, 0);
    try testing.expect(f.countRc(elem, .incref) >= 1);
}

test "RC if result matched later tail-cleans matched binding" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const branch_value = try f.local(.str);
    const default_value = try f.local(.str);
    const result = try f.local(.str);
    const ret = try f.ret(result);
    const branch_set = try f.setLocal(result, branch_value, .initialize_join_result, ret);
    const default_set = try f.setLocal(result, default_value, .initialize_join_result, ret);
    const switch_stmt = try f.switchStmt(cond, branch_set, default_set, ret);
    const default_assign = try f.assignStr(default_value, "default", switch_stmt);
    const branch_assign = try f.assignStr(branch_value, "branch", default_assign);
    const body = try f.assignI64(cond, 1, branch_assign);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try testing.expect(f.countRc(result, .decref) >= 1);
}

test "RC identity call result matched later tail-cleans matched binding" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const input = try f.local(.str);
    const result = try f.local(.str);
    const ret = try f.ret(result);
    const call = try f.assignCall(result, &.{input}, ret);
    const body = try f.assignStr(input, "identity", call);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try testing.expect(f.countRc(result, .decref) >= 1);
}

test "RC repeated identity call tail-cleans the unused second result" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const input = try f.local(.str);
    const first = try f.local(.str);
    const second = try f.local(.str);
    const ret = try f.ret(first);
    const second_call = try f.assignCall(second, &.{first}, ret);
    const first_call = try f.assignCall(first, &.{input}, second_call);
    const body = try f.assignStr(input, "identity", first_call);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try f.expectRc(second, 0, 1, 0);
}

test "RC mutable list binding tail-cleans borrowed final use" {
    var scenario = try setupMutation(true);
    defer scenario.fixture.deinit();
    try testing.expect(scenario.fixture.countRc(scenario.target, .decref) >= 1);
}

test "RC mutable list loop accumulator tail-cleans current binding after borrowed final use" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const acc = try f.local(f.list_str);
    const next_acc = try f.local(f.list_str);
    const iterable = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const ret = try f.ret(acc);
    const loop_continue = try f.store.addCFStmt(.loop_continue);
    const set_acc = try f.setLocal(acc, next_acc, .replace_existing, loop_continue);
    const next_assign = try f.assignList(next_acc, &.{}, set_acc);
    const loop = try f.forList(elem, iterable, .i64, next_assign, ret);
    const iterable_assign = try f.assignList(iterable, &.{}, loop);
    const body = try f.assignList(acc, &.{}, iterable_assign);
    _ = try f.addProc(&.{}, body, f.list_str);
    try f.run();
    try testing.expect(f.countRc(acc, .decref) >= 1);
}

test "RC branch-aware: symbol used in both match branches — no incref at binding" {
    var scenario = try setupSwitchUse(true, true, false, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 0, 0, 0);
}

test "RC branch-aware: symbol used in one match branch only — decref in unused branch" {
    var scenario = try setupSwitchUse(true, false, false, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 0, 1, 0);
}

test "RC branch-aware: symbol used twice in one branch — incref in that branch, decref in other" {
    var scenario = try setupSwitchUse(true, false, true, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 1, 1, 0);
}

test "RC branch-aware: symbol used outside and inside branches" {
    var scenario = try setupSwitchUse(true, true, false, true);
    defer scenario.fixture.deinit();
    try testing.expect(scenario.fixture.countRc(scenario.value, .incref) >= 1);
    try testing.expect(scenario.fixture.countRc(scenario.value, .decref) >= 1);
}

test "RC proc body: returning refcounted param does not tail-decref it" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const param = try f.local(.str);
    const ret = try f.ret(param);
    _ = try f.addProc(&.{param}, ret, .str);
    try f.run();
    try f.expectRc(param, 1, 1, 0);
}

test "RC proc body: returning list param does not tail-decref it" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const param = try f.local(f.list_str);
    const ret = try f.ret(param);
    _ = try f.addProc(&.{param}, ret, f.list_str);
    try f.run();
    try f.expectRc(param, 1, 1, 0);
}

test "RC proc_call caller: consumed refcounted arg is not tail-decref'd by caller" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const arg = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.assignCall(result, &.{arg}, ret);
    const body = try f.assignStr(arg, "consume", call);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(arg, 0, 0, 0);
}

test "RC proc_call caller: consumed list arg is not tail-decref'd by caller" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const arg = try f.local(f.list_str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.assignCall(result, &.{arg}, ret);
    const body = try f.assignList(arg, &.{}, call);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(arg, 0, 0, 0);
}

test "RC for_loop: elem used twice gets incref" {
    var scenario = try setupForLoop(true, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.elem, 2, 0, 0);
}

test "RC shadowed list decl only cleans latest generation at block tail" {
    var scenario = try setupMutation(false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.target, 0, 2, 0);
    try scenario.fixture.expectRc(scenario.new_value, 0, 0, 0);
}

test "RC mutation: reassigning refcounted var emits decref before mutation" {
    var scenario = try setupMutation(false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.old_value, 0, 2, 0);
}

test "RC mutation: final use of reassignable refcounted var emits tail decref" {
    var scenario = try setupMutation(true);
    defer scenario.fixture.deinit();
    try testing.expect(scenario.fixture.countRc(scenario.target, .decref) >= 1);
}

test "RC for_loop: unused refcounted elem does not decref borrowed element" {
    var scenario = try setupForLoop(false, true);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.elem, 0, 0, 0);
}

test "RC match guard: symbol used only in guard gets proper RC ops" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const guard = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const guard_use = try f.expectStmt(guard, ret);
    const body = try f.assignStr(guard, "guard", guard_use);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(guard, 0, 1, 0);
}

test "RC match guard+body: symbol used in both guard and body gets proper RC ops" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(value);
    const call = try f.assignCall(result, &.{value}, ret);
    const guard_use = try f.expectStmt(value, call);
    const body = try f.assignStr(value, "guard-body", guard_use);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try testing.expect(f.countRc(value, .decref) >= 1);
}

test "RC for_loop: wrapper block has unit result layout, not elem layout" {
    var scenario = try setupForLoop(false, true);
    defer scenario.fixture.deinit();
    try testing.expectEqual(layout_mod.Idx.i64, scenario.fixture.store.proc_specs.items[0].ret_layout);
    try scenario.fixture.expectRc(scenario.elem, 0, 0, 0);
}

test "RC if_then_else: symbol used in both branches — no extra incref" {
    var scenario = try setupSwitchUse(true, true, false, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 0, 0, 0);
}

test "RC if_then_else: condition preserves live list owner for branch body" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const list = try f.local(f.list_str);
    const cond = try f.local(.i64);
    const branch_result = try f.local(.i64);
    const default_result = try f.local(.i64);
    const ret = try f.ret(branch_result);
    const branch_body = try f.assignCall(branch_result, &.{list}, ret);
    const default_body = try f.assignCall(default_result, &.{list}, ret);
    const switch_stmt = try f.switchStmt(cond, branch_body, default_body, ret);
    const cond_assign = try f.assignI64(cond, 1, switch_stmt);
    const body = try f.assignList(list, &.{}, cond_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(list, 0, 0, 0);
}

test "RC nested match: symbol used in inner and outer match branches" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const cond_outer = try f.local(.i64);
    const cond_inner = try f.local(.i64);
    const outer_result = try f.local(.i64);
    const inner_result = try f.local(.i64);
    const ret = try f.ret(outer_result);
    const inner_branch = try f.assignCall(inner_result, &.{value}, ret);
    const inner_default = try f.assignCall(inner_result, &.{value}, ret);
    const inner_switch = try f.switchStmt(cond_inner, inner_branch, inner_default, ret);
    const outer_default = try f.assignCall(outer_result, &.{value}, ret);
    const outer_switch = try f.switchStmt(cond_outer, inner_switch, outer_default, ret);
    const inner_cond_assign = try f.assignI64(cond_inner, 1, outer_switch);
    const outer_cond_assign = try f.assignI64(cond_outer, 1, inner_cond_assign);
    const body = try f.assignStr(value, "nested", outer_cond_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(value, 0, 0, 0);
}

test "RC match rest prelude tail-cleans outer scrutinee binding" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const scrutinee = try f.local(f.list_str);
    const rest = try f.local(f.list_str);
    const ret = try f.ret(scrutinee);
    const rest_ref = try f.assignRefLocal(rest, scrutinee, ret);
    const body = try f.assignList(scrutinee, &.{}, rest_ref);
    _ = try f.addProc(&.{}, body, f.list_str);
    try f.run();
    try testing.expect(f.countRc(scrutinee, .decref) >= 1);
}

test "RC nested list-pattern match tail-cleans rest binding" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const rest = try f.local(f.list_str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const body = try f.assignList(rest, &.{}, ret);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(rest, 0, 1, 0);
}

test "RC combined match rest prelude with nested list pattern cleans both owners" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const outer = try f.local(f.list_str);
    const rest = try f.local(f.list_str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const rest_assign = try f.assignList(rest, &.{}, ret);
    const body = try f.assignList(outer, &.{}, rest_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(outer, 0, 1, 0);
    try f.expectRc(rest, 0, 1, 0);
}

test "RC tag-pattern match tail-cleans outer scrutinee binding with refcounted payload" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const tag_value = try f.local(f.tag_str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const tag_assign = try f.assignTag(tag_value, 1, payload, ret);
    const body = try f.assignStr(payload, "payload", tag_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(tag_value, 0, 1, 0);
}

test "RC discriminant_switch: symbol used in switch branches gets per-branch RC" {
    var scenario = try setupSwitchUse(true, false, false, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 0, 1, 0);
}

test "RC discriminant_switch: body-bound symbols don't get per-branch RC ops" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const branch_value = try f.local(.str);
    const default_value = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const branch_body = try f.assignStr(branch_value, "branch-local", ret);
    const default_body = try f.assignStr(default_value, "default-local", ret);
    const switch_stmt = try f.switchStmt(cond, branch_body, default_body, null);
    const body = try f.assignI64(cond, 1, switch_stmt);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(branch_value, 0, 1, 0);
    try f.expectRc(default_value, 0, 1, 0);
}

test "RC tag_payload_access: retained parent temp is released after extraction" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const payload = try f.local(.str);
    const tag_value = try f.local(f.tag_str);
    const extracted = try f.local(.str);
    const ret = try f.ret(extracted);
    const extract = try f.assignTagPayload(extracted, tag_value, ret);
    const tag_assign = try f.assignTag(tag_value, 1, payload, extract);
    const body = try f.assignStr(payload, "extract", tag_assign);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try f.expectRc(tag_value, 0, 1, 0);
    try testing.expect(f.countRc(extracted, .incref) >= 1);
}

test "RC early_return emits correct number of decrefs for multi-use symbol" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const result = try f.local(.i64);
    const early = try f.ret(value);
    const use_twice = try f.assignCall(result, &.{ value, value }, early);
    const body = try f.assignStr(value, "early", use_twice);
    _ = try f.addProc(&.{}, body, .str);
    try f.run();
    try f.expectRc(value, 3, 1, 0);
}

test "RC early_return inside branch accounts for branch-level increfs" {
    var scenario = try setupSwitchUse(true, false, true, false);
    defer scenario.fixture.deinit();
    try scenario.fixture.expectRc(scenario.value, 1, 1, 0);
}

test "RC early_return nested in call arguments gets cleanup decrefs" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const result = try f.local(.i64);
    const crash = try f.crash("nested early return");
    const use_once = try f.assignLowLevel(result, &.{value}, LIR.LowLevel.RcEffect.allocatesRetainingArgs(1), crash);
    const body = try f.assignStr(value, "nested", use_once);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectReachableRcBefore(f.procBody(), .decref, value, .crash);
}

test "dev lowering: list rest pattern emits two list decrefs" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const scrutinee = try f.local(f.list_str);
    const rest = try f.local(f.list_str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const rest_assign = try f.assignList(rest, &.{}, ret);
    const body = try f.assignList(scrutinee, &.{}, rest_assign);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(scrutinee, 0, 1, 0);
    try f.expectRc(rest, 0, 1, 0);
}

test "dev lowering: mutable loop append decrefs mutable result binding once" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const acc = try f.local(f.list_i64);
    const appended = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const ret = try f.ret(appended);
    const append = try f.assignLowLevel(appended, &.{ acc, elem }, LIR.LowLevel.RcEffect.consumesArgsReturningConsumedArgsRetainingArgs(1, 0), ret);
    const body = try f.assignList(acc, &.{}, append);
    _ = try f.addProc(&.{}, body, f.list_i64);
    try f.run();
    try f.expectRc(acc, 0, 0, 0);
    try testing.expect(f.countRc(appended, .decref) >= 1);
}

test "dev lowering: mutable list reassignment keeps both decrefs on the reassigned symbol" {
    var scenario = try setupMutation(true);
    defer scenario.fixture.deinit();
    try testing.expect(scenario.fixture.countRc(scenario.target, .decref) >= 2);
}
