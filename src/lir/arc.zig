//! Mechanical ARC insertion for LIR.
//!
//! This pass is the only non-builtin stage that may synthesize explicit
//! baseline automatic `incref` and `decref` statements. `decref` owns ordinary
//! zero-count cleanup; backends consume explicit RC statements without doing
//! reference-counting analysis.

const std = @import("std");
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;

pub const ResourceError = std.mem.Allocator.Error;

/// Public `insert` function.
pub fn insert(store: *LirStore, layouts: *const layout_mod.Store) ResourceError!void {
    var inserter = Inserter{
        .store = store,
        .layouts = layouts,
    };
    var local_contains_refcounted = try store.allocator.alloc(bool, store.locals.items.len);
    defer store.allocator.free(local_contains_refcounted);
    for (store.locals.items, 0..) |local, index| {
        local_contains_refcounted[index] = layouts.layoutContainsRefcounted(layouts.getLayout(local.layout_idx));
    }
    inserter.local_contains_refcounted = local_contains_refcounted;

    for (store.proc_specs.items) |*proc| {
        const body = proc.body orelse continue;
        var join_bodies = JoinBodyMap.init(store.allocator);
        defer join_bodies.deinit();
        var join_visit = std.AutoHashMap(LIR.CFStmtId, void).init(store.allocator);
        defer join_visit.deinit();
        try inserter.collectJoinBodies(body, &join_bodies, &join_visit);
        inserter.join_bodies = &join_bodies;
        defer inserter.join_bodies = null;
        var rewritten_joins = RewrittenJoinMap.init(store.allocator);
        defer {
            var iter = rewritten_joins.valueIterator();
            while (iter.next()) |entry| entry.keep.deinit();
            rewritten_joins.deinit();
        }
        inserter.rewritten_joins = &rewritten_joins;
        defer inserter.rewritten_joins = null;
        var owned = try OwnedSet.init(store.allocator, store.locals.items.len);
        defer owned.deinit();
        for (store.getLocalSpan(proc.args)) |param| {
            inserter.addOwnedIfRc(&owned, param);
        }
        proc.body = try inserter.rewritePath(body, &owned, .{});
        try inserter.writeProcJoinPoints(proc);
    }
}

const RewriteOptions = struct {
    boundaries: []const RewriteBoundary = &.{},
    loop_keep: ?*const OwnedSet = null,
    join_keeps: []const JoinKeep = &.{},
};

const RewriteBoundary = struct {
    stop: LIR.CFStmtId,
    replacement: LIR.CFStmtId,
    keep: *const OwnedSet,
};

const JoinKeep = struct {
    target: LIR.JoinPointId,
    keep: *const OwnedSet,
};

fn rewriteBoundaryForStop(boundaries: []const RewriteBoundary, cursor: LIR.CFStmtId) ?RewriteBoundary {
    var i = boundaries.len;
    while (i > 0) {
        i -= 1;
        if (boundaries[i].stop == cursor) return boundaries[i];
    }
    return null;
}

fn replacementReached(boundaries: []const RewriteBoundary, cursor: LIR.CFStmtId) bool {
    var i = boundaries.len;
    while (i > 0) {
        i -= 1;
        if (boundaries[i].replacement == cursor) return true;
    }
    return false;
}

fn appendRewriteBoundary(
    allocator: std.mem.Allocator,
    boundaries: []const RewriteBoundary,
    boundary: RewriteBoundary,
) ResourceError![]RewriteBoundary {
    const nested = try allocator.alloc(RewriteBoundary, boundaries.len + 1);
    @memcpy(nested[0..boundaries.len], boundaries);
    nested[boundaries.len] = boundary;
    return nested;
}

fn appendJoinKeep(
    allocator: std.mem.Allocator,
    join_keeps: []const JoinKeep,
    join_keep: JoinKeep,
) ResourceError![]JoinKeep {
    const nested = try allocator.alloc(JoinKeep, join_keeps.len + 1);
    @memcpy(nested[0..join_keeps.len], join_keeps);
    nested[join_keeps.len] = join_keep;
    return nested;
}

fn keepForJoin(join_keeps: []const JoinKeep, target: LIR.JoinPointId) ?*const OwnedSet {
    var i = join_keeps.len;
    while (i > 0) {
        i -= 1;
        if (join_keeps[i].target == target) return join_keeps[i].keep;
    }
    return null;
}

const LinearRewriteFrame = struct {
    stmt: LIR.CFStmtId,
    head: LIR.CFStmtId,
    retain_assign_ref_target: bool = true,
    retain_set_target: bool = true,
};

const Inserter = struct {
    store: *LirStore,
    layouts: *const layout_mod.Store,
    local_contains_refcounted: []const bool = &.{},
    join_bodies: ?*const JoinBodyMap = null,
    rewritten_joins: ?*RewrittenJoinMap = null,

    const CallArgOwnership = struct {
        retain_args: std.ArrayList(LIR.LocalId) = .empty,
        transfer_args: std.ArrayList(LIR.LocalId) = .empty,

        fn deinit(self: *CallArgOwnership, allocator: std.mem.Allocator) void {
            self.retain_args.deinit(allocator);
            self.transfer_args.deinit(allocator);
        }
    };

    const RewriteTask = union(enum) {
        path: *RewritePathTask,
        join: *RewriteJoinTask,
        switch_no_continuation: *RewriteSwitchNoContinuationTask,
        switch_after_continuation: *RewriteSwitchContinuationTask,
        switch_finish_continuation: *RewriteSwitchContinuationTask,
    };

    const RewritePathTask = struct {
        cursor: LIR.CFStmtId,
        owned: OwnedSet,
        options: RewriteOptions,
        frames: std.ArrayList(LinearRewriteFrame),
        result: *LIR.CFStmtId,
    };

    const RewriteJoinTask = struct {
        start: LIR.CFStmtId,
        id: LIR.JoinPointId,
        params: LIR.LocalSpan,
        body: LIR.CFStmtId = undefined,
        remainder: LIR.CFStmtId = undefined,
        incoming_owned: OwnedSet,
        entry_keep: OwnedSet,
        body_keep: OwnedSet,
        join_keeps: []JoinKeep = &.{},
        frames: std.ArrayList(LinearRewriteFrame),
        result: *LIR.CFStmtId,
    };

    const RewriteSwitchNoContinuationTask = struct {
        start: LIR.CFStmtId,
        cond: LIR.LocalId,
        branches: LIR.CFSwitchBranchSpan,
        branch_results: []LIR.CFStmtId,
        default_branch: LIR.CFStmtId = undefined,
        continuation: ?LIR.CFStmtId,
        frames: std.ArrayList(LinearRewriteFrame),
        result: *LIR.CFStmtId,
    };

    const RewriteSwitchContinuationTask = struct {
        start: LIR.CFStmtId,
        cond: LIR.LocalId,
        branches: LIR.CFSwitchBranchSpan,
        original_continuation: LIR.CFStmtId,
        continuation: LIR.CFStmtId = undefined,
        branch_results: []LIR.CFStmtId,
        default_branch: LIR.CFStmtId = undefined,
        entry_owned: OwnedSet,
        common: OwnedSet,
        parent_options: RewriteOptions,
        nested_boundaries: []RewriteBoundary = &.{},
        frames: std.ArrayList(LinearRewriteFrame),
        result: *LIR.CFStmtId,
    };

    const AnalysisTask = union(enum) {
        path: *AnalysisPathTask,
        resume_switch_continuation: *AnalysisSwitchContinuationTask,
    };

    const AnalysisPathTask = struct {
        cursor: LIR.CFStmtId,
        stop: LIR.CFStmtId,
        owned: OwnedSet,
        exits: *std.ArrayList(OwnedSet),
        loop_keep: ?*const OwnedSet,
    };

    const AnalysisSwitchContinuationTask = struct {
        continuation: LIR.CFStmtId,
        stop: LIR.CFStmtId,
        switch_exits: std.ArrayList(OwnedSet),
        parent_exits: *std.ArrayList(OwnedSet),
        loop_keep: ?*const OwnedSet,
    };

    fn rewritePath(self: *Inserter, start: LIR.CFStmtId, owned: *OwnedSet, options: RewriteOptions) ResourceError!LIR.CFStmtId {
        var result: LIR.CFStmtId = undefined;
        var tasks = std.ArrayList(RewriteTask).empty;
        defer {
            while (tasks.pop()) |task| self.destroyRewriteTask(task);
            tasks.deinit(self.store.allocator);
        }

        try self.pushRewritePath(&tasks, start, owned, options, &result);
        while (tasks.pop()) |task| {
            switch (task) {
                .path => |path| try self.processRewritePath(&tasks, path),
                .join => |join| try self.finishRewriteJoin(join),
                .switch_no_continuation => |switch_| try self.finishRewriteSwitchNoContinuation(switch_),
                .switch_after_continuation => |switch_| try self.processRewriteSwitchAfterContinuation(&tasks, switch_),
                .switch_finish_continuation => |switch_| try self.finishRewriteSwitchContinuation(switch_),
            }
        }
        return result;
    }

    fn pushRewritePath(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        start: LIR.CFStmtId,
        owned: *const OwnedSet,
        options: RewriteOptions,
        result: *LIR.CFStmtId,
    ) ResourceError!void {
        const task = try self.store.allocator.create(RewritePathTask);
        errdefer self.store.allocator.destroy(task);
        task.* = .{
            .cursor = start,
            .owned = try owned.clone(),
            .options = options,
            .frames = .empty,
            .result = result,
        };
        errdefer task.owned.deinit();
        try tasks.append(self.store.allocator, .{ .path = task });
    }

    fn takeRewriteFrames(path: *RewritePathTask) std.ArrayList(LinearRewriteFrame) {
        const frames = path.frames;
        path.frames = .empty;
        return frames;
    }

    fn processRewritePath(self: *Inserter, tasks: *std.ArrayList(RewriteTask), path: *RewritePathTask) ResourceError!void {
        errdefer self.destroyRewritePath(path);

        while (true) {
            if (rewriteBoundaryForStop(path.options.boundaries, path.cursor)) |boundary| {
                const tail = try self.releaseDifference(&path.owned, boundary.keep, boundary.replacement);
                path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                self.destroyRewritePath(path);
                return;
            }
            if (replacementReached(path.options.boundaries, path.cursor)) {
                path.result.* = try self.finishLinearRewrite(&path.frames, path.cursor);
                self.destroyRewritePath(path);
                return;
            }

            const stmt = self.store.getCFStmt(path.cursor);
            var current_start = path.cursor;
            switch (stmt) {
                .assign_ref => |assign| {
                    var retain_assign_ref_target = true;
                    switch (assign.op) {
                        .local => |source| {
                            if (assign.target != source) {
                                const move_value = try self.canMoveSetLocalValue(&path.owned, source, assign.next, path.options.loop_keep);
                                current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                                if (move_value) {
                                    path.owned.unset(source);
                                    retain_assign_ref_target = false;
                                }
                                self.addOwnedIfRc(&path.owned, assign.target);
                            } else {
                                retain_assign_ref_target = false;
                            }
                        },
                        else => {
                            current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                            self.addOwnedIfRc(&path.owned, assign.target);
                        },
                    }
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .retain_assign_ref_target = retain_assign_ref_target,
                    });
                    path.cursor = assign.next;
                },
                .assign_literal => |assign| {
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = assign.next;
                },
                .assign_call => |assign| {
                    var arg_ownership = try self.callArgOwnership(&path.owned, assign.args, assign.next, assign.target, path.options.loop_keep);
                    defer arg_ownership.deinit(self.store.allocator);
                    if (!self.spanUsesLocal(assign.args, assign.target)) {
                        current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    }
                    self.unsetArgs(&path.owned, arg_ownership.transfer_args.items);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    current_start = try self.retainArgs(arg_ownership.retain_args.items, current_start);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = assign.next;
                },
                .assign_call_erased => |assign| {
                    var arg_ownership = try self.callArgOwnership(&path.owned, assign.args, assign.next, assign.target, path.options.loop_keep);
                    defer arg_ownership.deinit(self.store.allocator);
                    if (!self.spanUsesLocal(assign.args, assign.target) and assign.closure != assign.target) {
                        current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    }
                    self.unsetArgs(&path.owned, arg_ownership.transfer_args.items);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    current_start = try self.retainLocalIfRc(assign.closure, current_start);
                    current_start = try self.retainArgs(arg_ownership.retain_args.items, current_start);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = assign.next;
                },
                .assign_packed_erased_fn => |assign| {
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = assign.next;
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
                        path.options.loop_keep,
                    );
                    const target_consumed = self.maskedArgsContainLocal(assign.args, assign.rc_effect.consume_args, assign.target);
                    if (target_consumed) {
                        path.owned.unset(assign.target);
                    } else {
                        current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    }
                    if (assign.rc_effect.consume_args != 0) {
                        self.unsetMaskedArgsExcept(&path.owned, assign.args, assign.rc_effect.consume_args & ~preserve_consumed_args, assign.target);
                    }
                    self.addOwnedIfRc(&path.owned, assign.target);
                    if (assign.rc_effect.consume_args != 0) {
                        current_start = try self.retainMaskedArgs(assign.args, preserve_consumed_args, current_start);
                    }
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = assign.next;
                },
                .assign_list => |assign| {
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = assign.next;
                },
                .assign_struct => |assign| {
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = assign.next;
                },
                .assign_tag => |assign| {
                    current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = assign.next;
                },
                .set_local => |assign| {
                    var retain_set_target = assign.target != assign.value;
                    if (assign.target != assign.value) {
                        const move_value = try self.canMoveSetLocalValue(&path.owned, assign.value, assign.next, path.options.loop_keep);
                        switch (assign.mode) {
                            .replace_existing => current_start = try self.releaseOldTargetIfNeeded(assign.target, &path.owned, current_start),
                            .initialize_join_result, .initialize_join_param => {},
                        }
                        if (move_value) {
                            path.owned.unset(assign.value);
                            retain_set_target = false;
                        }
                        self.addOwnedIfRc(&path.owned, assign.target);
                    }
                    try path.frames.append(self.store.allocator, .{
                        .stmt = path.cursor,
                        .head = current_start,
                        .retain_set_target = retain_set_target,
                    });
                    path.cursor = assign.next;
                },
                .debug => |debug_stmt| {
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = debug_stmt.next;
                },
                .expect => |expect_stmt| {
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = expect_stmt.next;
                },
                .incref => |rc| {
                    if (path.options.boundaries.len == 0) arcInvariant("ARC insertion received already-reference-counted LIR");
                    self.addOwnedIfRc(&path.owned, rc.value);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = rc.next;
                },
                .decref => |rc| {
                    if (path.options.boundaries.len == 0) arcInvariant("ARC insertion received already-reference-counted LIR");
                    path.owned.unset(rc.value);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = rc.next;
                },
                .free => |rc| {
                    if (path.options.boundaries.len == 0) arcInvariant("ARC insertion received already-reference-counted LIR");
                    path.owned.unset(rc.value);
                    try path.frames.append(self.store.allocator, .{ .stmt = path.cursor, .head = current_start });
                    path.cursor = rc.next;
                },
                .switch_stmt => |switch_stmt| {
                    try self.scheduleRewriteSwitch(tasks, path, path.cursor, switch_stmt);
                    self.destroyRewritePath(path);
                    return;
                },
                .join => |join_stmt| {
                    try self.scheduleRewriteJoin(tasks, path, path.cursor, join_stmt);
                    self.destroyRewritePath(path);
                    return;
                },
                .runtime_error => {
                    const tail = try self.releaseAll(&path.owned, path.cursor);
                    path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                    self.destroyRewritePath(path);
                    return;
                },
                .loop_continue => {
                    const tail = if (path.options.loop_keep) |keep| try self.releaseDifference(&path.owned, keep, path.cursor) else path.cursor;
                    path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                    self.destroyRewritePath(path);
                    return;
                },
                .loop_break => {
                    const tail = if (path.options.loop_keep) |keep| try self.releaseDifference(&path.owned, keep, path.cursor) else path.cursor;
                    path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                    self.destroyRewritePath(path);
                    return;
                },
                .jump => |jump_stmt| {
                    const keep = keepForJoin(path.options.join_keeps, jump_stmt.target) orelse
                        arcInvariant("ARC jump reached a join without an active ownership target");
                    const jump = try self.store.addCFStmt(.{ .jump = .{ .target = jump_stmt.target } });
                    const tail = try self.releaseDifference(&path.owned, keep, jump);
                    path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                    self.destroyRewritePath(path);
                    return;
                },
                .ret => |ret_stmt| {
                    var tail = path.cursor;
                    tail = try self.releaseAll(&path.owned, tail);
                    tail = try self.retainLocalIfRc(ret_stmt.value, tail);
                    path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                    self.destroyRewritePath(path);
                    return;
                },
                .crash => {
                    const tail = try self.releaseAll(&path.owned, path.cursor);
                    path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                    self.destroyRewritePath(path);
                    return;
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
        var cloned: LIR.CFStmtId = undefined;
        switch (stmt) {
            .assign_ref => |assign| {
                if (frame.retain_assign_ref_target) {
                    next = try self.retainLocalIfRc(assign.target, next);
                }
                cloned = try self.store.addCFStmt(.{ .assign_ref = .{
                    .target = assign.target,
                    .op = assign.op,
                    .next = next,
                } });
            },
            .assign_literal => |assign| {
                cloned = try self.store.addCFStmt(.{ .assign_literal = .{
                    .target = assign.target,
                    .value = assign.value,
                    .next = next,
                } });
            },
            .assign_call => |assign| {
                cloned = try self.store.addCFStmt(.{ .assign_call = .{
                    .target = assign.target,
                    .proc = assign.proc,
                    .args = assign.args,
                    .next = next,
                } });
            },
            .assign_call_erased => |assign| {
                next = try self.releaseLocalIfRc(assign.closure, next);
                cloned = try self.store.addCFStmt(.{ .assign_call_erased = .{
                    .target = assign.target,
                    .closure = assign.closure,
                    .args = assign.args,
                    .next = next,
                } });
            },
            .assign_packed_erased_fn => |assign| {
                if (assign.capture) |capture| {
                    next = try self.retainLocalIfRc(capture, next);
                }
                cloned = try self.store.addCFStmt(.{ .assign_packed_erased_fn = .{
                    .target = assign.target,
                    .proc = assign.proc,
                    .capture = assign.capture,
                    .capture_layout = assign.capture_layout,
                    .on_drop = assign.on_drop,
                    .next = next,
                } });
            },
            .assign_low_level => |assign| {
                if (assign.rc_effect.retain_args != 0) {
                    next = try self.retainMaskedArgs(assign.args, assign.rc_effect.retain_args, next);
                }
                if (assign.rc_effect.retain_result) {
                    next = try self.retainLocalIfRc(assign.target, next);
                }
                cloned = try self.store.addCFStmt(.{ .assign_low_level = .{
                    .target = assign.target,
                    .op = assign.op,
                    .rc_effect = assign.rc_effect,
                    .args = assign.args,
                    .next = next,
                } });
            },
            .assign_list => |assign| {
                next = try self.retainSpan(assign.elems, next);
                cloned = try self.store.addCFStmt(.{ .assign_list = .{
                    .target = assign.target,
                    .elems = assign.elems,
                    .next = next,
                } });
            },
            .assign_struct => |assign| {
                next = try self.retainSpan(assign.fields, next);
                cloned = try self.store.addCFStmt(.{ .assign_struct = .{
                    .target = assign.target,
                    .fields = assign.fields,
                    .next = next,
                } });
            },
            .assign_tag => |assign| {
                if (assign.payload) |payload| {
                    next = try self.retainLocalIfRc(payload, next);
                }
                cloned = try self.store.addCFStmt(.{ .assign_tag = .{
                    .target = assign.target,
                    .variant_index = assign.variant_index,
                    .discriminant = assign.discriminant,
                    .payload = assign.payload,
                    .next = next,
                } });
            },
            .set_local => |assign| {
                if (assign.target != assign.value and frame.retain_set_target) {
                    next = try self.retainLocalIfRc(assign.target, next);
                }
                cloned = try self.store.addCFStmt(.{ .set_local = .{
                    .target = assign.target,
                    .value = assign.value,
                    .mode = assign.mode,
                    .next = next,
                } });
            },
            .debug => |debug_stmt| {
                cloned = try self.store.addCFStmt(.{ .debug = .{
                    .message = debug_stmt.message,
                    .next = next,
                } });
            },
            .expect => |expect_stmt| {
                cloned = try self.store.addCFStmt(.{ .expect = .{
                    .condition = expect_stmt.condition,
                    .next = next,
                } });
            },
            .incref => |rc| {
                cloned = try self.store.addCFStmt(.{ .incref = .{
                    .value = rc.value,
                    .rc = rc.rc,
                    .count = rc.count,
                    .next = next,
                } });
            },
            .decref => |rc| {
                cloned = try self.store.addCFStmt(.{ .decref = .{
                    .value = rc.value,
                    .rc = rc.rc,
                    .next = next,
                } });
            },
            .free => |rc| {
                cloned = try self.store.addCFStmt(.{ .free = .{
                    .value = rc.value,
                    .rc = rc.rc,
                    .next = next,
                } });
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
        return self.attachPrefix(frame.head, frame.stmt, cloned);
    }

    fn attachPrefix(self: *Inserter, head: LIR.CFStmtId, old_tail: LIR.CFStmtId, new_tail: LIR.CFStmtId) LIR.CFStmtId {
        if (head == old_tail) return new_tail;

        var cursor = head;
        while (cursor != old_tail) {
            const stmt = self.store.getCFStmtPtr(cursor);
            switch (stmt.*) {
                .incref => |*rc| {
                    if (rc.next == old_tail) {
                        rc.next = new_tail;
                        return head;
                    }
                    cursor = rc.next;
                },
                .decref => |*rc| {
                    if (rc.next == old_tail) {
                        rc.next = new_tail;
                        return head;
                    }
                    cursor = rc.next;
                },
                .free => |*rc| {
                    if (rc.next == old_tail) {
                        rc.next = new_tail;
                        return head;
                    }
                    cursor = rc.next;
                },
                else => arcInvariant("ARC linear rewrite prefix contained a non-RC statement"),
            }
        }

        arcInvariant("ARC linear rewrite prefix did not reach the cloned statement");
    }

    fn scheduleRewriteJoin(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        path: *RewritePathTask,
        start: LIR.CFStmtId,
        join_stmt: anytype,
    ) ResourceError!void {
        if (self.rewritten_joins) |rewritten_joins| {
            if (rewritten_joins.get(start)) |rewritten| {
                const tail = try self.releaseDifference(&path.owned, &rewritten.keep, rewritten.stmt);
                path.result.* = try self.finishLinearRewrite(&path.frames, tail);
                return;
            }
        }

        const state = try self.store.allocator.create(RewriteJoinTask);
        var queued = false;
        errdefer if (!queued) self.store.allocator.destroy(state);
        state.* = .{
            .start = start,
            .id = join_stmt.id,
            .params = join_stmt.params,
            .incoming_owned = try path.owned.clone(),
            .entry_keep = try self.joinEntryOwnedSet(&path.owned, join_stmt.remainder),
            .body_keep = try self.joinBodyOwnedSet(&path.owned, join_stmt.params, join_stmt.body),
            .frames = takeRewriteFrames(path),
            .result = path.result,
        };
        errdefer if (!queued) state.incoming_owned.deinit();
        errdefer if (!queued) state.entry_keep.deinit();
        errdefer if (!queued) state.body_keep.deinit();
        errdefer if (!queued) state.frames.deinit(self.store.allocator);
        state.join_keeps = try appendJoinKeep(self.store.allocator, path.options.join_keeps, .{
            .target = join_stmt.id,
            .keep = &state.body_keep,
        });
        errdefer if (!queued) self.store.allocator.free(state.join_keeps);

        try tasks.append(self.store.allocator, .{ .join = state });
        queued = true;
        const join_options = RewriteOptions{
            .boundaries = path.options.boundaries,
            .loop_keep = &state.body_keep,
            .join_keeps = state.join_keeps,
        };
        try self.pushRewritePath(tasks, join_stmt.remainder, &state.entry_keep, .{
            .boundaries = join_options.boundaries,
            .loop_keep = join_options.loop_keep,
            .join_keeps = join_options.join_keeps,
        }, &state.remainder);
        try self.pushRewritePath(tasks, join_stmt.body, &state.body_keep, join_options, &state.body);
    }

    fn finishRewriteJoin(self: *Inserter, state: *RewriteJoinTask) ResourceError!void {
        errdefer self.destroyRewriteJoin(state);
        const join = try self.store.addCFStmt(.{ .join = .{
            .id = state.id,
            .params = state.params,
            .body = state.body,
            .remainder = state.remainder,
        } });
        if (self.rewritten_joins) |rewritten_joins| {
            try rewritten_joins.put(state.start, .{
                .keep = try state.entry_keep.clone(),
                .stmt = join,
            });
        }
        const tail = try self.releaseDifference(&state.incoming_owned, &state.entry_keep, join);
        state.result.* = try self.finishLinearRewrite(&state.frames, tail);
        self.destroyRewriteJoin(state);
    }

    fn scheduleRewriteSwitch(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        path: *RewritePathTask,
        start: LIR.CFStmtId,
        switch_stmt: anytype,
    ) ResourceError!void {
        if (switch_stmt.continuation) |continuation| {
            var exit_states = std.ArrayList(OwnedSet).empty;
            defer {
                for (exit_states.items) |*state| state.deinit();
                exit_states.deinit(self.store.allocator);
            }

            for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                try self.analyzeUntil(branch.body, &path.owned, continuation, &exit_states, path.options.loop_keep);
            }
            try self.analyzeUntil(switch_stmt.default_branch, &path.owned, continuation, &exit_states, path.options.loop_keep);

            if (exit_states.items.len == 0) {
                try self.scheduleRewriteSwitchNoContinuation(tasks, path, start, switch_stmt.cond, switch_stmt.branches, switch_stmt.default_branch, switch_stmt.continuation);
                return;
            }

            var common = try exit_states.items[0].clone();
            defer common.deinit();
            for (exit_states.items[1..]) |*state| common.intersect(state);
            try self.scheduleRewriteSwitchContinuation(tasks, path, start, switch_stmt.cond, switch_stmt.branches, continuation, &common);
            return;
        }

        try self.scheduleRewriteSwitchNoContinuation(tasks, path, start, switch_stmt.cond, switch_stmt.branches, switch_stmt.default_branch, null);
    }

    fn scheduleRewriteSwitchNoContinuation(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        path: *RewritePathTask,
        start: LIR.CFStmtId,
        cond: LIR.LocalId,
        branches_span: LIR.CFSwitchBranchSpan,
        default_branch: LIR.CFStmtId,
        continuation: ?LIR.CFStmtId,
    ) ResourceError!void {
        const branches = self.store.getCFSwitchBranches(branches_span);
        const state = try self.store.allocator.create(RewriteSwitchNoContinuationTask);
        var queued = false;
        errdefer if (!queued) self.store.allocator.destroy(state);
        const branch_results = try self.store.allocator.alloc(LIR.CFStmtId, branches.len);
        errdefer if (!queued) self.store.allocator.free(branch_results);
        state.* = .{
            .start = start,
            .cond = cond,
            .branches = branches_span,
            .branch_results = branch_results,
            .continuation = continuation,
            .frames = takeRewriteFrames(path),
            .result = path.result,
        };
        errdefer if (!queued) state.frames.deinit(self.store.allocator);

        try tasks.append(self.store.allocator, .{ .switch_no_continuation = state });
        queued = true;
        try self.pushRewritePath(tasks, default_branch, &path.owned, path.options, &state.default_branch);
        for (branches, 0..) |branch, index| {
            try self.pushRewritePath(tasks, branch.body, &path.owned, path.options, &state.branch_results[index]);
        }
    }

    fn finishRewriteSwitchNoContinuation(self: *Inserter, state: *RewriteSwitchNoContinuationTask) ResourceError!void {
        errdefer self.destroyRewriteSwitchNoContinuation(state);
        const branches = self.store.getCFSwitchBranches(state.branches);
        if (branches.len != state.branch_results.len) arcInvariant("ARC switch branch result count changed during rewrite");
        const rewritten_branches = try self.store.allocator.alloc(LIR.CFSwitchBranch, branches.len);
        defer self.store.allocator.free(rewritten_branches);
        for (branches, state.branch_results, 0..) |branch, rewritten, i| {
            rewritten_branches[i] = .{
                .value = branch.value,
                .body = rewritten,
            };
        }
        const switch_stmt = try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = state.cond,
            .branches = try self.store.addCFSwitchBranches(rewritten_branches),
            .default_branch = state.default_branch,
            .continuation = state.continuation,
        } });
        state.result.* = try self.finishLinearRewrite(&state.frames, switch_stmt);
        self.destroyRewriteSwitchNoContinuation(state);
    }

    fn scheduleRewriteSwitchContinuation(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        path: *RewritePathTask,
        start: LIR.CFStmtId,
        cond: LIR.LocalId,
        branches_span: LIR.CFSwitchBranchSpan,
        continuation: LIR.CFStmtId,
        common: *const OwnedSet,
    ) ResourceError!void {
        const branches = self.store.getCFSwitchBranches(branches_span);
        const state = try self.store.allocator.create(RewriteSwitchContinuationTask);
        var queued = false;
        errdefer if (!queued) self.store.allocator.destroy(state);
        const branch_results = try self.store.allocator.alloc(LIR.CFStmtId, branches.len);
        errdefer if (!queued) self.store.allocator.free(branch_results);
        state.* = .{
            .start = start,
            .cond = cond,
            .branches = branches_span,
            .original_continuation = continuation,
            .branch_results = branch_results,
            .entry_owned = try path.owned.clone(),
            .common = try common.clone(),
            .parent_options = path.options,
            .frames = takeRewriteFrames(path),
            .result = path.result,
        };
        errdefer if (!queued) state.entry_owned.deinit();
        errdefer if (!queued) state.common.deinit();
        errdefer if (!queued) state.frames.deinit(self.store.allocator);

        try tasks.append(self.store.allocator, .{ .switch_after_continuation = state });
        queued = true;
        try self.pushRewritePath(tasks, continuation, &state.common, path.options, &state.continuation);
    }

    fn processRewriteSwitchAfterContinuation(
        self: *Inserter,
        tasks: *std.ArrayList(RewriteTask),
        state: *RewriteSwitchContinuationTask,
    ) ResourceError!void {
        var handed_off = false;
        errdefer if (!handed_off) self.destroyRewriteSwitchContinuation(state);

        state.nested_boundaries = try appendRewriteBoundary(self.store.allocator, state.parent_options.boundaries, .{
            .stop = state.original_continuation,
            .replacement = state.continuation,
            .keep = &state.common,
        });
        const nested_options = RewriteOptions{
            .boundaries = state.nested_boundaries,
            .loop_keep = state.parent_options.loop_keep,
            .join_keeps = state.parent_options.join_keeps,
        };

        try tasks.append(self.store.allocator, .{ .switch_finish_continuation = state });
        handed_off = true;
        const switch_stmt = self.store.getCFStmt(state.start).switch_stmt;
        try self.pushRewritePath(tasks, switch_stmt.default_branch, &state.entry_owned, nested_options, &state.default_branch);
        const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
        if (branches.len != state.branch_results.len) arcInvariant("ARC switch branch result count changed during continuation rewrite");
        for (branches, 0..) |branch, index| {
            try self.pushRewritePath(tasks, branch.body, &state.entry_owned, nested_options, &state.branch_results[index]);
        }
    }

    fn finishRewriteSwitchContinuation(self: *Inserter, state: *RewriteSwitchContinuationTask) ResourceError!void {
        errdefer self.destroyRewriteSwitchContinuation(state);
        const branches = self.store.getCFSwitchBranches(state.branches);
        if (branches.len != state.branch_results.len) arcInvariant("ARC switch branch result count changed during rewrite");
        const rewritten_branches = try self.store.allocator.alloc(LIR.CFSwitchBranch, branches.len);
        defer self.store.allocator.free(rewritten_branches);
        for (branches, state.branch_results, 0..) |branch, rewritten, i| {
            rewritten_branches[i] = .{
                .value = branch.value,
                .body = rewritten,
            };
        }
        const switch_stmt = try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = state.cond,
            .branches = try self.store.addCFSwitchBranches(rewritten_branches),
            .default_branch = state.default_branch,
            .continuation = state.continuation,
        } });
        state.result.* = try self.finishLinearRewrite(&state.frames, switch_stmt);
        self.destroyRewriteSwitchContinuation(state);
    }

    fn analyzeUntil(
        self: *Inserter,
        start: LIR.CFStmtId,
        owned: *const OwnedSet,
        stop: LIR.CFStmtId,
        exits: *std.ArrayList(OwnedSet),
        loop_keep: ?*const OwnedSet,
    ) ResourceError!void {
        var tasks = std.ArrayList(AnalysisTask).empty;
        defer {
            while (tasks.pop()) |task| self.destroyAnalysisTask(task);
            tasks.deinit(self.store.allocator);
        }

        try self.pushAnalysisPath(&tasks, start, stop, owned, exits, loop_keep);
        while (tasks.pop()) |task| {
            switch (task) {
                .path => |path| try self.processAnalysisPath(&tasks, path),
                .resume_switch_continuation => |resume_task| try self.processAnalysisSwitchContinuation(&tasks, resume_task),
            }
        }
    }

    fn pushAnalysisPath(
        self: *Inserter,
        tasks: *std.ArrayList(AnalysisTask),
        start: LIR.CFStmtId,
        stop: LIR.CFStmtId,
        owned: *const OwnedSet,
        exits: *std.ArrayList(OwnedSet),
        loop_keep: ?*const OwnedSet,
    ) ResourceError!void {
        const task = try self.store.allocator.create(AnalysisPathTask);
        errdefer self.store.allocator.destroy(task);
        task.* = .{
            .cursor = start,
            .stop = stop,
            .owned = try owned.clone(),
            .exits = exits,
            .loop_keep = loop_keep,
        };
        errdefer task.owned.deinit();
        try tasks.append(self.store.allocator, .{ .path = task });
    }

    fn processAnalysisPath(self: *Inserter, tasks: *std.ArrayList(AnalysisTask), path: *AnalysisPathTask) ResourceError!void {
        errdefer self.destroyAnalysisPath(path);

        while (true) {
            if (path.cursor == path.stop) {
                try path.exits.append(self.store.allocator, try path.owned.clone());
                self.destroyAnalysisPath(path);
                return;
            }

            const stmt = self.store.getCFStmt(path.cursor);
            switch (stmt) {
                .assign_ref => |assign| {
                    switch (assign.op) {
                        .local => |source| {
                            if (assign.target != source) {
                                const move_value = try self.canMoveSetLocalValue(&path.owned, source, assign.next, path.loop_keep);
                                if (move_value) path.owned.unset(source);
                                self.addOwnedIfRc(&path.owned, assign.target);
                            }
                        },
                        else => self.addOwnedIfRc(&path.owned, assign.target),
                    }
                    path.cursor = assign.next;
                },
                .assign_literal => |assign| {
                    self.addOwnedIfRc(&path.owned, assign.target);
                    path.cursor = assign.next;
                },
                .assign_call => |assign| {
                    var arg_ownership = try self.callArgOwnership(&path.owned, assign.args, assign.next, assign.target, path.loop_keep);
                    defer arg_ownership.deinit(self.store.allocator);
                    self.unsetArgs(&path.owned, arg_ownership.transfer_args.items);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    path.cursor = assign.next;
                },
                .assign_call_erased => |assign| {
                    var arg_ownership = try self.callArgOwnership(&path.owned, assign.args, assign.next, assign.target, path.loop_keep);
                    defer arg_ownership.deinit(self.store.allocator);
                    self.unsetArgs(&path.owned, arg_ownership.transfer_args.items);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    path.cursor = assign.next;
                },
                .assign_packed_erased_fn => |assign| {
                    self.addOwnedIfRc(&path.owned, assign.target);
                    path.cursor = assign.next;
                },
                .assign_low_level => |assign| {
                    const preserve_consumed_args = try self.preserveConsumedArgMask(
                        assign.args,
                        assign.rc_effect.consume_args,
                        assign.next,
                        assign.target,
                        path.loop_keep,
                    );
                    const target_consumed = self.maskedArgsContainLocal(assign.args, assign.rc_effect.consume_args, assign.target);
                    if (target_consumed) {
                        path.owned.unset(assign.target);
                    }
                    self.unsetMaskedArgsExcept(&path.owned, assign.args, assign.rc_effect.consume_args & ~preserve_consumed_args, assign.target);
                    self.addOwnedIfRc(&path.owned, assign.target);
                    path.cursor = assign.next;
                },
                .assign_list => |assign| {
                    self.addOwnedIfRc(&path.owned, assign.target);
                    path.cursor = assign.next;
                },
                .assign_struct => |assign| {
                    self.addOwnedIfRc(&path.owned, assign.target);
                    path.cursor = assign.next;
                },
                .assign_tag => |assign| {
                    self.addOwnedIfRc(&path.owned, assign.target);
                    path.cursor = assign.next;
                },
                .set_local => |assign| {
                    if (assign.target != assign.value) {
                        const move_value = try self.canMoveSetLocalValue(&path.owned, assign.value, assign.next, path.loop_keep);
                        switch (assign.mode) {
                            .initialize_join_result, .initialize_join_param => {},
                            .replace_existing => {},
                        }
                        if (move_value) path.owned.unset(assign.value);
                    }
                    self.addOwnedIfRc(&path.owned, assign.target);
                    path.cursor = assign.next;
                },
                .debug => |debug_stmt| path.cursor = debug_stmt.next,
                .expect => |expect_stmt| path.cursor = expect_stmt.next,
                .incref => |rc| {
                    self.addOwnedIfRc(&path.owned, rc.value);
                    path.cursor = rc.next;
                },
                .decref => |rc| {
                    path.owned.unset(rc.value);
                    path.cursor = rc.next;
                },
                .free => |rc| {
                    path.owned.unset(rc.value);
                    path.cursor = rc.next;
                },
                .switch_stmt => |switch_stmt| {
                    if (switch_stmt.continuation) |continuation| {
                        const resume_task = try self.store.allocator.create(AnalysisSwitchContinuationTask);
                        var queued = false;
                        errdefer if (!queued) self.store.allocator.destroy(resume_task);
                        resume_task.* = .{
                            .continuation = continuation,
                            .stop = path.stop,
                            .switch_exits = .empty,
                            .parent_exits = path.exits,
                            .loop_keep = path.loop_keep,
                        };
                        try tasks.append(self.store.allocator, .{ .resume_switch_continuation = resume_task });
                        queued = true;

                        for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                            try self.pushAnalysisPath(tasks, branch.body, continuation, &path.owned, &resume_task.switch_exits, path.loop_keep);
                        }
                        try self.pushAnalysisPath(tasks, switch_stmt.default_branch, continuation, &path.owned, &resume_task.switch_exits, path.loop_keep);
                        self.destroyAnalysisPath(path);
                        return;
                    }

                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try self.pushAnalysisPath(tasks, branch.body, path.stop, &path.owned, path.exits, path.loop_keep);
                    }
                    try self.pushAnalysisPath(tasks, switch_stmt.default_branch, path.stop, &path.owned, path.exits, path.loop_keep);
                    self.destroyAnalysisPath(path);
                    return;
                },
                .join => {
                    // A join starts a separate loop/recursive ownership frame.
                    // Switch continuation analysis must not fold that frame into
                    // the parent switch's shared continuation.
                    self.destroyAnalysisPath(path);
                    return;
                },
                .runtime_error, .loop_continue, .loop_break, .jump, .ret, .crash => {
                    self.destroyAnalysisPath(path);
                    return;
                },
            }
        }
    }

    fn processAnalysisSwitchContinuation(
        self: *Inserter,
        tasks: *std.ArrayList(AnalysisTask),
        resume_task: *AnalysisSwitchContinuationTask,
    ) ResourceError!void {
        errdefer self.destroyAnalysisSwitchContinuation(resume_task);

        if (resume_task.switch_exits.items.len == 0) {
            self.destroyAnalysisSwitchContinuation(resume_task);
            return;
        }

        var common = try resume_task.switch_exits.items[0].clone();
        defer common.deinit();
        for (resume_task.switch_exits.items[1..]) |*state| common.intersect(state);
        try self.pushAnalysisPath(tasks, resume_task.continuation, resume_task.stop, &common, resume_task.parent_exits, resume_task.loop_keep);
        self.destroyAnalysisSwitchContinuation(resume_task);
    }

    fn destroyRewriteTask(self: *Inserter, task: RewriteTask) void {
        switch (task) {
            .path => |path| self.destroyRewritePath(path),
            .join => |join| self.destroyRewriteJoin(join),
            .switch_no_continuation => |switch_| self.destroyRewriteSwitchNoContinuation(switch_),
            .switch_after_continuation => |switch_| self.destroyRewriteSwitchContinuation(switch_),
            .switch_finish_continuation => |switch_| self.destroyRewriteSwitchContinuation(switch_),
        }
    }

    fn destroyRewritePath(self: *Inserter, path: *RewritePathTask) void {
        path.frames.deinit(self.store.allocator);
        path.owned.deinit();
        self.store.allocator.destroy(path);
    }

    fn destroyRewriteJoin(self: *Inserter, state: *RewriteJoinTask) void {
        state.frames.deinit(self.store.allocator);
        if (state.join_keeps.len != 0) self.store.allocator.free(state.join_keeps);
        state.incoming_owned.deinit();
        state.entry_keep.deinit();
        state.body_keep.deinit();
        self.store.allocator.destroy(state);
    }

    fn destroyRewriteSwitchNoContinuation(self: *Inserter, state: *RewriteSwitchNoContinuationTask) void {
        state.frames.deinit(self.store.allocator);
        self.store.allocator.free(state.branch_results);
        self.store.allocator.destroy(state);
    }

    fn destroyRewriteSwitchContinuation(self: *Inserter, state: *RewriteSwitchContinuationTask) void {
        state.frames.deinit(self.store.allocator);
        state.entry_owned.deinit();
        state.common.deinit();
        if (state.nested_boundaries.len != 0) self.store.allocator.free(state.nested_boundaries);
        self.store.allocator.free(state.branch_results);
        self.store.allocator.destroy(state);
    }

    fn destroyAnalysisTask(self: *Inserter, task: AnalysisTask) void {
        switch (task) {
            .path => |path| self.destroyAnalysisPath(path),
            .resume_switch_continuation => |resume_task| self.destroyAnalysisSwitchContinuation(resume_task),
        }
    }

    fn destroyAnalysisPath(self: *Inserter, path: *AnalysisPathTask) void {
        path.owned.deinit();
        self.store.allocator.destroy(path);
    }

    fn destroyAnalysisSwitchContinuation(self: *Inserter, resume_task: *AnalysisSwitchContinuationTask) void {
        for (resume_task.switch_exits.items) |*state| state.deinit();
        resume_task.switch_exits.deinit(self.store.allocator);
        self.store.allocator.destroy(resume_task);
    }

    fn addOwnedIfRc(self: *Inserter, owned: *OwnedSet, local: LIR.LocalId) void {
        if (self.localContainsRefcounted(local)) owned.set(local);
    }

    fn joinBodyOwnedSet(
        self: *Inserter,
        entry_owned: *const OwnedSet,
        params: LIR.LocalSpan,
        body: LIR.CFStmtId,
    ) ResourceError!OwnedSet {
        var owned = try OwnedSet.init(self.store.allocator, entry_owned.len());
        errdefer owned.deinit();
        var iter = entry_owned.bits.iterator(.{});
        while (iter.next()) |i| {
            const local: LIR.LocalId = @enumFromInt(@as(u32, @intCast(i)));
            if (try self.localValueUsedInPath(body, local, null)) {
                owned.set(local);
            }
        }
        for (self.store.getLocalSpan(params)) |param| {
            self.addOwnedIfRc(&owned, param);
        }
        return owned;
    }

    fn joinEntryOwnedSet(
        self: *Inserter,
        entry_owned: *const OwnedSet,
        remainder: LIR.CFStmtId,
    ) ResourceError!OwnedSet {
        var owned = try OwnedSet.init(self.store.allocator, entry_owned.len());
        errdefer owned.deinit();
        var iter = entry_owned.bits.iterator(.{});
        while (iter.next()) |i| {
            const local: LIR.LocalId = @enumFromInt(@as(u32, @intCast(i)));
            if (try self.localValueUsedInPath(remainder, local, null)) {
                owned.set(local);
            }
        }
        return owned;
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
        return !(try self.localValueUsedInPath(next, value, loop_keep));
    }

    fn retainMaskedArgs(self: *Inserter, span: LIR.LocalSpan, mask: u64, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var current = next;
        const locals = self.store.getLocalSpan(span);
        var i = locals.len;
        while (i > 0) {
            i -= 1;
            if (i >= 64) continue;
            if ((mask & argMaskBit(i)) != 0) {
                current = try self.retainLocalIfRc(locals[i], current);
            }
        }
        return current;
    }

    fn retainArgs(self: *Inserter, args: []const LIR.LocalId, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var current = next;
        var i = args.len;
        while (i > 0) {
            i -= 1;
            current = try self.retainLocalIfRc(args[i], current);
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
            if (i >= 64) break;
            const bit = argMaskBit(i);
            if ((mask & bit) == 0) continue;
            if (local == target) continue;
            if (try self.localValueUsedInPath(next, local, loop_keep)) {
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
        errdefer result.deinit(self.store.allocator);
        var transferred = try OwnedSet.init(self.store.allocator, owned.len());
        defer transferred.deinit();

        const locals = self.store.getLocalSpan(span);
        for (locals) |local| {
            if (!self.localContainsRefcounted(local)) continue;

            const used_after_call = local != target and try self.localValueUsedInPath(next, local, loop_keep);
            const can_transfer = owned.contains(local) and !used_after_call and !transferred.contains(local);

            if (can_transfer) {
                try result.transfer_args.append(self.store.allocator, local);
                transferred.set(local);
            } else {
                try result.retain_args.append(self.store.allocator, local);
            }
        }

        return result;
    }

    fn maskedArgsContainLocal(self: *Inserter, span: LIR.LocalSpan, mask: u64, needle: LIR.LocalId) bool {
        if (mask == 0) return false;
        const locals = self.store.getLocalSpan(span);
        for (locals, 0..) |local, i| {
            if (i >= 64) break;
            if ((mask & argMaskBit(i)) != 0 and local == needle) return true;
        }
        return false;
    }

    fn unsetArgs(_: *Inserter, owned: *OwnedSet, args: []const LIR.LocalId) void {
        for (args) |local| {
            owned.unset(local);
        }
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
            if (i >= 64) break;
            if ((mask & argMaskBit(i)) != 0 and local != except) {
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

    fn writeProcJoinPoints(self: *Inserter, proc: *LIR.LirProcSpec) ResourceError!void {
        const body = proc.body orelse {
            proc.join_points = LIR.JoinPointSpan.empty();
            return;
        };

        var joins: FinalJoinMap = .empty;
        defer joins.deinit(self.store.allocator);
        var visited = std.AutoHashMap(LIR.CFStmtId, void).init(self.store.allocator);
        defer visited.deinit();
        try self.collectFinalJoinPoints(body, &joins, &visited);

        const sorted = try self.store.allocator.alloc(LIR.JoinPoint, joins.count());
        defer self.store.allocator.free(sorted);
        for (joins.values(), 0..) |join, index| {
            sorted[index] = join;
        }
        std.mem.sort(LIR.JoinPoint, sorted, {}, joinPointLessThan);
        proc.join_points = try self.store.addJoinPointSpan(sorted);
    }

    fn collectFinalJoinPoints(
        self: *Inserter,
        start: LIR.CFStmtId,
        joins: *FinalJoinMap,
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
                .incref => |rc| try stack.append(self.store.allocator, rc.next),
                .decref => |rc| try stack.append(self.store.allocator, rc.next),
                .free => |rc| try stack.append(self.store.allocator, rc.next),
                .switch_stmt => |switch_stmt| {
                    if (switch_stmt.continuation) |continuation| {
                        try stack.append(self.store.allocator, continuation);
                    }
                    try stack.append(self.store.allocator, switch_stmt.default_branch);
                    for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                        try stack.append(self.store.allocator, branch.body);
                    }
                },
                .join => |join_stmt| {
                    const entry = try joins.getOrPut(self.store.allocator, join_stmt.id);
                    const join_point = LIR.JoinPoint{
                        .id = join_stmt.id,
                        .params = join_stmt.params,
                        .body = join_stmt.body,
                    };
                    if (entry.found_existing and !joinPointEql(entry.value_ptr.*, join_point)) {
                        arcInvariant("ARC final join-point output saw one join id with different data");
                    }
                    entry.value_ptr.* = join_point;
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
            }
        }
    }

    fn localValueUsedInPath(
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
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_literal => |assign| {
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_call => |assign| {
                    if (self.spanUsesLocal(assign.args, needle)) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_call_erased => |assign| {
                    if (assign.closure == needle or self.spanUsesLocal(assign.args, needle)) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_packed_erased_fn => |assign| {
                    if (assign.capture != null and assign.capture.? == needle) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_low_level => |assign| {
                    if (self.spanUsesLocal(assign.args, needle)) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_list => |assign| {
                    if (self.spanUsesLocal(assign.elems, needle)) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_struct => |assign| {
                    if (self.spanUsesLocal(assign.fields, needle)) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .assign_tag => |assign| {
                    if (assign.payload != null and assign.payload.? == needle) return true;
                    if (assign.target == needle) continue;
                    try stack.append(self.store.allocator, assign.next);
                },
                .set_local => |assign| {
                    if (assign.value == needle) return true;
                    if (assign.target == needle) continue;
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
                .join => |join_stmt| {
                    try stack.append(self.store.allocator, join_stmt.remainder);
                    try stack.append(self.store.allocator, join_stmt.body);
                },
                .jump => |jump_stmt| {
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
        var keep = try OwnedSet.init(self.store.allocator, owned.len());
        defer keep.deinit();
        return try self.releaseDifference(owned, &keep, next);
    }

    fn releaseDifference(self: *Inserter, owned: *const OwnedSet, keep: *const OwnedSet, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        var current = next;
        var iter = owned.bits.iterator(.{ .direction = .reverse });
        while (iter.next()) |i| {
            if (keep.bits.isSet(i)) continue;
            const local: LIR.LocalId = @enumFromInt(@as(u32, @intCast(i)));
            current = try self.releaseLocalIfRc(local, current);
        }
        return current;
    }

    fn retainLocalIfRc(self: *Inserter, local: LIR.LocalId, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        if (!self.localContainsRefcounted(local)) return next;
        const rc = self.rcHelperForLocal(.incref, local);
        return try self.store.addCFStmt(.{ .incref = .{
            .value = local,
            .rc = rc,
            .count = 1,
            .next = next,
        } });
    }

    fn releaseLocalIfRc(self: *Inserter, local: LIR.LocalId, next: LIR.CFStmtId) ResourceError!LIR.CFStmtId {
        if (!self.localContainsRefcounted(local)) return next;
        const rc = self.rcHelperForLocal(.decref, local);
        return try self.store.addCFStmt(.{ .decref = .{
            .value = local,
            .rc = rc,
            .next = next,
        } });
    }

    fn rcHelperForLocal(self: *const Inserter, op: layout_mod.RcOp, local: LIR.LocalId) layout_mod.RcHelper {
        const local_layout = self.store.getLocal(local).layout_idx;
        const helper = self.rcHelperForLayout(op, local_layout);
        if (self.layouts.rcHelperPlan(helper) == .noop) {
            arcInvariant("ARC attempted to emit a noop RC helper for a refcounted local");
        }
        return helper;
    }

    fn rcHelperForLayout(self: *const Inserter, op: layout_mod.RcOp, layout_idx: layout_mod.Idx) layout_mod.RcHelper {
        const layout_val = self.layouts.getLayout(layout_idx);
        return switch (layout_val.tag) {
            .closure => self.rcHelperForLayout(nestedDropOp(op), layout_val.getClosure().captures_layout_idx),
            else => .{ .op = op, .layout_idx = layout_idx },
        };
    }

    fn nestedDropOp(op: layout_mod.RcOp) layout_mod.RcOp {
        return switch (op) {
            .incref => .incref,
            .decref, .free => .decref,
        };
    }

    fn localContainsRefcounted(self: *const Inserter, local: LIR.LocalId) bool {
        const index = @intFromEnum(local);
        if (index >= self.local_contains_refcounted.len) arcInvariant("ARC local refcounted cache did not cover local");
        return self.local_contains_refcounted[index];
    }
};

const JoinBodyMap = std.AutoHashMap(LIR.JoinPointId, LIR.CFStmtId);
const FinalJoinMap = std.AutoArrayHashMapUnmanaged(LIR.JoinPointId, LIR.JoinPoint);

fn joinPointLessThan(_: void, a: LIR.JoinPoint, b: LIR.JoinPoint) bool {
    return @intFromEnum(a.id) < @intFromEnum(b.id);
}

fn joinPointEql(a: LIR.JoinPoint, b: LIR.JoinPoint) bool {
    return a.id == b.id and a.body == b.body and localSpanEql(a.params, b.params);
}

fn localSpanEql(a: LIR.LocalSpan, b: LIR.LocalSpan) bool {
    return a.start == b.start and a.len == b.len;
}

const RewrittenJoin = struct {
    keep: OwnedSet,
    stmt: LIR.CFStmtId,
};

const RewrittenJoinMap = std.AutoHashMap(LIR.CFStmtId, RewrittenJoin);

const OwnedSet = struct {
    allocator: std.mem.Allocator,
    bits: std.bit_set.DynamicBitSetUnmanaged,

    fn init(allocator: std.mem.Allocator, bit_len: usize) ResourceError!OwnedSet {
        const bits = try std.bit_set.DynamicBitSetUnmanaged.initEmpty(allocator, bit_len);
        return .{ .allocator = allocator, .bits = bits };
    }

    fn deinit(self: *OwnedSet) void {
        self.bits.deinit(self.allocator);
        self.bits = .{};
    }

    fn clone(self: *const OwnedSet) ResourceError!OwnedSet {
        const bits = try self.bits.clone(self.allocator);
        return .{ .allocator = self.allocator, .bits = bits };
    }

    fn len(self: *const OwnedSet) usize {
        return self.bits.capacity();
    }

    fn set(self: *OwnedSet, local: LIR.LocalId) void {
        self.bits.set(@intFromEnum(local));
    }

    fn unset(self: *OwnedSet, local: LIR.LocalId) void {
        self.bits.unset(@intFromEnum(local));
    }

    fn contains(self: *const OwnedSet, local: LIR.LocalId) bool {
        return self.bits.isSet(@intFromEnum(local));
    }

    fn intersect(self: *OwnedSet, other: *const OwnedSet) void {
        if (self.len() != other.len()) arcInvariant("ARC owned-set intersection length mismatch");
        self.bits.setIntersection(other.bits);
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
    next_join_point: u32 = 0,

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

    fn freshJoinPointId(self: *ArcTest) LIR.JoinPointId {
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
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

    fn addHostedProc(self: *ArcTest, args: []const LIR.LocalId, ret_layout: layout_mod.Idx) !LIR.LirProcSpecId {
        return try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.span(args),
            .body = null,
            .ret_layout = ret_layout,
            .hosted = .{
                .external_symbol_name = @enumFromInt(1),
                .dispatch_index = 0,
            },
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
            .value = .{ .str_literal = try self.store.insertStringView(text, 0, @intCast(text.len)) },
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
            .variant_index = discriminant,
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
            .op = .{ .tag_payload = .{ .source = source, .payload_idx = 0, .variant_index = 1, .tag_discriminant = 1 } },
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

    fn assignHostedCall(self: *ArcTest, target: LIR.LocalId, args: []const LIR.LocalId, next: LIR.CFStmtId) !LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_call = .{
            .target = target,
            .proc = try self.addHostedProc(args, self.store.getLocal(target).layout_idx),
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
                .runtime_error, .switch_stmt, .loop_continue, .loop_break, .join, .jump => return error.NonLinearPath,
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

test "RC pass-through: non-refcounted i64 block unchanged" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.i64);
    const ret = try f.ret(value);
    const body = try f.assignI64(value, 42, ret);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
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

test "RC shared neutral proc body is rewritten separately for each proc" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const value = try f.local(.str);
    const ret = try f.ret(value);
    const shared_body = try f.assignStr(value, "shared-proc-body", ret);
    const first = try f.addProc(&.{}, shared_body, .str);
    const second = try f.addProc(&.{}, shared_body, .str);

    try f.run();

    const first_body = f.store.getProcSpec(first).body orelse return error.MissingProcBody;
    const second_body = f.store.getProcSpec(second).body orelse return error.MissingProcBody;
    try testing.expect(first_body != shared_body);
    try testing.expect(second_body != shared_body);
    try testing.expect(first_body != second_body);
}

test "RC shared neutral branch tail is rewritten separately for each branch" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const value = try f.local(.str);
    const result = try f.local(.str);
    const ret = try f.ret(result);
    const shared_tail = try f.assignRefLocal(result, value, ret);
    const switch_stmt = try f.switchStmt(cond, shared_tail, shared_tail, null);
    const cond_assign = try f.assignI64(cond, 1, switch_stmt);
    const body = try f.assignStr(value, "shared-branch-tail", cond_assign);
    _ = try f.addProc(&.{}, body, .str);

    try f.run();
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

test "RC hosted call transfers unused refcounted arg to host" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const arg = try f.local(.str);
    const result = try f.local(.i64);
    const ret = try f.ret(result);
    const call = try f.assignHostedCall(result, &.{arg}, ret);
    const body = try f.assignStr(arg, "transferred to host", call);
    _ = try f.addProc(&.{}, body, .i64);
    try f.run();
    try f.expectRc(arg, 0, 0, 0);
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

test "RC nested continuation preserves outer stop when inner branch breaks outward" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const acc = try f.local(f.list_i64);
    const outer_cond = try f.local(.i64);
    const inner_cond = try f.local(.i64);
    const ret = try f.ret(acc);
    const inner_continuation = try f.store.addCFStmt(.runtime_error);
    const inner_switch = try f.switchStmt(inner_cond, inner_continuation, ret, inner_continuation);
    const outer_switch = try f.switchStmt(outer_cond, inner_switch, ret, ret);
    const body = try f.assignList(acc, &.{}, outer_switch);
    _ = try f.addProc(&.{}, body, f.list_i64);
    try f.run();
    try testing.expectEqual(@as(usize, 1), f.countRc(acc, .incref));
    try testing.expect(f.countRc(acc, .decref) >= 1);
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

test "RC join param move excludes old source from loop body ownership" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const source = try f.local(f.list_i64);
    const state = try f.local(f.list_i64);
    const result = try f.local(.i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(result);
    const body = try f.assignI64(result, 1, ret);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize_state = try f.setLocal(state, source, .initialize_join_param, jump);
    const remainder = try f.assignList(source, &.{}, initialize_state);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{state}),
        .body = body,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, join, .i64);
    try f.run();
    try f.expectRc(source, 0, 0, 0);
    try f.expectRc(state, 0, 1, 0);
}

test "RC switch continuation analysis stops at join ownership boundary" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const cond = try f.local(.i64);
    const source = try f.local(f.list_i64);
    const state = try f.local(f.list_i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(state);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize_state = try f.setLocal(state, source, .initialize_join_param, jump);
    const remainder = try f.assignList(source, &.{}, initialize_state);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{state}),
        .body = ret,
        .remainder = remainder,
    } });
    const switch_stmt = try f.switchStmt(cond, join, try f.store.addCFStmt(.runtime_error), ret);
    const body = try f.assignI64(cond, 1, switch_stmt);

    _ = try f.addProc(&.{}, body, f.list_i64);
    try f.run();
    try f.expectRc(source, 0, 0, 0);
}

test "RC join remainder starts from join entry ownership" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const pair_layout = try f.layouts.putStructFields(&[_]layout_mod.StructField{
        .{ .index = 0, .layout = f.list_i64 },
    });
    const source = try f.local(f.list_i64);
    const pair = try f.local(pair_layout);
    const extracted = try f.local(f.list_i64);
    const result = try f.local(f.list_i64);
    const elem = try f.local(.i64);
    const appended = try f.local(f.list_i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(result);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize_result = try f.setLocal(result, appended, .initialize_join_param, jump);
    const append = try f.assignLowLevel(appended, &.{ extracted, elem }, LIR.LowLevel.RcEffect.consumesArgsReturningConsumedArgsRetainingArgs(1, 0), initialize_result);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{result}),
        .body = ret,
        .remainder = append,
    } });
    const extract = try f.assignRefField(extracted, pair, 0, join);
    const make_pair = try f.assignStruct(pair, &.{source}, extract);
    const body = try f.assignList(source, &.{}, make_pair);

    _ = try f.addProc(&.{}, body, f.list_i64);
    try f.run();
    try f.expectRc(pair, 0, 1, 0);
}

test "RC join loop jump releases body-only list but keeps carried state" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const source = try f.local(f.list_i64);
    const state = try f.local(f.list_i64);
    const scratch = try f.local(f.list_i64);
    const next_state = try f.local(f.list_i64);
    const join_id = f.freshJoinPointId();

    const body_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const set_next_state = try f.setLocal(state, next_state, .initialize_join_param, body_jump);
    const next_state_assign = try f.assignList(next_state, &.{}, set_next_state);
    const body = try f.assignList(scratch, &.{}, next_state_assign);

    const initial_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize_state = try f.setLocal(state, source, .initialize_join_param, initial_jump);
    const remainder = try f.assignList(source, &.{}, initialize_state);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{state}),
        .body = body,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, join, .i64);
    try f.run();
    try f.expectRc(scratch, 0, 1, 0);
    try f.expectRc(state, 0, 0, 0);
}

test "RC join loop exit releases body-only list and preserves returned state" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const source = try f.local(f.list_i64);
    const state = try f.local(f.list_i64);
    const scratch = try f.local(f.list_i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(state);
    const body = try f.assignList(scratch, &.{}, ret);

    const initial_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize_state = try f.setLocal(state, source, .initialize_join_param, initial_jump);
    const remainder = try f.assignList(source, &.{}, initialize_state);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{state}),
        .body = body,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, join, f.list_i64);
    try f.run();
    try f.expectRc(scratch, 0, 1, 0);
    try testing.expect(f.countRc(state, .incref) >= 1);
    try testing.expect(f.countRc(state, .decref) >= 1);
}

test "RC iterator join borrowed element used twice gets increfs and no decref" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const elem = try f.local(.str);
    const result = try f.local(.i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(result);
    const body = try f.assignCall(result, &.{ elem, elem }, ret);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = body,
        .remainder = jump,
    } });

    _ = try f.addProc(&.{}, join, .i64);
    try f.run();
    try f.expectRc(elem, 2, 0, 0);
}

test "RC iterator join unused borrowed element has no RC statements" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const elem = try f.local(.str);
    const result = try f.local(.i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(result);
    const body = try f.assignI64(result, 1, ret);
    const jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = LIR.LocalSpan.empty(),
        .body = body,
        .remainder = jump,
    } });

    _ = try f.addProc(&.{}, join, .i64);
    try f.run();
    try f.expectRc(elem, 0, 0, 0);
}

test "RC mutable iterator accumulator replace cleans old state" {
    var f = try ArcTest.init(testing.allocator);
    defer f.deinit();
    const source = try f.local(f.list_i64);
    const acc = try f.local(f.list_i64);
    const next_acc = try f.local(f.list_i64);
    const join_id = f.freshJoinPointId();

    const ret = try f.ret(acc);
    const replace_acc = try f.setLocal(acc, next_acc, .replace_existing, ret);
    const body = try f.assignList(next_acc, &.{}, replace_acc);

    const initial_jump = try f.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    const initialize_acc = try f.setLocal(acc, source, .initialize_join_param, initial_jump);
    const remainder = try f.assignList(source, &.{}, initialize_acc);
    const join = try f.store.addCFStmt(.{ .join = .{
        .id = join_id,
        .params = try f.span(&.{acc}),
        .body = body,
        .remainder = remainder,
    } });

    _ = try f.addProc(&.{}, join, f.list_i64);
    try f.run();
    try testing.expect(f.countRc(acc, .decref) >= 1);
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
