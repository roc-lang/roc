//! Removes LIR switch edges whose discriminants cannot be produced.
//!
//! The pass is intentionally layout-neutral: it tracks values that LIR
//! statements construct or return, but it never changes tag-union layouts,
//! variant slots, or discriminants. It runs before ARC so deleted branches do
//! not need ownership repair.

const std = @import("std");
const core = @import("lir_core");

const LIR = core.LIR;
const LirProgram = core.Program;
const LirStore = core.LirStore;
const Allocator = std.mem.Allocator;

/// Analyze possible tag values and bypass switches that cannot take some of
/// their outgoing edges.
pub fn run(result: *LirProgram.Result) Allocator.Error!void {
    var pass = try Pass.init(result);
    defer pass.deinit();
    try pass.run();
}

const payload_struct_slot = std.math.maxInt(u16);

const PayloadSlot = struct {
    variant: u16,
    payload: u16,
};

const PayloadInfo = struct {
    slot: PayloadSlot,
    tags: TagSet = .{},

    fn deinit(self: *PayloadInfo, allocator: Allocator) void {
        self.tags.deinit(allocator);
    }
};

const FieldInfo = struct {
    field: u16,
    tags: TagSet = .{},

    fn deinit(self: *FieldInfo, allocator: Allocator) void {
        self.tags.deinit(allocator);
    }
};

const TagSet = struct {
    all: bool = false,
    values: std.ArrayList(u16) = .empty,

    fn deinit(self: *TagSet, allocator: Allocator) void {
        self.values.deinit(allocator);
    }

    fn markAll(self: *TagSet, allocator: Allocator) bool {
        if (self.all) return false;
        self.values.clearAndFree(allocator);
        self.all = true;
        return true;
    }

    fn add(self: *TagSet, allocator: Allocator, value: u16) Allocator.Error!bool {
        if (self.all) return false;
        for (self.values.items) |existing| {
            if (existing == value) return false;
        }
        try self.values.append(allocator, value);
        return true;
    }

    fn mergeFrom(self: *TagSet, allocator: Allocator, other: *const TagSet) Allocator.Error!bool {
        if (self.all) return false;
        if (other.all) return self.markAll(allocator);
        var changed = false;
        for (other.values.items) |value| {
            if (try self.add(allocator, value)) changed = true;
        }
        return changed;
    }

    fn contains(self: *const TagSet, value: u16) bool {
        if (self.all) return true;
        for (self.values.items) |existing| {
            if (existing == value) return true;
        }
        return false;
    }

    fn singleton(self: *const TagSet) ?u16 {
        if (self.all or self.values.items.len != 1) return null;
        return self.values.items[0];
    }

    fn hasKnownValues(self: *const TagSet) bool {
        return !self.all and self.values.items.len != 0;
    }
};

const ValueInfo = struct {
    tags: TagSet = .{},
    payloads: std.ArrayList(PayloadInfo) = .empty,
    fields: std.ArrayList(FieldInfo) = .empty,

    fn deinit(self: *ValueInfo, allocator: Allocator) void {
        for (self.payloads.items) |*payload| payload.deinit(allocator);
        self.payloads.deinit(allocator);
        for (self.fields.items) |*field| field.deinit(allocator);
        self.fields.deinit(allocator);
        self.tags.deinit(allocator);
    }

    fn markAll(self: *ValueInfo, allocator: Allocator) bool {
        var changed = self.tags.markAll(allocator);
        if (self.payloads.items.len != 0) {
            for (self.payloads.items) |*payload| payload.deinit(allocator);
            self.payloads.clearAndFree(allocator);
            changed = true;
        }
        if (self.fields.items.len != 0) {
            for (self.fields.items) |*field| field.deinit(allocator);
            self.fields.clearAndFree(allocator);
            changed = true;
        }
        return changed;
    }

    fn mergeFrom(self: *ValueInfo, allocator: Allocator, other: *const ValueInfo) Allocator.Error!bool {
        if (self.tags.all) return false;
        if (other.tags.all) return self.markAll(allocator);
        var changed = try self.tags.mergeFrom(allocator, &other.tags);
        for (other.payloads.items) |*payload| {
            if (try self.mergePayloadTags(allocator, payload.slot, &payload.tags)) changed = true;
        }
        for (other.fields.items) |*field| {
            if (try self.mergeFieldTags(allocator, field.field, &field.tags)) changed = true;
        }
        return changed;
    }

    fn mergePayloadTags(self: *ValueInfo, allocator: Allocator, slot: PayloadSlot, tags: *const TagSet) Allocator.Error!bool {
        if (self.tags.all) return false;
        for (self.payloads.items) |*payload| {
            if (payload.slot.variant == slot.variant and payload.slot.payload == slot.payload) {
                return payload.tags.mergeFrom(allocator, tags);
            }
        }
        try self.payloads.append(allocator, .{ .slot = slot });
        return self.payloads.items[self.payloads.items.len - 1].tags.mergeFrom(allocator, tags);
    }

    fn mergeFieldTags(self: *ValueInfo, allocator: Allocator, field_index: u16, tags: *const TagSet) Allocator.Error!bool {
        if (self.tags.all) return false;
        for (self.fields.items) |*field| {
            if (field.field == field_index) {
                return field.tags.mergeFrom(allocator, tags);
            }
        }
        try self.fields.append(allocator, .{ .field = field_index });
        return self.fields.items[self.fields.items.len - 1].tags.mergeFrom(allocator, tags);
    }

    fn payloadTags(self: *const ValueInfo, slot: PayloadSlot) ?*const TagSet {
        for (self.payloads.items) |*payload| {
            if (payload.slot.variant == slot.variant and payload.slot.payload == slot.payload) {
                return &payload.tags;
            }
        }
        return null;
    }

    fn fieldTags(self: *const ValueInfo, field_index: u16) ?*const TagSet {
        for (self.fields.items) |*field| {
            if (field.field == field_index) return &field.tags;
        }
        return null;
    }
};

const Pass = struct {
    result: *LirProgram.Result,
    store: *LirStore,
    allocator: Allocator,
    local_info: []ValueInfo,
    proc_returns: []ValueInfo,
    visited: std.AutoHashMap(LIR.CFStmtId, void),
    stack: std.ArrayList(LIR.CFStmtId),
    redirects: std.AutoHashMap(LIR.CFStmtId, LIR.CFStmtId),
    use_counts: []u32,

    fn init(result: *LirProgram.Result) Allocator.Error!Pass {
        const allocator = result.store.allocator;
        const local_info = try allocator.alloc(ValueInfo, result.store.locals.items.len);
        errdefer allocator.free(local_info);
        @memset(local_info, .{});

        const proc_returns = try allocator.alloc(ValueInfo, result.store.proc_specs.items.len);
        errdefer allocator.free(proc_returns);
        @memset(proc_returns, .{});

        const use_counts = try allocator.alloc(u32, result.store.locals.items.len);
        errdefer allocator.free(use_counts);
        @memset(use_counts, 0);

        return .{
            .result = result,
            .store = &result.store,
            .allocator = allocator,
            .local_info = local_info,
            .proc_returns = proc_returns,
            .visited = std.AutoHashMap(LIR.CFStmtId, void).init(allocator),
            .stack = .empty,
            .redirects = std.AutoHashMap(LIR.CFStmtId, LIR.CFStmtId).init(allocator),
            .use_counts = use_counts,
        };
    }

    fn deinit(self: *Pass) void {
        self.allocator.free(self.use_counts);
        self.redirects.deinit();
        self.stack.deinit(self.allocator);
        self.visited.deinit();
        for (self.proc_returns) |*ret| ret.deinit(self.allocator);
        self.allocator.free(self.proc_returns);
        for (self.local_info) |*info| info.deinit(self.allocator);
        self.allocator.free(self.local_info);
    }

    fn run(self: *Pass) Allocator.Error!void {
        try self.seedBoundaries();
        try self.analyze();
        try self.collectUseCounts();
        try self.rewriteSwitches();
        try self.removeDeadDiscriminantReads();
        try self.patchRedirects();
    }

    fn seedBoundaries(self: *Pass) Allocator.Error!void {
        for (self.store.proc_specs.items, 0..) |proc, proc_index| {
            if (proc.body == null or proc.hosted != null) {
                _ = self.proc_returns[proc_index].markAll(self.allocator);
            }
            for (self.store.getLocalSpan(proc.args)) |arg| {
                _ = self.local_info[@intFromEnum(arg)].markAll(self.allocator);
            }
        }
    }

    fn analyze(self: *Pass) Allocator.Error!void {
        var changed = true;
        while (changed) {
            changed = false;
            for (self.store.proc_specs.items, 0..) |proc, proc_index| {
                const body = proc.body orelse continue;
                if (try self.analyzeProc(@enumFromInt(@as(u32, @intCast(proc_index))), body)) {
                    changed = true;
                }
            }
        }
    }

    fn analyzeProc(self: *Pass, proc_id: LIR.LirProcSpecId, body: LIR.CFStmtId) Allocator.Error!bool {
        var changed = false;
        self.visited.clearRetainingCapacity();
        self.stack.clearRetainingCapacity();
        try self.stack.append(self.allocator, body);

        while (self.stack.pop()) |current| {
            if (self.visited.contains(current)) continue;
            try self.visited.put(current, {});
            if (try self.analyzeStmt(proc_id, self.store.getCFStmt(current))) changed = true;
        }

        return changed;
    }

    fn analyzeStmt(self: *Pass, proc_id: LIR.LirProcSpecId, stmt: LIR.CFStmt) Allocator.Error!bool {
        var changed = false;
        switch (stmt) {
            .init_uninitialized => |s| {
                if (self.localInfoMut(s.target).markAll(self.allocator)) changed = true;
                try self.pushStmt(s.next);
            },
            .assign_ref => |s| {
                if (try self.mergeRef(s.target, s.op)) changed = true;
                try self.pushStmt(s.next);
            },
            .assign_literal => |s| {
                if (self.localInfoMut(s.target).markAll(self.allocator)) changed = true;
                try self.pushStmt(s.next);
            },
            .assign_call => |s| {
                const callee_index = @intFromEnum(s.proc);
                if (callee_index >= self.proc_returns.len) tagReachabilityInvariant("callee proc id exceeded proc table");
                if (try self.localInfoMut(s.target).mergeFrom(self.allocator, &self.proc_returns[callee_index])) changed = true;
                try self.pushStmt(s.next);
            },
            .assign_call_erased => |s| {
                if (self.localInfoMut(s.target).markAll(self.allocator)) changed = true;
                try self.pushStmt(s.next);
            },
            .assign_packed_erased_fn => |s| {
                if (self.localInfoMut(s.target).markAll(self.allocator)) changed = true;
                try self.pushStmt(s.next);
            },
            .assign_low_level => |s| {
                if (self.localInfoMut(s.target).markAll(self.allocator)) changed = true;
                try self.pushStmt(s.next);
            },
            .assign_list => |s| {
                if (self.localInfoMut(s.target).markAll(self.allocator)) changed = true;
                try self.pushStmt(s.next);
            },
            .assign_struct => |s| {
                const target = self.localInfoMut(s.target);
                for (self.store.getLocalSpan(s.fields), 0..) |field, index| {
                    const source = self.localInfo(field);
                    if (try target.mergeFieldTags(self.allocator, @intCast(index), &source.tags)) changed = true;
                }
                try self.pushStmt(s.next);
            },
            .assign_tag => |s| {
                const target = self.localInfoMut(s.target);
                if (try target.tags.add(self.allocator, s.discriminant)) changed = true;
                if (s.payload) |payload| {
                    const source = self.localInfo(payload);
                    if (try target.mergePayloadTags(self.allocator, .{
                        .variant = s.discriminant,
                        .payload = payload_struct_slot,
                    }, &source.tags)) changed = true;
                    for (source.fields.items) |*field| {
                        if (try target.mergePayloadTags(self.allocator, .{
                            .variant = s.discriminant,
                            .payload = field.field,
                        }, &field.tags)) changed = true;
                    }
                }
                try self.pushStmt(s.next);
            },
            .set_local => |s| {
                if (try self.localInfoMut(s.target).mergeFrom(self.allocator, self.localInfo(s.value))) changed = true;
                try self.pushStmt(s.next);
            },
            .debug => |s| try self.pushStmt(s.next),
            .expect => |s| try self.pushStmt(s.next),
            .comptime_branch_taken => |s| try self.pushStmt(s.next),
            .incref => |s| try self.pushStmt(s.next),
            .decref => |s| try self.pushStmt(s.next),
            .decref_if_initialized => |s| try self.pushStmt(s.next),
            .free => |s| try self.pushStmt(s.next),
            .switch_stmt => |s| {
                for (self.store.getCFSwitchBranches(s.branches)) |branch| try self.pushStmt(branch.body);
                try self.pushStmt(s.default_branch);
                if (s.continuation) |continuation| try self.pushStmt(continuation);
            },
            .switch_initialized_payload => |s| {
                try self.pushStmt(s.initialized_branch);
                try self.pushStmt(s.uninitialized_branch);
            },
            .str_match => |s| {
                try self.pushStmt(s.on_match);
                try self.pushStmt(s.on_miss);
            },
            .str_match_set => |s| {
                for (self.store.getStrMatchArms(s.arms)) |arm| try self.pushStmt(arm.on_match);
                try self.pushStmt(s.on_miss);
            },
            .join => |s| {
                try self.pushStmt(s.body);
                try self.pushStmt(s.remainder);
            },
            .ret => |s| {
                if (try self.proc_returns[@intFromEnum(proc_id)].mergeFrom(self.allocator, self.localInfo(s.value))) changed = true;
            },
            .expect_err,
            .jump,
            .crash,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .loop_continue,
            .loop_break,
            => {},
        }
        return changed;
    }

    fn mergeRef(self: *Pass, target: LIR.LocalId, op: LIR.RefOp) Allocator.Error!bool {
        return switch (op) {
            .local => |source| self.localInfoMut(target).mergeFrom(self.allocator, self.localInfo(source)),
            .nominal => |ref| self.localInfoMut(target).mergeFrom(self.allocator, self.localInfo(ref.backing_ref)),
            .list_reinterpret => |ref| self.localInfoMut(target).mergeFrom(self.allocator, self.localInfo(ref.backing_ref)),
            .discriminant => |ref| self.localInfoMut(target).tags.mergeFrom(self.allocator, &self.localInfo(ref.source).tags),
            .field => |ref| self.mergeFieldRead(target, ref.source, ref.field_idx),
            .tag_payload => |ref| self.mergePayloadRead(target, ref.source, .{
                .variant = ref.tag_discriminant,
                .payload = ref.payload_idx,
            }),
            .tag_payload_struct => |ref| self.mergePayloadRead(target, ref.source, .{
                .variant = ref.tag_discriminant,
                .payload = payload_struct_slot,
            }),
        };
    }

    fn mergeFieldRead(self: *Pass, target: LIR.LocalId, source: LIR.LocalId, field_index: u16) Allocator.Error!bool {
        const source_info = self.localInfo(source);
        if (source_info.tags.all) return self.localInfoMut(target).markAll(self.allocator);
        const tags = source_info.fieldTags(field_index) orelse return false;
        return self.localInfoMut(target).tags.mergeFrom(self.allocator, tags);
    }

    fn mergePayloadRead(self: *Pass, target: LIR.LocalId, source: LIR.LocalId, slot: PayloadSlot) Allocator.Error!bool {
        const source_info = self.localInfo(source);
        if (source_info.tags.all) return self.localInfoMut(target).markAll(self.allocator);
        if (!source_info.tags.contains(slot.variant)) return false;
        const tags = source_info.payloadTags(slot) orelse return false;
        return self.localInfoMut(target).tags.mergeFrom(self.allocator, tags);
    }

    fn collectUseCounts(self: *Pass) Allocator.Error!void {
        @memset(self.use_counts, 0);
        for (self.store.cf_stmts.items) |stmt| {
            try self.countStmtUses(stmt);
        }
    }

    fn countStmtUses(self: *Pass, stmt: LIR.CFStmt) Allocator.Error!void {
        switch (stmt) {
            .assign_ref => |s| {
                switch (s.op) {
                    .local => |source| self.noteUse(source),
                    .discriminant => |ref| self.noteUse(ref.source),
                    .field => |ref| self.noteUse(ref.source),
                    .tag_payload => |ref| self.noteUse(ref.source),
                    .tag_payload_struct => |ref| self.noteUse(ref.source),
                    .list_reinterpret => |ref| self.noteUse(ref.backing_ref),
                    .nominal => |ref| self.noteUse(ref.backing_ref),
                }
            },
            .assign_call => |s| for (self.store.getLocalSpan(s.args)) |arg| self.noteUse(arg),
            .assign_call_erased => |s| {
                self.noteUse(s.closure);
                for (self.store.getLocalSpan(s.args)) |arg| self.noteUse(arg);
            },
            .assign_packed_erased_fn => |s| if (s.capture) |capture| self.noteUse(capture),
            .assign_low_level => |s| for (self.store.getLocalSpan(s.args)) |arg| self.noteUse(arg),
            .assign_list => |s| for (self.store.getLocalSpan(s.elems)) |elem| self.noteUse(elem),
            .assign_struct => |s| for (self.store.getLocalSpan(s.fields)) |field| self.noteUse(field),
            .assign_tag => |s| if (s.payload) |payload| self.noteUse(payload),
            .set_local => |s| self.noteUse(s.value),
            .debug => |s| self.noteUse(s.message),
            .expect => |s| self.noteUse(s.condition),
            .expect_err => |s| self.noteUse(s.message),
            .switch_stmt => |s| self.noteUse(s.cond),
            .switch_initialized_payload => |s| {
                self.noteUse(s.cond);
                self.noteUse(s.payload);
            },
            .str_match => |s| self.noteUse(s.source),
            .str_match_set => |s| self.noteUse(s.source),
            .ret => |s| self.noteUse(s.value),
            .incref => |s| self.noteUse(s.value),
            .decref => |s| self.noteUse(s.value),
            .decref_if_initialized => |s| {
                self.noteUse(s.cond);
                self.noteUse(s.value);
            },
            .free => |s| self.noteUse(s.value),
            .init_uninitialized,
            .assign_literal,
            .comptime_branch_taken,
            .join,
            .jump,
            .crash,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .loop_continue,
            .loop_break,
            => {},
        }
    }

    fn rewriteSwitches(self: *Pass) Allocator.Error!void {
        var stmt_index: usize = 0;
        while (stmt_index < self.store.cf_stmts.items.len) : (stmt_index += 1) {
            const stmt_id: LIR.CFStmtId = @enumFromInt(@as(u32, @intCast(stmt_index)));
            const stmt = self.store.getCFStmt(stmt_id);
            const switch_stmt = switch (stmt) {
                .switch_stmt => |s| s,
                else => continue,
            };
            const tags = &self.localInfo(switch_stmt.cond).tags;
            if (!tags.hasKnownValues()) continue;

            if (tags.singleton()) |value| {
                try self.redirects.put(stmt_id, self.targetForDiscriminant(switch_stmt, value));
                continue;
            }

            const branches = self.store.getCFSwitchBranches(switch_stmt.branches);
            var kept = std.ArrayList(LIR.CFSwitchBranch).empty;
            defer kept.deinit(self.allocator);
            for (branches) |branch| {
                if (branch.value > std.math.maxInt(u16)) continue;
                if (tags.contains(@intCast(branch.value))) {
                    try kept.append(self.allocator, branch);
                }
            }

            if (kept.items.len == 0) {
                try self.redirects.put(stmt_id, switch_stmt.default_branch);
            } else if (kept.items.len != branches.len) {
                self.store.getCFStmtPtr(stmt_id).* = .{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = try self.store.addCFSwitchBranches(kept.items),
                    .default_branch = switch_stmt.default_branch,
                    .continuation = switch_stmt.continuation,
                } };
            }
        }
    }

    fn targetForDiscriminant(self: *Pass, switch_stmt: anytype, value: u16) LIR.CFStmtId {
        for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
            if (branch.value == value) return branch.body;
        }
        return switch_stmt.default_branch;
    }

    fn removeDeadDiscriminantReads(self: *Pass) Allocator.Error!void {
        var stmt_index: usize = 0;
        while (stmt_index < self.store.cf_stmts.items.len) : (stmt_index += 1) {
            const stmt_id: LIR.CFStmtId = @enumFromInt(@as(u32, @intCast(stmt_index)));
            const stmt = self.store.getCFStmt(stmt_id);
            const assign = switch (stmt) {
                .assign_ref => |a| a,
                else => continue,
            };
            switch (assign.op) {
                .discriminant => {},
                else => continue,
            }
            const switch_stmt = switch (self.store.getCFStmt(assign.next)) {
                .switch_stmt => |s| s,
                else => continue,
            };
            if (switch_stmt.cond != assign.target) continue;
            if (self.useCount(assign.target) != 1) continue;
            const next = self.resolveRedirect(assign.next);
            if (next != assign.next) try self.redirects.put(stmt_id, next);
        }
    }

    fn patchRedirects(self: *Pass) Allocator.Error!void {
        if (self.redirects.count() == 0) return;

        for (self.store.proc_specs.items) |*proc| {
            if (proc.body) |body| proc.body = self.resolveRedirect(body);
            for (self.store.getJoinPointSpanMut(proc.join_points)) |*join_point| {
                join_point.body = self.resolveRedirect(join_point.body);
            }
        }

        var stmt_index: usize = 0;
        while (stmt_index < self.store.cf_stmts.items.len) : (stmt_index += 1) {
            const stmt_id: LIR.CFStmtId = @enumFromInt(@as(u32, @intCast(stmt_index)));
            const stmt = self.store.getCFStmtPtr(stmt_id);
            switch (stmt.*) {
                .init_uninitialized => |*s| s.next = self.resolveRedirect(s.next),
                .assign_ref => |*s| s.next = self.resolveRedirect(s.next),
                .assign_literal => |*s| s.next = self.resolveRedirect(s.next),
                .assign_call => |*s| s.next = self.resolveRedirect(s.next),
                .assign_call_erased => |*s| s.next = self.resolveRedirect(s.next),
                .assign_packed_erased_fn => |*s| s.next = self.resolveRedirect(s.next),
                .assign_low_level => |*s| s.next = self.resolveRedirect(s.next),
                .assign_list => |*s| s.next = self.resolveRedirect(s.next),
                .assign_struct => |*s| s.next = self.resolveRedirect(s.next),
                .assign_tag => |*s| s.next = self.resolveRedirect(s.next),
                .set_local => |*s| s.next = self.resolveRedirect(s.next),
                .debug => |*s| s.next = self.resolveRedirect(s.next),
                .expect => |*s| s.next = self.resolveRedirect(s.next),
                .comptime_branch_taken => |*s| s.next = self.resolveRedirect(s.next),
                .incref => |*s| s.next = self.resolveRedirect(s.next),
                .decref => |*s| s.next = self.resolveRedirect(s.next),
                .decref_if_initialized => |*s| s.next = self.resolveRedirect(s.next),
                .free => |*s| s.next = self.resolveRedirect(s.next),
                .switch_stmt => |*s| {
                    for (self.store.getCFSwitchBranchesMut(s.branches)) |*branch| {
                        branch.body = self.resolveRedirect(branch.body);
                    }
                    s.default_branch = self.resolveRedirect(s.default_branch);
                    if (s.continuation) |continuation| s.continuation = self.resolveRedirect(continuation);
                },
                .switch_initialized_payload => |*s| {
                    s.initialized_branch = self.resolveRedirect(s.initialized_branch);
                    s.uninitialized_branch = self.resolveRedirect(s.uninitialized_branch);
                },
                .str_match => |*s| {
                    s.on_match = self.resolveRedirect(s.on_match);
                    s.on_miss = self.resolveRedirect(s.on_miss);
                },
                .str_match_set => |*s| {
                    const arms = self.store.getStrMatchArms(s.arms);
                    const rewritten = try self.allocator.alloc(LIR.StrMatchArm, arms.len);
                    defer self.allocator.free(rewritten);
                    for (arms, rewritten) |arm, *out| {
                        out.* = arm;
                        out.on_match = self.resolveRedirect(arm.on_match);
                    }
                    s.arms = try self.store.addStrMatchArms(rewritten);
                    s.on_miss = self.resolveRedirect(s.on_miss);
                },
                .join => |*s| {
                    s.body = self.resolveRedirect(s.body);
                    s.remainder = self.resolveRedirect(s.remainder);
                },
                .ret,
                .expect_err,
                .jump,
                .crash,
                .runtime_error,
                .comptime_exhaustiveness_failed,
                .loop_continue,
                .loop_break,
                => {},
            }
        }
    }

    fn resolveRedirect(self: *Pass, stmt: LIR.CFStmtId) LIR.CFStmtId {
        var current = stmt;
        var steps: usize = 0;
        while (self.redirects.get(current)) |next| {
            current = next;
            steps += 1;
            if (steps > self.redirects.count()) tagReachabilityInvariant("redirect cycle detected");
        }
        return current;
    }

    fn localInfo(self: *const Pass, local: LIR.LocalId) *const ValueInfo {
        return &self.local_info[@intFromEnum(local)];
    }

    fn localInfoMut(self: *Pass, local: LIR.LocalId) *ValueInfo {
        return &self.local_info[@intFromEnum(local)];
    }

    fn pushStmt(self: *Pass, stmt: LIR.CFStmtId) Allocator.Error!void {
        try self.stack.append(self.allocator, stmt);
    }

    fn noteUse(self: *Pass, local: LIR.LocalId) void {
        self.use_counts[@intFromEnum(local)] += 1;
    }

    fn useCount(self: *Pass, local: LIR.LocalId) u32 {
        return self.use_counts[@intFromEnum(local)];
    }
};

fn tagReachabilityInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("tag reachability invariant violated: {s}", .{message});
    }
    unreachable;
}

test "tag reachability declarations are referenced" {
    std.testing.refAllDecls(@This());
}

const testing = std.testing;
const layout = @import("layout");

const TestProgram = struct {
    result: LirProgram.Result,
    inner_layout: layout.Idx,
    outer_layout: layout.Idx,

    fn init(allocator: Allocator) Allocator.Error!TestProgram {
        var result = try LirProgram.Result.init(allocator, .u64);
        errdefer result.deinit();
        const inner_layout = layout.Idx.bool;
        const outer_layout = try result.layouts.putTagUnion(&[_]layout.Idx{ inner_layout, .zst });
        return .{
            .result = result,
            .inner_layout = inner_layout,
            .outer_layout = outer_layout,
        };
    }

    fn deinit(self: *TestProgram) void {
        self.result.deinit();
    }

    fn local(self: *TestProgram, layout_idx: layout.Idx) Allocator.Error!LIR.LocalId {
        return try self.result.store.addLocal(.{ .layout_idx = layout_idx });
    }
};

test "tag reachability bypasses call result and direct payload switches" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const callee_inner = try f.local(f.inner_layout);
    const callee_ret = try f.local(f.outer_layout);
    const callee_ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = callee_ret } });
    const callee_assign_ret = try store.addCFStmt(.{ .assign_tag = .{
        .target = callee_ret,
        .variant_index = 0,
        .discriminant = 0,
        .payload = callee_inner,
        .next = callee_ret_stmt,
    } });
    const callee_body = try store.addCFStmt(.{ .assign_tag = .{
        .target = callee_inner,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = callee_assign_ret,
    } });
    const callee = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(1),
        .args = LIR.LocalSpan.empty(),
        .body = callee_body,
        .ret_layout = f.outer_layout,
    });

    const caller_ret = try f.local(f.inner_layout);
    const call_result = try f.local(f.outer_layout);
    const outer_disc = try f.local(.u16);
    const payload = try f.local(f.inner_layout);
    const inner_disc = try f.local(.u16);

    const success = try store.addCFStmt(.{ .ret = .{ .value = caller_ret } });
    const bad_outer = try store.addCFStmt(.{ .runtime_error = {} });
    const bad_inner = try store.addCFStmt(.{ .runtime_error = {} });
    const inner_switch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = inner_disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{.{ .value = 0, .body = bad_inner }}),
        .default_branch = success,
    } });
    const inner_disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = inner_disc,
        .op = .{ .discriminant = .{ .source = payload } },
        .next = inner_switch,
    } });
    const read_payload = try store.addCFStmt(.{ .assign_ref = .{
        .target = payload,
        .op = .{ .tag_payload_struct = .{
            .source = call_result,
            .variant_index = 0,
            .tag_discriminant = 0,
        } },
        .next = inner_disc_read,
    } });
    const outer_switch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = outer_disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{.{ .value = 1, .body = bad_outer }}),
        .default_branch = read_payload,
    } });
    const outer_disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = outer_disc,
        .op = .{ .discriminant = .{ .source = call_result } },
        .next = outer_switch,
    } });
    const caller_body = try store.addCFStmt(.{ .assign_call = .{
        .target = call_result,
        .proc = callee,
        .args = LIR.LocalSpan.empty(),
        .next = outer_disc_read,
    } });
    const caller = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(2),
        .args = try store.addLocalSpan(&[_]LIR.LocalId{caller_ret}),
        .body = caller_body,
        .ret_layout = f.inner_layout,
    });
    try f.result.root_procs.append(testing.allocator, caller);

    try run(&f.result);

    const call_stmt = store.getCFStmt(caller_body).assign_call;
    try testing.expectEqual(read_payload, call_stmt.next);

    const payload_stmt = store.getCFStmt(read_payload).assign_ref;
    try testing.expectEqual(success, payload_stmt.next);
}

test "tag reachability tracks per-payload tags through payload structs" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const pair_layout = try f.result.layouts.putStructFields(&[_]layout.StructField{
        .{ .index = 0, .layout = f.inner_layout },
        .{ .index = 1, .layout = f.inner_layout },
    });
    const outer_layout = try f.result.layouts.putTagUnion(&[_]layout.Idx{pair_layout});

    const first = try f.local(f.inner_layout);
    const second = try f.local(f.inner_layout);
    const pair = try f.local(pair_layout);
    const callee_ret = try f.local(outer_layout);
    const callee_ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = callee_ret } });
    const callee_assign_ret = try store.addCFStmt(.{ .assign_tag = .{
        .target = callee_ret,
        .variant_index = 0,
        .discriminant = 0,
        .payload = pair,
        .next = callee_ret_stmt,
    } });
    const fields = try store.addLocalSpan(&[_]LIR.LocalId{ first, second });
    const assign_pair = try store.addCFStmt(.{ .assign_struct = .{
        .target = pair,
        .fields = fields,
        .next = callee_assign_ret,
    } });
    const assign_second = try store.addCFStmt(.{ .assign_tag = .{
        .target = second,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = assign_pair,
    } });
    const callee_body = try store.addCFStmt(.{ .assign_tag = .{
        .target = first,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
        .next = assign_second,
    } });
    const callee = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(4),
        .args = LIR.LocalSpan.empty(),
        .body = callee_body,
        .ret_layout = outer_layout,
    });

    const call_result = try f.local(outer_layout);
    const payload = try f.local(f.inner_layout);
    const disc = try f.local(.u16);
    const success = try store.addCFStmt(.{ .ret = .{ .value = payload } });
    const bad = try store.addCFStmt(.{ .runtime_error = {} });
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{.{ .value = 0, .body = bad }}),
        .default_branch = success,
    } });
    const disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .{ .discriminant = .{ .source = payload } },
        .next = switch_stmt,
    } });
    const read_payload = try store.addCFStmt(.{ .assign_ref = .{
        .target = payload,
        .op = .{ .tag_payload = .{
            .source = call_result,
            .payload_idx = 1,
            .variant_index = 0,
            .tag_discriminant = 0,
        } },
        .next = disc_read,
    } });
    const caller_body = try store.addCFStmt(.{ .assign_call = .{
        .target = call_result,
        .proc = callee,
        .args = LIR.LocalSpan.empty(),
        .next = read_payload,
    } });
    const caller = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(5),
        .args = LIR.LocalSpan.empty(),
        .body = caller_body,
        .ret_layout = f.inner_layout,
    });
    try f.result.root_procs.append(testing.allocator, caller);

    try run(&f.result);

    const payload_stmt = store.getCFStmt(read_payload).assign_ref;
    try testing.expectEqual(success, payload_stmt.next);
}

test "tag reachability removes impossible explicit branches from multi-value sets" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const local = try f.local(f.inner_layout);
    const disc = try f.local(.u16);
    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = local } });
    const branch_zero = try store.addCFStmt(.{ .ret = .{ .value = local } });
    const branch_two = try store.addCFStmt(.{ .runtime_error = {} });
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 0, .body = branch_zero },
            .{ .value = 2, .body = branch_two },
        }),
        .default_branch = ret_stmt,
    } });
    const disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .{ .discriminant = .{ .source = local } },
        .next = switch_stmt,
    } });
    const assign_one = try store.addCFStmt(.{ .assign_tag = .{
        .target = local,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = disc_read,
    } });
    const body = try store.addCFStmt(.{ .assign_tag = .{
        .target = local,
        .variant_index = 0,
        .discriminant = 0,
        .payload = null,
        .next = assign_one,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(3),
        .args = LIR.LocalSpan.empty(),
        .body = body,
        .ret_layout = f.inner_layout,
    });
    try f.result.root_procs.append(testing.allocator, proc);

    try run(&f.result);

    const rewritten = store.getCFStmt(switch_stmt).switch_stmt;
    const branches = store.getCFSwitchBranches(rewritten.branches);
    try testing.expectEqual(@as(usize, 1), branches.len);
    try testing.expectEqual(@as(u64, 0), branches[0].value);
}

test "tag reachability keeps unrelated live discriminant reads" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const source = try f.local(f.outer_layout);
    const other = try f.local(f.outer_layout);
    const disc = try f.local(.u16);
    const other_disc = try f.local(.u16);

    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = disc } });
    const bad = try store.addCFStmt(.{ .runtime_error = {} });
    const other_switch = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = other_disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{.{ .value = 1, .body = ret_stmt }}),
        .default_branch = bad,
    } });
    const disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .{ .discriminant = .{ .source = source } },
        .next = other_switch,
    } });
    const other_disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = other_disc,
        .op = .{ .discriminant = .{ .source = other } },
        .next = disc_read,
    } });
    const assign_other = try store.addCFStmt(.{ .assign_tag = .{
        .target = other,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = other_disc_read,
    } });
    const body = try store.addCFStmt(.{ .assign_tag = .{
        .target = source,
        .variant_index = 1,
        .discriminant = 1,
        .payload = null,
        .next = assign_other,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(6),
        .args = LIR.LocalSpan.empty(),
        .body = body,
        .ret_layout = .u16,
    });
    try f.result.root_procs.append(testing.allocator, proc);

    try run(&f.result);

    const other_disc_stmt = store.getCFStmt(other_disc_read).assign_ref;
    try testing.expectEqual(disc_read, other_disc_stmt.next);

    const disc_stmt = store.getCFStmt(disc_read).assign_ref;
    try testing.expectEqual(ret_stmt, disc_stmt.next);
}

test "tag reachability retains branches for arg-derived tags" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const arg = try f.local(f.outer_layout);
    const disc = try f.local(.u16);

    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = arg } });
    const branch_zero = try store.addCFStmt(.{ .runtime_error = {} });
    const branch_one = try store.addCFStmt(.{ .runtime_error = {} });
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{
            .{ .value = 0, .body = branch_zero },
            .{ .value = 1, .body = branch_one },
        }),
        .default_branch = ret_stmt,
    } });
    const body = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .{ .discriminant = .{ .source = arg } },
        .next = switch_stmt,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(7),
        .args = try store.addLocalSpan(&[_]LIR.LocalId{arg}),
        .body = body,
        .ret_layout = f.outer_layout,
    });
    try f.result.root_procs.append(testing.allocator, proc);

    try run(&f.result);

    const read_stmt = store.getCFStmt(body).assign_ref;
    try testing.expectEqual(switch_stmt, read_stmt.next);

    const rewritten = store.getCFStmt(switch_stmt).switch_stmt;
    const branches = store.getCFSwitchBranches(rewritten.branches);
    try testing.expectEqual(@as(usize, 2), branches.len);
}

test "tag reachability retains branches for hosted call results" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const hosted = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(8),
        .args = LIR.LocalSpan.empty(),
        .body = null,
        .ret_layout = f.outer_layout,
        .hosted = .{
            .symbol = try store.insertString("roc_test_hosted"),
            .dispatch_index = 0,
        },
    });

    const result = try f.local(f.outer_layout);
    const disc = try f.local(.u16);

    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const bad = try store.addCFStmt(.{ .runtime_error = {} });
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{.{ .value = 0, .body = bad }}),
        .default_branch = ret_stmt,
    } });
    const disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .{ .discriminant = .{ .source = result } },
        .next = switch_stmt,
    } });
    const body = try store.addCFStmt(.{ .assign_call = .{
        .target = result,
        .proc = hosted,
        .args = LIR.LocalSpan.empty(),
        .next = disc_read,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(9),
        .args = LIR.LocalSpan.empty(),
        .body = body,
        .ret_layout = f.outer_layout,
    });
    try f.result.root_procs.append(testing.allocator, proc);

    try run(&f.result);

    const call_stmt = store.getCFStmt(body).assign_call;
    try testing.expectEqual(disc_read, call_stmt.next);

    const read_stmt = store.getCFStmt(disc_read).assign_ref;
    try testing.expectEqual(switch_stmt, read_stmt.next);
}

test "tag reachability retains branches for erased call results" {
    var f = try TestProgram.init(testing.allocator);
    defer f.deinit();
    const store = &f.result.store;

    const closure = try f.local(f.outer_layout);
    const result = try f.local(f.outer_layout);
    const disc = try f.local(.u16);

    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const bad = try store.addCFStmt(.{ .runtime_error = {} });
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = disc,
        .branches = try store.addCFSwitchBranches(&[_]LIR.CFSwitchBranch{.{ .value = 0, .body = bad }}),
        .default_branch = ret_stmt,
    } });
    const disc_read = try store.addCFStmt(.{ .assign_ref = .{
        .target = disc,
        .op = .{ .discriminant = .{ .source = result } },
        .next = switch_stmt,
    } });
    const body = try store.addCFStmt(.{ .assign_call_erased = .{
        .target = result,
        .closure = closure,
        .args = LIR.LocalSpan.empty(),
        .next = disc_read,
    } });
    const proc = try store.addProcSpec(.{
        .name = LIR.Symbol.fromRaw(10),
        .args = try store.addLocalSpan(&[_]LIR.LocalId{closure}),
        .body = body,
        .ret_layout = f.outer_layout,
    });
    try f.result.root_procs.append(testing.allocator, proc);

    try run(&f.result);

    const call_stmt = store.getCFStmt(body).assign_call_erased;
    try testing.expectEqual(disc_read, call_stmt.next);

    const read_stmt = store.getCFStmt(disc_read).assign_ref;
    try testing.expectEqual(switch_stmt, read_stmt.next);
}
