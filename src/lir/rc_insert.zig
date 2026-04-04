//! Statement-only LIR reference-count insertion.
//!
//! This pass operates purely over explicit locals and explicit control-flow.
//! Ownership must already be represented precisely in `CFStmt` result semantics
//! before codegen sees the IR.

const std = @import("std");
const builtin = @import("builtin");
const layout_mod = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;
const LayoutIdx = layout_mod.Idx;

const CFStmt = LIR.CFStmt;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const LirProcSpecId = LIR.LirProcSpecId;
const AssignRefStmt = std.meta.TagPayload(CFStmt, .assign_ref);
const AssignSymbolStmt = std.meta.TagPayload(CFStmt, .assign_symbol);
const AssignLiteralStmt = std.meta.TagPayload(CFStmt, .assign_literal);
const AssignCallStmt = std.meta.TagPayload(CFStmt, .assign_call);
const AssignLowLevelStmt = std.meta.TagPayload(CFStmt, .assign_low_level);
const AssignListStmt = std.meta.TagPayload(CFStmt, .assign_list);
const AssignStructStmt = std.meta.TagPayload(CFStmt, .assign_struct);
const AssignTagStmt = std.meta.TagPayload(CFStmt, .assign_tag);
const UseKind = enum {
    borrow,
    consume,
};

const UseSummary = struct {
    borrow_count: u32 = 0,
    consume_count: u32 = 0,

    fn total(self: UseSummary) u32 {
        return self.borrow_count + self.consume_count;
    }
};

const AliasSource = struct {
    owner_key: u64,
    forwards_ownership: bool,
};

const OwnedInputSpan = struct {
    start: u32,
    len: u16,
};

const JoinInfo = struct {
    params: LIR.LocalSpan,
    body: CFStmtId,
    remainder: CFStmtId,
};

const LocalDefinitionKind = union(enum) {
    proc_param,
    join_param,
    stmt_result: LIR.ResultSemantics,
};

/// Inserts explicit reference-count statements into statement-only LIR.
pub const RcInsertPass = struct {
    allocator: Allocator,
    store: *LirStore,
    layout_store: *const layout_mod.Store,

    symbol_use_counts: std.AutoHashMap(u64, u32),
    symbol_use_summaries: std.AutoHashMap(u64, UseSummary),
    direct_use_summaries: std.AutoHashMap(u64, UseSummary),
    direct_forwarding_alias_borrows: std.AutoHashMap(u64, u32),
    local_alias_sources: std.AutoHashMap(u64, AliasSource),
    local_owned_input_spans: std.AutoHashMap(u64, OwnedInputSpan),
    owned_input_locals: std.ArrayListUnmanaged(LocalId),
    local_definition_kinds: std.AutoHashMap(u64, LocalDefinitionKind),
    owned_proc_params: std.AutoHashMap(u64, void),
    promoted_fresh_join_params: std.AutoHashMap(u64, void),
    owning_join_params: std.AutoHashMap(u64, void),
    active_join_params: std.AutoHashMap(u32, LIR.LocalSpan),
    joins_by_id: std.AutoHashMap(u32, JoinInfo),
    proc_param_use_kinds: std.ArrayListUnmanaged([]UseKind),

    /// Initializes an RC insertion pass over the provided LIR store.
    pub fn init(allocator: Allocator, store: *LirStore, layout_store: *const layout_mod.Store) Allocator.Error!RcInsertPass {
        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = layout_store,
            .symbol_use_counts = std.AutoHashMap(u64, u32).init(allocator),
            .symbol_use_summaries = std.AutoHashMap(u64, UseSummary).init(allocator),
            .direct_use_summaries = std.AutoHashMap(u64, UseSummary).init(allocator),
            .direct_forwarding_alias_borrows = std.AutoHashMap(u64, u32).init(allocator),
            .local_alias_sources = std.AutoHashMap(u64, AliasSource).init(allocator),
            .local_owned_input_spans = std.AutoHashMap(u64, OwnedInputSpan).init(allocator),
            .owned_input_locals = .empty,
            .local_definition_kinds = std.AutoHashMap(u64, LocalDefinitionKind).init(allocator),
            .owned_proc_params = std.AutoHashMap(u64, void).init(allocator),
            .promoted_fresh_join_params = std.AutoHashMap(u64, void).init(allocator),
            .owning_join_params = std.AutoHashMap(u64, void).init(allocator),
            .active_join_params = std.AutoHashMap(u32, LIR.LocalSpan).init(allocator),
            .joins_by_id = std.AutoHashMap(u32, JoinInfo).init(allocator),
            .proc_param_use_kinds = .empty,
        };
    }

    /// Releases all temporary analysis state owned by this pass.
    pub fn deinit(self: *RcInsertPass) void {
        self.symbol_use_counts.deinit();
        self.symbol_use_summaries.deinit();
        self.direct_use_summaries.deinit();
        self.direct_forwarding_alias_borrows.deinit();
        self.local_alias_sources.deinit();
        self.local_owned_input_spans.deinit();
        self.owned_input_locals.deinit(self.allocator);
        self.local_definition_kinds.deinit();
        self.owned_proc_params.deinit();
        self.promoted_fresh_join_params.deinit();
        self.owning_join_params.deinit();
        self.active_join_params.deinit();
        self.joins_by_id.deinit();
        for (self.proc_param_use_kinds.items) |use_kinds| {
            self.allocator.free(use_kinds);
        }
        self.proc_param_use_kinds.deinit(self.allocator);
    }

    /// Inserts RC statements for one proc body.
    pub fn insertRcOpsForProc(self: *RcInsertPass, proc_id: LirProcSpecId) Allocator.Error!void {
        try self.rebuildProcParamUseKinds();
        try self.insertRcOpsForProcWithPreparedParamUseKinds(proc_id);
    }

    fn insertRcOpsForProcWithPreparedParamUseKinds(self: *RcInsertPass, proc_id: LirProcSpecId) Allocator.Error!void {
        self.clearAnalysisState();

        const proc = self.store.getProcSpec(proc_id);
        const proc_param_use_kinds = if (@intFromEnum(proc_id) < self.proc_param_use_kinds.items.len)
            self.proc_param_use_kinds.items[@intFromEnum(proc_id)]
        else
            &.{};
        try self.recordProcParamDefinitions(proc.args, proc_param_use_kinds);
        try self.collectAliasSemanticsInStmt(proc.body);
        try self.inferJoinParamAliasSources(proc.body);
        try self.inferOwningJoinParams(proc.body);
        try self.countUsesInStmt(proc.body);
        self.store.getProcSpecPtr(proc_id).body = try self.processStmt(proc.body);
    }

    /// Inserts RC statements for every proc currently stored in the LIR store.
    pub fn insertRcOpsForAllProcs(self: *RcInsertPass) Allocator.Error!void {
        try self.rebuildProcParamUseKinds();
        for (0..self.store.getProcSpecs().len) |i| {
            try self.insertRcOpsForProcWithPreparedParamUseKinds(@enumFromInt(@as(u32, @intCast(i))));
        }
    }

    fn clearAnalysisState(self: *RcInsertPass) void {
        self.symbol_use_counts.clearRetainingCapacity();
        self.symbol_use_summaries.clearRetainingCapacity();
        self.direct_use_summaries.clearRetainingCapacity();
        self.direct_forwarding_alias_borrows.clearRetainingCapacity();
        self.local_alias_sources.clearRetainingCapacity();
        self.local_owned_input_spans.clearRetainingCapacity();
        self.owned_input_locals.clearRetainingCapacity();
        self.local_definition_kinds.clearRetainingCapacity();
        self.owned_proc_params.clearRetainingCapacity();
        self.promoted_fresh_join_params.clearRetainingCapacity();
        self.owning_join_params.clearRetainingCapacity();
        self.active_join_params.clearRetainingCapacity();
        self.joins_by_id.clearRetainingCapacity();
    }

    fn cloneSwitchBranches(self: *RcInsertPass, span: LIR.CFSwitchBranchSpan) Allocator.Error![]LIR.CFSwitchBranch {
        return self.allocator.dupe(LIR.CFSwitchBranch, self.store.getCFSwitchBranches(span));
    }

    fn rebuildProcParamUseKinds(self: *RcInsertPass) Allocator.Error!void {
        try self.normalizeBorrowedProcReturns();
        try self.normalizeFreshProcReturns();

        for (self.proc_param_use_kinds.items) |use_kinds| {
            self.allocator.free(use_kinds);
        }
        self.proc_param_use_kinds.clearRetainingCapacity();

        const proc_specs = self.store.getProcSpecs();
        try self.proc_param_use_kinds.resize(self.allocator, proc_specs.len);
        for (self.proc_param_use_kinds.items) |*use_kinds| {
            use_kinds.* = &.{};
        }

        var changed = true;
        var iterations: u32 = 0;
        while (changed) {
            changed = false;
            iterations += 1;
            if (builtin.mode == .Debug and iterations > 64) {
                std.debug.panic(
                    "RcInsertPass invariant violated: proc param ownership fixed point did not converge after {d} iterations",
                    .{iterations},
                );
            }

            for (proc_specs, 0..) |proc, proc_index| {
                const new_use_kinds = try self.analyzeProcParamUseKinds(proc_index, proc);
                const old_use_kinds = self.proc_param_use_kinds.items[proc_index];
                if (std.mem.eql(UseKind, old_use_kinds, new_use_kinds)) {
                    self.allocator.free(new_use_kinds);
                    continue;
                }

                self.allocator.free(old_use_kinds);
                self.proc_param_use_kinds.items[proc_index] = new_use_kinds;
                changed = true;
            }
        }
    }

    fn normalizeFreshProcReturns(self: *RcInsertPass) Allocator.Error!void {
        for (0..self.store.getProcSpecs().len) |proc_index| {
            const proc_id: LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
            const proc_ptr = self.store.getProcSpecPtr(proc_id);
            if (proc_ptr.result_contract != .fresh) continue;

            self.clearAnalysisState();
            try self.recordProcParamDefinitions(proc_ptr.args, &.{});
            try self.collectAliasSemanticsInStmt(proc_ptr.body);
            try self.inferJoinParamAliasSources(proc_ptr.body);
            try self.markFreshReturnJoinParamsInStmt(proc_ptr.body);
            try self.inferOwningJoinParams(proc_ptr.body);
            proc_ptr.body = try self.normalizeFreshProcReturnsInStmt(proc_ptr.body);
        }
    }

    fn localCarriesOwnedValue(self: *const RcInsertPass, local: LocalId) bool {
        const initial_def_kind = self.local_definition_kinds.get(localKey(local)) orelse std.debug.panic(
            "RcInsertPass invariant violated: missing definition kind for local {d}",
            .{@intFromEnum(local)},
        );
        switch (initial_def_kind) {
            .join_param => {
                return self.promoted_fresh_join_params.contains(localKey(local)) or
                    self.owning_join_params.contains(localKey(local));
            },
            else => {},
        }

        var current = local;
        var steps: u32 = 0;

        while (self.local_alias_sources.get(localKey(current))) |source| {
            if (!source.forwards_ownership) return false;
            current = @enumFromInt(@as(u32, @intCast(source.owner_key)));
            steps += 1;
            if (builtin.mode == .Debug and steps > 1024) {
                std.debug.panic(
                    "RcInsertPass invariant violated: ownership-carrying walk did not converge for local {d}",
                    .{@intFromEnum(local)},
                );
            }
        }

        const def_kind = self.local_definition_kinds.get(localKey(current)) orelse std.debug.panic(
            "RcInsertPass invariant violated: missing definition kind for local {d}",
            .{@intFromEnum(current)},
        );
        return switch (def_kind) {
            .proc_param => self.owned_proc_params.contains(localKey(current)),
            .join_param => self.promoted_fresh_join_params.contains(localKey(current)) or
                self.owning_join_params.contains(localKey(current)),
            .stmt_result => |semantics| semantics == .fresh,
        };
    }

    fn inferOwningJoinParams(self: *RcInsertPass, stmt_id: CFStmtId) Allocator.Error!void {
        var changed = true;
        var iterations: u32 = 0;
        while (changed) {
            changed = false;
            iterations += 1;
            if (builtin.mode == .Debug and iterations > 64) {
                std.debug.panic(
                    "RcInsertPass invariant violated: join-param ownership fixed point did not converge after {d} iterations",
                    .{iterations},
                );
            }
            try self.markOwningJoinParamsInStmt(stmt_id, &changed);
        }
    }

    fn inferJoinParamAliasSources(self: *RcInsertPass, stmt_id: CFStmtId) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try self.inferJoinParamAliasSources(assign.next),
            .assign_ref => |assign| try self.inferJoinParamAliasSources(assign.next),
            .assign_literal => |assign| try self.inferJoinParamAliasSources(assign.next),
            .assign_call => |assign| try self.inferJoinParamAliasSources(assign.next),
            .assign_low_level => |assign| try self.inferJoinParamAliasSources(assign.next),
            .assign_list => |assign| try self.inferJoinParamAliasSources(assign.next),
            .assign_struct => |assign| try self.inferJoinParamAliasSources(assign.next),
            .assign_tag => |assign| try self.inferJoinParamAliasSources(assign.next),
            .debug => |debug_stmt| try self.inferJoinParamAliasSources(debug_stmt.next),
            .expect => |expect_stmt| try self.inferJoinParamAliasSources(expect_stmt.next),
            .incref => |inc| try self.inferJoinParamAliasSources(inc.next),
            .decref => |dec| try self.inferJoinParamAliasSources(dec.next),
            .free => |free_stmt| try self.inferJoinParamAliasSources(free_stmt.next),
            .switch_stmt => |switch_stmt| {
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.inferJoinParamAliasSources(branch.body);
                }
                try self.inferJoinParamAliasSources(switch_stmt.default_branch);
            },
            .borrow_scope => |scope| {
                try self.inferJoinParamAliasSources(scope.body);
                try self.inferJoinParamAliasSources(scope.remainder);
            },
            .join => |join| {
                try self.inferJoinParamAliasSources(join.body);
                try self.inferJoinParamAliasSources(join.remainder);
                try self.inferAliasSourcesForJoin(join);
            },
            .jump, .ret, .runtime_error, .scope_exit, .crash => {},
        }
    }

    fn inferAliasSourcesForJoin(
        self: *RcInsertPass,
        join: std.meta.TagPayload(CFStmt, .join),
    ) Allocator.Error!void {
        const params = self.store.getLocalSpan(join.params);
        if (params.len == 0) return;

        const seen = try self.allocator.alloc(bool, params.len);
        defer self.allocator.free(seen);
        const consistent = try self.allocator.alloc(bool, params.len);
        defer self.allocator.free(consistent);
        const roots = try self.allocator.alloc(?LocalId, params.len);
        defer self.allocator.free(roots);
        @memset(seen, false);
        @memset(consistent, true);
        for (roots) |*root| root.* = null;

        try self.scanIncomingJumpAliasRoots(join.remainder, join.id, params, seen, consistent, roots);

        for (params, seen, consistent, roots) |param, param_seen, is_consistent, maybe_root| {
            if (!param_seen or !is_consistent) continue;
            const root = maybe_root orelse continue;
            if (root == param) continue;
            const key = localKey(param);
            const gop = try self.local_alias_sources.getOrPut(key);
            if (gop.found_existing) {
                if (builtin.mode == .Debug and (gop.value_ptr.owner_key != localKey(root) or !gop.value_ptr.forwards_ownership)) {
                    std.debug.panic(
                        "RcInsertPass invariant violated: join param {d} inferred alias root {d} conflicts with existing alias source",
                        .{ @intFromEnum(param), @intFromEnum(root) },
                    );
                }
            } else {
                gop.value_ptr.* = .{
                    .owner_key = localKey(root),
                    .forwards_ownership = true,
                };
            }
        }
    }

    fn scanIncomingJumpAliasRoots(
        self: *RcInsertPass,
        stmt_id: CFStmtId,
        join_id: LIR.JoinPointId,
        params: []const LocalId,
        seen: []bool,
        consistent: []bool,
        roots: []?LocalId,
    ) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try self.scanIncomingJumpAliasRoots(assign.next, join_id, params, seen, consistent, roots),
            .assign_ref => |assign| try self.scanIncomingJumpAliasRoots(assign.next, join_id, params, seen, consistent, roots),
            .assign_literal => |assign| try self.scanIncomingJumpAliasRoots(assign.next, join_id, params, seen, consistent, roots),
            .assign_call => |assign| try self.scanIncomingJumpAliasRoots(assign.next, join_id, params, seen, consistent, roots),
            .assign_low_level => |assign| try self.scanIncomingJumpAliasRoots(assign.next, join_id, params, seen, consistent, roots),
            .assign_list => |assign| try self.scanIncomingJumpAliasRoots(assign.next, join_id, params, seen, consistent, roots),
            .assign_struct => |assign| try self.scanIncomingJumpAliasRoots(assign.next, join_id, params, seen, consistent, roots),
            .assign_tag => |assign| try self.scanIncomingJumpAliasRoots(assign.next, join_id, params, seen, consistent, roots),
            .debug => |debug_stmt| try self.scanIncomingJumpAliasRoots(debug_stmt.next, join_id, params, seen, consistent, roots),
            .expect => |expect_stmt| try self.scanIncomingJumpAliasRoots(expect_stmt.next, join_id, params, seen, consistent, roots),
            .incref => |inc| try self.scanIncomingJumpAliasRoots(inc.next, join_id, params, seen, consistent, roots),
            .decref => |dec| try self.scanIncomingJumpAliasRoots(dec.next, join_id, params, seen, consistent, roots),
            .free => |free_stmt| try self.scanIncomingJumpAliasRoots(free_stmt.next, join_id, params, seen, consistent, roots),
            .switch_stmt => |switch_stmt| {
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.scanIncomingJumpAliasRoots(branch.body, join_id, params, seen, consistent, roots);
                }
                try self.scanIncomingJumpAliasRoots(switch_stmt.default_branch, join_id, params, seen, consistent, roots);
            },
            .borrow_scope => |scope| {
                try self.scanIncomingJumpAliasRoots(scope.body, join_id, params, seen, consistent, roots);
                try self.scanIncomingJumpAliasRoots(scope.remainder, join_id, params, seen, consistent, roots);
            },
            .join => |nested_join| {
                try self.scanIncomingJumpAliasRoots(nested_join.body, join_id, params, seen, consistent, roots);
                try self.scanIncomingJumpAliasRoots(nested_join.remainder, join_id, params, seen, consistent, roots);
            },
            .jump => |jump| {
                if (jump.target != join_id) return;

                const args = self.store.getLocalSpan(jump.args);
                if (builtin.mode == .Debug and args.len != params.len) {
                    std.debug.panic(
                        "RcInsertPass invariant violated: jump to join {d} passed {d} args while alias-root scan expected {d}",
                        .{ @intFromEnum(join_id), args.len, params.len },
                    );
                }

                for (args, 0..) |arg, i| {
                    const root = self.forwardingAliasRepresentative(arg);
                    if (root) |resolved_root| {
                        // Treat self-recursive loop backedges as neutral. This matches the
                        // `cor` model, where the recursive env slot is the same carried
                        // binding across iterations rather than a conflicting new owner.
                        if (resolved_root == params[i]) continue;
                    }

                    seen[i] = true;
                    if (!consistent[i]) continue;
                    if (roots[i]) |existing| {
                        if (root == null or root.? != existing) {
                            consistent[i] = false;
                            roots[i] = null;
                        }
                    } else {
                        roots[i] = root;
                        if (root == null) consistent[i] = false;
                    }
                }
            },
            .ret, .runtime_error, .scope_exit, .crash => {},
        }
    }

    fn forwardingAliasRepresentative(self: *const RcInsertPass, local: LocalId) ?LocalId {
        var current = local;
        var steps: u32 = 0;
        while (self.local_alias_sources.get(localKey(current))) |source| {
            if (!source.forwards_ownership) return null;
            current = @enumFromInt(@as(u32, @intCast(source.owner_key)));
            steps += 1;
            if (builtin.mode == .Debug and steps > 1024) {
                std.debug.panic(
                    "RcInsertPass invariant violated: forwarding alias representative walk did not converge for local {d}",
                    .{@intFromEnum(local)},
                );
            }
        }
        return current;
    }

    fn ownershipRootForConsumedValue(self: *const RcInsertPass, local: LocalId) ?LocalId {
        if (!self.layoutNeedsRc(self.localLayout(local))) return null;

        var current = local;
        var steps: u32 = 0;
        while (self.local_alias_sources.get(localKey(current))) |source| {
            if (!source.forwards_ownership) return null;
            current = @enumFromInt(@as(u32, @intCast(source.owner_key)));
            steps += 1;
            if (builtin.mode == .Debug and steps > 1024) {
                std.debug.panic(
                    "RcInsertPass invariant violated: consumed-value ownership root walk did not converge for local {d}",
                    .{@intFromEnum(local)},
                );
            }
        }

        return current;
    }

    fn recordOwnedInputRoots(self: *RcInsertPass, target: LocalId, inputs: []const LocalId) Allocator.Error!void {
        if (!self.layoutNeedsRc(self.localLayout(target))) return;

        const start = @as(u32, @intCast(self.owned_input_locals.items.len));
        var added: u16 = 0;
        for (inputs) |input| {
            const root = self.ownershipRootForConsumedValue(input) orelse continue;

            var seen = false;
            for (self.owned_input_locals.items[start..]) |existing| {
                if (existing == root) {
                    seen = true;
                    break;
                }
            }
            if (seen) continue;

            try self.owned_input_locals.append(self.allocator, root);
            added += 1;
        }

        if (added == 0) return;
        try self.local_owned_input_spans.put(localKey(target), .{
            .start = start,
            .len = added,
        });
    }

    fn ownedInputRoots(self: *const RcInsertPass, local: LocalId) []const LocalId {
        const span = self.local_owned_input_spans.get(localKey(local)) orelse return &.{};
        const start: usize = span.start;
        return self.owned_input_locals.items[start..][0..span.len];
    }

    fn markOwningJoinParamsInStmt(self: *RcInsertPass, stmt_id: CFStmtId, changed: *bool) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try self.markOwningJoinParamsInStmt(assign.next, changed),
            .assign_ref => |assign| try self.markOwningJoinParamsInStmt(assign.next, changed),
            .assign_literal => |assign| try self.markOwningJoinParamsInStmt(assign.next, changed),
            .assign_call => |assign| try self.markOwningJoinParamsInStmt(assign.next, changed),
            .assign_low_level => |assign| try self.markOwningJoinParamsInStmt(assign.next, changed),
            .assign_list => |assign| try self.markOwningJoinParamsInStmt(assign.next, changed),
            .assign_struct => |assign| try self.markOwningJoinParamsInStmt(assign.next, changed),
            .assign_tag => |assign| try self.markOwningJoinParamsInStmt(assign.next, changed),
            .debug => |debug_stmt| try self.markOwningJoinParamsInStmt(debug_stmt.next, changed),
            .expect => |expect_stmt| try self.markOwningJoinParamsInStmt(expect_stmt.next, changed),
            .incref => |inc| try self.markOwningJoinParamsInStmt(inc.next, changed),
            .decref => |dec| try self.markOwningJoinParamsInStmt(dec.next, changed),
            .free => |free_stmt| try self.markOwningJoinParamsInStmt(free_stmt.next, changed),
            .switch_stmt => |switch_stmt| {
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.markOwningJoinParamsInStmt(branch.body, changed);
                }
                try self.markOwningJoinParamsInStmt(switch_stmt.default_branch, changed);
            },
            .borrow_scope => |scope| {
                try self.markOwningJoinParamsInStmt(scope.body, changed);
                try self.markOwningJoinParamsInStmt(scope.remainder, changed);
            },
            .join => |join| {
                try self.markOwningJoinParamsForJoin(join, changed);
                try self.markOwningJoinParamsInStmt(join.body, changed);
                try self.markOwningJoinParamsInStmt(join.remainder, changed);
            },
            .jump, .ret, .runtime_error, .scope_exit, .crash => {},
        }
    }

    fn markOwningJoinParamsForJoin(
        self: *RcInsertPass,
        join: std.meta.TagPayload(CFStmt, .join),
        changed: *bool,
    ) Allocator.Error!void {
        const params = self.store.getLocalSpan(join.params);
        if (params.len == 0) return;

        const seen = try self.allocator.alloc(bool, params.len);
        defer self.allocator.free(seen);
        const all_owned = try self.allocator.alloc(bool, params.len);
        defer self.allocator.free(all_owned);
        const incoming_args = try self.allocator.alloc(?LocalId, params.len);
        defer self.allocator.free(incoming_args);
        var incoming_jump_count: u32 = 0;
        @memset(seen, false);
        @memset(all_owned, true);
        for (incoming_args) |*arg| arg.* = null;

        try self.scanIncomingJumpsForJoin(join.body, join.id, params, seen, all_owned, &incoming_jump_count, incoming_args);
        try self.scanIncomingJumpsForJoin(join.remainder, join.id, params, seen, all_owned, &incoming_jump_count, incoming_args);

        for (params, seen, all_owned, incoming_args) |param, param_seen, param_owned, maybe_incoming_arg| {
            if (!param_seen or !param_owned) continue;

            const carries_into_owned_slot = if (incoming_jump_count > 1)
                true
            else if (maybe_incoming_arg) |incoming_arg|
                self.localDefinitionIntroducesFreshOwnership(incoming_arg)
            else
                false;

            if (!carries_into_owned_slot) continue;
            const gop = try self.owning_join_params.getOrPut(localKey(param));
            if (!gop.found_existing) {
                gop.value_ptr.* = {};
                changed.* = true;
            }
        }
    }

    fn localDefinitionIntroducesFreshOwnership(self: *const RcInsertPass, local: LocalId) bool {
        const root = self.ownershipRootForConsumedValue(local) orelse return false;
        const def_kind = self.local_definition_kinds.get(localKey(root)) orelse std.debug.panic(
            "RcInsertPass invariant violated: missing definition kind for local {d}",
            .{@intFromEnum(root)},
        );
        return switch (def_kind) {
            .stmt_result => |semantics| semantics == .fresh,
            .join_param => self.localCarriesOwnedValue(root),
            .proc_param => false,
        };
    }

    fn scanIncomingJumpsForJoin(
        self: *RcInsertPass,
        stmt_id: CFStmtId,
        join_id: LIR.JoinPointId,
        params: []const LocalId,
        seen: []bool,
        all_owned: []bool,
        incoming_jump_count: *u32,
        incoming_args: []?LocalId,
    ) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try self.scanIncomingJumpsForJoin(assign.next, join_id, params, seen, all_owned, incoming_jump_count, incoming_args),
            .assign_ref => |assign| try self.scanIncomingJumpsForJoin(assign.next, join_id, params, seen, all_owned, incoming_jump_count, incoming_args),
            .assign_literal => |assign| try self.scanIncomingJumpsForJoin(assign.next, join_id, params, seen, all_owned, incoming_jump_count, incoming_args),
            .assign_call => |assign| try self.scanIncomingJumpsForJoin(assign.next, join_id, params, seen, all_owned, incoming_jump_count, incoming_args),
            .assign_low_level => |assign| try self.scanIncomingJumpsForJoin(assign.next, join_id, params, seen, all_owned, incoming_jump_count, incoming_args),
            .assign_list => |assign| try self.scanIncomingJumpsForJoin(assign.next, join_id, params, seen, all_owned, incoming_jump_count, incoming_args),
            .assign_struct => |assign| try self.scanIncomingJumpsForJoin(assign.next, join_id, params, seen, all_owned, incoming_jump_count, incoming_args),
            .assign_tag => |assign| try self.scanIncomingJumpsForJoin(assign.next, join_id, params, seen, all_owned, incoming_jump_count, incoming_args),
            .debug => |debug_stmt| try self.scanIncomingJumpsForJoin(debug_stmt.next, join_id, params, seen, all_owned, incoming_jump_count, incoming_args),
            .expect => |expect_stmt| try self.scanIncomingJumpsForJoin(expect_stmt.next, join_id, params, seen, all_owned, incoming_jump_count, incoming_args),
            .incref => |inc| try self.scanIncomingJumpsForJoin(inc.next, join_id, params, seen, all_owned, incoming_jump_count, incoming_args),
            .decref => |dec| try self.scanIncomingJumpsForJoin(dec.next, join_id, params, seen, all_owned, incoming_jump_count, incoming_args),
            .free => |free_stmt| try self.scanIncomingJumpsForJoin(free_stmt.next, join_id, params, seen, all_owned, incoming_jump_count, incoming_args),
            .switch_stmt => |switch_stmt| {
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.scanIncomingJumpsForJoin(branch.body, join_id, params, seen, all_owned, incoming_jump_count, incoming_args);
                }
                try self.scanIncomingJumpsForJoin(switch_stmt.default_branch, join_id, params, seen, all_owned, incoming_jump_count, incoming_args);
            },
            .borrow_scope => |scope| {
                try self.scanIncomingJumpsForJoin(scope.body, join_id, params, seen, all_owned, incoming_jump_count, incoming_args);
                try self.scanIncomingJumpsForJoin(scope.remainder, join_id, params, seen, all_owned, incoming_jump_count, incoming_args);
            },
            .join => |nested_join| {
                try self.scanIncomingJumpsForJoin(nested_join.body, join_id, params, seen, all_owned, incoming_jump_count, incoming_args);
                try self.scanIncomingJumpsForJoin(nested_join.remainder, join_id, params, seen, all_owned, incoming_jump_count, incoming_args);
            },
            .jump => |jump| {
                if (jump.target != join_id) return;
                if (incoming_jump_count.* == 0) {
                    const args = self.store.getLocalSpan(jump.args);
                    if (builtin.mode == .Debug and args.len != incoming_args.len) {
                        std.debug.panic(
                            "RcInsertPass invariant violated: jump to join {d} passed {d} args while incoming-arg scan expected {d}",
                            .{ @intFromEnum(join_id), args.len, incoming_args.len },
                        );
                    }
                    for (args, 0..) |arg, i| {
                        incoming_args[i] = arg;
                    }
                }
                incoming_jump_count.* += 1;

                const args = self.store.getLocalSpan(jump.args);
                if (builtin.mode == .Debug and args.len != params.len) {
                    std.debug.panic(
                        "RcInsertPass invariant violated: jump to join {d} passed {d} args while ownership scan expected {d}",
                        .{ @intFromEnum(join_id), args.len, params.len },
                    );
                }

                for (args, params, 0..) |arg, param, i| {
                    seen[i] = true;
                    if (self.forwardingAliasRepresentative(arg)) |resolved_root| {
                        if (resolved_root == param) continue;
                        if (self.forwardingAliasRepresentative(param)) |param_root| {
                            if (resolved_root == param_root) continue;
                        }
                    }
                    const carries_ownership = self.localCarriesOwnedValue(arg);
                    all_owned[i] = all_owned[i] and carries_ownership;
                }
            },
            .ret, .runtime_error, .scope_exit, .crash => {},
        }
    }

    fn returnedJoinParam(self: *RcInsertPass, params: []const LocalId, local: LocalId) ?LocalId {
        for (params) |param| {
            if (param == local) return param;
        }

        var current = local;
        var steps: u32 = 0;
        while (self.local_alias_sources.get(localKey(current))) |source| {
            const owner: LocalId = @enumFromInt(@as(u32, @intCast(source.owner_key)));
            for (params) |param| {
                if (param == owner) return param;
            }
            current = owner;
            steps += 1;
            if (builtin.mode == .Debug and steps > 1024) {
                std.debug.panic(
                    "RcInsertPass invariant violated: return-join root walk did not converge for local {d}",
                    .{@intFromEnum(local)},
                );
            }
        }

        return null;
    }

    fn markFreshReturnJoinParamsInStmt(self: *RcInsertPass, stmt_id: CFStmtId) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try self.markFreshReturnJoinParamsInStmt(assign.next),
            .assign_ref => |assign| try self.markFreshReturnJoinParamsInStmt(assign.next),
            .assign_literal => |assign| try self.markFreshReturnJoinParamsInStmt(assign.next),
            .assign_call => |assign| try self.markFreshReturnJoinParamsInStmt(assign.next),
            .assign_low_level => |assign| try self.markFreshReturnJoinParamsInStmt(assign.next),
            .assign_list => |assign| try self.markFreshReturnJoinParamsInStmt(assign.next),
            .assign_struct => |assign| try self.markFreshReturnJoinParamsInStmt(assign.next),
            .assign_tag => |assign| try self.markFreshReturnJoinParamsInStmt(assign.next),
            .debug => |debug_stmt| try self.markFreshReturnJoinParamsInStmt(debug_stmt.next),
            .expect => |expect_stmt| try self.markFreshReturnJoinParamsInStmt(expect_stmt.next),
            .incref => |inc| try self.markFreshReturnJoinParamsInStmt(inc.next),
            .decref => |dec| try self.markFreshReturnJoinParamsInStmt(dec.next),
            .free => |free_stmt| try self.markFreshReturnJoinParamsInStmt(free_stmt.next),
            .switch_stmt => |switch_stmt| {
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.markFreshReturnJoinParamsInStmt(branch.body);
                }
                try self.markFreshReturnJoinParamsInStmt(switch_stmt.default_branch);
            },
            .borrow_scope => |scope| {
                try self.markFreshReturnJoinParamsInStmt(scope.body);
                try self.markFreshReturnJoinParamsInStmt(scope.remainder);
            },
            .join => |join| {
                try self.markFreshReturnJoinParamsInJoinBody(self.store.getLocalSpan(join.params), join.body);
                try self.markFreshReturnJoinParamsInStmt(join.body);
                try self.markFreshReturnJoinParamsInStmt(join.remainder);
            },
            .jump, .ret, .runtime_error, .scope_exit, .crash => {},
        }
    }

    fn markFreshReturnJoinParamsInJoinBody(self: *RcInsertPass, params: []const LocalId, stmt_id: CFStmtId) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try self.markFreshReturnJoinParamsInJoinBody(params, assign.next),
            .assign_ref => |assign| try self.markFreshReturnJoinParamsInJoinBody(params, assign.next),
            .assign_literal => |assign| try self.markFreshReturnJoinParamsInJoinBody(params, assign.next),
            .assign_call => |assign| try self.markFreshReturnJoinParamsInJoinBody(params, assign.next),
            .assign_low_level => |assign| try self.markFreshReturnJoinParamsInJoinBody(params, assign.next),
            .assign_list => |assign| try self.markFreshReturnJoinParamsInJoinBody(params, assign.next),
            .assign_struct => |assign| try self.markFreshReturnJoinParamsInJoinBody(params, assign.next),
            .assign_tag => |assign| try self.markFreshReturnJoinParamsInJoinBody(params, assign.next),
            .debug => |debug_stmt| try self.markFreshReturnJoinParamsInJoinBody(params, debug_stmt.next),
            .expect => |expect_stmt| try self.markFreshReturnJoinParamsInJoinBody(params, expect_stmt.next),
            .incref => |inc| try self.markFreshReturnJoinParamsInJoinBody(params, inc.next),
            .decref => |dec| try self.markFreshReturnJoinParamsInJoinBody(params, dec.next),
            .free => |free_stmt| try self.markFreshReturnJoinParamsInJoinBody(params, free_stmt.next),
            .switch_stmt => |switch_stmt| {
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.markFreshReturnJoinParamsInJoinBody(params, branch.body);
                }
                try self.markFreshReturnJoinParamsInJoinBody(params, switch_stmt.default_branch);
            },
            .borrow_scope => |scope| {
                try self.markFreshReturnJoinParamsInJoinBody(params, scope.body);
                try self.markFreshReturnJoinParamsInJoinBody(params, scope.remainder);
            },
            .join => |join| {
                try self.markFreshReturnJoinParamsInJoinBody(params, join.body);
                try self.markFreshReturnJoinParamsInJoinBody(params, join.remainder);
            },
            .ret => |ret_stmt| {
                if (!self.layoutNeedsRc(self.localLayout(ret_stmt.value))) return;
                if (self.returnedJoinParam(params, ret_stmt.value)) |param| {
                    try self.promoted_fresh_join_params.put(localKey(param), {});
                }
            },
            .jump, .runtime_error, .scope_exit, .crash => {},
        }
    }

    fn normalizeFreshProcReturnsInStmt(self: *RcInsertPass, stmt_id: CFStmtId) Allocator.Error!CFStmtId {
        const stmt = self.store.getCFStmt(stmt_id);
        return switch (stmt) {
            .assign_symbol => |assign| try self.store.addCFStmt(.{ .assign_symbol = .{
                .target = assign.target,
                .symbol = assign.symbol,
                .next = try self.normalizeFreshProcReturnsInStmt(assign.next),
            } }),
            .assign_ref => |assign| try self.store.addCFStmt(.{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .next = try self.normalizeFreshProcReturnsInStmt(assign.next),
            } }),
            .assign_literal => |assign| try self.store.addCFStmt(.{ .assign_literal = .{
                .target = assign.target,
                .result = assign.result,
                .value = assign.value,
                .next = try self.normalizeFreshProcReturnsInStmt(assign.next),
            } }),
            .assign_call => |assign| try self.store.addCFStmt(.{ .assign_call = .{
                .target = assign.target,
                .result = assign.result,
                .proc = assign.proc,
                .args = assign.args,
                .next = try self.normalizeFreshProcReturnsInStmt(assign.next),
            } }),
            .assign_low_level => |assign| try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .args = assign.args,
                .next = try self.normalizeFreshProcReturnsInStmt(assign.next),
            } }),
            .assign_list => |assign| try self.store.addCFStmt(.{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .elems = assign.elems,
                .next = try self.normalizeFreshProcReturnsInStmt(assign.next),
            } }),
            .assign_struct => |assign| try self.store.addCFStmt(.{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .fields = assign.fields,
                .next = try self.normalizeFreshProcReturnsInStmt(assign.next),
            } }),
            .assign_tag => |assign| try self.store.addCFStmt(.{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .discriminant = assign.discriminant,
                .args = assign.args,
                .next = try self.normalizeFreshProcReturnsInStmt(assign.next),
            } }),
            .debug => |debug_stmt| try self.store.addCFStmt(.{ .debug = .{
                .message = debug_stmt.message,
                .next = try self.normalizeFreshProcReturnsInStmt(debug_stmt.next),
            } }),
            .expect => |expect_stmt| try self.store.addCFStmt(.{ .expect = .{
                .condition = expect_stmt.condition,
                .next = try self.normalizeFreshProcReturnsInStmt(expect_stmt.next),
            } }),
            .switch_stmt => |switch_stmt| blk: {
                var rewritten_branches: std.ArrayListUnmanaged(LIR.CFSwitchBranch) = .empty;
                defer rewritten_branches.deinit(self.allocator);

                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try rewritten_branches.append(self.allocator, .{
                        .value = branch.value,
                        .body = try self.normalizeFreshProcReturnsInStmt(branch.body),
                    });
                }

                break :blk try self.store.addCFStmt(.{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = try self.store.addCFSwitchBranches(rewritten_branches.items),
                    .default_branch = try self.normalizeFreshProcReturnsInStmt(switch_stmt.default_branch),
                } });
            },
            .borrow_scope => |scope| try self.store.addCFStmt(.{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.normalizeFreshProcReturnsInStmt(scope.body),
                .remainder = try self.normalizeFreshProcReturnsInStmt(scope.remainder),
            } }),
            .join => |join| blk: {
                const join_key = @intFromEnum(join.id);
                const gop = try self.active_join_params.getOrPut(join_key);
                if (builtin.mode == .Debug and gop.found_existing) {
                    std.debug.panic(
                        "RcInsertPass invariant violated: normalizeFreshProcReturns encountered duplicate active join {d}",
                        .{join_key},
                    );
                }
                gop.value_ptr.* = join.params;
                defer _ = self.active_join_params.remove(join_key);

                break :blk try self.store.addCFStmt(.{ .join = .{
                    .id = join.id,
                    .params = join.params,
                    .body = try self.normalizeFreshProcReturnsInStmt(join.body),
                    .remainder = try self.normalizeFreshProcReturnsInStmt(join.remainder),
                } });
            },
            .ret => |ret_stmt| blk: {
                if (!self.layoutNeedsRc(self.localLayout(ret_stmt.value))) break :blk stmt_id;
                if (self.localCarriesOwnedValue(ret_stmt.value)) break :blk stmt_id;

                const ret_id = try self.store.addCFStmt(.{ .ret = .{ .value = ret_stmt.value } });
                break :blk try self.store.addCFStmt(.{ .incref = .{
                    .value = ret_stmt.value,
                    .count = 1,
                    .next = ret_id,
                } });
            },
            .jump => |jump| blk: {
                const params_span = self.active_join_params.get(@intFromEnum(jump.target)) orelse break :blk stmt_id;
                const args = self.store.getLocalSpan(jump.args);
                const params = self.store.getLocalSpan(params_span);
                if (builtin.mode == .Debug and args.len != params.len) {
                    std.debug.panic(
                        "RcInsertPass invariant violated: normalizeFreshProcReturns saw jump {d} with {d} args, expected {d}",
                        .{ @intFromEnum(jump.target), args.len, params.len },
                    );
                }

                var promoted_args: std.ArrayListUnmanaged(LocalId) = .empty;
                defer promoted_args.deinit(self.allocator);

                for (args, params) |arg, param| {
                    if (!self.promoted_fresh_join_params.contains(localKey(param))) continue;
                    if (!self.layoutNeedsRc(self.localLayout(arg))) continue;
                    if (self.localCarriesOwnedValue(arg)) continue;
                    try promoted_args.append(self.allocator, arg);
                }

                if (promoted_args.items.len == 0) break :blk stmt_id;

                const jump_id = try self.store.addCFStmt(.{ .jump = jump });
                break :blk try self.prependRetainedBorrowArgIncrefs(promoted_args.items, jump_id);
            },
            .runtime_error, .scope_exit, .incref, .decref, .free, .crash => stmt_id,
        };
    }

    fn normalizeBorrowedProcReturns(self: *RcInsertPass) Allocator.Error!void {
        const proc_count = self.store.getProcSpecs().len;
        if (proc_count == 0) return;

        const borrowed_return_procs = try self.allocator.alloc(bool, proc_count);
        defer self.allocator.free(borrowed_return_procs);

        var any_borrowed_returns = false;
        for (self.store.getProcSpecs(), 0..) |proc, i| {
            borrowed_return_procs[i] = proc.result_contract == .borrow_of_param;
            any_borrowed_returns = any_borrowed_returns or borrowed_return_procs[i];
        }
        if (!any_borrowed_returns) return;

        for (0..proc_count) |proc_index| {
            const proc_id: LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
            const proc_ptr = self.store.getProcSpecPtr(proc_id);
            proc_ptr.body = try self.normalizeBorrowedProcReturnsInStmt(
                proc_ptr.body,
                borrowed_return_procs,
                borrowed_return_procs[proc_index],
            );
            if (borrowed_return_procs[proc_index]) {
                proc_ptr.result_contract = .fresh;
            }
        }
    }

    fn normalizeBorrowedProcReturnsInStmt(
        self: *RcInsertPass,
        stmt_id: CFStmtId,
        borrowed_return_procs: []const bool,
        current_proc_promotes_borrowed_return: bool,
    ) Allocator.Error!CFStmtId {
        const stmt = self.store.getCFStmt(stmt_id);
        return switch (stmt) {
            .assign_symbol => |assign| try self.store.addCFStmt(.{ .assign_symbol = .{
                .target = assign.target,
                .symbol = assign.symbol,
                .next = try self.normalizeBorrowedProcReturnsInStmt(
                    assign.next,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .assign_ref => |assign| try self.store.addCFStmt(.{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .next = try self.normalizeBorrowedProcReturnsInStmt(
                    assign.next,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .assign_literal => |assign| try self.store.addCFStmt(.{ .assign_literal = .{
                .target = assign.target,
                .result = assign.result,
                .value = assign.value,
                .next = try self.normalizeBorrowedProcReturnsInStmt(
                    assign.next,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .assign_call => |assign| blk: {
                const proc_index = @intFromEnum(assign.proc);
                const normalized_result = if (proc_index < borrowed_return_procs.len and borrowed_return_procs[proc_index])
                    LIR.ResultSemantics.fresh
                else
                    assign.result;
                break :blk try self.store.addCFStmt(.{ .assign_call = .{
                    .target = assign.target,
                    .result = normalized_result,
                    .proc = assign.proc,
                    .args = assign.args,
                    .next = try self.normalizeBorrowedProcReturnsInStmt(
                        assign.next,
                        borrowed_return_procs,
                        current_proc_promotes_borrowed_return,
                    ),
                } });
            },
            .assign_low_level => |assign| try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .args = assign.args,
                .next = try self.normalizeBorrowedProcReturnsInStmt(
                    assign.next,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .assign_list => |assign| try self.store.addCFStmt(.{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .elems = assign.elems,
                .next = try self.normalizeBorrowedProcReturnsInStmt(
                    assign.next,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .assign_struct => |assign| try self.store.addCFStmt(.{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .fields = assign.fields,
                .next = try self.normalizeBorrowedProcReturnsInStmt(
                    assign.next,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .assign_tag => |assign| try self.store.addCFStmt(.{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .discriminant = assign.discriminant,
                .args = assign.args,
                .next = try self.normalizeBorrowedProcReturnsInStmt(
                    assign.next,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .debug => |debug_stmt| try self.store.addCFStmt(.{ .debug = .{
                .message = debug_stmt.message,
                .next = try self.normalizeBorrowedProcReturnsInStmt(
                    debug_stmt.next,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .expect => |expect_stmt| try self.store.addCFStmt(.{ .expect = .{
                .condition = expect_stmt.condition,
                .next = try self.normalizeBorrowedProcReturnsInStmt(
                    expect_stmt.next,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .incref => |inc| try self.store.addCFStmt(.{ .incref = .{
                .value = inc.value,
                .count = inc.count,
                .next = try self.normalizeBorrowedProcReturnsInStmt(
                    inc.next,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .decref => |dec| try self.store.addCFStmt(.{ .decref = .{
                .value = dec.value,
                .next = try self.normalizeBorrowedProcReturnsInStmt(
                    dec.next,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .free => |free_stmt| try self.store.addCFStmt(.{ .free = .{
                .value = free_stmt.value,
                .next = try self.normalizeBorrowedProcReturnsInStmt(
                    free_stmt.next,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .switch_stmt => |switch_stmt| blk: {
                var rewritten_branches: std.ArrayListUnmanaged(LIR.CFSwitchBranch) = .empty;
                defer rewritten_branches.deinit(self.allocator);
                const original_branches = try self.cloneSwitchBranches(switch_stmt.branches);
                defer self.allocator.free(original_branches);

                for (original_branches) |branch| {
                    try rewritten_branches.append(self.allocator, .{
                        .value = branch.value,
                        .body = try self.normalizeBorrowedProcReturnsInStmt(
                            branch.body,
                            borrowed_return_procs,
                            current_proc_promotes_borrowed_return,
                        ),
                    });
                }

                break :blk try self.store.addCFStmt(.{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = try self.store.addCFSwitchBranches(rewritten_branches.items),
                    .default_branch = try self.normalizeBorrowedProcReturnsInStmt(
                        switch_stmt.default_branch,
                        borrowed_return_procs,
                        current_proc_promotes_borrowed_return,
                    ),
                } });
            },
            .borrow_scope => |scope| try self.store.addCFStmt(.{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.normalizeBorrowedProcReturnsInStmt(
                    scope.body,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
                .remainder = try self.normalizeBorrowedProcReturnsInStmt(
                    scope.remainder,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .join => |join| try self.store.addCFStmt(.{ .join = .{
                .id = join.id,
                .params = join.params,
                .body = try self.normalizeBorrowedProcReturnsInStmt(
                    join.body,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
                .remainder = try self.normalizeBorrowedProcReturnsInStmt(
                    join.remainder,
                    borrowed_return_procs,
                    current_proc_promotes_borrowed_return,
                ),
            } }),
            .jump, .runtime_error, .scope_exit, .crash => stmt_id,
            .ret => |ret_stmt| blk: {
                if (!current_proc_promotes_borrowed_return) break :blk stmt_id;
                if (!self.layoutNeedsRc(self.localLayout(ret_stmt.value))) break :blk stmt_id;

                const ret_id = try self.store.addCFStmt(.{ .ret = .{ .value = ret_stmt.value } });
                break :blk try self.store.addCFStmt(.{ .incref = .{
                    .value = ret_stmt.value,
                    .count = 1,
                    .next = ret_id,
                } });
            },
        };
    }

    fn analyzeProcParamUseKinds(self: *RcInsertPass, proc_index: usize, proc: LIR.LirProcSpec) Allocator.Error![]UseKind {
        self.clearAnalysisState();
        const params = self.store.getLocalSpan(proc.args);
        const seed_use_kinds = if (proc_index < self.proc_param_use_kinds.items.len and self.proc_param_use_kinds.items[proc_index].len == params.len)
            self.proc_param_use_kinds.items[proc_index]
        else
            &.{};
        const assumed_use_kinds = try self.allocator.alloc(UseKind, params.len);
        defer self.allocator.free(assumed_use_kinds);
        for (assumed_use_kinds, 0..) |*use_kind, i| {
            use_kind.* = if (seed_use_kinds.len == params.len) seed_use_kinds[i] else .borrow;
        }
        try self.recordProcParamDefinitions(proc.args, assumed_use_kinds);
        try self.collectAliasSemanticsInStmt(proc.body);
        try self.inferJoinParamAliasSources(proc.body);
        try self.inferOwningJoinParams(proc.body);
        try self.countUsesInStmt(proc.body);

        const use_kinds = try self.allocator.alloc(UseKind, params.len);
        for (params, 0..) |param, i| {
            const returned_aliases_param = switch (proc.result_contract) {
                .alias_of_param => |contract| contract.param_index == i,
                else => false,
            };
            const ownership_consumed = self.paramOwnershipIsConsumed(param);
            use_kinds[i] = if (ownership_consumed or returned_aliases_param) .consume else .borrow;
        }
        return use_kinds;
    }

    fn localKey(local: LocalId) u64 {
        return @intFromEnum(local);
    }

    fn localLayout(self: *const RcInsertPass, local: LocalId) LayoutIdx {
        return self.store.getLocal(local).layout_idx;
    }

    fn layoutNeedsRc(self: *const RcInsertPass, layout_idx: LayoutIdx) bool {
        return self.layout_store.layoutContainsRefcounted(self.layout_store.getLayout(layout_idx));
    }

    fn bumpUseCount(self: *RcInsertPass, local: LocalId, count: u32) Allocator.Error!void {
        if (!self.layoutNeedsRc(self.localLayout(local))) return;
        const gop = try self.symbol_use_counts.getOrPut(localKey(local));
        if (!gop.found_existing) gop.value_ptr.* = 0;
        gop.value_ptr.* += count;
    }

    fn bumpUse(self: *RcInsertPass, local: LocalId) Allocator.Error!void {
        try self.bumpUseCount(local, 1);
    }

    fn mergeUseSummary(self: *RcInsertPass, local: LocalId, summary: UseSummary) Allocator.Error!void {
        if (!self.layoutNeedsRc(self.localLayout(local))) return;
        const gop = try self.symbol_use_summaries.getOrPut(localKey(local));
        if (!gop.found_existing) gop.value_ptr.* = .{};
        gop.value_ptr.borrow_count += summary.borrow_count;
        gop.value_ptr.consume_count += summary.consume_count;
    }

    fn mergeBorrowOnlySummary(self: *RcInsertPass, local: LocalId, count: u32) Allocator.Error!void {
        if (count == 0) return;
        if (!self.layoutNeedsRc(self.localLayout(local))) return;
        const gop = try self.symbol_use_summaries.getOrPut(localKey(local));
        if (!gop.found_existing) gop.value_ptr.* = .{};
        gop.value_ptr.borrow_count += count;
    }

    fn markUse(self: *RcInsertPass, local: LocalId, kind: UseKind) Allocator.Error!void {
        if (!self.layoutNeedsRc(self.localLayout(local))) return;
        try self.bumpUse(local);
        const gop = try self.symbol_use_summaries.getOrPut(localKey(local));
        if (!gop.found_existing) gop.value_ptr.* = .{};
        const direct_gop = try self.direct_use_summaries.getOrPut(localKey(local));
        if (!direct_gop.found_existing) direct_gop.value_ptr.* = .{};
        switch (kind) {
            .borrow => {
                gop.value_ptr.borrow_count += 1;
                direct_gop.value_ptr.borrow_count += 1;
            },
            .consume => {
                gop.value_ptr.consume_count += 1;
                direct_gop.value_ptr.consume_count += 1;
            },
        }
    }

    fn useSummary(self: *const RcInsertPass, local: LocalId) UseSummary {
        return self.symbol_use_summaries.get(localKey(local)) orelse .{};
    }

    fn directUseSummary(self: *const RcInsertPass, local: LocalId) UseSummary {
        return self.direct_use_summaries.get(localKey(local)) orelse .{};
    }

    fn paramOwnershipIsConsumed(self: *const RcInsertPass, param: LocalId) bool {
        if (self.directUseSummary(param).consume_count != 0) return true;

        var it = self.direct_use_summaries.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.consume_count == 0) continue;
            const local: LocalId = @enumFromInt(@as(u32, @intCast(entry.key_ptr.*)));
            if (local == param) return true;
            if (self.localForwardsOwnership(local, param)) return true;
        }

        return false;
    }

    fn recordForwardingAliasBorrow(self: *RcInsertPass, local: LocalId) Allocator.Error!void {
        if (!self.layoutNeedsRc(self.localLayout(local))) return;
        const gop = try self.direct_forwarding_alias_borrows.getOrPut(localKey(local));
        if (!gop.found_existing) gop.value_ptr.* = 0;
        gop.value_ptr.* += 1;
    }

    fn directNonForwardingBorrowCount(self: *const RcInsertPass, local: LocalId) u32 {
        const direct = self.directUseSummary(local).borrow_count;
        const forwarding = self.direct_forwarding_alias_borrows.get(localKey(local)) orelse 0;
        if (builtin.mode == .Debug and forwarding > direct) {
            std.debug.panic(
                "RcInsertPass invariant violated: local {d} recorded {d} forwarding alias borrows but only {d} direct borrows",
                .{ @intFromEnum(local), forwarding, direct },
            );
        }
        return direct - forwarding;
    }

    fn recordLocalDefinition(self: *RcInsertPass, local: LocalId, kind: LocalDefinitionKind) Allocator.Error!void {
        const key = localKey(local);
        const gop = try self.local_definition_kinds.getOrPut(key);
        if (builtin.mode == .Debug and gop.found_existing) {
            std.debug.panic(
                "RcInsertPass invariant violated: local {d} recorded multiple definition kinds ({s} then {s})",
                .{ @intFromEnum(local), @tagName(gop.value_ptr.*), @tagName(kind) },
            );
        }
        gop.value_ptr.* = kind;
    }

    fn recordProcParamDefinitions(
        self: *RcInsertPass,
        params_span: LIR.LocalSpan,
        param_use_kinds: []const UseKind,
    ) Allocator.Error!void {
        const params = self.store.getLocalSpan(params_span);
        if (builtin.mode == .Debug and param_use_kinds.len != 0 and params.len != param_use_kinds.len) {
            std.debug.panic(
                "RcInsertPass invariant violated: proc params span had {d} params but ownership table provided {d} kinds",
                .{ params.len, param_use_kinds.len },
            );
        }

        for (params, 0..) |param, i| {
            try self.recordLocalDefinition(param, .proc_param);
            if (param_use_kinds.len != 0 and param_use_kinds[i] == .consume) {
                try self.owned_proc_params.put(localKey(param), {});
            }
        }
    }

    fn recordJoinParamDefinitions(self: *RcInsertPass, params_span: LIR.LocalSpan) Allocator.Error!void {
        for (self.store.getLocalSpan(params_span)) |param| {
            try self.recordLocalDefinition(param, .join_param);
        }
    }

    fn recordStmtDefinition(self: *RcInsertPass, local: LocalId, semantics: LIR.ResultSemantics) Allocator.Error!void {
        try self.recordLocalDefinition(local, .{ .stmt_result = semantics });
    }

    fn refOpSource(op: LIR.RefOp) LocalId {
        return switch (op) {
            .local => |local| local,
            .discriminant => |disc| disc.source,
            .field => |field| field.source,
            .tag_payload => |payload| payload.source,
            .nominal => |nominal| nominal.backing_ref,
        };
    }

    fn propagateResultUses(self: *RcInsertPass, target: LocalId, semantics: LIR.ResultSemantics) Allocator.Error!void {
        const target_use_count = self.symbol_use_counts.get(localKey(target)) orelse 0;
        if (target_use_count == 0) return;
        const target_summary = self.useSummary(target);

        switch (semantics) {
            .fresh => {},
            .alias_of => |aliased| {
                try self.bumpUseCount(aliased.owner, target_summary.total());
                try self.mergeUseSummary(aliased.owner, target_summary);
            },
            .borrow_of => |borrowed| {
                try self.bumpUseCount(borrowed.owner, target_summary.total());
                try self.mergeBorrowOnlySummary(borrowed.owner, target_summary.total());
            },
        }
    }

    fn countUsesInStmt(self: *RcInsertPass, stmt_id: CFStmtId) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try self.countUsesInStmt(assign.next),
            .assign_ref => |assign| {
                try self.markUse(refOpSource(assign.op), .borrow);
                if (assign.result == .alias_of) {
                    try self.recordForwardingAliasBorrow(refOpSource(assign.op));
                }
                try self.countUsesInStmt(assign.next);
                try self.propagateResultUses(assign.target, assign.result);
            },
            .assign_literal => |assign| try self.countUsesInStmt(assign.next),
            .assign_call => |assign| {
                const args = self.store.getLocalSpan(assign.args);
                const proc_index = @intFromEnum(assign.proc);
                const param_use_kinds = if (proc_index < self.proc_param_use_kinds.items.len)
                    self.proc_param_use_kinds.items[proc_index]
                else
                    &.{};
                if (builtin.mode == .Debug and param_use_kinds.len != 0 and args.len != param_use_kinds.len) {
                    std.debug.panic(
                        "RcInsertPass invariant violated: proc {d} called with {d} args but ownership summary expects {d}",
                        .{ proc_index, args.len, param_use_kinds.len },
                    );
                }
                if (param_use_kinds.len == 0) {
                    for (args) |arg| try self.markUse(arg, .borrow);
                } else {
                    for (args, param_use_kinds) |arg, use_kind| try self.markUse(arg, use_kind);
                }
                try self.countUsesInStmt(assign.next);
                try self.propagateResultUses(assign.target, assign.result);
            },
            .assign_low_level => |assign| {
                const args = self.store.getLocalSpan(assign.args);
                const arg_ownership = assign.op.getArgOwnership();
                if (builtin.mode == .Debug and args.len != arg_ownership.len) {
                    std.debug.panic(
                        "RcInsertPass invariant violated: low-level {s} passed {d} args but ownership table expects {d}",
                        .{ @tagName(assign.op), args.len, arg_ownership.len },
                    );
                }
                for (args, arg_ownership) |arg, ownership| {
                    try self.markUse(arg, switch (ownership) {
                        .borrow => .borrow,
                        .consume => .consume,
                    });
                }
                try self.countUsesInStmt(assign.next);
                try self.propagateResultUses(assign.target, assign.result);
            },
            .assign_list => |assign| {
                for (self.store.getLocalSpan(assign.elems)) |elem| try self.markUse(elem, .consume);
                try self.countUsesInStmt(assign.next);
            },
            .assign_struct => |assign| {
                for (self.store.getLocalSpan(assign.fields)) |field| try self.markUse(field, .consume);
                try self.countUsesInStmt(assign.next);
            },
            .assign_tag => |assign| {
                for (self.store.getLocalSpan(assign.args)) |arg| try self.markUse(arg, .consume);
                try self.countUsesInStmt(assign.next);
            },
            .debug => |stmt| {
                try self.markUse(stmt.message, .borrow);
                try self.countUsesInStmt(stmt.next);
            },
            .expect => |stmt| {
                try self.markUse(stmt.condition, .borrow);
                try self.countUsesInStmt(stmt.next);
            },
            .runtime_error => {},
            .incref => |inc| {
                try self.markUse(inc.value, .borrow);
                try self.countUsesInStmt(inc.next);
            },
            .decref => |dec| {
                try self.markUse(dec.value, .borrow);
                try self.countUsesInStmt(dec.next);
            },
            .free => |free_stmt| {
                try self.markUse(free_stmt.value, .borrow);
                try self.countUsesInStmt(free_stmt.next);
            },
            .switch_stmt => |switch_stmt| {
                try self.markUse(switch_stmt.cond, .borrow);
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.countUsesInStmt(branch.body);
                }
                try self.countUsesInStmt(switch_stmt.default_branch);
            },
            .join => |join| {
                const join_key = @intFromEnum(join.id);
                const gop = try self.active_join_params.getOrPut(join_key);
                if (builtin.mode == .Debug and gop.found_existing) {
                    std.debug.panic(
                        "RcInsertPass invariant violated: nested/duplicate active join {d}",
                        .{join_key},
                    );
                }
                gop.value_ptr.* = join.params;
                defer _ = self.active_join_params.remove(join_key);

                try self.countUsesInStmt(join.body);
                try self.countUsesInStmt(join.remainder);
            },
            .borrow_scope => |scope| {
                try self.countUsesInStmt(scope.body);
                try self.countUsesInStmt(scope.remainder);
            },
            .scope_exit => {},
            .jump => |jump| {
                const params = self.active_join_params.get(@intFromEnum(jump.target)) orelse std.debug.panic(
                    "RcInsertPass invariant violated: jump to unknown active join {d}",
                    .{@intFromEnum(jump.target)},
                );
                const args = self.store.getLocalSpan(jump.args);
                const param_locals = self.store.getLocalSpan(params);
                if (builtin.mode == .Debug and args.len != param_locals.len) {
                    std.debug.panic(
                        "RcInsertPass invariant violated: jump to join {d} passed {d} args, expected {d}",
                        .{ @intFromEnum(jump.target), args.len, param_locals.len },
                    );
                }
                for (args, param_locals) |arg, param| {
                    const use_kind: UseKind = if (self.localCarriesOwnedValue(param)) .consume else .borrow;
                    try self.markUse(arg, use_kind);
                }
            },
            .ret => |ret| try self.markUse(ret.value, .consume),
            .crash => {},
        }
    }

    fn resultSemantics(stmt: CFStmt) LIR.ResultSemantics {
        return switch (stmt) {
            .assign_symbol => .fresh,
            .assign_ref => |assign| assign.result,
            .assign_literal => |assign| assign.result,
            .assign_call => |assign| assign.result,
            .assign_low_level => |assign| assign.result,
            .assign_list => |assign| assign.result,
            .assign_struct => |assign| assign.result,
            .assign_tag => |assign| assign.result,
            else => unreachable,
        };
    }

    fn recordAliasSemantics(self: *RcInsertPass, target: LocalId, semantics: LIR.ResultSemantics) Allocator.Error!void {
        const target_key = localKey(target);
        switch (semantics) {
            .fresh => {},
            .alias_of => |source| {
                try self.local_alias_sources.put(target_key, .{
                    .owner_key = localKey(source.owner),
                    .forwards_ownership = true,
                });
            },
            .borrow_of => |borrowed| {
                try self.local_alias_sources.put(target_key, .{
                    .owner_key = localKey(borrowed.owner),
                    .forwards_ownership = false,
                });
            },
        }
    }

    fn collectAliasSemanticsInStmt(self: *RcInsertPass, stmt_id: CFStmtId) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| {
                try self.recordStmtDefinition(assign.target, .fresh);
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .assign_ref => |assign| {
                try self.recordStmtDefinition(assign.target, assign.result);
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
                }
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .assign_literal => |assign| {
                try self.recordStmtDefinition(assign.target, assign.result);
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
                }
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .assign_call => |assign| {
                try self.recordStmtDefinition(assign.target, assign.result);
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
                }
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .assign_low_level => |assign| {
                try self.recordStmtDefinition(assign.target, assign.result);
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
                }
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .assign_list => |assign| {
                try self.recordStmtDefinition(assign.target, assign.result);
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
                    try self.recordOwnedInputRoots(assign.target, self.store.getLocalSpan(assign.elems));
                }
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .assign_struct => |assign| {
                try self.recordStmtDefinition(assign.target, assign.result);
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
                    try self.recordOwnedInputRoots(assign.target, self.store.getLocalSpan(assign.fields));
                }
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .assign_tag => |assign| {
                try self.recordStmtDefinition(assign.target, assign.result);
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
                    try self.recordOwnedInputRoots(assign.target, self.store.getLocalSpan(assign.args));
                }
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .debug => |stmt_debug| try self.collectAliasSemanticsInStmt(stmt_debug.next),
            .expect => |stmt_expect| try self.collectAliasSemanticsInStmt(stmt_expect.next),
            .runtime_error, .incref, .decref, .free, .scope_exit, .jump, .ret, .crash => {},
            .switch_stmt => |switch_stmt| {
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.collectAliasSemanticsInStmt(branch.body);
                }
                try self.collectAliasSemanticsInStmt(switch_stmt.default_branch);
            },
            .borrow_scope => |scope| {
                try self.collectAliasSemanticsInStmt(scope.body);
                try self.collectAliasSemanticsInStmt(scope.remainder);
            },
            .join => |join| {
                try self.joins_by_id.put(@intFromEnum(join.id), .{
                    .params = join.params,
                    .body = join.body,
                    .remainder = join.remainder,
                });
                try self.recordJoinParamDefinitions(join.params);
                try self.collectAliasSemanticsInStmt(join.body);
                try self.collectAliasSemanticsInStmt(join.remainder);
            },
        }
    }

    fn resultIsFresh(stmt: CFStmt) bool {
        return switch (resultSemantics(stmt)) {
            .fresh => true,
            else => false,
        };
    }

    fn closesOwningAliasRepresentative(self: *const RcInsertPass, stmt: CFStmt, _: LocalId) bool {
        return switch (stmt) {
            .assign_ref => false,
            .assign_call => |assign| switch (assign.result) {
                .alias_of => |aliased| blk: {
                    const args = self.store.getLocalSpan(assign.args);
                    const proc_index = @intFromEnum(assign.proc);
                    const param_use_kinds = if (proc_index < self.proc_param_use_kinds.items.len)
                        self.proc_param_use_kinds.items[proc_index]
                    else
                        &.{};
                    if (param_use_kinds.len == 0) break :blk false;
                    for (args, param_use_kinds) |arg, use_kind| {
                        if (use_kind != .consume) continue;
                        if (self.localForwardsOwnership(arg, aliased.owner)) break :blk true;
                    }
                    break :blk false;
                },
                else => false,
            },
            .assign_low_level => |assign| switch (assign.result) {
                .alias_of => |aliased| blk: {
                    const args = self.store.getLocalSpan(assign.args);
                    const arg_ownership = assign.op.getArgOwnership();
                    for (args, arg_ownership) |arg, ownership| {
                        if (ownership != .consume) continue;
                        if (self.localForwardsOwnership(arg, aliased.owner)) break :blk true;
                    }
                    break :blk false;
                },
                else => false,
            },
            else => false,
        };
    }

    fn prependLocalDecrefIfNotForwarded(
        self: *RcInsertPass,
        value: LocalId,
        forwarded_values: []const LocalId,
        next: CFStmtId,
    ) Allocator.Error!CFStmtId {
        const value_root = self.ownershipRootForConsumedValue(value);
        for (forwarded_values) |forwarded_value| {
            if (self.localForwardsOwnership(forwarded_value, value)) {
                return next;
            }
            if (value_root) |root| {
                if (self.ownershipRootForConsumedValue(forwarded_value)) |forwarded_root| {
                    if (forwarded_root == root) {
                        return next;
                    }
                }
            }
        }

        return try self.store.addCFStmt(.{ .decref = .{
            .value = value,
            .next = next,
        } });
    }

    fn stmtConsumesForwardedOwnership(self: *const RcInsertPass, stmt: CFStmt, owner: LocalId) bool {
        switch (stmt) {
            .assign_call => |assign| {
                const args = self.store.getLocalSpan(assign.args);
                const proc_index = @intFromEnum(assign.proc);
                const param_use_kinds = if (proc_index < self.proc_param_use_kinds.items.len)
                    self.proc_param_use_kinds.items[proc_index]
                else
                    &.{};
                if (param_use_kinds.len == 0) return false;
                for (args, param_use_kinds) |arg, use_kind| {
                    if (use_kind != .consume) continue;
                    if (self.localTransfersOwnership(arg, owner)) return true;
                }
                return false;
            },
            .assign_low_level => |assign| {
                const args = self.store.getLocalSpan(assign.args);
                const arg_ownership = assign.op.getArgOwnership();
                for (args, arg_ownership) |arg, ownership| {
                    if (ownership != .consume) continue;
                    if (self.localTransfersOwnership(arg, owner)) return true;
                }
                return false;
            },
            .assign_list => |assign| {
                for (self.store.getLocalSpan(assign.elems)) |arg| {
                    if (self.localTransfersOwnership(arg, owner)) return true;
                }
                return false;
            },
            .assign_struct => |assign| {
                for (self.store.getLocalSpan(assign.fields)) |arg| {
                    if (self.localTransfersOwnership(arg, owner)) return true;
                }
                return false;
            },
            .assign_tag => |assign| {
                for (self.store.getLocalSpan(assign.args)) |arg| {
                    if (self.localTransfersOwnership(arg, owner)) return true;
                }
                return false;
            },
            else => return false,
        }
    }

    fn stmtClosesForwardedOwnership(self: *const RcInsertPass, stmt: CFStmt, owner: LocalId) bool {
        return switch (stmt) {
            .decref => |dec| self.localTransfersOwnership(dec.value, owner),
            .free => |free_stmt| self.localTransfersOwnership(free_stmt.value, owner),
            else => false,
        };
    }

    fn localTransfersOwnership(self: *const RcInsertPass, value: LocalId, owner: LocalId) bool {
        if (!self.localCarriesOwnedValue(value)) return false;
        if (self.localForwardsOwnership(value, owner)) return true;

        const value_root = self.ownershipRootForConsumedValue(value) orelse return false;
        const owner_root = self.ownershipRootForConsumedValue(owner) orelse return false;
        return value_root == owner_root;
    }

    fn remainingParamsAfterStmtConsumes(
        self: *RcInsertPass,
        params: []const LocalId,
        stmt: CFStmt,
    ) Allocator.Error![]const LocalId {
        var removed_any = false;
        for (params) |param| {
            if (self.stmtConsumesForwardedOwnership(stmt, param)) {
                removed_any = true;
                break;
            }
        }
        if (!removed_any) return params;

        var remaining = std.ArrayListUnmanaged(LocalId).empty;
        for (params) |param| {
            if (self.stmtConsumesForwardedOwnership(stmt, param)) continue;
            try remaining.append(self.allocator, param);
        }
        return remaining.toOwnedSlice(self.allocator);
    }

    fn remainingParamsAfterStmtCloses(
        self: *RcInsertPass,
        params: []const LocalId,
        stmt: CFStmt,
    ) Allocator.Error![]const LocalId {
        var removed_any = false;
        for (params) |param| {
            if (self.stmtClosesForwardedOwnership(stmt, param)) {
                removed_any = true;
                break;
            }
        }
        if (!removed_any) return params;

        var remaining = std.ArrayListUnmanaged(LocalId).empty;
        for (params) |param| {
            if (self.stmtClosesForwardedOwnership(stmt, param)) continue;
            try remaining.append(self.allocator, param);
        }
        return remaining.toOwnedSlice(self.allocator);
    }

    fn closeLocalOnExit(self: *RcInsertPass, value: LocalId, stmt_id: CFStmtId) Allocator.Error!CFStmtId {
        return self.closeLocalOnExitRec(value, &.{}, stmt_id);
    }

    fn closeLocalOnExitRec(
        self: *RcInsertPass,
        value: LocalId,
        internal_join_ids: []const LIR.JoinPointId,
        stmt_id: CFStmtId,
    ) Allocator.Error!CFStmtId {
        const stmt = self.store.getCFStmt(stmt_id);
        return switch (stmt) {
            .assign_symbol => |assign| try self.store.addCFStmt(.{ .assign_symbol = .{
                .target = assign.target,
                .symbol = assign.symbol,
                .next = try self.closeLocalOnExitRec(value, internal_join_ids, assign.next),
            } }),
            .assign_ref => |assign| try self.store.addCFStmt(.{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .next = try self.closeLocalOnExitRec(value, internal_join_ids, assign.next),
            } }),
            .assign_literal => |assign| try self.store.addCFStmt(.{ .assign_literal = .{
                .target = assign.target,
                .result = assign.result,
                .value = assign.value,
                .next = try self.closeLocalOnExitRec(value, internal_join_ids, assign.next),
            } }),
            .assign_call => |assign| try self.store.addCFStmt(.{ .assign_call = .{
                .target = assign.target,
                .result = assign.result,
                .proc = assign.proc,
                .args = assign.args,
                .next = if (self.stmtConsumesForwardedOwnership(stmt, value))
                    assign.next
                else
                    try self.closeLocalOnExitRec(value, internal_join_ids, assign.next),
            } }),
            .assign_low_level => |assign| try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .args = assign.args,
                .next = if (self.stmtConsumesForwardedOwnership(stmt, value))
                    assign.next
                else
                    try self.closeLocalOnExitRec(value, internal_join_ids, assign.next),
            } }),
            .assign_list => |assign| try self.store.addCFStmt(.{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .elems = assign.elems,
                .next = if (self.stmtConsumesForwardedOwnership(stmt, value))
                    assign.next
                else
                    try self.closeLocalOnExitRec(value, internal_join_ids, assign.next),
            } }),
            .assign_struct => |assign| try self.store.addCFStmt(.{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .fields = assign.fields,
                .next = if (self.stmtConsumesForwardedOwnership(stmt, value))
                    assign.next
                else
                    try self.closeLocalOnExitRec(value, internal_join_ids, assign.next),
            } }),
            .assign_tag => |assign| try self.store.addCFStmt(.{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .discriminant = assign.discriminant,
                .args = assign.args,
                .next = if (self.stmtConsumesForwardedOwnership(stmt, value))
                    assign.next
                else
                    try self.closeLocalOnExitRec(value, internal_join_ids, assign.next),
            } }),
            .debug => |stmt_debug| try self.store.addCFStmt(.{ .debug = .{
                .message = stmt_debug.message,
                .next = try self.closeLocalOnExitRec(value, internal_join_ids, stmt_debug.next),
            } }),
            .expect => |stmt_expect| try self.store.addCFStmt(.{ .expect = .{
                .condition = stmt_expect.condition,
                .next = try self.closeLocalOnExitRec(value, internal_join_ids, stmt_expect.next),
            } }),
            .incref => |inc| try self.store.addCFStmt(.{ .incref = .{
                .value = inc.value,
                .count = inc.count,
                .next = try self.closeLocalOnExitRec(value, internal_join_ids, inc.next),
            } }),
            .decref => |dec| try self.store.addCFStmt(.{ .decref = .{
                .value = dec.value,
                .next = if (self.stmtClosesForwardedOwnership(stmt, value))
                    dec.next
                else
                    try self.closeLocalOnExitRec(value, internal_join_ids, dec.next),
            } }),
            .free => |free_stmt| try self.store.addCFStmt(.{ .free = .{
                .value = free_stmt.value,
                .next = if (self.stmtClosesForwardedOwnership(stmt, value))
                    free_stmt.next
                else
                    try self.closeLocalOnExitRec(value, internal_join_ids, free_stmt.next),
            } }),
            .switch_stmt => |switch_stmt| blk: {
                var rewritten_branches: std.ArrayListUnmanaged(LIR.CFSwitchBranch) = .empty;
                defer rewritten_branches.deinit(self.allocator);
                const original_branches = try self.cloneSwitchBranches(switch_stmt.branches);
                defer self.allocator.free(original_branches);

                for (original_branches) |branch| {
                    try rewritten_branches.append(self.allocator, .{
                        .value = branch.value,
                        .body = try self.closeLocalOnExitRec(value, internal_join_ids, branch.body),
                    });
                }

                break :blk try self.store.addCFStmt(.{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = try self.store.addCFSwitchBranches(rewritten_branches.items),
                    .default_branch = try self.closeLocalOnExitRec(value, internal_join_ids, switch_stmt.default_branch),
                } });
            },
            .borrow_scope => |scope| try self.store.addCFStmt(.{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.closeLocalOnExitRec(value, internal_join_ids, scope.body),
                .remainder = try self.closeLocalOnExitRec(value, internal_join_ids, scope.remainder),
            } }),
            .join => |join| blk: {
                const nested_join_ids = try self.allocator.alloc(LIR.JoinPointId, internal_join_ids.len + 1);
                defer self.allocator.free(nested_join_ids);
                @memcpy(nested_join_ids[0..internal_join_ids.len], internal_join_ids);
                nested_join_ids[internal_join_ids.len] = join.id;

                var body_internal_join_ids = std.ArrayListUnmanaged(LIR.JoinPointId).empty;
                defer body_internal_join_ids.deinit(self.allocator);
                try body_internal_join_ids.appendSlice(self.allocator, nested_join_ids);
                try self.appendReachableJoinIds(join.body, &body_internal_join_ids);
                try self.appendReachableJoinIds(join.remainder, &body_internal_join_ids);

                const join_params = self.store.getLocalSpan(join.params);
                var forwarded_join_params = std.ArrayListUnmanaged(LocalId).empty;
                defer forwarded_join_params.deinit(self.allocator);
                for (join_params) |join_param| {
                    if (!self.localTransfersOwnership(join_param, value)) continue;
                    try forwarded_join_params.append(self.allocator, join_param);
                }
                var rewritten_body = try self.closeLocalOnExitRec(value, nested_join_ids, join.body);
                if (forwarded_join_params.items.len > 0) {
                    rewritten_body = try self.closeJoinParamsOnExit(
                        forwarded_join_params.items,
                        body_internal_join_ids.items,
                        rewritten_body,
                    );
                }

                break :blk try self.store.addCFStmt(.{ .join = .{
                    .id = join.id,
                    .params = join.params,
                    .body = rewritten_body,
                    .remainder = try self.closeLocalOnExitRec(value, nested_join_ids, join.remainder),
                } });
            },
            .ret => |ret_stmt| blk: {
                const forwarded = [_]LocalId{ret_stmt.value};
                break :blk try self.prependLocalDecrefIfNotForwarded(value, &forwarded, stmt_id);
            },
            .runtime_error, .crash => try self.prependLocalDecrefIfNotForwarded(value, &.{}, stmt_id),
            .scope_exit => stmt_id,
            .jump => |jump| blk: {
                for (internal_join_ids) |internal_join_id| {
                    if (jump.target != internal_join_id) continue;

                    if (self.ownershipRootForConsumedValue(value)) |root| {
                        if (self.joinBodyUsesOwnershipRoot(jump.target, root)) break :blk stmt_id;
                    }

                    break :blk try self.prependLocalDecrefIfNotForwarded(
                        value,
                        self.store.getLocalSpan(jump.args),
                        stmt_id,
                    );
                }
                break :blk try self.prependLocalDecrefIfNotForwarded(
                    value,
                    self.store.getLocalSpan(jump.args),
                    stmt_id,
                );
            },
        };
    }

    fn localForwardsOwnership(self: *const RcInsertPass, value: LocalId, owner: LocalId) bool {
        if (!self.localCarriesOwnedValue(value)) return false;
        return self.localForwardsOwnershipRec(value, owner, 0);
    }

    fn localForwardsOwnershipRec(self: *const RcInsertPass, value: LocalId, owner: LocalId, depth: u32) bool {
        if (value == owner) return true;
        if (builtin.mode == .Debug and depth > 1024) {
            std.debug.panic(
                "RcInsertPass invariant violated: ownership forwarding walk did not converge while checking whether local {d} forwards ownership of {d}",
                .{ @intFromEnum(value), @intFromEnum(owner) },
            );
        }

        if (self.local_alias_sources.get(localKey(value))) |source| {
            if (!source.forwards_ownership) return false;
            const alias_owner: LocalId = @enumFromInt(@as(u32, @intCast(source.owner_key)));
            if (self.localForwardsOwnershipRec(alias_owner, owner, depth + 1)) return true;
        }

        for (self.ownedInputRoots(value)) |input_root| {
            if (self.localForwardsOwnershipRec(input_root, owner, depth + 1)) return true;
        }

        return false;
    }

    fn localConsumeIncrefCount(self: *const RcInsertPass, value: LocalId, consume_uses: u32) u32 {
        if (consume_uses == 0) return 0;
        if (!self.layoutNeedsRc(self.localLayout(value))) return 0;

        if (self.local_definition_kinds.get(localKey(value))) |def_kind| switch (def_kind) {
            .stmt_result => |semantics| switch (semantics) {
                .borrow_of => return 0,
                else => {},
            },
            else => {},
        };

        var current = value;
        var steps: u32 = 0;
        while (self.local_alias_sources.get(localKey(current))) |source| {
            if (!source.forwards_ownership) return consume_uses;
            current = @enumFromInt(@as(u32, @intCast(source.owner_key)));
            steps += 1;
            if (builtin.mode == .Debug and steps > 1024) {
                std.debug.panic(
                    "RcInsertPass invariant violated: alias chain did not converge while checking consume incref count for local {d}",
                    .{@intFromEnum(value)},
                );
            }
        }

        return consume_uses -| 1;
    }

    fn prependConsumeArgIncrefs(self: *RcInsertPass, consume_args: []const LocalId, next: CFStmtId) Allocator.Error!CFStmtId {
        var rewritten = next;
        var unique_args: std.ArrayListUnmanaged(LocalId) = .empty;
        defer unique_args.deinit(self.allocator);
        var consume_counts: std.ArrayListUnmanaged(u32) = .empty;
        defer consume_counts.deinit(self.allocator);

        for (consume_args) |arg| {
            if (!self.layoutNeedsRc(self.localLayout(arg))) continue;

            for (unique_args.items, 0..) |seen, i| {
                if (seen == arg) {
                    consume_counts.items[i] += 1;
                    break;
                }
            } else {
                try unique_args.append(self.allocator, arg);
                try consume_counts.append(self.allocator, 1);
            }
        }

        for (unique_args.items, consume_counts.items) |arg, consume_count| {
            const incref_count = self.localConsumeIncrefCount(arg, consume_count);
            if (incref_count == 0) continue;
            rewritten = try self.store.addCFStmt(.{ .incref = .{
                .value = arg,
                .count = @intCast(incref_count),
                .next = rewritten,
            } });
        }

        return rewritten;
    }

    fn prependRetainedBorrowArgIncrefs(self: *RcInsertPass, borrowed_args: []const LocalId, next: CFStmtId) Allocator.Error!CFStmtId {
        var rewritten = next;
        var unique_args: std.ArrayListUnmanaged(LocalId) = .empty;
        defer unique_args.deinit(self.allocator);
        var retain_counts: std.ArrayListUnmanaged(u32) = .empty;
        defer retain_counts.deinit(self.allocator);

        for (borrowed_args) |arg| {
            if (!self.layoutNeedsRc(self.localLayout(arg))) continue;

            for (unique_args.items, 0..) |seen, i| {
                if (seen == arg) {
                    retain_counts.items[i] += 1;
                    break;
                }
            } else {
                try unique_args.append(self.allocator, arg);
                try retain_counts.append(self.allocator, 1);
            }
        }

        for (unique_args.items, retain_counts.items) |arg, retain_count| {
            rewritten = try self.store.addCFStmt(.{ .incref = .{
                .value = arg,
                .count = @intCast(retain_count),
                .next = rewritten,
            } });
        }

        return rewritten;
    }

    fn prependJoinParamDecrefs(
        self: *RcInsertPass,
        params: []const LocalId,
        forwarded_values: []const LocalId,
        next: CFStmtId,
        external_jump_target: ?LIR.JoinPointId,
    ) Allocator.Error!CFStmtId {
        var rewritten = next;
        var closed_roots: std.ArrayListUnmanaged(u64) = .empty;
        defer closed_roots.deinit(self.allocator);
        for (params) |param| {
            if (!self.layoutNeedsRc(self.localLayout(param))) continue;
            if (!self.localCarriesOwnedValue(param)) continue;
            const param_root = self.ownershipRootForConsumedValue(param);

            var forwarded_to_param_peer = false;
            for (params) |other_param| {
                if (other_param == param) continue;
                if (self.localForwardsOwnership(other_param, param)) {
                    forwarded_to_param_peer = true;
                    break;
                }
            }
            if (forwarded_to_param_peer) continue;

            var forwarded = false;
            var forwarded_by_local: ?LocalId = null;
            var forwarded_by_root: ?LocalId = null;
            for (forwarded_values) |value| {
                if (self.localForwardsOwnership(value, param)) {
                    forwarded = true;
                    forwarded_by_local = value;
                    break;
                }
                if (param_root) |root| {
                    if (self.ownershipRootForConsumedValue(value)) |forwarded_root| {
                        if (forwarded_root == root) {
                            forwarded = true;
                            forwarded_by_root = value;
                            break;
                        }
                    }
                }
            }
            if (forwarded) continue;

            if (param_root) |root| {
                const root_key = localKey(root);
                var already_closed = false;
                for (closed_roots.items) |seen_root_key| {
                    if (seen_root_key == root_key) {
                        already_closed = true;
                        break;
                    }
                }
                if (already_closed) continue;
                try closed_roots.append(self.allocator, root_key);
            }

            if (external_jump_target) |target_join_id| {
                if (param_root) |root| {
                    if (self.joinBodyUsesOwnershipRoot(target_join_id, root)) continue;
                }
            }

            if (self.forwardingAliasRepresentative(param)) |alias_root| {
                if (self.local_alias_sources.contains(localKey(alias_root))) continue;
            }

            rewritten = try self.store.addCFStmt(.{ .decref = .{
                .value = param,
                .next = rewritten,
            } });
        }
        return rewritten;
    }

    fn localUseTouchesOwnershipRoot(self: *const RcInsertPass, local: LocalId, root: LocalId) bool {
        return self.localTransfersOwnership(local, root);
    }

    fn stmtUsesOwnershipRoot(self: *const RcInsertPass, stmt_id: CFStmtId, root: LocalId) bool {
        const stmt = self.store.getCFStmt(stmt_id);
        switch (stmt) {
            .assign_symbol, .assign_literal, .runtime_error, .scope_exit, .crash => return false,
            .assign_ref => |assign| {
                if (self.localUseTouchesOwnershipRoot(refOpSource(assign.op), root)) return true;
                return self.stmtUsesOwnershipRoot(assign.next, root);
            },
            .assign_call => |assign| {
                for (self.store.getLocalSpan(assign.args)) |arg| {
                    if (self.localUseTouchesOwnershipRoot(arg, root)) return true;
                }
                return self.stmtUsesOwnershipRoot(assign.next, root);
            },
            .assign_low_level => |assign| {
                for (self.store.getLocalSpan(assign.args)) |arg| {
                    if (self.localUseTouchesOwnershipRoot(arg, root)) return true;
                }
                return self.stmtUsesOwnershipRoot(assign.next, root);
            },
            .assign_list => |assign| {
                for (self.store.getLocalSpan(assign.elems)) |arg| {
                    if (self.localUseTouchesOwnershipRoot(arg, root)) return true;
                }
                return self.stmtUsesOwnershipRoot(assign.next, root);
            },
            .assign_struct => |assign| {
                for (self.store.getLocalSpan(assign.fields)) |arg| {
                    if (self.localUseTouchesOwnershipRoot(arg, root)) return true;
                }
                return self.stmtUsesOwnershipRoot(assign.next, root);
            },
            .assign_tag => |assign| {
                for (self.store.getLocalSpan(assign.args)) |arg| {
                    if (self.localUseTouchesOwnershipRoot(arg, root)) return true;
                }
                return self.stmtUsesOwnershipRoot(assign.next, root);
            },
            .debug => |stmt_debug| {
                if (self.localUseTouchesOwnershipRoot(stmt_debug.message, root)) return true;
                return self.stmtUsesOwnershipRoot(stmt_debug.next, root);
            },
            .expect => |stmt_expect| {
                if (self.localUseTouchesOwnershipRoot(stmt_expect.condition, root)) return true;
                return self.stmtUsesOwnershipRoot(stmt_expect.next, root);
            },
            .incref => |inc| {
                if (self.localUseTouchesOwnershipRoot(inc.value, root)) return true;
                return self.stmtUsesOwnershipRoot(inc.next, root);
            },
            .decref => |dec| {
                if (self.localUseTouchesOwnershipRoot(dec.value, root)) return true;
                return self.stmtUsesOwnershipRoot(dec.next, root);
            },
            .free => |free_stmt| {
                if (self.localUseTouchesOwnershipRoot(free_stmt.value, root)) return true;
                return self.stmtUsesOwnershipRoot(free_stmt.next, root);
            },
            .switch_stmt => |switch_stmt| {
                if (self.localUseTouchesOwnershipRoot(switch_stmt.cond, root)) return true;
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    if (self.stmtUsesOwnershipRoot(branch.body, root)) return true;
                }
                return self.stmtUsesOwnershipRoot(switch_stmt.default_branch, root);
            },
            .borrow_scope => |scope| {
                if (self.stmtUsesOwnershipRoot(scope.body, root)) return true;
                return self.stmtUsesOwnershipRoot(scope.remainder, root);
            },
            .join => |join| {
                if (self.stmtUsesOwnershipRoot(join.body, root)) return true;
                return self.stmtUsesOwnershipRoot(join.remainder, root);
            },
            .jump => |jump| {
                for (self.store.getLocalSpan(jump.args)) |arg| {
                    if (self.localUseTouchesOwnershipRoot(arg, root)) return true;
                }
                return false;
            },
            .ret => |ret_stmt| return self.localUseTouchesOwnershipRoot(ret_stmt.value, root),
        }
    }

    fn joinBodyUsesOwnershipRoot(self: *const RcInsertPass, join_id: LIR.JoinPointId, root: LocalId) bool {
        const join_info = self.joins_by_id.get(@intFromEnum(join_id)) orelse std.debug.panic(
            "RcInsertPass invariant violated: missing join info for join {d}",
            .{@intFromEnum(join_id)},
        );
        return self.stmtUsesOwnershipRoot(join_info.body, root);
    }

    fn appendReachableJoinIds(
        self: *RcInsertPass,
        stmt_id: CFStmtId,
        ids: *std.ArrayListUnmanaged(LIR.JoinPointId),
    ) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try self.appendReachableJoinIds(assign.next, ids),
            .assign_ref => |assign| try self.appendReachableJoinIds(assign.next, ids),
            .assign_literal => |assign| try self.appendReachableJoinIds(assign.next, ids),
            .assign_call => |assign| try self.appendReachableJoinIds(assign.next, ids),
            .assign_low_level => |assign| try self.appendReachableJoinIds(assign.next, ids),
            .assign_list => |assign| try self.appendReachableJoinIds(assign.next, ids),
            .assign_struct => |assign| try self.appendReachableJoinIds(assign.next, ids),
            .assign_tag => |assign| try self.appendReachableJoinIds(assign.next, ids),
            .debug => |stmt_debug| try self.appendReachableJoinIds(stmt_debug.next, ids),
            .expect => |stmt_expect| try self.appendReachableJoinIds(stmt_expect.next, ids),
            .incref => |inc| try self.appendReachableJoinIds(inc.next, ids),
            .decref => |dec| try self.appendReachableJoinIds(dec.next, ids),
            .free => |free_stmt| try self.appendReachableJoinIds(free_stmt.next, ids),
            .switch_stmt => |switch_stmt| {
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.appendReachableJoinIds(branch.body, ids);
                }
                try self.appendReachableJoinIds(switch_stmt.default_branch, ids);
            },
            .borrow_scope => |scope| {
                try self.appendReachableJoinIds(scope.body, ids);
                try self.appendReachableJoinIds(scope.remainder, ids);
            },
            .join => |join| {
                for (ids.items) |existing| {
                    if (existing == join.id) break;
                } else {
                    try ids.append(self.allocator, join.id);
                }
                try self.appendReachableJoinIds(join.body, ids);
                try self.appendReachableJoinIds(join.remainder, ids);
            },
            .jump, .ret, .runtime_error, .scope_exit, .crash => {},
        }
    }

    fn closeJoinParamsOnExit(
        self: *RcInsertPass,
        params: []const LocalId,
        internal_join_ids: []const LIR.JoinPointId,
        stmt_id: CFStmtId,
    ) Allocator.Error!CFStmtId {
        return self.closeJoinParamsOnExitMode(params, internal_join_ids, stmt_id, true);
    }

    fn closeJoinParamsOnExitMode(
        self: *RcInsertPass,
        params: []const LocalId,
        internal_join_ids: []const LIR.JoinPointId,
        stmt_id: CFStmtId,
        close_on_external_jump: bool,
    ) Allocator.Error!CFStmtId {
        const stmt = self.store.getCFStmt(stmt_id);
        return switch (stmt) {
            .assign_symbol => |assign| try self.store.addCFStmt(.{ .assign_symbol = .{
                .target = assign.target,
                .symbol = assign.symbol,
                .next = try self.closeJoinParamsOnExitMode(params, internal_join_ids, assign.next, close_on_external_jump),
            } }),
            .assign_ref => |assign| try self.store.addCFStmt(.{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .next = try self.closeJoinParamsOnExitMode(params, internal_join_ids, assign.next, close_on_external_jump),
            } }),
            .assign_literal => |assign| try self.store.addCFStmt(.{ .assign_literal = .{
                .target = assign.target,
                .result = assign.result,
                .value = assign.value,
                .next = try self.closeJoinParamsOnExitMode(params, internal_join_ids, assign.next, close_on_external_jump),
            } }),
            .assign_call => |assign| try self.store.addCFStmt(.{ .assign_call = .{
                .target = assign.target,
                .result = assign.result,
                .proc = assign.proc,
                .args = assign.args,
                .next = blk: {
                    const remaining = try self.remainingParamsAfterStmtConsumes(params, stmt);
                    defer if (remaining.ptr != params.ptr) self.allocator.free(remaining);
                    break :blk try self.closeJoinParamsOnExitMode(remaining, internal_join_ids, assign.next, close_on_external_jump);
                },
            } }),
            .assign_low_level => |assign| try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .args = assign.args,
                .next = blk: {
                    const remaining = try self.remainingParamsAfterStmtConsumes(params, stmt);
                    defer if (remaining.ptr != params.ptr) self.allocator.free(remaining);
                    break :blk try self.closeJoinParamsOnExitMode(remaining, internal_join_ids, assign.next, close_on_external_jump);
                },
            } }),
            .assign_list => |assign| try self.store.addCFStmt(.{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .elems = assign.elems,
                .next = blk: {
                    const remaining = try self.remainingParamsAfterStmtConsumes(params, stmt);
                    defer if (remaining.ptr != params.ptr) self.allocator.free(remaining);
                    break :blk try self.closeJoinParamsOnExitMode(remaining, internal_join_ids, assign.next, close_on_external_jump);
                },
            } }),
            .assign_struct => |assign| try self.store.addCFStmt(.{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .fields = assign.fields,
                .next = blk: {
                    const remaining = try self.remainingParamsAfterStmtConsumes(params, stmt);
                    defer if (remaining.ptr != params.ptr) self.allocator.free(remaining);
                    break :blk try self.closeJoinParamsOnExitMode(remaining, internal_join_ids, assign.next, close_on_external_jump);
                },
            } }),
            .assign_tag => |assign| try self.store.addCFStmt(.{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .discriminant = assign.discriminant,
                .args = assign.args,
                .next = blk: {
                    const remaining = try self.remainingParamsAfterStmtConsumes(params, stmt);
                    defer if (remaining.ptr != params.ptr) self.allocator.free(remaining);
                    break :blk try self.closeJoinParamsOnExitMode(remaining, internal_join_ids, assign.next, close_on_external_jump);
                },
            } }),
            .debug => |stmt_debug| try self.store.addCFStmt(.{ .debug = .{
                .message = stmt_debug.message,
                .next = try self.closeJoinParamsOnExitMode(params, internal_join_ids, stmt_debug.next, close_on_external_jump),
            } }),
            .expect => |stmt_expect| try self.store.addCFStmt(.{ .expect = .{
                .condition = stmt_expect.condition,
                .next = try self.closeJoinParamsOnExitMode(params, internal_join_ids, stmt_expect.next, close_on_external_jump),
            } }),
            .incref => |inc| try self.store.addCFStmt(.{ .incref = .{
                .value = inc.value,
                .count = inc.count,
                .next = try self.closeJoinParamsOnExitMode(params, internal_join_ids, inc.next, close_on_external_jump),
            } }),
            .decref => |dec| blk: {
                const remaining = try self.remainingParamsAfterStmtCloses(params, stmt);
                defer if (remaining.ptr != params.ptr) self.allocator.free(remaining);
                break :blk try self.store.addCFStmt(.{ .decref = .{
                    .value = dec.value,
                    .next = if (remaining.len == 0)
                        dec.next
                    else
                        try self.closeJoinParamsOnExitMode(remaining, internal_join_ids, dec.next, close_on_external_jump),
                } });
            },
            .free => |free_stmt| blk: {
                const remaining = try self.remainingParamsAfterStmtCloses(params, stmt);
                defer if (remaining.ptr != params.ptr) self.allocator.free(remaining);
                break :blk try self.store.addCFStmt(.{ .free = .{
                    .value = free_stmt.value,
                    .next = if (remaining.len == 0)
                        free_stmt.next
                    else
                        try self.closeJoinParamsOnExitMode(remaining, internal_join_ids, free_stmt.next, close_on_external_jump),
                } });
            },
            .switch_stmt => |switch_stmt| blk: {
                var rewritten_branches: std.ArrayListUnmanaged(LIR.CFSwitchBranch) = .empty;
                defer rewritten_branches.deinit(self.allocator);
                const original_branches = try self.cloneSwitchBranches(switch_stmt.branches);
                defer self.allocator.free(original_branches);

                for (original_branches) |branch| {
                    try rewritten_branches.append(self.allocator, .{
                        .value = branch.value,
                        .body = try self.closeJoinParamsOnExitMode(params, internal_join_ids, branch.body, close_on_external_jump),
                    });
                }

                break :blk try self.store.addCFStmt(.{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = try self.store.addCFSwitchBranches(rewritten_branches.items),
                    .default_branch = try self.closeJoinParamsOnExitMode(params, internal_join_ids, switch_stmt.default_branch, close_on_external_jump),
                } });
            },
            .borrow_scope => |scope| try self.store.addCFStmt(.{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.closeJoinParamsOnExitMode(params, internal_join_ids, scope.body, close_on_external_jump),
                .remainder = try self.closeJoinParamsOnExitMode(params, internal_join_ids, scope.remainder, close_on_external_jump),
            } }),
            .join => |join| blk: {
                const nested_join_ids = try self.allocator.alloc(LIR.JoinPointId, internal_join_ids.len + 1);
                defer self.allocator.free(nested_join_ids);
                @memcpy(nested_join_ids[0..internal_join_ids.len], internal_join_ids);
                nested_join_ids[internal_join_ids.len] = join.id;

                var body_internal_join_ids = std.ArrayListUnmanaged(LIR.JoinPointId).empty;
                defer body_internal_join_ids.deinit(self.allocator);
                try body_internal_join_ids.appendSlice(self.allocator, nested_join_ids);
                try self.appendReachableJoinIds(join.body, &body_internal_join_ids);
                try self.appendReachableJoinIds(join.remainder, &body_internal_join_ids);

                var remainder_internal_join_ids = std.ArrayListUnmanaged(LIR.JoinPointId).empty;
                defer remainder_internal_join_ids.deinit(self.allocator);
                try remainder_internal_join_ids.appendSlice(self.allocator, nested_join_ids);
                try self.appendReachableJoinIds(join.remainder, &remainder_internal_join_ids);

                const nested_params = self.store.getLocalSpan(join.params);
                var outer_params_for_nested_body: std.ArrayListUnmanaged(LocalId) = .empty;
                defer outer_params_for_nested_body.deinit(self.allocator);
                var outer_params_for_nested_remainder: std.ArrayListUnmanaged(LocalId) = .empty;
                defer outer_params_for_nested_remainder.deinit(self.allocator);
                for (params) |param| {
                    var shadowed_by_nested_join = false;
                    for (nested_params) |nested_param| {
                        if (self.localTransfersOwnership(nested_param, param)) {
                            shadowed_by_nested_join = true;
                            break;
                        }
                    }
                    const descendant_transfer_in_body = self.stmtContainsDescendantJoinOwnershipTransfer(join.body, param);
                    const descendant_transfer_in_remainder = self.stmtContainsDescendantJoinOwnershipTransfer(join.remainder, param);
                    if (!shadowed_by_nested_join and !descendant_transfer_in_remainder) {
                        try outer_params_for_nested_body.append(self.allocator, param);
                    }

                    if (!descendant_transfer_in_body) {
                        try outer_params_for_nested_remainder.append(self.allocator, param);
                    }
                }
                const outer_body_params = outer_params_for_nested_body.items;
                const outer_remainder_params = outer_params_for_nested_remainder.items;

                break :blk try self.store.addCFStmt(.{ .join = .{
                    .id = join.id,
                    .params = join.params,
                    .body = try self.closeJoinParamsOnExitMode(
                        outer_body_params,
                        body_internal_join_ids.items,
                        join.body,
                        close_on_external_jump,
                    ),
                    .remainder = try self.closeJoinParamsOnExitMode(
                        outer_remainder_params,
                        remainder_internal_join_ids.items,
                        join.remainder,
                        close_on_external_jump,
                    ),
                } });
            },
            .jump => |jump| if (for (internal_join_ids) |internal_join_id| {
                if (jump.target == internal_join_id) break true;
            } else false)
                stmt_id
            else if (!close_on_external_jump)
                stmt_id
            else
                try self.prependJoinParamDecrefs(params, self.store.getLocalSpan(jump.args), stmt_id, jump.target),
            .ret => |ret_stmt| blk: {
                const forwarded = [_]LocalId{ret_stmt.value};
                break :blk try self.prependJoinParamDecrefs(params, &forwarded, stmt_id, null);
            },
            .runtime_error, .crash => try self.prependJoinParamDecrefs(params, &.{}, stmt_id, null),
            .scope_exit => stmt_id,
        };
    }

    fn stmtContainsDescendantJoinOwnershipTransfer(
        self: *const RcInsertPass,
        stmt_id: CFStmtId,
        owner: LocalId,
    ) bool {
        return switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| self.stmtContainsDescendantJoinOwnershipTransfer(assign.next, owner),
            .assign_ref => |assign| self.stmtContainsDescendantJoinOwnershipTransfer(assign.next, owner),
            .assign_literal => |assign| self.stmtContainsDescendantJoinOwnershipTransfer(assign.next, owner),
            .assign_call => |assign| self.stmtContainsDescendantJoinOwnershipTransfer(assign.next, owner),
            .assign_low_level => |assign| self.stmtContainsDescendantJoinOwnershipTransfer(assign.next, owner),
            .assign_list => |assign| self.stmtContainsDescendantJoinOwnershipTransfer(assign.next, owner),
            .assign_struct => |assign| self.stmtContainsDescendantJoinOwnershipTransfer(assign.next, owner),
            .assign_tag => |assign| self.stmtContainsDescendantJoinOwnershipTransfer(assign.next, owner),
            .debug => |stmt_debug| self.stmtContainsDescendantJoinOwnershipTransfer(stmt_debug.next, owner),
            .expect => |stmt_expect| self.stmtContainsDescendantJoinOwnershipTransfer(stmt_expect.next, owner),
            .incref => |inc| self.stmtContainsDescendantJoinOwnershipTransfer(inc.next, owner),
            .decref => |dec| self.stmtContainsDescendantJoinOwnershipTransfer(dec.next, owner),
            .free => |free_stmt| self.stmtContainsDescendantJoinOwnershipTransfer(free_stmt.next, owner),
            .switch_stmt => |switch_stmt| blk: {
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    if (self.stmtContainsDescendantJoinOwnershipTransfer(branch.body, owner)) break :blk true;
                }
                break :blk self.stmtContainsDescendantJoinOwnershipTransfer(switch_stmt.default_branch, owner);
            },
            .borrow_scope => |scope| self.stmtContainsDescendantJoinOwnershipTransfer(scope.body, owner) or
                self.stmtContainsDescendantJoinOwnershipTransfer(scope.remainder, owner),
            .join => |join| blk: {
                for (self.store.getLocalSpan(join.params)) |param| {
                    if (self.localTransfersOwnership(param, owner)) break :blk true;
                }
                break :blk self.stmtContainsDescendantJoinOwnershipTransfer(join.body, owner) or
                    self.stmtContainsDescendantJoinOwnershipTransfer(join.remainder, owner);
            },
            .jump, .ret, .runtime_error, .scope_exit, .crash => false,
        };
    }

    fn processStmt(self: *RcInsertPass, stmt_id: CFStmtId) Allocator.Error!CFStmtId {
        const stmt = self.store.getCFStmt(stmt_id);
        return switch (stmt) {
            .assign_symbol => |assign| try self.processAssignSymbol(assign),
            .assign_ref => |assign| try self.processAssignRef(assign),
            .assign_literal => |assign| try self.processAssignLiteral(assign),
            .assign_call => |assign| try self.processAssignCall(assign),
            .assign_low_level => |assign| try self.processAssignLowLevel(assign),
            .assign_list => |assign| try self.processAssignList(assign),
            .assign_struct => |assign| try self.processAssignStruct(assign),
            .assign_tag => |assign| try self.processAssignTag(assign),
            .debug => |debug_stmt| try self.store.addCFStmt(.{ .debug = .{
                .message = debug_stmt.message,
                .next = try self.processStmt(debug_stmt.next),
            } }),
            .expect => |expect_stmt| try self.store.addCFStmt(.{ .expect = .{
                .condition = expect_stmt.condition,
                .next = try self.processStmt(expect_stmt.next),
            } }),
            .incref, .decref, .free, .runtime_error => stmt_id,
            .switch_stmt => |switch_stmt| blk: {
                var rewritten_branches: std.ArrayListUnmanaged(LIR.CFSwitchBranch) = .empty;
                defer rewritten_branches.deinit(self.allocator);
                const original_branches = try self.cloneSwitchBranches(switch_stmt.branches);
                defer self.allocator.free(original_branches);

                for (original_branches) |branch| {
                    try rewritten_branches.append(self.allocator, .{
                        .value = branch.value,
                        .body = try self.processStmt(branch.body),
                    });
                }

                const branch_span = try self.store.addCFSwitchBranches(rewritten_branches.items);
                break :blk try self.store.addCFStmt(.{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = branch_span,
                    .default_branch = try self.processStmt(switch_stmt.default_branch),
                } });
            },
            .join => |join| blk: {
                var rewritten_body = try self.processStmt(join.body);
                const internal_join_ids = [_]LIR.JoinPointId{join.id};
                rewritten_body = try self.closeJoinParamsOnExit(
                    self.store.getLocalSpan(join.params),
                    &internal_join_ids,
                    rewritten_body,
                );

                break :blk try self.store.addCFStmt(.{ .join = .{
                    .id = join.id,
                    .params = join.params,
                    .body = rewritten_body,
                    .remainder = try self.processStmt(join.remainder),
                } });
            },
            .borrow_scope => |scope| try self.store.addCFStmt(.{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.processStmt(scope.body),
                .remainder = try self.processStmt(scope.remainder),
            } }),
            .scope_exit, .jump, .ret, .crash => stmt_id,
        };
    }

    fn wrapAssignLike(self: *RcInsertPass, stmt: CFStmt, target: LocalId, next_raw: CFStmtId) Allocator.Error!CFStmtId {
        return self.wrapAssignLikeProcessedNext(stmt, target, try self.processStmt(next_raw));
    }

    fn wrapAssignLikeProcessedNext(
        self: *RcInsertPass,
        stmt: CFStmt,
        target: LocalId,
        next_processed: CFStmtId,
    ) Allocator.Error!CFStmtId {
        const use_count = self.symbol_use_counts.get(localKey(target)) orelse 0;
        var next = next_processed;
        const target_layout = self.localLayout(target);
        if (self.layoutNeedsRc(target_layout) and use_count > 0) {
            if (resultIsFresh(stmt)) {
                next = try self.closeLocalOnExit(target, next);
            } else if (self.closesOwningAliasRepresentative(stmt, target)) {
                const summary = self.useSummary(target);
                if (summary.consume_count == 0 and self.directNonForwardingBorrowCount(target) != 0) {
                    next = try self.closeLocalOnExit(target, next);
                }
            }
        }

        if (self.layoutNeedsRc(target_layout) and resultIsFresh(stmt)) {
            const summary = self.useSummary(target);
            if (summary.consume_count > 1) {
                next = try self.store.addCFStmt(.{ .incref = .{
                    .value = target,
                    .count = @intCast(summary.consume_count - 1),
                    .next = next,
                } });
            }
        }

        if (self.layoutNeedsRc(target_layout) and self.closesOwningAliasRepresentative(stmt, target)) {
            const summary = self.useSummary(target);
            if (summary.consume_count > 1) {
                next = try self.store.addCFStmt(.{ .incref = .{
                    .value = target,
                    .count = @intCast(summary.consume_count - 1),
                    .next = next,
                } });
            }
        }

        if (self.layoutNeedsRc(target_layout) and use_count == 0 and resultIsFresh(stmt)) {
            next = try self.store.addCFStmt(.{ .decref = .{
                .value = target,
                .next = next,
            } });
        }

        return try self.rebuildAssignStmt(stmt, next);
    }

    fn rebuildAssignStmt(self: *RcInsertPass, stmt: CFStmt, next: CFStmtId) Allocator.Error!CFStmtId {
        return switch (stmt) {
            .assign_symbol => |assign| self.store.addCFStmt(.{ .assign_symbol = .{
                .target = assign.target,
                .symbol = assign.symbol,
                .next = next,
            } }),
            .assign_ref => |assign| self.store.addCFStmt(.{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
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
            .assign_low_level => |assign| self.store.addCFStmt(.{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .args = assign.args,
                .next = next,
            } }),
            .assign_list => |assign| self.store.addCFStmt(.{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .elems = assign.elems,
                .next = next,
            } }),
            .assign_struct => |assign| self.store.addCFStmt(.{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .fields = assign.fields,
                .next = next,
            } }),
            .assign_tag => |assign| self.store.addCFStmt(.{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .discriminant = assign.discriminant,
                .args = assign.args,
                .next = next,
            } }),
            else => unreachable,
        };
    }

    fn processAssignRef(self: *RcInsertPass, assign: AssignRefStmt) Allocator.Error!CFStmtId {
        const target_layout = self.localLayout(assign.target);
        const target_use_count = self.symbol_use_counts.get(localKey(assign.target)) orelse 0;
        if (self.layoutNeedsRc(target_layout) and target_use_count == 0) {
            return self.processStmt(assign.next);
        }
        var next = try self.processStmt(assign.next);

        if (self.layoutNeedsRc(target_layout)) switch (assign.result) {
            .borrow_of => {
                const summary = self.useSummary(assign.target);
                if (summary.consume_count > 0) {
                    next = try self.store.addCFStmt(.{ .incref = .{
                        .value = assign.target,
                        .count = @intCast(summary.consume_count),
                        .next = next,
                    } });
                }
            },
            else => {},
        };

        return self.wrapAssignLikeProcessedNext(.{ .assign_ref = assign }, assign.target, next);
    }

    fn processAssignSymbol(self: *RcInsertPass, assign: AssignSymbolStmt) Allocator.Error!CFStmtId {
        return self.wrapAssignLike(.{ .assign_symbol = assign }, assign.target, assign.next);
    }

    fn processAssignLiteral(self: *RcInsertPass, assign: AssignLiteralStmt) Allocator.Error!CFStmtId {
        return self.wrapAssignLike(.{ .assign_literal = assign }, assign.target, assign.next);
    }

    fn processAssignCall(self: *RcInsertPass, assign: AssignCallStmt) Allocator.Error!CFStmtId {
        const rewritten = try self.wrapAssignLike(.{ .assign_call = assign }, assign.target, assign.next);
        const args = self.store.getLocalSpan(assign.args);
        const proc_index = @intFromEnum(assign.proc);
        const param_use_kinds = if (proc_index < self.proc_param_use_kinds.items.len)
            self.proc_param_use_kinds.items[proc_index]
        else
            &.{};
        if (builtin.mode == .Debug and param_use_kinds.len != 0 and args.len != param_use_kinds.len) {
            std.debug.panic(
                "RcInsertPass invariant violated: proc {d} called with {d} args but ownership summary expects {d}",
                .{ proc_index, args.len, param_use_kinds.len },
            );
        }
        if (param_use_kinds.len == 0) return rewritten;

        var consume_args: std.ArrayListUnmanaged(LocalId) = .empty;
        defer consume_args.deinit(self.allocator);
        var consumed_borrow_args: std.ArrayListUnmanaged(LocalId) = .empty;
        defer consumed_borrow_args.deinit(self.allocator);
        for (args, param_use_kinds) |arg, use_kind| {
            if (use_kind != .consume) continue;
            try consume_args.append(self.allocator, arg);
            if (!self.localCarriesOwnedValue(arg)) {
                try consumed_borrow_args.append(self.allocator, arg);
            }
        }
        const with_consumed_borrow_increfs = try self.prependRetainedBorrowArgIncrefs(
            consumed_borrow_args.items,
            rewritten,
        );
        return self.prependConsumeArgIncrefs(consume_args.items, with_consumed_borrow_increfs);
    }

    fn processAssignLowLevel(self: *RcInsertPass, assign: AssignLowLevelStmt) Allocator.Error!CFStmtId {
        const rewritten = try self.wrapAssignLike(.{ .assign_low_level = assign }, assign.target, assign.next);
        const args = self.store.getLocalSpan(assign.args);
        const arg_ownership = assign.op.getArgOwnership();
        if (builtin.mode == .Debug and args.len != arg_ownership.len) {
            std.debug.panic(
                "RcInsertPass invariant violated: low-level {s} passed {d} args but ownership table expects {d}",
                .{ @tagName(assign.op), args.len, arg_ownership.len },
            );
        }

        var consume_args: std.ArrayListUnmanaged(LocalId) = .empty;
        defer consume_args.deinit(self.allocator);
        var consumed_borrow_args: std.ArrayListUnmanaged(LocalId) = .empty;
        defer consumed_borrow_args.deinit(self.allocator);
        var retained_borrow_args: std.ArrayListUnmanaged(LocalId) = .empty;
        defer retained_borrow_args.deinit(self.allocator);
        for (args, arg_ownership) |arg, ownership| {
            if (ownership == .consume) {
                try consume_args.append(self.allocator, arg);
                if (!self.localCarriesOwnedValue(arg)) {
                    try consumed_borrow_args.append(self.allocator, arg);
                }
            }
        }
        for (args, arg_ownership, 0..) |arg, ownership, i| {
            if (ownership != .borrow) continue;
            if (!assign.op.borrowedArgRetainedByResult(i)) continue;
            try retained_borrow_args.append(self.allocator, arg);
        }
        const with_retained_borrow_increfs = try self.prependRetainedBorrowArgIncrefs(
            retained_borrow_args.items,
            rewritten,
        );
        const with_consumed_borrow_increfs = try self.prependRetainedBorrowArgIncrefs(
            consumed_borrow_args.items,
            with_retained_borrow_increfs,
        );
        return self.prependConsumeArgIncrefs(consume_args.items, with_consumed_borrow_increfs);
    }

    fn processAssignList(self: *RcInsertPass, assign: AssignListStmt) Allocator.Error!CFStmtId {
        const rewritten = try self.wrapAssignLike(.{ .assign_list = assign }, assign.target, assign.next);
        const elems = self.store.getLocalSpan(assign.elems);
        var retained_borrow_args: std.ArrayListUnmanaged(LocalId) = .empty;
        defer retained_borrow_args.deinit(self.allocator);
        for (elems) |elem| {
            if (self.localCarriesOwnedValue(elem)) continue;
            try retained_borrow_args.append(self.allocator, elem);
        }
        const with_retained_borrow_increfs = try self.prependRetainedBorrowArgIncrefs(
            retained_borrow_args.items,
            rewritten,
        );
        return self.prependConsumeArgIncrefs(elems, with_retained_borrow_increfs);
    }

    fn processAssignStruct(self: *RcInsertPass, assign: AssignStructStmt) Allocator.Error!CFStmtId {
        const rewritten = try self.wrapAssignLike(.{ .assign_struct = assign }, assign.target, assign.next);
        const fields = self.store.getLocalSpan(assign.fields);
        var retained_borrow_args: std.ArrayListUnmanaged(LocalId) = .empty;
        defer retained_borrow_args.deinit(self.allocator);
        for (fields) |field| {
            if (self.localCarriesOwnedValue(field)) continue;
            try retained_borrow_args.append(self.allocator, field);
        }
        const with_retained_borrow_increfs = try self.prependRetainedBorrowArgIncrefs(
            retained_borrow_args.items,
            rewritten,
        );
        return self.prependConsumeArgIncrefs(fields, with_retained_borrow_increfs);
    }

    fn processAssignTag(self: *RcInsertPass, assign: AssignTagStmt) Allocator.Error!CFStmtId {
        const rewritten = try self.wrapAssignLike(.{ .assign_tag = assign }, assign.target, assign.next);
        const args = self.store.getLocalSpan(assign.args);
        var retained_borrow_args: std.ArrayListUnmanaged(LocalId) = .empty;
        defer retained_borrow_args.deinit(self.allocator);
        for (args) |arg| {
            if (self.localCarriesOwnedValue(arg)) continue;
            try retained_borrow_args.append(self.allocator, arg);
        }
        const with_retained_borrow_increfs = try self.prependRetainedBorrowArgIncrefs(
            retained_borrow_args.items,
            rewritten,
        );
        return self.prependConsumeArgIncrefs(args, with_retained_borrow_increfs);
    }
};
