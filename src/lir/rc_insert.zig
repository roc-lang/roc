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

/// Inserts explicit reference-count statements into statement-only LIR.
pub const RcInsertPass = struct {
    allocator: Allocator,
    store: *LirStore,
    layout_store: *const layout_mod.Store,

    symbol_use_counts: std.AutoHashMap(u64, u32),
    symbol_use_summaries: std.AutoHashMap(u64, UseSummary),
    local_alias_sources: std.AutoHashMap(u64, AliasSource),
    active_join_params: std.AutoHashMap(u32, LIR.LocalSpan),
    proc_param_use_kinds: std.ArrayListUnmanaged([]UseKind),

    /// Initializes an RC insertion pass over the provided LIR store.
    pub fn init(allocator: Allocator, store: *LirStore, layout_store: *const layout_mod.Store) Allocator.Error!RcInsertPass {
        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = layout_store,
            .symbol_use_counts = std.AutoHashMap(u64, u32).init(allocator),
            .symbol_use_summaries = std.AutoHashMap(u64, UseSummary).init(allocator),
            .local_alias_sources = std.AutoHashMap(u64, AliasSource).init(allocator),
            .active_join_params = std.AutoHashMap(u32, LIR.LocalSpan).init(allocator),
            .proc_param_use_kinds = .empty,
        };
    }

    /// Releases all temporary analysis state owned by this pass.
    pub fn deinit(self: *RcInsertPass) void {
        self.symbol_use_counts.deinit();
        self.symbol_use_summaries.deinit();
        self.local_alias_sources.deinit();
        self.active_join_params.deinit();
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
        try self.countUsesInStmt(proc.body);
        try self.collectAliasSemanticsInStmt(proc.body);
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
        self.local_alias_sources.clearRetainingCapacity();
        self.active_join_params.clearRetainingCapacity();
    }

    fn rebuildProcParamUseKinds(self: *RcInsertPass) Allocator.Error!void {
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
                const new_use_kinds = try self.analyzeProcParamUseKinds(proc);
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

    fn analyzeProcParamUseKinds(self: *RcInsertPass, proc: LIR.LirProcSpec) Allocator.Error![]UseKind {
        self.clearAnalysisState();
        try self.countUsesInStmt(proc.body);

        const params = self.store.getLocalSpan(proc.args);
        const use_kinds = try self.allocator.alloc(UseKind, params.len);
        for (params, 0..) |param, i| {
            const summary = self.useSummary(param);
            const returned_aliases_param = switch (proc.result_contract) {
                .alias_of_param => |contract| contract.param_index == i,
                else => false,
            };
            use_kinds[i] = if (summary.consume_count != 0 or returned_aliases_param) .consume else .borrow;
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

    fn countUsesInLocals(self: *RcInsertPass, locals: []const LocalId) Allocator.Error!void {
        for (locals) |local| try self.bumpUse(local);
    }

    fn mergeUseSummary(self: *RcInsertPass, local: LocalId, summary: UseSummary) Allocator.Error!void {
        if (!self.layoutNeedsRc(self.localLayout(local))) return;
        const gop = try self.symbol_use_summaries.getOrPut(localKey(local));
        if (!gop.found_existing) gop.value_ptr.* = .{};
        gop.value_ptr.borrow_count += summary.borrow_count;
        gop.value_ptr.consume_count += summary.consume_count;
    }

    fn markUse(self: *RcInsertPass, local: LocalId, kind: UseKind) Allocator.Error!void {
        if (!self.layoutNeedsRc(self.localLayout(local))) return;
        try self.bumpUse(local);
        const gop = try self.symbol_use_summaries.getOrPut(localKey(local));
        if (!gop.found_existing) gop.value_ptr.* = .{};
        switch (kind) {
            .borrow => gop.value_ptr.borrow_count += 1,
            .consume => gop.value_ptr.consume_count += 1,
        }
    }

    fn useSummary(self: *const RcInsertPass, local: LocalId) UseSummary {
        return self.symbol_use_summaries.get(localKey(local)) orelse .{};
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
                try self.mergeUseSummary(borrowed.owner, target_summary);
            },
        }
    }

    fn countUsesInStmt(self: *RcInsertPass, stmt_id: CFStmtId) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try self.countUsesInStmt(assign.next),
            .assign_ref => |assign| {
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
                for (args, param_locals) |arg, _| {
                    try self.markUse(arg, .consume);
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
            .alias_of => |source| try self.local_alias_sources.put(target_key, .{
                .owner_key = localKey(source.owner),
                .forwards_ownership = true,
            }),
            .borrow_of => |borrowed| try self.local_alias_sources.put(target_key, .{
                .owner_key = localKey(borrowed.owner),
                .forwards_ownership = false,
            }),
        }
    }

    fn collectAliasSemanticsInStmt(self: *RcInsertPass, stmt_id: CFStmtId) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try self.collectAliasSemanticsInStmt(assign.next),
            .assign_ref => |assign| {
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
                }
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .assign_literal => |assign| {
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
                }
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .assign_call => |assign| {
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
                }
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .assign_low_level => |assign| {
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
                }
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .assign_list => |assign| {
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
                }
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .assign_struct => |assign| {
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
                }
                try self.collectAliasSemanticsInStmt(assign.next);
            },
            .assign_tag => |assign| {
                if (self.layoutNeedsRc(self.localLayout(assign.target))) {
                    try self.recordAliasSemantics(assign.target, assign.result);
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

    fn appendDecrefToTerminalPaths(self: *RcInsertPass, value: LocalId, stmt_id: CFStmtId) Allocator.Error!CFStmtId {
        return self.appendDecrefToTerminalPathsRec(value, &.{}, stmt_id);
    }

    fn appendDecrefToTerminalPathsRec(
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
                .next = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, assign.next),
            } }),
            .assign_ref => |assign| try self.store.addCFStmt(.{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .next = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, assign.next),
            } }),
            .assign_literal => |assign| try self.store.addCFStmt(.{ .assign_literal = .{
                .target = assign.target,
                .result = assign.result,
                .value = assign.value,
                .next = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, assign.next),
            } }),
            .assign_call => |assign| try self.store.addCFStmt(.{ .assign_call = .{
                .target = assign.target,
                .result = assign.result,
                .proc = assign.proc,
                .args = assign.args,
                .next = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, assign.next),
            } }),
            .assign_low_level => |assign| try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .args = assign.args,
                .next = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, assign.next),
            } }),
            .assign_list => |assign| try self.store.addCFStmt(.{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .elems = assign.elems,
                .next = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, assign.next),
            } }),
            .assign_struct => |assign| try self.store.addCFStmt(.{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .fields = assign.fields,
                .next = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, assign.next),
            } }),
            .assign_tag => |assign| try self.store.addCFStmt(.{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .discriminant = assign.discriminant,
                .args = assign.args,
                .next = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, assign.next),
            } }),
            .debug => |stmt_debug| try self.store.addCFStmt(.{ .debug = .{
                .message = stmt_debug.message,
                .next = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, stmt_debug.next),
            } }),
            .expect => |stmt_expect| try self.store.addCFStmt(.{ .expect = .{
                .condition = stmt_expect.condition,
                .next = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, stmt_expect.next),
            } }),
            .incref => |inc| try self.store.addCFStmt(.{ .incref = .{
                .value = inc.value,
                .count = inc.count,
                .next = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, inc.next),
            } }),
            .decref => |dec| try self.store.addCFStmt(.{ .decref = .{
                .value = dec.value,
                .next = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, dec.next),
            } }),
            .free => |free_stmt| try self.store.addCFStmt(.{ .free = .{
                .value = free_stmt.value,
                .next = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, free_stmt.next),
            } }),
            .switch_stmt => |switch_stmt| blk: {
                var rewritten_branches: std.ArrayListUnmanaged(LIR.CFSwitchBranch) = .empty;
                defer rewritten_branches.deinit(self.allocator);

                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try rewritten_branches.append(self.allocator, .{
                        .value = branch.value,
                        .body = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, branch.body),
                    });
                }

                break :blk try self.store.addCFStmt(.{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = try self.store.addCFSwitchBranches(rewritten_branches.items),
                    .default_branch = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, switch_stmt.default_branch),
                } });
            },
            .borrow_scope => |scope| try self.store.addCFStmt(.{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, scope.body),
                .remainder = try self.appendDecrefToTerminalPathsRec(value, internal_join_ids, scope.remainder),
            } }),
            .join => |join| blk: {
                const nested_join_ids = try self.allocator.alloc(LIR.JoinPointId, internal_join_ids.len + 1);
                defer self.allocator.free(nested_join_ids);
                @memcpy(nested_join_ids[0..internal_join_ids.len], internal_join_ids);
                nested_join_ids[internal_join_ids.len] = join.id;

                break :blk try self.store.addCFStmt(.{ .join = .{
                    .id = join.id,
                    .params = join.params,
                    .body = try self.appendDecrefToTerminalPathsRec(value, nested_join_ids, join.body),
                    .remainder = try self.appendDecrefToTerminalPathsRec(value, nested_join_ids, join.remainder),
                } });
            },
            .ret => |ret_stmt| try self.store.addCFStmt(.{ .decref = .{
                .value = value,
                .next = try self.store.addCFStmt(.{ .ret = ret_stmt }),
            } }),
            .runtime_error, .crash => try self.store.addCFStmt(.{ .decref = .{
                .value = value,
                .next = stmt_id,
            } }),
            .scope_exit => stmt_id,
            .jump => |jump| blk: {
                for (internal_join_ids) |internal_join_id| {
                    if (jump.target == internal_join_id) break :blk stmt_id;
                }
                break :blk try self.store.addCFStmt(.{ .decref = .{
                    .value = value,
                    .next = stmt_id,
                } });
            },
        };
    }

    fn localForwardsOwnership(self: *const RcInsertPass, value: LocalId, owner: LocalId) bool {
        var current = value;
        var steps: u32 = 0;
        while (true) {
            if (current == owner) return true;
            const source = self.local_alias_sources.get(localKey(current)) orelse return false;
            if (!source.forwards_ownership) return false;
            current = @enumFromInt(@as(u32, @intCast(source.owner_key)));
            steps += 1;
            if (builtin.mode == .Debug and steps > 1024) {
                std.debug.panic(
                    "RcInsertPass invariant violated: alias chain did not converge while checking ownership forwarding for local {d}",
                    .{@intFromEnum(value)},
                );
            }
        }
    }

    fn localConsumeIncrefCount(self: *const RcInsertPass, value: LocalId, consume_uses: u32) u32 {
        if (consume_uses == 0) return 0;
        if (!self.layoutNeedsRc(self.localLayout(value))) return 0;

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

    fn prependJoinParamDecrefs(
        self: *RcInsertPass,
        params: []const LocalId,
        forwarded_values: []const LocalId,
        next: CFStmtId,
    ) Allocator.Error!CFStmtId {
        var rewritten = next;
        for (params) |param| {
            if (!self.layoutNeedsRc(self.localLayout(param))) continue;

            var forwarded = false;
            for (forwarded_values) |value| {
                if (self.localForwardsOwnership(value, param)) {
                    forwarded = true;
                    break;
                }
            }
            if (forwarded) continue;

            rewritten = try self.store.addCFStmt(.{ .decref = .{
                .value = param,
                .next = rewritten,
            } });
        }
        return rewritten;
    }

    fn closeJoinParamsOnExit(
        self: *RcInsertPass,
        params: []const LocalId,
        internal_join_ids: []const LIR.JoinPointId,
        stmt_id: CFStmtId,
    ) Allocator.Error!CFStmtId {
        const stmt = self.store.getCFStmt(stmt_id);
        return switch (stmt) {
            .assign_symbol => |assign| try self.store.addCFStmt(.{ .assign_symbol = .{
                .target = assign.target,
                .symbol = assign.symbol,
                .next = try self.closeJoinParamsOnExit(params, internal_join_ids, assign.next),
            } }),
            .assign_ref => |assign| try self.store.addCFStmt(.{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .next = try self.closeJoinParamsOnExit(params, internal_join_ids, assign.next),
            } }),
            .assign_literal => |assign| try self.store.addCFStmt(.{ .assign_literal = .{
                .target = assign.target,
                .result = assign.result,
                .value = assign.value,
                .next = try self.closeJoinParamsOnExit(params, internal_join_ids, assign.next),
            } }),
            .assign_call => |assign| try self.store.addCFStmt(.{ .assign_call = .{
                .target = assign.target,
                .result = assign.result,
                .proc = assign.proc,
                .args = assign.args,
                .next = try self.closeJoinParamsOnExit(params, internal_join_ids, assign.next),
            } }),
            .assign_low_level => |assign| try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .args = assign.args,
                .next = try self.closeJoinParamsOnExit(params, internal_join_ids, assign.next),
            } }),
            .assign_list => |assign| try self.store.addCFStmt(.{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .elems = assign.elems,
                .next = try self.closeJoinParamsOnExit(params, internal_join_ids, assign.next),
            } }),
            .assign_struct => |assign| try self.store.addCFStmt(.{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .fields = assign.fields,
                .next = try self.closeJoinParamsOnExit(params, internal_join_ids, assign.next),
            } }),
            .assign_tag => |assign| try self.store.addCFStmt(.{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .discriminant = assign.discriminant,
                .args = assign.args,
                .next = try self.closeJoinParamsOnExit(params, internal_join_ids, assign.next),
            } }),
            .debug => |stmt_debug| try self.store.addCFStmt(.{ .debug = .{
                .message = stmt_debug.message,
                .next = try self.closeJoinParamsOnExit(params, internal_join_ids, stmt_debug.next),
            } }),
            .expect => |stmt_expect| try self.store.addCFStmt(.{ .expect = .{
                .condition = stmt_expect.condition,
                .next = try self.closeJoinParamsOnExit(params, internal_join_ids, stmt_expect.next),
            } }),
            .incref => |inc| try self.store.addCFStmt(.{ .incref = .{
                .value = inc.value,
                .count = inc.count,
                .next = try self.closeJoinParamsOnExit(params, internal_join_ids, inc.next),
            } }),
            .decref => |dec| try self.store.addCFStmt(.{ .decref = .{
                .value = dec.value,
                .next = try self.closeJoinParamsOnExit(params, internal_join_ids, dec.next),
            } }),
            .free => |free_stmt| try self.store.addCFStmt(.{ .free = .{
                .value = free_stmt.value,
                .next = try self.closeJoinParamsOnExit(params, internal_join_ids, free_stmt.next),
            } }),
            .switch_stmt => |switch_stmt| blk: {
                var rewritten_branches: std.ArrayListUnmanaged(LIR.CFSwitchBranch) = .empty;
                defer rewritten_branches.deinit(self.allocator);

                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try rewritten_branches.append(self.allocator, .{
                        .value = branch.value,
                        .body = try self.closeJoinParamsOnExit(params, internal_join_ids, branch.body),
                    });
                }

                break :blk try self.store.addCFStmt(.{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = try self.store.addCFSwitchBranches(rewritten_branches.items),
                    .default_branch = try self.closeJoinParamsOnExit(params, internal_join_ids, switch_stmt.default_branch),
                } });
            },
            .borrow_scope => |scope| try self.store.addCFStmt(.{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.closeJoinParamsOnExit(params, internal_join_ids, scope.body),
                .remainder = try self.closeJoinParamsOnExit(params, internal_join_ids, scope.remainder),
            } }),
            .join => |join| blk: {
                const nested_join_ids = try self.allocator.alloc(LIR.JoinPointId, internal_join_ids.len + 1);
                defer self.allocator.free(nested_join_ids);
                @memcpy(nested_join_ids[0..internal_join_ids.len], internal_join_ids);
                nested_join_ids[internal_join_ids.len] = join.id;

                break :blk try self.store.addCFStmt(.{ .join = .{
                    .id = join.id,
                    .params = join.params,
                    .body = try self.closeJoinParamsOnExit(params, nested_join_ids, join.body),
                    .remainder = try self.closeJoinParamsOnExit(params, nested_join_ids, join.remainder),
                } });
            },
            .jump => |jump| if (for (internal_join_ids) |internal_join_id| {
                if (jump.target == internal_join_id) break true;
            } else false)
                stmt_id
            else
                try self.prependJoinParamDecrefs(params, self.store.getLocalSpan(jump.args), stmt_id),
            .ret => |ret_stmt| blk: {
                const forwarded = [_]LocalId{ret_stmt.value};
                break :blk try self.prependJoinParamDecrefs(params, &forwarded, stmt_id);
            },
            .runtime_error, .crash => try self.prependJoinParamDecrefs(params, &.{}, stmt_id),
            .scope_exit => stmt_id,
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

                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
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
        const use_count = self.symbol_use_counts.get(localKey(target)) orelse 0;
        var next = try self.processStmt(next_raw);
        const target_layout = self.localLayout(target);

        if (self.layoutNeedsRc(target_layout) and resultIsFresh(stmt)) {
            const summary = self.useSummary(target);
            if (use_count > 0 and summary.borrow_count != 0 and summary.consume_count == 0) {
                next = try self.appendDecrefToTerminalPaths(target, next);
                return try self.rebuildAssignStmt(stmt, next);
            }
        }

        if (self.layoutNeedsRc(target_layout) and resultIsFresh(stmt)) {
            const summary = self.useSummary(target);
            if (summary.consume_count > 1) {
                const decref_stmt = try self.store.addCFStmt(.{ .decref = .{
                    .value = target,
                    .next = next,
                } });
                const incref_stmt = try self.store.addCFStmt(.{ .incref = .{
                    .value = target,
                    .count = @intCast(summary.consume_count - 1),
                    .next = decref_stmt,
                } });
                return try self.rebuildAssignStmt(stmt, incref_stmt);
            }
        }

        if (self.layoutNeedsRc(target_layout) and use_count == 0 and resultIsFresh(stmt)) {
            const free_stmt = try self.store.addCFStmt(.{ .free = .{
                .value = target,
                .next = next,
            } });
            return try self.rebuildAssignStmt(stmt, free_stmt);
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
        return self.wrapAssignLike(.{ .assign_ref = assign }, assign.target, assign.next);
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
        for (args, param_use_kinds) |arg, use_kind| {
            if (use_kind == .consume) try consume_args.append(self.allocator, arg);
        }
        return self.prependConsumeArgIncrefs(consume_args.items, rewritten);
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
        for (args, arg_ownership) |arg, ownership| {
            if (ownership == .consume) try consume_args.append(self.allocator, arg);
        }
        return self.prependConsumeArgIncrefs(consume_args.items, rewritten);
    }

    fn processAssignList(self: *RcInsertPass, assign: AssignListStmt) Allocator.Error!CFStmtId {
        const rewritten = try self.wrapAssignLike(.{ .assign_list = assign }, assign.target, assign.next);
        return self.prependConsumeArgIncrefs(self.store.getLocalSpan(assign.elems), rewritten);
    }

    fn processAssignStruct(self: *RcInsertPass, assign: AssignStructStmt) Allocator.Error!CFStmtId {
        const rewritten = try self.wrapAssignLike(.{ .assign_struct = assign }, assign.target, assign.next);
        return self.prependConsumeArgIncrefs(self.store.getLocalSpan(assign.fields), rewritten);
    }

    fn processAssignTag(self: *RcInsertPass, assign: AssignTagStmt) Allocator.Error!CFStmtId {
        const rewritten = try self.wrapAssignLike(.{ .assign_tag = assign }, assign.target, assign.next);
        return self.prependConsumeArgIncrefs(self.store.getLocalSpan(assign.args), rewritten);
    }
};
