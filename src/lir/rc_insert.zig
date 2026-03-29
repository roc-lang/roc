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
    has_borrow: bool = false,
    has_consume: bool = false,
};

/// Inserts explicit reference-count statements into statement-only LIR.
pub const RcInsertPass = struct {
    allocator: Allocator,
    store: *LirStore,
    layout_store: *const layout_mod.Store,

    symbol_use_counts: std.AutoHashMap(u64, u32),
    symbol_use_summaries: std.AutoHashMap(u64, UseSummary),
    symbol_alias_sources: std.AutoHashMap(u64, u64),
    active_join_params: std.AutoHashMap(u32, LIR.LocalSpan),

    /// Initializes an RC insertion pass over the provided LIR store.
    pub fn init(allocator: Allocator, store: *LirStore, layout_store: *const layout_mod.Store) Allocator.Error!RcInsertPass {
        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = layout_store,
            .symbol_use_counts = std.AutoHashMap(u64, u32).init(allocator),
            .symbol_use_summaries = std.AutoHashMap(u64, UseSummary).init(allocator),
            .symbol_alias_sources = std.AutoHashMap(u64, u64).init(allocator),
            .active_join_params = std.AutoHashMap(u32, LIR.LocalSpan).init(allocator),
        };
    }

    /// Releases all temporary analysis state owned by this pass.
    pub fn deinit(self: *RcInsertPass) void {
        self.symbol_use_counts.deinit();
        self.symbol_use_summaries.deinit();
        self.symbol_alias_sources.deinit();
        self.active_join_params.deinit();
    }

    /// Inserts RC statements for one proc body.
    pub fn insertRcOpsForProc(self: *RcInsertPass, proc_id: LirProcSpecId) Allocator.Error!void {
        self.symbol_use_counts.clearRetainingCapacity();
        self.symbol_use_summaries.clearRetainingCapacity();
        self.symbol_alias_sources.clearRetainingCapacity();
        self.active_join_params.clearRetainingCapacity();

        const proc = self.store.getProcSpec(proc_id);
        try self.countUsesInStmt(proc.body);
        self.store.getProcSpecPtr(proc_id).body = try self.processStmt(proc.body);
    }

    /// Inserts RC statements for every proc currently stored in the LIR store.
    pub fn insertRcOpsForAllProcs(self: *RcInsertPass) Allocator.Error!void {
        for (0..self.store.getProcSpecs().len) |i| {
            try self.insertRcOpsForProc(@enumFromInt(@as(u32, @intCast(i))));
        }
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

    fn bumpUse(self: *RcInsertPass, local: LocalId) Allocator.Error!void {
        if (!self.layoutNeedsRc(self.localLayout(local))) return;
        const gop = try self.symbol_use_counts.getOrPut(localKey(local));
        if (!gop.found_existing) gop.value_ptr.* = 0;
        gop.value_ptr.* += 1;
    }

    fn countUsesInLocals(self: *RcInsertPass, locals: []const LocalId) Allocator.Error!void {
        for (locals) |local| try self.bumpUse(local);
    }

    fn mergeUseSummary(self: *RcInsertPass, local: LocalId, summary: UseSummary) Allocator.Error!void {
        if (!self.layoutNeedsRc(self.localLayout(local))) return;
        const gop = try self.symbol_use_summaries.getOrPut(localKey(local));
        if (!gop.found_existing) gop.value_ptr.* = .{};
        gop.value_ptr.has_borrow = gop.value_ptr.has_borrow or summary.has_borrow;
        gop.value_ptr.has_consume = gop.value_ptr.has_consume or summary.has_consume;
    }

    fn markUse(self: *RcInsertPass, local: LocalId, kind: UseKind) Allocator.Error!void {
        if (!self.layoutNeedsRc(self.localLayout(local))) return;
        try self.bumpUse(local);
        const gop = try self.symbol_use_summaries.getOrPut(localKey(local));
        if (!gop.found_existing) gop.value_ptr.* = .{};
        switch (kind) {
            .borrow => gop.value_ptr.has_borrow = true,
            .consume => gop.value_ptr.has_consume = true,
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
                try self.bumpUse(aliased.owner);
                try self.mergeUseSummary(aliased.owner, target_summary);
            },
            .borrow_of => |borrowed| {
                try self.bumpUse(borrowed.owner);
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
                for (self.store.getLocalSpan(assign.args)) |arg| try self.markUse(arg, .consume);
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
                    if (arg == param) continue;
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
            .alias_of => |source| try self.symbol_alias_sources.put(target_key, localKey(source.owner)),
            .borrow_of => |borrowed| try self.symbol_alias_sources.put(target_key, localKey(borrowed.owner)),
        }
    }

    fn resultIsFresh(stmt: CFStmt) bool {
        return switch (resultSemantics(stmt)) {
            .fresh => true,
            else => false,
        };
    }

    fn appendDecrefToTerminalPaths(self: *RcInsertPass, value: LocalId, stmt_id: CFStmtId) Allocator.Error!CFStmtId {
        const stmt = self.store.getCFStmt(stmt_id);
        return switch (stmt) {
            .assign_symbol => |assign| try self.store.addCFStmt(.{ .assign_symbol = .{
                .target = assign.target,
                .symbol = assign.symbol,
                .next = try self.appendDecrefToTerminalPaths(value, assign.next),
            } }),
            .assign_ref => |assign| try self.store.addCFStmt(.{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .next = try self.appendDecrefToTerminalPaths(value, assign.next),
            } }),
            .assign_literal => |assign| try self.store.addCFStmt(.{ .assign_literal = .{
                .target = assign.target,
                .result = assign.result,
                .value = assign.value,
                .next = try self.appendDecrefToTerminalPaths(value, assign.next),
            } }),
            .assign_call => |assign| try self.store.addCFStmt(.{ .assign_call = .{
                .target = assign.target,
                .result = assign.result,
                .proc = assign.proc,
                .args = assign.args,
                .next = try self.appendDecrefToTerminalPaths(value, assign.next),
            } }),
            .assign_low_level => |assign| try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .args = assign.args,
                .next = try self.appendDecrefToTerminalPaths(value, assign.next),
            } }),
            .assign_list => |assign| try self.store.addCFStmt(.{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .elems = assign.elems,
                .next = try self.appendDecrefToTerminalPaths(value, assign.next),
            } }),
            .assign_struct => |assign| try self.store.addCFStmt(.{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .fields = assign.fields,
                .next = try self.appendDecrefToTerminalPaths(value, assign.next),
            } }),
            .assign_tag => |assign| try self.store.addCFStmt(.{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .discriminant = assign.discriminant,
                .args = assign.args,
                .next = try self.appendDecrefToTerminalPaths(value, assign.next),
            } }),
            .debug => |stmt_debug| try self.store.addCFStmt(.{ .debug = .{
                .message = stmt_debug.message,
                .next = try self.appendDecrefToTerminalPaths(value, stmt_debug.next),
            } }),
            .expect => |stmt_expect| try self.store.addCFStmt(.{ .expect = .{
                .condition = stmt_expect.condition,
                .next = try self.appendDecrefToTerminalPaths(value, stmt_expect.next),
            } }),
            .incref => |inc| try self.store.addCFStmt(.{ .incref = .{
                .value = inc.value,
                .count = inc.count,
                .next = try self.appendDecrefToTerminalPaths(value, inc.next),
            } }),
            .decref => |dec| try self.store.addCFStmt(.{ .decref = .{
                .value = dec.value,
                .next = try self.appendDecrefToTerminalPaths(value, dec.next),
            } }),
            .free => |free_stmt| try self.store.addCFStmt(.{ .free = .{
                .value = free_stmt.value,
                .next = try self.appendDecrefToTerminalPaths(value, free_stmt.next),
            } }),
            .switch_stmt => |switch_stmt| blk: {
                var rewritten_branches: std.ArrayListUnmanaged(LIR.CFSwitchBranch) = .empty;
                defer rewritten_branches.deinit(self.allocator);

                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try rewritten_branches.append(self.allocator, .{
                        .value = branch.value,
                        .body = try self.appendDecrefToTerminalPaths(value, branch.body),
                    });
                }

                break :blk try self.store.addCFStmt(.{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = try self.store.addCFSwitchBranches(rewritten_branches.items),
                    .default_branch = try self.appendDecrefToTerminalPaths(value, switch_stmt.default_branch),
                } });
            },
            .borrow_scope => |scope| try self.store.addCFStmt(.{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.appendDecrefToTerminalPaths(value, scope.body),
                .remainder = try self.appendDecrefToTerminalPaths(value, scope.remainder),
            } }),
            .join => |join| try self.store.addCFStmt(.{ .join = .{
                .id = join.id,
                .params = join.params,
                .body = try self.appendDecrefToTerminalPaths(value, join.body),
                .remainder = try self.appendDecrefToTerminalPaths(value, join.remainder),
            } }),
            .ret => |ret_stmt| try self.store.addCFStmt(.{ .decref = .{
                .value = value,
                .next = try self.store.addCFStmt(.{ .ret = ret_stmt }),
            } }),
            .runtime_error, .crash => try self.store.addCFStmt(.{ .decref = .{
                .value = value,
                .next = stmt_id,
            } }),
            .scope_exit, .jump => stmt_id,
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
            .join => |join| try self.store.addCFStmt(.{ .join = .{
                .id = join.id,
                .params = join.params,
                .body = try self.processStmt(join.body),
                .remainder = try self.processStmt(join.remainder),
            } }),
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
        const semantics = resultSemantics(stmt);
        var next = try self.processStmt(next_raw);

        const target_layout = self.localLayout(target);
        if (self.layoutNeedsRc(target_layout)) try self.recordAliasSemantics(target, semantics);

        if (self.layoutNeedsRc(target_layout) and resultIsFresh(stmt)) {
            const summary = self.useSummary(target);
            if (use_count > 0 and summary.has_borrow and !summary.has_consume) {
                next = try self.appendDecrefToTerminalPaths(target, next);
                return try self.rebuildAssignStmt(stmt, next);
            }
        }

        if (self.layoutNeedsRc(target_layout) and use_count > 1 and resultIsFresh(stmt)) {
            const decref_stmt = try self.store.addCFStmt(.{ .decref = .{
                .value = target,
                .next = next,
            } });
            const incref_stmt = try self.store.addCFStmt(.{ .incref = .{
                .value = target,
                .count = @intCast(use_count - 1),
                .next = decref_stmt,
            } });
            return try self.rebuildAssignStmt(stmt, incref_stmt);
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
        return self.wrapAssignLike(.{ .assign_call = assign }, assign.target, assign.next);
    }

    fn processAssignLowLevel(self: *RcInsertPass, assign: AssignLowLevelStmt) Allocator.Error!CFStmtId {
        return self.wrapAssignLike(.{ .assign_low_level = assign }, assign.target, assign.next);
    }

    fn processAssignList(self: *RcInsertPass, assign: AssignListStmt) Allocator.Error!CFStmtId {
        return self.wrapAssignLike(.{ .assign_list = assign }, assign.target, assign.next);
    }

    fn processAssignStruct(self: *RcInsertPass, assign: AssignStructStmt) Allocator.Error!CFStmtId {
        return self.wrapAssignLike(.{ .assign_struct = assign }, assign.target, assign.next);
    }

    fn processAssignTag(self: *RcInsertPass, assign: AssignTagStmt) Allocator.Error!CFStmtId {
        return self.wrapAssignLike(.{ .assign_tag = assign }, assign.target, assign.next);
    }
};
